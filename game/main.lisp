(in-package :aero-fighter)

#+nil
(launch)

;; --- Event Handling --- ;;

(defun update! (game)
  "Following TEA, update the game state."
  (incf (game-frame game))
  (track-transition game)
  (case (game-mode game)
    (:playing (update-playing! game))
    (:booting (update-booting! game))
    (:waiting (update-waiting! game))
    (:dead    (update-dead! game))))

(defun update-booting! (game)
  (when (> (game-frame game)
           (* 2 +frame-rate+))
    ;; NOTE: 2025-02-06 If we attempt to set the controller value too quickly
    ;; (say, in `launch'), Raylib might not be ready yet and report that it
    ;; can't find any controllers. By doing it here instead, the system has had
    ;; 2 seconds to warm up, and at least experimentally the controllers can be
    ;; detected by this point.
    (find-gamepads! game)
    (auto-set-gamepad! game)
    (setf (game-mode game) :waiting)))

(defun update-waiting! (game)
  "We're waiting for the player to start the game."
  (update-environment! game)
  (move! (game-logo game))
  (when (switch-gamepad? game)
    (setf (game-anything-pressed? game) t)
    (set-gamepad! (mod (1+ +gamepad+) (length (game-gamepads game)))))
  (when (or (raylib:is-key-down +key-space+)
            (raylib:is-gamepad-button-down +gamepad+ +gamepad-start+))
    (setf (game-mode game) :playing)
    (setf (game-frame-started game) (game-frame game))))

(defun update-dead! (game)
  "The player is dead, and they might restart the game."
  (update-environment! game)
  (update-enemies! game)
  (when (pressing-start?)
    (reset-game! game)))

(defun update-enemies! (game)
  "Spawn and move the enemies around."
  (let ((fc (game-frame game)))
    (maybe-spawn-something! game)
    (move-enemies! (game-blobs game))
    (move-enemies! (game-buildings game))
    (move-enemies! (game-tanks game))
    (move-enemies! (game-missiles game))
    (move-enemies! (game-cannons game))
    (t:transduce (t:map (lambda (ship) (move-evil-ship! (cdr ship) (game-buildings game))))
                 #'t:for-each (game-evil-ships game))
    (tick! (game-tanks game) fc)
    (tick! (game-evil-ships game) fc)
    (despawn! (game-explosions game) fc)))

(defun update-player! (game)
  "Handle powerups, player input, and collisions."
  (let* ((fighter (game-fighter game))
         (beam    (fighter-beam fighter))
         (fc      (game-frame game)))
    (maybe-spawn-powerup! game)
    (tick! (game-powerups game) fc)
    (tick! fighter fc)
    (despawn! (game-powerups game) fc)
    (handle-player-input! game)
    (when (beam-shooting? beam)
      (damage-from-shot! game beam (game-blobs game))
      (damage-from-shot! game beam (game-tanks game))
      (damage-from-shot! game beam (game-evil-ships game))
      (damage-from-shot! game beam (game-missiles game)))
    (handle-fighter-collisions! game)
    (when-let* ((pu (colliding-entity fighter (game-powerups game))))
      (collect-powerup! game (car pu) (cdr pu)))))

(defun update-environment! (game)
  "Move the ground, etc."
  (t:transduce (t:map (lambda (road) (move! (cdr road))))
               #'t:for-each (game-road game))
  (t:transduce (t:map (lambda (ground) (move! (cdr ground))))
               #'t:for-each (game-ground game)))

(defun update-playing! (game)
  "Logic specific to a started game."
  (update-environment! game)
  (update-enemies! game)
  (update-player! game)
  (bump-score-by-frame! game)
  (bump-level! game))

(defun collect-powerup! (game key pu)
  "Alter the fighter according to a powerup he just grabbed."
  (let* ((fighter (game-fighter game))
         (beam    (fighter-beam fighter)))
    (etypecase pu
      (ammo (when (< (fighter-bombs fighter) +bomb-max-capacity+)
              (incf (fighter-bombs fighter))
              (->> game game-sounds sounds-get-ammo raylib:play-sound)))
      (wide (unless (max-beam? (game-sprites game) (->> beam beam-animated animated-sprite))
              (setf (fighter-beam fighter)
                    (@beam (upgrade-beam (game-sprites game)
                                         (->> beam beam-animated animated-sprite))
                           (fighter-pos fighter)
                           (raylib:rectangle-width (fighter-bbox fighter))
                           +beam-y-offset+))
              (->> game game-sounds sounds-beam-collect raylib:play-sound)))
      (shield
       (setf (->> game game-fighter fighter-shielded?) t)
       (->> game game-sounds sounds-shield-up raylib:play-sound))
      (god-mode
       (setf (fighter-god-mode? fighter) t)
       (->> game game-sounds sounds-god-mode raylib:play-sound)))
    (remhash key (game-powerups game))))

(defun bump-score-by-frame! (game)
  "Gradually increase the score over time."
  (when (zerop (mod (game-frame game) +frame-rate+))
    (incf (game-score game))))

(defun bump-level! (game)
  "If the player's score is good enough, bump the level and make the game harder."
  (when (>= (game-score game)
            (game-level-thresh game))
    (incf (game-level-thresh game) +level-progression-interval+)
    (incf (game-level game))
    (setf (gethash (game-frame game) (game-cannons game))
          (let ((sprites (game-sprites game)))
            (->> game game-sounds sounds-cannon raylib:play-sound)
            (@cannon (sprites-cannon-bulb sprites)
                     (sprites-cannon-beam sprites))))))

(defun handle-fighter-collisions! (game)
  "If the fighter got hit by something, kill and (maybe) respawn him."
  (let* ((fighter (game-fighter game))
         (fc      (game-frame game)))
    (when (and (eq :ok (fighter-status fighter))
               (or (when-let* ((pos (or (direct-collision! fighter (game-blobs game))
                                        (direct-collision! fighter (game-evil-ships game))
                                        (direct-collision! fighter (game-missiles game)))))
                     ;; An explosion over top of the collided enemy.
                     (explode! game pos))
                   ;; Certain enemies/obstacles don't get destroyed upon collision.
                   (near-entity-collision? fighter (game-buildings game))
                   (near-entity-collision? fighter (game-cannons game))
                   ;; Or a tank/ship just shot you.
                   (got-shot? fighter (game-tanks game))
                   (got-shot? fighter (game-evil-ships game))))
      (cond ((fighter-shielded? fighter)
             ;; This grants invincibility for the same span as being actually
             ;; killed, so that collision on the next frame doesn't immediately
             ;; kill the fighter anyway.
             (setf (fighter-shielded? fighter) nil)
             (set-status! fighter :hit fc)
             (setf (->> fighter fighter-shield aura-disperse-fc) fc)
             (set-animation! (->> fighter fighter-shield aura-animated) :disperse fc)
             (->> game game-sounds sounds-shield-down raylib:play-sound))
            (t (explode! game (fighter-pos fighter))
               ;; Kill the fighter.
               (-<>> fighter
                 (fighter-beam)
                 (beam-animated)
                 (animated-sprite)
                 (downgrade-beam (game-sprites game))
                 (kill-fighter! fighter <> (->> game game-sounds sounds-explosion-4) fc))
               (decf (game-lives game))
               (when (<= (game-lives game) 0)
                 (setf (game-mode game) :dead)))))))

(defun damage-from-shot! (game beam enemies)
  "Check for hits by the fighter's beam and apply damage if necessary."
  (let ((hits (enemies-hit-by-beam beam enemies))
        (dmg  (->> game game-fighter fighter-beam-dmg))
        (fc   (game-frame game))
        (sounds (game-sounds game)))
    (t:transduce (t:comp (t:filter (lambda (enemy) (vulnerable? (cdr enemy) fc)))
                         (t:map (lambda (enemy)
                                  (damage! (cdr enemy) dmg fc)
                                  (explode! game (pos (cdr enemy)))
                                  enemy))
                         ;; Despawn the enemy if it's dead, and reward the
                         ;; player with some points.
                         (t:filter (lambda (enemy) (<= (health (cdr enemy)) 0)))
                         (t:map (lambda (enemy)
                                  (remhash (car enemy) enemies)
                                  (incf (game-score game) (points (cdr enemy)))
                                  (let ((sound (case (mod (car enemy) 3)
                                                 (0 (sounds-explosion-1 sounds))
                                                 (1 (sounds-explosion-2 sounds))
                                                 (t (sounds-explosion-3 sounds)))))
                                    (raylib:play-sound sound)))))
                 #'t:for-each hits)))

(defun handle-player-input! (game)
  "Alter the state according to what the human player is doing."
  (let* ((fighter (game-fighter game))
         (beam    (fighter-beam fighter))
         (fc      (game-frame game)))
    (maybe-set-warp-direction! fighter)
    (when (eq :warped (move! fighter))
      (->> game game-sounds sounds-warp raylib:play-sound))
    (when (and (not (beam-shooting? beam))
               (trying-to-shoot?))
      (shoot-beam! beam fc))
    (when (and (can-bomb? fighter fc)
               (or (raylib:is-key-down +key-enter+)
                   (raylib:is-gamepad-button-down +gamepad+ +gamepad-b+)))
      (launch-bomb! game)))
  #++
  (when (pressing-start?)
    ;; TODO: 2024-11-30 Exclude this from release builds somehow.
    (break "Aero Fighter: Paused"))
  #+nil
  (debugging-gamepad))

(defun launch-bomb! (game)
  "Kill all the enemies."
  (let ((fighter (game-fighter game))
        (fc      (game-frame game)))
    (decf (fighter-bombs fighter))
    (setf (fighter-bomb-fc fighter) fc)
    (bump-score-and-explode! game (game-tanks game))
    (bump-score-and-explode! game (game-evil-ships game))
    (bump-score-and-explode! game (game-blobs game))
    (bump-score-and-explode! game (game-missiles game))
    (clear-all-enemies! game)
    (->> game game-sounds sounds-bomb-use raylib:play-sound)))

(defun bump-score-and-explode! (game enemies)
  (t:transduce (t:map (lambda (enemy)
                        ;; FIXME: 2024-11-23 You could easily use a global in
                        ;; `launch-bomb!' for each enemy type, and thereby
                        ;; restore the closed-form point addition, instead of
                        ;; doing it interatively here.
                        (incf (game-score game) (points (cdr enemy)))
                        (explode! game (pos (cdr enemy)))))
               #'t:for-each enemies))

(defun maybe-spawn-something! (game)
  "Potentially spawn an enemy / powerup depending on the predetermined spawn
schedule."
  (let ((n       (random 5000))
        (fc      (game-frame game))
        (level   (game-level game))
        (sprites (game-sprites game))
        (fighter (game-fighter game)))
    (when (and (zerop (mod fc +frame-rate+))
               (not (bomb-cooling-down? fighter fc)))
      (setf (gethash fc (game-missiles game))
            (@missile (sprites-missile sprites) (sprites-missile-shadow sprites))))
    (cond ((and (< n 4)
                ;; Don't spawn a bomb if the player is already full.
                (has-bomb-capacity? fighter))
           (setf (gethash fc (game-powerups game))
                 (@ammo (sprites-bomb sprites) fc)))
          ((< n 36)
           (setf (gethash fc (game-buildings game))
                 (@building (sprites-building sprites)
                            (game-buildings game)
                            (game-road game)
                            (sprites-building-shadow sprites))))
          ((and (< n 52)
                ;; Only one Evil Ship at a time.
                (zerop (hash-table-count (game-evil-ships game))))
           (setf (gethash fc (game-evil-ships game))
                 (@evil-ship (sprites-evil-ship sprites)
                             (beam-by-level game)
                             (sprites-shadow sprites)
                             (fighter-pos fighter)
                             fc)))
          ;; --- Varying Spawn Rate by Level --- ;;
          ((< n (* level 92))
           (setf (gethash fc (game-blobs game))
                 (@blob (sprites-blob sprites) (sprites-blob-shadow sprites))))
          ((< n (* level 117))
           (setf (gethash fc (game-tanks game))
                 (@tank (sprites-tank sprites)
                        (beam-by-level game)
                        (game-buildings game)
                        fc))))))

;; --- Rendering --- ;;

(defun render (game)
  "Following TEA, render the updated state of a game."
  (with-drawing
    (raylib:clear-background +white+)
    (with-2d-camera (game-camera game)
      #++ (debugging-dots)
      (case (game-mode game)
        (:playing (render-playing game))
        (:booting (render-booting game))
        (:waiting (render-waiting game))
        (:dead    (render-dead game))))
    (raylib:draw-fps 10 (- +screen-height+ 25))
    #++ (raylib:draw-text (format nil "FC: ~a" (game-frame game)) 10 (- +screen-height+ 45) 20 raylib:+lightgray+)
    #++ (raylib:draw-text (format nil "DMG: ~a" (->> game game-fighter fighter-beam-dmg)) 10 (- +screen-height+ 75) 20 raylib:+lightgray+)
    #++ (raylib:draw-text (format nil "SCORE: ~a" (game-score game)) 10 (- +screen-height+ 75) 20 raylib:+lightgray+)))

(defun render-booting (game)
  "The initial boot-up animation."
  (raylib:clear-background +black+)
  (draw-su-logo (->> game game-sprites sprites-su-logo) (game-frame game)))

(defun render-waiting (game)
  "Render a splash screen."
  (let ((fc (game-frame game)))
    (draw (game-ground game) fc)
    (draw (game-road game) fc)
    (draw (game-logo game) fc)
    (draw-controller! game)
    (when (< (mod fc +frame-rate+) 30)
      (raylib:draw-text (format nil "PRESS START")
                        (- 0 37) 40 10 +white+))
    (when (and (< (mod (+ fc 30) +frame-rate+) 30)
               (not (game-anything-pressed? game)))
      (raylib:draw-texture (->> game game-sprites sprites-test)
                           (- +world-max-x+ 50)
                           (- +world-max-y+ 10)
                           +white+))
    (when (game-anything-pressed? game)
      (let* ((sprite    (->> game game-sprites sprites-numbers))
             (texture   (sprite-texture sprite))
             (animation (gethash :numbers (sprite-animations sprite))))
        (draw-at-frame texture animation
                       (game-curr-pad-pos game)
                       +gamepad+)))))

(defun draw-controller! (game)
  "Draw a little controller in the corner, with button highlights depending on what
the player is pressing."
  (raylib:draw-texture (->> game game-sprites sprites-controller)
                       (- +world-max-x+ 32)
                       (- +world-max-y+ 16)
                       +white+)
  ;; --- Named Buttons --- ;;
  (cond ((or (raylib:is-key-down +key-space+)
             (raylib:is-gamepad-button-down +gamepad+ +gamepad-a+))
         (setf (game-anything-pressed? game) t)
         (raylib:draw-rectangle (- +world-max-x+ 5)
                                (- +world-max-y+ 8)
                                2 2 +red+)
         (raylib:draw-text "SHOOT" (- +world-max-x+ 35) (- +world-max-y+ 25) 10 +white+))
        ((or (raylib:is-key-down +key-enter+)
             (raylib:is-gamepad-button-down +gamepad+ +gamepad-b+))
         (setf (game-anything-pressed? game) t)
         (raylib:draw-rectangle (- +world-max-x+ 9)
                                (- +world-max-y+ 5)
                                2 2 +red+)
         (raylib:draw-text "BOMB" (- +world-max-x+ 30) (- +world-max-y+ 25) 10 +white+))
        ((raylib:is-gamepad-button-down +gamepad+ +gamepad-x+)
         (setf (game-anything-pressed? game) t)
         (raylib:draw-rectangle (- +world-max-x+ 9)
                                (- +world-max-y+ 11)
                                2 2 +red+)
         (raylib:draw-text "WARP" (- +world-max-x+ 30) (- +world-max-y+ 25) 10 +white+))
        ((raylib:is-gamepad-button-down +gamepad+ +gamepad-y+)
         (setf (game-anything-pressed? game) t)
         (raylib:draw-rectangle (- +world-max-x+ 13)
                                (- +world-max-y+ 8)
                                2 2 +red+)
         (raylib:draw-text "WARP" (- +world-max-x+ 30) (- +world-max-y+ 25) 10 +white+))
        ;; TODO: 2024-12-12 Warp button for keyboard.
        ((or (raylib:is-gamepad-button-down +gamepad+ +gamepad-left-shoulder+))
         (setf (game-anything-pressed? game) t)
         (raylib:draw-rectangle (- +world-max-x+ 26)
                                (- +world-max-y+ 15)
                                6 1 +red+)
         (raylib:draw-text "WARP" (- +world-max-x+ 30) (- +world-max-y+ 25) 10 +white+))
        ((or (raylib:is-gamepad-button-down +gamepad+ +gamepad-right-shoulder+))
         (setf (game-anything-pressed? game) t)
         (raylib:draw-rectangle (- +world-max-x+ 12)
                                (- +world-max-y+ 15)
                                6 1 +red+)
         (raylib:draw-text "WARP" (- +world-max-x+ 30) (- +world-max-y+ 25) 10 +white+)))
  ;; --- Directions --- ;;
  (when (or (raylib:is-key-down +key-up+)
            (raylib:is-gamepad-button-down +gamepad+ +gamepad-up+))
    (setf (game-anything-pressed? game) t)
    (raylib:draw-rectangle (- +world-max-x+ 25)
                           (- +world-max-y+ 10)
                           2 2 +red+))
  (when (or (raylib:is-key-down +key-down+)
            (raylib:is-gamepad-button-down +gamepad+ +gamepad-down+))
    (setf (game-anything-pressed? game) t)
    (raylib:draw-rectangle (- +world-max-x+ 25)
                           (- +world-max-y+ 6)
                           2 2 +red+))
  (when (or (raylib:is-key-down +key-left+)
            (raylib:is-gamepad-button-down +gamepad+ +gamepad-left+))
    (setf (game-anything-pressed? game) t)
    (raylib:draw-rectangle (- +world-max-x+ 27)
                           (- +world-max-y+ 8)
                           2 2 +red+))
  (when (or (raylib:is-key-down +key-right+)
            (raylib:is-gamepad-button-down +gamepad+ +gamepad-right+))
    (setf (game-anything-pressed? game) t)
    (raylib:draw-rectangle (- +world-max-x+ 23)
                           (- +world-max-y+ 8)
                           2 2 +red+)))

(defun render-enemies (game)
  "Render all enemies."
  (let ((fc (game-frame game)))
    (draw (game-buildings game) fc)
    (draw (game-tanks game) fc)
    (draw (game-cannons game) fc)
    (draw (game-blobs game) fc)
    (draw (game-evil-ships game) fc)
    (draw (game-missiles game) fc)
    (draw (game-explosions game) fc)))

(defun render-playing (game)
  "Render a running game."
  (let* ((fc (game-frame game))
         (diff (- fc (game-frame-started game))))
    (draw (game-ground game) fc)
    (draw (game-road game) (game-frame game))
    (render-enemies game)
    (draw (game-powerups game) fc)
    (draw (game-fighter game) fc)
    (draw-hud game)
    (when-let* ((fighter (game-fighter game))
                (dir     (fighter-warp-dir fighter)))
      (draw-ghost (game-warp-ghost game) dir (fighter-pos fighter) (game-frame game)))
    (when (and (< diff (* 1.5 +frame-rate+))
               (< (mod diff (/ +frame-rate+ 2)) 15))
      (raylib:draw-text "REACH GOD MODE!" -47 0 10 +white+))
    #++ (debugging-nearness (game-fighter game) (game-blobs game))))

(defun render-dead (game)
  "Render the Game Over screen."
  (draw (game-ground game) (game-frame game))
  (draw (game-road game) (game-frame game))
  (render-enemies game)
  (draw-hud game)
  (raylib:draw-text (format nil "GAME OVER, DUDE")
                    -96 0 20 +white+)
  (raylib:draw-text (format nil "(Press Start)")
                    -36 20 10 +white+)
  (raylib:draw-text (format nil "AERO FIGHTER by Colin W")
                    (- +world-max-x+ 136)
                    (- +world-max-y+ 20)
                    10 +white+)
  (raylib:draw-text (format nil "Concept by JPJ")
                    (- +world-max-x+ 82)
                    (- +world-max-y+ 10)
                    10 +white+))

#++
(defun draw-hud (game)
  "The various status conditions that the player needs to see."
  (raylib:draw-text (format nil "LIVES: ~a" (game-lives game))
                    (+ +world-min-x+ 5)
                    (+ +world-min-y+ 5)
                    10 raylib:+gray+)
  (raylib:draw-text (format nil "BOMBS: ~a" (fighter-bombs (game-fighter game)))
                    (+ +world-min-x+ 5)
                    (+ +world-min-y+ 15)
                    10 raylib:+gray+)
  (raylib:draw-text (format nil "SCORE: ~a" (game-score game))
                    (+ +world-min-x+ 5)
                    (+ +world-min-y+ 25)
                    10 raylib:+gray+)
  (raylib:draw-text (format nil "LEVEL: ~a" (game-level game))
                    (+ +world-min-x+ 5)
                    (+ +world-min-y+ 35)
                    10 raylib:+gray+))

#++
(defun debugging-dots ()
  "For confirmation of certain coordinates in the game world."
  (raylib:draw-pixel 0 0 raylib:+red+)
  (raylib:draw-pixel +world-min-x+ +world-min-y+ raylib:+red+)
  (raylib:draw-pixel +world-min-x+ +world-max-y+ raylib:+red+)
  (raylib:draw-pixel +world-max-x+ +world-max-y+ raylib:+red+)
  (raylib:draw-pixel +world-max-x+ +world-min-y+ raylib:+red+))

#++
(defun debugging-nearness (fighter blobs)
  "Testing whether nearness detection is sufficient."
  (t:transduce (t:comp (t:map #'cdr)
                       (t:filter (lambda (blob) (near? fighter blob)))
                       (t:map (lambda (blob) (raylib:draw-line-v (pos fighter) (pos blob) raylib:+green+))))
               #'t:for-each blobs))

#+nil
(defun debugging-lines ()
  "For confirmation of certain coordinates in the game world."
  (raylib:draw-line +world-min-x+ +world-min-y+ +world-max-x+ +world-min-y+ raylib:+darkgray+)
  (raylib:draw-line +world-min-x+ +world-min-y+ +world-min-x+ +world-max-y+ raylib:+darkgray+)
  (raylib:draw-line +world-min-x+ +world-max-y+ +world-max-x+ +world-max-y+ raylib:+darkgray+)
  (raylib:draw-line +world-max-x+ +world-min-y+ +world-max-x+ +world-max-y+ raylib:+darkgray+))

;; --- Sound --- ;;

(defun track-transition (game)
  "Handle the transition of the Intro track into the Main track."
  (unless (->> game game-track raylib:is-music-stream-playing)
    (setf (game-track game) (->> game game-music music-main))
    (->> game game-track raylib:play-music-stream))
  (->> game game-track raylib:update-music-stream))

;; --- Colours --- ;;

(defun set-colours ()
  "Set certain global veriables with `raylib:color' values that I have allocated
manually. This avoids weird memory errors at runtime that occur in prebuilt
executables."
  (setf +white+ (white))
  (setf +black+ (black))
  (setf +red+   (red))
  (setf +faded-white+ (faded-white))
  (setf +very-faded-white+ (very-faded-white))
  (setf +very-faded-blue+ (very-faded-blue)))

;; --- Top-level --- ;;

(defun assets-path ()
  "Assets are installed to difficult locations depending on the platform."
  (or #+(and :release :linux) #p"/usr/share/aero-fighter/assets/"
      #p"assets/"))

(defun maybe-load-shared-objects ()
  "Under SBCL, loaded `.so' files are automatically unloaded when an Image is
created via `save-lisp-and-die'. Further, since the Image might be built with an
`.so' in one place but run with one from another, we need to be clever about
reloading. Hence this function.

In general, if an `.so' has been loaded once, doing so again causes no harm.
This helps the local development use-case."
  (or #+(and sbcl release linux) (raylib:load-shared-objects :target :linux)
      #+sbcl (raylib:load-shared-objects)))

(defun event-loop (game)
  "Loop until a signal to quit has been received."
  (unless (raylib:window-should-close)
    (update! game)
    (render game)
    (event-loop game)))

(defun launch ()
  "Launch the game."
  (maybe-load-shared-objects)
  (raylib:init-window +screen-width+ +screen-height+ "Aero Fighter")
  (raylib:init-audio-device)
  (raylib:set-target-fps +frame-rate+)
  (set-colours)
  (let ((game (@game :assets (assets-path))))
    (->> game game-track raylib:play-music-stream)
    (event-loop game)
    (ungame game))
  (raylib:close-audio-device)
  (raylib:close-window))
