(in-package :aero-fighter)

#+nil
(launch)

;; --- Event Handling --- ;;

(defun update! (game)
  "Following TEA, update the game state."
  (incf (game-frame game))
  (case (game-mode game)
    (playing (update-playing! game))
    (waiting (update-waiting! game))
    (dead    (update-dead! game))))

(defun update-waiting! (game)
  "We're waiting for the player to start the game."
  (when (or (raylib:is-key-down +key-space+)
            (raylib:is-gamepad-button-down +gamepad+ +gamepad-start+))
    (setf (game-mode game) 'playing)))

(defun update-dead! (game)
  "The player is dead, and they might restart the game."
  (update-enemies! game)
  (when (or (raylib:is-key-down +key-space+)
            (raylib:is-gamepad-button-down +gamepad+ +gamepad-start+))
    (reset-game! game)))

(defun update-enemies! (game)
  "Spawn and move the enemies around."
  (let ((fc (game-frame game)))
    (maybe-spawn-blob! game)
    (maybe-spawn-building! game)
    (maybe-spawn-tank! game)
    (maybe-spawn-ship! game)
    (maybe-spawn-missile! game)
    (move-enemies! (game-blobs game))
    (move-enemies! (game-buildings game))
    (move-enemies! (game-tanks game))
    (move-enemies! (game-evil-ships game))
    (move-enemies! (game-missiles game))
    (move-enemies! (game-cannons game))
    (tick! (game-tanks game) fc)
    (tick! (game-evil-ships game) fc)
    (despawn! (game-explosions game) fc)))

(defun update-player! (game)
  "Handle powerups, player input, and collisions."
  (let* ((fighter (game-fighter game))
         (beam    (fighter-beam fighter))
         (fc      (game-frame game)))
    (maybe-spawn-ammo! game)
    (maybe-spawn-wide! game)
    (tick! (game-powerups game) fc)
    (tick! fighter fc)
    (despawn! (game-powerups game) fc)
    (handle-player-input! game)
    (handle-fighter-collisions! game)
    (when (beam-shooting? beam)
      (damage-from-shot! game beam (game-blobs game))
      (damage-from-shot! game beam (game-tanks game))
      (damage-from-shot! game beam (game-evil-ships game))
      (damage-from-shot! game beam (game-missiles game)))
    (when-let* ((pu (colliding-entity fighter (game-powerups game))))
      (collect-powerup! game (car pu) (cdr pu)))))

(defun update-playing! (game)
  "Logic specific to a started game."
  (update-enemies! game)
  (update-player! game)
  (bump-score-by-frame! game)
  (bump-level! game))

(defun collect-powerup! (game key pu)
  "Alter the fighter according to a powerup he just grabbed."
  (let* ((fighter (game-fighter game))
         (beam    (fighter-beam fighter)))
    (typecase pu
      (ammo (when (< (fighter-bombs fighter) +bomb-max-capacity+)
              (remhash key (game-powerups game))
              (incf (fighter-bombs fighter))))
      (wide (let ((beam (@beam (upgrade-beam (game-sprites game)
                                             (->> beam beam-animated animated-sprite))
                               (fighter-pos fighter)
                               (raylib:rectangle-width (fighter-bbox fighter))
                               +beam-y-offset+)))
              (setf (fighter-beam fighter) beam)
              (remhash key (game-powerups game)))))))

(defun bump-score-by-frame! (game)
  "Gradually increase the score over time."
  (when (zerop (mod (game-frame game) +frame-rate+))
    (incf (game-score game))))

(defun bump-level! (game)
  "If the player's score is good enough, bump the level and make the game harder."
  (when (>= (game-score game)
            (game-level-thresh game))
    (incf (game-level-thresh game) +level-progression-interval+)
    (incf (game-level game))))

(defun handle-fighter-collisions! (game)
  "If the fighter got hit by something, kill and (maybe) respawn him."
  (let* ((fighter (game-fighter game))
         (fc      (game-frame game)))
    (when (and (eq 'ok (fighter-status fighter))
               (or (when-let* ((pos (or (direct-collision! fighter (game-blobs game))
                                        (direct-collision! fighter (game-evil-ships game))
                                        (direct-collision! fighter (game-missiles game)))))
                     ;; An explosion over top of the collided enemy.
                     (explode! game pos))
                   ;; Certain enemies/obstacles don't get destroyed upon collision.
                   (enemy-collision? fighter (game-buildings game))
                   (enemy-collision? fighter (game-cannons game))
                   ;; Or a tank/ship just shot you.
                   (got-shot? fighter (game-tanks game))
                   (got-shot? fighter (game-evil-ships game))))
      (explode! game (fighter-pos fighter))
      ;; Kill the fighter.
      (-<>> fighter
        (fighter-beam)
        (beam-animated)
        (animated-sprite)
        (downgrade-beam (game-sprites game))
        (kill-fighter! fighter <> fc))
      (decf (game-lives game))
      (when (<= (game-lives game) 0)
        (setf (game-mode game) 'dead)))))

(defun damage-from-shot! (game beam enemies)
  "Check for hits by the fighter's beam and apply damage if necessary."
  (let ((hits (enemies-hit-by-beam beam enemies))
        (fc   (game-frame game)))
    (t:transduce (t:comp (t:filter (lambda (enemy) (vulnerable? (cdr enemy) fc)))
                         (t:map (lambda (enemy)
                                  (damage! (cdr enemy) fc)
                                  (explode! game (pos (cdr enemy)))
                                  enemy))
                         ;; Despawn the enemy if it's dead, and reward the
                         ;; player with some points.
                         (t:filter (lambda (enemy) (<= (health (cdr enemy)) 0)))
                         (t:map (lambda (enemy)
                                  (remhash (car enemy) enemies)
                                  (incf (game-score game) (points (cdr enemy))))))
                 #'t:for-each hits)))

(defun handle-player-input! (game)
  "Alter the state according to what the human player is doing."
  (let* ((fighter (game-fighter game))
         (beam    (fighter-beam fighter))
         (fc      (game-frame game)))
    (maybe-set-warp-direction! fighter)
    (move! fighter)
    (when (and (not (beam-shooting? beam))
               (trying-to-shoot?))
      (shoot-beam! beam fc))
    (when (and (can-bomb? fighter fc)
               (or (raylib:is-key-down +key-enter+)
                   (raylib:is-gamepad-button-down +gamepad+ +gamepad-b+)))
      (launch-bomb! game)))
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
    (clear-all-enemies! game)))

(defun bump-score-and-explode! (game enemies)
  (t:transduce (t:map (lambda (enemy)
                        ;; FIXME: 2024-11-23 You could easily use a global in
                        ;; `launch-bomb!' for each enemy type, and thereby
                        ;; restore the closed-form point addition, instead of
                        ;; doing it interatively here.
                        (incf (game-score game) (points (cdr enemy)))
                        (explode! game (pos (cdr enemy)))))
               #'t:for-each enemies))

(defun render (game)
  "Following TEA, render the updated state of a game."
  (with-drawing
    (raylib:clear-background raylib:+raywhite+)
    (with-2d-camera (game-camera game)
      #++ (debugging-dots)
      (case (game-mode game)
        (playing (render-playing game))
        (waiting (render-waiting game))
        (dead    (render-dead game))))
    (raylib:draw-fps 10 (- +screen-height+ 50))
    (raylib:draw-text (format nil "FC: ~a" (game-frame game)) 10 (- +screen-height+ 25) 20 raylib:+lightgray+)
    #++ (raylib:draw-text (format nil "SCORE: ~a" (game-score game)) 10 (- +screen-height+ 75) 20 raylib:+lightgray+)))

(defun render-waiting (game)
  "Render a splash screen."
  (declare (ignore game))
  (raylib:draw-text (format nil "PRESS SPACE TO PLAY")
                    (- 0 55) 0 10 raylib:+gray+))

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
  (let ((fc (game-frame game)))
    (render-enemies game)
    (draw (game-powerups game) fc)
    (draw (game-fighter game) fc)
    (draw-hud game)
    (when-let* ((fighter (game-fighter game))
                (dir     (fighter-warp-dir fighter)))
      (draw-ghost (game-warp-ghost game) dir (fighter-pos fighter) (game-frame game)))
    #++ (debugging-nearness (game-fighter game) (game-blobs game))))

(defun render-dead (game)
  "Render the Game Over screen."
  (render-enemies game)
  (draw-hud game)
  (raylib:draw-text (format nil "GAME OVER, DUDE")
                    (- 0 45) 0 10 raylib:+gray+))

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

(defun debugging-dots ()
  "For confirmation of certain coordinates in the game world."
  (raylib:draw-pixel 0 0 raylib:+red+)
  (raylib:draw-pixel +world-min-x+ +world-min-y+ raylib:+red+)
  (raylib:draw-pixel +world-min-x+ +world-max-y+ raylib:+red+)
  (raylib:draw-pixel +world-max-x+ +world-max-y+ raylib:+red+)
  (raylib:draw-pixel +world-max-x+ +world-min-y+ raylib:+red+))

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

(defun event-loop (game)
  "Loop until a signal to quit has been received."
  (unless (raylib:window-should-close)
    (update! game)
    (render game)
    (event-loop game)))

(defun launch ()
  "Launch the game."
  (raylib:init-window +screen-width+ +screen-height+ "raylib/CL Example")
  (raylib:set-target-fps +frame-rate+)
  (let ((game (@game)))
    ;; TODO: 2024-11-25 Remove.
    (setf (gethash 0 (game-cannons game))
          (->> game game-sprites sprites-cannon @cannon))
    (event-loop game)
    (ungame game))
  (raylib:close-window))
