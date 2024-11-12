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
  (when (or (raylib:is-key-down +key-space+)
            (raylib:is-gamepad-button-down +gamepad+ +gamepad-start+))
    (reset-game! game)))

(defun update-playing! (game)
  "Logic specific to a started game."
  (maybe-spawn-blob! game)
  (maybe-spawn-building! game)
  (maybe-spawn-tank! game)
  (maybe-spawn-ammo! game)
  (maybe-spawn-wide! game)
  (move-enemies! (game-blobs game))
  (move-enemies! (game-buildings game))
  (move-enemies! (game-tanks game))
  (t:transduce (t:map (lambda (tank) (maybe-tank-shoot! (cdr tank) (game-frame game))))
               #'t:for-each (game-tanks game))
  (handle-player-input! game)
  (handle-fighter-collisions! game)
  (let* ((fighter (game-fighter game))
         (beam    (fighter-beam fighter))
         (fc      (game-frame game)))
    (when (beam-shooting? beam)
      (damage-from-shot! game beam (game-blobs game))
      (damage-from-shot! game beam (game-tanks game)))
    (let ((pu (colliding-powerup fighter (game-powerups game))))
      (typecase (cdr pu)
        (ammo (when (< (fighter-bombs fighter) +bomb-max-capacity+)
                (remhash (car pu) (game-powerups game))
                (incf (fighter-bombs fighter))))
        (wide (when-let* ((next (gethash (animated-sprite (beam-animated beam))
                                         (game-beams game)))
                          (beam (beam next (fighter-pos fighter))))
                (setf (fighter-beam fighter) beam)
                (remhash (car pu) (game-powerups game))))))
    (tick! fighter fc)
    (tick! (fighter-beam fighter) fc)
    (tick! (game-powerups game) fc)
    (tick! (game-tanks game) fc)
    (despawn-powerups! (game-powerups game) fc))
  (bump-score-by-frame! game))

(defun bump-score-by-frame! (game)
  "Gradually increase the score over time."
  (when (zerop (mod (game-frame game) +frame-rate+))
    (incf (game-score game))))

(defun handle-fighter-collisions! (game)
  "If the fighter got hit by something, kill and (maybe) respawn him."
  (let* ((fighter (game-fighter game))
         (fc      (game-frame game)))
    (when (and (eq 'ok (fighter-status fighter))
               (or (enemy-collision? fighter (game-blobs game))
                   (enemy-collision? fighter (game-buildings game))
                   ;; Collision with tanks shouldn't occur because the fighter
                   ;; is in the air on they're on the ground?
                   ;;
                   ;; (enemy-collision? fighter (game-tanks game))
                   (t:transduce #'t:pass
                                (t:anyp (lambda (pair)
                                          (let* ((beam (tank-beam (cdr pair))))
                                            (and (beam-shooting? beam)
                                                 (colliding? fighter beam)))))
                                (game-tanks game))))
      (kill-fighter! fighter (sprites-beam-2 (game-sprites game)) fc)
      (decf (game-lives game))
      #+nil
      (when (<= (game-lives game) 0)
        (setf (game-mode game) 'dead)))))

(defun damage-from-shot! (game beam enemies)
  "Check for hits by the fighter's beam and apply damage if necessary."
  (let ((hits (enemies-hit-by-beam beam enemies))
        (fc   (game-frame game)))
    (t:transduce (t:comp (t:filter (lambda (enemy) (vulnerable? (cdr enemy) fc)))
                         (t:map (lambda (enemy)
                                  (damage! (cdr enemy) fc)
                                  enemy))
                         ;; Despawn the enemy if it's dead, and reward the
                         ;; player with some points.
                         (t:filter (lambda (enemy) (<= (health (cdr enemy)) 0)))
                         (t:map (lambda (enemy)
                                  (remhash (car enemy) enemies)
                                  (incf (game-score game) 100))))
                 #'t:for-each hits)))

(defun handle-player-input! (game)
  "Alter the state according to what the human player is doing."
  (let* ((fighter (game-fighter game))
         (beam    (fighter-beam fighter))
         (fc      (game-frame game)))
    (when (and (raylib:is-key-down +key-tab+)
               (can-warp? fighter fc))
      (setf (fighter-warp-next? fighter) t)
      (setf (fighter-warp-fc fighter) fc))
    (move! fighter)
    (when (and (not (beam-shooting? beam))
               ;; Can't shoot while respawning.
               (not (eq 'hit (fighter-status fighter)))
               (or (raylib:is-key-pressed +key-space+)
                   ;; Doesn't crash if the gamepad isn't plugged in.
                   (raylib:is-gamepad-button-pressed +gamepad+ +gamepad-a+)))
      (shoot-beam! beam fc))
    (when (and (can-bomb? fighter fc)
               (eq 'ok (fighter-status fighter))
               (or (raylib:is-key-down +key-enter+)
                   (raylib:is-gamepad-button-down +gamepad+ +gamepad-b+)))
      (decf (fighter-bombs fighter))
      (setf (fighter-bomb-fc fighter) fc)
      (clear-all-enemies! game))))
;; (debugging-gamepad))

(defun debugging-gamepad ()
  (let ((last-pressed (raylib:get-gamepad-button-pressed)))
    (when (not (zerop last-pressed))
      (break (format nil "Button: ~a" last-pressed)))))

(defun render (game)
  "Following TEA, render the updated state of a game."
  (with-drawing
    (raylib:clear-background raylib:+raywhite+)
    (with-2d-camera (game-camera game)
      (debugging-dots)
      (case (game-mode game)
        (playing (render-playing game))
        (waiting (render-waiting game))
        (dead    (render-dead game))))
    (raylib:draw-fps 10 (- +screen-height+ 50))
    (raylib:draw-text (format nil "FC: ~a" (game-frame game)) 10 (- +screen-height+ 25) 20 raylib:+lightgray+)))

(defun render-waiting (game)
  "Render a splash screen."
  (declare (ignore game))
  (raylib:draw-text (format nil "PRESS SPACE TO PLAY")
                    (- 0 55) 0 10 raylib:+gray+))

(defun render-playing (game)
  "Render a running game."
  (let* ((fighter (game-fighter game))
         (beam    (fighter-beam fighter))
         (fc      (game-frame game)))
    (draw (game-buildings game) fc)
    (draw (game-blobs game) fc)
    (draw (game-tanks game) fc)
    (draw (game-powerups game) fc)
    (when (beam-shooting? beam)
      (draw beam fc))
    (draw fighter fc))
  (draw-hud game))
;; (debugging-nearness (game-fighter game) (game-blobs game)))

(defun render-dead (game)
  "Render the Game Over screen."
  (declare (ignore game))
  (raylib:draw-text (format nil "GAME OVER, DUDE")
                    (- 0 45) 0 10 raylib:+gray+))

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
                    10 raylib:+gray+))

(defun debugging-dots ()
  "For confirmation of certain coordinates in the game world."
  (raylib:draw-pixel 0 0 raylib:+red+)
  (raylib:draw-pixel +world-min-x+ +world-min-y+ raylib:+red+)
  (raylib:draw-pixel +world-min-x+ +world-max-y+ raylib:+red+)
  (raylib:draw-pixel +world-max-x+ +world-max-y+ raylib:+red+)
  (raylib:draw-pixel +world-max-x+ +world-min-y+ raylib:+red+))

(defun debugging-keypress ()
  "Print the key currently being pressed."
  (let ((key (raylib:get-key-pressed)))
    (when (and key (not (zerop key)))
      (break (format nil "KEY: ~a" key))
      #+nil
      (raylib:draw-text (format nil "KEY: ~a" key)
                        (- +screen-width+ 75)
                        (- +screen-height+ 25)
                        20
                        raylib:+lightgray+))))

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
  (let ((game (game)))
    (event-loop game)
    (ungame game))
  (raylib:close-window))
