(in-package :aero-fighter)

#+nil
(launch)

;; --- Event Handling --- ;;

(defun update (game)
  "Following TEA, update the game state."
  (incf (game-frame game))
  (case (game-mode game)
    (playing (update-playing game))
    (waiting (update-waiting game))
    (dead    (update-dead game))))

(defun update-waiting (game)
  "We're waiting for the player to start the game."
  (when (raylib:is-key-down +key-space+)
    (setf (game-mode game) 'playing)))

(defun update-dead (game)
  "The player is dead, and they might restart the game."
  (when (raylib:is-key-down +key-space+)
    (reset-game game)))

(defun update-playing (game)
  "Logic specific to a started game."
  (maybe-spawn-blob game)
  (maybe-spawn-building game)
  (maybe-spawn-tank game)
  (move-enemies (game-blobs game))
  (move-enemies (game-buildings game))
  (move-enemies (game-tanks game))
  (t:transduce (t:comp (t:map #'cdr)
                       (t:map (lambda (tank) (maybe-tank-shoot tank (game-frame game)))))
               #'t:for-each (game-tanks game))
  (handle-player-input game)
  (let* ((fighter (game-fighter game))
         (beam    (fighter-beam fighter))
         (fc      (game-frame game)))
    (when (and (eq 'ok (fighter-status fighter))
               (or (enemy-collision? fighter (game-blobs game))
                   (enemy-collision? fighter (game-buildings game))
                   (t:transduce #'t:pass
                                (t:anyp (lambda (pair)
                                          (let* ((beam (tank-beam (cdr pair))))
                                            (and (beam-shooting? beam)
                                                 (colliding? fighter beam)))))
                                (game-tanks game))))
      (kill-fighter fighter fc)
      (decf (game-lives game))
      #+nil
      (when (<= (game-lives game) 0)
        (setf (game-mode game) 'dead)))
    (when (beam-shooting? beam)
      (damage-from-shot beam (game-blobs game))
      (damage-from-shot beam (game-tanks game)))
    (update-fighter-status fighter fc)
    (update-beam-status (fighter-beam fighter) fc)
    (t:transduce (t:map (lambda (tank) (update-tank-status (cdr tank) (game-frame game))))
                 #'t:for-each (game-tanks game))))

(defun damage-from-shot (beam enemies)
  "Check for hits by the fighter's beam and apply damage if necessary."
  (let ((hits (enemies-hit-by-beam beam enemies)))
    (t:transduce (t:comp (t:map (lambda (enemy)
                                  (apply-damage (cdr enemy))
                                  enemy))
                         ;; Despawn the enemy if it's dead.
                         (t:filter (lambda (enemy) (<= (health (cdr enemy)) 0)))
                         (t:map (lambda (enemy) (remhash (car enemy) enemies))))
                 #'t:for-each hits)))

(defun handle-player-input (game)
  "Alter the state according to what the human player is doing."
  (let* ((fighter (game-fighter game))
         (beam    (fighter-beam fighter)))
    (move fighter)
    (when (and (not (beam-shooting? beam))
               ;; Can't shoot while respawning.
               (not (eq 'hit (fighter-status fighter)))
               (raylib:is-key-down +key-space+))
      (shoot-beam beam (game-frame game)))))

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
    (update game)
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
