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
  nil)

(defun update-playing (game)
  "Logic specific to a started game."
  (maybe-spawn-blob game)
  (move-all-blobs game)
  (move (game-fighter game))
  (blob-collision (game-fighter game) (game-blobs game) (game-frame game))
  (update-fighter-status (game-fighter game) (game-frame game)))

(defun update-dead (game)
  "The player is dead, and they might restart the game."
  nil)

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
  nil)

(defun render-playing (game)
  "Render a running game."
  (draw-all-blobs game)
  (draw-fighter (game-fighter game) (game-frame game))
  (debugging-nearness (game-fighter game) (game-blobs game)))

(defun render-dead (game)
  "Render the Game Over screen."
  nil)

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
