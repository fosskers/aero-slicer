(in-package :aero-fighter)

#+nil
(launch)

;; --- Types --- ;;

(defstruct (game (:constructor game))
  "The state of the running game."
  (camera  (camera))
  (fighter (fighter))
  (blobs   (make-hash-table :size 16))
  (frame   0))

(defun camera ()
  "Initialise a 2D Camera."
  (let* ((center-x (/ +screen-width+ 2.0))
         (center-y (/ +screen-height+ 2.0))
         (offset   (raylib:make-vector2 :x center-x :y center-y))
         (target   (raylib:make-vector2 :x 0.0 :y 0.0)))
    (raylib:make-camera-2d :offset offset :target target :rotation 0.0 :zoom 3.0)))

(defun ungame (game)
  "Release various resources."
  (raylib:unload-texture (fighter-texture (game-fighter game))))

;; --- Event Handling --- ;;

(defun update (game)
  "Following TEA, update the game state."
  (move (game-fighter game))
  (incf (game-frame game)))

(defun render (game)
  "Following TEA, render the updated state of a game."
  (with-drawing
    (raylib:clear-background raylib:+raywhite+)
    (with-2d-camera (game-camera game)
      (debugging-dots)
      (draw (game-fighter game)))
    (raylib:draw-fps 10 10)
    (raylib:draw-text (format nil "FC: ~a" (game-frame game)) 10 (- +screen-height+ 25) 20 raylib:+lightgray+)))

(defun debugging-dots ()
  "For confirmation of certain coordinates in the game world."
  (raylib:draw-pixel 0 0 raylib:+red+)
  (raylib:draw-pixel -128 -120 raylib:+red+)
  (raylib:draw-pixel -128 119 raylib:+red+)
  (raylib:draw-pixel 127 119 raylib:+red+)
  (raylib:draw-pixel 127 -120 raylib:+red+))

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
