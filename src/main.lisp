(in-package :aero-fighter)

#+nil
(launch)

;; --- Types --- ;;

;; NOTE: When you add a texture here, make sure to unload it in `ungame' below.
(defstruct (textures (:constructor textures))
  "A bank of the various loaded textures."
  (fighter (raylib:load-texture "assets/fighter.png"))
  (blob    (raylib:load-texture "assets/blob.png")))

(defstruct game
  "The state of the running game."
  (camera   (camera))
  (textures nil)
  (fighter  nil)
  (blobs    (make-hash-table :size 16))
  (frame    0))

(defun game ()
  "Initialise the various game resources."
  (let ((textures (textures)))
    (make-game :textures textures
               :fighter (fighter :texture (textures-fighter textures)))))

(defun camera ()
  "Initialise a 2D Camera."
  (let* ((center-x (/ +screen-width+ 2.0))
         (center-y (/ +screen-height+ 2.0))
         (offset   (raylib:make-vector2 :x center-x :y center-y))
         (target   (raylib:make-vector2 :x 0.0 :y 0.0)))
    (raylib:make-camera-2d :offset offset :target target :rotation 0.0 :zoom 3.0)))

(defun ungame (game)
  "Release various resources."
  (let ((textures (game-textures game)))
    (raylib:unload-texture (textures-fighter textures))
    (raylib:unload-texture (textures-blob textures))))

;; --- Event Handling --- ;;

(defun update (game)
  "Following TEA, update the game state."
  (incf (game-frame game))
  (maybe-spawn-blob game)
  (move-all-blobs game)
  (move (game-fighter game)))

(defun render (game)
  "Following TEA, render the updated state of a game."
  (with-drawing
    (raylib:clear-background raylib:+raywhite+)
    (with-2d-camera (game-camera game)
      (debugging-dots)
      (draw-all-blobs game)
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
