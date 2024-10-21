(in-package :aero-fighter)

#+nil
(launch)

;; NOTE: When you add a texture here, make sure to unload it in `ungame' below.
(defstruct (sprites (:constructor sprites))
  "A bank of various sprites and their loaded textures."
  (fighter (sprite #p"assets/fighter.json"))
  (blob    (sprite #p"assets/blob.json")))

(defstruct game
  "The state of the running game."
  (camera  (camera))
  (sprites nil)
  (fighter nil)
  (blobs   (make-hash-table :size 16))
  (frame   0))

(defun game ()
  "Initialise the various game resources."
  (let ((sprites (sprites)))
    (make-game :sprites sprites
               :fighter (fighter (sprites-fighter sprites)))))

(defun camera ()
  "Initialise a 2D Camera."
  (let* ((center-x (/ +screen-width+ 2.0))
         (center-y (/ +screen-height+ 2.0))
         (offset   (raylib:make-vector2 :x center-x :y center-y))
         (target   (raylib:make-vector2 :x 0.0 :y 0.0)))
    ;; TODO: Restore to Zoom 3 once testing is done.
    (raylib:make-camera-2d :offset offset :target target :rotation 0.0 :zoom 2.0)))

(defun ungame (game)
  "Release various resources."
  (let ((sprites (game-sprites game)))
    (raylib:unload-texture (sprite-texture (animated-sprite (fighter-animated (sprites-fighter sprites)))))
    (raylib:unload-texture (sprite-texture (sprites-blob sprites)))))
