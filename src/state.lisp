(in-package :aero-fighter)

#+nil
(launch)

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
    ;; TODO: Restore to Zoom 3 once testing is done.
    (raylib:make-camera-2d :offset offset :target target :rotation 0.0 :zoom 2.0)))

(defun ungame (game)
  "Release various resources."
  (let ((textures (game-textures game)))
    (raylib:unload-texture (textures-fighter textures))
    (raylib:unload-texture (textures-blob textures))))
