(in-package :aero-fighter)

#+nil
(launch)

(defstruct (sprite (:copier nil) (:predicate nil))
  (texture nil :read-only t)
  (pos (raylib:make-vector2 :x 0.0 :y 0.0))
  (bbox nil :read-only t))

(defun draw-sprite (sprite)
  (raylib:draw-texture-rec (sprite-texture sprite) (sprite-bbox sprite) (sprite-pos sprite) raylib:+white+))

(defun make-fighter ()
  "Read in the fighter texture."
  (let* ((sprite (raylib:load-texture "assets/fighter.png"))
         (bbox   (raylib:make-rectangle :x 0.0 :y 0.0
                                        :width  (float (raylib:texture-width sprite))
                                        :height (float (raylib:texture-height sprite)))))
    (make-sprite :texture sprite :bbox bbox)))
