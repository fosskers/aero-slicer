(in-package :aero-fighter)

#+nil
(launch)

;; --- Types --- ;;

(defstruct sprite
  (texture nil :read-only t)
  (pos (raylib:make-vector2 :x 0.0 :y 0.0))
  (bbox nil :read-only t))

(defun make-fighter ()
  "Read in the fighter texture."
  (let* ((sprite (raylib:load-texture "assets/fighter.png"))
         (bbox   (raylib:make-rectangle :x 0.0 :y 0.0
                                        :width  (float (raylib:texture-width sprite))
                                        :height (float (raylib:texture-height sprite)))))
    (make-sprite :texture sprite :bbox bbox)))

(defmethod min-x ((sprite sprite))
  (raylib:vector2-x (sprite-pos sprite)))
(defmethod max-x ((sprite sprite))
  (+ 15 (raylib:vector2-x (sprite-pos sprite))))
(defmethod min-y ((sprite sprite))
  (raylib:vector2-y (sprite-pos sprite)))
(defmethod max-y ((sprite sprite))
  (+ 15 (raylib:vector2-y (sprite-pos sprite))))

(defun draw-sprite (sprite)
  (raylib:draw-texture-rec (sprite-texture sprite) (sprite-bbox sprite) (sprite-pos sprite) raylib:+white+))

;; --- Movement --- ;;

;; TODO: 2024-10-19 Use ints?
(defun move-fighter (fighter)
  "Move the fighter depending on the current button presses."
  (let* ((pos (sprite-pos fighter)))
    (when (raylib:is-key-down +key-right+)
      (setf (raylib:vector2-x pos) (min 112.0 (+ 2.0 (raylib:vector2-x pos)))))
    (when (raylib:is-key-down +key-left+)
      (setf (raylib:vector2-x pos) (max -128.0 (+ -2.0 (raylib:vector2-x pos)))))
    (when (raylib:is-key-down +key-down+)
      (setf (raylib:vector2-y pos) (min 104.0 (+ 2.0 (raylib:vector2-y pos)))))
    (when (raylib:is-key-down +key-up+)
      (setf (raylib:vector2-y pos) (max -120.0 (+ -2.0 (raylib:vector2-y pos)))))))

