(in-package :aero-fighter)

#+nil
(launch)

;; --- Types --- ;;

(defstruct fighter
  "The player's fighter ship."
  (animated nil :type animated)
  (pos      nil :type raylib:vector2))

(defun fighter (sprite)
  "A smart-constructor for `fighter'."
  (make-fighter :animated (animated :sprite sprite)
                :pos (raylib:make-vector2 :x 0.0 :y 0.0)))

;; --- Generics --- ;;

(defmethod min-x ((fighter fighter))
  (raylib:vector2-x (fighter-pos fighter)))
(defmethod max-x ((fighter fighter))
  (+ 15 (raylib:vector2-x (fighter-pos fighter))))
(defmethod min-y ((fighter fighter))
  (raylib:vector2-y (fighter-pos fighter)))
(defmethod max-y ((fighter fighter))
  (+ 15 (raylib:vector2-y (fighter-pos fighter))))

(defmethod draw ((fighter fighter))
  (raylib:draw-texture-v (sprite-texture (animated-sprite (fighter-animated fighter)))
                         (fighter-pos fighter)
                         raylib:+white+))

(defmethod move ((fighter fighter))
  "Move the fighter depending on the current button presses."
  (let* ((pos (fighter-pos fighter)))
    (when (raylib:is-key-down +key-right+)
      (setf (raylib:vector2-x pos) (min +112.0 (+ +2.0 (raylib:vector2-x pos)))))
    (when (raylib:is-key-down +key-left+)
      (setf (raylib:vector2-x pos) (max -128.0 (+ -2.0 (raylib:vector2-x pos)))))
    (when (raylib:is-key-down +key-down+)
      (setf (raylib:vector2-y pos) (min +104.0 (+ +2.0 (raylib:vector2-y pos)))))
    (when (raylib:is-key-down +key-up+)
      (setf (raylib:vector2-y pos) (max -120.0 (+ -2.0 (raylib:vector2-y pos)))))))
