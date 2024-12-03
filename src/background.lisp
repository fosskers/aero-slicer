(in-package :aero-fighter)

#+nil
(launch)

(defstruct ground
  "A row of ground tiles."
  (texture nil :type raylib:texture)
  (pos     nil :type raylib:vector2))

(defun @ground (texture &key (y +world-min-y+))
  "Smart constructor for a `ground'."
  (make-ground :texture texture
               :pos (raylib:make-vector2 :x (float +world-min-x+)
                                         :y (float y))))

(defun entire-ground (texture)
  "Construct enough `ground' to cover the screen."
  (t:transduce (t:comp (t:take 15)
                       (t:map (lambda (n) (cons n (@ground texture :y (+ +world-min-y+ (* n 16)))))))
               #'t:hash-table (t:ints 0)))

(defmethod move! ((ground ground))
  "A gradual scroll down the screen."
  (incf (->> ground ground-pos raylib:vector2-y) 0.25)
  (when (> (->> ground ground-pos raylib:vector2-y)
           +world-max-y+)
    (setf (->> ground ground-pos raylib:vector2-y) (float +world-min-y+))))

(defmethod draw ((ground ground) fc)
  (declare (ignore fc))
  (dotimes (n 16)
    (raylib:draw-texture-v (ground-texture ground)
                           (ground-pos ground)
                           raylib:+white+)
    (incf (->> ground ground-pos raylib:vector2-x) 16))
  (setf (->> ground ground-pos raylib:vector2-x) (float +world-min-x+)))
