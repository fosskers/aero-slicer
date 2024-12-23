(in-package :aero-fighter)

#+nil
(launch)

;; --- Road --- ;;

(defstruct road
  (texture nil :type raylib:texture)
  (pos     nil :type raylib:vector2)
  (bbox    nil :type raylib:rectangle))

(defun @road (texture &key (y +world-min-y+))
  "Smart constructor for a `road' tile."
  (let ((pos (raylib:make-vector2 :x -24.0 :y (float y))))
    (make-road :texture texture
               :pos pos
               :bbox (raylib:make-rectangle :x (raylib:vector2-x pos)
                                            :y (raylib:vector2-y pos)
                                            :width (float (raylib:texture-width texture))
                                            :height (float (raylib:texture-height texture))))))

(defun entire-road (texture)
  "Construct enough `road' to cover the center of the screen."
  (t:transduce (t:comp (t:take 16)
                       (t:map (lambda (n)
                                (cons n (@road texture :y (+ (- +world-min-y+ 16)
                                                             (* n 16)))))))
               #'t:hash-table (t:ints 0)))

(defmethod move! ((road road))
  (incf (->> road road-pos raylib:vector2-y) +slowest-scroll-rate+)
  (incf (->> road road-bbox raylib:rectangle-y) +slowest-scroll-rate+)
  (when (= (->> road road-pos raylib:vector2-y) (1+ +world-max-y+))
    (setf (->> road road-pos raylib:vector2-y) (float (- +world-min-y+ 16)))
    (setf (->> road road-bbox raylib:rectangle-y) (float (- +world-min-y+ 16)))))

(defmethod draw ((road road) fc)
  (declare (ignore fc))
  (raylib:draw-texture-v (road-texture road) (road-pos road) +white+))

(defmethod pos ((road road))
  (road-pos road))

(defmethod bbox ((road road))
  (road-bbox road))

;; --- Ground --- ;;

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
  (t:transduce (t:comp (t:take 16)
                       (t:map (lambda (n)
                                (cons n (@ground texture :y (+ (- +world-min-y+ 16)
                                                               (* n 16)))))))
               #'t:hash-table (t:ints 0)))

(defmethod move! ((ground ground))
  "A gradual scroll down the screen."
  (incf (->> ground ground-pos raylib:vector2-y) +slowest-scroll-rate+)
  (when (= (->> ground ground-pos raylib:vector2-y) (1+ +world-max-y+))
    (setf (->> ground ground-pos raylib:vector2-y) (float (- +world-min-y+ 16)))))

(defmethod draw ((ground ground) fc)
  (declare (ignore fc))
  (dotimes (n 16)
    (raylib:draw-texture-v (ground-texture ground)
                           (ground-pos ground)
                           +white+)
    (incf (->> ground ground-pos raylib:vector2-x) 16))
  (setf (->> ground ground-pos raylib:vector2-x) (float +world-min-x+)))
