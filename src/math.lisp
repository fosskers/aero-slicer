;;; Various math utilities.

(in-package :aero-fighter)

#++
(launch)

;; --- General --- ;;

(defun neg? (n)
  "Is a given number negative?"
  (< n 0))

(declaim (ftype (function (real) single-float) real->float))
(defun real->float (n)
  "Truncate a `real' to a `single-float'."
  (coerce n 'single-float))

;; --- Vectors --- ;;

(defgeneric euclidean-distance (v1 v2)
  (:documentation "The distance between two vectors."))

(defmethod euclidean-distance ((x-diff real) (y-diff real))
  "If you already had the X/Y diffs on hand."
  (sqrt (+ (expt x-diff 2)
           (expt y-diff 2))))

(defmethod euclidean-distance ((v1 raylib:vector2) (v2 raylib:vector2))
  (sqrt (+ (expt (- (raylib:vector2-x v2)
                    (raylib:vector2-x v1))
                 2)
           (expt (- (raylib:vector2-y v2)
                    (raylib:vector2-y v1))
                 2))))

#+nil
(let ((a (raylib:make-vector2 :x 2.0 :y -6.0))
      (b (raylib:make-vector2 :x 7.0 :y 3.0)))
  (euclidean-distance a b))

(defun angle (v)
  "The angle of a vector relative to the positive x-axis. Assumes that the Y-axis
is flipped, as it is for Raylib / graphics in general."
  (let ((angle (atan (- (raylib:vector2-y v))
                     (raylib:vector2-x v))))
    (if (neg? angle)
        (+ angle (* 2 pi))
        angle)))

(defun greater-angle? (v1 v2)
  "Does some V1 have a greater angle along the unit circle than a V2?"
  (let ((a1 (angle v1))
        (a2 (angle v2)))
    (< a2 a1 (+ a2 pi))))

(declaim (ftype (function (single-float single-float real) (values single-float single-float)) rotate))
(defun rotate (x y radians)
  "Rotate a vector by the given angle."
  (let ((cos-r (cos radians))
        (sin-r (sin radians)))
    (values (real->float (- (* x cos-r) (* y sin-r)))
            (real->float (+ (* x sin-r) (* y cos-r))))))

#++
(rotate 0 1 (/ pi 2))

#++
(defun between (v1 v2)
  "A new (normalised) vector between two given ones."
  (let* ((x (+ (raylib:vector2-x v1)
               (raylib:vector2-x v2)))
         (y (+ (raylib:vector2-y v1)
               (raylib:vector2-y v2)))
         (mag (magnitude x y)))
    (values (/ x mag) (/ y mag))))

#++
(between (raylib:make-vector2 :x 1.0  :y 0.0)
         (raylib:make-vector2 :x -1.0 :y 0.0))

(defun magnitude (v)
  "The length of a vector."
  (sqrt (+ (expt (raylib:vector2-x v) 2)
           (expt (raylib:vector2-y v) 2))))

(defun dot-product (u v)
  (+ (* (raylib:vector2-x u)
        (raylib:vector2-x v))
     (* (raylib:vector2-y u)
        (raylib:vector2-y v))))

(defun angle-between (u v)
  "The angle in radians between two vectors."
  (acos (min 1.0
             (/ (dot-product u v)
                (* (magnitude u)
                   (magnitude v))))))

#++
(angle-between (raylib:make-vector2 :x 1.0 :y 0.0)
               (raylib:make-vector2 :x 0.0 :y 1.0))
