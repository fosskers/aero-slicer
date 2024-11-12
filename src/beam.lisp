(in-package :aero-fighter)

#+nil
(launch)

;; --- Types --- ;;

(defstruct beam
  "The beam shot from the ship or tank."
  (animated  nil :type animated)
  (pos       nil :type raylib:vector2)
  (bbox      nil :type raylib:rectangle)
  (x-offset  0.0 :type single-float)
  (shooting? nil :type symbol)
  ;; The frame on which the shot was started.
  (shot-fc   0   :type fixnum)
  ;; The total duration, in frames, that the shot should be active for.
  (shot-dur  0   :type fixnum :read-only t))

(defun beam (sprite fighter-pos)
  "Construct the narrowest beam."
  (let* ((animated (make-animated :sprite sprite :default 'shooting :active 'shooting))
         (rect     (bounding-box animated))
         (x-offset (- 8 (/ (raylib:rectangle-width rect) 2)))
         (x        (+ x-offset (raylib:vector2-x fighter-pos)))
         (y        (+ +beam-y-offset+ (raylib:vector2-y fighter-pos))))
    (make-beam :animated animated
               :pos (raylib:make-vector2 :x x :y y)
               :bbox (raylib:make-rectangle :x x
                                            :y y
                                            :width (raylib:rectangle-width rect)
                                            :height (raylib:rectangle-height rect))
               :x-offset x-offset
               :shot-dur (sprite-duration (animated-sprite animated)))))

;; --- Status --- ;;

(defun shoot-beam! (beam fc)
  "Fire away!"
  (setf (beam-shooting? beam) t)
  (setf (beam-shot-fc beam) fc))

;; --- Generics --- ;;

(defmethod tick! ((beam beam) fc)
  "Turn the beam off, etc., depending on how much time has passed."
  (cond ((and (beam-shooting? beam)
              (> (- fc (beam-shot-fc beam))
                 (beam-shot-dur beam)))
         (setf (animated-frame (beam-animated beam)) 0)
         (setf (beam-shooting? beam) nil))))

(defmethod draw ((beam beam) fc)
  (draw-animated (beam-animated beam) (beam-pos beam) fc))

(defmethod pos ((beam beam))
  (beam-pos beam))

(defmethod bbox ((beam beam))
  (beam-bbox beam))
