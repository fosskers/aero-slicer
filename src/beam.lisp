(in-package :aero-fighter)

;; --- Types --- ;;

(defstruct beam
  "The beam shot from the ship or tank."
  (animated  nil :type animated)
  (pos       nil :type raylib:vector2)
  (bbox      nil :type raylib:rectangle)
  (shooting? nil :type symbol)
  ;; The frame on which the shot was started.
  (shot-fc   0   :type fixnum)
  ;; The total duration, in frames, that the shot should be active for.
  (shot-dur  0   :type fixnum :read-only t))

(defun shot-duration (sprite)
  "How long is the duration of the shot?"
  (t:transduce (t:comp (t:map #'cdr)
                       (t:map #'animation-frames)
                       #'t:concatenate
                       (t:map #'frame-duration-fs))
               #'+ (sprite-animations sprite)))

;; --- Status --- ;;

(defun update-beam-status! (beam fc)
  "Turn the beam off, etc., depending on how much time has passed."
  (cond ((and (beam-shooting? beam)
              (> (- fc (beam-shot-fc beam))
                 (beam-shot-dur beam)))
         (setf (beam-shooting? beam) nil))))

(defun shoot-beam! (beam fc)
  "Fire away!"
  (setf (beam-shooting? beam) t)
  (setf (beam-shot-fc beam) fc))

;; --- Generics --- ;;

(defmethod draw ((beam beam) fc)
  (draw-animated (beam-animated beam) (beam-pos beam) fc))

(defmethod pos ((beam beam))
  (beam-pos beam))

(defmethod bbox ((beam beam))
  (beam-bbox beam))
