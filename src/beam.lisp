(in-package :aero-fighter)

#+nil
(launch)

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

;; TODO: 2024-11-09 Beam offset should depend on beam width.
(defun beam-1 (sprite)
  "Construct the narrowest beam."
  (let* ((animated (make-animated :sprite sprite :default 'shooting :active 'shooting))
         (rect     (bounding-box animated)))
    (make-beam :animated animated
               :pos (raylib:make-vector2 :x (+ +beam-x-offset+ +fighter-spawn-x+)
                                         :y (+ +beam-y-offset+ +fighter-spawn-y+))
               :bbox (raylib:make-rectangle :x (+ +beam-x-offset+ +fighter-spawn-x+)
                                            :y (+ +beam-y-offset+ +fighter-spawn-y+)
                                            :width (raylib:rectangle-width rect)
                                            :height (raylib:rectangle-height rect))
               :shot-dur (shot-duration (animated-sprite animated)))))

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
         (setf (beam-shooting? beam) nil))))

(defmethod draw ((beam beam) fc)
  (draw-animated (beam-animated beam) (beam-pos beam) fc))

(defmethod pos ((beam beam))
  (beam-pos beam))

(defmethod bbox ((beam beam))
  (beam-bbox beam))
