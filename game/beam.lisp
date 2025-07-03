;;; Beams. Used by the fighter, tanks, and the evil fighter.
;;;
;;; A single beam animation should last between 400ms and 464ms. At minimum a
;;; single animation frame can last 1 screen frame (16ms). Thus, 25 to 29 screen
;;; frames are available for each animation.
;;;
;;; Roughly: 8 frames (128ms) for the expansion, 9 frames (144ms) at peak, 8
;;; frames to wind down.

(in-package :aero-slicer)

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

(defun @beam (sprite parent-pos parent-width y-offset)
  "Construct a beam."
  (let* ((animated (make-animated :sprite sprite :default :shooting :active :shooting))
         (rect     (bounding-box animated))
         (x-offset (- (/ parent-width 2)
                      (/ (raylib:rectangle-width rect) 2)))
         (x        (+ x-offset (raylib:vector2-x parent-pos)))
         (y        (+ y-offset (raylib:vector2-y parent-pos))))
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
  (setf (beam-shot-fc beam) fc)
  ;; While it is redundant to tell the `animated' what the active animation is
  ;; here (since there is only one), calling `set-animation!' handles setting
  ;; the frame values in a consistent way. Previously it was more ad-hoc.
  (set-animation! (beam-animated beam) :shooting fc))

;; --- Generics --- ;;

(defmethod tick! ((beam beam) fc)
  "Turn the beam off, etc., depending on how much time has passed."
  (cond ((and (beam-shooting? beam)
              (>= (- fc (beam-shot-fc beam))
                  (beam-shot-dur beam)))
         (setf (beam-shooting? beam) nil))))

(defmethod draw ((beam beam) fc)
  (draw-animated (beam-animated beam) (beam-pos beam) fc :colour +faded-white+))

(defmethod pos ((beam beam))
  (beam-pos beam))

(defmethod bbox ((beam beam))
  (beam-bbox beam))
