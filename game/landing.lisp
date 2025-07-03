;;; Display of the logo and various options.

(in-package :aero-slicer)

#+nil
(launch)

;; --- Second Universe Logo --- ;;

(defun draw-su-logo (texture fc)
  "Draw the Second Universe Logo with fade-in, fade-out."
  (let* ((colour (if (< fc +frame-rate+)
                     (raylib:color-alpha +white+ (/ 1.0 (- (1+ +frame-rate+) fc)))
                     +white+)))
    (raylib:draw-texture texture -16 -16 colour)))

;; --- Logo --- ;;

(defstruct logo
  "The Aero Fighter logo on the landing screen."
  (texture nil :type raylib:texture)
  (pos     nil :type raylib:vector2))

(defun @logo (texture)
  "A smart constructor for a `logo'."
  (make-logo :texture texture
             :pos (raylib:make-vector2 :x (+ 16.0 +world-min-x+)
                                       :y (float +world-min-y+))))

(defmethod move! ((logo logo))
  (unless (>= (->> logo logo-pos raylib:vector2-y) +logo-lower-bound+)
    (incf (->> logo logo-pos raylib:vector2-y) 5.0)))

(defmethod draw ((logo logo) fc)
  (raylib:draw-texture-v (logo-texture logo)
                         (logo-pos logo)
                         +white+))
