(in-package :aero-fighter)

#+nil
(launch)

(defun debugging-dots ()
  (raylib:draw-pixel 0 0 raylib:+red+)
  (raylib:draw-pixel -128 -120 raylib:+red+)
  (raylib:draw-pixel -128 119 raylib:+red+)
  (raylib:draw-pixel 127 119 raylib:+red+)
  (raylib:draw-pixel 127 -120 raylib:+red+))

(defun event-loop-inner (camera fighter fc)
  (unless (raylib:window-should-close)
    (with-drawing
      (with-2d-camera camera
        (raylib:clear-background raylib:+raywhite+)
        ;; (raylib:draw-rectangle 0 0 10 10 raylib:+red+)
        (debugging-dots)
        (move-fighter fighter)
        (draw-sprite fighter))
      (raylib:draw-fps 10 10)
      (raylib:draw-text (format nil "FC: ~a" fc) 10 (- +screen-height+ 25) 20 raylib:+lightgray+))
    (incf fc)
    (event-loop-inner camera fighter fc)))

(defun event-loop (camera)
  (let ((sprite (make-fighter)))
    (event-loop-inner camera sprite 0)
    (raylib:unload-texture (sprite-texture sprite))))

(defun init-camera ()
  "Produce a 2D Camera."
  (let* ((center-x (/ +screen-width+ 2.0))
         (center-y (/ +screen-height+ 2.0))
         (offset   (raylib:make-vector2 :x center-x :y center-y))
         (target   (raylib:make-vector2 :x 0.0 :y 0.0)))
    (raylib:make-camera-2d :offset offset :target target :rotation 0.0 :zoom 3.0)))

(defun launch ()
  "Launch the game."
  (raylib:init-window +screen-width+ +screen-height+ "raylib/CL Example")
  (raylib:set-target-fps 60)
  (event-loop (init-camera))
  (raylib:close-window))
