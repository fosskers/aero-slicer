(defpackage aero-fighter
  (:use :cl))

(in-package :aero-fighter)

(defparameter +screen-width+ (* 256 3))
(defparameter +screen-height+ (* 240 3))

(defun event-loop ()
  (let* ((sprite (raylib:load-texture "/home/colin/code/common-lisp/save-the-farm/data/sprites/farmer.png"))
         (pos    (raylib:make-vector2 :x 0.0 :y 0.0))
         (bbox   (raylib:make-rectangle :x 0.0 :y 0.0
                                        :width (/ (raylib:texture-width sprite) 7.0)
                                        :height (float (raylib:texture-height sprite))))
         (center-x (/ +screen-width+ 2.0))
         (center-y (/ +screen-height+ 2.0))
         (offset   (raylib:make-vector2 :x center-x :y center-y))
         (target   (raylib:make-vector2 :x 0.0 :y 0.0))
         (camera   (raylib:make-camera-2d :offset offset :target target :rotation 0.0 :zoom 3.0)))
    (labels ((recurse ()
               (unless (raylib:window-should-close)
                 (raylib:begin-drawing)
                 (raylib:begin-mode-2d camera)
                 (raylib:clear-background raylib:+raywhite+)
                 (raylib:draw-rectangle 0 0 10 10 raylib:+red+)
                 (raylib:draw-pixel 0 0 raylib:+blue+)
                 (raylib:draw-texture-rec sprite bbox pos raylib:+white+)
                 (raylib:end-mode-2d)
                 (raylib:end-drawing)
                 (recurse))))
      (recurse)
      (raylib:unload-texture sprite))))

(defun launch ()
  "Launch the game."
  (raylib:init-window +screen-width+ +screen-height+ "raylib/CL Example")
  (raylib:set-target-fps 60)
  (event-loop)
  (raylib:close-window))

#+nil
(launch)
