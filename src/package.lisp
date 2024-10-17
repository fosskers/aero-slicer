(defpackage aero-fighter
  (:use :cl))

(in-package :aero-fighter)

(defparameter +screen-width+ 800)
(defparameter +screen-height+ 450)

(defun event-loop ()
  (unless (raylib:window-should-close)
    (raylib:begin-drawing)
    (raylib:clear-background raylib:+raywhite+)
    (raylib:draw-text "Good morning!" 190 200 20 raylib:+lightgray+)
    (raylib:end-drawing)
    (event-loop)))

(defun launch ()
  "Launch the game."
  (raylib:init-window +screen-width+ +screen-height+ "raylib/CL Example")
  (raylib:set-target-fps 60)
  (event-loop)
  (raylib:close-window))

#+nil
(launch)
