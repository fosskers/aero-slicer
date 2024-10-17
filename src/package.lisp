(defpackage aero-fighter
  (:use :cl))

(in-package :aero-fighter)

(defparameter +screen-width+ (* 256 3))
(defparameter +screen-height+ (* 240 3))

(defmacro with-drawing (&body body)
  `(progn (raylib:begin-drawing)
          ,@body
          (raylib:end-drawing)))

(defmacro with-2d-camera (camera &body body)
  `(progn (raylib:begin-mode-2d ,camera)
          ,@body
          (raylib:end-mode-2d)))

(defstruct (sprite (:copier nil) (:predicate nil))
  (texture nil :read-only t)
  (pos (raylib:make-vector2 :x 0.0 :y 0.0))
  (bbox nil :read-only t))

(defun draw-sprite (sprite)
  (raylib:draw-texture-rec (sprite-texture sprite) (sprite-bbox sprite) (sprite-pos sprite) raylib:+white+))

(defun event-loop-inner (camera sprite fc)
  (unless (raylib:window-should-close)
    (with-drawing
      (with-2d-camera camera
        (raylib:clear-background raylib:+raywhite+)
        ;; (raylib:draw-rectangle 0 0 10 10 raylib:+red+)
        (raylib:draw-pixel 0 0 raylib:+red+)
        (draw-sprite sprite))
      (raylib:draw-fps 10 10)
      (raylib:draw-text (format nil "FC: ~a" fc) 10 (- +screen-height+ 25) 20 raylib:+lightgray+))
    (incf fc)
    (event-loop-inner camera sprite fc)))

(defun event-loop (camera)
  (let ((sprite (make-farmer)))
    (event-loop-inner camera sprite 0)
    (raylib:unload-texture (sprite-texture sprite))))

(defun init-camera ()
  "Produce a 2D Camera."
  (let* ((center-x (/ +screen-width+ 2.0))
         (center-y (/ +screen-height+ 2.0))
         (offset   (raylib:make-vector2 :x center-x :y center-y))
         (target   (raylib:make-vector2 :x 0.0 :y 0.0)))
    (raylib:make-camera-2d :offset offset :target target :rotation 0.0 :zoom 3.0)))

(defun make-farmer ()
  "Read in the farmer texture."
  (let* ((sprite (raylib:load-texture "/home/colin/code/common-lisp/save-the-farm/data/sprites/farmer.png"))
         (bbox   (raylib:make-rectangle :x 0.0 :y 0.0
                                        :width (/ (raylib:texture-width sprite) 7.0)
                                        :height (float (raylib:texture-height sprite)))))
    (make-sprite :texture sprite :bbox bbox)))

(defun launch ()
  "Launch the game."
  (raylib:init-window +screen-width+ +screen-height+ "raylib/CL Example")
  (raylib:set-target-fps 60)
  (event-loop (init-camera))
  (raylib:close-window))

#+nil
(launch)
