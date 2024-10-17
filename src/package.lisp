(defpackage aero-fighter
  (:use :cl))

(in-package :aero-fighter)

;; --- Globals --- ;;

(defparameter +screen-width+ (* 256 3))
(defparameter +screen-height+ (* 240 3))

;; --- Macros --- ;;

(defmacro with-drawing (&body body)
  `(progn (raylib:begin-drawing)
          ,@body
          (raylib:end-drawing)))

(defmacro with-2d-camera (camera &body body)
  `(progn (raylib:begin-mode-2d ,camera)
          ,@body
          (raylib:end-mode-2d)))

#+nil
(launch)
