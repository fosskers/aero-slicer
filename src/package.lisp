(defpackage aero-fighter
  (:use :cl))

(in-package :aero-fighter)

#+nil
(launch)

;; --- Globals --- ;;

(defparameter +screen-width+ (* 256 3))
(defparameter +screen-height+ (* 240 3))
(defparameter +frame-rate+ 60)

;; --- Keys --- ;;

(defparameter +key-right+ 262)
(defparameter +key-left+ 263)
(defparameter +key-down+ 264)
(defparameter +key-up+ 265)

;; --- Macros --- ;;

(defmacro with-drawing (&body body)
  `(progn (raylib:begin-drawing)
          ,@body
          (raylib:end-drawing)))

(defmacro with-2d-camera (camera &body body)
  `(progn (raylib:begin-mode-2d ,camera)
          ,@body
          (raylib:end-mode-2d)))

;; --- Generics --- ;;

(defgeneric move (sprite)
  (:documentation "Attempt to move the `sprite' on the screen."))

(defgeneric draw (sprite)
  (:documentation "Render the `sprite' on the screen."))

(defgeneric min-x (sprite)
  (:documentation "The lowest (closest to 0) X value occupied by this sprite."))
(defgeneric max-x (sprite)
  (:documentation "The highest (furthest to 0) X value occupied by this sprite."))
(defgeneric min-y (sprite)
  (:documentation "The lowest (closest to 0) Y value occupied by this sprite."))
(defgeneric max-y (sprite)
  (:documentation "The highest (furthest to 0) Y value occupied by this sprite."))
