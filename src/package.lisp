(defpackage aero-fighter
  (:use :cl)
  (:local-nicknames (#:jzon #:com.inuoe.jzon)
                    (#:p #:filepaths)
                    (#:t #:transducers)))

(in-package :aero-fighter)

#+nil
(launch)

;; --- Globals --- ;;

(defparameter +world-pixels-x+ 256)
(defparameter +world-pixels-y+ 240)
(defparameter +screen-width+ (* +world-pixels-x+ 3))
(defparameter +screen-height+ (* +world-pixels-y+ 3))
(defparameter +frame-rate+ 60)
(defparameter +world-min-x+ -128)
(defparameter +world-max-x+ +127)
(defparameter +world-min-y+ -120)
(defparameter +world-max-y+ +119)

;; --- Keys --- ;;

(defparameter +key-right+ 262)
(defparameter +key-left+  263)
(defparameter +key-down+  264)
(defparameter +key-up+    265)

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

(defgeneric pos (sprite)
  (:documentation "The location of a sprite as a `Vector2'."))

(defgeneric min-x (sprite)
  (:documentation "The lowest (closest to 0) X value occupied by this sprite."))
(defgeneric max-x (sprite)
  (:documentation "The highest (furthest to 0) X value occupied by this sprite."))
(defgeneric min-y (sprite)
  (:documentation "The lowest (closest to 0) Y value occupied by this sprite."))
(defgeneric max-y (sprite)
  (:documentation "The highest (furthest to 0) Y value occupied by this sprite."))
