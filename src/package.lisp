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
(defparameter +millis-per-frame+ (/ 1000 (float +frame-rate+)))
(defparameter +world-min-x+ -128)
(defparameter +world-max-x+ +127)
(defparameter +world-min-y+ -120)
(defparameter +world-max-y+ +119)
(defparameter +nearness-radius+ 32.0
  "The minimum distance between two sprites to be considered near enough for a
collision check.")
(defparameter +fighter-spawn-x+ -8.0
  "The X coordinate of the fighter's spawn point (after death, etc.).")
(defparameter +fighter-spawn-y+ (float (- +world-max-y+ 24))
  "The Y coordinate of the fighter's spawn point.")

;; --- Keys --- ;;

(defparameter +key-right+ 262)
(defparameter +key-left+  263)
(defparameter +key-down+  264)
(defparameter +key-up+    265)
(defparameter +key-space+ 32)

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

(defgeneric pos (sprite)
  (:documentation "The location of a sprite as a `raylib:vector2'."))

(defgeneric bbox (sprite)
  (:documentation "A `raylib:rectangle' that represents the bounding box of a sprite."))

(defgeneric min-x (sprite)
  (:documentation "The lowest (closest to 0) X value occupied by this sprite."))
(defgeneric max-x (sprite)
  (:documentation "The highest (furthest to 0) X value occupied by this sprite."))
(defgeneric min-y (sprite)
  (:documentation "The lowest (closest to 0) Y value occupied by this sprite."))
(defgeneric max-y (sprite)
  (:documentation "The highest (furthest to 0) Y value occupied by this sprite."))

;; --- Transducers --- ;;

(defun first-or (default)
  "Reducer: A variant of `first' that doesn't raise a condition when the
transduction is empty."
  (lambda (&optional (acc nil a-p) (input nil i-p))
    (cond ((and a-p i-p) (t:make-reduced :val input))
          ((and a-p (not i-p)) acc)
          (t default))))
