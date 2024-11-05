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
(defparameter +warp-cooldown+ (* 2 +frame-rate+)
  "How soon the player can warp the fighter again.")
(defparameter +warp-distance+ 32
  "The pixel distance moved by the fighter during a warp.")
(defparameter +beam-x-offset+ 6
  "The X position of the beam relative to the fighter.")
(defparameter +beam-y-offset+ -224
  "The Y position of the beam relative to the fighter.")
(defparameter +tank-beam-x-offset+ 4
  "The X position of the beam relative to a tank.")
(defparameter +tank-beam-y-offset+ 15
  "The Y position of the beam relative to a tank.")

;; --- Keys --- ;;

(defparameter +key-right+ #.(cffi:foreign-enum-value 'raylib:keyboard-key :right))
(defparameter +key-left+  #.(cffi:foreign-enum-value 'raylib:keyboard-key :left))
(defparameter +key-down+  #.(cffi:foreign-enum-value 'raylib:keyboard-key :down))
(defparameter +key-up+    #.(cffi:foreign-enum-value 'raylib:keyboard-key :up))
(defparameter +key-space+ #.(cffi:foreign-enum-value 'raylib:keyboard-key :space))
(defparameter +key-tab+   #.(cffi:foreign-enum-value 'raylib:keyboard-key :tab))

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

(defgeneric move! (sprite)
  (:documentation "Attempt to move the `sprite' on the screen."))

(defgeneric pos (sprite)
  (:documentation "The location of a sprite as a `raylib:vector2'."))

(defgeneric bbox (sprite)
  (:documentation "A `raylib:rectangle' that represents the bounding box of a sprite."))

(defgeneric draw (entity fc)
  (:documentation "Anything that can be drawn to the screen."))

(defmethod draw ((entities hash-table) fc)
  "Multiple drawable things collected into a `hash-table'."
  (with-hash-table-iterator (iter entities)
    (labels ((recurse ()
               (multiple-value-bind (entry? key entity) (iter)
                 (declare (ignore key))
                 (when entry?
                   (draw entity fc)
                   (recurse)))))
      (recurse))))

(defgeneric health (entity)
  (:documentation "The amount of health the unit has."))

(defgeneric damage! (entity)
  (:documentation "Mutably apply damage to some entity."))

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
