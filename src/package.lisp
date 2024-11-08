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
(defparameter +bomb-cooldown+ (* 2 +frame-rate+)
  "How soon the player can use another bomb.")
(defparameter +bomb-ammo-spawn-timeout+ (* 5 +frame-rate+))
(defparameter +bomb-max-capacity+ 3
  "The max number of bombs that the fighter has hold.")
(defparameter +beam-x-offset+ 6
  "The X position of the beam relative to the fighter.")
(defparameter +beam-y-offset+ -224
  "The Y position of the beam relative to the fighter.")
(defparameter +tank-beam-x-offset+ 5
  "The X position of the beam relative to a tank.")
(defparameter +tank-beam-y-offset+ 14
  "The Y position of the beam relative to a tank.")
(defparameter +blob-points+ 100)
(defparameter +tank-points+ 200)

;; --- Keys --- ;;

(defparameter +key-right+ #.(cffi:foreign-enum-value 'raylib:keyboard-key :right))
(defparameter +key-left+  #.(cffi:foreign-enum-value 'raylib:keyboard-key :left))
(defparameter +key-down+  #.(cffi:foreign-enum-value 'raylib:keyboard-key :down))
(defparameter +key-up+    #.(cffi:foreign-enum-value 'raylib:keyboard-key :up))
(defparameter +key-space+ #.(cffi:foreign-enum-value 'raylib:keyboard-key :space))
(defparameter +key-tab+   #.(cffi:foreign-enum-value 'raylib:keyboard-key :tab))
(defparameter +key-enter+ #.(cffi:foreign-enum-value 'raylib:keyboard-key :enter))

;; --- Gamepad --- ;;

#+nil
(raylib:is-gamepad-available 0)
#+nil
(raylib:get-gamepad-name 0)

(defparameter +gamepad+ 0)
(defparameter +gamepad-a+ #.(cffi:foreign-enum-value 'raylib:gamepad-button :right-face-right))
(defparameter +gamepad-b+ #.(cffi:foreign-enum-value 'raylib:gamepad-button :right-face-down))
(defparameter +gamepad-up+    #.(cffi:foreign-enum-value 'raylib:gamepad-button :left-face-up))
(defparameter +gamepad-down+  #.(cffi:foreign-enum-value 'raylib:gamepad-button :left-face-down))
(defparameter +gamepad-left+  #.(cffi:foreign-enum-value 'raylib:gamepad-button :left-face-left))
(defparameter +gamepad-right+ #.(cffi:foreign-enum-value 'raylib:gamepad-button :left-face-right))

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

(defgeneric expired? (powerup fc)
  (:documentation "Has too much timed passed since the powerup spawned?"))

(defgeneric min-x (sprite)
  (:documentation "The lowest (closest to 0) X value occupied by this sprite."))
(defgeneric max-x (sprite)
  (:documentation "The highest (furthest to 0) X value occupied by this sprite."))
(defgeneric min-y (sprite)
  (:documentation "The lowest (closest to 0) Y value occupied by this sprite."))
(defgeneric max-y (sprite)
  (:documentation "The highest (furthest to 0) Y value occupied by this sprite."))

;; --- Utilities --- ;;

(defun random-position ()
  "A random position within the visible area of the world. Useful for spawning
powerups."
  (raylib:make-vector2 :x (float (- (random +world-pixels-x+) +world-max-x+))
                       :y (float (- (random +world-pixels-y+) +world-max-y+))))

#+nil
(random-position)

