(defpackage aero-fighter
  (:use :cl :arrow-macros)
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
(defparameter +warp-distance+ 48
  "The pixel distance moved by the fighter during a warp.")
(defparameter +bomb-cooldown+ (* 2 +frame-rate+)
  "How soon the player can use another bomb.")
(defparameter +powerup-newness-timeout+ (* 4 +frame-rate+)
  "After which the powerup should start flashing.")
(defparameter +powerup-spawn-timeout+ (* 6 +frame-rate+)
  "After which the powerup should despawn entirely.")
(defparameter +bomb-max-capacity+ 3
  "The max number of bombs that the fighter has hold.")
(defparameter +beam-y-offset+ -224
  "The Y position of the beam relative to the fighter. Note that the Y offset is
fixed for all beam types as they are all the same height, but the X offset is
programmatic based on each beam's width.")
(defparameter +tank-beam-y-offset+ 13
  "The Y position of the beam relative to a tank.")
(defparameter +tank-base-hp+ 1
  "The amount of max HP of the tank to which the Level is added.")
(defparameter +blob-base-hp+ 0
  "The amount of max HP of the blob to which the Level is added.")
(defparameter +evil-ship-base-hp+ 2
  "The amount of max HP of the evil fighter to which the Level is added.")
(defparameter +evil-ship-beam-y-offset+ 13
  "The Y position of the beam relative to a evil ship.")
(defparameter +enemy-invincibility-frames+ (/ +frame-rate+ 2)
  "The same length as the duration of the fighter's beam. This ensures that a
single beam shot will never damage the same enemy twice.")
(defparameter +level-progression-interval+ 5000)

;; --- Fixed Positions --- ;;

(defparameter +icon-y+   (+ 2 +world-min-y+))
(defparameter +mini-icon-y+ (+ 7 +world-min-y+))
(defparameter +life-1-x+ (+ 6 +world-min-x+))
(defparameter +life-2-x+ (+ 18 +world-min-x+))
(defparameter +life-3-x+ (+ 30 +world-min-x+))
(defparameter +ammo-1-x+ (+ 218 +world-min-x+))
(defparameter +ammo-2-x+ (+ 230 +world-min-x+))
(defparameter +ammo-3-x+ (+ 242 +world-min-x+))
(defparameter +beam-1-x+ (+ 98 +world-min-x+))
(defparameter +beam-2-x+ (+ 106 +world-min-x+))
(defparameter +beam-3-x+ (+ 114 +world-min-x+))
(defparameter +beam-4-x+ (+ 122 +world-min-x+))
(defparameter +beam-5-x+ (+ 130 +world-min-x+))
(defparameter +beam-6-x+ (+ 138 +world-min-x+))
(defparameter +beam-7-x+ (+ 146 +world-min-x+))
(defparameter +beam-8-x+ (+ 154 +world-min-x+))
(defparameter +score-y+  (float (+ 2 +world-min-y+)))
(defparameter +score-x+  (float (+ 153 +world-min-x+)))
(defparameter *score-pos* (raylib:make-vector2 :x +score-x+ :y +score-y+))
(defparameter +level-x+  (+ 100 +world-min-x+))
(defparameter +level-y+  (+ 2 +world-min-y+))

;; --- Colours --- ;;

(defparameter +faded-white+ (raylib:color-alpha raylib:+white+ 0.85))
(defparameter +very-faded-white+ (raylib:color-alpha raylib:+white+ 0.5))
(defparameter +very-faded-blue+ (raylib:color-alpha raylib:+blue+ 0.35))

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
(raylib:get-gamepad-name 2)

;; NOTE: See also `debugging-gamepad' in `main.lisp'.
(defparameter +gamepad+ 2)
(defparameter +gamepad-a+ #.(cffi:foreign-enum-value 'raylib:gamepad-button :right-face-right))
(defparameter +gamepad-b+ #.(cffi:foreign-enum-value 'raylib:gamepad-button :right-face-down))
(defparameter +gamepad-up+    #.(cffi:foreign-enum-value 'raylib:gamepad-button :left-face-up))
(defparameter +gamepad-down+  #.(cffi:foreign-enum-value 'raylib:gamepad-button :left-face-down))
(defparameter +gamepad-left+  #.(cffi:foreign-enum-value 'raylib:gamepad-button :left-face-left))
(defparameter +gamepad-right+ #.(cffi:foreign-enum-value 'raylib:gamepad-button :left-face-right))
(defparameter +gamepad-start+ #.(cffi:foreign-enum-value 'raylib:gamepad-button :middle-right))
(defparameter +gamepad-left-shoulder+ #.(cffi:foreign-enum-value 'raylib:gamepad-button :left-trigger-1))
(defparameter +gamepad-right-shoulder+ #.(cffi:foreign-enum-value 'raylib:gamepad-button :right-trigger-1))

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

(defgeneric tick! (entity fc)
  (:documentation "Update some internal timer-based state of an `entity'."))

(defmethod tick! ((entities hash-table) fc)
  "Tick multiple objects collected into a Hash Table."
  (t:transduce (t:map (lambda (entity) (tick! (cdr entity) fc))) #'t:for-each entities))

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

(defgeneric damage! (entity fc)
  (:documentation "Mutably apply damage to some entity."))

(defgeneric vulnerable? (entity fc)
  (:documentation "Can a given entity accept damage right now?"))

(defgeneric expired? (entity fc)
  (:documentation "Has too much timed passed since an entity spawned?"))

(defgeneric points (entity)
  (:documentation "The number of points yielded by an enemy upon death."))

(defgeneric min-x (sprite)
  (:documentation "The lowest (closest to 0) X value occupied by this sprite."))
(defgeneric max-x (sprite)
  (:documentation "The highest (furthest to 0) X value occupied by this sprite."))
(defgeneric min-y (sprite)
  (:documentation "The lowest (closest to 0) Y value occupied by this sprite."))
(defgeneric max-y (sprite)
  (:documentation "The highest (furthest to 0) Y value occupied by this sprite."))

;; --- Utilities --- ;;

(defun despawn! (entities fc)
  "Despawn old entities/animations if enough time has passed."
  (t:transduce (t:comp (t:filter (lambda (pu) (expired? (cdr pu) fc)))
                       (t:map (lambda (pu) (remhash (car pu) entities))))
               #'t:for-each entities))

(defun random-position ()
  "A random position within the visible area of the world. Useful for spawning
powerups."
  (let ((y-spread (- (- +world-max-y+ 16)
                     (+ +world-min-y+ 16))))
    (raylib:make-vector2 :x (float (max +world-min-x+ (- (random +world-pixels-x+) +world-max-x+ 16)))
                         :y (float (- (random y-spread)
                                      (/ y-spread 2))))))

#+nil
(random-position)

;; NOTE: Borrowed from Alexandria, which is Public Domain.
(defmacro when-let* (bindings &body body)
  "Creates new variable bindings, and conditionally executes BODY.

BINDINGS must be either single binding of the form:

 (variable initial-form)

or a list of bindings of the form:

 ((variable-1 initial-form-1)
  (variable-2 initial-form-2)
  ...
  (variable-n initial-form-n))

Each INITIAL-FORM is executed in turn, and the variable bound to the
corresponding value. INITIAL-FORM expressions can refer to variables
previously bound by the WHEN-LET*.

Execution of WHEN-LET* stops immediately if any INITIAL-FORM evaluates to NIL.
If all INITIAL-FORMs evaluate to true, then BODY is executed as an implicit
PROGN."
  (let ((binding-list (if (and (consp bindings) (symbolp (car bindings)))
                          (list bindings)
                          bindings)))
    (labels ((bind (bindings body)
               (if bindings
                   `(let (,(car bindings))
                      (when ,(caar bindings)
                        ,(bind (cdr bindings) body)))
                   `(progn ,@body))))
      (bind binding-list body))))
