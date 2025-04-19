(defpackage aero-fighter
  (:use :cl :arrow-macros)
  (:shadow #:shadow)
  (:local-nicknames (#:json #:parcom/json)
                    (#:p #:filepaths)
                    (#:t #:transducers))
  (:export #:launch))

(in-package :aero-fighter)

#-(or sbcl ecl)
(error "Aero Fighter can only be compiled with SBCL or ECL.")

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
(defparameter +slowest-scroll-rate+ 1.0
  "The rate at which the slowest objects move down the screen.")
(defparameter +blob-y-speed+ 1.5)
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
(defparameter +powerup-spawn-interval+ 1000
  "How often, in points, a beam/shield powerup should spawn.")
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
(defparameter +shadow-offset+ 16)
(defparameter +tank-beam-y-offset+ 13
  "The Y position of the beam relative to a tank.")
(defparameter +evil-ship-paranoia-radius+ 128
  "How far away from you the evil ship wants to stay.")
(defparameter +evil-ship-beam-y-offset+ 13
  "The Y position of the beam relative to a evil ship.")
(defparameter +enemy-invincibility-frames+ (/ +frame-rate+ 2)
  "The same length as the duration of the fighter's beam. This ensures that a
single beam shot will never damage the same enemy twice.")
(defparameter +level-progression-interval+ 5000
  "How often, in points, that the difficulty level should increase.")

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
(defparameter +level-x+  (+ 100 +world-min-x+))
(defparameter +level-y+  (+ 2 +world-min-y+))
(defparameter +logo-lower-bound+ (+ +world-min-y+ 32))
(defparameter +curr-pad-x+ (float (- +world-max-x+ 38)))
(defparameter +curr-pad-y+ (float (- +world-max-y+ 9)))

;; --- Colours --- ;;

;; NOTE: 2024-12-21 These can't be global variables, as they store a `cobject',
;; which can't be saved into the Lisp image's heap when creating an
;; "executable".
;;
;; 2024-12-23 Furthermore, even referencing preallocated `cobject's like
;; `raylib:+blue+' was causing a memory error within a built Lisp image
;; executable under SBCL.
;;
;; 2024-12-23 (later) Actually, I did discover a little trick. If I set these to
;; `nil' initially, I can set them properly in `launch', then reference them
;; globally as usual.

(defparameter +white+ nil)
(defparameter +black+ nil)
(defparameter +red+   nil)
(defparameter +faded-white+ nil)
(defparameter +very-faded-white+ nil)
(defparameter +very-faded-blue+ nil)

(defun white ()
  (raylib:make-color :r 255 :g 255 :b 255 :a 255))

(defun black ()
  (raylib:make-color :r 0 :g 0 :b 0 :a 255))

(defun red ()
  (raylib:make-color :r 230 :g 41 :b 55 :a 255))

(defun faded-white ()
  (raylib:make-color :r 255 :g 255 :b 255 :a 216))

(defun very-faded-white ()
  (raylib:make-color :r 255 :g 255 :b 255 :a 127))

(defun very-faded-blue ()
  (raylib:make-color :r 0 :g 121 :b 241 :a 89))

;; --- Keys --- ;;

(defparameter +key-right+ #.(raylib:keyboard-key :right))
(defparameter +key-left+  #.(raylib:keyboard-key :left))
(defparameter +key-down+  #.(raylib:keyboard-key :down))
(defparameter +key-up+    #.(raylib:keyboard-key :up))
(defparameter +key-space+ #.(raylib:keyboard-key :space))
(defparameter +key-tab+   #.(raylib:keyboard-key :tab))
(defparameter +key-enter+ #.(raylib:keyboard-key :enter))

;; --- Gamepad --- ;;

#+nil
(raylib:is-gamepad-available 0)
#+nil
(raylib:get-gamepad-name 0)

;; NOTE: See also `debugging-gamepad' in `input.lisp'.
(defparameter +gamepad+ 0)
(defparameter +gamepad-a+ #.(raylib:gamepad-button :right-face-right))
(defparameter +gamepad-b+ #.(raylib:gamepad-button :right-face-down))
(defparameter +gamepad-x+ #.(raylib:gamepad-button :right-face-up))
(defparameter +gamepad-y+ #.(raylib:gamepad-button :right-face-left))
(defparameter +gamepad-up+    #.(raylib:gamepad-button :left-face-up))
(defparameter +gamepad-down+  #.(raylib:gamepad-button :left-face-down))
(defparameter +gamepad-left+  #.(raylib:gamepad-button :left-face-left))
(defparameter +gamepad-right+ #.(raylib:gamepad-button :left-face-right))
(defparameter +gamepad-start+ #.(raylib:gamepad-button :middle-right))
(defparameter +gamepad-select+ #.(raylib:gamepad-button :middle-left))
(defparameter +gamepad-left-shoulder+ #.(raylib:gamepad-button :left-trigger-1))
(defparameter +gamepad-right-shoulder+ #.(raylib:gamepad-button :right-trigger-1))

;; --- Macros --- ;;

(defmacro with-drawing (&body body)
  `(progn (raylib:begin-drawing)
          ,@body
          (raylib:end-drawing)))

(defmacro with-2d-camera (camera &body body)
  `(progn (raylib:begin-mode-2d ,camera)
          ,@body
          (raylib:end-mode-2d)))

(defmacro with-flipped-sprite (rect &body body)
  "Raylib Trivia: to flip a sprite, you must pass a `rectangle' with a negative
width to functions like `draw-texture-rect'. Since my bounding rect's are stored
within the individual sprite frames and should really not be mutated, this macro
adds some safety."
  `(let ((width (raylib:rectangle-width ,rect)))
     (setf (raylib:rectangle-width ,rect) (* -1 width))
     ,@body
     (setf (raylib:rectangle-width ,rect) width)))

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

(defmethod bbox ((rect raylib:rectangle))
  rect)

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

(defgeneric damage! (entity dmg fc)
  (:documentation "Mutably apply damage to some entity."))

(defgeneric vulnerable? (entity fc)
  (:documentation "Can a given entity accept damage right now?"))

(defgeneric expired? (entity fc)
  (:documentation "Has too much timed passed since an entity spawned?"))

(defgeneric points (entity)
  (:documentation "The number of points yielded by an enemy upon death."))

(defgeneric beam (entity)
  (:documentation "The inner `beam' belonging to this entity."))

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

(defun path->string (path)
  "Read the contents of a file into a string."
  (with-open-file (stream path :direction :input)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))

#+nil
(->> (path->string #p"assets/graphics/beam-2.json")
     (json:parse))
