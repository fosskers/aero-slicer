(defpackage raylib
  (:use :cl)
  (:local-nicknames (#:tg #:trivial-garbage))
  ;; --- Types --- ;;
  (:export #:vector2 #:make-vector2 #:vector2-x #:vector2-y
           #:rectangle #:make-rectangle #:rectangle-x #:rectangle-y #:rectangle-width #:rectangle-height
           #:color #:make-color #:color-alpha
           #:texture #:texture-width #:texture-height
           #:audio-stream #:sound
           #:music #:music-looping
           #:camera-2d #:make-camera-2d
           #:keyboard-key #:gamepad-button)
  ;; --- Functions --- ;;
  (:export #:init-window #:close-window
           #:init-audio-device #:close-audio-device
           #:set-target-fps #:window-should-close
           #:begin-drawing #:end-drawing
           #:begin-mode-2d #:end-mode-2d
           #:clear-background #:draw-fps #:draw-text #:draw-circle #:draw-rectangle
           #:load-texture #:unload-texture #:is-texture-valid #:draw-texture #:draw-texture-v #:draw-texture-rec
           #:load-sound #:unload-sound #:play-sound
           #:load-music-stream #:unload-music-stream #:is-music-stream-playing #:play-music-stream #:update-music-stream
           #:is-key-pressed #:is-key-down
           #:is-gamepad-button-pressed #:is-gamepad-button-down #:get-gamepad-name #:is-gamepad-available
           #:check-collision-recs)
  (:documentation "A light wrapping of necessary Raylib types and functions; ECL-specific."))

(in-package :raylib)

;; TODO: 2024-12-25 Probably need an `eval-when' here.
(ffi:load-foreign-library #p"/home/colin/code/common-lisp/aero-fighter/vendored/raylib/src/libraylib.so")
(ffi:load-foreign-library #p"/home/colin/code/common-lisp/aero-fighter/c/shim.so")

(ffi:clines "#include \"shim.h\"")

;; --- Vectors --- ;;

;; ffi:get-slot-value

(ffi:def-struct vector2-raw
    (x :float)
  (y :float))

;; TODO: 2024-01-02 Split the files so that the shared libs / headers are pulled in first?
(ffi:def-function ("_MakeVector2" make-vector2-raw)
    ((x :float)
     (y :float))
  :returning (* vector2-raw))

#++
(ffi:get-slot-value (make-vector2-raw 1.0 2.0) 'vector2-raw 'x)

(defstruct (vector2 (:constructor @vector2))
  (pointer nil :type si:foreign-data))

(defun make-vector2 (&key x y)
  (let* ((ptr (make-vector2-raw x y))
         (v   (@vector2 :pointer ptr)))
    (tg:finalize v (lambda () (ffi:free-foreign-object ptr)))))

#++
(make-vector2 :x 1.0 :y 2.0)

#++
(defmacro vector2-x (v)
  "The X slot of a `Vector2'."
  `(ffi:get-slot-value (vector2-pointer ,v) 'vector2-raw 'x))

#++
(defmacro vector2-y (v)
  "The Y slot of a `Vector2'."
  `(slot (vector2-pointer ,v) 'y))

#++
(let ((v (make-vector2 :x 1.0 :y 2.0)))
  (setf (vector2-x v) 1000.0)
  (vector2-x v))
