(defpackage raylib
  (:use :cl :sb-alien)
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
           #:is-key-pressed #:is-key-down #:is-gamepad-button-pressed #:is-gamepad-button-down
           #:check-collision-recs)
  (:documentation "A light wrapping of necessary Raylib types and functions; SBCL-specific."))

(in-package :raylib)

;; TODO: 2024-12-25 Probably need an `eval-when' here.
(load-shared-object #p"/home/colin/code/common-lisp/aero-fighter/vendored/raylib/src/libraylib.so")
(load-shared-object #p"/home/colin/code/common-lisp/aero-fighter/c/shim.so")

#++
(progn
  (unload-shared-object #p"/home/colin/code/common-lisp/aero-fighter/vendored/raylib/src/libraylib.so")
  (load-shared-object #p"/home/colin/code/common-lisp/aero-fighter/vendored/raylib/src/libraylib.so"))

#++
(progn
  (unload-shared-object #p"/home/colin/code/common-lisp/aero-fighter/c/shim.so")
  (load-shared-object #p"/home/colin/code/common-lisp/aero-fighter/c/shim.so"))

;; --- Vectors --- ;;

(define-alien-type nil
    (struct vector2-raw
            (x float)
            (y float)))

(define-alien-routine ("_MakeVector2" make-vector2-raw) (* (struct vector2-raw))
  (x float)
  (y float))

(defstruct (vector2 (:constructor @vector2))
  (pointer nil :type alien))

(defun make-vector2 (&key x y)
  (let* ((ptr (make-vector2-raw x y))
         (v   (@vector2 :pointer ptr)))
    (tg:finalize v (lambda () (free-alien ptr)))))

(defmacro vector2-x (v)
  "The X slot of a `Vector2'."
  `(sb-alien:slot (vector2-pointer ,v) 'x))

(defmacro vector2-y (v)
  "The Y slot of a `Vector2'."
  `(sb-alien:slot (vector2-pointer ,v) 'y))

#++
(let ((v (make-vector2 :x 1.0 :y 2.0)))
  (setf (vector2-x v) 1000.0)
  (vector2-x v))

#++
(tg:gc :full t :verbose t)

;; --- Rectangles --- ;;

(define-alien-type nil
    (struct rectangle-raw
            (x float)
            (y float)
            (width float)
            (height float)))

(define-alien-routine ("_MakeRectangle" make-rectangle-raw) (* (struct rectangle-raw))
  (x float)
  (y float)
  (width float)
  (height float))

(defstruct (rectangle (:constructor @rectangle))
  (pointer nil :type alien))

(defun make-rectangle (&key x y width height)
  (let* ((pointer (make-rectangle-raw x y width height))
         (rect    (@rectangle :pointer pointer)))
    (tg:finalize rect (lambda () (free-alien pointer)))))

(defmacro rectangle-x (r)
  `(sb-alien:slot (rectangle-pointer ,r) 'x))

(defmacro rectangle-y (r)
  `(sb-alien:slot (rectangle-pointer ,r) 'y))

(defmacro rectangle-width (r)
  `(sb-alien:slot (rectangle-pointer ,r) 'width))

(defmacro rectangle-height (r)
  `(sb-alien:slot (rectangle-pointer ,r) 'height))

;; --- Colour --- ;;

(define-alien-type nil
    (struct color-raw
            (r unsigned-char)
            (g unsigned-char)
            (b unsigned-char)
            (a unsigned-char)))

(define-alien-routine ("_MakeColor" make-color-raw) (* (struct color-raw))
  (r unsigned-char)
  (g unsigned-char)
  (b unsigned-char)
  (a unsigned-char))

(defstruct (color (:constructor @color))
  (pointer nil :type alien))

(defun make-color (&key r g b a)
  (let* ((pointer (make-color-raw r g b a))
         (color   (@color :pointer pointer)))
    (tg:finalize color (lambda () (free-alien pointer)))))

(define-alien-routine ("_ColorAlpha" color-alpha-raw) (* (struct color-raw))
  (color (* (struct color-raw)))
  (alpha float))

(defun color-alpha (color alpha)
  (let* ((ptr (color-alpha-raw (color-pointer color) alpha))
         (new (@color :pointer ptr)))
    (tg:finalize new (lambda () (free-alien ptr)))))

;; --- Textures --- ;;

(define-alien-type nil
    (struct texture-2d-raw
            (id unsigned-int)
            (width int)
            (height int)
            (mipmaps int)
            (format int)))

(defstruct (texture-2d (:constructor @texture-2d))
  (pointer nil :type alien))

(define-alien-routine ("_LoadTexture" load-texture-raw) (* (struct texture-2d-raw))
  (file-name c-string))

(defun load-texture (file-name)
  (let* ((pointer (load-texture-raw file-name))
         (texture (@texture-2d :pointer pointer)))
    (tg:finalize texture (lambda () (free-alien pointer)))))

#++
(progn
  (init-window 300 300 "hello!")
  (let ((p (load-texture-raw "assets/logo.png")))
    (format t "~a~%" (slot p 'id))
    (format t "~a~%" (slot p 'width))
    (format t "~a~%" (slot p 'height)))
  (close-window))

(defmacro texture-width (r)
  `(sb-alien:slot (texture-pointer ,r) 'width))

(defmacro texture-height (r)
  `(sb-alien:slot (texture-pointer ,r) 'height))

;; --- Sounds and Music --- ;;

(define-alien-type nil
    (struct audio-stream
            (buffer (* t))
            (processor (* t))
            (sample-rate unsigned-int)
            (sample-size unsigned-int)
            (channels unsigned-int)))

(define-alien-type nil
    (struct sound-raw
            (stream (struct audio-stream))
            (frame-count unsigned-int)))

(defstruct (sound (:constructor @sound))
  (pointer nil :type alien))

(define-alien-type nil
    (struct music-raw
            (stream (struct audio-stream))
            (frame-count unsigned-int)
            (looping boolean)
            (ctx-type int)
            (ctx-data (* t))))

(defstruct (music (:constructor @music))
  (pointer nil :type alien))

(defmacro music-looping (m)
  `(sb-alien:slot (music-pointer ,m) 'looping))

;; --- Camera --- ;;

(define-alien-type nil
    (struct camera-2d-raw
            (offset (struct vector2-raw))
            (target (struct vector2-raw))
            (rotation float)
            (zoom float)))

(defstruct (camera-2d (:constructor @camera-2d))
  (pointer nil :type alien))

(define-alien-routine ("_MakeCamera2D" make-camera-2d-raw) (* (struct camera-2d-raw))
  (offset (* (struct vector2-raw)))
  (target (* (struct vector2-raw)))
  (rotation float)
  (zoom float))

(defun make-camera-2d (&key offset target rotation zoom)
  (let* ((pointer (make-camera-2d-raw (vector2-pointer offset)
                                      (vector2-pointer target)
                                      rotation zoom))
         (camera (@camera-2d :pointer pointer)))
    (tg:finalize camera (lambda () (free-alien pointer)))))

;; --- Keyboard and Gamepad --- ;;

;; HACK: 2024-12-29 Hacked manually as I couldn't figure out a first-class way
;; to reference enum values.
(defun keyboard-key (kw)
  (case kw
    (:right 262)
    (:left  263)
    (:down  264)
    (:up    265)
    (:space 32)
    (:tab   258)
    (:enter 257)
    (t (error "Unknown keyboard key: ~a" kw))))

#++
(keyboard-key :kim)

(defun gamepad-button (kw)
  (case kw
    (:unknown 0)
    (:left-face-up 1)
    (:left-face-right 2)
    (:left-face-down 3)
    (:left-face-left 4)
    (:right-face-up 5)
    (:right-face-right 6)
    (:right-face-down 7)
    (:right-face-left 8)
    (:left-trigger-1 9)
    (:left-trigger-2 10)
    (:right-trigger-1 11)
    (:right-trigger-2 12)
    (:middle-left 13)
    (:middle 14)
    (:middle-right 15)
    (:left-thumb 16)
    (:right-thumb 17)))

;; --- Window --- ;;

(define-alien-routine ("InitWindow" init-window) void
  (width int)
  (height int)
  (title c-string))

(define-alien-routine ("CloseWindow" close-window) void)

(define-alien-routine ("InitAudioDevice" init-audio-device) void)

(define-alien-routine ("CloseAudioDevice" close-audio-device) void)

(define-alien-routine ("SetTargetFPS" set-target-fps) void
  (fps int))

(define-alien-routine ("WindowShouldClose" window-should-close) boolean)

(define-alien-routine ("BeginDrawing" begin-drawing) void)

(define-alien-routine ("EndDrawing" end-drawing) void)

(define-alien-routine ("_BeginMode2D" begin-mode-2d-raw) void
  (camera (* (struct camera-2d-raw))))

(defun begin-mode-2d (camera)
  (begin-mode-2d-raw (camera-2d-pointer camera)))

(define-alien-routine ("EndMode2D" end-mode-2d) void)

(define-alien-routine ("_ClearBackground" clear-background-raw) void
  (color (* (struct color-raw))))

(defun clear-background (color)
  (clear-background-raw (color-pointer color)))

(define-alien-routine ("DrawFPS" draw-fps) void
  (pos-x int)
  (pos-y int))

(define-alien-routine ("_DrawText" draw-text-raw) void
  (text c-string)
  (pos-x int)
  (pos-y int)
  (font-size int)
  (color (* (struct color-raw))))

(defun draw-text (text pos-x pos-y font-size color)
  (draw-text-raw text pos-x pos-y font-size (color-pointer color)))

(define-alien-routine ("_DrawCircle" draw-circle-raw) void
  (center-x int)
  (center-y int)
  (radius int)
  (color (* (struct color-raw))))

(defun draw-circle (center-x center-y radius color)
  (draw-circle-raw center-x center-y radius (color-pointer color)))

(define-alien-routine ("_DrawRectangle" draw-rectangle-raw) void
  (pos-x int)
  (pos-y int)
  (width int)
  (height int)
  (color (* (struct color-raw))))

(defun draw-rectangle (pos-x pos-y width height color)
  (draw-rectangle-raw pos-x pos-y width height (color-pointer color)))

#++
(progn
  (let ((colour (make-color :r 255 :g 255 :b 255 :a 255)))
    (init-window 300 300 "hello!")
    (set-target-fps 60)
    (loop while (not (window-should-close))
          do (progn (begin-drawing)
                    (clear-background colour)
                    (draw-fps 0 0)
                    (end-drawing)))
    (close-window)))
