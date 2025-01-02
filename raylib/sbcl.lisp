(in-package :raylib)

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
  `(slot (vector2-pointer ,v) 'x))

(defmacro vector2-y (v)
  "The Y slot of a `Vector2'."
  `(slot (vector2-pointer ,v) 'y))

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
  `(slot (rectangle-pointer ,r) 'x))

(defmacro rectangle-y (r)
  `(slot (rectangle-pointer ,r) 'y))

(defmacro rectangle-width (r)
  `(slot (rectangle-pointer ,r) 'width))

(defmacro rectangle-height (r)
  `(slot (rectangle-pointer ,r) 'height))

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
    (struct texture-raw
            (id unsigned-int)
            (width int)
            (height int)
            (mipmaps int)
            (format int)))

(defstruct (texture (:constructor @texture))
  (pointer nil :type alien))

(define-alien-routine ("_LoadTexture" load-texture-raw) (* (struct texture-raw))
  (file-name c-string))

(defun load-texture (file-name)
  (let* ((pointer (load-texture-raw file-name))
         (texture (@texture :pointer pointer)))
    (tg:finalize texture (lambda () (free-alien pointer)))))

(define-alien-routine ("_UnloadTexture" unload-texture-raw) void
  (texture (* (struct texture-raw))))

(defun unload-texture (texture)
  (unload-texture-raw (texture-pointer texture)))

#++
(progn
  (init-window 300 300 "hello!")
  (let ((p (load-texture-raw "assets/logo.png")))
    (format t "~a~%" (slot p 'id))
    (format t "~a~%" (slot p 'width))
    (format t "~a~%" (slot p 'height)))
  (close-window))

(defmacro texture-width (r)
  `(slot (texture-pointer ,r) 'width))

(defmacro texture-height (r)
  `(slot (texture-pointer ,r) 'height))

(define-alien-routine ("_IsTextureValid" is-texture-valid-raw) (boolean 8)
  (texture (* (struct texture-raw))))

(defun is-texture-valid (texture)
  (is-texture-valid-raw (texture-pointer texture)))

(define-alien-routine ("_DrawTexture" draw-texture-raw) void
  (texture (* (struct texture-raw)))
  (pos-x int)
  (pos-y int)
  (tint (* (struct color-raw))))

(defun draw-texture (texture pos-x pos-y tint)
  (draw-texture-raw (texture-pointer texture)
                    pos-x pos-y
                    (color-pointer tint)))

(define-alien-routine ("_DrawTextureV" draw-texture-v-raw) void
  (texture  (* (struct texture-raw)))
  (position (* (struct vector2-raw)))
  (tint     (* (struct color-raw))))

(defun draw-texture-v (texture position tint)
  (draw-texture-v-raw (texture-pointer texture)
                      (vector2-pointer position)
                      (color-pointer tint)))

(define-alien-routine ("_DrawTextureRec" draw-texture-rec-raw) void
  (texture  (* (struct texture-raw)))
  (source   (* (struct rectangle-raw)))
  (position (* (struct vector2-raw)))
  (tint     (* (struct color-raw))))

(defun draw-texture-rec (texture source position tint)
  (draw-texture-rec-raw (texture-pointer texture)
                        (rectangle-pointer source)
                        (vector2-pointer position)
                        (color-pointer tint)))

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

(define-alien-routine ("_LoadSound" load-sound-raw) (* (struct sound-raw))
  (file-name c-string))

(defun load-sound (file-name)
  (let* ((pointer (load-sound-raw file-name))
         (sound   (@sound :pointer pointer)))
    (tg:finalize sound (lambda () (free-alien pointer)))))

(define-alien-routine ("_UnloadSound" unload-sound-raw) void
  (sound (* (struct sound-raw))))

(defun unload-sound (sound)
  (unload-sound-raw (sound-pointer sound)))

(define-alien-routine ("_PlaySound" play-sound-raw) void
  (sound (* (struct sound-raw))))

(defun play-sound (sound)
  (play-sound-raw (sound-pointer sound)))

(define-alien-type nil
    (struct music-raw
            (stream (struct audio-stream))
            (frame-count unsigned-int)
            (looping (boolean 8))
            (ctx-type int)
            (ctx-data (* t))))

(defstruct (music (:constructor @music))
  (pointer nil :type alien))

(defmacro music-looping (m)
  `(slot (music-pointer ,m) 'looping))

(define-alien-routine ("_LoadMusicStream" load-music-stream-raw) (* (struct music-raw))
  (file-name c-string))

(defun load-music-stream (file-name)
  (let* ((pointer (load-music-stream-raw file-name))
         (music   (@music :pointer pointer)))
    (tg:finalize music (lambda () (free-alien pointer)))))

(define-alien-routine ("_UnloadMusicStream" unload-music-stream-raw) void
  (music (* (struct music-raw))))

(defun unload-music-stream (music)
  (unload-music-stream-raw (music-pointer music)))

(define-alien-routine ("_IsMusicStreamPlaying" is-music-stream-playing-raw) (boolean 8)
  (music (* (struct music-raw))))

(defun is-music-stream-playing (music)
  (is-music-stream-playing-raw (music-pointer music)))

(define-alien-routine ("_PlayMusicStream" play-music-stream-raw) void
  (music (* (struct music-raw))))

(defun play-music-stream (music)
  (play-music-stream-raw (music-pointer music)))

(define-alien-routine ("_UpdateMusicStream" update-music-stream-raw) void
  (music (* (struct music-raw))))

(defun update-music-stream (music)
  (update-music-stream-raw (music-pointer music)))

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

(define-alien-routine ("GetGamepadName" get-gamepad-name) c-string
  (gamepad int))

(define-alien-routine ("IsGamepadAvailable" is-gamepad-available) (boolean 8)
  (gamepad int))

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

(define-alien-routine ("WindowShouldClose" window-should-close) (boolean 8))

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
  (radius float)
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

;; --- Input --- ;;

(define-alien-routine ("IsKeyPressed" is-key-pressed) (boolean 8)
  (key int))

(define-alien-routine ("IsKeyDown" is-key-down) (boolean 8)
  (key int))

(define-alien-routine ("IsGamepadButtonPressed" is-gamepad-button-pressed) (boolean 8)
  (gamepad int)
  (key int))

(define-alien-routine ("IsGamepadButtonDown" is-gamepad-button-down) (boolean 8)
  (gamepad int)
  (key int))

;; --- Collision --- ;;

(define-alien-routine ("_CheckCollisionRecs" check-collision-recs-raw) (boolean 8)
  (rec1 (* (struct rectangle-raw)))
  (rec2 (* (struct rectangle-raw))))

(defun check-collision-recs (rec1 rec2)
  (check-collision-recs-raw (rectangle-pointer rec1)
                            (rectangle-pointer rec2)))

