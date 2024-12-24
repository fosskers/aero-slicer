(defpackage raylib
  (:use :cl)
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
           #:load-texture #:unload-texture #:draw-texture #:draw-texture-v #:draw-texture-rec
           #:load-sound #:unload-sound #:play-sound
           #:load-music-stream #:unload-music-stream #:is-music-stream-playing #:play-music-stream #:update-music-stream
           #:is-key-pressed #:is-key-down #:is-gamepad-button-pressed #:is-gamepad-button-down
           #:check-collision-recs)
  (:documentation "A light wrapping of necessary Raylib types and functions."))

(in-package :raylib)

(cffi:define-foreign-library libraylib
  (:unix "/home/colin/code/common-lisp/aero-fighter/vendored/raylib/src/libraylib.so.550")
  (t (:default "libraylib")))

(cffi:use-foreign-library libraylib)

;; --- Types --- ;;

(cffi:defcstruct vector2
  (x :float)
  (y :float))

(cobj:define-cobject-class (vector2 (:struct vector2)))

#++
(let ((v (make-vector2 :x 1.0 :y 2.0)))
  (setf (vector2-x v) 100.0)
  v)

(cffi:defcstruct rectangle
  (x      :float)
  (y      :float)
  (width  :float)
  (height :float))

(cobj:define-cobject-class (rectangle (:struct rectangle)))

#++
(make-rectangle :x 0.0 :y 0.0 :width 16.0 :height 16.0)

(cffi:defcstruct color
  (r :unsigned-char)
  (g :unsigned-char)
  (b :unsigned-char)
  (a :unsigned-char))

(cobj:define-cobject-class (color (:struct color)))

#++
(make-color :r 255 :g 255 :b 255 :a 255)

(cffi:defcstruct texture
  (id      :unsigned-int)
  (width   :int)
  (height  :int)
  (mipmaps :int)
  (format  :int))

(cobj:define-cobject-class (texture (:struct texture)))

(cffi:defcstruct audio-stream
  (buffer      :pointer)
  (processor   :pointer)
  (sample-rate :unsigned-int)
  (sample-size :unsigned-int)
  (channels    :unsigned-int))

(cobj:define-cobject-class (audio-stream (:struct audio-stream)))

(cffi:defcstruct sound
  (stream      (:struct audio-stream))
  (frame-count :unsigned-int))

(cobj:define-cobject-class (sound (:struct sound)))

(cffi:defcstruct music
  (stream      (:struct audio-stream))
  (frame-count :unsigned-int)
  (looping     :bool)
  (ctx-type    :int)
  (ctx-data    :pointer))

(cobj:define-cobject-class (music (:struct music)))

(cffi:defcstruct camera-2d
  (offset   (:struct vector2))
  (target   (:struct vector2))
  (rotation :float)
  (zoom     :float))

(cobj:define-cobject-class (camera-2d (:struct camera-2d)))

(cffi:defcenum keyboard-key
  (:right 262)
  (:left  263)
  (:down  264)
  (:up    265)
  (:space 32)
  (:tab   258)
  (:enter 257))

#++
(cffi:foreign-enum-value 'keyboard-key :up)

(cffi:defcenum gamepad-button
  :unknown
  :left-face-up
  :left-face-right
  :left-face-down
  :left-face-left
  :right-face-up
  :right-face-right
  :right-face-down
  :right-face-left
  :left-trigger-1
  :left-trigger-2
  :right-trigger-1
  :right-trigger-2
  :middle-left
  :middle
  :middle-right
  :left-thumb
  :right-thumb)

#++
(cffi:foreign-enum-value 'gamepad-button :middle)

;; --- Functions --- ;;

(cffi:defcfun "InitWindow" :void
  (width  :int)
  (height :int)
  (title  :string))

(cffi:defcfun "CloseWindow" :void)

(cffi:defcfun "InitAudioDevice" :void)

(cffi:defcfun "CloseAudioDevice" :void)

(cffi:defcfun "SetTargetFPS" :void
  (fps :int))

(cffi:defcfun "WindowShouldClose" :bool)

(cffi:defcfun "BeginDrawing" :void)

(cffi:defcfun "EndDrawing" :void)

(cffi:defcfun "BeginMode2D" :void
  (camera (:pointer (:struct camera-2d))))

(cffi:defcfun "EndMode2D" :void)

(cffi:defcfun "ClearBackground" :void
  (color (:pointer (:struct color))))

(cffi:defcfun "DrawFPS" :void
  (pos-x :int)
  (pos-y :int))

(cffi:defcfun "DrawText" :void
  (text      :string)
  (pos-x     :int)
  (pos-y     :int)
  (font-size :int)
  (color     (:pointer (:struct color))))

(cffi:defcfun "DrawCircle" :void
  (center-x :int)
  (center-y :int)
  (radius   :int)
  (color    (:pointer (:struct color))))

(cffi:defcfun "DrawRectangle" :void
  (pos-x  :int)
  (pos-y  :int)
  (width  :int)
  (height :int)
  (color  (:pointer (:struct color))))

(cffi:defcfun "LoadTexture" (:pointer (:struct texture))
  (file-name :string))

(cffi:defcfun "UnloadTexture" :void
  (texture (:pointer (:struct texture))))

(cffi:defcfun "DrawTexture" :void
  (texture (:pointer (:struct texture)))
  (pos-x   :int)
  (pos-y   :int)
  (tint    (:pointer (:struct color))))

(cffi:defcfun "DrawTextureV" :void
  (texture  (:pointer (:struct texture)))
  (position (:pointer (:struct vector2)))
  (tint     (:pointer (:struct color))))

(cffi:defcfun "DrawTextureRec" :void
  (texture   (:pointer (:struct texture)))
  (rectangle (:pointer (:struct rectangle)))
  (position  (:pointer (:struct vector2)))
  (tint      (:pointer (:struct color))))

(cffi:defcfun "ColorAlpha" (:pointer (:struct color))
  (color (:pointer (:struct color)))
  (alpha :float))

(cffi:defcfun "LoadSound" (:pointer (:struct sound))
  (file-name :string))

(cffi:defcfun "UnloadSound" :void
  (sound (:pointer (:struct sound))))

(cffi:defcfun "PlaySound" :void
  (sound (:pointer (:struct sound))))

(cffi:defcfun "LoadMusicStream" (:pointer (:struct music))
  (file-name :string))

(cffi:defcfun "UnloadMusicStream" :void
  (music (:pointer (:struct music))))

(cffi:defcfun "PlayMusicStream" :void
  (music (:pointer (:struct music))))

(cffi:defcfun "IsMusicStreamPlaying" :bool
  (music (:pointer (:struct music))))

(cffi:defcfun "UpdateMusicStream" :void
  (music (:pointer (:struct music))))

(cffi:defcfun "IsKeyPressed" :bool
  (key :int))

(cffi:defcfun "IsKeyDown" :bool
  (key :int))

(cffi:defcfun "IsGamepadButtonDown" :bool
  (gamepad :int)
  (button  :int))

(cffi:defcfun "IsGamepadButtonPressed" :bool
  (gamepad :int)
  (button  :int))

(cffi:defcfun "CheckCollisionRecs" :bool
  (rec1 (:pointer (:struct rectangle)))
  (rec2 (:pointer (:struct rectangle))))
