(defpackage raylib
  (:use :cl #+sbcl :sb-alien)
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
  (:documentation "A light wrapping of necessary Raylib types and functions."))

(in-package :raylib)

;; TODO: 2024-12-25 Probably need an `eval-when' here.
#+sbcl
(progn
  (load-shared-object #p"lib/libraylib.so")
  (load-shared-object #p"lib/libshim.so"))

;; NOTE: 2025-01-03 We preload the shared libraries here to ensure that all functions
;; are already visible when we start to reference them in other files.
#+ecl
(progn
  (ffi:load-foreign-library #p"/home/colin/code/common-lisp/aero-fighter/lib/libraylib.so")
  (ffi:load-foreign-library #p"/home/colin/code/common-lisp/aero-fighter/lib/libshim.so"))

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
    (:right-thumb 17)
    (t (error "Unknown gamepad button: ~a" kw))))

;; Sanity test: This should work as-is under either compiler.
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
