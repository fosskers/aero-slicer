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
  (load-shared-object #p"/home/colin/code/common-lisp/aero-fighter/vendored/raylib/src/libraylib.so")
  (load-shared-object #p"/home/colin/code/common-lisp/aero-fighter/raylib/shim.so"))

;; NOTE: 2025-01-03 We preload the shared libraries here to ensure that all functions
;; are already visible when we start to reference them in other files.
#+ecl
(progn
  (ffi:load-foreign-library #p"/home/colin/code/common-lisp/aero-fighter/vendored/raylib/src/libraylib.so")
  (ffi:load-foreign-library #p"/home/colin/code/common-lisp/aero-fighter/raylib/shim.so"))
