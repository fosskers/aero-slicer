(defpackage raylib-ecl
  (:use :cl))

(in-package :raylib-ecl)

;; TODO: 2024-12-25 Probably need an `eval-when' here.
(ffi:load-foreign-library #p"/home/colin/code/common-lisp/aero-fighter/vendored/raylib/src/libraylib.so.550")

(ffi:def-struct texture
    (id      :unsigned-int)
  (width   :int)
  (height  :int)
  (mipmaps :int)
  (format  :int))

#++
(progn
  (ffi:clines "#include \"raylib.h\"")
  (ffi:def-function ("LoadTexture" load-texture)
      ((file-name (* :unsigned-char)))
    :returning texture))

#++
(ffi:def-function ("LoadTexture" load-texture))

;; Can't call c-inline through in the interpreter, dev workflow would be awful.
#++
(defun load-texture (file-name)
  (ffi:c-inline (file-name) (:string) texture
                "LoadTexture(#0)"
                :one-liner t))

