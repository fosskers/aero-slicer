(defpackage raylib-sbcl
  (:use :cl :sb-alien))

(in-package :raylib-sbcl)

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
  (@vector2 :pointer (make-vector2-raw x y)))

#++
(make-vector2 :x 1.0 :y 2.0)

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

;; --- Textures --- ;;

(define-alien-type nil
    (struct texture-2d
            (width int)
            (height int)
            (mipmaps int)
            (format int)))

(define-alien-routine ("_LoadTexture" takeo) (* (struct texture-2d))
  (file-name c-string))

#++
(takeo "assets/logo.png")

;; Duh! Can't load a Texture until I have an OpenGL context!
