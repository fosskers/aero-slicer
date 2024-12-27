(defpackage raylib-sbcl
  (:use :cl :sb-alien)
  (:local-nicknames (#:tg #:trivial-garbage)))

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

;; --- Textures --- ;;

(define-alien-type nil
    (struct texture-2d
            (width int)
            (height int)
            (mipmaps int)
            (format int)))

(define-alien-routine ("_LoadTexture" load-texture-raw) (* (struct texture-2d))
  (file-name c-string))

#++
(takeo "assets/logo.png")

#++
(progn
  (init-window 300 300 "hello!")
  (let ((p (load-texture-raw "assets/logo.png")))
    (format t "~a~%" (slot p 'width)))
  (close-window))

;; Duh! Can't load a Texture until I have an OpenGL context!

;; --- Window --- ;;

(define-alien-routine ("InitWindow" init-window) void
  (width int)
  (height int)
  (title c-string))

(define-alien-routine ("CloseWindow" close-window) void)
