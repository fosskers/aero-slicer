(in-package :raylib)

;; NOTE: 2025-01-02 Must be here or ECL fails to find various symbols.
(ffi:clines "#include \"shim.h\"")
(ffi:clines "#include <stdlib.h>")

(defun free! (ptr)
  "A custom call to C's `free' that ensures everything is properly reset."
  (ffi:c-inline (ptr) (:object) :void
                "void *ptr = ecl_foreign_data_pointer_safe(#0);
                 #0->foreign.size = 0;
                 #0->foreign.data = NULL;
                 free(ptr);" :one-liner nil))

;; --- Vectors --- ;;

(ffi:def-struct vector2-raw
    (x :float)
  (y :float))

(ffi:def-function ("_MakeVector2" make-vector2-raw)
    ((x :float)
     (y :float))
  :returning (* vector2-raw))

(defstruct (vector2 (:constructor @vector2))
  (pointer nil :type si:foreign-data))

(defun make-vector2 (&key x y)
  (let* ((ptr (make-vector2-raw x y))
         (v   (@vector2 :pointer ptr)))
    (tg:finalize v (lambda () (free! ptr)))))

#++
(make-vector2 :x 1.0 :y 2.0)

(defmacro vector2-x (v)
  "The X slot of a `Vector2'."
  `(ffi:get-slot-value (vector2-pointer ,v) 'vector2-raw 'x))

(defmacro vector2-y (v)
  "The Y slot of a `Vector2'."
  `(ffi:get-slot-value (vector2-pointer ,v) 'vector2-raw 'y))

#++
(let ((v (make-vector2 :x 1.0 :y 2.0)))
  (setf (vector2-x v) 1000.0)
  (vector2-x v))
