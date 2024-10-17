(cl:in-package #:raylib)

(cobj:define-cobject-class #:raylib)

(cl:in-package #:rlgl)

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (cffi:defctype matrix (:struct raylib:matrix)))

(cl:progn . #.(cl:remove-if
               (cl:lambda (form)
                 (alexandria:destructuring-case form
                   ((cobj::define-struct-cobject-class (name cl:&rest args))
                    (cl:declare (cl:ignore args))
                    (cl:eq name 'matrix))))
               (cl:cdr (cl:macroexpand '(cobj:define-cobject-class #:rlgl)))))

(cl:in-package #:claw-raylib)

(do-symbols (symbol :raylib)
  (when (find-class symbol nil)
    (when-let ((definition (ignore-errors (cobj::cobject-class-definition symbol))))
      (export (cobj::cobject-class-definition-symbols definition) :raylib))))

(do-symbols (symbol :rlgl)
  (when (find-class symbol nil)
    (when-let ((definition (ignore-errors (cobj::cobject-class-definition symbol))))
      (export (cobj::cobject-class-definition-symbols definition) :rlgl))))
