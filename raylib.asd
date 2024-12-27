(defsystem "raylib"
  :depends-on (:cffi :cffi-object :trivial-garbage)
  :components ((:module "lisp"
                :components ((:file "raylib")
                             (:file "raylib-sbcl" :if-feature :sbcl)))))
