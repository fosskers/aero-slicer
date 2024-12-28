(defsystem "raylib"
  :depends-on (:trivial-garbage)
  :components ((:module "lisp"
                :components ((:file "raylib-sbcl" :if-feature :sbcl)))))
