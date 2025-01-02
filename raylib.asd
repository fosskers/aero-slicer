(defsystem "raylib"
  :depends-on (:trivial-garbage)
  :serial t
  :components ((:module "lisp"
                :components
                ((:file "raylib-sbcl" :if-feature :sbcl)
                 (:file "raylib-ecl-pkg" :if-feature :ecl)
                 (:file "raylib-ecl" :if-feature :ecl)))))
