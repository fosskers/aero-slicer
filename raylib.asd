(defsystem "raylib"
  :depends-on (:trivial-garbage)
  :serial t
  :components ((:module "raylib"
                :components ((:file "package")
                             (:file "sbcl" :if-feature :sbcl)
                             (:file "ecl" :if-feature :ecl)))))
