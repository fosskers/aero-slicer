(defsystem "raylib"
  :depends-on (:cffi :cffi-object)
  :components ((:module "src"
                :components ((:file "raylib")))))
