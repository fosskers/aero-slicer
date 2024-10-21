(defsystem "filepaths"
  :version "0.1.2"
  :author "Colin Woodbury <colin@fosskers.ca>"
  :license "LGPL-3.0-only"
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "filepaths"))))
  :description "Modern and consistent filepath manipulation.")
