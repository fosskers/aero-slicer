(defsystem "transducers"
  :version "1.3.0"
  :author "Colin Woodbury <colin@fosskers.ca>"
  :license "MPL-2.0"
  :depends-on ()
  :components ((:module "transducers"
                :components
                ((:file "package")
                 (:file "transducers")
                 (:file "reducers")
                 (:file "sources")
                 (:file "entry")
                 (:file "conditions")
                 (:file "utils"))))
  :description "Ergonomic, efficient data processing.")
