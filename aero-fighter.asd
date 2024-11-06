(defsystem "aero-fighter"
  :version "0.0.1"
  :author "Colin Woodbury <colin@fosskers.ca>"
  :license "MPL-2.0"
  :depends-on (:claw-raylib
               :com.inuoe.jzon
               :filepaths
               :transducers)
  :components ((:module "src"
                :components ((:file "package")
                             (:file "state")
                             (:file "aseprite")
                             (:file "powerups")
                             (:file "beam")
                             (:file "fighter")
                             (:file "enemies")
                             (:file "collision")
                             (:file "main"))))
  :description "JP's shooter from the past.")

(defsystem "aero-fighter/gen"
  :version "0.0.1"
  :author "Colin Woodbury <colin@fosskers.ca>"
  :license "MPL-2.0"
  :depends-on ()
  :components ((:module "gen"
                :components ((:file "package")))))
