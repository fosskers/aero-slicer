(defsystem "aero-fighter"
  :version "0.0.1"
  :author "Colin Woodbury <colin@fosskers.ca>"
  :license "MPL-2.0"
  :depends-on (:arrow-macros
               :claw-raylib
               :com.inuoe.jzon
               :filepaths
               :transducers)
  :components ((:module "src"
                :components ((:file "package")
                             (:file "math")
                             (:file "background")
                             (:file "landing")
                             (:file "aseprite")
                             (:file "state")
                             (:file "beam")
                             (:file "fighter")
                             (:file "powerups")
                             (:file "enemies")
                             (:file "collision")
                             (:file "hud")
                             (:file "input")
                             (:file "main"))))
  :description "JP's shooter from the past.")

(defsystem "aero-fighter/gen"
  :version "0.0.1"
  :author "Colin Woodbury <colin@fosskers.ca>"
  :license "MPL-2.0"
  :depends-on ()
  :components ((:module "gen"
                :components ((:file "package")))))
