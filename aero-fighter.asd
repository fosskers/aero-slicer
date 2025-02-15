(defsystem "aero-fighter"
  :version "25.2.16"
  :author "Colin Woodbury <colin@fosskers.ca>"
  :license "MPL-2.0"
  :homepage "https://github.com/fosskers/aero-fighter"
  :depends-on (:arrow-macros
               :com.inuoe.jzon
               :filepaths
               :raylib
               :transducers)
  :serial t
  :components ((:module "game"
                :components ((:file "package")
                             (:file "math")
                             (:file "background")
                             (:file "landing")
                             (:file "aseprite")
                             (:file "beam")
                             (:file "fighter")
                             (:file "state")
                             (:file "input")
                             (:file "powerups")
                             (:file "enemies")
                             (:file "collision")
                             (:file "hud")
                             (:file "main"))))
  :description "JP's shooter from the past.")
