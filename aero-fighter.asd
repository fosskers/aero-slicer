(defsystem "aero-fighter"
  :version "0.0.1"
  :author "Colin Woodbury <colin@fosskers.ca>"
  :license "MPL-2.0"
  :homepage "https://github.com/fosskers/aero-fighter"
  :depends-on (:arrow-macros
               :com.inuoe.jzon
               :filepaths
               :raylib
               :transducers)
  :build-operation "program-op"
  :build-pathname "aero-fighter"
  :entry-point "aero-fighter:launch"
  :serial t
  :components ((:module "lisp"
                :components ((:file "package")
                             (:file "math")
                             (:file "background")
                             (:file "landing")
                             (:file "aseprite")
                             (:file "beam")
                             (:file "fighter")
                             (:file "state")
                             (:file "powerups")
                             (:file "enemies")
                             (:file "collision")
                             (:file "hud")
                             (:file "input")
                             (:file "main"))))
  :description "JP's shooter from the past.")
