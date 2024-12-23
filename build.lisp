(require :asdf)

;; Force ASDF to only look here for systems.
(asdf:initialize-source-registry `(:source-registry (:tree ,(uiop:getcwd)) :ignore-inherited-configuration))

;; Load and compile the binary.
(format t "--- LOADING SYSTEM ---~%")
(asdf:load-system :aero-fighter)
(format t "--- COMPILING EXECUTABLE ---~%")

#+ecl
(asdf:make-build :aero-fighter
                 :type :program
                 :move-here #p"./"
                 :prologue-code
                 '(progn
                   (format t "--- LOADING ---~%")
                   (defpackage :asdf/operate)
                   (defpackage :asdf/lisp-action)
                   (format t "--- LOADED ---~%"))
                 :epilogue-code
                 '(progn
                   (format t "--- RUNNING ---~%")
                   (aero-fighter:launch)
                   (format t "--- DONE ---~%")
                   (si:exit)))

#+sbcl
(progn
  (format t "POLICY: ~a~%" sb-c::*policy*)
  (sb-ext:save-lisp-and-die #p"aero-fighter"
                            :toplevel #'aero-fighter:launch
                            :executable t
                            :compression t))

(format t "--- DONE ---~%")

#+ecl
(si:exit)
