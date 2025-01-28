(require :asdf)

;; Force ASDF to only look here for systems.
(asdf:initialize-source-registry `(:source-registry (:tree ,(uiop:getcwd)) :ignore-inherited-configuration))

#+ecl
(progn
  (setf c:*user-linker-flags* "-Wl,-rpath,/home/colin/code/common-lisp/aero-fighter/lib/ -L/home/colin/code/common-lisp/aero-fighter/lib/")
  (setf c:*user-linker-libs*  "-lraylib -lshim")
  (declaim (optimize (speed 3) (debug 1) (safety 1))))

;; Compiling as a separate step ensures that FFI-related systems are properly
;; compiled and linked. Otherwise, under ECL, just naively loading can result in
;; missing C symbols. Once we've compiled like this at least once, subsequent
;; `load-system' calls, including in the REPL, "just work".
(format t "--- COMPILING SYSTEM ---~%")
(asdf:compile-system :aero-fighter)
(format t "--- LOADING SYSTEM ---~%")
(asdf:load-system :aero-fighter)

#+ecl
(progn
  (format t "--- ECL: COMPILING EXECUTABLE ---~%")
  (asdf:make-build :aero-fighter
                   :type :program
                   :move-here #p"./"
                   :epilogue-code
                   '(progn
                     (aero-fighter:launch)
                     (ext:quit)))
  (ext:quit))

#+sbcl
(progn
  (format t "--- SBCL: SAVING IMAGE ---~%")
  (format t "POLICY: ~a~%" sb-c::*policy*)
  (sb-ext:save-lisp-and-die #p"aero-fighter"
                            :toplevel #'aero-fighter:launch
                            :executable t
                            :compression t))
