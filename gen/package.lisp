(defpackage aero-fighter/gen
  (:use :cl))

(in-package :aero-fighter/gen)

(let ((arch "x86_64-pc-linux-gnu")
      (path (merge-pathnames #P"lib/" (asdf:component-pathname (asdf:find-system '#:claw-raylib)))))
  (dolist (lib '("raylib" "rlgl" "raygui"))
    (uiop:run-program
     (list "gcc" "-O3" "-fPIC" "-shared" "-o"
           (namestring (merge-pathnames (format nil "lib~A-adapter.so" lib) path))
           (namestring (merge-pathnames (format nil "lib~A-adapter.~A.c" lib arch) path))))))
