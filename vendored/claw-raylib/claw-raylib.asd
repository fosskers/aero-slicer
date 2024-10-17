(defsystem claw-raylib
  :version "1.0.0"
  :author "Bohong Huang <1281299809@qq.com>"
  :maintainer "Bohong Huang <1281299809@qq.com>"
  :license "Apache-2.0"
  :homepage "https://github.com/bohonghuang/claw-raylib"
  :bug-tracker "https://github.com/bohonghuang/claw-raylib/issues"
  :source-control (:git "https://github.com/bohonghuang/claw-raylib.git")
  :depends-on (#:alexandria
               #:global-vars
               #:cffi #:cffi-ops #:cffi-object #:cffi-object.ops
               #:claw-raylib.defcfun
               #:claw-raylib.library
               #:claw-raylib.raylib
               #:claw-raylib.rlgl)
  :components ((:file "package")
               (:file "object" :depends-on ("package"))
               (:file "macros" :depends-on ("package"))
               (:file "unexport" :depends-on ("package"))
               (:file "reexport" :depends-on ("package" "unexport"))))

