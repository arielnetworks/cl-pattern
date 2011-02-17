(in-package :cl-user)

(defpackage cl-pattern-asd
  (:use :cl :asdf))

(in-package :cl-pattern-asd)

(defsystem cl-pattern
  :version "0.1"
  :author "Tomohiro Matsuyama"
  :license "LLGPL"
  :depends-on (:cl-annot :alexandria)
  :components
  ((:module "src"
    :serial t
    :components ((:file "package")
                 (:file "condition")
                 (:file "pattern")
                 (:file "case")
                 (:file "compile")
                 (:file "match")))))
