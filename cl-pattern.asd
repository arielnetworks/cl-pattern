(in-package :cl-user)

(defpackage cl-pattern-asd
  (:use :cl :asdf))

(in-package :cl-pattern-asd)

(defsystem cl-pattern
  :version "0.1"
  :depends-on (:alexandria)
  :components ((:module "src"
                        :serial t
                        :components ((:file "package")
                                     (:file "utils")
                                     (:file "let")
                                     (:file "match")))))
