(in-package :cl-user)
(defpackage cl-pattern-test-asd
  (:use :cl :asdf))
(in-package :cl-pattern-test-asd)

(defsystem cl-pattern-test
  :depends-on (:cl-test-more :cl-pattern)
  :components ((:module "t"
                :serial t
                :components ((:file "match")))))
