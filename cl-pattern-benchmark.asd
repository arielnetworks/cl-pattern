(in-package :cl-user)
(defpackage cl-pattern-benchmark-asd
  (:use :cl :asdf))
(in-package :cl-pattern-benchmark-asd)

(defsystem cl-pattern-benchmark
  :depends-on (:cl-pattern)
  :components ((:module "etc"
                :components ((:file "benchmark")))))
