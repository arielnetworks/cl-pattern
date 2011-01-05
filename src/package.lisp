(in-package :cl-user)

(defpackage cl-pattern
  (:use :cl :alexandria)
  (:nicknames :pattern)
  (:export :let+
           :let+/test
           :match
           :match/test))
