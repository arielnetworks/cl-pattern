(defpackage cl-pattern.benchmark
  (:use :cl
        :cl-pattern))
(in-package :cl-pattern.benchmark)

(declaim (optimize (speed 3)))

(defun benchmark ()
  (let ((triple '(1 2 3))
        (n 10000000))
    (format t "~&MATCH~%")
    (time
     (dotimes (i n)
       (match triple
         ((a &optional b c) (+ a b c)))))
    (format t "~&DESTRUCTURING-BIND~%")
    (time
     (dotimes (i n)
       (destructuring-bind (a &optional b c)
           triple
         (+ a b c))))))

(benchmark)
