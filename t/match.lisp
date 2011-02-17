(in-package :cl-user)

(defpackage cl-annot-test
  (:use :cl
        :cl-test-more
        :cl-pattern))

(in-package :cl-annot-test)

(is (match 1
      (x x))
    1
    "match variable")
(is (match '(1 2)
      ((x y) (+ x y)))
    3
    "match cons")
(is (match '(:bar 1)
      ((:foo _) "Foo!")
      ((:bar _) "Bar!"))
    "Bar!"
    "match variant")
(is (match '(x 1)
      (('x x) x))
    1
    "match quoted symbol")
(is-error (match 5
            (1 'one)
            (2 'two)
            (3 'three)
            (4 'four))
          match-error
          "match error")
(is (match 5
      (1 'one)
      (2 'two)
      (3 'three)
      (4 'four)
      (_ 'otherwise))
    'otherwise
    "match otherwise")

(defun sum (list)
  (match list
    (() 0)
    ((x . xs) (+ x (sum xs)))))

(is (sum '(1 2 3))
    6
    "sum")

(finalize)
