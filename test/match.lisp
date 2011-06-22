(in-package :cl-user)
(defpackage cl-pattern.test
  (:use :cl
        :cl-test-more
        :cl-pattern))
(in-package :cl-pattern.test)

(plan 22)

(is (match 1
      (x x))
    1
    "match variable")
(is (match t
      (t 1))
    1
    "match t")
(is (match nil
      (nil 1))
    1
    "match nil")
(is (match '(1 2)
      ((x y) (+ x y)))
    3
    "match cons")
(is (match '("a")
      (("a") 1))
    1
    "match string")
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
(is (match '(:a :b :c)
      ((:a :b) t)
      (_ 'otherwise))
    'otherwise
    "match otherwise")
(is (match ()
      ((&optional a) a))
    nil
    "match optional empty")
(is (match '(1)
      ((&optional a b) (list a b)))
    '(1 nil)
    "match optional lack")
(is (match '(1 2)
      ((&optional a b) (+ a b)))
    3
    "match optional enough")
(is (match '(1 2)
      ((&optional a) a)
      ((&optional a b) (+ a b)))
    3
    "match optional multi")
(is (match '(())
      (((&optional a)) a))
    nil
    "match optional nest empty")
(is (match '((1))
      (((&optional a b)) (list a b)))
    '(1 nil)
    "match optional nest lack")
(is (match '((1 2))
      (((&optional a b)) (+ a b)))
    3
    "match optional nest enough")
(is (match '((()))
      ((((&optional a))) a))
    nil
    "match optional nest 3")
(is (match '((1) "a")
      (((1 &optional a) "a" &optional b) (list a b)))
    '(nil nil)
    "match complex")

(defun sum (list)
  (match list
    (() 0)
    ((x . xs) (+ x (sum xs)))))

(is (sum '(1 2 3))
    6
    "sum")

(is-expand (lambda-match (('foo a) a))
           (lambda ($arg)
             (match $arg
               (('foo a) a)))
           "lambda-match expansion")
(is (funcall (lambda-match (('foo a) a)) '(foo 1))
    1
    "lambda-match")

(finalize)
