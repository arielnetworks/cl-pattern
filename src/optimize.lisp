(in-package :cl-pattern)

(defmacro %equal (var value)
  (typecase value
    (string `(string= ,var ,value))
    (otherwise `(eql ,var ,value))))
