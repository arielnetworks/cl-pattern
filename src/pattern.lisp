(in-package :cl-pattern)
(use-syntax :annot)
(declaim (optimize (speed 3)))

(defmacro %equal (pattern value)
  (typecase pattern
    (null `(null ,value))
    (cons `(eq ',(cadr pattern) ,value))
    ((or symbol character) `(eq ,pattern ,value))
    (number `(eql ,pattern ,value))
    (t `(equal ,pattern ,value))))

(defun pattern-type (pattern)
  (etypecase pattern
    ((member t nil) :const)
    (keyword :const)
    (symbol :var)
    (cons (case (car pattern)
            (quote :const)
            (&optional :optional)
            (t :cons)))
    (atom :const)))

(defun free-variables (pattern)
  (case (pattern-type pattern)
    (:var (list pattern))
    (:cons (append (free-variables (car pattern))
                   (free-variables (cdr pattern))))))

(defun optional-patterns (pattern)
  (if (consp pattern)
      (mapcar (lambda (sub-pattern)
                (cons (car pattern)
                      sub-pattern))
              (cons nil
                    (optional-patterns (cdr pattern))))
      pattern))
