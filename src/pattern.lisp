(in-package :cl-pattern)

(annot:enable-annot-syntax)

(defmacro %equal (pattern value)
  (cond
    ((null pattern)
     `(null ,value))
    ((atom pattern)
     `(eq ,pattern ,value))
    ((consp pattern)
     ;; special case: quote form
     `(eq ',(cadr pattern) ,value))
    (t
     `(equal ,pattern ,value))))

(defun pattern-type (pattern)
  (etypecase pattern
    (null :const)
    (keyword :const)
    (symbol :var)
    (cons
     (if (eq (car pattern) 'quote)
         ;; special case: quote form
         :const
         :cons))
    (atom :const)))
