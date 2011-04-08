(in-package :cl-pattern)
(use-syntax annot-syntax)

(defmacro %equal (pattern value)
  (typecase pattern
    (null `(null ,value))
    (string `(equal ,pattern ,value))
    (atom `(eq ,pattern ,value))
    (cons `(eq ',(cadr pattern) ,value))
    (t `(equal ,pattern ,value))))

(defun pattern-type (pattern)
  (etypecase pattern
    (null :const)
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