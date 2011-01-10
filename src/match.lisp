(in-package :cl-pattern)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun compile-match-clauses (value clauses)
    (declare (type symbol value))
    (if clauses
        (let+ ((((pattern . body) . rest) clauses)
               (body `(lambda () ,@body)))
          (cond
            ((otherwise-variable-p pattern) body)
            ((variable-p pattern)
             `(let+ ((,pattern ,value)) ,body))
            ((consp pattern)
             `(handler-case
                  (let+ ((,pattern ,value)) ,body)
                (error () ,(compile-match-clauses value rest))))
            (t `(if (%equal ,value ,pattern)
                    ,body
                    ,(compile-match-clauses value rest)))))
        '(error "match error"))))

(defmacro match (value &body clauses)
  (once-only (value)
    `(funcall ,(compile-match-clauses value clauses))))
