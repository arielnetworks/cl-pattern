(in-package :cl-pattern)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun compile-match-clauses (value clauses &key test)
    (declare (type symbol value))
    (if clauses
        (let+ ((((pattern . body) . rest) clauses)
               (body `(lambda () ,@body)))
          (cond
            ((otherwise-variable-p pattern) body)
            ((variable-p pattern)
             `(let+/test ,test ((,pattern ,value)) ,body))
            ((consp pattern)
             `(handler-case
                  (let+/test ,test ((,pattern ,value)) ,body)
                (error () ,(compile-match-clauses value rest :test test))))
            (t `(if (funcall ,test ,pattern ,value)
                    ,body
                    ,(compile-match-clauses value rest :test test)))))
        '(error "match error"))))

(defmacro match/test (test value &body clauses)
  (once-only (value)
    `(funcall ,(compile-match-clauses value clauses :test test))))

(defmacro match (value &body clauses)
  `(match/test #'equal ,value ,@clauses))
