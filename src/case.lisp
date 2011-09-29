(in-package :cl-pattern)
(use-syntax :annot)
(declaim (optimize (speed 3)))

@eval-always
(defun compile-case-clause (var clause else)
  (destructuring-bind (pattern then)
      clause
    (ecase (pattern-type pattern)
      (:const
       `(if (%equal ,pattern ,var) ,then ,else))
      (:var
       `(let ((,pattern ,var)) ,then))
      (:cons
       `(if (consp ,var)
            (let ((,(car pattern) (car ,var))
                  (,(cdr pattern) (cdr ,var)))
              (declare (ignorable ,(car pattern) ,(cdr pattern)))
              ,then)
            ,else)))))

@eval-always
(defun compile-case (var clauses else)
  (reduce (lambda (clause else) (compile-case-clause var clause else))
          clauses
          :initial-value else
          :from-end t))

(defmacro %case (arg clauses else)
  (if (atom arg)
      (compile-case arg clauses else)
      (with-gensyms (var)
        `(let ((,var ,arg))
           ,(compile-case var clauses else)))))
