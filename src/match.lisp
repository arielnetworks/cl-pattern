(in-package :cl-pattern)
(use-syntax annot-syntax)

(defmacro %match (vars clauses else)
  (let ((groups (partition-match-clauses clauses)))
    (compile-match-groups vars groups else)))

(defmacro match-values (args &body clauses)
  (loop for arg in args
        for var = (gensym "VAR")
        if (atom arg)
          collect arg into vars
        else
          collect var into vars
          and collect `(,var ,arg) into bindings
        finally
     (return
       (let ((then `(%match ,vars ,clauses (%match-error))))
         (if bindings
             `(let ,bindings ,then)
             then)))))

@export
(defmacro with-match-parameters ((&key (test 'equal) unbound) &body body)
  `(macrolet ((pattern-equal (pattern value)
                (list ',test pattern value)))
     (symbol-macrolet ((pattern-unbound ,unbound))
       ,@body)))

@export
(defmacro match* (arg &body clauses)
  `(match-values (,arg)
     ,@(loop for (pattern . then) in clauses
             collect `((,pattern) ,@then))))

@export
(defmacro match (arg &body clauses)
  `(with-match-parameters ()
     (match* ,arg ,@clauses)))

@export
(defmacro lambda-match (&body clauses)
  (with-gensyms (arg)
    `(lambda (,arg)
       (match ,arg ,@clauses))))
