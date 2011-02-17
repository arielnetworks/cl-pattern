(in-package :cl-pattern)

(annot:enable-annot-syntax)

(defmacro %match (vars clauses else)
  (let ((groups (partition-match-clauses clauses)))
    (compile-match-groups vars groups else)))

@export
(defmacro match* (args &body clauses)
  (loop for arg in args
        for var = (gensym "VAR")
        if (atom arg)
          collect arg into vars
        else
          collect var into vars
          and collect `(,var ,arg) into bindings
        finally
     (return
       (let ((body `(%match ,vars ,clauses (%match-error))))
         (if bindings
             `(let ,bindings ,body)
             body)))))

@export
(defmacro match (arg &body clauses)
  `(match* (,arg)
     ,@(loop for (pattern . then) in clauses
             collect `((,pattern) ,@then))))
