(in-package :cl-pattern)
(use-syntax annot-syntax)

(defun partition-match-clauses (clauses)
  (loop with groups
        with group-type
        with group-clauses
        for clause in clauses
        for head-pattern = (caar clause)
        for head-pattern-type = (pattern-type head-pattern)
        do
     (or group-type (setf group-type head-pattern-type))
     (if (eq head-pattern-type group-type)
         (push clause
               group-clauses)
         (progn
           (push (cons group-type (nreverse group-clauses)) groups)
           (setf group-type head-pattern-type
                 group-clauses (list clause))))
        finally
     (if group-type
         (push (cons group-type (nreverse group-clauses)) groups))
     (return (nreverse groups))))

(defun compile-match-variable (vars clauses else)
  `(%match
    ,(cdr vars)
    ,(loop for ((var . rest) . then) in clauses
           if (string-equal var "_")
             collect `(,rest ,@then)
           else
             collect `(,rest (let ((,var ,(car vars)))
                               ,@then)))
    ,else))

(defun compile-match-constant (vars clauses else)
  (loop with alist
        for ((constant . rest) . then) in clauses
        for sub-clause = (cons rest then)
        for assoc = (assoc constant alist :test #'equal)
        if assoc
          do (setf (cdr assoc) (cons sub-clause (cdr assoc)))
        else
          do (push `(,constant ,sub-clause) alist)
        finally
     (return
       `(%case ,(car vars)
          ,(loop for pair in (nreverse alist)
                 for constant = (car pair)
                 for sub-clauses = (nreverse (cdr pair))
                 collect
                 `(,constant
                    (%match ,(cdr vars) ,sub-clauses (%match-error))))
          ,else))))

(defun compile-match-constructor (vars clauses else)
  (let ((var (car vars)))
    (with-gensyms (car cdr)
      `(if (consp ,var)
           (let ((,car (car ,var))
                 (,cdr (cdr ,var)))
             (declare (ignorable ,car ,cdr))
             (%match
              ,`(,car ,cdr ,@(cdr vars))
              ,(loop for (((par . pdr) . rest) . then) in clauses
                     collect `(,`(,par ,pdr ,@rest) ,@then))
              ,else))
           ,else))))

(defun compile-match-optional-null (restvars clauses else)
  `(%match
    ,`(,@restvars)
    ,(loop for ((pattern . rest) . then) in clauses
           for fv = (cdr pattern)
           collect
           (cons `(,@rest)
                  (if fv
                      `((let ,fv (declare (ignorable ,@fv)) ,@then))
                      then)))
    ,else))

(defun compile-match-optional-constructor (var restvars clauses else)
  (with-gensyms (car cdr)
    `(let ((,car (car ,var))
           (,cdr (cdr ,var)))
       (declare (ignorable ,car ,cdr))
       (%match
        ,`(,car ,cdr ,@restvars)
        ,(loop for ((pattern . rest) . then) in clauses
               append
               (loop with pfv = (free-variables (cdr pattern))
                     for sub-pattern in (optional-patterns (cdr pattern))
                     for (par . pdr) = sub-pattern
                     for sfv = (free-variables sub-pattern)
                     for fv = (remove-if (lambda (v) (member v sfv)) pfv)
                     collect
                     (cons `(,par ,pdr ,@rest)
                            (if fv
                                `((let ,fv (declare (ignorable ,@fv)) ,@then))
                                then))))
        ,else))))

(defun compile-match-optional (vars clauses else)
  (let ((var (car vars)))
    `(cond
       ((null ,var)
        ,(compile-match-optional-null (cdr vars) clauses else))
       ((consp ,var)
        ,(compile-match-optional-constructor var (cdr vars) clauses else))
       (t ,else))))

(defun compile-match-empty (clauses else)
  (loop for (pattern . then) in clauses
        if (null pattern)
          do (return
               (if (> (length then) 1)
                   `(progn ,@then)
                   (car then)))
        finally (return else)))

(defun compile-match-group (vars group else)
  (if vars
      (ecase (car group)
        (:var (compile-match-variable vars (cdr group) else))
        (:const (compile-match-constant vars (cdr group) else))
        (:cons (compile-match-constructor vars (cdr group) else))
        (:optional (compile-match-optional vars (cdr group) else)))
      (compile-match-empty (cdr group) else)))

(defun compile-match-groups (vars groups else)
  (reduce (lambda (group else)
            (compile-match-group vars group else))
          groups
          :initial-value else
          :from-end t))
