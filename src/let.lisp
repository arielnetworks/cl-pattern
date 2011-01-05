(in-package :cl-pattern)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun malformed-binding ()
    (error "Malformed let+ binding"))

  (defun bind (bindings body &key test)
    (let ((binding (car bindings))
          (body (if (cdr bindings)
                    (list (bind (cdr bindings) body :test test))
                    body)))
      (typecase binding
        (symbol (sbind binding nil body :test test))
        (cons (let ((value (car (last binding)))
                    (names (butlast binding)))
                (case (length names)
                  (0 (malformed-binding))
                  (1 (dbind (car names) value body :test test))
                  (t (mbind names value body :test test)))))
        (otherwise (malformed-binding)))))

  (defun mbind (vars value body &key test)
    (let (new-vars ignore-vars bindings)
      (dolist (var vars)
        (cond
          ((ignore-variable-p var)
           (push var new-vars)
           (push (make-ignore-variable) ignore-vars))
          ((variable-p var)
           (push var new-vars))
          (t (push (list var (gensym "VAR")) bindings))))
      `(multiple-value-bind ,(nreverse new-vars) ,value
         ,@(when ignore-vars
             `((declare (ignore ,@ignore-vars))))
         ,@(if bindings
               `((let+/test ,test ,(nreverse bindings) ,@body))
               body))))
  
  (defun dbind (pattern value body &key test)
    (cond
      ((unquoted-form-p pattern)
       (destructuring-bind (pattern conditions ignore-vars)
           (compile-pattern pattern :test test)
         `(destructuring-bind ,pattern ,value
            ,@(when ignore-vars
                `((declare (ignore ,@ignore-vars))))
            ,@(when conditions
                `((unless (and ,@conditions)
                    (error "match error"))))
            ,@body)))
      (t (sbind pattern value body :test test))))

  (defun sbind (name value body &key test)
    (cond
      ((ignore-variable-p name)
       (with-gensyms (var)
         `(let ((,var ,value))
            (declare (ignore ,var))
            ,@body)))
      ((symbolp name)
       `(let ((,name ,value)) ,@body))
      (t
       `(if (funcall ,test ,name ,value)
            (progn ,@body)
            (error "match error")))))

  (defun compile-pattern (pattern &key test)
    (cond
      ((variable-p pattern)
       (if (ignore-variable-p pattern)
           (let ((ignore-var (make-ignore-variable)))
             (list ignore-var nil (list ignore-var)))
           (list pattern nil nil)))
      ((unquoted-form-p pattern)
       (destructuring-bind (car car-conditions car-ignore-vars)
           (compile-pattern (car pattern) :test test)
         (if (cdr pattern)
             (destructuring-bind (cdr cdr-conditions cdr-ignore-vars)
                 (compile-pattern (cdr pattern) :test test)
               (list (cons car cdr)
                     (append car-conditions cdr-conditions)
                     (append car-ignore-vars cdr-ignore-vars)))
             (list (cons car nil) car-conditions car-ignore-vars))))
      (t
       (with-gensyms (var)
         (list var `((funcall ,test ,var ,pattern)) nil))))))

(defmacro let+/test (test bindings &body body)
  (bind bindings body :test test))

(defmacro let+ (bindings &body body)
  (bind bindings body :test #'equal))
