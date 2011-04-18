(in-package :cl-pattern)
(use-syntax annot-syntax)

@export
(defvar *unbound* nil)

@export
(defmacro pattern-equal (pattern value)
  `(equal ,pattern ,value))

(defmacro %equal (pattern value)
  (typecase pattern
    (cons `(pattern-equal ',(cadr pattern) ,value))
    (t `(pattern-equal ,pattern ,value))))

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

(defun free-variables-bindings (vars)
  (mapcar (lambda (var)
            (list var '*unbound*))
          vars))

(defun optional-patterns (pattern)
  (if (consp pattern)
      (mapcar (lambda (sub-pattern)
                (cons (car pattern)
                      sub-pattern))
              (cons nil
                    (optional-patterns (cdr pattern))))
      pattern))
