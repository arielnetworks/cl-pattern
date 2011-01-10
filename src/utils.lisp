(in-package :cl-pattern)

(defun variable-p (symbol)
  (and symbol
       (symbolp symbol)
       (not (keywordp symbol))))

(defun make-ignore-variable ()
  (gensym "IGNORE"))

(defun ignore-variable-p (symbol)
  (and (symbolp symbol)
       (string-equal (symbol-name symbol) "_")))

(defun otherwise-variable-p (symbol)
  (or (eq symbol t)
      (eq symbol 'otherwise)
      (ignore-variable-p symbol)))

(defun quoted-form-p (form)
  (and (consp form)
       (eq (car form) 'quote)))

(defun unquoted-form-p (form)
  (and (consp form)
       (not (quoted-form-p form))))

(defun unquote-form (form)
  (if (quoted-form-p form)
      (cadr form)
      form))

