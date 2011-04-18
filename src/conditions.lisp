(in-package :cl-pattern)
(use-syntax annot-syntax)

@export
(define-condition match-error (error)
  ())

(defun %match-error ()
  (error (make-condition 'match-error)))
