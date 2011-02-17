(in-package :cl-pattern)

(annot:enable-annot-syntax)

@export
(define-condition match-error (error) ())

(defun %match-error ()
  (error (make-condition 'match-error)))
