(in-package :cl-pattern)
(use-syntax :annot)

@export
@export-accessors
(define-condition match-error (error)
  ((arguments :initarg :arguments
              :initform nil
              :reader match-arguments)
   (patterns :initarg :patterns
             :initform nil
             :reader match-patterns))
  (:report (lambda (condition stream)
             (format stream "Can't match ~A with ~{~S~^ or ~}."
                     (match-arguments condition)
                     (match-patterns condition)))))

(defun %match-error (&optional args patterns)
  (cerror "Continue."
          'match-error
          :arguments args
          :patterns patterns))
