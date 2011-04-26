(defpackage cl-pattern
  (:nicknames :pattern)
  (:use :cl)
  (:import-from :alexandria
                :with-gensyms)
  (:import-from :syntax
                :use-syntax)
  (:import-from :syntax-annot
                :annot-syntax)
  (:import-from :annot.eval-when
                :eval-always)
  (:import-from :annot.class
                :export-accessors))
