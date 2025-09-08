;;;; package.lisp

(defpackage #:cl-coverage-tools
  (:use #:cl #:sb-c)
  (:export
   #:enable-coverage
   #:coverage-data
   #:source-map-from-source
   #:normalize-coverage-data
   #:process-coverage-data))
