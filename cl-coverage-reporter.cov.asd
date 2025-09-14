(asdf:defsystem #:cl-coverage-reporter.cov
  :author "Bruno Dias"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-ascii-table
               #+sbcl sb-cover
               #:cl-coverage-tools)
  :components ((:file "cov-coverage")))
