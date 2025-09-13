(asdf:defsystem #:cl-coverage-reporter.cli
  :author "Bruno Dias"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-ascii-table
               #+sbcl sb-cover
               #:cl-coverage-tools)
  :components ((:file "cli-coverage")))
