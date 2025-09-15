(asdf:defsystem #:cl-coverage-reporter.cli
  :author "Bruno Dias"
  :version "1.0.0"
  :serial t
  :depends-on (#:cl-ascii-table
               #+sbcl sb-cover
               #:cl-coverage-tools)
  :components ((:file "cli-coverage")))
