;;;; cl-coverage-tools.asd

(asdf:defsystem #:cl-coverage-tools
  :description "Tools to help build coverage tools."
  :author "Bruno Dias"
  :license  "Unlicense"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria
               #:closer-mop
               #+sbcl sb-cover)
  :components ((:file "package")
               (:file "cl-coverage-tools")))
