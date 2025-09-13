(defpackage #:cl-coverage-reporter.cli
  (:use #:cl)
  (:export
   #:report))

(in-package :cl-coverage-reporter.cli)

(defun system-files (system-designator)
  "Return a list of pathname objects for all files in SYSTEM-DESIGNATOR."
  (let* ((sys (asdf:find-system system-designator))
         (components (asdf:component-children sys)))
    (labels ((collect-files (component)
               (etypecase component
                 (asdf:cl-source-file
                  (list (asdf:component-pathname component)))
                 (asdf:module
                  (mapcan #'collect-files (asdf:component-children component)))
                 (asdf:system
                  (mapcan #'collect-files (asdf:component-children component))))))
      (mapcan #'collect-files components))))

(defun calculate (expressions)
  (let ((results (list :expression-hit 0
                       :expression-missed 0
                       :branch-hit 0
                       :branch-missed 0)))
    (loop for expression in expressions
          do (destructuring-bind (kind executed position length)
                 expression
               (declare (ignorable position length))
               (incf (getf results (if executed
                                       (if (eq :expression kind)
                                           :expression-hit
                                           :branch-hit)
                                       (if (eq :expression kind)
                                           :expression-missed
                                           :branch-missed)))))
          finally (return results))))

(defun report (systems)
  "Write a report to *standard-output* for the selected SYSTEMS.
 NOTE: For now, it only works for systems that contains a single package."
  (labels ((run (pkg files table)
             (loop for file in files
                   for filename = (namestring file)
                   collect (progn
                             (sb-cover::refresh-coverage-info filename)
                             (let* ((data (calculate
                                           (cl-coverage-tools:process-coverage-data
                                            pkg
                                            filename))))
                               (ascii-table:add-row table (list filename
                                                                (getf data :branch-hit)
                                                                (getf data :expression-hit)
                                                                (getf data :branch-missed)
                                                                (getf data :expression-missed))))))))
    (let* ((table (ascii-table:make-table
                   '("File" "Branches" "Expressions" "Branches missed"  "Expressions missed")
                   :header "Coverage"))
           (results (loop for system-name in systems
                          collect (run (uiop:find-package* system-name)
                                       (system-files system-name)
                                       table))))
      (ascii-table:display table))))
