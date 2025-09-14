(defpackage #:cl-coverage-reporter.cov
  (:use #:cl)
  (:export
   #:report))

(in-package :cl-coverage-reporter.cov)

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

(defun report (systems)
  "Write a cov report to *standard-output* for the selected SYSTEMS
 that can be loaded on a emacs plug-in.
 NOTE: For now, it only works for systems that contains a single package."
  (labels ((run (pkg files)
             (loop for file in files
                   for filename = (namestring file)
                   collect (progn
                             (sb-cover::refresh-coverage-info filename)
                             (let ((data (cl-coverage-tools:process-coverage-data
                                          pkg
                                          filename))
                                   (output-filename (merge-pathnames
                                                     (concatenate 'string
                                                                  filename
                                                                  (namestring #P".cov"))
                                                     (namestring #P".cov"))))
                               (with-open-file (output output-filename :direction :output
                                                       :if-does-not-exist :create
                                                       :if-exists :supersede)
                                 (dolist (record data)
                                   (destructuring-bind (kind executed position length)
                                       record
                                     (declare (ignorable kind))
                                     (write-string (format nil "EXP:~D,~D,~D~%" executed position length) output)))))))))
    (loop for system-name in systems
          collect (run (uiop:find-package* system-name)
                       (system-files system-name))))
  t)
