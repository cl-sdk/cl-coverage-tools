(in-package #:cl-coverage-tools)

(defun enable-coverage ()
  "Enable coverage.
 NOTE: It's sbcl-only for now."
  #+sbcl (progn t
                (require :sb-cover)
                (declaim (optimize sb-cover:store-coverage-data)))
  #-sbcl (warn "not implemented"))

(defun coverage-data ()
  "Return the coverage data collected by `sb-cover`.

 The coverage data is a hash-table where the keys are the filenames
 and values are a list of pairs (FORM-DATA . EXECUTED).

 EXECUTED = BOOLEAN

 REVERSE-PATH = (LIST INTEGER)

 FORM-DATA = ((:THEN REVERSE-PATH) . EXECUTED)
          | ((:ELSE REVERSE-PATH) . EXECUTED)
          | (REVERSE-PATH  . EXECUTED)

 -- NOTE: This is the type of what we want:
 PATH = (reverse REVERSE-PATH) = (cons FORM-INDEX &rest (LIST INTEGER))

 Example:

 ((1 1 3 1) . T)

 - Last \"1\" is the FORM-INDEX to access on the list of forms
 and locations that will be generated later on using `read-and-record-source-map`.
 - (3 1 1) - path to walk to find the form.

   0 1 2 3|0 1|0 1
 ( a b c ( x ( m n ) y)) => n"
  #+sbcl (car sb-int::*code-coverage-info*)
  #-sbcl nil)

(defun source-map-from-source (pkg source)
  (with-input-from-string (stream source)
    (loop with sb-cover::*current-package* = pkg
          with map = nil
          with form = nil
          with eof = nil
          for i from 0
          do (setf (values form map)
                   (handler-case
                       (sb-cover::read-and-record-source-map stream)
                     (end-of-file ()
                       (setf eof t))
                     (error (error)
                       (warn "Error when recording source map for toplevel form ~A:~%  ~A" i error)
                       (values nil
                               (make-hash-table)))))
          until eof
          when map
            collect (cons form map))))

(defun normalize-coverage-data (records)
  "Get the coverage data recorded from `sb-cover`
 to a normalized data form.

 (list kind executed form-index form-path)"
  (let ((group-by (make-hash-table :test 'equal)))
    (loop for record in records
          for form-data = (car record)
          for executed = (cdr record)
          if (member (caar record) '(:then :else))
            do (destructuring-bind (kind &rest rest)
                   form-data
                 (let ((form-path (reverse (butlast rest))))
                   (setf
                    (gethash form-path group-by)
                    (push (list
                           kind
                           executed
                           (alexandria:lastcar rest)
                           form-path)
                          (gethash form-path group-by)))))
          else
            do (let ((form-path (reverse (butlast form-data))))
                 (setf
                  (gethash form-path group-by)
                  (push (list :expression
                              executed
                              (alexandria:lastcar form-data)
                              form-path)
                        (gethash form-data group-by))))
          finally (return group-by))
    (let (result)
      (maphash (lambda (source-path expressions)
                 (declare (ignorable source-path))
                 (setf result
                       (append (if (= (length expressions) 1)
                                   expressions
                                   (remove-if (lambda (expression)
                                                (equal :expression (car expression)))
                                              expressions))
                               result)))
               group-by)
      result)))

(defun process-coverage-data (pkg filename)
  "Given the package PKG and its related file, process the data coverage

 Returns in a form (list (list kind executed position length))"
  (let* ((source (uiop:read-file-string filename))
         (hashtable (sb-cover::code-coverage-hashtable))
         (coverage-data (normalize-coverage-data (gethash filename hashtable)))
         (maps (source-map-from-source pkg source))
         result)
    ;; calculate locations
    (dolist (record coverage-data)
      (print record)
      (destructuring-bind (kind executed form-index form-path)
          record
        (handler-case
            (let ((source-form (car (nth form-index maps)))
                  (source-map (cdr (nth form-index maps))))
              (multiple-value-bind (start end)
                  (sb-cover::source-path-source-position (cons 0 form-path)
                                                         source-form
                                                         source-map)
                (push (list kind executed start (- end start)) result)))
          (error ()
            (warn "Error finding source location for source path ~A in file ~A~%" form-path filename)))))
    result))
