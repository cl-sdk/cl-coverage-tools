(ql:quickload '(:cl-coverage-tools
                :cl-coverage-reporter.cov))

(cl-coverage-tools:enable-coverage)


(defpackage #:example
  (:use #:cl))

(in-package :example)

(defun fn-unused () nil)

(defun fn-then (x)
  (if (= 1 x)
      'ok
      'fail))

(defun fn-else (x)
  (if (= 1 x)
      'ok
      'fail))

(defun fn-both (x)
  (if (= 1 x)
      'ok
      'fail))

(5am:def-suite example.suite)
(5am:in-suite example.suite)

(5am:def-test test-ok ()
  (5am:is (equal (fn-then 1) 'ok)))

(5am:def-test test-fail ()
  (5am:is (equal (fn-else 0) 'fail)))

(5am:def-test test-both-ok ()
  (5am:is (equal (fn-both 1) 'ok)))

(5am:def-test test-both-fail ()
  (5am:is (equal (fn-both 0) 'fail)))
