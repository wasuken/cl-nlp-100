(defpackage cl-nlp-100/tests/main
  (:use :cl
        :cl-nlp-100
        :rove))
(in-package :cl-nlp-100/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-nlp-100)' in your Lisp.

(deftest act-1
	(testing "problem 1"
			 (let* ((str "stressed")
				   (expected (reverse str)))
			   (ok (equal (rvs str) expected)))))
