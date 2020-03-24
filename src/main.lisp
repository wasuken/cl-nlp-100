(defpackage cl-nlp-100
  (:use :cl))
(in-package :cl-nlp-100)

;;; Common
(defun string->string-one (text)
  (mapcar #'(lambda (x) (coerce `(,x) 'string)) (concatenate 'list text)))

;;; Problems
;;; 1
(defun rvs (str)
  (labels ((r (lst)
			 (cond (lst `(,(car lst) ,(r (cdr lst)))))))
	(format nil "~{~A~}" (r (string->string-one str)))))
