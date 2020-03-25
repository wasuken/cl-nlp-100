(defpackage cl-nlp-100
  (:use :cl :cl-ppcre)
  ;; ここ最高にゴミでは。
  (:export #:p-0 #:p-1 #:p-2 #:p-3 #:p-4 #:p-5 #:p-6 #:p-7 #:p-8 #:p-9 #:p-10
		   #:p-11 #:p-12 #:p-13 #:p-14 #:p-15 #:p-16 #:p-17 #:p-18 #:p-19 #:p-20
		   #:p-21 #:p-22 #:p-23 #:p-24 #:p-25 #:p-26 #:p-27 #:p-28 #:p-29 #:p-30
		   #:p-31 #:p-32 #:p-33 #:p-34 #:p-35 #:p-36 #:p-37 #:p-38 #:p-39 #:p-40
		   #:p-41 #:p-42 #:p-43 #:p-44 #:p-45 #:p-46 #:p-47 #:p-48 #:p-49 #:p-50
		   #:p-51 #:p-52 #:p-53 #:p-54 #:p-55 #:p-56 #:p-57 #:p-58 #:p-59 #:p-60
		   #:p-61 #:p-62 #:p-63 #:p-64 #:p-65 #:p-66 #:p-67 #:p-68 #:p-69 #:p-70
		   #:p-71 #:p-72 #:p-73 #:p-74 #:p-75 #:p-76 #:p-77 #:p-78 #:p-79 #:p-80
		   #:p-81 #:p-82 #:p-83 #:p-84 #:p-85 #:p-86 #:p-87 #:p-88 #:p-89 #:p-90
		   #:p-91 #:p-92 #:p-93 #:p-94 #:p-95 #:p-96 #:p-97 #:p-98 #:p-99
		   #:p-100))
(in-package :cl-nlp-100)

;;; Common
(defun string->string-one (text)
  (mapcar #'(lambda (x) (coerce `(,x) 'string)) (concatenate 'list text)))

(defun text->words (text)
  (ppcre:split " " (ppcre:regex-replace-all ",|\\." text "")))

(defun str-take (str num)
  (labels ((st (sl n)
			 (cond ((or (zerop n) (zerop (length sl)))
					"")
				   (t (concatenate 'string (car sl) (st (cdr sl) (1- n)))))))
	(st (string->string-one str) num)))

(defun take (lst n)
  (cond ((not (or (zerop n) (zerop (length lst))))
		 (cons (car lst) (take (cdr lst) (1- n))))
		(t nil)))

(defun drop (lst n)
  (cond ((not (or (zerop n) (zerop (length lst))))
		 (drop (cdr lst) (1- n)))
		(t lst)))

;;; Problems
;;; 00
(defun p-0 (str)
  (labels ((r (lst)
			 (cond (lst (concatenate 'string (r (cdr lst)) (car lst)))
				   (t ""))))
	(r (string->string-one str))))

;;; 01
(defun p-1 (str)
  (labels ((td-cycle (lst td)
			 (cond ((not lst) "")
				   (td (concatenate 'string (car lst) (td-cycle (cdr lst) (not td))))
				   (t (concatenate 'string (td-cycle (cdr lst) (not td)))))))
	(td-cycle (string->string-one str) t)))

;;; 02
(defun p-2 (a b)
  (reduce #'(lambda (sum x)
			  (concatenate 'string sum (car x) (cdr x)))
		  (mapcar #'cons (string->string-one a) (string->string-one b))
		  :initial-value ""))

;;; 03
(defun p-3 (str)
  (mapcar #'length (text->words str)))

;;; 04
(defun p-4 (str first-ones)
  (let ((tbl (make-hash-table))
		(cnt 1))
	(dolist (word (text->words str))
	  (if (find cnt first-ones)
		  (setf (gethash (intern (car (string->string-one word))) tbl) cnt)
		  (setf (gethash (intern (str-take word 2)) tbl) cnt))
	  (incf cnt))
	tbl))

;;; 05
(defun p-5 (str num wordp)
  (let ((nodes (if wordp
				   (text->words str)
				   (string->string-one (ppcre:regex-replace-all ",|\\." str "")))))
	(labels ((n-gram (lst n)
			   (cond ((>= (length lst) n)
					  (cons (take lst n) (n-gram (drop lst 1) n)))
					 (t nil))))
	  (n-gram nodes num))))
