(defpackage cl-nlp-100
  (:use :cl :cl-ppcre)
  ;; ここ最高にゴミでは。
  (:export #:p-0 #:p-1 #:p-2 #:p-3 #:p-4 #:p-5 #:p-6-1 #:p-6-2 #:p-6-3 #:p-6-4
		   #:p-7 #:p-8 #:p-9 #:p-9-m #:p-10
		   #:p-11 #:p-12 #:p-13 #:p-14 #:p-15 #:p-16 #:p-17 #:p-18 #:p-19 #:p-20
		   #:p-21 #:p-22 #:p-23 #:p-24 #:p-25 #:p-26 #:p-27 #:p-28 #:p-29 #:p-30
		   #:p-31 #:p-32 #:p-33 #:p-34 #:p-35 #:p-36 #:p-37 #:p-38 #:p-39 #:p-40
		   #:p-41 #:p-42 #:p-43 #:p-44 #:p-45 #:p-46 #:p-47 #:p-48 #:p-49 #:p-50
		   #:p-51 #:p-52 #:p-53 #:p-54 #:p-55 #:p-56 #:p-57 #:p-58 #:p-59 #:p-60
		   #:p-61 #:p-62 #:p-63 #:p-64 #:p-65 #:p-66 #:p-67 #:p-68 #:p-69 #:p-70
		   #:p-71 #:p-72 #:p-73 #:p-74 #:p-75 #:p-76 #:p-77 #:p-78 #:p-79 #:p-80
		   #:p-81 #:p-82 #:p-83 #:p-84 #:p-85 #:p-86 #:p-87 #:p-88 #:p-89 #:p-90
		   #:p-91 #:p-92 #:p-93 #:p-94 #:p-95 #:p-96 #:p-97 #:p-98 #:p-99
		   #:p-100
		   #:read-lines
		   #:slurp))
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

(defun to-str (x)
  (if (stringp x) x (write-to-string x)))

(defun del-nth (n lst)
  (cond ((zerop n)
		 (cdr lst))
		((<= (length lst) n)
		 lst)
		(t
		 (append (take lst n) (drop lst (1+ n))))))

(defun shuffle-lst (lst)
  (labels ((sf (lst)
			 (if (zerop (length lst))
				 lst
				 (let ((del-pos (random (length lst))))
				   `(,(nth del-pos lst)
					  ,@(sf (del-nth del-pos lst)))))))
	(sf lst)))

(defun part-shuffle-lst (lst begin end)
  (let ((begin-before-target (take lst begin))
		(target-after-end (drop lst end)))
	`(,@begin-before-target
		,@(shuffle-lst (take (drop lst begin) (- end begin)))
		,@target-after-end)))

(defun read-lines (path)
  (with-open-file (s path)
	(loop for line = (read-line s nil)
	   while line
	   collect line)))

(defun slurp (path)
  (format nil "~{~A~^~&~}" (read-lines path)))

;;; Problems

;;; act 1

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

;;; 06
(defun p-6-1 (a b)
  (let ((a-lst (p-5 a 2 nil))
		(b-lst (p-5 b 2 nil)))
	(remove-duplicates (append a-lst b-lst)
					   :test #'equal)))

(defun p-6-2 (a b)
  (let* ((a-lst (p-5 a 2 nil))
		 (b-lst (p-5 b 2 nil))
		 (ab-lst (append a-lst b-lst)))
	(remove-duplicates
	 (remove-if-not #'(lambda (x)
						(<= 2 (count x ab-lst :test #'equal)))
					ab-lst)
	 :test #'equal)))

(defun p-6-3 (a b)
  (let ((a-lst (remove-duplicates (p-5 a 2 nil)))
		(b-lst (remove-duplicates (p-5 b 2 nil))))
	(remove-duplicates
	 (remove-if #'(lambda (x) (find x b-lst :test #'equal)) a-lst))))

(defun p-6-4 (a b tgt)
  (and (find tgt (p-5 a 2 nil) :test #'equal)
	   (find tgt (p-5 b 2 nil) :test #'equal)))

;;; 07
(defun p-7 (x y z)
  (let ((x-to-str (to-str x))
		(y-to-str (to-str y))
		(z-to-str (to-str z)))
	(format nil "~A時の~Aは~A" x-to-str y-to-str z-to-str)))

;;; 08
(defun p-8 (str)
  (let ((chrs (concatenate 'list str)))
	(format nil "~{~A~}" (mapcar #'(lambda (x) (if (and (<= 97 (char-code x))
														(<= (char-code x) 122))
												   (code-char (- 219 (char-code x)))
												   x))
								 chrs))))

;;; 09
(defun p-9 (str)
  (let* ((words (text->words str)))
	(format nil "~{~A ~}"
			(mapcar #'(lambda (x)
						(if (<= (length x) 4)
							x
							(format nil "~{~A~}"
									(part-shuffle-lst (string->string-one x)
											  1
											  (1- (length (string->string-one x)))))))
					words))))

;; (defun p-9-m (str)
;;   (let* ((words (text->words str))
;; 		 (template-lst (mapcar #'(lambda (x) (if (< 4 (length x))
;; 												 (intern "replace")
;; 												 x))
;; 							   words))
;; 		 (shuffle-replace-lst (shuffle-lst (remove-if-not #'(lambda (x) (< 4 (length x))) words))))
;; 	(format nil "~{~A ~}" (reverse (reduce #'(lambda (res x)
;; 											   (cond ((stringp x)
;; 													  (cons x res))
;; 													 (t (let ((first (car shuffle-replace-lst)))
;; 														  (setf shuffle-replace-lst (del-nth 0 shuffle-replace-lst))
;; 														  (cons first res)))))
;; 										   template-lst
;; 										   :initial-value '())))))

;;; act 2

;;; 10
(defun p-10 (path)
  (length (read-lines path)))

;;; 11
(defun p-11 (path)
  (format nil "~{~A~^~%~}" (mapcar #'(lambda (x)
									   (ppcre:regex-replace-all "\\t" x " "))
								   (read-lines path))))

;;; 12
(defun p-12 (path o-path-1 o-path-2)
  (let* ((two-lst (mapcar #'(lambda (x) (let ((sp (ppcre:split "\\t" x)))
										  (cons (car sp) (car (cdr sp)))))
						  (read-lines path))))
	(with-open-file (s o-path-1 :direction :output)
	  (format s  "~{~A~^~%~}" (mapcar #'car two-lst)))
	(with-open-file (s o-path-2 :direction :output)
	  (format s  "~{~A~^~%~}" (mapcar #'cdr two-lst)))))

;;; 13
(defun p-13 (o-path sep i-path-1 i-path-2)
  (with-open-file (s o-path :direction :output)
	(format s "~{~A~^~%~}" (mapcar #'(lambda (x y)
									   (concatenate 'string x sep y))
								   (read-lines i-path-1)
								   (read-lines i-path-2)))))

(defun p-14 (path n)
  (format nil "~{~A~^~&~}" (take (read-lines path) n)))
