(defpackage cl-nlp-100/tests/main
  (:use :cl
        :cl-nlp-100
		:cl-ppcre
        :rove))
(in-package :cl-nlp-100/tests/main)

;;; common
(defun hash-table-eq (a b)
  (let ((a-key-value-lst (maphash #'(lambda (k v) (cons k v)) a))
		(b-key-value-lst (maphash #'(lambda (k v) (cons k v)) b)))
	(equal a-key-value-lst b-key-value-lst)))

;; NOTE: To run this test file, execute `(asdf:test-system :cl-nlp-100)' in your Lisp.

(deftest act-1
  (testing "problem 00"
	(let* ((str "stressed")
		   (expected (reverse str)))
	  (ok (equal (cl-nlp-100:p-0 str) expected))))
  (testing "problem 01"
	(let* ((str "パタトクカシーー")
		   (expected "パトカー"))
	  (ok (equal (cl-nlp-100:p-1 str) expected))))
  (testing "problem 02"
	(let* ((str-a "パトカー")
		   (str-b "タクシー")
		   (expected "パタトクカシーー"))
	  (ok (equal (cl-nlp-100:p-2 str-a str-b) expected))))
  (testing "problem 03"
	(let* ((str "Now I need a drink, alcoholic of course, after the heavy lectures involving quantum mechanics.")
		   (expected '(3 1 4 1 5 9 2 6 5 3 5 8 9 7 9)))
	  (ok (equal (cl-nlp-100:p-3 str) expected))))
  (testing "problem 04"
	(let* ((str "Hi He Lied Because Boron Could Not Oxidize Fluorine. New Nations Might Also Sign Peace Security Clause. Arthur King Can.")
		   (exp-attr-lst '((H . 1)
						   (He . 2)
						   (Li . 3)
						   (Be . 4)
						   (B . 5)
						   (C . 6)
						   (N . 7)
						   (O . 8)
						   (F . 9)
						   (Ne . 10)
						   (Na . 11)
						   (Mi . 12)
						   (Al . 13)
						   (Si . 14)
						   (P . 15)
						   (S . 16)
						   (Cl . 17)
						   (Ar . 18)
						   (K . 19)
						   (Ca . 20)))
		   (expected (make-hash-table)))
	  (dolist (attr exp-attr-lst)
	  	(setf (gethash (car attr) expected) (cdr attr)))
	  (ok (hash-table-eq (cl-nlp-100:p-4 str (mapcar #'cdr
	  												 (remove-if-not #'(lambda (x)
	  																	(= (length (symbol-name (car x))) 1))
	  																exp-attr-lst)))
	  					 expected))
	  ))
  (testing "problem 05"
	(let* ((str "I am an NLPer")
		   (expected-word '(("I" "am") ("am" "an") ("an" "NLPer")))
		   (expected-chr '(("I" " ") (" " "a") ("a" "m") ("m" " ")
						   (" " "a") ("a" "n") ("n" " ") (" " "N")
						   ("N" "L") ("L" "P") ("P" "e") ("e" "r"))))
	  (ok (equal (cl-nlp-100:p-5 str 2 t) expected-word))
	  (ok (equal (cl-nlp-100:p-5 str 2 nil) expected-chr))))
  (testing "problem 06"
	(let ((str-a "paraparaparadise")
		  (str-b "paragraph")
		  (exp-1 '(("a" "d") ("d" "i") ("i" "s") ("s" "e") ("p" "a") ("a" "r") ("a" "g")
				   ("g" "r") ("r" "a") ("a" "p") ("p" "h")))
		  (exp-2 '(("p" "a") ("a" "r") ("r" "a") ("a" "p")))
		  (exp-3 '(("a" "d") ("d" "i") ("i" "s") ("s" "e")) ))
	  (ok (equal (cl-nlp-100:p-6-1 str-a str-b)
				 exp-1))
	  (ok (equal (cl-nlp-100:p-6-2 str-a str-b)
				 exp-2))
	  (ok (equal (cl-nlp-100:p-6-3 str-a str-b)
				 exp-3))
	  (ok (not (cl-nlp-100:p-6-4 str-a str-b '("s" "e"))))))
  (testing "problem 07"
	(let ((x 12)
		  (y "気温")
		  (z 22.4)
		  (expected "12時の気温は22.4"))
	  (ok (equal (cl-nlp-100:p-7 x y z) expected))))
  (testing "problem 08"
	(let ((input-and-expected "hogezzZZほげfuga"))
	  (ok (equal (cl-nlp-100:p-8 (cl-nlp-100:p-8 input-and-expected))
				 input-and-expected))))
  (testing "problem 09"
	(let ((input "I couldn't believe that I could actually understand what I was reading : the phenomenal power of the human mind .")
		  (expected-regex "I .* that I .* what I was .* : the .* of the .* mind"))
	  (ok (ppcre:scan-to-strings expected-regex (cl-nlp-100:p-9 input)))))
  ;; (testing "problem 09 勘違い"
  ;; 	(let ((input "I couldn't believe that I could actually understand what I was reading : the phenomenal power of the human mind .")
  ;; 		  (expected-regex "I .* that I .* what I was .* : the .* of the .* mind"))
  ;; 	  (ok (ppcre:scan-to-strings expected-regex (cl-nlp-100:p-9-m input)))))
  )
