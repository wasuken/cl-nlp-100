(defpackage cl-nlp-100/tests/main
  (:use :cl
        :cl-nlp-100
		:cl-ppcre
		:gzip-stream
        :rove))
(in-package :cl-nlp-100/tests/main)

;;; common
(defun hash-table-eq (a b)
  (let ((a-key-value-lst (maphash #'(lambda (k v) (cons k v)) a))
		(b-key-value-lst (maphash #'(lambda (k v) (cons k v)) b)))
	(equal a-key-value-lst b-key-value-lst)))

(defun clean-files (&rest files)
  (dolist (file files)
	(when (probe-file file)
	  (delete-file (probe-file file)))))

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

(deftest act-2
	(let ((filepath "./files/hightemp.txt"))
	  (testing "problem 10"
			   (ok (= (cl-nlp-100:p-10 filepath) 24)))
	  (testing "problem 11"
			   (ok (string= (cl-nlp-100:p-11 filepath)
							(cl-nlp-100:slurp "./files/hightemp_tab_to_space.txt"))))
	  (testing "problem 12"
			   (clean-files "./files/col_1.txt" "./files/col_2.txt")
			   (cl-nlp-100:p-12 filepath "./files/col_1.txt" "./files/col_2.txt")
			   (ok (string= (cl-nlp-100:slurp "./files/col_1.txt")
							(cl-nlp-100:slurp "./files/cmd_cut_1.txt")))
			   (ok (string= (cl-nlp-100:slurp "./files/col_2.txt")
							(cl-nlp-100:slurp "./files/cmd_cut_2.txt")))
			   (clean-files "./files/col_1.txt" "./files/col_2.txt"))
	  (testing "problem 13"
			   (clean-files "./files/col1_2_merged.txt")
			   (cl-nlp-100:p-13 "./files/col1_2_merged.txt"
								"	"
								"./files/cmd_cut_1.txt"
								"./files/cmd_cut_2.txt")
			   (ok (string= (cl-nlp-100:slurp "./files/col1_2_merged.txt")
							(cl-nlp-100:slurp "./files/cmd_col1_2_merged.txt")))
			   (clean-files "./files/col1_2_merged.txt"))
	  (testing "problem 14"
			   (ok (string= (cl-nlp-100:p-14 "./files/hightemp.txt" 3)
							(cl-nlp-100:slurp "./files/cmd_head3.txt"))))
	  (testing "problem 15"
			   (ok (string= (cl-nlp-100:p-15 "./files/hightemp.txt" 3)
							(cl-nlp-100:slurp "./files/cmd_tail3.txt"))))
	  (testing "problem 16"
			   (clean-files "./files/split_test.txt0" "./files/split_test.txt1"
							"./files/split_test.txt2")
			   (cl-nlp-100:p-16 "./files/split_test.txt" 3)
			   (ok (string= (cl-nlp-100:slurp "./files/split_test.txt0")
							(cl-nlp-100:slurp "./files/cmd_split_00")))
			   (ok (string= (cl-nlp-100:slurp "./files/split_test.txt1")
							(cl-nlp-100:slurp "./files/cmd_split_01")))
			   (ok (string= (cl-nlp-100:slurp "./files/split_test.txt2")
							(cl-nlp-100:slurp "./files/cmd_split_02")))
			   (clean-files "./files/split_test.txt0" "./files/split_test.txt1"
							"./files/split_test.txt2"))
	  (testing "problem 17"
	  		   (ok (string= (cl-nlp-100:p-17 "./files/hightemp.txt" 0)
	  						(cl-nlp-100:slurp "files/cmd_sort_uniq.txt"))))
	  ;; 一致しなくてもよいらしいので...
	  ;; (testing "problem 18"
	  ;; 		   (ok (string= (cl-nlp-100:p-18 "./files/hightemp.txt" 2)
	  ;; 						(cl-nlp-100:slurp "files/cmd_3_sort.txt"))))
	  ;; (testing "problem 19"
	  ;; 		   (ok (string= (cl-nlp-100:p-19 "./files/hightemp.txt" 0)
	  ;; 						(cl-nlp-100:slurp "files/cmd_1_count_sort.txt"))))
	  ))

(deftest act-3
  ;; ここ妥協
  (when (not (probe-file "./files/act3.json"))
	(cl-nlp-100:download-file "./files/act3.json.gz"
							  "http://www.cl.ecei.tohoku.ac.jp/nlp100/data/jawiki-country.json.gz")
	(gzip-stream:gunzip "./files/act3.json.gz" "./files/act3.json"))
  (print (cl-nlp-100:p-20 "./files/act3.json" "イギリス" "./files/english_contents.txt"))
  ;; (testing "problem 20"
  ;; 	(string= (cl-nlp-100:slurp "./files/p-20.json")
  ;; 			 (cl-nlp-100:slurp "./files/english_contents.txt")))
  )
