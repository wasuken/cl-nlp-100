(defsystem "cl-nlp-100"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on ("cl-ppcre" "parse-float")
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "cl-nlp-100/tests"))))

(defsystem "cl-nlp-100/tests"
  :author ""
  :license ""
  :depends-on ("cl-nlp-100"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for cl-nlp-100"
  :perform (test-op (op c) (symbol-call :rove :run c)))
