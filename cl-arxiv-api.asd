;;;; cl-arxiv-api.asd

(asdf:defsystem #:cl-arxiv-api
  :description "Bindings for API of arXiv.org"
  :author "Alexandr Popolitov <popolit@gmail.com>"
  :license "MIT"
  :serial t
  :version "0.1"
  :depends-on (#:trivial-http #:cl-interpol #:cl-ppcre #:iterate #:cxml)
  :components ((:file "package")
	       (:file "compose-query")
	       (:file "parse-query")
               (:file "cl-arxiv-api")))

