;;;; cl-arxiv-api.asd

(asdf:defsystem #:cl-arxiv-api
  :description "Bindings for API of arXiv.org"
  :author "Alexandr Popolitov <popolit@gmail.com>"
  :license "MIT"
  :serial t
  :depends-on (#:trivial-http)
  :components ((:file "package")
	       (:file "compose-query")
	       (:file "parse-query")
               (:file "cl-arxiv-api")))

