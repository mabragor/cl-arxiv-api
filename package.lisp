;;;; package.lisp

(defpackage #:cl-arxiv-api
  (:use #:cl #:trivial-http #:iterate #:cxml)
  (:export #:arxiv-get-raw #:arxiv-get #:parse-arxiv-response))

