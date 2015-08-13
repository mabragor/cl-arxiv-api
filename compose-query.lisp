
(in-package #:cl-arxiv-api)

;;; here we will write DSL for conveniently composing queries to arXiv API

(defparameter *arxiv-api-url* "http://export.arxiv.org/api/")

(defparameter *method-names* '("query"))

(defparameter *query-params-abbrevs*
  '((:title . "ti") (:author . "au") (:abstract . "abs") (:comment . "co")
    (:journal-reference . "jr") (:category . "cat") (:report-number . "rn")
    (:id . "id") (:all . "all")))

(defparameter *query-connectives*
  '((:and . "AND") (:or . "OR") (:andnot . "ANDNOT")))

(defun query-head (query)
  (intern (string (car query)) "KEYWORD"))

(defun atomic-query-p (query)
  (assoc (query-head query) *query-params-abbrevs*))

(defun composite-query-p (query)
  (assoc (query-head query) *query-connectives*))


(defun serialize-query (query &optional (toplevel t))
  "Transform query form Lisp-form to string form"
  (cond ((atomic-query-p query) ...)
	((composite-query-p query) ...)
	(t (error "I don't know this arXiv query: ~a, perhaps you misspelled it?" (car query)))))
  
;; I want to be able to write something like
;; (author (and "Morozov" "Mironov"))
;; and not
;; (and (author "Morozov") (author "Mironov"))
;; in other words, I want "atomic" words to zzap through boolean operators

;; The engine of the API is smart enough to just allow me to do that, without
;; need to manually zzap

;; Suddenly, there is also EXACT word, which occurs if I use double quotes ... interesting


