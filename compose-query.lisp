
(in-package #:cl-arxiv-api)

;;; here we will write DSL for conveniently composing queries to arXiv API

(cl-interpol:enable-interpol-syntax)

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
  (and (listp query)
       (assoc (query-head query) *query-params-abbrevs*)))

(defun composite-query-p (query)
  (and (listp query)
       (assoc (query-head query) *query-connectives*)))

(defun %serialize-query (query &optional toplevel)
  (cond ((atomic-query-p query) (format nil "~a: ~a"
					(cdr (assoc (query-head query) *query-params-abbrevs*))
					(%serialize-query (cadr query))))
	((composite-query-p query)
	 (let ((it (format nil #?"~a $((cdr (assoc (query-head query) *query-connectives*))) ~a"
			   (%serialize-query (cadr query))
			   (if (equal 3 (length query))
			       (%serialize-query (caddr query))
			       (%serialize-query (cons (car query) (cddr query)))))))
	   (if (not toplevel)
	       #?"($(it))"
	       it)))
	((stringp query)
	 (let ((it (cl-ppcre:split "(\\s|\\t)+" query)))
	   (if (equal 1 (length it))
	       (car it)
	       (if toplevel
		   (%serialize-query (cons :and it) t)
		   #?"EXACT $((joinl "_" it))"))))
	(t (error "I don't know this arXiv query: ~a, perhaps you misspelled it?" query))))

(defun serialize-query (query)
  "Transform query form Lisp-form to string form"
  (%serialize-query query t))

(defun arxiv-get (query)
  (http-get #?"$(*arxiv-api-url*)query?search_query=$((escape-url-query (serialize-query query)))"))
  
;; I want to be able to write something like
;; (author (and "Morozov" "Mironov"))
;; and not
;; (and (author "Morozov") (author "Mironov"))
;; in other words, I want "atomic" words to zzap through boolean operators

;; The engine of the API is smart enough to just allow me to do that, without
;; need to manually zzap

;; Suddenly, there is also EXACT word, which occurs if I use double quotes ... interesting


