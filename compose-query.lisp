
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
		   #?"EXACT $((format nil "~{~a~^_~}" it))"))))
	(t (error "I don't know this arXiv query: ~a, perhaps you misspelled it?" query))))

(defun serialize-query (query)
  "Transform query form Lisp-form to string form"
  (%serialize-query query t))

(define-condition http-get-error (error simple-condition)
  ((code :initarg :code)
   (headers :initarg :headers)))

(defmethod print-object ((condition http-get-error) stream)
  (if *print-escape*
      (call-next-method)
      (format stream "HTTP-GET-ERROR code: ~a, headers: ~a"
	      (slot-value condition 'code)
	      (slot-value condition 'headers))))

(defun http-simple-get (request)
  (destructuring-bind (code headers stream)
      (http-get request)
    (if (equal 200 code)
	(format nil "~{~a~^~%~}"
		(iter (for line in-stream stream using #'read-line)
		      (collect line)))
	(error 'http-get-error :code code :headers headers))))

(defun http-retrying-get (request)
  (handler-case (http-simple-get request)
    (http-get-error (e) (if (equal 503 (slot-value e 'code))
			    (let ((delay (let ((it (cdr (assoc :retry-after (slot-value e 'headers)))))
					   (if it
					       (parse-integer it)
					       10))))
			      (warn "Got code 503, retrying after ~a seconds" delay)
			      (sleep delay)
			      (http-retrying-get request))
			    (error e)))))

(let ((sort-by-map '((:relevance . "relevance") (:rel . "relevance")
		     (:last-updated . "lastUpdatedDate") (:update . "lastUpdatedDate")
		     (:submitted . "submittedDate") (:submit . "submittedDate")
		     ))
      (sort-order-map '((:ascending . "ascending") (:asc . "ascending")
			(:descending . "descending") (:desc . "descending")
			)))
  (defun arxiv-get-raw (query &key
			(start nil start-p) (max-results nil max-results-p) (id-list nil id-list-p)
                                (sort-by nil sort-by-p) (sort-order nil sort-order-p)
                                (auto-retry nil))
    (let ((query-str (if query #?"search_query=$((escape-url-query (serialize-query query)))"))
	  (start-str (if start-p #?"start=$(start)"))
	  (max-results-str (if max-results-p #?"max_results=$(max-results)"))
	  (id-list-str (if id-list-p (format nil "id_list=~{~a~^,~}" id-list)))
	  (sort-by-str (if sort-by-p #?"sortBy=$((cdr (assoc sort-by sort-by-map)))"))
	  (sort-order-str (if sort-order-p #?"sortOrder=$((cdr (assoc sort-order sort-order-map)))")))
      ;; (format t "id list: ~a~%" id-list-str)
      (let ((final-query (format nil #?"~aquery?~a"
                                 *arxiv-api-url*
                                 (format nil "~{~a~^&~}"
                                         (remove-if-not #'identity
                                                        (list query-str id-list-str start-str max-results-str
                                                              sort-by-str sort-order-str))))))
        ;; (format t "final query: ~a~%" final-query)
        (if auto-retry
            (http-retrying-get final-query)
            (http-simple-get final-query))))))
  
;; I want to be able to write something like
;; (author (and "Morozov" "Mironov"))
;; and not
;; (and (author "Morozov") (author "Mironov"))
;; in other words, I want "atomic" words to zzap through boolean operators

;; The engine of the API is smart enough to just allow me to do that, without
;; need to manually zzap

;; Suddenly, there is also EXACT word, which occurs if I use double quotes ... interesting


;; (defparameter *a* (arxiv-get '(:author (:and "Popolitov" "Morozov"))))

;; OK, it seems that I don't need an access through OAI-MPH, because it's too coarce --
;; using it I can download only all metadata

;; I can track papers of a given author(s) by keeping my own database
;; of papers

;; Some manual work is required to correctly filter wrong people from search results,
;; but this should be doable.
