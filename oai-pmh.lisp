
(in-package #:cl-arxiv-api)

;;; Here we will write fetching of metadata (in a bulk fashion, through OAI-PMH protocol)

(defparameter *oai-pmh-url* "http://export.arxiv.org/oai2/")

;; Ok, which queries can I write, I wonder?

;; Let's write a simple Identify request first...

(defparameter *known-verbs* '(:identify :get-record
			      :list-identifiers :list-metadata-formats
			      :list-records :list-sets))

(defun camelcaseize (name)
  (format nil "~{~a~}" (mapcar #'string-capitalize (cl-ppcre:split "-" (string-downcase name)))))

(defun subcamcaseize (name)
  (let ((lst (cl-ppcre:split "-" (string-downcase name))))
    (format nil "~{~a~}" (cons (car lst)
			       (mapcar #'string-capitalize (cdr lst))))))

(defun group2 (lst)
  (iter (generate elt in lst)
	(let ((one (next elt))
	      (two (next elt)))
	  (collect (list one two)))))

(defun compose-oai-pmh-request (verb &rest args)
  (if (not (find verb *known-verbs* :test (lambda (x y)
					    (string= (string x) (string y)))))
      (error "Unknown OAI-PMH verb: ~a. Please, refer to the docs" verb))
  (format nil "~a?verb=~a~a"
	  *oai-pmh-url* (camelcaseize verb)
	  (if args
	      (format nil "?~{~a~^&~}" (mapcar (lambda (x)
						 #?"$((subcamcaseize (car x)))=$((escape-url-query (cadr x)))")
					       (group2 args)))
	      "")))

(defun arxiv-identify ()
  (%parse-arxiv-response (http-simple-get (compose-oai-pmh-request :identify))))

(defun arxiv-get-record (identifier metadata-prefix)
  (%parse-arxiv-response
   (http-simple-get (compose-oai-pmh-request :get-record
					     :identifier identifier
					     :metadata-prefix metadata-prefix))))

(defun arxiv-list-identifiers (metadata-prefix &key from until set resumption-token)
  (%parse-arxiv-response
   (http-simple-get (apply #'compose-oai-pmh-request
			   `(:list-identifiers
			     ,@(if from `(:from ,from))
			     ,@(if until `(:until ,until))
			     :metadata-prefix ,metadata-prefix
			     ,@(if set `(:set ,set))
			     ,@(if resumption-token `(:resumption-token ,resumption-token)))))))

(defun arxiv-list-metadata-formats (&key identifier)
  (%parse-arxiv-response
   (http-simple-get (apply #'compose-oai-pmh-request
			   `(:list-metadata-formats
			     ,@(if identifier `(:identifier ,identifier)))))))

;; OK, maybe metadata-prefix is not always required after all ...

(defun arxiv-list-records (metadata-prefix &key from until set resumption-token)
  (%parse-arxiv-response
   (http-simple-get (apply #'compose-oai-pmh-request
			   `(:list-records
			     ,@(if from `(:from ,from))
			     ,@(if until `(:until ,until))
			     :metadata-prefix ,metadata-prefix
			     ,@(if set `(:set ,set))
			     ,@(if resumption-token `(:resumption-token ,resumption-token)))))))

(defun arxiv-list-sets (&key resumption-token)
  (%parse-arxiv-response
   (http-simple-get (apply #'compose-oai-pmh-request
			   `(:list-sets
			     ,@(if resumption-token `(:resumption-token ,resumption-token)))))))


;; Now I can successfully do very basic requests.
;; I wonder, how to recognize resumption tokens in replies, how to use them
;; ... and how to fetch all the metadata I need for, say 1992 and 1993
;; ... I also wonder, how much disk space it would eventually use?
