
(in-package #:cl-arxiv-api)

;;; Here we will write fetching of metadata (in a bulk fashion, through OAI-PMH protocol)

(defparameter *oai-pmh-url* "http://export.arxiv.org/oai2/")

;; Ok, which queries can I write, I wonder?

;; Let's write a simple Identify request first...

(defparameter *known-verbs* '(:identify :get-record
			      :list-identifiers :list-metadata-formats
			      :list-records :list-sets
			      :nasty-verb))

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

(defun arxiv-nasty-verb ()
  (%parse-arxiv-response (http-simple-get (compose-oai-pmh-request :nasty-verb))))

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

(define-condition oai-pmh-error (error simple-condition)
  ((errors :initarg :errors)
   (rest :initarg rest)))
(define-condition bad-argument (oai-pmh-error) ())
(define-condition bad-resumption-token (oai-pmh-error) ())
(define-condition bad-verb (oai-pmh-error) ())
(define-condition cannot-disseminate-format (oai-pmh-error) ())
(define-condition id-does-not-exist (oai-pmh-error) ())
(define-condition no-records-match (oai-pmh-error) ())
(define-condition no-metadata-formats (oai-pmh-error) ())
(define-condition no-set-hierarchy (oai-pmh-error) ())

;; I can check that:
;; * response field matches the request verb sent
;; * error is among the ones that should be issued by this verb

(defparameter *valid-errors* '((:bad-argument . :all-verbs)
			       (:bad-resumption-token :list-identifiers :list-records :list-sets)
			       ;; OK, this is really a KLUDGE
			       (:bad-verb . :all-verbs)
			       (:cannot-disseminate-format :get-record :list-identifiers :list-records)
			       (:id-does-not-exist :get-record :list-metadata-formats)
			       (:no-records-match :list-identifiers :list-records)
			       (:no-metadata-formats :list-metadata-formats)
			       (:no-set-hierarchy :list-sets :list-identifiers :list-records)))

;; Would be cool if I could propagate errors from XML-fields to actual Common Lisp exceptions
;; But what to do, if there are multiple errors in the same response?
;; Well, in this case I can just signal generic OAI-PMH-ERROR and it will crash the app

  
(defun error-message (err)
  (cadr err))

(defparameter *err-code->sym* '(("badArgument" . bad-argument)
				("badResumptionToken" . bad-resumption-token)
				("badVerb" . bad-verb)
				("cannotDisseminateFormat" . cannot-disseminate-format)
				("idDoesNotExist" . id-does-not-exist)
				("noRecordsMatch" . no-records-match)
				("noMetadataFormats" . no-metadata-formats)
				("noSetHierarchy" . no-set-hierarchy)))

(defun error-symbol (err)
  (let ((it (cadr (assoc "code" (car err) :test #'equal))))
    (or (cdr (assoc it *err-code->sym* :test #'equal))
	it)))
	      

(defun parse-oai-pmh-response (response)
  (if (not (equal '("OAI-PMH" . "http://www.openarchives.org/OAI/2.0/") (car response)))
      (error "Expected response to be OAI-PMH v2.0, but got: ~a, please investigate manually."
	     (car response)))
  (let ((errors nil)
	(rest nil))
    (iter (for elt in (cdr response))
	  (cond ((and (stringp elt) (string= #?"\n" elt))
		 (next-iteration))
		((equal "error" (caar elt)) (push (cdr elt) errors))
		(t (push elt rest))))
    (setf errors (nreverse errors)
	  rest (nreverse rest))
    (when errors
      (if (equal 1 (length errors))
	  (error (error-symbol (car errors))
		 :format-control "~a"
		 :format-arguments (list (error-message (car errors)))
		 :rest rest)
	  (error 'oai-pmh-error :errors errors :rest rest)))
    rest))

	
    
	  
	
