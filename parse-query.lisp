
(in-package #:cl-arxiv-api)



;; (enable-quasiquote-2.0)

;;; Here we will parse response of arXiv, which is in Atom 1.0 format

(defun %parse-arxiv-response (str)
  (parse str (cxml-xmls:make-xmls-builder)))

(defun parse-arxiv-response (str)
  (try-to-descend (%parse-arxiv-response str)))

(define-condition arxiv-parse-error (error simple-condition) ())
(defmacro fail-parse ()
  `(error 'arxiv-parse-error))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *parsers* (make-hash-table :test #'equal))
  (defparameter *default-namespace* :atom)
  (defparameter *namespace-map* '((:atom . "http://www.w3.org/2005/Atom")
				  (:opensearch . "http://a9.com/-/spec/opensearch/1.1/")
				  (:arxiv . "http://arxiv.org/schemas/atom")))
  (defun ensure-namespace-free (thing)
    (if (atom thing)
	thing
	(car thing)))
  (defun understand-spec (spec)
    (destructuring-bind (name space) (if (not (atom spec))
					 (list (car spec) (cdr (assoc (cadr spec) *namespace-map*)))
					 (list spec (cdr (assoc *default-namespace* *namespace-map*))))
      (values (cond ((stringp name) name)
		    ((symbolp name) (cg-common-ground::subcamcaseize name))
		    (t (error "Don't know how to coerce this to an XML identifier: ~a" name)))
	      (cond ((stringp name) (destringify-symbol name "KEYWORD"))
		    ((symbolp name) (intern (string name) "KEYWORD"))
		    (t (error "Don't know how to coerce this to keyword: ~a" name)))
	      space))))
	

(defun empty-line-p (smth)
  (and (stringp smth)
       (cl-ppcre:all-matches-as-strings "^(\\n|\\s|\\t|\\r)*$" smth)
       t))
  
			
;; (defun %car-eq-p (to-what smth)
;;   (and (consp smth)
;;        (let ((it (car smth)))
;; 	 (destructuring-bind (to-what-car to-what-cdr) to-what
;; 	   (handler-case (destructuring-bind (it-car it-cdr) it
;; 			   ...))))))
    
	
;; (defmacro car-eq (&rest to-what)
;;   `(%car-eq-p ',to-what elt))

(defmacro define-parser (thing &body body)
    (multiple-value-bind (str-name kwd-name namespace) (understand-spec thing)
      `(setf (gethash (cons ,str-name ,namespace) *parsers*)
	     (lambda (it)
	       (let ((kwd ,kwd-name))
		 (declare (ignorable kwd))
		 ,@body)))))
	       

(defmacro define-default-parser (thing)
  `(define-parser ,thing
       (cons kwd (caddr it))))

(defmacro define-default-parser-for (&rest things)
  `(progn ,@(mapcar (lambda (x)
		      `(define-default-parser ,x))
		    things)))

(define-default-parser-for :title :id :updated :published :summary :name
			   (:comment :arxiv) (:doi :arxiv))

(defun try-to-descend (smth)
  (if (not (consp smth))
      (fail-parse)
      (let ((parser (gethash (car smth) *parsers*)))
	(if (not parser)
	    (fail-parse)
	    (funcall parser smth)))))

(defun parse-as-list (smth)
  (iter (for elt in smth)
	(format t "~a~%" elt)
	(when (not (empty-line-p elt))
	  (handler-case (collect (try-to-descend elt))
	    (arxiv-parse-error () (warn "Failed to parse ~s, collecting verbatim" elt)
			       (collect elt))))))


(define-parser :feed
  (parse-as-list (cddr it)))

(define-parser :entry
  (cons :entry (parse-as-list (cddr it))))

(define-parser :link
  (let ((it (parse-as-list (cadr it))))
    (let ((name (car (or (cdr (assoc "rel" it :test #'equal))
			 (cdr (assoc "title" it :test #'equal))
			 (fail-parse))))
	  (href (car (or (cdr (assoc "href" it :test #'equal))
			 (fail-parse)))))
      (cons (intern #?"$((string-upcase name))-LINK" "KEYWORD") href))))

(defmacro define-integer-parser (thing)
  `(define-parser ,thing
     (cons kwd (parse-integer (caddr it)))))

(defmacro define-integer-parser-for (&rest things)
  `(progn ,@(mapcar (lambda (x)
		      `(define-integer-parser ,x))
		    things)))

(define-integer-parser-for
    (:total-results :opensearch)
    (:start-index :opensearch)
  (:items-per-page :opensearch))

(define-parser :author
  (let ((it (cdr (assoc :name (parse-as-list (cddr it))))))
    (if (not it)
	(fail-parse)
	(cons kwd it))))

(define-parser :category
  (let ((it (cadr (assoc "term" (parse-as-list (cadr it)) :test #'equal))))
    (if (not it)
	(fail-parse)
	(cons kwd it))))

(define-parser ("primary_category" :arxiv)
  (let ((it (cadr (assoc "term" (parse-as-list (cadr it)) :test #'equal))))
    (if (not it)
	(fail-parse)
	(cons :primary-category it))))

(define-parser ("journal_ref" :arxiv)
  (cons :journal-ref (caddr it)))

(defun hash->assoc (hash)
  (iter (for (key val) in-hashtable hash)
	(collect (cons key val))))

;; (defun simplify-arxiv-response (lst)
;;   (remove-if-not #'identity
;; 		 (iter (for elt in lst)
;; 		       (collect (if (empty-line-p elt)
;; 				    nil
;; 				    (top-down-parse (elt :default-namespace :atom)
;; 				      (:link `(:link . ,
;; 				      (:entry (simplify-response-entry
;; 				      ))))))
	