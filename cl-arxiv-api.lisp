;;;; cl-arxiv-api.lisp

(in-package #:cl-arxiv-api)

(defun arxiv-get (query &key
		  (start nil start-p) (max-results nil max-results-p) (id-list nil id-list-p)
                          (sort-by nil sort-by-p) (sort-order nil sort-order-p)
                          (auto-retry nil auto-retry-p))
  (parse-arxiv-response (apply #'arxiv-get-raw `(,query ,@(if start-p `(:start ,start))
							,@(if max-results-p `(:max-results ,max-results))
							,@(if id-list-p `(:id-list ,id-list))
							,@(if sort-by-p `(:sort-by ,sort-by))
							,@(if sort-order-p `(:sort-order ,sort-order))
                                                        ,@(if auto-retry-p `(:auto-retry ,auto-retry))))))

