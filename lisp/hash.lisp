;;; Returns an almost prime number >= num.  See CMUCL.
(defun round-prime (num)
  (setq num (ceiling num))
  (when (= (rem num 2) 0) (incf num 1))
  (when (= (rem num 3) 0) (incf num 2))
  (when (= (rem num 7) 0) (incf num 4))
  num)

(defun illegal-hash-test (test)
  (error "Hash test ~s is not one of EQ, EQL, EQUAL or EQUALP." test))

;;; To the Lisp user:
;;; - SIZE: COUNT can be <= SIZE before rehashing: N-BUCKETS*THREHSOLD
;;; - THRESHOLD: Load factor before rehashing: SIZE/N-BUCKETS
;;;
;;; Internally:
;;; - N-BUCKETS: size/threshold

(defun compute-n-buckets (size threshold)
  (round-prime (/ size threshold)))

(defun compute-size (n-buckets threshold)
  (floor (* n-buckets threshold)))

(defun resize-hash-table (table)
  (let* ((size (hash-table-size table))
	 (rehash-size (hash-table-rehash-size table))
	 (load (hash-table-rehash-threshold table))
	 (new-n (compute-n-buckets (if (integerp rehash-size)
				       (+ size rehash-size)
				       (* size rehash-size))
				   load)))
    (open-address-hash-table-n-buckets-setter
     (integer-index new-n) table)
    (open-address-hash-table-size-setter
     (integer-index (compute-size new-n load)) table))
  (clrhash table))

(defun MAKE-HASH-TABLE (&key (test 'eql)
			     (rehash-threshold 0.8f0)
			     (size (* 83 rehash-threshold))
			     (rehash-size 200))
  (let* ((test-name (cond ((symbolp test) test)
			  ((eq test #'eq) 'eq)
			  ((eq test #'eql) 'eql)
			  ((eq test #'equal) 'equal)
			  ((eq test #'equalp) 'equalp)))
	 (rehash-threshold (min rehash-threshold 0.85f0))
	 (n-buckets (compute-n-buckets size rehash-threshold))
	 (size (compute-size n-buckets rehash-threshold)))
    (make-open-address-hash-table
     (find-type
      (case test-name
	(eq 'open-address-eq-hash-table)
	(eql 'open-address-eql-hash-table)
	(equal 'open-address-equal-hash-table)
	(equalp 'open-address-equalp-hash-table)))
     (integer-index size) rehash-size (integer-index n-buckets))))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                   ADDITIONAL ACCESSORS                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod HASH-TABLE-REHASH-THRESHOLD ((table open-address-hash-table))
  (/ (float (hash-table-size table))
     (hash-table-n-buckets table)))

(defmethod HASH-TABLE-SIZE ((table open-address-hash-table))
  (open-address-hash-table-size table))
(defmethod HASH-TABLE-COUNT ((table open-address-hash-table))
  (open-address-hash-table-count table))
(defmethod HASH-TABLE-REHASH-SIZE ((table open-address-hash-table))
  (open-address-hash-table-rehash-size table))

(defmethod HASH-TABLE-TEST ((table open-address-eq-hash-table)) 'eq)
(defmethod HASH-TABLE-TEST ((table open-address-eql-hash-table)) 'eql)
(defmethod HASH-TABLE-TEST ((table open-address-equal-hash-table)) 'equal)
(defmethod HASH-TABLE-TEST ((table open-address-equalp-hash-table)) 'equalp)
(defun hash-table-n-buckets (table) (open-address-hash-table-n-buckets table))

(defun HASH-TABLE-P (x) (typep x 'hash-table))

;;; This can be substantially speed up by copying the internal structure.
(defun copy-hash-table (x)
  (let ((copy (make-hash-table
	       :test (hash-table-test x)
	       :size (hash-table-size x)
	       :rehash-size (hash-table-rehash-size x)
	       :rehash-threshold (hash-table-rehash-threshold x))))
    (maphash #'(lambda (key val)
		 (setf (gethash key copy) val))
	     x)
    copy))
			       
	       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                       METHODS                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod CLRHASH ((table open-address-hash-table))
  (open-address-clrhash table))

(defun gethash-values (result default)
  (if (eq result not-found)
      (values default nil)
      (values result t)))
  
(defmethod GETHASH (key (table open-address-eq-hash-table)
			&optional default)
  (gethash-values (open-address-eq-gethash key table not-found) default))
(defmethod GETHASH (key (table open-address-eql-hash-table)
			&optional default)
  (gethash-values (open-address-eql-gethash key table not-found) default))
(defmethod GETHASH (key (table open-address-equal-hash-table)
			&optional default)
  (gethash-values (open-address-equal-gethash key table not-found) default))
(defmethod GETHASH (key (table open-address-equalp-hash-table)
			&optional default)
  (gethash-values (open-address-equalp-gethash key table not-found) default))


(defmethod REMHASH (key (table open-address-eq-hash-table))
  (open-address-eq-remhash key table))
(defmethod REMHASH (key (table open-address-eql-hash-table))
  (open-address-eql-remhash key table))
(defmethod REMHASH (key (table open-address-equal-hash-table))
  (open-address-equal-remhash key table))
(defmethod REMHASH (key (table open-address-equalp-hash-table))
  (open-address-equalp-remhash key table))

(defmethod (SETF GETHASH) (new-value key (table open-address-eq-hash-table)
				     &optional default)
  (declare (ignore default))
  (open-address-eq-sethash key table new-value))
(defmethod (SETF GETHASH) (new-value key (table open-address-eql-hash-table)
				     &optional default)
  (declare (ignore default))
  (open-address-eql-sethash key table new-value))
(defmethod (SETF GETHASH) (new-value key (table open-address-equal-hash-table)
				     &optional default)
  (declare (ignore default))
  (open-address-equal-sethash key table new-value))
(defmethod (SETF GETHASH) (new-value key (table open-address-equalp-hash-table)
				     &optional default)
  (declare (ignore default))
  (open-address-equalp-sethash key table new-value))

(defmethod Rehash ((table open-address-eq-hash-table))
  (open-address-eq-rehash table) nil)
(defmethod Rehash ((table open-address-eql-hash-table))
  (open-address-eql-rehash table) nil)
(defmethod Rehash ((table open-address-equal-hash-table))
  (open-address-equal-rehash table) nil)
(defmethod Rehash ((table open-address-equalp-hash-table))
  (open-address-equalp-rehash table) nil)

(defmethod MAPHASH (function (hash-table open-address-hash-table))
  (open-address-maphash function hash-table))

