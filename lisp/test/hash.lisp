;;; test printing of hash tables!!!
;;; Should time access with different keys and tests!!!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 16.1 HASH TABLE FUNCTIONS                                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; MAKE-HASH-TABLE
(deftest hash-table-default
    (let ((h (make-hash-table)))
      (and (zerop (hash-table-count h))
	   (integerp (hash-table-size h))
	   (numberp (hash-table-rehash-size h))
	   (> (hash-table-rehash-size h) 1)
	   (floatp (hash-table-rehash-threshold h))
	   (<= 0 (hash-table-rehash-threshold h) 1)
	   (eq (hash-table-test h) 'eql)))
  t)

(deftest hash-table-test
    (and (eq (hash-table-test (make-hash-table :test #'eq)) 'eq)
	 (eq (hash-table-test (make-hash-table :test #'eql)) 'eql)
	 (eq (hash-table-test (make-hash-table :test #'equal)) 'equal)
	 #-(and cmu (not eclipse))
	 (eq (hash-table-test (make-hash-table :test #'equalp)) 'equalp))
  t)

(deftest hash-table-size
    (let* ((asked-for 10)
	   (h (make-hash-table :size asked-for))
	   (original (hash-table-size h)))
      (dotimes (n (1+ asked-for)) (setf (gethash n h) n))
      (<= asked-for original (hash-table-size h)))
  t)
(deftest hash-table-size2
    (let* ((asked-for 10)
	   (h (make-hash-table :size asked-for :rehash-threshold 1.0))
	   (original (hash-table-size h)))
      (dotimes (n (1+ asked-for)) (setf (gethash n h) n))
      (<= asked-for original (hash-table-size h)))
  t)

#+not-now
(deftest hash-table-big
    (let* ((asked-for 4096)
	   (h (make-hash-table :size asked-for))
	   (original (hash-table-size h)))
      (dotimes (n (1+ asked-for)) (setf (gethash n h) n))
      (<= asked-for original (hash-table-size h)))
  t)

#+not-now
(deftest hash-table-arithmetic-rehash-size
    (let* ((asked-for 1024)
	   (increment 5)
	   (h (make-hash-table :size asked-for :rehash-size increment))
	   (original (hash-table-size h)))
      (dotimes (n (+ 2 asked-for)) (setf (gethash n h) n))
      (<= asked-for original (+ original increment)
	  (hash-table-size h) (* original 1.9)))
  t)

#+not-now
(deftest hash-table-geometric-rehash-size
    (let* ((asked-for 1024)
	   (increment 5.0)
	   (h (make-hash-table :size asked-for :rehash-size increment))
	   (original (hash-table-size h)))
      (dotimes (n (+ 2 asked-for)) (setf (gethash n h) n))
       (<= asked-for original (* original increment)
	  (hash-table-size h)))
  t)

#+not-now
(deftest hash-table-threshold
    (let* ((asked-for 1024)
	  (h (make-hash-table :size asked-for :rehash-size 5
			      :rehash-threshold 0.5))
	  (sizes (list (hash-table-size h))))
      (dotimes (n (1+ asked-for))
	(setf (gethash n h) (1+ n))
	(push (hash-table-size h) sizes))
      (apply #'>= sizes))
  t)

;;; HASH-TABLE-P
(deftest hash-table-p (hash-table-p (make-hash-table)) t)
(deftest hash-table-p-nil (hash-table-p (make-array 24)) nil)

;;; HASH-TABLE-COUNT
(deftest hash-table-count
    (let* ((h (make-hash-table))
	   (counts (list (hash-table-count h))))
      (dotimes (n 10)
	(setf (gethash n h) n)
	(push (hash-table-count h) counts))
      (and (apply #'> counts) (= (car counts) 10)))
  t)

;;; HASH-TABLE-REHASH-SIZE
(deftest hash-table-rehash-size-integer
    (let ((s (hash-table-rehash-size (make-hash-table :rehash-size 5))))
      (and (integerp s) (>= s 5)))
  t)
(deftest hash-table-rehash-size-float
    (let ((s (hash-table-rehash-size (make-hash-table :rehash-size 5.0))))
      (and (floatp s) (>= s 5.0)))
  t)

;;; HASH-TABLE-REHASH-THRESHOLD
(deftest hash-table-rehash-threshold
    (let ((s (hash-table-rehash-threshold (make-hash-table :rehash-threshold 0.5))))
      (and (floatp s) (<= 0.0 s 1.0)))
  t)

;;; HASH-TABLE-SIZE
;; see above

;;; HASH-TABLE-TEST
;; see above

;;; GETHASH
(deftest gethash
    (let ((table (make-hash-table)))
      (multiple-value-call #'values
	(gethash 1 table)
	(gethash 1 table 2)
	(setf (gethash 1 table) "one")
	(setf (gethash 2 table) "two")
	(gethash 1 table)
	(gethash 2 table)
	(gethash nil table)
	(setf (gethash nil table) nil)
	(gethash nil table)))
  nil nil 2 nil "one" "two" "one" t "two" t nil nil nil nil t)

(deftest gethash2
    (let ((table (make-hash-table :rehash-threshold 1.0)))
      (multiple-value-call #'values
	(gethash 1 table)
	(gethash 1 table 2)
	(setf (gethash 1 table) "one")
	(setf (gethash 2 table) "two")
	(gethash 1 table)
	(gethash 2 table)
	(gethash nil table)
	(setf (gethash nil table) nil)
	(gethash nil table)))
  nil nil 2 nil "one" "two" "one" t "two" t nil nil nil nil t)

;;; REMHASH
(deftest remhash
    (let ((table (make-hash-table)))
      (multiple-value-call #'values
	(setf (gethash 100 table) "C")
	(gethash 100 table)
	(remhash 100 table)
	(gethash 100 table)
	(remhash 100 table)))
  "C" "C" t t nil nil nil)

(deftest remhash2
    (let ((table (make-hash-table :rehash-threshold 1.0)))
      (multiple-value-call #'values
	(setf (gethash 100 table) "C")
	(gethash 100 table)
	(remhash 100 table)
	(gethash 100 table)
	(remhash 100 table)))
  "C" "C" t t nil nil nil)

;;; MAPHASH
(deftest maphash
    (let ((table (make-hash-table)))
      (dotimes (i 10) (setf (gethash i table) i))
      (let ((sum-of-squares 0))
	(maphash #'(lambda (key val)
		     (let ((square (* val val)))
		       (incf sum-of-squares square)
		       (setf (gethash key table) square)))
		 table)
	(multiple-value-call #'values
	  sum-of-squares
	  (hash-table-count table)
	  (maphash #'(lambda (key val)
		       (when (oddp val) (remhash key table)))
		   table)
	  (hash-table-count table))))
  285 10 nil 5)

(deftest maphash2
    (let ((table (make-hash-table :rehash-threshold 1.0)))
      (dotimes (i 10) (setf (gethash i table) i))
      (let ((sum-of-squares 0))
	(maphash #'(lambda (key val)
		     (let ((square (* val val)))
		       (incf sum-of-squares square)
		       (setf (gethash key table) square)))
		 table)
	(multiple-value-call #'values
	  sum-of-squares
	  (hash-table-count table)
	  (maphash #'(lambda (key val)
		       (when (oddp val) (remhash key table)))
		   table)
	  (hash-table-count table))))
  285 10 nil 5)
	  
;;; WITH-HASH-TABLE-ITERATOR
(deftest with-hash-table-iterator
    (let ((table (make-hash-table)))
      (dotimes (i 10) (setf (gethash i table) i))
      (let ((sum-of-squares 0))
	(with-hash-table-iterator (next table)
	  (loop (multiple-value-bind (entryp key val) (next)
		  (if entryp
		      (let ((square (* val val)))
			(incf sum-of-squares square)
			(setf (gethash key table) square))
		    (return)))))
	(multiple-value-call #'values
	  sum-of-squares
	  (hash-table-count table)
	  (with-hash-table-iterator (next table)
	    (loop (multiple-value-bind (entryp key val) (next)
		    (if entryp
			(when (oddp val) (remhash key table))
		      (return)))))
	  (hash-table-count table))))
  285 10 nil 5)

(deftest with-hash-table-iterator2
    (let ((table (make-hash-table :rehash-threshold 1.0)))
      (dotimes (i 10) (setf (gethash i table) i))
      (let ((sum-of-squares 0))
	(with-hash-table-iterator (next table)
	  (loop (multiple-value-bind (entryp key val) (next)
		  (if entryp
		      (let ((square (* val val)))
			(incf sum-of-squares square)
			(setf (gethash key table) square))
		    (return)))))
	(multiple-value-call #'values
	  sum-of-squares
	  (hash-table-count table)
	  (with-hash-table-iterator (next table)
	    (loop (multiple-value-bind (entryp key val) (next)
		    (if entryp
			(when (oddp val) (remhash key table))
		      (return)))))
	  (hash-table-count table))))
  285 10 nil 5)

;;; CLRHASH
(deftest clrhash
    (let ((table (make-hash-table)))
      (dotimes (i 100) (setf (gethash i table) (format nil "~r" i)))
      (multiple-value-call #'values
	(hash-table-count table)
	(gethash 57 table)
	(null (clrhash table))
	(hash-table-count table)
	(gethash 57 table)))
  100 "fifty-seven" t nil 0 nil nil)

(deftest clrhash2
    (let ((table (make-hash-table :rehash-threshold 1.0)))
      (dotimes (i 100) (setf (gethash i table) (format nil "~r" i)))
      (multiple-value-call #'values
	(hash-table-count table)
	(gethash 57 table)
	(null (clrhash table))
	(hash-table-count table)
	(gethash 57 table)))
  100 "fifty-seven" t nil 0 nil nil)

;;; different hash-table tests
(deftest equalp-hash-table
    (let* ((tests (list #'eq #'eql #'equal
			#-(and cmu (not eclipse)) #'equalp))
	   (x 1) (y 2)
	   (sets `(((1 . a) (1 . b))
		   ((,(complex x y) . 1)
		    (,(complex x y) . 2))
		   ((,(list 'a 'b 'c) . 1) (,(list 'a 'b 'c) . 2))
		   (("abc" . 1) (,(copy-seq "abc") . 2))
		   (("def" . 1) ("DEF" . 2))
		   ((,(list* 'a 'b 'c) . 1) (,(list* 'a 'b 'c) . 2))))
	   all-results)
      (declare (special x y))		;defeat compiler coalescing
      (dolist (test tests)
	(let ((h (make-hash-table :test test)))
	  (dolist (set sets)
	    (dolist (pair set)
	      (setf (gethash (car pair) h) (cdr pair))))
	  (let ((results (list (hash-table-count h)
			       (hash-table-test h))))
	    (dolist (set sets)
	      (push (gethash (caar set) h) results))
	    (push (nreverse results) all-results))))
      (nreverse all-results))
  ((EQ 11 B 1 1 1 1 1)		;Assuming = fixnums are eq
   (EQL 10 B 2 1 1 1 1)		;= integeral complex must be eql
   (EQUAL 7 B 2 2 2 1 2)
   #-(and cmu (not eclipse))
   (EQUALP 6 B 2 2 2 2 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 16.2 PRIMITIVE HASH FUNCTION                                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest sxhash1
    (= (sxhash (list 'list "ab"))
       (sxhash (list 'list (copy-seq "ab"))))
  t)
(deftest sxhash2
    (= (sxhash "a") (sxhash (copy-seq "a")))
  t)

(deftest sxhash-char (= (sxhash #\a) (sxhash #\a)) t)
(deftest sxhash-string (= (sxhash "abc") (sxhash "abc")) t)
(deftest sxhash-bv (= (sxhash #*100) (sxhash #*100)) t)

