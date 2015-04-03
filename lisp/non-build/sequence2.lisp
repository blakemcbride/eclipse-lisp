;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 14.3 MODIFYING SEQUENCES                                     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod FILL ((sequence list) item &key (start 0) end)
  (loop for sub on (nthcdr start sequence)
	do (list-check-end start end)
	do (rplaca sub item))
  sequence)
(defmethod FILL ((sequence vector) item &key (start 0) end)
  (loop for index from start below (or end (length sequence))
	do (setf (elt sequence index) item))
  sequence)
	

(defmethod REPLACE ((sequence1 list) (sequence2 list)
		      &key (start1 0) end1 (start2 0) end2)
  (when (and (eq sequence1 sequence2)
	     (> start1 start2))
    ;; copy relevent section
    (setq sequence2 (subseq sequence2 start2 end2)
	  start2 0
	  end2 nil))
  (loop for sub1 on (nthcdr start1 sequence1)
	and elt2 in (nthcdr start2 sequence2)
	do (list-check-end start1 end1)
	do (list-check-end start2 end2)
	do (rplaca sub1 elt2))
  sequence1)
(defmethod REPLACE ((sequence1 list) (sequence2 vector)
		    &key (start1 0) end1 (start2 0) end2)
  (with-simple-vector (sequence2 start2 end2)
    (loop for sub1 on (nthcdr start1 sequence1)
	  and i2 from start2 below end2
	  do (list-check-end start1 end1)
	  do (rplaca sub1 (fast-aref sequence2 i2))))
  sequence1)
(macrolet ((def-replace (type set get &optional (type2 type))
	     `(defmethod REPLACE ((sequence1 ,type) (sequence2 ,type2)
				 &key (start1 0) end1 (start2 0) end2)
	       (with-simple-vector (sequence1 start1 end1)
		 (with-simple-vector (sequence2 start2 end2)
		   (if (and (eq sequence1 sequence2)
			    (> start1 start2))
		       ;; Align ends of vector and iterate from-end.
		       (let ((span (min (- end1 start1)
					(- end2 start2))))
			 (loop for i1 from (1- (+ start1 span)) downto start1
			       and i2 from (1- (+ start2 span)) downto start2
			       do (,set sequence1 i1 (,get sequence2 i2))))
		       (loop for i1 from start1 below end1
			     and i2 from start2 below end2
			     do (,set sequence1 i1 (,get sequence2 i2))))))
	       sequence1)))
  (def-replace VECTOR fast-set-aref fast-aref)
  (def-replace BASE-STRING set-base-char-elt base-char-elt)
  (def-replace BASE-STRING set-base-char-elt extended-char-elt EXTENDED-STRING)
  (def-replace EXTENDED-STRING set-extended-char-elt base-char-elt BASE-STRING)
  (def-replace EXTENDED-STRING set-extended-char-elt extended-char-elt))
	   
;;; IWBNI we had a specialized version for bit-vectors!

(defmethod REPLACE ((sequence1 vector) (sequence2 list)
		    &key (start1 0) end1 (start2 0) end2)
  (with-simple-vector (sequence1 start1 end1)
    (loop for i1 from start1 below end1
	  and elt2 in (nthcdr start2 sequence2)
	  do (list-check-end start2 end2)
	  do (fast-set-aref sequence1 i1 elt2)))
  sequence1)
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod substitute-seq (newitem item (sequence LIST)
				   test key
				   (from-end null) start end count)
  (loop with n = 0
	for elt in sequence
	and index from 0
	collect (if (subst-test elt) newitem elt)))


(defmethod remove-duplicate-seq ((sequence LIST) test key
				 (from-end null) start end)
  (loop for sub on sequence
	and index from 0
	and dup-end = end then (when dup-end (1- dup-end))
	for elt = (car sub)
	unless (dup-test elt sub nil 1 dup-end)
	collect elt))

(defmethod nsubstitute-seq (newitem item (sequence LIST)
				    test key
				    (from-end null) start end count)
  (loop with n = 0
	for sub on sequence
	and index from 0
	until (and count (>= n count))
	when (subst-test (car sub))
	do (rplaca sub newitem)
	finally (return sequence)))

	
(defmethod substitute-seq (newitem item (sequence LIST)
				   test key
				   (from-end t) start end count)
  (if count
      (multiple-value-bind (seq length)
	  (reverse-sub-list sequence 0 nil)
	(unless end (setq end length))
	(nreverse (nsubstitute-seq newitem item seq test key nil
				    (- length end) (- length start)
				    count)))
      (substitute-seq newitem item sequence test key nil start end count)))
(defmethod nsubstitute-seq (newitem item (sequence LIST)
				    test key
				    (from-end t) start end count)
  (substitute-seq newitem item sequence test key from-end start end count))

(defmethod remove-seq (item (sequence LIST) test key
			    (from-end t) start end count)
  (multiple-value-bind (seq length)
      (reverse-sub-list sequence 0 nil)
    (unless end (setq end length))
    (nreverse (nremove-seq item seq test key nil
			     (- length end) (- length start)
			     count))))
(defmethod nremove-seq (item (sequence LIST) test key
				    (from-end t) start end count)
  (remove-seq item sequence test key from-end start end count))
(defmethod remove-duplicate-seq ((sequence LIST) test key
				 (from-end t) start end)
  (multiple-value-bind (seq length)
      (reverse-sub-list sequence 0 nil)
    (unless end (setq end length))
    (nreverse (nremove-duplicate-seq seq test key nil
				       (- length end) (- length start)))))
(defmethod nremove-duplicate-seq ((sequence LIST) test key
				  (from-end t) start end)
  (remove-duplicate-seq sequence test key from-end start end))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod substitute-seq (newitem item (sequence VECTOR)
				   test key
				   (from-end null) start end count)
  (loop with n = 0 and length = (length sequence)
	with copy = (make-vector (array-element-type sequence) length)
	for index from 0 below length
	for elt = (elt sequence index)
	do (setf (elt copy index) 
		 (if (subst-test elt) newitem elt))
	finally (return copy)))
(defmethod remove-seq (item (sequence VECTOR) test key
			    (from-end null) start end count)
  (loop with n = 0 and length = (length sequence) and cindex = 0
	with copy = (make-vector (array-element-type sequence) length)
	for index from 0 below length
	for elt = (elt sequence index)
	if (subst-test elt) do (decf length)
	else do (setf (elt copy cindex) elt
		      cindex (1+ cindex))
	finally (return (trim-vector copy length))))
(defmethod remove-duplicate-seq ((sequence VECTOR) test key
				 (from-end null) start end)
  (loop with length = (length sequence) and cindex = 0
	with copy = (make-vector (array-element-type sequence) length)
	for index from 0 below length
	for elt = (elt sequence index)
	if (dup-test elt sequence nil (1+ index) end) do (decf length)
	else do (setf (elt copy cindex) elt
		      cindex (1+ cindex))
	finally (return (trim-vector copy length))))
(defmethod substitute-seq (newitem item (sequence VECTOR)
				   test key
				   (from-end t) start end count)
  (loop with n = 0 and length = (length sequence)
	with copy = (make-vector (array-element-type sequence) length)
	for index from (1- length) downto 0
	for elt = (elt sequence index)
	do (setf (elt copy index) 
		 (if (subst-test elt) newitem elt))
	finally (return copy)))
(defmethod remove-seq (item (sequence VECTOR) test key
			    (from-end t) start end count)
  (loop with n = 0 and length = (length sequence) and cindex = 0
	with copy = (make-vector (array-element-type sequence) length)
	for index from (1- length) downto 0
	for elt = (elt sequence index)
	if (subst-test elt) do (decf length)
	else do (setf (elt copy cindex) elt
		      cindex (1+ cindex))
	finally (return (nreverse (trim-vector copy length)))))
(defmethod remove-duplicate-seq ((sequence VECTOR) test key
				 (from-end t) start end)
  (loop with length = (length sequence) and cindex = 0
	with copy = (make-vector (array-element-type sequence) length)
	for index from (1- length) downto 0
	for elt = (elt sequence index)
	if (dup-test elt sequence t start index) do (decf length)
	else do (setf (elt copy cindex) elt
		      cindex (1+ cindex))
	finally (return (nreverse (trim-vector copy length)))))

(defmethod nsubstitute-seq (newitem item (sequence VECTOR)
				    test key
				    (from-end null) start end count)
  (loop with n = 0 and length = (length sequence)
	for index from 0 below (min (or end length) length)
	until (and count (>= n count))
	when (subst-test (elt sequence index))
	do (setf (elt sequence index) newitem))
  sequence)
(defmethod nremove-seq (item (sequence VECTOR) test key
			     (from-end null) start end count)
  (loop with n = 0 and length = (length sequence) and cindex = 0
	for index from 0 below length
	for elt = (elt sequence index)
	until (and count (>= n count) (= cindex index))
	if (subst-test elt) do (decf length)
	else do (setf (elt sequence cindex) elt
		      cindex (1+ cindex))
	finally (return (trim-vector sequence length))))
(defmethod nremove-duplicate-seq ((sequence VECTOR) test key
				  (from-end null) start end)
  (loop with length = (length sequence) and cindex = 0
	for index from 0 below length
	for elt = (elt sequence index)
	if (dup-test elt sequence nil (1+ index) end) do (decf length)
	else do (setf (elt sequence cindex) elt
		      cindex (1+ cindex))
	finally (return (trim-vector sequence length))))
(defmethod nsubstitute-seq (newitem item (sequence VECTOR)
				    test key
				    (from-end t) start end count)
  (loop with n = 0 and length = (length sequence)
	for index from (1- (min (or end length) length)) downto start
	until (and count (>= n count))
	when (subst-test (elt sequence index))
	do (setf (elt sequence index) newitem))
  sequence)
(defmethod nremove-seq (item (sequence VECTOR) test key
			    (from-end t) start end count)
  (let ((length (length sequence)))
    (nreverse
     (nremove-seq item (nreverse sequence) test key nil
		  (if end (- length end) 0) (- length start) count))))
(defmethod nremove-duplicate-seq ((sequence VECTOR) test key
				  (from-end t) start end)
  (let ((length (length sequence)))
    (nreverse
     (nremove-duplicate-seq (nreverse sequence) test key nil
			    (if end (- length end) 0) (- length start)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun REMOVE-IF-NOT (predicate sequence
		      &key (start 0) end from-end key count)
  (remove-seq nil sequence (complement (itemless-predicate predicate)) key
	      from-end start end count))


(defun DELETE-IF (predicate sequence
			      &key (start 0) end from-end key count)
  (nremove-seq nil sequence (itemless-predicate predicate) key
	      from-end start end count))

(defun DELETE-IF-NOT (predicate sequence
		      &key (start 0) end from-end key count)
  (nremove-seq nil sequence (complement (itemless-predicate predicate)) key
	      from-end start end count))


(defun REMOVE-DUPLICATES (sequence
			    &key (start 0) end from-end
			    key (test #'eql) test-not)
  (remove-duplicate-seq sequence (item-predicate test test-not)
			key from-end start end))
	      

(defun SUBSTITUTE (newitem item sequence
			     &key (start 0) end from-end
			     key (test #'eql)
			     test-not count)
  (substitute-seq newitem item sequence
		  (item-predicate test test-not)
		  key from-end start end count))

(defun SUBSTITUTE-IF (newitem predicate sequence
		  &key (start 0) end from-end key count)
  (substitute-seq newitem nil sequence (itemless-predicate predicate) key
		  from-end start end count))

(defun SUBSTITUTE-IF-NOT (newitem predicate sequence
			  &key (start 0) end from-end key count)
  (substitute-seq newitem nil sequence (complement (itemless-predicate predicate)) key
		  from-end start end count)) 

(defun NSUBSTITUTE (newitem item sequence
			     &key (start 0) end from-end
			     key (test #'eql)
			     test-not count)
  (nsubstitute-seq newitem item sequence
		  (item-predicate test test-not)
		  key from-end start end count))

(defun NSUBSTITUTE-IF (newitem predicate sequence
		  &key (start 0) end from-end key count)
  (nsubstitute-seq newitem nil sequence (itemless-predicate predicate) key
		   from-end start end count))

(defun NSUBSTITUTE-IF-NOT (newitem predicate sequence
			  &key (start 0) end from-end key count)
  (nsubstitute-seq newitem nil sequence
		   (complement (itemless-predicate predicate)) key
		   from-end start end count)) 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 14.4 SEARCHING SEQUENCES FOR ITEMS                           ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The following can be further improved by defining more methods for
;;; special cases.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod position-seq (item (sequence LIST) test key (from-end null) start end)
  (dolist (elt (nthcdr start sequence) nil)
    (list-check-end-noinc start end start)
    (when (funcall test item (key-item key elt)) (return start))
    (incf start)))
(defmethod position-seq (item (sequence LIST) test key (from-end t) start end)
  (let ((reversed nil))
    (dolist (item (nthcdr start sequence))
      (when (and end (>= start end)) (return))
      (push item reversed)
      (incf start))
    (dolist (elt reversed nil)
      (decf start)
      (when (funcall test item (key-item key elt)) (return start)))))


(macrolet ((def-forward-seq-method (name type etype get update pre post)
	     `(defmethod ,name ((item ,etype) (sequence ,type)
				test key (from-end null) start end)
		(with-simple-vector (sequence start end)
		  (loop ,@pre
			for i from start below end
			for elt = ,get
			when (funcall test item (key-item key elt))
			do ,update
			,@post))))
	   (def-reversed-seq-method (name type etype get update pre post)
	     `(defmethod ,name ((item ,etype) (sequence ,type)
				test key (from-end t) start end)
		(with-simple-vector (sequence start end)
		  (loop ,@pre
			for i from (1- end) downto start
			for elt = ,get
			when (funcall test item (key-item key elt))
			do ,update
			,@post))))
	   (def-seq-methods2 (name type etype get update pre post)
	     `(progn
		(def-forward-seq-method ,name ,type ,etype ,get
		  ,update ,pre ,post)
		(def-reversed-seq-method ,name ,type ,etype ,get
		  ,update ,pre ,post)))
	   (def-seq-methods (name update &optional pre post)
	     `(progn
		(def-seq-methods2 ,name VECTOR t
		  (fast-aref sequence i) ,update ,pre ,post)
		(def-seq-methods2 ,name BASE-STRING character
		  (char-character (base-char-elt sequence i))
		  ,update ,pre ,post)
		(def-seq-methods2 ,name EXTENDED-STRING character
		  (wchar-character (extended-char-elt sequence i))
		  ,update ,pre ,post))))
  (def-seq-methods FIND-SEQ (return elt))
  (def-seq-methods POSITION-SEQ (return i))
  (def-seq-methods COUNT-SEQ (incf count)
    (with count = 0)
    (finally (return count))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun FIND (item sequence &key (start 0) end from-end key
		    (test #'eql) test-not)
  (find-seq item sequence (item-predicate test test-not)
	    key from-end start end))

(defun POSITION (item sequence &key (start 0) end from-end key
		    (test #'eql) test-not)
  (position-seq item sequence (item-predicate test test-not)
		key from-end start end))
(defun POSITION-IF (predicate sequence &key (start 0) end from-end key)
  (position-seq nil sequence (itemless-predicate predicate) key from-end start end))
(defun POSITION-IF-NOT (predicate sequence &key (start 0) end from-end key)
  (position-seq nil sequence (complement (itemless-predicate predicate))
		key from-end start end))

(defun COUNT (item sequence &key (start 0) end from-end key
		    (test #'eql) test-not)
  (count-seq item sequence (item-predicate test test-not)
	     key from-end start end))
(defun COUNT-IF (predicate sequence &key (start 0) end from-end key)
  (count-seq nil sequence (itemless-predicate predicate) key from-end start end))
(defun COUNT-IF-NOT (predicate sequence &key (start 0) end from-end key)
  (count-seq nil sequence (complement (itemless-predicate  predicate))
	     key from-end start end))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; THIS NEEDS TO BE CLEANED UP!!!
(defmacro def-mismatch (list1 list2 elt1 elt2)
  (macrolet
      ((def-sub (test name start end sequence)
	 `(if ,test
	      '(,name (nthcdr ,start ,sequence) (cdr ,name))
	      '(,end (or ,end (length ,sequence)))))
       (def-empty (test sub index end)
	 `(if ,test
	      '(or (null ,sub) (and ,end (>= ,index ,end)))
	      '(>= ,index ,end))))
    (let ((sub1 (def-sub list1 sub1 start1 end1 sequence1))
	  (sub2 (def-sub list2 sub2 start2 end2 sequence2))
	  (empty1 (def-empty list1 sub1 index1 end1))
	  (empty2 (def-empty list2 sub2 index2 end2)))
      `(do (,sub1 ,sub2
		  (index1 start1 (1+ index1))
		  (index2 start2 (1+ index2)))
	   ()
	 (let ((empty1 ,empty1)
	       (empty2 ,empty2))
	   (cond ((and empty1 empty2) (return nil))
		 ((or empty1 empty2) (return index1))
		 ((not (funcall test
				(key-item key ,elt1)
				(key-item key ,elt2)))
		  (return index1))))))))

(defmethod mismatch-seq ((sequence1 LIST) (sequence2 LIST)
			 test key (from-end null)
			 start1 end1 start2 end2)
  (def-mismatch t t (car sub1) (car sub2)))
(defmethod mismatch-seq ((sequence1 LIST) (sequence2 VECTOR)
			 test key (from-end null)
			 start1 end1 start2 end2)
  (def-mismatch t nil (car sub1) (elt sequence2 index2)))
(defmethod mismatch-seq ((sequence1 VECTOR) (sequence2 LIST)
			 test key (from-end null)
			 start1 end1 start2 end2)
  (def-mismatch nil t (elt sequence1 index1) (car sub2)))
(defmethod mismatch-seq ((sequence1 VECTOR) (sequence2 VECTOR)
			 test key (from-end null)
			 start1 end1 start2 end2)
  (def-mismatch nil nil (elt sequence1 index1) (elt sequence2 index2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro def-rmismatch (list1 list2 elt1 elt2)
  (macrolet
      ((def-rsub (test name start end sequence)
	 `(if ,test
	      '(,name (multiple-value-bind (sub length)
			  (reverse-sub-list ,sequence ,start ,end)
			(setq ,end length) sub)
		      (cdr ,name))
	      '(,end (or ,end (length ,sequence))))))
    (let ((sub1 (def-rsub list1 sub1 start1 end1 sequence1))
	  (sub2 (def-rsub list2 sub2 start2 end2 sequence2)))
      `(do* (,sub1 ,sub2
		   (index1 (1- end1) (1- index1))
		   (index2 (1- end2) (1- index2)))
	   ()
	 (let ((empty1 (< index1 start1))
	       (empty2 (< index2 start2)))
	   (cond ((and empty1 empty2) (return nil))
		 ((or empty1 empty2) (return (1+ index1)))
		 ((not (funcall test
				(key-item key ,elt1)
				(key-item key ,elt2)))
		  (return (1+ index1)))))))))

(defmethod mismatch-seq ((sequence1 LIST) (sequence2 LIST)
			 test key (from-end t)
			 start1 end1 start2 end2)
  (def-rmismatch t t (car sub1) (car sub2)))
(defmethod mismatch-seq ((sequence1 LIST) (sequence2 VECTOR)
			 test key (from-end t)
			 start1 end1 start2 end2)
  (def-rmismatch t nil (car sub1) (elt sequence2 index2)))
(defmethod mismatch-seq ((sequence1 VECTOR) (sequence2 LIST)
			 test key (from-end t)
			 start1 end1 start2 end2)
  (def-rmismatch nil t (elt sequence1 index1) (car sub2)))
(defmethod mismatch-seq ((sequence1 VECTOR) (sequence2 VECTOR)
			 test key (from-end t)
			 start1 end1 start2 end2)
  (def-rmismatch nil nil (elt sequence1 index1) (elt sequence2 index2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun MISMATCH (sequence1 sequence2
			     &key (start1 0) end1 (start2 0) end2
			     from-end key (test #'eql)
			     test-not)
  (mismatch-seq sequence1 sequence2
		(item-predicate test test-not) key
		from-end start1 end1 start2 end2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Potential optimization: when compiler uses declarations to pick the
;;; correct sequence method at compile-time, this should be split into
;;; methods to take advantage of optimizations to MISMATCH.
(defun SEARCH (sequence1 sequence2
	       &key (start1 0) end1 (start2 0) end2
		    from-end key (test #'eql) test-not)
  (if (mismatch sequence1 sequence2 :start1 start1 :end1 end1
		:start2 start2 :end2 end2 :from-end from-end
		:key key :test test :test-not test-not)
      (do* ((span (- (or end1 (length sequence1)) start1))
	    (end2 (or end2 (length sequence2)))
	    (stop (- end2 span))
	    (i start2 (1+ i)))
	  ((> i stop) nil)
	(let ((index (if from-end (- stop i) i)))
	  (unless (mismatch sequence1 sequence2
			    :start1 start1 :end1 end1
			    :start2 index :end2 (+ index span)
			    :key key :test test :test-not test-not)
	    (return index))))
    0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 14.5 SORTING AND MERGING                                     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Stable merges: list-merge is destructive (i.e. non-consing)
;;; Note that the order of arguments to the predicate is in violation
;;; of the ANSI.  The spec is wrong and should be changed.

;;; Handles list or vector sequences.  
(defmethod vector-merge (result-vector sequence1 sequence2 pred key)
  (loop with elt1 = nil and elt2 = nil
	with listp1 = (listp sequence1)
	and listp2 = (listp sequence2)

	with state1 = (if listp1 sequence1 0)
	and state2 = (if listp2 sequence2 0)

	with end1 = (if listp1 nil (length sequence1))
	and end2 = (if listp2 nil (length sequence2))

	for index from 0 below (length result-vector)

	if (or (eql state1 end1)
	       (unless (eql state2 end2)
		 (compare (setq elt2 (if listp2 (car state2)
					 (elt sequence2 state2)))
			  (setq elt1 (if listp1 (car state1)
					 (elt sequence1 state1)))
			  pred key)))
	do (setf (elt result-vector index) elt2
		 state2 (if listp2 (cdr state2) (1+ state2)) )
	else
	do (setf (elt result-vector index) elt1
		 state1 (if listp1 (cdr state1) (1+ state1))))
  result-vector)
	

;;; Second value is used by list-mergesort.
(defun list-merge (list1 list2 pred key)
  (do* ((result (cons nil nil))
	(splice result))
      ((or (null list1) (null list2))
       (rplacd splice (or list1 list2))
       (values (cdr result)
	       (last splice)))
    (let ((lesser-list (if (compare (first list2)
				    (first list1)
				    pred key)
			   (prog1 list2 (pop list2))
			 (prog1 list1 (pop list1)))))
      (setq splice
	(setf (cdr splice) lesser-list)))))

  

(defun MERGE (result-type sequence1 sequence2 predicate &key key)
  (let* ((type (expand-type result-type))
	 (stype (canonicalize-sequence-type-specifier type)))
    (check-typef
     (if (consp stype)
	 (vector-merge (make-vector (second stype)
				    (+ (length sequence1) (length sequence2)))
		       sequence1 sequence2 predicate key)
	 ;; No reason not to coerce to list, since we'll need the cons
	 ;; cells anyway. 
	 (list-merge (coerce-to-list sequence1)
		     (coerce-to-list sequence2)
		     predicate key))
     type)))

;;; Bottom-up merge sort ala Sedgewick. 
(defun list-mergesort (list pred key)
  (do ((head (cons :header list))
       (n 1 (* 2 n))                       
       unsorted		    
       list1 list2 temp
       last)
      ((eq list1 (cdr head)) list1)
    (setf unsorted (cdr head))
    (setf last head)
    (loop
     (let ((n-1 (1- n)))
       (setf list1 unsorted)
       (setq temp (nthcdr n-1 list1))
       (cond (temp
	      (setf list2 (cdr temp))
	      (setf (cdr temp) nil)
	      (setf temp (nthcdr n-1 list2))
	      (cond (temp
		     (setf unsorted (cdr temp))
		     (setf (cdr temp) nil))
		    (t (setf unsorted nil)))
	      (multiple-value-bind (merged-head merged-last)
		  (list-merge list1 list2 pred key)
		(setf (cdr last) merged-head)
		(setf last merged-last))
	      (when (null unsorted) (return)))
	     (t  (setf (cdr last) list1)
		 (return)))))))

(defun STABLE-SORT (sequence predicate &key key)
  (if (listp sequence)
      (list-mergesort sequence predicate key)
    ;; We still want to use mergesort for vectors, since its the
    ;; fastest stable sort.  Unfortunately, merge sort of vectors is a
    ;; real pain, so we cons up a list to work with.
    (replace sequence
	     (list-mergesort (coerce-to-list sequence)
			     predicate key))))

;;; An alternative for stable vector sort might be something like the
;;; following, but in-place-vector-merge would be difficult (swapping???)
;;; and might require a stack anyway, so we might as well use the
;;; technique above. 
#|(defun vector-mergesort1 (v left right pred key)
  (if (< left right)
      (let ((i (truncate (+ left right) 2)))
	(vector-mergesort1 v left i pred key)
	(vector-mergesort v (1+ i) right pred key)
	(in-place-vector-merge v left right i pred key))
    v))|#


;;; Quicksorting vectors.
;;; Baase style two-pointer split loop (see Baker).
(defun split1 (v i j x pred key)		;hole is at v(i)
  (if (= i j) i			
    (let ((vj (aref v j)))			;Copy elt to vj
      (cond ((compare vj x pred key)
	     (setf (aref v i) vj)
	     (split2 v (1+ i) j x pred key))
	    (t (split1 v i (1- j) x pred key))))))

(defun split2 (v i j x pred key)		;hole is at v(j)
  (if (= i j) i
    (let ((vi (aref v i)))			;Copy elt to vi
      (cond ((not (compare vi x pred key))
	     (setf (aref v j) vi)
	     (split1 v i (1- j) x pred key))
	    (t (split2 v (1+ i) j x pred key))))))
	  

(defun quicksort-vector (v left right pred key)
  (if (< left right)
      (let* ((x (aref v left))	                ;Create a hole at left
	     (i (split1 v left (1- right) x
			pred key)))             ;Do partition
	(setf (aref v i) x)	                ;Put x back into hole
	(quicksort-vector v left i pred key)
	(quicksort-vector v (1+ i) right pred key))
    v))

(defun SORT (sequence predicate &key key)
  (if (listp sequence)
      (quicksort-list sequence nil predicate key)
      (quicksort-vector sequence 0 (length sequence) predicate key)))
