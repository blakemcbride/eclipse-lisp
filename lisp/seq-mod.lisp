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



