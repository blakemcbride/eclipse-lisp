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
