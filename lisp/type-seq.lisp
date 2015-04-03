;;; Non-type system versions of sequence functions that are used
;;; within the type system itself.

(defmacro list-check-end (start end &optional val)
  `(when ,end (if (>= ,start ,end) (return ,val) (incf ,start))))

;;; Variable capture!!!
(defmacro subst-test (elt)
  `(and (>= index start)
	(or (null end) (< index end))
	(or (null count) (< n count))
	(funcall test item (key-item key ,elt))
	(incf n)))

;;; Variable capture!
(defmacro dup-test (elt sequence from-end start end)
  `(and (>= index start)
	(or (null end) (< index end))
	(plusp (count-seq (key-item key ,elt) ,sequence test key ,from-end
			  ,start ,end))))

(defun item-predicate (test test-not)
  (if test-not (complement test-not) test))

(defun itemless-predicate (predicate)
  #'(lambda (item elt)
      (declare (ignore item))
      (funcall predicate elt)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 14.1 SIMPLE SEQUENCE FUNCTIONS                               ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric NREVERSE (sequence))
(defmethod NREVERSE ((sequence LIST)) (nreverse-list sequence))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 14.3 MODIFYING SEQUENCES                                     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod nremove-duplicate-seq ((sequence LIST) test key
				  (from-end null) start end)
  (do ((dup-end end (when end (1- dup-end)))
       (index 0 (1+ index))
       (sub sequence next)
       (next (cdr sequence) (cdr next))	    
       (last nil))
      ((endp sub) sequence)
    (if (dup-test (car sub) sub nil 1 dup-end)
	(if last
	    (rplacd last next)
	    (setq sequence next))
	(setq last sub))))

(defmethod remove-seq (item (sequence LIST) test key
			    (from-end null) start end count)
  (loop with n = 0
	for elt in sequence
	and index from 0
	unless (subst-test elt)
	collect elt))

(defmethod nremove-seq (item (sequence LIST) test key
			     (from-end null) start end count)
  (do ((n 0)
       (index 0 (1+ index))
       (sub sequence next)
       (next (cdr sequence) (cdr next))	    
       (last nil))
      ((endp sub) sequence)
    (when (and count (>= n count)) (return sequence))
    (if (subst-test (car sub))
	(if last
	    (rplacd last next)
	    (setq sequence next))
	(setq last sub))))

(defun REMOVE (item sequence &key (start 0) end from-end
		      key (test #'eql) test-not count)
  (remove-seq item sequence (item-predicate test test-not)
	      key from-end start end count))

(defun REMOVE-IF (predicate sequence
			      &key (start 0) end from-end key count)
  (remove-seq nil sequence (itemless-predicate predicate) key
	      from-end start end count))

(defun DELETE (item sequence &key (start 0) end from-end
		      key (test #'eql)
		      test-not count)
  (nremove-seq item sequence (item-predicate test test-not)
	       key from-end start end count))

(defun DELETE-DUPLICATES (sequence
			    &key (start 0) end from-end
			    key (test #'eql) test-not)
  (nremove-duplicate-seq sequence (item-predicate test test-not)
			 key from-end start end)) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 14.4 SEARCHING SEQUENCES FOR ITEMS                           ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod find-seq (item (sequence LIST) test key from-end start end)
  (dolist (elt (if from-end
		   (multiple-value-bind (sub length)
		       (reverse-sub-list sequence start end)
		     (setq end length) sub)
		   (nthcdr start sequence))
	       nil)
    (list-check-end start end)
    (when (funcall test item (key-item key elt)) (return elt))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod count-seq (item (sequence LIST) test key from-end start end
			   &aux (count 0))
  (dolist (elt (if from-end
		   (let ((n start) (reversed nil))
		     (dolist (item (nthcdr start sequence) reversed)
		       (list-check-end n end)
		       (push item reversed)))
		   (nthcdr start sequence))
	       count)
    (list-check-end start end count)
    (when (funcall test item (key-item key elt)) (incf count))))

(defun FIND-IF (predicate sequence &key (start 0) end from-end key)
  (find-seq nil sequence (itemless-predicate predicate)
	    key from-end start end))

(defun FIND-IF-NOT (predicate sequence &key (start 0) end from-end key)
  (find-seq nil sequence (complement (itemless-predicate predicate))
	    key from-end start end))


