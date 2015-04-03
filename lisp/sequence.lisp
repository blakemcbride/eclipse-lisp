;;; Potential optimization: use fast-aref/with-simple-vector
;;; throughout here and string.lisp 

;;; Potential optimization: add special cases to methods for specialized
;;; and/or simple vectors, particularly for strings.  This is a
;;; particularly big win in that it allows us to use specialized access
;;; rather than ELT, which not only dispatches, but does bounds checking
;;; as well.

;;; Potential optimization: add special cases to methods when test, key,
;;; end, etc. are null.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ERROR REPORTING, CHECKING AND OTHER UTILITIES                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun out-of-bounds-error (index length)
  (error 'out-of-bounds-error :datum index :length length))

;;; Index is an integer that will be used to limit the bounds of sequence.
(defun check-bounds (index length)
  (if (<= 0 index length)
      index
      (out-of-bounds-error index length)))

;;; Index is an integer that will be used to access an element.
(defun check-index (index length)
  (if (< -1 index length)
      index
      (out-of-bounds-error index length)))

(defun check-complex-index (vector integer)
  (check-index integer (index-fixnum (complex-array-fill-pointer vector))))

(defun check-simple-index (vector integer)
  (fixnum-index (check-index integer (index-fixnum (vector-size vector)))))

;;; Checks both start and end, and defaults end to length.
(defun check-both-bounds (start end length)
  (check-bounds start length)
  (if end
      (check-bounds end length)
      length))

(defun check-typef (x type)
  (if (typep x type) x
      (error 'type-error :datum x :expected-type type)))


(defun canonicalize-sequence-type-specifier (type-specifier-spec
					     &optional (errorp t))
  (let ((type (expand-type type-specifier-spec)))
    (cond ((subtypep type 'list) 'list)
	  ((subtypep type 'base-string) '(simple-array base-char))
	  ((subtypep type 'string) '(simple-array character))
	  ((subtypep type 'bit-vector) '(simple-array bit))
	  ((subtypep type 'vector) '(simple-array t))
	  (errorp (error 'type-error :datum type-specifier-spec
			 :expected-type 'sequence-type-specifier)))))

(defun sequence-type-specifier-p (type-specifier-spec)
  (not (null (canonicalize-sequence-type-specifier
	      type-specifier-spec nil))))


(defmacro list-check-end-noinc (start end &optional val)
  `(when (and ,end (>= ,start ,end)) (return ,val)))


(defun trim-vector (vector length)
  (let ((current (length vector)))
    (if (= current length) vector
	(adjust-array vector current :fill-pointer length))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; INTERNAL ACCESS                                              ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun fast-aref (array row-major-index)
  (etypecase array
    (simple-base-string (char-character
			 (base-char-elt array row-major-index)))
    (simple-extended-string (wchar-character
			     (extended-char-elt array row-major-index)))
    (simple-vector (general-elt array row-major-index))
    (simple-bit-vector (index-fixnum (bit-elt array row-major-index)))
    (array (fast-aref (simple-array-contents array)
		      (+ row-major-index (complex-array-offset array))))))

(defun fast-set-aref (array row-major-index new-element)
  (etypecase array
    (simple-base-string
     (char-character
      (set-base-char-elt array row-major-index (character-char new-element))))
    (simple-extended-string
     (wchar-character
      (set-extended-char-elt array row-major-index (character-wchar new-element))))
    (simple-vector (set-general-elt array row-major-index new-element))
    (simple-bit-vector
     (index-fixnum
      (set-bit-elt array row-major-index (integer-index new-element))))
    (array (fast-set-aref (simple-array-contents array)
			  (+ row-major-index (complex-array-offset array))
			  new-element))))


;;; Uses of this should be replaced by with-simple-vector.
;;; Note that offset accumulates over chain of displaced arrays.
(defun get-simple-vector (array offset)
  (etypecase array
    (simple-basic-vector (values array offset))
    (simple-array (values (simple-array-contents array) offset))
    (array (get-simple-vector (simple-array-contents array)
			      (+ offset (complex-array-offset array))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SIZE                                                         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; As in Dylan, returns the number of elements actually stored.
;;; This respects fill-pointers, hash-table-count. 
(defgeneric size (collection))
(defmethod size ((collection LIST)) (list-length collection))
(defmethod size ((collection SIMPLE-BASIC-VECTOR)) (vector-size collection))
(defmethod size ((collection VECTOR)) (fill-pointer collection))
(defmethod size ((collection ARRAY)) (array-total-size collection))
(defmethod size ((collection HASH-TABLE)) (hash-table-count collection))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ITERATION                                                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun reverse-sub-list (list start end)
  (let ((length start)
	(sub nil))
    (dolist (item (nthcdr start list))
      (when (and end (>= length end)) (return))
      (push item sub)
      (incf length))
    (values sub length)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This could also be done a a not-yet-implemented loop COLLECT-AS
;;; keyword.    
(defmacro accumulate-sequence ((accumulator-name
				result-type-spec data &optional
				(length-computer '#'length))
			       &body body)
  (let ((result-type (cl:gensym "RESULT-TYPE"))
	(result (cl:gensym "RESULT"))
	(vectorp (cl:gensym "VECTORP"))
	(state (cl:gensym "STATE")))
    `(let* ((,result-type (expand-type ,result-type-spec))
	    (,result (sequence-result ,result-type ,data
				     ,length-computer))
	    (,vectorp ,result)
	    (,state (if ,vectorp 0 nil)))
       (macrolet
	   ((,accumulator-name (new-elt)
			       `(let ((new ,new-elt))
				  (cond
				   (,',vectorp
				    (setf (elt ,',result ,',state)
					  new)
				    (incf ,',state))
				   (,',result
				    (rplacd ,',state
					    (setq ,',state (list new))))
				   (t (setq ,',result
					    (setq ,',state (list new)))))
				  new)))
	 ,@body (check-typef ,result ,result-type)))))

  
(defun sequence-result (type data &optional (length #'length))
  (let ((type (canonicalize-sequence-type-specifier type)))
    (when (consp type)
      (make-array (funcall length data)
		  :element-type (second type)))))

(defun sum-lengths (list)
  (loop for sequence in list
	sum (length sequence)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 14.1 SIMPLE SEQUENCE FUNCTIONS                               ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgeneric ELT (sequence index))
(defmethod elt ((sequence list) (index integer))
  (let ((list sequence))
    (declare (type list list))
    (dotimes (x index (car list))
      (if (endp list)
	  (out-of-bounds-error index x)
	  (setq list (cdr list))))))

(defmethod elt ((sequence SIMPLE-BASE-STRING) (index integer))
  (base-schar sequence index))

(defmethod elt ((sequence SIMPLE-EXTENDED-STRING) (index integer))
  (extended-schar sequence index))

(defmethod elt ((sequence SIMPLE-BIT-VECTOR) (index integer))
  (sbvref sequence index))

(defmethod elt ((sequence SIMPLE-VECTOR) (index integer))
  (svref sequence index))

(defmethod elt ((sequence VECTOR) (index integer))
  (elt (simple-array-contents sequence)
       (+ (complex-array-offset sequence)
	  (check-complex-index sequence index))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgeneric (SETF ELT) (new-elt sequence index))
(defmethod (setf elt) (new-elt (sequence list) (index integer))
  (let ((list sequence))
    (declare (type list list))
    (dotimes (x index (setf (car list) new-elt))
      (if (endp list)
	  (out-of-bounds-error index x)
	  (setq list (cdr list))))))

(defmethod (setf elt) ((new-elt character)
			 (sequence SIMPLE-BASE-STRING) (index integer))
  (set-base-schar sequence index new-elt))

(defmethod (setf elt) ((new-elt character)
			 (sequence SIMPLE-EXTENDED-STRING) (index integer))
  (set-extended-schar sequence index new-elt))

(defmethod (setf elt) ((new-elt integer)
			 (sequence SIMPLE-BIT-VECTOR) (index integer))
  (set-sbvref sequence index new-elt))

(defmethod (setf elt) (new-elt (sequence SIMPLE-VECTOR) (index integer))
  (setf (svref sequence index) new-elt))

(defmethod (setf elt) (new-elt (sequence VECTOR) (index integer))
  (setf (elt (simple-array-contents sequence)
	       (+ (complex-array-offset sequence)
		  (check-complex-index sequence index)))
	new-elt))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod coerce-to-list ((sequence LIST))
  sequence)

(defmethod coerce-to-list ((sequence VECTOR))
  (loop for index from 0 below (length sequence)
	collect (elt sequence index)))

(defmethod coerce-to-list ((sequence t))
  (error 'type-error :datum sequence :expected-type 'sequence))

(defmethod coerce-to-vector ((sequence LIST) element-type)
  (loop with result = (make-vector element-type (length sequence))
	for index from 0
	and elt in sequence
	do (setf (elt result index) elt)
	finally (return result)))

(defmethod coerce-to-vector ((sequence VECTOR) element-type)
  (loop with length = (length sequence)
	with result = (make-vector element-type length)
	for index from 0 below length
	do (setf (elt result index) (elt sequence index))
	finally (return result)))

(defmethod coerce-to-vector ((sequence t) (element-type t))
  (error 'type-error :datum sequence :expected-type 'sequence))

(defun coerce-to-sequence (sequence type)
  (let ((type (expand-type type)))
    (if (typep sequence type)
	sequence
	(coerce-to-sequence1 sequence type))))

(defun coerce-to-sequence1 (sequence type)
  (let ((stype (canonicalize-sequence-type-specifier type)))
    (check-typef
     (if (consp stype)
	 (coerce-to-vector sequence (second stype))
	 (coerce-to-list sequence))
     type)))
      

(defmethod SUBSEQ ((sequence LIST) start &optional end)
  (loop for elt in (nthcdr start sequence)
	do (list-check-end start end copy)
	collect elt into copy
	finally (return copy)))

(macrolet ((def-subseq (type set get construct)
	     `(defmethod SUBSEQ ((sequence ,type) start &optional end)
		(with-simple-vector (sequence start end)
		  (loop with copy = ,construct
			for index from start below end
			and cindex from 0
			do (,set copy cindex (,get sequence index))
			finally (return copy))))))
  (def-subseq VECTOR fast-set-aref fast-aref
    (make-vector (array-element-type sequence) (- end start)))
  (def-subseq BASE-STRING set-base-char-elt base-char-elt
    (make-base-char-vector (- end start)))
  (def-subseq EXTENDED-STRING set-extended-char-elt extended-char-elt
    (make-extended-char-vector (- end start))))


(defun (setf SUBSEQ) (new-subsequence sequence start &optional end)
  (replace sequence new-subsequence
	     :start1 start :end1 end)
  new-subsequence)

(defmethod COPY-SEQ ((sequence LIST))
  (loop for item in sequence collect item))
(defmethod COPY-SEQ ((sequence VECTOR))
  (subseq sequence 0))

(defgeneric REVERSE (sequence))
(defmethod REVERSE ((sequence LIST) &aux result)
  (dolist (item sequence result) (push item result)))
(defmethod REVERSE ((sequence VECTOR))
  (let* ((length (length sequence))
	 (copy (make-vector (array-element-type sequence) length)))
    (do ((forward-index 0 (1+ forward-index))
	 (backward-index (1- length) (1- backward-index)))
	((= forward-index length) copy)
      (declare (type fixnum forward-index backward-index))
      (setf (elt copy forward-index)
	    (elt sequence backward-index)))))

(defmethod NREVERSE ((sequence VECTOR) &aux (length (length sequence)))
  (do ((forward-index 0 (1+ forward-index))
       (backward-index (1- length) (1- backward-index)))
      ((>= forward-index backward-index) sequence)
    (declare (type fixnum forward-index backward-index))
    (rotatef (elt sequence forward-index) (elt sequence backward-index))))

(defun MAKE-SEQUENCE (type size
			     &key (initial-element nil initial-element-p))
  (let* ((stype (canonicalize-sequence-type-specifier type))
	 (s (if (consp stype)
		(initialize-vector size (second stype) 
				   initial-element initial-element-p
				   nil nil nil)
		(make-list size :initial-element initial-element))))
    (check-typef s type)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 14.2 CONCATENATING, MAPPING, AND REDUCING SEQUENCES          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Potential Optimization: define 1-sequence and 2-sequence versions of
;;; these as generic functions, specialized to various kinds of sequences.

(defun CONCATENATE (result-type &rest sequences)
  (declare (dynamic-extent sequeneces))  
  (accumulate-sequence (add-elt result-type sequences #'sum-lengths)
     (dolist (sequence sequences)
       (if (listp sequence)
	   (dolist (elt sequence) (add-elt elt))
	   (dotimes (i (length sequence)) (add-elt (elt sequence i)))))))


(defun list-collection-element (collection state)	
  (declare (ignore collection))
  (car state))

(defun get-multi-iterators (sequences)
  (loop for sequence in sequences
	for listp = (listp sequence)
	collect (if listp sequence 0) into states
	collect (if listp nil (length sequence)) into limits
	collect (if listp #'list-collection-element #'elt) into getters
	collect (if listp #'cdr #'1+) into nexts
	finally (return (values states limits getters nexts))))

(defun finished-multi-iterators (states limits)
  (loop for state in states
	and limit in limits
	when (eql state limit)
	return t))

(defun multi-iterator-elts (getters sequences states)
  (loop for getter in getters
	and sequence in sequences
	and state in states
	collect (funcall getter sequence state)))

(defun advance-multi-iterators (nexts states)
  (loop for states on states
	and next in nexts
	do (rplaca states (funcall next (car states)))))

(defun MAP (result-type function &rest sequences)
  (declare (dynamic-extent sequences))
  (multiple-value-bind (states limits getters nexts)
      (get-multi-iterators sequences)
    (if result-type
	(let ((length 0))
	  (trim-vector
	   (accumulate-sequence (add-elt result-type (first sequences))
	     (loop when (finished-multi-iterators states limits)
		   return nil
		   do (add-elt (apply function
				  (multi-iterator-elts getters sequences
						       states)))
		   do (advance-multi-iterators nexts states)
		   do (incf length)))
	   length))
	(loop when (finished-multi-iterators states limits)
	      return nil
	      do (apply function
			(multi-iterator-elts getters sequences states))
	      do (advance-multi-iterators nexts states))))) 

(defmethod MAP-INTO ((result-sequence LIST) function &rest sequences)
  (declare (dynamic-extent sequences))
  (multiple-value-bind (states limits getters nexts)
      (get-multi-iterators sequences)
    (loop for sub on result-sequence
	  when (finished-multi-iterators states limits)
	  return nil
	  do (rplaca sub
		     (apply function
			    (multi-iterator-elts getters sequences states)))
	  do (advance-multi-iterators nexts states))
    result-sequence))

(defmethod MAP-INTO ((result-sequence VECTOR) function
		       &rest sequences)
  (declare (dynamic-extent sequences))
  (multiple-value-bind (result offset) (get-simple-vector result-sequence 0)
    (multiple-value-bind (states limits getters nexts)
	(get-multi-iterators sequences)
      (loop with length = (array-dimension result-sequence 0)
	    for index from offset below (+ length offset)
	    when (finished-multi-iterators states limits)
	    return (when (array-has-fill-pointer-p result-sequence)
		     (setf (fill-pointer result-sequence) (- index offset)))
	    ;; Can't use ELT, because that checks fill-pointer
	    do (fast-set-aref result index
			      (apply function
				     (multi-iterator-elts getters sequences states)))
	    do (advance-multi-iterators nexts states))))
    result-sequence)

(macrolet
    ((def-pred (name test early-val default)
       `(defun ,name (predicate &rest sequences)
	  (declare (dynamic-extent sequences))
	  (multiple-value-bind (states limits getters nexts)
	      (get-multi-iterators sequences)
	    (loop for item = (if (finished-multi-iterators states limits)
				 (return ,default)
				 (apply predicate
					(multi-iterator-elts
					 getters sequences states)))
		  ,test item return ,early-val
		  do (advance-multi-iterators nexts states))))))
  (def-pred SOME 	when 	item	nil)
  (def-pred EVERY 	unless	nil	t)
  (def-pred NOTANY	when	nil	t)
  (def-pred NOTEVERY	unless	t	nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod reduce-seq (value (sequence LIST) function key
			     (from-end null) start end ivs)
  (setq sequence (nthcdr start sequence))
  (when (null ivs)
    (if sequence
	(setq value (key-item key (pop sequence))
	      start (1+ start))
	(return-from reduce-seq (funcall function))))
  (loop for item in sequence
	do (list-check-end start end value)
	do (setq value (funcall function value (key-item key item)))
	finally (return value)))

(defmethod reduce-seq (value (sequence LIST) function key
			     (from-end t) start end ivs)
  (reduce-seq value (reverse-sub-list sequence start end)
	      #'(lambda (&rest args)
		  (apply function (nreverse args)))
	      key nil 0 nil ivs))

(defmethod reduce-seq (value (sequence VECTOR) function key
			     (from-end null) start end ivs)
  (unless end (setq end (length sequence)))
  (when (null ivs)
    (if (> end start)
	(setq value (key-item key (elt sequence start))
	      start (1+ start))
	(return-from reduce-seq (funcall function))))
  (loop for index from start below end
	do (setq value (funcall function value
				(key-item key (elt sequence index))))
	finally (return value)))

(defmethod reduce-seq (value (sequence VECTOR) function key
			     (from-end t) start end ivs)
  (unless end (setq end (length sequence)))
  (when (null ivs)
    (if (> end start)
	(setq value (key-item key (elt sequence (decf end))))
	(return-from reduce-seq (funcall function))))
  (loop for index from (1- end) downto start
	do (setq value (funcall function
				(key-item key (elt sequence index))
				value))
	finally (return value)))


(defun REDUCE (function sequence
			  &key (start 0) end from-end key
			  (initial-value nil ivs))
  (reduce-seq initial-value sequence function (or key #'identity)
	      from-end start end ivs))

