;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ERROR REPORTING, CHECKING AND OTHER UTILITIES                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun illegal-array-axis (array axis-number)
  (error "Array axis number ~s is not valid for array ~s."
	 axis-number array))

(defun check-subscript (i rank array)
  (unless (= i rank)
    (error "Wrong number of indices for rank ~d array ~s."
	   rank array)))

(defun missing-fill-pointer-error (array)
  (error "~s does not have a fill pointer." array))

(defconstant ARRAY-DIMENSION-LIMIT most-positive-fixnum)
(defconstant ARRAY-RANK-LIMIT 8)
(defconstant ARRAY-TOTAL-SIZE-LIMIT array-dimension-limit)
(deftype Index () `(integer 0 ,(1- ARRAY-TOTAL-SIZE-LIMIT)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 17.1 ARRAY CREATION                                          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun make-uninitialized-vector (size type &aux initialize-p)
  (values (case (upgraded-array-element-type type)
	    (base-char (make-base-char-vector size))
	    (character (make-extended-char-vector size))
	    (bit (make-bit-vector size))
	    (t ;; If a general vector is uninitialized to EOA, strange
	     ;; bugs can appear when an element is later accessed. 
	     (setq initialize-p t)
	     (make-general-vector size)))
	  initialize-p))


;;; Potential optimization: treat this as a special form in the
;;; compiler to avoid consing and bounds checks, and to use the
;;; appropriate specialized form of (setf aref).
(defun vectorize (element-type &rest args)
  (declare (dynamic-extent args))
  (loop with length = (length args)
	with vector = (make-uninitialized-vector (fixnum-index length) element-type)
	for i fixnum from 0 below length
	and arg in args
	do (setf (row-major-aref vector i) arg)
	finally (return vector)))

(defun VECTOR (&rest args) (apply #'vectorize 't args))

(defun initialize-vector (size type
			  element element-p
			  initial-contents contents-p
			  dimensions)
  (multiple-value-bind (vector init-p)
      (make-uninitialized-vector (fixnum-index size) type)
    (unless (or element-p contents-p) (setq element-p init-p))
    (cond ((and element-p contents-p)
	   (error "Cannot specify both :initial-element and :initial-contents."))
	  (element-p
	   (dotimes (i size vector)
	     (setf (row-major-aref vector i) element)))
	  (contents-p
	   (let ((index 0))
	     (labels
		 ((fill (axis dims contents &aux length)
		    (cond ((null dims)
			   (setf (row-major-aref vector index) contents)
			   (incf index))
			  ((not (typep contents 'sequence))
			   (error
			    ":initial-contents ~s is not nested deep enough for rank ~d array."
			    initial-contents (length dimensions)))
			  ((not (= (setf length (car dims))
				   (length contents)))
			   (error ":initial-contents subsequence ~s is not of length ~d."
				  contents length))
			  (t
			   (dotimes (i (car dims))
			       (fill (1+ axis) (cdr dims)
				     ;; This is terrible if contents is a list!!!
				     (elt contents i)))))))
	       (fill 0 dimensions initial-contents)))))
    vector))

(defun make-vector (type size &key (initial-element nil initializep))
  (initialize-vector size type initial-element initializep
		     nil nil nil))

(defun make-or-adjust-array (array dimensions
				   &key (element-type (if array
							  (array-element-type array)
							  t))
				   (initial-element nil initial-element-p)
				   (initial-contents nil initial-contents-p)
				   adjustable fill-pointer
				   displaced-to displaced-index-offset)
  (when (integerp dimensions) (setf dimensions (list dimensions)))
  (when (and displaced-index-offset (not displaced-to))
    (error
     "Attempt to create a non displaced array with a displaced-index-offset of ~s."
     displaced-index-offset))
  (let* ((size (apply #'* dimensions))
	 (rank (length dimensions))
	 (contents (or displaced-to
		       (initialize-vector
			size element-type
			initial-element initial-element-p
			initial-contents initial-contents-p
			dimensions))))
    (cond ((or fill-pointer adjustable displaced-to array)
	   (when array
	     (check-subscript rank (array-rank array) array)
	     (unless (or initial-contents-p displaced-to)
	       (if (= rank 0)
		   (setf (row-major-aref contents 0) (aref array))
		   (copy-array-contents (array-dimensions array) array
					dimensions contents
					initial-element
					initial-element-p))))
	   (when (and fill-pointer (/= rank 1))
	     (error "Only vectors can have fill pointers."))
	   (if (integerp fill-pointer)
	       (check-bounds fill-pointer size)
	       (setf fill-pointer size))
	   (cond (displaced-to
		  (when (or initial-element-p initial-contents-p)
		    (error "Displaced arrays cannot also specify
                        :initial-element or :initial-contents."))
		  (unless (subtypep (array-element-type displaced-to)
				    (upgraded-array-element-type element-type))
		    (error "Array of :element-type ~s cannot be :displaced-to ~s."
			   element-type displaced-to))
		  (unless (<= 0
			      (setq displaced-index-offset
				    (or displaced-index-offset 0))
			      (- (array-total-size displaced-to) size))
		    (error "Invalid displaced-index-offset (~d) for ~
                           size ~d array displaced to size ~d array."
			   displaced-index-offset size
			   (array-total-size displaced-to))))
		 (t (setq displaced-index-offset 0)))
	   (if (and array (not (typep array 'simple-array)))
	       (update-array array contents (fixnum-index rank)
			     dimensions displaced-index-offset fill-pointer)
	       (make-complex-array
		(find-type
		 (if (= rank 1)
		     (element-vector-type 'complex-array
					  (upgraded-array-element-type element-type))
		     'complex-array))
		(fixnum-index rank) dimensions contents
		displaced-index-offset fill-pointer)))
	  ((= rank 1) contents)
	  (t (make-simple-array (fixnum-index rank) dimensions contents)))))

(defun MAKE-ARRAY (dimensions &rest keys)
  (declare (dynamic-extent keys))
  (apply #'make-or-adjust-array nil dimensions keys))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 17.2 ARRAY ACCESS                                            ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun SVREF (simple-vector index)
  (general-elt simple-vector
	       (check-simple-index simple-vector index)))
(defun (setf SVREF) (new-elt simple-vector index)
  (set-general-elt simple-vector
		   (check-simple-index simple-vector index) new-elt))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgeneric ROW-MAJOR-AREF (array index))

(defmethod row-major-aref ((array SIMPLE-BASE-STRING) (index integer))
  (base-schar array index))

(defmethod row-major-aref ((array SIMPLE-EXTENDED-STRING) (index integer))
  (extended-schar array index))

(defmethod row-major-aref ((array SIMPLE-VECTOR) (index integer))
  (svref array index))

(defmethod row-major-aref ((array SIMPLE-BIT-VECTOR) (index integer))
  (sbvref array index))

(defmethod row-major-aref ((array SIMPLE-ARRAY) (index integer))
  (row-major-aref (simple-array-contents array) index))

(defmethod row-major-aref ((array ARRAY) (index integer))
  (row-major-aref (simple-array-contents array)
		   (+ index (complex-array-offset array))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgeneric (setf ROW-MAJOR-AREF) (new-elt array index))

(defmethod (setf row-major-aref) ((new-elt character)
				  (array SIMPLE-BASE-STRING) (index integer))
  (set-base-schar array index new-elt))

(defmethod (setf row-major-aref) ((new-elt character)
				  (array SIMPLE-EXTENDED-STRING) (index integer))
  (set-extended-schar array index new-elt))

(defmethod (setf row-major-aref) (new-elt
				  (array SIMPLE-VECTOR) (index integer))
  (setf (svref array index) new-elt))

(defmethod (setf row-major-aref) ((new-elt integer)
				  (array SIMPLE-BIT-VECTOR) (index integer))
  (set-sbvref array index new-elt))

(defmethod (setf row-major-aref) (new-elt
				  (array SIMPLE-ARRAY) (index integer))
  (if (= (array-rank array) 1)
      (error 'type-error :datum new-elt
	     :expected-type (array-element-type array))
      (setf (row-major-aref (simple-array-contents array) index) new-elt)))

(defmethod (setf row-major-aref) (new-elt (array ARRAY) (index integer))
  (setf (row-major-aref (simple-array-contents array)
			(+ index (complex-array-offset array)))
	new-elt))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun AREF (array &rest indices)
  (declare (dynamic-extent indices))
  (row-major-aref
   array (apply #'array-row-major-index array indices)))

(defun (SETF AREF) (new-elt array &rest indices)
  (declare (dynamic-extent indices))
  (setf (row-major-aref
	 array (apply #'array-row-major-index array indices))
	new-elt))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 17.3 ARRAY INFORMATION                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgeneric ARRAY-ELEMENT-TYPE (array))
(defmethod array-element-type ((array base-string)) 'base-char)
(defmethod array-element-type ((array string)) 'character)
(defmethod array-element-type ((array bit-vector)) 'bit)
(defmethod array-element-type ((array vector)) 't)
(defmethod array-element-type ((array array))
  (array-element-type (simple-array-contents array)))

(defgeneric ARRAY-RANK (array))
(defmethod array-rank ((array vector)) 1)
(defmethod array-rank ((array array))
  (index-fixnum (simple-array-rank array)))

(defgeneric ADJUSTABLE-ARRAY-P (array))
(defmethod ADJUSTABLE-ARRAY-P ((array simple-array)) nil)
(defmethod ADJUSTABLE-ARRAY-P ((array array)) t)

(defgeneric ARRAY-DISPLACEMENT (array))
(defmethod array-displacement ((array simple-array))
  (values nil 0))
(defmethod array-displacement ((array array))
  (values (simple-array-contents array)
	  (index-fixnum (complex-array-offset array))))

(defgeneric ARRAY-DIMENSION (array axis-number))
(defmethod array-dimension ((array simple-basic-vector) (axis-number integer))
  (if (zerop axis-number)
      (index-fixnum (vector-size array))
      (illegal-array-axis array axis-number)))
(defmethod array-dimension ((array array) (axis-number integer))
  (if (< -1 axis-number (simple-array-rank array))
      (index-fixnum (simple-array-dimension
		     array (fixnum-index axis-number)))
      (illegal-array-axis-number array axis-number)))

(defgeneric ARRAY-TOTAL-SIZE (array))
(defmethod ARRAY-TOTAL-SIZE ((array simple-basic-vector))
  (index-fixnum (vector-size array)))
(defmethod ARRAY-TOTAL-SIZE ((array array))
  (do ((rank (simple-array-rank array))
       (i (fixnum-index 0) (1+ i))
       (j (fixnum-index 1)
	  (* j (simple-array-dimension array i))))
      ((= i rank) (index-fixnum j))
    (declare (type index rank i j))))

(defun ARRAY-DIMENSIONS (array)
  (do ((i (array-rank array))
       (d nil))
      ((= i 0) d)
    (push (array-dimension array (decf i)) d)))

(defun ARRAY-IN-BOUNDS-P (array &rest indices
			  &aux (r (index-fixnum (array-rank array))))
  (declare (dynamic-extent indices))
  (check-subscript (length indices) r array)
  (do ((i 0 (1+ i))
       (s indices (cdr s)))
      ((>= i r) t)
    (unless (< -1 (car s) (index-fixnum (array-dimension array i)))
      (return nil))))

(defun ARRAY-ROW-MAJOR-INDEX (array &rest indices)
  (declare (dynamic-extent indices))
  (do ((i 0 (1+ i))
       (j 0 (+ (* j (index-fixnum (array-dimension array i)))
	       (car s)))
       (s indices (cdr s)))
      ((endp s)
       (check-subscript i (index-fixnum (array-rank array)) array)
       j)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 17.5 FILL POINTERS                                           ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Note that all complex vectors have fill pointers, whether
;;; expressely provided to make-array or not.
(defgeneric ARRAY-HAS-FILL-POINTER-P (array))
(defmethod array-has-fill-pointer-p ((array SIMPLE-ARRAY)) nil)
(defmethod array-has-fill-pointer-p ((array ARRAY))
  (= (array-rank array) 1))

(defun FILL-POINTER (array)
  (if (array-has-fill-pointer-p array)
      (complex-array-fill-pointer array)
    (missing-fill-pointer-error array)))

(defun (setf FILL-POINTER) (fp array)
  (cond ((array-has-fill-pointer-p array)
	 (check-bounds fp (array-total-size array))
	 (set-complex-array-fill-pointer array fp))
	(t (missing-fill-pointer-error array))))

;;; A "reasonable" growth algorithm:
;;; Grow geometrically (factor of 2) until we reach page-size,
;;; then grow one page at a time until we reach the soft-limit,
;;; then grow arithmetically by a small factor of the requested
;;; extension.
;;; All conditions satisfy minimum-extension.
;;; Assumes (< page-size soft-limit).
(defun extension (current-size minimum-extension page-size soft-limit)
  (let ((requested (+ current-size minimum-extension)))
    (cond ((< requested page-size)
	   (max requested
		(* 2 current-size)))
	  ((< requested soft-limit)
	   (* (ceiling requested page-size)
	      page-size))
	  (t (+ current-size (* 4 minimum-extension))))))

(defun VECTOR-PUSH (new-element vector
		    &aux (fp (fill-pointer vector)))
  (unless (= fp (array-dimension vector 0))
    (fast-set-aref vector fp new-element)
    (set-complex-array-fill-pointer vector (the fixnum (1+ fp)))
    fp))

;;; In our implementation, only a complex vector has a fill pointer,
;;; and all such vectors are actually adjustable.
(defun VECTOR-PUSH-EXTEND (new-element vector
			   &optional (extension 10))
  (etypecase vector
    (complex-basic-vector
     (let* ((size (array-dimension vector 0))
	    (fp (complex-array-fill-pointer vector)))
       (when (= fp size)
	 (adjust-array vector
		       (extension size extension 4096 102400)))
       (fast-set-aref vector fp new-element)
       (set-complex-array-fill-pointer vector (the fixnum (1+ fp)))
       fp))))

(defun VECTOR-POP (vector &aux (fp (fill-pointer vector)))
  (when (= fp 0)
    (error "The fill pointer of the vector ~S is zero." vector))
  (setf (fill-pointer vector) (decf fp))
  (aref vector fp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 17.6 CHANGING THE DIMENSIONS OF AN ARRAY                     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun copy-array-contents (old-dimensions old new-dimensions new
			    initial-element initial-element-p
			    &aux (n 0) (o 0))
  (macrolet ((post-incf (x)
	       `(let ((old ,x)) (setq ,x (1+ old)) old)))
    (labels ((copy (new-dims old-dims &aux (old-key (car old-dims)))
	       (dotimes (i (car new-dims)
			  (when (and old-key (< i old-key))
			    (incf o (apply #'* (- old-key i) (rest old-dims)))))
		 (unless (and old-key (< i old-key))
		   (setf old-dims nil))
		 (cond ((cdr new-dims)
			(copy (cdr new-dims) (cdr old-dims)))
		       ((or old-dims initial-element-p)
			(setf (row-major-aref new (post-incf n))
			  (if old-dims
			      (row-major-aref old (post-incf o))
			    initial-element)))
		       (t (incf n))))))
      (copy new-dimensions old-dimensions)
      new)))

;;; For now, we play it safe destructively by modifying only complex arrays.
;;; We could avoid creating new arrays for certain other combinations
;;; of input, but that would not be a conservative reading of the spec.
(defun ADJUST-ARRAY (array dimensions &rest keys)
  (declare (dynamic-extent keys))
  (apply #'make-or-adjust-array array dimensions keys))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                     PRINTING                                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod PRINT-OBJECT ((v VECTOR) s)
  (pprint-logical-block (s nil :prefix "#(" :suffix ")")
    (let ((end (length v)) (i 0))
      (when (plusp end)
	(loop (pprint-pop)
	      (write-toplevel (aref v i) s)
	      (if (= (incf i) end) (return nil))
	      (write-char #\space s)
	      (pprint-newline :fill s))))))

(defun write-array (array s prefix)
  (let* ((rank (array-rank array))
	 (bottom (1- rank))
	 (indices (make-list rank :initial-element 0))
	 (dims (array-dimensions array))
	 (prefix (or prefix (format nil "#~DA(" rank))))
    (labels ((pretty-slice (slice prefix s)
	       (pprint-logical-block (s nil :prefix prefix :suffix ")")
		 (let ((end (nth slice dims))
		       (spot (nthcdr slice indices))
		       (i 0))
		   (when (plusp end)
		     (loop (pprint-pop)
			   (setf (car spot) i)
			   (if (= slice bottom)
			       (write-toplevel (apply #'aref array indices) s)
			       (pretty-slice (1+ slice) "(" s))
			   (if (= (incf i) end) (return nil))
			   (write-char #\space s)
			   (pprint-newline (if (= slice bottom)
					       :fill :linear)
					   s)))))))
      (pretty-slice 0 prefix s))))

(defmethod PRINT-OBJECT ((array ARRAY) s)
  (cond #+not-in-current-implementation
	((and *print-readably*
	      (not (eq (array-element-type array) t)))
	 ;; This code will only be used if/when we support specialized
	 ;; arrays other than strings and bit-vectors.
	 (print-eval
	  `(make-array ',(array-dimensions array)
		       :element-type ',(array-element-type array)
		       :initial-contents  ;We're lazy!
		       ',(read-from-string
			  (with-output-to-string (s)
			    (write-array array s "("))))
	  s array))
	((not (or *print-array* *print-readably*))
	 (print-unreadable-object
	     (array s :type t :identity t)))
	((zerop (array-rank array))
	 (write-string "#0A " s)
	 (write (aref array) :stream s))
	(T (write-array array s nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                    UPGRADING                                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun UPGRADED-ARRAY-ELEMENT-TYPE (typespec &optional environment)
  (let ((type (expand-type typespec t environment)))
    (cond ((subtypep type 'base-char environment) 'base-char)
	  ((subtypep type 'character environment) 'character)
	  ((subtypep type 'bit environment) 'bit)
	  (t t))))

;;; Returns the specialized vector type-specifier appropriate for a
;;; given atomic array type and element-type.
(defun element-vector-type (array-type-specifier
			    element-type-specifier)
  (case element-type-specifier
    (bit (case array-type-specifier
	   (simple-array 'simple-bit-vector)
	   (complex-array 'complex-bit-vector)
	   (array 'bit-vector)))
    (base-char (case array-type-specifier
		 (simple-array 'simple-base-string)
		 (complex-array 'complex-base-string)
		 (array 'base-string)))
    (character (case array-type-specifier
		 (simple-array 'simple-extended-string)
		 (complex-array 'complex-extended-string)
		 (array 'string)))
    (t (case array-type-specifier
	 (simple-array 'simple-vector)
	 (complex-array 'complex-vector)
	 (array 'general-vector)))))
