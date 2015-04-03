;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                ACCESS UTILITIES                              ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; These all take fixnum indices and do bounds checking.

(defun sbvref (bit-vector index)
  (index-fixnum
   (bit-elt bit-vector (check-simple-index bit-vector index))))
(defun set-sbvref (bit-vector index bit)
  (set-bit-elt bit-vector (check-simple-index bit-vector index)
	       (fixnum-index bit))
  bit)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgeneric row-major-bitref (array index))
(defmethod row-major-bitref ((array simple-bit-vector) (index integer))
  (sbvref array index))

(defmethod row-major-bitref ((array simple-array) (index integer))
  (row-major-bitref (simple-array-contents array) index))

(defmethod row-major-bitref ((array array) (index integer))
  (row-major-bitref (simple-array-contents array)
		    (+ index (complex-array-offset array))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgeneric (setf row-major-bitref) (bit array index))
(defmethod (setf row-major-bitref) ((bit integer)
				    (array simple-bit-vector) (index integer))
  (set-sbvref array index bit))

(defmethod (setf row-major-bitref) ((bit integer)
				    (array simple-array) (index integer))
  (setf (row-major-bitref (simple-array-contents array) index)
	bit))

(defmethod (setf row-major-bitref) ((bit integer) (array array) (index integer))
  (setf (row-major-bitref (simple-array-contents array)
			  (+ index (complex-array-offset array)))
	bit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 17.4 FUNCTIONS ON ARRAYS OF BITS                             ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This happens to work on non-simple bit arrays, too.
(defun SBIT (array &rest indices)
  (declare (dynamic-extent indices))
  (row-major-bitref
   array (apply #'array-row-major-index array indices)))

(defun (SETF SBIT) (new-elt array &rest indices)
  (declare (dynamic-extent indices))
  (setf (row-major-bitref
	 array (apply #'array-row-major-index array indices))
	new-elt))

(defun BIT (array &rest indices)
  (declare (dynamic-extent indices))
  (apply #'sbit array indices))

(defun (SETF BIT) (array &rest indices)
  (declare (dynamic-extent indices))
  (apply #'(cl:setf sbit) array indices))

(defun bit-boole (op bit-array1 bit-array2 result-array)
  (multiple-value-bind (array1 offset1)
      (get-simple-vector bit-array1 0)
    (multiple-value-bind (array2 offset2)
	(get-simple-vector bit-array2 0)
      (multiple-value-bind (array3 offset3)
	  (cond ((eq 't result-array)
		 (setq result-array bit-array1)
		 (values array1 offset1))
		(result-array
		 (get-simple-vector result-array 0))
		(t		;null result-array
		 (setq result-array
		   (make-array (array-dimensions bit-array1)
			       :element-type 'bit))
		 (get-simple-vector result-array 0)))
	(let ((l (array-total-size bit-array1)))
	  (unless (= l (array-total-size bit-array2)
		     (array-total-size result-array))
	    (error "~s, ~s and ~s are not the same length."
		   bit-array1 bit-array2 result-array))
	  (if (= 0 offset1 offset2 offset3)
	      (let ((size (ceiling l digit-size)))
		(dotimes (i size)
		  (set-digitref array3 i
				(boole op
				       (digitref array1 i)
				       (digitref array2 i)))))
	    ;; It is important to still wiz along by bytes, but
	    ;; dealing with the synchronization problems is too hard
	    ;; for this release!!!
	    (dotimes (i l)
	      (let ((a (sbit array1 (+ i offset1)))
		    (b (sbit array2 (+ i offset2))))
		(declare (type bit a b))
		(setf (sbit array3 (+ i offset3))
		   (logand 1 (boole op a b))))))))))
  result-array)

(macrolet ((def-bit (op)
	       `(defun ,(make-name "BIT-~a" op)
		    (bit-array1 bit-array2 &optional result)
		  (bit-boole ,(make-name "BOOLE-~a"op)
			     bit-array1 bit-array2 result))))
  (def-bit and)
  (def-bit ior)
  (def-bit xor)
  (def-bit eqv)
  (def-bit nand)
  (def-bit nor)
  (def-bit andc1)
  (def-bit andc2)
  (def-bit orc1)
  (def-bit orc2))

(defun bit-not (bit-array &optional result)
  (bit-boole boole-c1 bit-array bit-array result))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                     PRINTING                                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod PRINT-OBJECT ((object bit-vector) s)
  (cond ((or *print-array* *print-readably*)
	 (write-string "#*" s)
	 (dotimes (i (length object))
	   (write-char (if (zerop (bit object i)) #\0 #\1) s)))
	(t (print-unreadable-object
	       (object s :type t :identity t)))))