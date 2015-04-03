
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 12.7 LOGICAL OPERATIONS ON NUMBERS                           ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun boole-fixnum (op a b)
  (declare (type fixnum op a b ))
  (cond ((logtest-fixnum op eclipse:boole-c1) (setq a (lognot-fixnum a)))
	((not (logtest-fixnum op eclipse:boole-1)) (setq a 0)))
  (cond ((logtest-fixnum op eclipse:boole-c2) (setq b (lognot-fixnum b)))
	((not (logtest-fixnum op eclipse:boole-2)) (setq b 0)))
  (let ((c (cond ((logtest-fixnum op b-and) (logand-fixnum a b))
		 ((logtest-fixnum op b-xor) (logxor-fixnum a b))
		 (t (logior-fixnum a b)))))
    (declare (type fixnum c))
    (when (logtest-fixnum op b-not) (setq c (lognot-fixnum c)))
    c))

(defun BOOLE (op integer1 integer2)
  (if (and (typep integer1 'fixnum) (typep integer2 'fixnum))
      (boole-fixnum op integer1 integer2)
      (let* ((xint1 (integer-xint integer1))
	     (xint2 (integer-xint integer2))
	     (size (max (xint-size xint1) (xint-size xint2)))
	     (xint (make-xint size)))
	(dotimes (i size (xint-integer xint))
	  (set-digitref xint i
			(boole-fixnum op
				      (digitref-extend xint1 i)
				      (digitref-extend xint2 i)))))))

(macrolet
    ((def-log (op basis)
       (let ((name (make-name "LOG~a" op))
	     (key (make-name "BOOLE-~a" op)))
	 `(defun ,name (&optional (n ,basis) &rest integers)
	    (declare (dynamic-extent integers))
	    (if integers
		(dolist (integer integers n)
		  (setq n (boole ,key n integer)))
		n)))))
  (def-LOG IOR 0)
  (def-LOG XOR 0)
  (def-LOG AND -1)
  (def-LOG EQV -1))

(macrolet ((def-log (op)
	       (let ((name (make-name "LOG~a" op))
		     (key (make-name "BOOLE-~a" op)))
		 `(defun ,name (integer1 integer2)
		    (boole ,key integer1 integer2)))))
  (def-LOG NAND)
  (def-LOG NOR)
  (def-LOG ANDC1)
  (def-LOG ANDC2)
  (def-LOG ORC1)
  (def-LOG ORC2))

(defun LOGNOT (integer) (boole boole-c1 integer 0))

;;; We really only need to check all the digits if the smaller one is
;;; negative.  Otherwise we could check just min number of digits.
;;; However, there's usually few enough digits that it its better to
;;; do less screwing around.
(defun logtest-xint (xint1 xint2)
  (dotimes (i (max (xint-size xint1)
		   (xint-size xint2))
	     nil)
    (when (logtest-fixnum (digitref-extend xint1 i)
			  (digitref-extend xint2 i))
      (return t))))

(defun LOGTEST (integer1 integer2)
  (if (and (fixnump integer1)
	   (fixnump integer2))
      (logtest-fixnum integer1 integer2)
      (logtest-xint (integer-xint integer1)
		    (integer-xint integer2))))

(defun LOGBITP (index integer)
  (if (fixnump index)
      (if (fixnump integer)
	  (logbitp-fixnum index integer)
	  (multiple-value-bind (word offset)
	      (floor index digit-size)
	     (logbitp-fixnum offset
			     (digitref-extend
			      (bignum-xint integer)
			      word))))
      (minusp integer)))

(defun ash-chunk-xint (xint count index)
  (cond ((minusp index) 0)
	(t
	 (when (minusp count)
	   (incf index) (incf count digit-size))
	 (logior (ash-fixnum (digitref-extend xint index)
			     count)
		 (ash-fixnum (if (zerop index) 0
				 (digitref-extend xint (1- index)))
			     (- count digit-size))))))

(defun ash-chunk-fixnum (integer count index)
  (if (minusp index) 0
    (case index
      (0 (ash-fixnum integer count))
      (1 (ash-fixnum integer (- count digit-size)))
      (2 (ash-fixnum integer (- count digit-size digit-size)))
      (t (if (minusp integer) -1 0)))))

(defun ash-xint (xint count result size)
  (multiple-value-bind (digits bits) (truncate count digit-size)
    (dotimes (i size result)
      (set-digitref result i (ash-chunk-xint xint bits (- i digits))))))

(defun ASH (integer count)
  (let ((result-length (+ (integer-length integer) count)))
    (cond ((<= result-length 0)
	   (if (minusp integer) -1 0))
	  ((not (fixnump result-length))
	   (error 'ARITHMETIC-ERROR
		  :operation 'ash
		  :operands (list integer count)))
	  ((and (<= result-length
		    #+machine-compile fixnum-size
		    #-machine-compile host::fixnum-size)
		(fixnump integer))
	   ;(if (fixnump integer) ;why doesn't this work???
	       (ash-fixnum integer count)
	       ;(ash-chunk-xint (integer-xint integer) count 0))
	   )
	  (t (let* ((size (compute-xint-size result-length))
		    (xint (make-xint size)))
	       (if (fixnump integer)
		   (multiple-value-bind (digits bits)
		       (truncate count digit-size)
		     (dotimes (i size (xint-integer xint))
		       (set-digitref xint i
				     (ash-chunk-fixnum integer bits (- i digits)))))
		   (xint-integer (ash-xint (integer-xint integer) count xint size))))))))
		     
	  
(defun logcount-xint (xint)
  (let ((size (xint-size xint))
	(count 0))
    (if (signp-xint xint (1- size))
	(dotimes (i size count)
	  (incf count
		(logcount-fixnum
		 (logand-fixnum digit-mask
				(lognot-fixnum (digitref xint i))))))
	(dotimes (i size count)
	  (incf count (logcount-fixnum (digitref xint i)))))))

(defun LOGCOUNT (integer)
  (etypecase integer
    (fixnum (logcount-fixnum
	     (if (minusp integer)
		 (lognot-fixnum integer)
		 integer)))
    (integer (logcount-xint (bignum-xint integer)))))


(defun INTEGER-LENGTH (integer)
  (etypecase integer
    (fixnum (length-fixnum integer))
    (integer (length-xint (bignum-xint integer)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 12.8 BYTE MANIPULATION FUNCTIONS                             ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun BYTE (size position) (cons size position))
(defun BYTE-SIZE (bytespec) (car bytespec))
(defun BYTE-POSITION (bytespec) (cdr bytespec))

(defun LDB (bytespec integer)
  (logandc2 (ash integer (- (byte-position bytespec)))
	    (- (ash 1 (byte-size bytespec)))))

(defun LDB-TEST (bytespec integer)
  (logtest (mask-field bytespec -1) integer))

(defun MASK-FIELD (bytespec integer)
  (ash (ldb bytespec integer) (byte-position bytespec)))

(defun DPB (newbyte bytespec integer)
  (logxor integer
	  (mask-field bytespec integer)
	  (ash (logandc2 newbyte
			 (- (ash 1 (byte-size bytespec))))
	       (byte-position bytespec))))

(defun DEPOSIT-FIELD (newbyte bytespec integer)
  (dpb (ash newbyte (- (byte-position bytespec)))
       bytespec integer))


