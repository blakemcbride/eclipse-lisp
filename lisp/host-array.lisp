(locally-pretty-fast

(defun eclipse::make-base-char-vector
  (size &aux
	(class eclipse::simple-base-string-classobj)
	(wrapper eclipse::simple-base-string-wrapperobj)
	(contents (host:make-array size :element-type 'base-char)))
  (make-base-char-vector class wrapper size contents))

(defun eclipse::make-extended-char-vector
  (size &aux
	(class eclipse::simple-extended-string-classobj)
	(wrapper eclipse::simple-extended-string-wrapperobj)
	(contents (host:make-array size :element-type 'character)))
  (make-extended-char-vector class wrapper size contents))

(defun eclipse::make-general-vector
  (size &aux
	(class eclipse::simple-vector-classobj)
	(wrapper eclipse::simple-vector-wrapperobj)
	(contents (host:make-array size :element-type 't)))
  (make-general-vector class wrapper size contents))

(defun eclipse::make-digit-vector
  (size class &aux
	(wrapper (if (eq class eclipse::simple-bit-vector-classobj)
		     eclipse::simple-bit-vector-wrapperobj
		     (static-access class eclipse::wrapper eclipse::class)))
	(contents
	 ;; Clearing the unused bits in the last digit allows
	 ;; the digit to be used in bitwise logical operations.
	 (let* ((n (ceiling size eclipse::digit-size))
		(x (host:make-array n :element-type 'eclipse:index)))
	   (when (plusp n)
	     (setf (aref x (1- n)) 0))
	   x)))
  (make-digit-vector class wrapper size contents))

(defun eclipse::make-simple-array
  (rank dims contents &aux
	(class eclipse::simple-array-classobj)
	(dimensions (make-array rank
				:element-type 'eclipse:index
				:initial-contents
				(mapcar #'ffixnum-index dims)))
	(wrapper eclipse::simple-array-wrapperobj))
  (make-simple-nd-array class wrapper
			rank dimensions contents))

(defun eclipse::make-complex-array
  (class rank dims contents offset fill-pointer &aux
	 (dimensions (make-array rank
				 :element-type 'eclipse:index
				 :initial-contents
				 (mapcar #'ffixnum-index dims)))
	 (wrapper (cond ((eq class eclipse::complex-array-classobj)
			 eclipse::complex-array-wrapperobj)
			((eq class eclipse::complex-bit-vector-classobj)
			 eclipse::complex-bit-vector-wrapperobj)
			((eq class eclipse::complex-base-string-classobj)
			 eclipse::complex-base-string-wrapperobj)
			((eq class eclipse::complex-extended-string-classobj)
			 eclipse::complex-extended-string-wrapperobj)
			((eq class eclipse::complex-vector-classobj)
			 eclipse::complex-vector-wrapperobj)
			(t
			 (static-access class eclipse::wrapper eclipse::class)))))
  (make-complex-array class wrapper
		      rank dimensions contents
		      offset fill-pointer))
 
(defun ffixnum-index (x) (eclipse::fixnum-index x))
)

(deftype eclipse::simple-basic-vector () '(or eclipse::basic-vector simple-string))
(deftype eclipse::complex-basic-vector () 'eclipse::simple-nd-array)
(deftype eclipse:SIMPLE-STRING ()
  '(or simple-base-string eclipse::base-char-vector eclipse::extended-char-vector))
(deftype eclipse:SIMPLE-BASE-STRING () '(or simple-base-string eclipse::base-char-vector))
(deftype eclipse:SIMPLE-VECTOR () '(or simple-vector eclipse::general-vector))
(deftype eclipse:SIMPLE-BIT-VECTOR () '(or simple-bit-vector eclipse::digit-vector))
(deftype eclipse:SIMPLE-ARRAY ()
  '(or (and eclipse::simple-nd-array (not eclipse::complex-array))
       eclipse::basic-vector cl:string bit-vector))
(deftype eclipse:ARRAY ()
  '(or eclipse::complex-array eclipse::simple-nd-array
       eclipse::basic-vector cl:string bit-vector))

(defun eclipse::update-array (array contents rank dimensions offset fp)
  (setf (simple-nd-array-contents array) contents
	(simple-nd-array-rank array) rank
	(simple-nd-array-dimensions array)
	(make-array rank
		    :element-type 'eclipse:index
		    :initial-contents
		    (mapcar #'ffixnum-index dimensions))
	(complex-array-fill-pointer array) fp
	(complex-array-offset array) offset)
  array)


(locally-pretty-fast
 
;;; Extended to work on simple vectors from the host Lisp.
(defun eclipse::vector-size (x)
  (if (vectorp x) (length x)
      (basic-vector-size x)))

(defun eclipse::simple-array-contents (x) (simple-nd-array-contents x))
(defun eclipse::simple-array-rank (x) (simple-nd-array-rank x))
(defun eclipse::complex-array-offset (x) (complex-array-offset x))
(defun eclipse::complex-array-fill-pointer (x) (complex-array-fill-pointer x))

(defun eclipse::simple-array-dimension (array index)
  (elt (simple-nd-array-dimensions array) index))

(defun eclipse::set-complex-array-fill-pointer (array fp)
  (setf (complex-array-fill-pointer array) fp))

;;; The following two are extended to work on simple strings from the host Lisp.
(defun eclipse::base-char-elt (array index)
  (let ((string (if (simple-string-p array)
		    array
		    (base-char-vector-contents array))))
    (declare (type simple-string string))
    (schar string index)))

(defun eclipse::set-base-char-elt (array index char)
  (let ((string (if (simple-string-p array)
		    array
		    (base-char-vector-contents array))))
    (declare (type simple-string string))
    (setf (schar string index) char)))
    
            
(defun eclipse:charp-simple-base-string (string)
  (etypecase string
    (integer (if (zerop string)
		 nil
		 (error "Strange number ~s passed to charp-simple-base-string" string)))
    (cl:string (let* ((size (length string))
		      (s (eclipse::make-base-char-vector size))
		      (data (base-char-vector-contents s)))
	      (replace data string)
	      s))))
;;; Another name for base-char-vector-contents.
(defun eclipse::simple-base-string-charp (string)
  (string string))

(defun eclipse::extended-char-elt (array index)
  (schar (extended-char-vector-contents array) index))
(defun eclipse::set-extended-char-elt (array index char)
  (setf (schar (extended-char-vector-contents array) index) char))

;;; The folowing two are extended to work on simple vectors from the host Lisp.
(defun eclipse::general-elt (array index)
  (if (vectorp array) (elt array index)
      (svref (general-vector-contents array) index)))
(defun eclipse::set-general-elt (array index elt)
  (if (vectorp array) (setf (elt array index) elt)
      (setf (svref (general-vector-contents array) index) elt)))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                        DIGIT VECTORS                          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; When we compile this for speed on allegro, we find that:
;;; (setq xxx (float 5/7 1.0d0)) => a number
;;; (rational xxx) => strange error that doesn't make sense
;;; Could it be that some operator is mis-declared?

(locally-pretty-fast
 
;;; Returns an index.
(defun eclipse::indexref (array index)
  (if (simple-bit-vector-p array)
      (loop with n = 0
	    for ni from 0
	    and ai from (* index eclipse::digit-size)
	    below (min (length array) (* (1+ index) eclipse::digit-size))
	    do (setf (ldb (byte 1 ni) n)
		     (sbit array ai))
	    finally (return n))
      (aref (digit-vector-contents array) index)))
(defun eclipse::set-indexref (array index index-digit)
  (setf (aref (digit-vector-contents array) index)
	(logand eclipse::digit-mask index-digit)))

;;; Returns a fixnum.
(defun eclipse::digitref (array index)
  (eclipse:index-fixnum (eclipse::indexref array index)))
(defun eclipse::set-digitref (array index digit)
  (eclipse::set-indexref array index
			 (eclipse:integer-index
			  (logand eclipse::digit-mask digit)))
  digit)

;;; The following two are extended to work on simple bit-vectors from the host Lisp.
;;; The "bit" set/returned by these is an index, not a fixnum.
(defun eclipse::bit-elt (array index)
  (if (simple-bit-vector-p array) (sbit array index)
      (multiple-value-bind (digit-index bit-index)
	  (floor index eclipse::digit-size)
	(ldb (byte 1 bit-index)
	     (eclipse::indexref array digit-index)))))
		 
(defun eclipse::set-bit-elt (array index bit)
  (etypecase bit
    (bit (if (simple-bit-vector-p array) (setf (sbit array index) bit)
	     (multiple-value-bind (digit-index bit-index)
		 (floor index eclipse::digit-size)
	       (eclipse::set-indexref
		array digit-index
		(dpb bit (byte 1 bit-index)
		     (eclipse::indexref array digit-index)))))))
  bit)

;;; BIGNUMS
(defun signed-int-integer (int)
  (let ((x (logand #xffffffff int)))
    (if (logbitp 31 x)
	(- (mask-field (byte 32 0) (+ 1 (lognot x))))
      x)))

(defun eclipse::fixnum-int (integer) (the fixnum integer))

(defun int-xint (int)
  (declare (type ec:int int))
  (nload-xint (eclipse::make-xint 2) int))

(defun eclipse::integer-xint (integer)
  (if (host::fixnump integer)
      (int-xint (eclipse::fixnum-int integer))
      (eclipse::bignum-xint integer)))

;;; A cast and then a pointer reference? The result must be cast into
;;; a signed quantity, though.
(defun eclipse::xint-fixnum (xint)
  (signed-int-integer
   (dpb (eclipse::digitref xint 1)
	(byte eclipse::digit-size eclipse::digit-size)
	(eclipse::digitref xint 0))))

;;; Just a cast.
(defun eclipse::xint-bignum (xint &aux (l (eclipse::xint-size xint)))
  (do* ((i (1- l) (1- i))
	(sum (eclipse::digitref xint i)
	     (+ (* eclipse::digit-base sum)
		(let ((ref (eclipse::digitref xint i)))
		  (when (>= ref eclipse::digit-base)
		    (warn "~d >= ~d." ref eclipse::digit-base))
		  ref))))
      ((zerop i)
       (if (eclipse::signp-xint xint (1- l))
	   (- (mask-field (byte (integer-length sum) 0)
			  (+ 1 (lognot sum))))
	   sum))))

;;; Just a union access.
(defun eclipse::bignum-xint (n)
  (let* ((size (eclipse::compute-xint-size (integer-length n)))
	 (p (eclipse::make-xint size)))
    (setq n (mask-field (byte (* size eclipse::digit-size) 0) n))
    (dotimes (i size p)
      (eclipse::set-digitref p i (mod n eclipse::digit-base))
      (setq n (floor n eclipse::digit-base)))))

(defun eclipse::lognot-fixnum (fix) (lognot (the fixnum fix)))
(defun eclipse::logcount-fixnum (f) (logcount (the fixnum f)))
(defun eclipse::length-fixnum (f) (integer-length (the fixnum f)))

;;; N.B.: This relies on fixnum-size being a host:fixnum!
(defconstant host::fixnum-size #.(integer-length cl:most-positive-fixnum))
(defun eclipse::ash-fixnum (int count)
  (declare (type fixnum int count))
  (let ((result (ash int count)))
    (if (logbitp 32 result)
	(lognot (ldb (byte host::fixnum-size 0)
		     (lognot result)))
	(ldb (byte host::fixnum-size 0) result))))

(defun eclipse::logtest-fixnum (f1 f2) (declare (type fixnum f1 f2))
  (logtest f1 f2))
(defun eclipse::logand-fixnum (f1 f2) (declare (type fixnum f1 f2))
  (logand f1 f2))
(defun eclipse::logxor-fixnum (f1 f2) (declare (type fixnum f1 f2))
  (logxor f1 f2))
(defun eclipse::logior-fixnum (f1 f2) (declare (type fixnum f1 f2))
  (logior f1 f2))
(defun eclipse::logbitp-fixnum (f1 f2) (declare (type fixnum f1 f2))
  (logbitp f1 f2))


;(defconstant digit-high (deposit-field -1 (byte eclipse::digit-size eclipse::digit-size) 0))
(defun eclipse::DIGIT-LOW (v) (logand v eclipse::digit-mask))
(defun eclipse::DIGIT-HIGH (v)
  ;; N.B.: ash/and ordering is different in C! 
  ;; i.e. (ash (logand v digit-high) (- digitsize))
  (logand (ash v (- eclipse::digit-size))
	  eclipse::digit-mask))

(defun eclipse::signp-digit (digit)
  (logbitp #.(1- eclipse::digit-size)
	   digit))

(defun eclipse::make-xint (size)
  (eclipse::make-digit-vector (eclipse::integer-index (* size eclipse::digit-size))
			      (eclipse::find-type 'eclipse::integer)))

(defun eclipse::make-bit-vector (size)
  (eclipse::make-digit-vector (eclipse::integer-index size)
			      eclipse::simple-bit-vector-classobj))

(defun eclipse::xint-size (xint)
  (values (ceiling (basic-vector-size xint) eclipse::digit-size)))

(defun nload-xint (xint int)
  (let* ((i (eclipse::int-integer int))
	 (n (mask-field (byte (* 2 eclipse::digit-size) 0) i)))
    (eclipse::set-digitref xint 0 (ldb (byte eclipse::digit-size 0) n))
    (eclipse::set-digitref xint 1 (ldb (byte eclipse::digit-size eclipse::digit-size) n))
    xint))

;;; Reset length field to avoid operating on pad digits.
(defun eclipse::trim-xint (xint new-size)
  (let ((contents (digit-vector-contents xint)))
    (cond ((= new-size (length contents))
	   xint)
	  (t (setf (basic-vector-size xint) (* new-size eclipse::digit-size))
	     (setf (digit-vector-contents xint)
		   (subseq contents 0 new-size))
	     xint))))
)
