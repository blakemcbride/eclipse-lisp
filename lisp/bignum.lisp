;;; XINTs are a "vector" of SHORT digits, arranged from least
;;; significant to most significant.  Digits are accessed with
;;; DIGITREF. 
;;; 
;;; XINTs are created with MAKE-XINT, and the number of digits is
;;; obtained with XINT-SIZE.
;;; 
;;; XINT-INT extracts the signed c:int value of xint, iff xint
;;; happens to represent a value that small.  The consequences are
;;; undefined, otherwise. LOAD-XINT places a signed c:int into the low
;;; two digits of xint.
;;;
;;; xint arguments are generally assumed to be in cannonical form: 
;;; - positive xints are padded with an extra high order 0 digit if
;;;   the otherwise high order digit has a 1 in its most significant
;;;   bit. 
;;; - negative xints are padded with an extra high order digit-mask
;;;   digit if the otherwise high order digit has a 0 in its most
;;;   significant bit. 
;;; Thus signp-xint of the highest digit can be used to tell if a
;;; cannonicalized xint is negative.  xint-integer trims off
;;; unnecessary digits, so bignums seen by users are always
;;; cannonicalized. 
;;;
;;; uxint arguments represent unsigned numbers and are not
;;; cannonicalized.  They may have a 1 as the most significant bit of
;;; any digit, and they may have any number of 0 pad digits.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ACCESS                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun compute-xint-size (bits)
  (ceiling (+ 1 bits) digit-size))

(defun SIGNP-XINT (xint last-index)
  (signp-digit (digitref xint last-index)))

(defun DIGITREF-EXTEND (xint i &aux (l (xint-size xint)))
  (if (zerop l)
      0
      (if (>= i l)
	  (if (signp-xint xint (1- l))
	      digit-mask
	      0)
	  (digitref xint i))))

(defun COMPLEMENT-DIGIT (digit)
  (logand-fixnum digit-mask (lognot-fixnum digit)))

(defun length-xint (xint)
  (let* ((last (1- (xint-size xint)))
	 (digit (digitref xint last)))
    (+ (* last digit-size)
       (if (and (= digit DIGIT-MASK)
		(signp-digit (digitref xint (1- last))))
	   -1
	   (length-fixnum (if (signp-digit digit)
			      (complement-digit digit)
			      digit))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CONVERSION                                                   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun XINT-DOUBLE-FLOAT (xint)
  (let ((l (xint-size xint)))
    (flet ((digit (xint i minusp)
		  (let ((digit (digitref xint i)))
		    (if minusp
			(- (if (zerop i) -1 0) (complement-digit digit))
			digit))))
      (if (zerop l)
	  0.0d0
	  (do* ((i (1- l) (1- i))
		(minusp (signp-xint xint i))
		(sum (float (digit xint i minusp) 1.0d0)
		     (add (mult digit-base sum)
			  (digit xint i minusp))))
	      ((zerop i) sum))))))

(defun DOUBLE-FLOAT-XINT (double)
  (let* ((minusp (minusp double))
	 (n (if minusp (- -1 double) double))
	 (size (compute-xint-size (log n 2.0d0)))
	 (p (make-xint size)))
    (declare (type double-float double n)
	     (type fixnum size))
    (dotimes (i size p)
      (multiple-value-bind (next digit) (ffloor n digit-base)
	(declare (type double-float next digit))
	(let ((digit (truncate digit)))
	  (declare (type fixnum digit))
	  (set-digitref p i (if minusp
				(complement-digit digit)
				 digit))
	  (setq n next))))))

(defun XINT-INTEGER (xint) (xint-integer2 xint nil))
(defun XINT-INTEGER2 (xint copyp)
  (let* ((last (1- (xint-size xint)))
	 (minusp (signp-xint xint last))
	 (pad (if minusp digit-mask 0)))
    (setq last
	  (do ((n last (1- n)))
	      ((zerop n) (if (xor minusp (signp-xint xint 0)) 1 0))
	    (let ((digit (digitref xint n)))
	      (when (/= digit pad)
		(return (+ n
			   (if (xor minusp (signp-digit digit))
			       1 0)))))))
    (cond ((zerop last)
	   (let ((digit (digitref xint last)))
	     (if (signp-digit digit)
		 (- -1 (complement-digit digit))
		 digit)))
	  ((<= (length-xint xint) fixnum-size)
	   (xint-fixnum xint))
	  (t (xint-bignum
	      (if copyp
		  (copy-xint xint (1+ last))
		  (trim-xint xint (1+ last))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TRAVERSALS                                                   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ncopy-xint (new old n)
  (dotimes (i n new)
    (set-digitref new i (digitref-extend old i))))

(defun copy-xint (old n)
  (ncopy-xint (make-xint n) old n))

(defun cmp-uxint (a b &optional size)
  (do* ((na (xint-size a))
	(nb (xint-size b))
	(i (1- (or size (max na nb))) (1- i)))
      ((minusp i) 0)
    (let ((a (if (>= i na) 0 (digitref a i)))
	  (b (if (>= i nb) 0 (digitref b i))))
      (cond ((< a b) (return -1))
	    ((> a b) (return 1))))))

;; Side-effects uxint to represent the positive SHORT n.
(defun nload-uxint (uxint n)
  (do ((size (xint-size uxint))
       (i 1 (1+ i)))
      ((>= i size) (set-digitref uxint 0 n) uxint)
    (set-digitref uxint i 0)))

(defun NCOMPLEMENT-XINT (v)
  (let ((carry 1))
    (dotimes (i (xint-size v) v)
      (let ((digit (+ carry
		      (logand digit-mask
			      (lognot (digitref v i))))))
	(setq carry (digit-high digit))
	(set-digitref v i (digit-low digit))))))

(defun nadd-xint (a b c n)
  (let ((carry 0))
    (dotimes (i n c)
      (let ((sum (+ carry (digitref-extend a i) (digitref-extend b i))))
	(set-digitref c i (digit-low sum))
	(setq carry (digit-high sum))))))

(defun nclear-uxint (xint size)
  (dotimes (k size) (set-digitref xint k 0)))

(defun mult-uxint (a b)
  (let* ((len-a (xint-size a))
	 (len-b (xint-size b))
	 (len-res (+ len-a len-b))
	 (res (make-xint len-res)))
    (nclear-uxint res len-res)
    (dotimes (i len-a res)
      (let ((carry-digit 0)
	    (x (digitref a i))
	    (k i))
	(dotimes (j len-b)
	  (let* ((low (mult x (digitref b j)))
		 (high (digit-high low)))
	    (setq low (digit-low low))
	    (setq low (add low carry-digit)
		  high (add high (digit-high low))
		  low (digit-low low))
	    (setq low (add low (digitref res k))
		  high (add high (digit-high low))
		  low (digit-low low))
	    (set-digitref res k low)
	    (setf carry-digit high))
	  (incf k))
	(set-digitref res k carry-digit)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SIGNED, CANONICALIZED ARITHMETIC                             ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun cmp-xint (a b)
  (let* ((na (xint-size a))
	 (nb (xint-size b))
	 (sign-a (signp-xint a (1- na)))
	 (sign-b (signp-xint b (1- nb))))
    (cond ((and sign-a (not sign-b)) -1)
	  ((and sign-b (not sign-a)) 1)
	  ((< na nb) (if sign-a 1 -1))
	  ((> na nb) (if sign-a -1 1))
	  (t (do ((i (1- na) (1- i)))
		 ((minusp i) 0)
	       (let ((a (digitref a i))
		     (b (digitref b i)))
		 (cond ((< a b) (return -1))
		       ((> a b) (return 1)))))))))

(defun add-xint (a b)
  (let ((n (1+ (max (xint-size a) (xint-size b)))))
    (nadd-xint a b (make-xint n) n)))

(defun nsubt-xint (a b c n)
  (nadd-xint a (ncomplement-xint b) c n))

(defun subt-xint (a b)
  (let* ((n (1+ (max (xint-size a) (xint-size b))))
	 (b (copy-xint b n)))
    (nsubt-xint a b b n)))

(defun mult-xint (a b)
  (let* ((la (xint-size a))
	 (lb (xint-size b))
	 (sign-a (signp-xint a (1- la)))
	 (sign-b (signp-xint b (1- lb)))
	 (sign (xor sign-a sign-b)))
    (when sign-a (ncomplement-xint a))
    (when sign-b (ncomplement-xint b))
    (let ((c (mult-uxint a b)))
      (when sign-a (ncomplement-xint a))
      (when sign-b (ncomplement-xint b))
      (if sign (ncomplement-xint c) c))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DIVISION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun xint-buf (symbol size)
  (let ((buf (symbol-value symbol)))
    (if (>= (xint-size buf) size)
	buf
      (set symbol (make-xint size)))))

;;; Q and R is bashed. NUM and DENOM are not.
;;; SUBTRAHEND is the denominator shifted over so that its most
;;; significant bit lines up with the most significant bit of the NUM.
;;; With each step of the division, SUBTRAHEND gets shifted right by 1
;;; bit. 
;;; REM is the remainder - it keeps getting SUBTRAHEND or 0 subtracted
;;; off.  We decide whether to subtract off SUBTRAHEND or 0 by whether
;;; REM>=SUBTRAHEND.  If so, we subtract off SUBTRAHEND and add a 1 to
;;; Q in the current bit position. 

(defparameter *long-division-rem* (make-xint 8))
(defparameter *long-division-subtrahend* (make-xint 8))
(defparameter *long-division-buf* (make-xint 8))
(defun long-division (num denom q r &aux
			  (size (1+ (max (xint-size num) (xint-size denom))))
			  (subtrahend (xint-buf '*long-division-subtrahend* size))
			  (rem (xint-buf '*long-division-rem* size))
			  (buf (xint-buf '*long-division-buf* size))
			  (buf-size (xint-size buf)))
  (flet ((length-uxint (uxint)
		       (do ((last (1- (xint-size uxint)) (1- last))
			    digit)
			   ((not (zerop (setq digit (digitref uxint last))))
			    (+ (* last digit-size)
			       (length-fixnum digit))))))
    ;; This keeps us from bashing values we need.
    (symbol-macrolet ((sub-copy (ncopy-xint buf subtrahend buf-size)))
      (nclear-uxint q (xint-size q))
      (ncopy-xint rem num (xint-size rem))
      (ncopy-xint subtrahend denom (xint-size subtrahend))
      (do* ((shift (- (length-uxint num) (length-uxint denom))
		   (1- shift))
	    (subtrahend (ash-xint sub-copy shift subtrahend size)
			(ash-xint sub-copy -1 subtrahend size)))
	  ((< shift 0)
	   (values q (if r
			 (ncopy-xint r rem (xint-size r))
			 rem)))
	(when (>= (cmp-uxint rem subtrahend size) 0)
	  (nsubt-xint rem sub-copy rem size)
	  (multiple-value-bind (digits bits)
	      (truncate shift digit-size)
	    (set-digitref q digits
			  (logior (digitref q digits) (ash 1 bits)))))))))

(defparameter *trunc-xint* (make-xint 8))
(defun TRUNC-XINT (a b)
  (let* ((la (xint-size a))
	 (lb (xint-size b))
	 (sign-a (signp-xint a (1- la)))
	 (sign-b (signp-xint b (1- lb)))
	 (sign (xor sign-a sign-b))
	 (q (make-xint la))
	 (r (make-xint la)))
    (when sign-a
      ;; Without this growth, the long-division won't work.
      ;; IWBNI we knew why.  Failure case is a=#x8000...
      (let ((buf (xint-buf '*trunc-xint* (1+ la))))
	(setq a (ncopy-xint buf a (xint-size buf))))
      (ncomplement-xint a))
    (when sign-b (ncomplement-xint b))
    (multiple-value-bind (q r) (long-division a b q r)
      (when sign-a (ncomplement-xint r))
      (when sign-b (ncomplement-xint b))
      (when sign (ncomplement-xint q))
      (values q r))))

;;; DIVISION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar xint-one (integer-xint 1))
(defvar xint-zero (integer-xint 0))

;;; Q is just for internal use by long-division; bashed.
(defparameter *gcd-uxint-q* (make-xint 8))
(defparameter *gcd-uxint-v* (make-xint 8))
(defun gcd-uxint (a b q r &aux
		    (size (xint-size r))
		    (q-buf (xint-buf '*gcd-uxint-q* size))
		    (v-buf (xint-buf '*gcd-uxint-v* size)))
  (do ((u a)
       (v b))
      ((<= (cmp-uxint u xint-zero size) 0) v)
    (when (< (cmp-uxint u v size) 0) (rotatef u v))
    (setq u (ncopy-xint q-buf u size)
	  v (ncopy-xint v-buf v size))
    (setq u (nth-value 1 (long-division u v q r)))))

(defparameter *div-xint-r* (make-xint 8))
(defparameter *div-xint-q* (make-xint 8))
(defparameter *div-xint-buf* (make-xint 8))
;;; Uses xint-integer2, which makes a copy of the xint iff a bignum
;;; ends up being returned.  This is because the xint's passed to
;;; xint-integer2 here are either user arguments (N & D) or pool
;;; allocated xints, which need to be copied (unless, of course, only
;;; a fixnum ends up being returned).
(defun DIV-XINT (n d)
  (let* ((ln (xint-size n))
	 (ld (xint-size d))
	 (l (max ln ld))
	 (sign-n (signp-xint n (1- ln)))
	 (sign-d (signp-xint d (1- ld)))
	 (sign (xor sign-n sign-d))
	 (q (xint-buf '*div-xint-q* l))
	 (r (xint-buf '*div-xint-r* l)))
    (when sign-n (ncomplement-xint n))
    (when sign-d (ncomplement-xint d))
    (flet ((make-ratio (a b)
		       (make-ratio (xint-integer2
				    (if sign (ncomplement-xint a) a)
				    t)
				   (xint-integer2 b t)))
	   (fix-signs ()
		      (when sign-n (ncomplement-xint n))
		      (when sign-d (ncomplement-xint d))))
      (let ((gcd (gcd-uxint n d q r)))
	(cond ((= (cmp-uxint gcd d l) 0)
	       (prog1
		   (xint-integer2
		    (let ((reduced
			   (if (= (cmp-uxint gcd xint-one l) 0)
			       n
			       (long-division n gcd q r))))
		      (if sign (ncomplement-xint reduced) reduced))
		    t)
		 (fix-signs)))
	      ((= (cmp-uxint gcd xint-one l) 0)
	       (prog1 (make-ratio n d) (fix-signs)))
	      (t (prog1
		     (make-ratio (let ((buf (xint-buf '*div-xint-buf* l)))
				   ;; Copy result so we can reuse q.
				   (ncopy-xint buf
					       (long-division n gcd q r)
					       (xint-size buf)))
				 (long-division d gcd q r))
		   (fix-signs))))))))


