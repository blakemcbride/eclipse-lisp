;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                   PRINT OBJECT METHODS                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; N.B.  The spec says that print-object methods are not intended to
;;; be called from user code.  These methods are called in the context
;;; of write, which must do level abbreviation, so it descends a
;;; level.  Some of these methods call pprint-fill, which also
;;; descends a level (to do level abbreviation).  Since we don't want
;;; to descend twice, and since these methods are not supposed to be
;;; called directly, we ASCEND a level before calling pprint-fill.

(defgeneric PRINT-OBJECT (object stream))

(defmethod PRINT-OBJECT (object stream)
  (print-unreadable-object (object stream :identity t :type t))
  object)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                          LISTS                               ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod PRINT-OBJECT ((object CONS) s)
  (pprint-fill s object t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                          CHARACTERS                          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun write-char-bits (char stream)
  (when (char-bit char :control t)
    (write-string "Control-" stream))
  (when (char-bit char :meta t)
    (write-string "Meta-" stream))
  (setq char (set-char-bit char :control nil :meta nil))
  (let ((name (char-name char)))
    (if name
	(write-string name stream)
      (write-char char stream))))

(defun print-character-pretty (char stream readably-p identify-p)
  (when readably-p (write-string "#\\" stream))
  (let ((name (char-name char)))
    (cond (name (write-string name stream))
	  ((not (graphic-char-p char))
	   (write-char-bits char stream)
	   (setq identify-p nil))
	  (t (write-char char stream))))
  (when (and identify-p (or (char-bit char :control t :meta t)
			    (member char '(#\rubout))))
    (write-string " (" stream)
    (case char
      (#\rubout (write-string "Delete" stream))
      (t (write-char-bits char stream)))
    (write-char #\) stream)))

(defmethod PRINT-OBJECT ((char CHARACTER) stream)
  (if (or *print-escape* *print-readably*)
      (print-character-pretty char stream t nil)
    (write-char char stream)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                          RATIONALS                           ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun write-leading-radix (s ratiop &aux (radix *print-base*))
  (when (and *print-radix* (or ratiop (/= radix 10)))
    (write-char #\# s)
    (write-char (case radix
		  (2 #\b)
		  (8 #\o)
		  (16 #\x)
		  (t (print-fixnum10 radix s) #\r)))))

(defun write-trailing-radix (s)
  (when (and *print-radix* (= *print-base* 10))
    (write-char #\. s)))

(defun write-sign (x s)
  (cond ((minusp x) (write-char #\- s) (- x))
	(t x)))

(defun PRINT-FIXNUM (fixnum s &aux (radix *print-base*))
  (multiple-value-bind (digits d)
      (truncate fixnum radix)
    (unless (zerop digits)
      (print-fixnum digits s))
    (write-char (digit-char d radix) s)))

(defun PRINT-FIXNUM10 (fixnum s)
  (let ((*print-base* 10)) (print-fixnum fixnum s)))

(defun print-integer1 (x s)
  (if (fixnump x)
      (print-fixnum x s)
      (print-bignum x s)))

(defun print-integer2 (x s)
  (print-integer1 (write-sign x s) s))

(defun PRINT-INTEGER (x s)
  (write-leading-radix s nil)
  (print-integer2 x s)
  (write-trailing-radix s))

(defmethod PRINT-OBJECT ((x INTEGER) s) (print-integer x s))
(defmethod PRINT-OBJECT ((x RATIO) s)
  (write-leading-radix s t)
  (print-integer2 (numerator x) s)
  (write-char #\/ s)
  (print-integer1 (denominator x) s))


;;; Comes from CMUCL
;;; *BASE-POWER* holds the number that we keep dividing into the bignum for
;;; each *print-base*.  We want this number as close to *most-positive-fixnum*
;;; as possible, i.e. (floor (log most-positive-fixnum *print-base*)).
;;; 
(defparameter *base-power* (make-array 37 :initial-element nil))
;;; *FIXNUM-POWER--1* holds the number of digits for each *print-base* that
;;; fit in the corresponding *base-power*.
(defparameter *fixnum-power--1* (make-array 37 :initial-element nil))
;;; We first generate the correct value for
;;; *base-power* and *fixnum-power--1* if we have not already.  Then we call
;;; bignum-print-aux to do the printing.
(defun PRINT-BIGNUM (x s)
  (unless (aref *base-power* *print-base*)
    (do ((power-1 -1 (1+ power-1))
	 (new-divisor *print-base* (* new-divisor *print-base*))
	 (divisor 1 new-divisor))
	((not (fixnump new-divisor))
	 (setf (aref *base-power* *print-base*) divisor)
	 (setf (aref *fixnum-power--1* *print-base*) power-1))))
  (bignum-print-aux x
		    (aref *base-power* *print-base*)
		    (aref *fixnum-power--1* *print-base*)
		    s))

(defun bignum-print-aux (bign divisor power-1 stream)
  (multiple-value-bind (newbig fix) (truncate bign divisor)
    (if (fixnump newbig)
	(print-fixnum newbig stream)
	(bignum-print-aux newbig divisor power-1 stream))
    (do ((zeros power-1 (1- zeros))
	 (base-power *print-base* (* base-power *print-base*)))
	((> base-power fix)
	 (dotimes (i zeros) (write-char #\0 stream))
	 (print-fixnum fix stream)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                          COMPLEX                             ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod PRINT-OBJECT ((x COMPLEX) s)
  (write-string "#C(" s)
  (write-toplevel (complex-realpart x) s)
  (write-char #\space s)
  (write-toplevel (complex-imagpart x) s)
  (write-char #\) s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                          SYMBOLS                             ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Don't forget to make sure symbols . .. and ... print with escapes!!!

(defun non-embeddable-symbol-char-p (c)
  (not (member (get-character-syntax c *readtable*)
	       '(:constituent
		 :non-terminating-macro
		 :single-escape
		 :multiple-escape))))
(defun escape-char-p (c)
  (member (get-character-syntax c *readtable*)
	  '(:single-escape :multiple-escape)))

;;; We don't handle empty strings.  Should we???
(defun PRINT-CASE (string stream
		   &optional (case *print-case*)
			     (read-case (readtable-case *readtable*))
			     (escape (or *print-escape*
					 *print-readably*))
		   &aux (length (length string)))
  (flet ((change (escape-predicate affected-predicate)
	   (let (capitalize (new-word t)
		 (escaped
		  (when escape
		    (or (not (find #\. string :test-not #'eql))
			(let* ((decimal (find #\. string))
			       (base (if decimal 10 *print-base*))
			       (c0 (char string 0))
			       (potnum (and (or (digit-char-p c0 base)
						(find c0 "+-^_."))
					    (not (find (char string (1- length))
						       "+-"))))
			       (alphap nil) (digitp nil))
			  (or (some
			       #'(cl:lambda (c)
					    (or (and escape-predicate
						     (funcall escape-predicate c))
						(non-embeddable-symbol-char-p c)
						(when potnum
						  (cond ((digit-char-p c base)
							 (setq alphap nil
							       digitp t))
							((alpha-char-p c)
							 (if alphap
							     (setq potnum nil)
							   (setq alphap t)))
							(t (setq alphap nil)))
						  nil)))
			       string)
			      (and potnum digitp)))))))
	     (when escaped (write-char #\| stream))
	     (dotimes (i length)
	       (let ((char (char string i)))
		 (when (eq case :capitalize)
		   (if (alphanumericp char)
		       (setf capitalize new-word new-word nil)
		     (setf capitalize nil new-word t)))
		 ;; IWBNI this checked that #\\ was a single escape
		 ;; character in the current readtble, and if not
		 ;; found, one that was.
		 (when (and escape (escape-char-p char)) (write-char #\\ stream))
		 (write-char 
		  (cond ((and affected-predicate (not escaped)
			      (funcall affected-predicate char))
			 (ecase case
			   (:upcase (char-upcase char))
			   (:downcase (char-downcase char))
			   (:capitalize
			    (cond (capitalize (setf capitalize nil)
					      (char-upcase char))
				  (t (char-downcase char))))))
			(t char))
		  stream)))
	     (when escaped (write-char #\| stream)))))
    (ecase read-case
      (:upcase (change #'lower-case-p #'upper-case-p))
      (:downcase  (change #'upper-case-p #'lower-case-p))
      (:preserve (change nil nil))
      ((:invert :unescaped-invert)
       (if (some #'lower-case-p string)
	   (if (some #'upper-case-p string)
	       (change nil nil)
	     (progn (setq case :upcase)
		    (change nil #'lower-case-p)))
	 (progn (setq case :downcase)
		(change nil #'upper-case-p)))))))



(defmethod PRINT-OBJECT ((symbol SYMBOL) stream)
  ;; Package qualifier...
  (cond ((not (or *print-escape* *print-readably*)) nil)
	;; Apparently uninterned (even if somehow accessible!)
	((null (symbol-package symbol))
	 (when *print-gensym* (write-string "#:" stream)))
	;; Accessible in current package.
	((eq symbol (find-symbol (symbol-name symbol)))
	 nil)
	((keywordp symbol)
	 (write-char #\: stream))
	(t (let ((pkg (symbol-package symbol)))
	     (print-case (package-name pkg) stream)
	     (write-char #\: stream)
	     (unless (eq :external
			 (nth-value 1 (find-symbol
				       (symbol-name symbol) pkg)))
	       (write-char #\: stream)))))
  (print-case (symbol-name symbol) stream))


#+controversial
(defmethod PRINT-OBJECT :AROUND ((f function) stream)
  (when *print-readably*
    (let ((name (nth-value 2 (function-lambda-expression f))))
      (when (and (fboundp name) (eql f (fdefinition name)))
	(return-from print-object
	  (print-eval `(fdefinition ',name)  stream f)))))
  (call-next-method))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                          STRUCTURES                          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun print-structure (struct stream)
  (funcall (formatter "~<#S(~;~W ~:I~@_~@{:~A ~W~^ ~:_~}~;)~:>")
	   stream
	   (make-structure-keylist struct)))

(defmethod PRINT-OBJECT ((struct structure-object) stream)
  (print-structure struct stream))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                        METAOBJECTS                           ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun format-method (stream method &optional
			     (name (let ((gf (method-generic-function method)))
				     (when gf (generic-function-name gf)))))
  (format stream (formatter "~s~{ ~s~} ~s")
	  name
	  (method-qualifiers method)
	  (mapcar #'type-name (method-specializers method))))

(defun print-metaobject (object stream name uniquep)
  (print-unreadable-object (object stream :identity (not uniquep) :type t)
    (write-toplevel name stream))
  object)

(defmethod PRINT-OBJECT ((object TYPE) stream)
  (print-unreadable-object (object stream :type t)
    (write-toplevel (type-name object) stream)))

(defmethod PRINT-OBJECT ((object STANDARD-SYSTEM-CLASS) stream)
  (print-metaobject object stream (class-name object) t))

(defmethod PRINT-OBJECT ((object CLASS) stream)
  (let ((name (class-name object)))
    (print-metaobject object stream name
		      (and (sealed-class-p object)
			   (eq (find-type name nil)
			       object)))))

(defmethod PRINT-OBJECT ((object GENERIC-FUNCTION) stream)
  (let ((name (generic-function-name object)))
    (print-metaobject object stream name
		      (and (generic-function-sealed-p object)
			   (eq (fdefinition name)
			       name)))))

(defmethod PRINT-OBJECT ((object SLOT-DEFINITION) stream)
  (print-metaobject object stream (slot-definition-name object) nil))

(defmethod PRINT-OBJECT ((object METHOD) stream)
  (print-unreadable-object (object stream :identity t :type t)
    (format-method stream object))
  object)

(defmethod PRINT-OBJECT ((object METHOD-COMBINATION) stream)
  (print-unreadable-object (object stream :identity t :type t)
    (format stream #"~s~{ ~s~}"
	    (class-name-of object)
	    (method-combination-options object)))
  object)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                        FLOATS                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Comes from CMUCL.
;;;  Written by Bill Maddox
;;;
;;; FLONUM-TO-STRING (and its subsidiary function FLOAT-STRING) does most of 
;;; the work for all printing of floating point numbers in the printer and in
;;; FORMAT.  It converts a floating point number to a string in a free or 
;;; fixed format with no exponent.  The interpretation of the arguments is as 
;;; follows:
;;;
;;;     X        - The floating point number to convert, which must not be
;;;                negative.
;;;     WIDTH    - The preferred field width, used to determine the number
;;;                of fraction digits to produce if the FDIGITS parameter
;;;                is unspecified or NIL.  If the non-fraction digits and the
;;;                decimal point alone exceed this width, no fraction digits
;;;                will be produced unless a non-NIL value of FDIGITS has been
;;;                specified.  Field overflow is not considerd an error at this
;;;                level.
;;;     FDIGITS  - The number of fractional digits to produce. Insignificant
;;;                trailing zeroes may be introduced as needed.  May be
;;;                unspecified or NIL, in which case as many digits as possible
;;;                are generated, subject to the constraint that there are no
;;;                trailing zeroes.
;;;     SCALE    - If this parameter is specified or non-NIL, then the number
;;;                printed is (* x (expt 10 scale)).  This scaling is exact,
;;;                and cannot lose precision.
;;;     FMIN     - This parameter, if specified or non-NIL, is the minimum
;;;                number of fraction digits which will be produced, regardless
;;;                of the value of WIDTH or FDIGITS.  This feature is used by
;;;                the ~E format directive to prevent complete loss of
;;;                significance in the printed value due to a bogus choice of
;;;                scale factor.
;;;
;;; Most of the optional arguments are for the benefit for FORMAT and are not
;;; used by the printer.
;;;
;;; Returns:
;;; (VALUES DIGIT-STRING DIGIT-LENGTH LEADING-POINT TRAILING-POINT DECPNT)
;;; where the results have the following interpretation:
;;;
;;;     DIGIT-STRING    - The decimal representation of X, with decimal point.
;;;     DIGIT-LENGTH    - The length of the string DIGIT-STRING.
;;;     LEADING-POINT   - True if the first character of DIGIT-STRING is the
;;;                       decimal point.
;;;     TRAILING-POINT  - True if the last character of DIGIT-STRING is the
;;;                       decimal point.
;;;     POINT-POS       - The position of the digit preceding the decimal
;;;                       point.  Zero indicates point before first digit.
;;;
;;; WARNING: For efficiency, there is a single string object *digit-string*
;;; which is modified destructively and returned as the value of
;;; FLONUM-TO-STRING.  Thus the returned value is not valid across multiple 
;;; calls.
;;;
;;; NOTE:  FLONUM-TO-STRING goes to a lot of trouble to guarantee accuracy.
;;; Specifically, the decimal number printed is the closest possible 
;;; approximation to the true value of the binary number to be printed from 
;;; among all decimal representations  with the same number of digits.  In
;;; free-format output, i.e. with the number of digits unconstrained, it is 
;;; guaranteed that all the information is preserved, so that a properly-
;;; rounding reader can reconstruct the original binary number, bit-for-bit, 
;;; from its printed decimal representation. Furthermore, only as many digits
;;; as necessary to satisfy this condition will be printed.
;;;
;;; FLOAT-STRING actually generates the digits for positive numbers.  The
;;; algorithm is essentially that of algorithm Dragon4 in "How to Print 
;;; Floating-Point Numbers Accurately" by Steele and White.  The current 
;;; (draft) version of this paper may be found in [CMUC]<steele>tradix.press.
;;; DO NOT EVEN THINK OF ATTEMPTING TO UNDERSTAND THIS CODE WITHOUT READING 
;;; THE PAPER!

(defvar *digits* "0123456789")

(defvar *digit-string*
  (make-array 50 :element-type 'base-char :fill-pointer 0 :adjustable t)) 

(defun flonum-to-string (x &optional width fdigits scale fmin)
  (cond ((zerop x)
	 ;;zero is a special case which float-string cannot handle
	 (if fdigits
	     (let ((s (make-string (1+ fdigits) :initial-element #\0)))
	       (setf (schar s 0) #\.)
	       (values s (length s) t (zerop fdigits) 0))
	   (values "." 1 t t 0)))
	(t
	 (setf (fill-pointer *digit-string*) 0)
	 (multiple-value-bind (sig exp)
	     (integer-decode-float x)
	   (let* ((precision (float-precision x))
		  (digits (float-digits x))
		  (fudge (- digits precision))
		  (width (if width (max width 1) nil)))
	     (float-string (ash sig (- fudge)) (+ exp fudge) precision width
			   fdigits scale fmin))))))


(defun float-string (fraction exponent precision width fdigits scale fmin)
  (let ((r fraction) (s 1) (m- 1) (m+ 1) (k 0)
	(digits 0) (decpnt 0) (cutoff nil) (roundup nil) u low high)
    ;;Represent fraction as r/s, error bounds as m+/s and m-/s.
    ;;Rational arithmetic avoids loss of precision in subsequent calculations.
    (cond ((> exponent 0)
	   (setq r (ash fraction exponent))
	   (setq m- (ash 1 exponent))	   
	   (setq m+ m-))                   
	  ((< exponent 0)
	   (setq s (ash 1 (- exponent)))))
    ;;adjust the error bounds m+ and m- for unequal gaps
    (when (= fraction (ash 1 precision))
      (setq m+ (ash m+ 1))
      (setq r (ash r 1))
      (setq s (ash s 1)))
    ;;scale value by requested amount, and update error bounds
    (when scale
      (if (minusp scale)
	  (let ((scale-factor (expt 10 (- scale))))
	    (setq s (* s scale-factor)))
	  (let ((scale-factor (expt 10 scale)))
	    (setq r (* r scale-factor))
	    (setq m+ (* m+ scale-factor))
	    (setq m- (* m- scale-factor)))))
    ;;scale r and s and compute initial k, the base 10 logarithm of r
    (do ()
        ((>= r (ceiling s 10)))
      (decf k)
      (setq r (* r 10))
      (setq m- (* m- 10))
      (setq m+ (* m+ 10)))
    (do ()(nil)
      (do ()
	  ((< (+ (ash r 1) m+) (ash s 1)))
	(setq s (* s 10))
	(incf k))
      ;;determine number of fraction digits to generate
      (cond (fdigits
	     ;;use specified number of fraction digits
	     (setq cutoff (- fdigits))
	     ;;don't allow less than fmin fraction digits
	     (if (and fmin (> cutoff (- fmin))) (setq cutoff (- fmin))))
	    (width
	     ;;use as many fraction digits as width will permit
             ;;but force at least fmin digits even if width will be exceeded
	     (if (< k 0)
		 (setq cutoff (- 1 width))
		 (setq cutoff (1+ (- k width))))
	     (if (and fmin (> cutoff (- fmin))) (setq cutoff (- fmin)))))
      ;;If we decided to cut off digit generation before precision has
      ;;been exhausted, rounding the last digit may cause a carry propagation.
      ;;We can prevent this, preserving left-to-right digit generation, with
      ;;a few magical adjustments to m- and m+.  Of course, correct rounding
      ;;is also preserved.
      (when (or fdigits width)
	(let ((a (- cutoff k))
	      (y s))
	  (if (>= a 0)
	      (dotimes (i a) (setq y (* y 10)))
	      (dotimes (i (- a)) (setq y (ceiling y 10))))
	  (setq m- (max y m-))
	  (setq m+ (max y m+))
	  (when (= m+ y) (setq roundup t))))
      (when (< (+ (ash r 1) m+) (ash s 1)) (return)))
    ;;zero-fill before fraction if no integer part
    (when (< k 0)
      (setq decpnt digits)
      (vector-push-extend #\. *digit-string*)
      (dotimes (i (- k))
	(incf digits) (vector-push-extend #\0 *digit-string*)))
    ;;generate the significant digits
    (do ()(nil)
      (decf k)
      (when (= k -1)
	(vector-push-extend #\. *digit-string*)
	(setq decpnt digits))
      (multiple-value-setq (u r) (truncate (* r 10) s))
      (setq m- (* m- 10))
      (setq m+ (* m+ 10))
      (setq low (< (ash r 1) m-))
      (if roundup
	  (setq high (>= (ash r 1) (- (ash s 1) m+)))
	  (setq high (> (ash r 1) (- (ash s 1) m+))))
      ;;stop when either precision is exhausted or we have printed as many
      ;;fraction digits as permitted
      (when (or low high (and cutoff (<= k cutoff))) (return))
      (vector-push-extend (char *digits* u) *digit-string*)
      (incf digits))
    ;;if cutoff occured before first digit, then no digits generated at all
    (when (or (not cutoff) (>= k cutoff))
      ;;last digit may need rounding
      (vector-push-extend (char *digits*
				(cond ((and low (not high)) u)
				      ((and high (not low)) (1+ u))
				      (t (if (<= (ash r 1) s) u (1+ u)))))
			  *digit-string*)
      (incf digits))
    ;;zero-fill after integer part if no fraction
    (when (>= k 0)
      (dotimes (i k) (incf digits) (vector-push-extend #\0 *digit-string*))
      (vector-push-extend #\. *digit-string*)
      (setq decpnt digits))
    ;;add trailing zeroes to pad fraction if fdigits specified
    (when fdigits
      (dotimes (i (- fdigits (- digits decpnt)))
	(incf digits)
	(vector-push-extend #\0 *digit-string*)))
    ;;all done
    (values
     *digit-string* (1+ digits) (= decpnt 0) (= decpnt digits) decpnt)))

;;; Entry point for the float printer as called by PRINT, PRIN1, PRINC,
;;; etc.  The argument is printed free-format, in either exponential or 
;;; non-exponential notation, depending on its magnitude.
;;;
;;; NOTE: When a number is to be printed in exponential format, it is scaled in
;;; floating point.  Since precision may be lost in this process, the
;;; guaranteed accuracy properties of FLONUM-TO-STRING are lost.  The
;;; difficulty is that FLONUM-TO-STRING performs extensive computations with
;;; integers of similar magnitude to that of the number being printed.  For
;;; large exponents, the bignums really get out of hand.  If bignum arithmetic
;;; becomes reasonably fast and the exponent range is not too large, then it
;;; might become attractive to handle exponential notation with the same
;;; accuracy as non-exponential notation, using the method described in the
;;; Steele and White paper.

(defun PRINT-FLOAT (x stream)
  (cond ((zerop x)
	 (write-string "0.0" stream)
	 (print-float-exponent x 0 stream))
	(t
	 (print-float1 (write-sign x stream)
		       stream (float 1/1000 x) (float 10000000.0d0 x)))))

(defun print-float1 (x stream e-min e-max)
  (if (and (>= x e-min) (< x e-max))
      ;;free format
      (multiple-value-bind (str len lpoint tpoint)
			   (flonum-to-string x)
	(declare (ignore len))
	(when lpoint (write-char #\0 stream))
	(write-string str stream)
	(when tpoint (write-char #\0 stream))
	(print-float-exponent x 0 stream))
      ;;exponential format 
      (multiple-value-bind (f ex)
			   (scale-exponent x)
	(multiple-value-bind (str len lpoint tpoint)
			     (flonum-to-string f nil nil 1)
	  (declare (ignore len))
	  (when lpoint (write-char #\0 stream))
	  (write-string str stream)
	  (when tpoint (write-char #\0 stream))
	  ;; subtract out scale factor of 1 passed to flonum-to-string
	  (print-float-exponent x (1- ex) stream)))))


;;; Print the appropriate exponent marker for X and the specified exponent.
(defun PRINT-FLOAT-EXPONENT (x exp stream)
  (declare (type float x) (type fixnum exp))
  (let ((*print-radix* nil)
	(plusp (plusp exp)))
    (if (typep x *read-default-float-format*)
	(unless (eql exp 0)
	  (format stream #"e~:[~;+~]~D" plusp exp))
	(format stream #"~C~:[~;+~]~D" 
		(etypecase x
		  (single-float #\f)
		  (double-float #\d)
		  (short-float #\s)
		  (long-float #\L))
		plusp exp))))


(defun FLOAT-DENORMALIZED-P (x)
  (< x least-positive-normalized-long-float))
;;; Given a non-negative floating point number, SCALE-EXPONENT returns a new
;;; floating point number Z in the range (0.1, 1.0] and and exponent E such
;;; that Z * 10^E is (approximately) equal to the original number.  There may
;;; be some loss of precision due the floating point representation.  The
;;; scaling is always done with long float arithmetic, which helps printing of
;;; lesser precisions as well as avoiding generic arithmetic.
;;;
;;; When computing our initial scale factor using EXPT, we pull out part of
;;; the computation to avoid over/under flow.  When denormalized, we must pull
;;; out a large factor, since there is more negative exponent range than
;;; positive range.
(defun SCALE-EXPONENT (original-x)
  (let* ((x (coerce original-x 'long-float)))
    (multiple-value-bind (sig exponent)
			 (decode-float x)
      (declare (ignore sig))
      (if (= x 0.0l0)
	  (values (float 0.0l0 original-x) 1)
	  (let* ((ex (round (* exponent (log 2l0 10))))
		 (x (if (minusp ex)
			(if (float-denormalized-p x)
			    (* x 1.0l16 (expt 10.0l0 (- (- ex) 16)))
			    (* x 10.0l0 (expt 10.0l0 (- (- ex) 1))))
			(/ x 10.0l0 (expt 10.0l0 (1- ex))))))
	    (do ((d 10.0l0 (* d 10.0l0))
		 (y x (/ x d))
		 (ex ex (1+ ex)))
		((< y 1.0l0)
		 (do ((m 10.0l0 (* m 10.0l0))
		      (z y (* y m))
		      (ex ex (1- ex)))
		     ((>= z 0.1l0)
		      (values (float z original-x) ex))))))))))


(defmethod PRINT-OBJECT ((f FLOAT) s) (print-float f s))