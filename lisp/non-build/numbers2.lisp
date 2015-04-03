;;; 12.5.2 TRIGONOMETRIC AND RELATED FUNCTIONS
(defmethod ABS ((x COMPLEX))
  (with-complex (r i x)
		(sqrt (add (mult r r) (mult i i)))))

(defmethod PHASE ((number COMPLEX))
  (atan (imagpart number) (realpart number)))
(defmethod PHASE ((number DOUBLE-FLOAT))
  (if (minusp number) pi 0.0d0))
(defmethod PHASE ((number REAL))
  (if (minusp number) single-pi 0.0f0))

(macrolet ((def-signum (type negative positive)
	     `(defmethod SIGNUM ((x ,type))
		(cond ((zerop x) x)
		      ((minusp x) ,negative)
		      (t ,positive)))))
  (def-signum INTEGER -1 1)
  (def-signum RATIO -1 1)
  (def-signum SINGLE-FLOAT -1.0f0 1.0f0)
  (def-signum DOUBLE-FLOAT -1.0d0 1.0d0))
(defmethod SIGNUM ((x COMPLEX))
  (if (zerop x) x (/ x (abs x))))
  
(defun CIS (radians) (complex (cos radians) (sin radians)))

(macrolet ((def-trig (name)
	     `(progn
		(defmethod-math1 ,name (INTEGER))
		(defmethod-math1 ,name (RATIO))
		(defmethod-math1 ,name (SINGLE-FLOAT))
		(defmethod-math1 ,name (DOUBLE-FLOAT))
		(defmethod ,name ((x COMPLEX))
		  (,(make-name "~a-COMPLEX" name) x)))))
  (def-trig SIN)
  (def-trig COS)
  (def-trig TAN)
  (def-trig SINH)
  (def-trig COSH)
  (def-trig TANH)
  (def-trig atan1)
  (def-trig ASINH))


(macrolet ((def-trig1 (name type)
	     `(defmethod ,name ((x ,type))
		(if (<= -1 x 1)
		    (,(make-name "~a1-~a" name type) x)
		    (,(make-name "~a-COMPLEX" name) x))))
	   (def-trig (name)
	     `(progn
		(def-trig1 ,name INTEGER)
		(def-trig1 ,name RATIO)
		(def-trig1 ,name SINGLE-FLOAT)
		(def-trig1 ,name DOUBLE-FLOAT)
		(defmethod ,name ((x COMPLEX))
		  (,(make-name "~a-COMPLEX" name) x)))))
  (def-trig ASIN)
  (def-trig ACOS)
  (def-trig ATANH))

(macrolet ((def-trig1 (name type)
	     `(defmethod ,name ((x ,type))
		(if (>= x 1)
		    (,(make-name "~a1-~a" name type) x)
		    (,(make-name "~a-COMPLEX" name) x))))
	   (def-trig (name)
	     `(progn
		(def-trig1 ,name INTEGER)
		(def-trig1 ,name RATIO)
		(def-trig1 ,name SINGLE-FLOAT)
		(def-trig1 ,name DOUBLE-FLOAT)
		(defmethod ,name ((x COMPLEX))
		  (,(make-name "~a-COMPLEX" name) x)))))
  (def-trig ACOSH))


(defmethod-math2 atan2 (SINGLE-FLOAT SINGLE-FLOAT))
(defmethod-math2 atan2 (DOUBLE-FLOAT DOUBLE-FLOAT))
(defmethod-math2 atan2 (INTEGER INTEGER))
(defmethod-math2 atan2 (RATIO RATIO))
(defmethod-math2 atan2 (SINGLE-FLOAT DOUBLE-FLOAT))
(defmethod-math2 atan2 (SINGLE-FLOAT RATIO))
(defmethod-math2 atan2 (SINGLE-FLOAT INTEGER))
(defmethod-math2 atan2 (DOUBLE-FLOAT SINGLE-FLOAT))
(defmethod-math2 atan2 (DOUBLE-FLOAT RATIO))
(defmethod-math2 atan2 (DOUBLE-FLOAT INTEGER))
(defmethod-math2 atan2 (RATIO SINGLE-FLOAT))
(defmethod-math2 atan2 (RATIO DOUBLE-FLOAT))
(defmethod-math2 atan2 (RATIO INTEGER))
(defmethod-math2 atan2 (INTEGER RATIO))
(defmethod-math2 atan2 (INTEGER SINGLE-FLOAT))
(defmethod-math2 atan2 (INTEGER DOUBLE-FLOAT))

(defun ATAN (y &optional (x nil xp))
  (if xp (atan2 y x) (atan1 y)))

#+not-yet
(define-compiler-macro atan (y &optional (x nil xp))
  (if xp `(atan2 ,y ,x) `(atan1 ,y)))
      
(defun sin-complex (z)
  (with-complex (r i z real)
    (complex (* (sin r) (cosh i)) (* (cos r) (sinh i)))))
(defun cos-complex (z)
  (with-complex (r i z real)
    (complex (* (cos r) (cosh i)) (- (* (sin r) (sinh i))))))
(defun tan-complex (z) (/ (sin z) (cos z)))
(defun i* (z) (complex (imagpart z) (- (realpart z))))
(defun zroot (z) (sqrt (- 1 (* z z))))
(defun asin-complex (z) (- (i* (log (+ (i* z) (zroot z))))))
(defun acos-complex (z) (- (i* (log (+ z (i* (zroot z)))))))
(defun atan1-complex (z)
  (with-complex (r i z real)
    (/ (- (log (complex (- 1 i) r))
	  (log (complex (+ 1 i) (- r))))
       #c(0 2))))
(defun sinh-complex (z) (/ (- (exp z) (exp (- z))) 2))
(defun cosh-complex (z) (/ (+ (exp z) (exp (- z))) 2))
(defun tanh-complex (z) (let ((e (exp z))
			      (e-minus (exp (- z))))
			  (/ (- e e-minus)
			     (+ e e-minus))))

(defun asinh-complex (z) (log (+ z (sqrt (1+ (* z z))))))
(defun acosh-complex (z)
  (* 2 (log (+ (sqrt (/ (1+ z) 2))
	       (sqrt (/ (1- z) 2))))))
(defun atanh-complex (z) (/ (- (log (+ 1 z))
			       (log (- 1 z)))
			    2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 12.6 TYPE CONVERSIONS AND COMPONENT EXTRACTIONS ON NUMBERS   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rational1 (number)
  (etypecase number
    (rational number)
    (float (multiple-value-bind (signif expon sign)
	       (integer-decode-float number)
	     (let ((s (* signif sign)))
	       (declare (integer s))
	       (if (plusp expon)
		   (* s (ash 1 expon))
		 (make-ratio s (ash 1 (- expon)))))))))

(defun RATIONAL (number)
  (let ((n (rational1 number)))
    (if (ratiop n)
	(/ (numerator n) (denominator n))
      n)))

;;; From CMUCL:
;;; Thanks to Kim Fateman, who stole this function rationalize-float
;;; from macsyma's rational. Macsyma'a rationalize was written by the
;;; legendary Gosper (rwg). Gosper is now working for Symbolics.  Guy
;;; Steele said about Gosper, "He has been called the only living 17th
;;; century mathematician and is also the best pdp-10 hacker I know."
;;; So, if you can understand or debug this code you win big.
(defun RATIONALIZE (number)
  (macrolet
      ((frob (x eps type)
	 `(let ((y ())
		(a ()))
	    (do ((xx ,x (setq y (/ (float 1.0 ,x) (- xx (float a ,x)))))
		 (num (setq a (truncate ,x))
		      (+ (* (setq a (truncate y)) num) onum))
		 (den 1 (+ (* a den) oden))
		 (onum 1 num)
		 (oden 0 den))
		((and (not (zerop den))
		      (not (> (abs (/ (- ,x (/ (float num ,x)
					       (float den ,x)))
				      ,x))
			      ,eps)))
		 (make-ratio num den))
	      (declare (type ,type number))))))
    (cond ((rationalp number) number)
	  ((zerop number) 0)
	  ((minusp number) (- (rationalize (- number))))
	  ((single-float-p number) (frob number single-float-epsilon single-float))
	  ((double-float-p number) (frob number double-float-epsilon double-float)))))


(defun MOD (number divisor)
  (let ((rem (rem number divisor)))
    (if (and (not (zerop rem))
	     (if (minusp divisor)
		 (plusp number)
		 (minusp number)))
	(+ rem divisor)
      rem)))

;;; On all Unix implementations we've seen, the float-radix for all
;;; floating point numbers is always 2.  This can be confirmed on
;;; SVID-3 compliant systems by looking at FLT_RADIX in <float.h>, or
;;; _EXPBASE in <values.h>.
(defmethod FLOAT-RADIX ((float FLOAT)) 2)

(defmethod FLOAT-DIGITS ((float SINGLE-FLOAT)) float-signif)
(defmethod FLOAT-DIGITS ((float DOUBLE-FLOAT)) double-signif)

(macrolet ((def-scale (type precision)
	     `(defmethod FLOAT-PRECISION ((float ,type))
		(if (= float 0.0f0) 0
		    ;; Should deal with denormalized numbers here!!!
		    ,precision))))
  (def-scale SINGLE-FLOAT float-signif)
  (def-scale DOUBLE-FLOAT double-signif))

;;; It turns out that on all implementations, the largest exponent is
;;; representable by a fixnum. (i.e. the second value returned by
;;; (decode-float most-positive-double-float) or second argument to
;;; scale-float).
(defmethod-math2 SCALE-FLOAT (SINGLE-FLOAT INTEGER))
(defmethod-math2 SCALE-FLOAT (DOUBLE-FLOAT INTEGER))

(macrolet ((def-scale (type)
	     `(defmethod DECODE-FLOAT ((float ,type))
		(,(make-name "DECODE-~a" type) float))))
  (def-scale SINGLE-FLOAT)
  (def-scale DOUBLE-FLOAT))
  

(defun INTEGER-DECODE-FLOAT (float)
  (let ((digits (float-digits float)))
    (multiple-value-bind (signif expt sign)
	(decode-float float)
      (values (truncate (scale-float signif digits))
	      (- expt digits)
	      (if (minusp sign) -1 1)))))

(defmethod-math2 float-sign2 (SINGLE-FLOAT SINGLE-FLOAT))
(defmethod-math2 float-sign2 (SINGLE-FLOAT DOUBLE-FLOAT))
(defmethod-math2 float-sign2 (DOUBLE-FLOAT SINGLE-FLOAT))
(defmethod-math2 float-sign2 (DOUBLE-FLOAT DOUBLE-FLOAT))

(defun FLOAT-SIGN (float1 &optional (float2 (float 1 float1)))
  (float-sign2 float1 float2))

#+not-yet
(define-compiler-macro float-sign (float1 &optional (float2 `(float 1 ,float1)))
  `(float2 ,float1 ,float2))

(defun COMPLEX (realpart &optional (imagpart 0))
  (cond ((and (rationalp realpart) (eql imagpart 0))
	 realpart)
	((or (typep realpart 'double-float)
	     (typep imagpart 'double-float))
	 (make-complex (float realpart 1.0d0)
		       (float imagpart 1.0d0)))
	((or (typep realpart 'single-float)
	     (typep imagpart 'single-float))
	 (make-complex (float realpart 1.0f0)
		       (float imagpart 1.0f0)))
	((not (typep realpart 'rational))
	 (error 'type-error :datum realpart :expected-type 'real))
	((not (typep imagpart 'rational))
	 (error 'type-error :datum imagpart :expected-type 'real))
	(t (make-complex realpart imagpart))))

(defmethod REALPART ((number REAL)) number)
(defmethod REALPART ((number COMPLEX)) (complex-realpart number))

(defmethod IMAGPART ((number REAL)) (* 0 number))
(defmethod IMAGPART ((number COMPLEX)) (complex-imagpart number))

