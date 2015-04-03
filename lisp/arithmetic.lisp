;;; Potential optimization: There are places where we perform a
;;; computation involving a series of generic operations on numbers
;;; known to be integers.  It would be a good idea to test whether the
;;; numbers are fixnums at the highest level and then use a C version
;;; of the computation.  Examples: div, truncate and friends; rational
;;; arithmetic.

;;; These four should really be defined to use the c globals FLT_MAX,
;;; DBL_MAX, FLT_MIN, DBL_MIN, repsectively, which are defined in
;;; <float.h>!!!  This would require the compiler to be able to:
;;;   - reference a c global "variable" directly (i.e. without refering to
;;;     its symbol-value),
;;;   - Figure out that a C global variable that is really a macro
;;;     shold not introduce an extern declaration, but should
;;;     introduce an #include.
;;; In addition, it may be necessary to refer to FLT_XXX as ((double)
;;; FLT_XXX) in order to create the correct Lisp value.

(defconstant MOST-POSITIVE-SINGLE-FLOAT
    #.(cl:scale-float (cl:float float-mantissa 1.0f0) (cl:- float-exponent float-signif)))
(defconstant MOST-POSITIVE-DOUBLE-FLOAT
    #.(cl:scale-float (cl:float double-mantissa 1.0d0) (cl:- double-exponent double-signif)))
(defconstant LEAST-POSITIVE-NORMALIZED-SINGLE-FLOAT
    #.(cl:scale-float 2.0f0 (cl:- 1 float-exponent)))
(defconstant LEAST-POSITIVE-NORMALIZED-DOUBLE-FLOAT
    #.(cl:scale-float 2.0d0 (cl:- 1 double-exponent)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                   GENERIC ARITHMETIC                         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FLOAT
;;; When one arg is a FLOAT and the other a REAL, we promote the real
;;; arg to a float and call the primitive.
(macrolet
    ((def-math (name)
       `(progn
	  (defmethod-math2 ,name (SINGLE-FLOAT DOUBLE-FLOAT))
	  (defmethod-math2 ,name (SINGLE-FLOAT SINGLE-FLOAT))
	  (defmethod-math2 ,name (SINGLE-FLOAT RATIO))
	  (defmethod-math2 ,name (SINGLE-FLOAT INTEGER))

	  (defmethod-math2 ,name (DOUBLE-FLOAT DOUBLE-FLOAT))
	  (defmethod-math2 ,name (DOUBLE-FLOAT SINGLE-FLOAT))
	  (defmethod-math2 ,name (DOUBLE-FLOAT RATIO))
	  (defmethod-math2 ,name (DOUBLE-FLOAT INTEGER))

	  (defmethod-math2 ,name (RATIO SINGLE-FLOAT))
	  (defmethod-math2 ,name (RATIO DOUBLE-FLOAT))
	  (defmethod-math2 ,name (INTEGER SINGLE-FLOAT))
	  (defmethod-math2 ,name (INTEGER DOUBLE-FLOAT)))))
  (def-math add)
  (def-math subt)
  (def-math mult)
  (def-math div))


;;; When both args are float, we promote the weaker type one and call
;;; the primitive.
(macrolet
    ((def-math (name &optional (op name))
       `(progn
	  (defmethod-math2 ,name (SINGLE-FLOAT SINGLE-FLOAT) ,op)
	  (defmethod-math2 ,name (SINGLE-FLOAT DOUBLE-FLOAT) ,op)
	  (defmethod-math2 ,name (DOUBLE-FLOAT SINGLE-FLOAT) ,op)
	  (defmethod-math2 ,name (DOUBLE-FLOAT DOUBLE-FLOAT) ,op))))
  (def-math eq-number eq)
  (def-math lt)
  (def-math gt)
  (def-math le)
  (def-math ge))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; INTEGER
;;; Both arguments must be integers.  Call the primitive.
(macrolet ((def-math (name &optional (op name))
	     `(defmethod-math2 ,name (INTEGER INTEGER) ,op)))
  (def-math add)
  (def-math subt)
  (def-math mult)

  (def-math eq-number eq)
  (def-math lt)
  (def-math gt)
  (def-math le)
  (def-math ge))
  
(defmethod div ((x INTEGER) (y INTEGER))
  (if (and (fixnump x) (fixnump y))
      (let* ((gcd-abs (gcd x y))
	     (gcd (if (minusp y) (- gcd-abs) gcd-abs))
	     (n (truncate-fixnum x gcd)))
	(if (= gcd y) n
	    (make-ratio n (truncate-fixnum y gcd))))
      (if (= x 0) 0
	  (div-xint (integer-xint x) (integer-xint y)))))

;;; Integers are compared with floats by converting the integer to a float.
(macrolet
    ((def-math (name &optional (op name))
       `(progn (defmethod-math2 ,name (INTEGER SINGLE-FLOAT) ,op)
	       (defmethod-math2 ,name (SINGLE-FLOAT INTEGER) ,op)
	       (defmethod-math2 ,name (INTEGER DOUBLE-FLOAT) ,op)
	       (defmethod-math2 ,name (DOUBLE-FLOAT INTEGER) ,op))))
  (def-math eq-number eq)
  (def-math gt)
  (def-math lt)
  (def-math ge)
  (def-math le))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; RATIONAL
;;; For two rationals, get the numerator and denominator of both and
;;; do the generic arithmetic.
(macrolet
    ((def-math (name)
       `(defmethod ,name ((x RATIONAL) (y RATIONAL))
	  (with-ratios (nx dx x) (ny dy y)
		       (div (,name (mult nx dy)
				   (mult dx ny))
			    (mult dx dy))))))
  (def-math add)
  (def-math subt))

(defmethod mult ((x RATIONAL) (y RATIONAL))
  (with-ratios (nx dx x) (ny dy y)
	       (div (mult nx ny)
		    (mult dx dy))))

(defmethod div ((x RATIONAL) (y RATIONAL))
  (with-ratios (nx dx x) (ny dy y)
	       (div (mult nx dy)
		    (mult dx ny))))

(macrolet
    ((def-math (name op)
       `(defmethod ,name ((x RATIONAL) (y RATIONAL))
	  (with-ratios (nx dx x) (ny dy y)
		       (if (eq-integer-integer dx dy)
			   (,op nx ny)
			   (,op (mult nx dy)
				(mult ny dx)))))))
  (def-math eq-number eq-integer-integer)
  (def-math gt gt-integer-integer)
  (def-math lt lt-integer-integer)
  (def-math ge ge-integer-integer)
  (def-math le le-integer-integer))

;;; Ratios are compared with floats by converting the float to a
;;; rational.
(macrolet
    ((def-math1 (name type1 type2)
       `(defmethod ,name ((x ,type1) (y ,type2))
	  (,name ,(if (eq type1 'ratio) 'x '(rational x))
		 ,(if (eq type2 'ratio) 'y '(rational y)))))
     (def-math (name)
       `(progn (def-math1 ,name RATIO FLOAT)
	       (def-math1 ,name FLOAT RATIO))))
  (def-math eq-number)
  (def-math gt)
  (def-math lt)
  (def-math ge)
  (def-math le))
       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 12.2 PREDICATES ON NUMBERS                                   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ZEROP (number)
  (if (complexp number)
      (and (eq-number (realpart number) 0)
	   (eq-number (imagpart number) 0))
    (eq-number number 0)))
(defun PLUSP (number) (gt number 0))
(defun MINUSP (number) (lt number 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 12.3 COMPARISONS ON NUMBERS                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(macrolet ((def-comp (op op2)
	       `(defun ,op (number &rest more-numbers)
		  (declare (dynamic-extent more-numbers))
		  (dolist (next more-numbers t)
		    (if (,op2 number next)
			(setq number next)
		      (return nil))))))
  (def-comp = eq-number)
  (def-comp < lt)
  (def-comp <= le)
  (def-comp > gt)
  (def-comp >= ge))

(defun /= (number &rest more-numbers)
  (declare (dynamic-extent more-numbers))
  (do* ((head number (car nlist))
	(nlist more-numbers (cdr nlist)))
       ((atom nlist) t)
     (declare (type list nlist))
     (unless (do* ((nl nlist (cdr nl)))
		  ((atom nl) t)
	       (declare (type list nl))
	       (when (eq-number head (car nl)) (return nil)))
       (return nil))))

;;; IWBNI we specialized the two argument case.
(defun MAX (number &rest more-numbers)
  (declare (dynamic-extent more-numbers))
  (dolist (other more-numbers number)
    (when (gt other number)
      (setq number other))))

(defun MIN (number &rest more-numbers)
  (declare (dynamic-extent more-numbers))
  (dolist (other more-numbers number)
    (when (lt other number)
      (setq number other))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 12.3 ARITHMETIC OPERATIONS                                   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(macrolet ((def-math (op op2 init)
	     `(defun ,op (&optional (number ,init) &rest numbers)
		(declare (dynamic-extent numbers))
		(if numbers
		    (dolist (num numbers number)
		      (setq number (,op2 number num)))
		    number))))
  (def-math + add 0)
  (def-math * mult 1))

(macrolet ((def-math (op op2 init)
	       `(defun ,op (number &rest more-numbers)
		  (declare (dynamic-extent more-numbers))
		  (if more-numbers
		      (dolist (subtrahend more-numbers number)
			(setf number (,op2 number subtrahend)))
		      (,op2 ,init number)))))
  (def-math - subt 0)
  (def-math / div 1))

(defun 1+ (number) (add number 1))
(defun 1- (number) (subt number 1))

(defun gcd2 (u v)
  (do ()
      ((<= u 0) v)
    (when (< u v) (rotatef u v))
    (setf u (rem u v))))

(defun GCD (&optional number &rest numbers)
  (declare (dynamic-extent numbers))
  (do ((a (if number (abs number) 0)
	  (gcd2 a (abs (car b-list))))
       (b-list numbers (cdr b-list)))
      ((null b-list) a)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 12.5 IRRATIONAL AND TRANSCENDENTAL FUNCTIONS                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 12.5.2 TRIGONOMETRIC AND RELATED FUNCTIONS
(defmethod ABS ((x REAL))
  (if (minusp x) (subt 0 x) x))


