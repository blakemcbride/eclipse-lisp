;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CONVERSIONS TO C PRIMITIVES

(declaim
 (inline eclipse:double-float-double eclipse::single-float-double
	 eclipse::integer-double eclipse::ratio-double
	 eclipse::double-float-float eclipse:single-float-float
	 eclipse::integer-float eclipse::ratio-float
	 eclipse:ratio-numerator eclipse:ratio-denominator
	 eclipse::complex-imagpart eclipse::complex-realpart
 ))

;;; To ec:double: 
(defun eclipse:double-float-double (x) (the double-float x))
(defun eclipse::single-float-double (x) (float (the single-float x) 1.0d0))
(defun eclipse::integer-double (x)
  (if (host::fixnump x)
      (float (the integer x) 1.0d0)
      (eclipse::double-float-double (eclipse::xint-double-float (eclipse::integer-xint x)))))
(defun eclipse::ratio-double (x) (declare (type ratio x))
  (/ (eclipse::integer-double (numerator x))
     (eclipse::integer-double (denominator x))))

;;; To ec:single:
(defun eclipse:single-float-float (x) (the single-float x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CONVERSIONS FROM C PRIMITIVES
(defun eclipse:double-double-float (x) (the ec:double x))
(defun eclipse:float-single-float (x) (the ec:float x))
(defun eclipse::float-double-float (x) (float (the ec:float x) 1.0d0))
(defun eclipse:double-single-float (x) (float (the ec:double x) 1.0f0))
;;; Needs to make bignum if necessary!!!
(defun eclipse:int-integer (x) (the ec:int x))
(defun eclipse:int-fixnum (x) (the ec:int x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
;;; OTHER CONSTRUCTORS AND ACCESSORS

(defun eclipse:make-ratio (x y) (declare (type integer x y))
  (when (minusp y) (error "Bad arguments ~d ~d to make-ratio." x y))
  (/ x y))
(defun eclipse:ratio-numerator (x) (numerator (the ratio x)))
(defun eclipse:ratio-denominator (x) (denominator (the ratio x)))

(defun eclipse:make-complex (x y) (declare (type real x y)) (complex x y))
(defun eclipse:complex-realpart (x) (realpart (the complex x)))
(defun eclipse:complex-imagpart (x) (imagpart (the complex x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
;;; ARITHMETIC

;;; On most machines, we can use % on fixnums.
(defun eclipse::rem-fixnum (x y) (declare (type fixnum x y)) (rem x y))

#+(and cmu hppa)			;bad hp cmucl bug!
(defun eclipse::truncate-fixnum (x y)
  (declare (type fixnum x y))
  (values
   (if (and (minusp x) (minusp y))
       (truncate (- x) (- y))
       (truncate x y))))

#-(and cmu hppa)
(defun eclipse::truncate-fixnum (x y)
  (declare (type fixnum x y))
  (values (truncate x y)))

(defmacro defop2 (name op t1 t2 type)
  (let ((name (eclipse::make-name-in-package :eclipse "~a-~a-~a" name t1 t2)))
    `(progn (declaim (ftype (function (,t1 ,t2) ,type) ,name))
	    (defun ,name (x y) (values (,op (the ,t1 x) (the ,t2 y)))))))

(defmacro defop1 (name op t1 type)
  (let ((name (eclipse::make-name-in-package :eclipse "~a-~a" name t1)))
    `(progn (declaim (ftype (function (,t1) ,type) ,name))
	    (defun ,name (x) (values (,op (the ,t1 x)))))))

(defmacro defprim2 (name op types1 types2 &optional type)
  `(progn
     ,@(loop for type1 in types1
	     append
	     (loop for type2 in types2
		   collect
		   `(defop2 ,name ,op ,type1 ,type2
		      ,(cond ((eq type 'boolean) 'boolean)
			     ((or (eq type1 'double-float)
				  (eq type2 'double-float))
			      'double-float)
			     ((or (eq type1 'single-float)
				  (eq type2 'single-float)
				  (eq type 'float))
			      'single-float)
			     (t 'integer)))))))

(macrolet
    ((def-int (name op)
       `(defun ,(eclipse::make-name-in-package :eclipse "~a-INTEGER-INTEGER" name) (x y)
	  (declare (integer x y))
	  (if (and (host::fixnump x) (host::fixnump y))
	      (,op x y)
	      (eclipse::xint-integer
	       (,(eclipse::make-name-in-package :eclipse "~a-XINT" name)
		(eclipse::integer-xint x) (eclipse::integer-xint y)))))))
  (def-int add +)
  (def-int subt -)
  (def-int mult *))

(defprim2 add + (single-float double-float) (single-float double-float))
(defprim2 subt - (single-float double-float) (single-float double-float))
(defprim2 mult * (single-float double-float) (single-float double-float))
(defprim2 div / (single-float double-float) (single-float double-float))

(defprim2 add + (integer ratio) (single-float double-float))
(defprim2 subt - (integer ratio) (single-float double-float))
(defprim2 mult * (integer ratio) (single-float double-float))
(defprim2 div / (integer ratio) (single-float double-float))
(defprim2 add + (single-float double-float) (integer ratio))
(defprim2 subt - (single-float double-float) (integer ratio))
(defprim2 mult * (single-float double-float) (integer ratio))
(defprim2 div / (single-float double-float) (integer ratio))

(defprim2 eq = (integer single-float double-float) (integer single-float double-float) boolean)
(defprim2 gt > (integer single-float double-float) (integer single-float double-float) boolean)
(defprim2 lt < (integer single-float double-float) (integer single-float double-float) boolean)
(defprim2 ge >= (integer single-float double-float) (integer single-float double-float) boolean)
(defprim2 le <= (integer single-float double-float) (integer single-float double-float) boolean)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
;;; EC:DOUBLE FUNCTIONS

(defmacro defprim1 (name &optional (op name) no-rational)
  `(progn
     ,@(unless no-rational
	 `((defop1 ,name ,op integer single-float)
	   (defop1 ,name ,op ratio single-float)))
     (defop1 ,name ,op single-float single-float)
     (defop1 ,name ,op double-float double-float)))

;;; Note: exp-integer(x) should signal a type-error if result is too
;;; large to fit in a float. (eg. x is a bignum)

;;; Note: ec:rint might be modified by hardware switches controlled by
;;; other processes.  Better role our own.

(defprim1 exp)				;ec:exp
(defprim1 ftruncate ftruncate t)	;ec:aint
(defprim1 ffloor ffloor t)		;ec:floor
(defprim1 fceiling fceiling t)		;ec:ceil
(defprim1 fround fround t)		;ec:rint (If possible, see above.)
(defprim1 sqrt1 sqrt)			;ec:sqrt
(defprim1 log)				;ec:log
(defprim1 sin)				;ec:sin
(defprim1 asin1 asin)			;ec:asin
(defprim1 sinh)				;ec:sinh
(defprim1 asinh)			;ec:asinh
(defprim1 cos)				;ec:cos
(defprim1 acos1 acos)			;ec:acos
(defprim1 cosh)				;ec:cosh
(defprim1 acosh1 acosh)			;ec:acosh
(defprim1 tan)				;ec:tan
(defprim1 atan1 atan)			;ec:atan
(defprim1 tanh)				;ec:tanh
(defprim1 atanh1 atanh)			;ec:atanh

(defprim2 expt2 expt (integer ratio single-float double-float) (ratio single-float double-float)
  float)				;ec:pow
(defprim2 atan2 atan (integer ratio single-float double-float)
  (integer ratio single-float double-float) float) ;ec:atan2
(defprim2 float2 float (integer ratio single-float double-float) (single-float double-float))
;;; ec:scalbn,  or if necessary ec:ldexp
(defop2 scale-float scale-float single-float integer single-float)
(defop2 scale-float scale-float double-float integer double-float)

(defun ec::copysign (x y) (declare (type ec:double x y))
  (if (eclipse::xor (minusp x) (minusp y))
      (- x) x))

(defmacro def-double-float-primitive2 (name ((x t1) (y t2)) val)
  (flet ((doubler (type) (case type
			   (double-float 'eclipse::double-float-double)
			   (single-float 'eclipse::single-float-double)
			   (ratio 'eclipse::ratio-double)
			   (integer 'eclipse::integer-double))))
    `(defun ,(eclipse::make-name-in-package :eclipse "~a-~a-~a" name t1 t2) (x y)
       (let ((,x (,(doubler t1) x))
	     (,y (,(doubler t2) y)))
	 (,(if (or (eq t1 'double-float) (eq t2 'double-float))
	       'eclipse:double-double-float
	       'eclipse:double-single-float) ,val)))))

(macrolet ((def-primitive2 (name val)
	     `(progn
		(def-double-float-primitive2 ,name
		       ((x single-float) (y single-float))
		       ,val)
		(def-double-float-primitive2 ,name
		       ((x single-float) (y double-float))
		       ,val)
		(def-double-float-primitive2 ,name
		       ((x double-float) (y single-float))
		       ,val)
		(def-double-float-primitive2 ,name
		       ((x double-float) (y double-float))
		       ,val))))
  (def-primitive2 float-sign2 (ec::copysign y x)))

;;; Argument must be an "integral" float.
(declaim (ftype (function (single-float) integer)
		eclipse::truncate-single-float)
	 (ftype (function (double-float) integer)
		eclipse::truncate-double-float))
(defun eclipse::truncate-single-float (x)
  (if (<= most-negative-fixnum x most-positive-fixnum)
      (values (truncate (the single-float x)))
      (eclipse::xint-integer
       (eclipse::double-float-xint (float (the single-float x) 1.0d0)))))
  
(defun eclipse::truncate-double-float (x)
  (if (<= most-negative-fixnum x most-positive-fixnum)
      (values (truncate (the double-float x)))
      (eclipse::xint-integer
       (eclipse::double-float-xint (the double-float x)))))

;;; Use ec:frexp, ec:fabs, and ec:signbit!
;;; Don't forget to return three values!
(declaim (ftype (function (single-float)
			  (values single-float integer single-float))
		eclipse::decode-single-float)
	 (ftype (function (double-float)
			  (values double-float integer double-float))
		eclipse::decode-double-float))
(defun eclipse::decode-double-float (float)
  (decode-float (the double-float float)))
(defun eclipse::decode-single-float (float)
  (decode-float (the single-float float)))
