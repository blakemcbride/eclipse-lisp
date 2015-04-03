;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 12.6 TYPE CONVERSIONS AND COMPONENT EXTRACTIONS ON NUMBERS   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod float2 ((number DOUBLE-FLOAT) (other DOUBLE-FLOAT))
  number)
(defmethod float2 ((number SINGLE-FLOAT) (other SINGLE-FLOAT))
  number)
(defmethod-math2 float2 (SINGLE-FLOAT DOUBLE-FLOAT))
(defmethod-math2 float2 (RATIO DOUBLE-FLOAT))
(defmethod-math2 float2 (INTEGER DOUBLE-FLOAT))
(defmethod-math2 float2 (DOUBLE-FLOAT SINGLE-FLOAT))
(defmethod-math2 float2 (RATIO SINGLE-FLOAT))
(defmethod-math2 float2 (INTEGER SINGLE-FLOAT))

;;; This default method is not required by the specification of FLOAT,
;;; but is required by the specification of COERCE.
(defmethod float2 ((number t) (other t))
  (error 'type-error :datum number :expected-type 'real))

(defun FLOAT (number &optional (other (if (double-float-p number)
					  1.0d0 1.0f0)))
  (float2 number other))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; INTEGER TRUNCATION

(macrolet ((def-int1 (name)
	     `(defmethod ,(make-name "~a1" name) ((number INTEGER))
		(values number 0))))
  (def-int1 truncate)
  (def-int1 floor)
  (def-int1 ceiling)
  (def-int1 round))
(macrolet ((def-int1 (name)
	     `(defmethod ,(make-name "~a1" name) ((number INTEGER))
		(values (float number) 0))))
  (def-int1 ftruncate)
  (def-int1 ffloor)
  (def-int1 fceiling)
  (def-int1 fround))

(defmethod truncate2 ((number INTEGER) (divisor INTEGER))
  (if (and (fixnump number) (fixnump divisor))
      (values (truncate-fixnum number divisor)
	      (rem-fixnum number divisor))
      (multiple-value-bind (q r)
	  (trunc-xint (integer-xint number) (integer-xint divisor))
	(values (xint-integer q) (xint-integer r)))))

(defmethod ceiling2 ((number INTEGER) (divisor INTEGER))
  (multiple-value-bind (tru rem) (truncate2 number divisor)
    (if (and (not (zerop rem))
	     (if (minusp divisor)
		 (minusp number)
		 (plusp number)))
	(values (+ tru 1) (- rem divisor))
	(values tru rem))))

(defmethod floor2 ((number INTEGER) (divisor INTEGER))
  (multiple-value-bind (tru rem) (truncate2 number divisor)
    (if (and (not (zerop rem))
	     (if (minusp divisor)
		 (plusp number)
		 (minusp number)))
	(values (1- tru) (+ rem divisor))
	(values tru rem))))

(defmethod round2 ((number INTEGER) (divisor INTEGER))
  (multiple-value-bind (tru rem) (truncate2 number divisor)
    (let ((thresh (make-ratio (abs divisor) 2)))
      (cond ((or (> rem thresh)
		 (and (= rem thresh) (oddp tru)))
	     (if (minusp divisor)
		 (values (- tru 1) (+ rem divisor))
		 (values (+ tru 1) (- rem divisor))))
	    ((let ((minus-thresh (- thresh)))
	       (or (< rem minus-thresh)
		   (and (= rem minus-thresh) (oddp tru))))
	     (if (minusp divisor)
		 (values (+ tru 1) (- rem divisor))
		 (values (- tru 1) (+ rem divisor))))
	    (t (values tru rem))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NON-INTEGRAL TRUNCATION ...

;;; N.B.: Rational truncation must be used before converting to float,
;;; rather than vice versa, because it may be that numerator and
;;; divisor are both too big for single-float, but the quotient is not.
(macrolet ((def-fint2 (name iname)
	     `(defmethod ,name ((number RATIONAL) (divisor RATIONAL))
		(multiple-value-bind (tru rem) (,iname number divisor)
		    (values (float tru) rem)))))
  (def-fint2 ftruncate2 truncate2)
  (def-fint2 fceiling2 ceiling2)
  (def-fint2 ffloor2 floor2)
  (def-fint2 fround2 round2))

(macrolet ((def-fint1 (name iname)
	     `(defmethod ,name ((number RATIO))
		(let ((q (with-ratio (n d number)
				     (,iname n d))))
		  (values (float q) (- number q))))))
  (def-fint1 ftruncate1 truncate2)
  (def-fint1 fceiling1 ceiling2)
  (def-fint1 ffloor1 floor2)
  (def-fint1 fround1 round2))

(macrolet
    ((def-trunc1 (name type val)
       `(defmethod ,name ((number ,type))
	  (let ((q ,val)) (values q (- number q)))))
     (def-trunc2 (name type1 type2 val)
       `(defmethod ,name ((number ,type1) (divisor ,type2))
	  (let ((q ,val)) (values q (- number (* q divisor)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; RATIONAL TRUNCATION
  
  (macrolet ((def-irat1 (name)
	       `(def-trunc1 ,(make-name "~a1" name) RATIO
		  (with-ratio (n d number)
			      (,(make-name "~a2" name) n d)))))
    (def-irat1 truncate)
    (def-irat1 floor)
    (def-irat1 ceiling)
    (def-irat1 round))
  (macrolet ((def-irat2 (name)
	       `(def-trunc2 ,name RATIONAL RATIONAL
		  (with-ratios (n1 d1 number) (n2 d2 divisor)
			       (,name (mult n1 d2)
				      (mult n2 d1))))))
    (def-irat2 truncate2)
    (def-irat2 floor2)
    (def-irat2 ceiling2)
    (def-irat2 round2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FLOAT TRUNCATION

  (macrolet
      ((def-ftrunc1 (name op type)
	 `(def-trunc1 ,name ,type (,(make-name "~a-~a" op type) number)))
       (def-itrunc1 (name op type)
	 `(def-trunc1 ,name ,type
	    (,(make-name "TRUNCATE-~a" type)
	     (,(make-name "F~a-~a" op type) number))))

       (def-ftrunc2 (name op type1 type2 &optional (type type1))
	 `(def-trunc2 ,name ,type1 ,type2
	    (,(make-name "~a-~a" op type) (/ number divisor))))
       (def-itrunc2 (name op type1 type2 &optional (type type1))
	 `(def-trunc2 ,name ,type1 ,type2
	    (,(make-name "TRUNCATE-~a" type)
	     (,(make-name "F~a-~a" op type) (/ number divisor)))))

       (def-float2 (def op &aux (name (make-name "~a2" op)))
	 `(progn (,def ,name ,op SINGLE-FLOAT DOUBLE-FLOAT double-float)
		 (,def ,name ,op SINGLE-FLOAT SINGLE-FLOAT)
		 (,def ,name ,op SINGLE-FLOAT RATIO)
		 (,def ,name ,op SINGLE-FLOAT INTEGER)
		 (,def ,name ,op DOUBLE-FLOAT DOUBLE-FLOAT)
		 (,def ,name ,op DOUBLE-FLOAT SINGLE-FLOAT)
		 (,def ,name ,op DOUBLE-FLOAT RATIO)
		 (,def ,name ,op DOUBLE-FLOAT INTEGER)

		 (,def ,name ,op RATIO SINGLE-FLOAT single-float)
		 (,def ,name ,op INTEGER SINGLE-FLOAT single-float)
		 (,def ,name ,op RATIO DOUBLE-FLOAT double-float)
		 (,def ,name ,op INTEGER DOUBLE-FLOAT double-float)))
       (def-float (name)
	 (let ((name1 (make-name "~a1" name))
	       (fname1 (make-name "F~a1" name))
	       (fname (make-name "F~a" name)))
	   `(progn
	      (def-itrunc1 ,name1 ,name SINGLE-FLOAT)
	      (def-itrunc1 ,name1 ,name DOUBLE-FLOAT)
	      (def-ftrunc1 ,fname1 ,fname SINGLE-FLOAT)
	      (def-ftrunc1 ,fname1 ,fname DOUBLE-FLOAT)
	      (def-float2 def-itrunc2 ,name)
	      (def-float2 def-ftrunc2 ,fname)))))
    (def-float truncate)
    (def-float floor)
    (def-float ceiling)
    (def-float round)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(macrolet ((def-trunc (name)
	     `(defun ,name (number &optional (divisor 1 divisorp))
		(if divisorp
		    (,(make-name "~a2" name) number divisor)
		    (,(make-name "~a1" name) number)))))
  (def-trunc truncate)
  (def-trunc floor)
  (def-trunc ceiling)
  (def-trunc round)
  (def-trunc ftruncate)
  (def-trunc ffloor)
  (def-trunc fceiling)
  (def-trunc fround))


(defmethod REM ((number INTEGER) (divisor INTEGER))
  (if (and (fixnump number) (fixnump divisor))
      (rem-fixnum number divisor)
      (nth-value 1 (truncate2 number divisor))))

(defmethod REM ((number REAL) (divisor REAL))
  (nth-value 1 (truncate2 number divisor)))


(defmethod NUMERATOR ((number INTEGER)) number)
(defmethod NUMERATOR ((number RATIO)) (ratio-numerator number))
(defmethod DENOMINATOR ((number INTEGER)) 1)
(defmethod DENOMINATOR ((number RATIO)) (ratio-denominator number))

