;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant PI 3.14159265358979323846264338327950288419716939937511L0)
(defconstant single-pi #.(cl:float cl:pi 1.0f0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 12.10 IMPLEMENTATION PARAMETERS                              ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconstant LEAST-POSITIVE-SINGLE-FLOAT
    #+denormalized #.(cl:/ least-positive-normalized-single-float
			   (cl:scale-float 0.5f0 float-signif))
    #-denormalized least-positive-normalized-single-float)
(defconstant MOST-NEGATIVE-SINGLE-FLOAT		(- most-positive-single-float))
(defconstant LEAST-NEGATIVE-NORMALIZED-SINGLE-FLOAT (- least-positive-normalized-single-float))
(defconstant LEAST-NEGATIVE-SINGLE-FLOAT	(- least-positive-single-float))

(defconstant LEAST-POSITIVE-DOUBLE-FLOAT
    #+denormalized #.(cl:/ least-positive-normalized-double-float
			   (cl:scale-float 0.5d0 double-signif))
    #-denormalized least-positive-normalized-double-float)
(defconstant LEAST-NEGATIVE-DOUBLE-FLOAT	(- least-positive-double-float))
(defconstant LEAST-NEGATIVE-NORMALIZED-DOUBLE-FLOAT (- least-positive-normalized-double-float))
(defconstant MOST-NEGATIVE-DOUBLE-FLOAT		(- most-positive-double-float))

(defconstant MOST-POSITIVE-SHORT-FLOAT 	most-positive-single-float)
(defconstant LEAST-POSITIVE-SHORT-FLOAT least-positive-single-float)
(defconstant LEAST-POSITIVE-NORMALIZED-SHORT-FLOAT least-positive-normalized-single-float)
(defconstant LEAST-NEGATIVE-NORMALIZED-SHORT-FLOAT least-negative-normalized-single-float)
(defconstant LEAST-NEGATIVE-SHORT-FLOAT	least-negative-single-float)
(defconstant MOST-NEGATIVE-SHORT-FLOAT	most-negative-single-float)

(defconstant MOST-POSITIVE-LONG-FLOAT 	most-positive-double-float)
(defconstant LEAST-POSITIVE-LONG-FLOAT 	least-positive-double-float)
(defconstant LEAST-POSITIVE-NORMALIZED-LONG-FLOAT least-positive-normalized-double-float)
(defconstant LEAST-NEGATIVE-NORMALIZED-LONG-FLOAT least-negative-normalized-double-float)
(defconstant LEAST-NEGATIVE-LONG-FLOAT 	least-negative-double-float)
(defconstant MOST-NEGATIVE-LONG-FLOAT 	most-negative-double-float)

;;; These two are really ((double) FLT_EPSILON) and DBL_EPSILON,
;;; respectively!!!  See top of arithmetic.lisp.
(defconstant SINGLE-FLOAT-EPSILON #.(cl:scale-float 2.0f0 (cl:- float-signif)))
(defconstant DOUBLE-FLOAT-EPSILON #.(cl:scale-float 2.0d0 (cl:- double-signif)))
(defconstant SHORT-FLOAT-EPSILON 	single-float-epsilon)
(defconstant LONG-FLOAT-EPSILON		double-float-epsilon)

(defconstant SINGLE-FLOAT-NEGATIVE-EPSILON	single-float-epsilon)
(defconstant DOUBLE-FLOAT-NEGATIVE-EPSILON	double-float-epsilon)
(defconstant SHORT-FLOAT-NEGATIVE-EPSILON	single-float-negative-epsilon)
(defconstant LONG-FLOAT-NEGATIVE-EPSILON	double-float-negative-epsilon)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; COMPLEX

;;; Potential optimization: Rather than using complex arithmetic,
;;; IWBNI we determined the complex-part-type and then did the
;;; appropriate non-generic arithmetic.  We will wait until we have
;;; specialized representations for (complex single-float) and
;;; (complex double-float).

;;; As long as one argument is complex, get the real and imaginary
;;; parts of both and do the generic math.
(macrolet ((def-math (name type1 type2)
	     `(defmethod ,name ((x ,type1) (y ,type2))
		(with-complexes (rx ix x) (ry iy y)
				(complex (add rx ry) (add ix iy))))))
  (def-math add COMPLEX NUMBER)
  (def-math add NUMBER COMPLEX))
(macrolet ((def-math (name type1 type2)
	     `(defmethod ,name ((x ,type1) (y ,type2))
		(with-complexes (rx ix x) (ry iy y)
				(complex (subt rx ry) (subt ix iy))))))
  (def-math subt COMPLEX NUMBER)
  (def-math subt NUMBER COMPLEX))
(macrolet ((def-math (name type1 type2)
	     `(defmethod ,name ((x ,type1) (y ,type2))
		(with-complexes (rx ix x) (ry iy y)
				(complex (subt (mult rx ry) (mult ix iy))
					 (add (mult rx iy) (mult ix ry)))))))
  (def-math mult COMPLEX NUMBER)
  (def-math mult NUMBER COMPLEX))
(macrolet ((def-math (name type1 type2)
	     `(defmethod ,name ((x ,type1) (y ,type2))
		(with-complexes (rx ix x) (ry iy y)
				(let ((denom (add (mult ry ry) (mult iy iy))))
				  (complex (div (add (mult rx ry) (mult ix iy))
						denom)
					   (div (subt (mult ry ix) (mult iy rx))
						denom)))))))
  (def-math div COMPLEX NUMBER)
  (def-math div NUMBER COMPLEX))

(defmethod eq-number ((x COMPLEX) (y NUMBER))
  (and (eq-number (realpart x) (realpart y))
       (eq-number (imagpart x) (imagpart y))))
(defmethod eq-number ((x NUMBER) (y COMPLEX))
  (and (eq-number (realpart x) (realpart y))
       (eq-number (imagpart x) (imagpart y))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 12.2 PREDICATES ON NUMBERS                                   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun EVENP (number) (not (logtest 1 number)))
(defun ODDP (number) (logtest 1 number))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 12.3 ARITHMETIC OPERATIONS                                   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun CONJUGATE (number)
  (if (complexp number)
      (complex (realpart number) (- (imagpart number)))
    number))


(defun LCM (&optional (number 1) &rest numbers)
  (declare (dynamic-extent numbers))
  (dolist (other numbers number)
    (setq number (abs (* (truncate (max number other)
				   (gcd number other))
			 (min number other))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 12.5 IRRATIONAL AND TRANSCENDENTAL FUNCTIONS                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 12.5.1 EXPONENTIAL AND LOGARITHMIC FUNCTIONS

(defmethod-math1 EXP (DOUBLE-FLOAT))
(defmethod-math1 EXP (SINGLE-FLOAT))
(defmethod-math1 EXP (RATIO))
(defmethod-math1 EXP (INTEGER))

(defmethod EXP ((number complex))
  (mult (exp (realpart number))
	(cis (imagpart number))))

(defun expt-integer (base power)
  (declare (type integer power))
  (cond ((minusp power)

	 (/ 1 (expt-integer base (- power))))
	((eql base 2) (ash 1 power))
	(t (do ((nextn (ash power -1) (ash power -1))
		(total (if (oddp power) base
			   (etypecase base
			     (rational 1)
			     (single-float 1.0f0)
			     (double-float 1.0d0)
			     ((complex double-float) #c(1.0d0 0.0d0))
			     ((complex single-float) #c(1.0f0 0.0f0))
			     ((complex rational) 1)))
		       (if (oddp power) (* base total) total)))
	       ((zerop nextn) total)
	     (setq base (* base base))
	     (setq power nextn)))))

(defmethod EXPT ((base REAL) (power INTEGER))
  (expt-integer base power))
(defmethod EXPT ((base COMPLEX) (power INTEGER))
  (expt-integer base power))


(macrolet ((expt-branch (base power)
		       `(exp (* ,power (log ,base)))))
  (macrolet ((def-expt (type1 type2)
	       `(defmethod EXPT ((base ,type1) (power ,type2))
		  (expt-branch base power))))
    (def-expt COMPLEX FLOAT)
    (def-expt COMPLEX RATIO)
    (def-expt NUMBER COMPLEX))

  (macrolet ((check-sign (base power default)
			 `(if (minusp ,base)
			      (expt-branch ,base ,power)
			      ,default)))

    (macrolet ((def-expt (type1 type2)
		 `(defmethod EXPT ((base ,type1) (power ,type2))
		    (check-sign base power
				(,(make-name "EXPT2-~a-~a" type1 type2)
				 base power)))))
      (def-expt DOUBLE-FLOAT DOUBLE-FLOAT)
      (def-expt SINGLE-FLOAT DOUBLE-FLOAT)
      (def-expt RATIO DOUBLE-FLOAT)
      (def-expt INTEGER DOUBLE-FLOAT)

      (def-expt DOUBLE-FLOAT SINGLE-FLOAT)
      (def-expt SINGLE-FLOAT SINGLE-FLOAT)
      (def-expt RATIO SINGLE-FLOAT)
      (def-expt INTEGER SINGLE-FLOAT)

      (def-expt DOUBLE-FLOAT RATIO)
      (def-expt SINGLE-FLOAT RATIO)
      (def-expt RATIO RATIO)
      (def-expt INTEGER RATIO))))


(macrolet ((log-branch (number)
		       `(complex (log1 (abs ,number)) (phase ,number))))

  (defmethod log1 ((number COMPLEX))
    (log-branch number))

  (macrolet ((check-sign (number default)
			 `(if (minusp ,number)
			      (log-branch ,number)
			      ,default)))

    (macrolet ((def-log (type)
		 `(defmethod log1 ((number ,type))
		    (check-sign number
				(,(make-name "LOG-~a" type) number)))))
      (def-log INTEGER)
      (def-log RATIO)
      (def-log SINGLE-FLOAT)
      (def-log DOUBLE-FLOAT))))
			 
(defun LOG (number &optional (base nil base-p))
  (if base-p
      (/ (log1 number) (log1 base))
      (log1 number)))

#+not-yet
(define-compiler-macro log (number &optional (base nil basep))
  (if basep `(/ (log1 ,number) (log1 ,base))
    `(log1 ,number)))

(macrolet ((sqrt-branch (number)
			`(exp (/ (log1 ,number) 2))))
  
  (defmethod SQRT ((number COMPLEX))
    (sqrt-branch number))

  (macrolet ((check-sign (number default)
			 `(if (minusp ,number)
			      (sqrt-branch ,number)
			      ,default)))
    (macrolet ((def-sqrt (type)
		 `(defmethod SQRT ((number ,type))
		    (check-sign number
				(,(make-name "SQRT1-~a" type) number)))))
      (def-sqrt SINGLE-FLOAT)
      (def-sqrt DOUBLE-FLOAT)
      (def-sqrt RATIO)
      (def-sqrt INTEGER))))

;;; From discussion on comp.lang.lisp and Akira Kurihara.
(defun isqrt1 (n)
  (cond ((> n 24)
	 (let* ((n-len-quarter (ash (integer-length n) -2))
		(n-half (ash n (- (ash n-len-quarter 1))))
		(n-half-isqrt (isqrt1 n-half))
		(init-value (ash (1+ n-half-isqrt) n-len-quarter)))
	   (loop
	    (let ((iterated-value
		   (ash (+ init-value (truncate n init-value)) -1)))
	      (unless (< iterated-value init-value)
		(return init-value))
	      (setq init-value iterated-value)))))
	((> n 15) 4)
	((> n  8) 3)
	((> n  3) 2)
	((> n  0) 1)
	(t 0)))

(defun ISQRT (n)
  ;; theoretically (> n 7) ,i.e., n-len-quarter > 0
  (etypecase n
    (integer (isqrt1 n))))
