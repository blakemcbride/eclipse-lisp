;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 6.1 LOGICAL VALUES                                           ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf (global-declaration 'nil 'global-variable 'special) t)
(setf (global-declaration 'nil 'global-variable 'constant) t)
(setf (global-declaration 't 'global-variable 'special) t)
(setf (global-declaration 't 'global-variable 'constant) t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 6.2.2 SPECIFIC DATA TYPE PREDICATES                          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(macrolet
    ((def-class-p (predicate class-name)
       `(defun ,predicate (x)
	  (not (null (classp x ,(host::class-var class-name)))))))
  (def-class-p RATIONALP rational)
  (def-class-p FLOATP float)
  (def-class-p REALP real)
  (def-class-p NUMBERP number)
  (def-class-p SIMPLE-STRING-P simple-string)
  (def-class-p STRINGP string)
  (def-class-p BIT-VECTOR-P bit-vector)
  (def-class-p VECTORP vector)
  (def-class-p ARRAYP array)
  (def-class-p FUNCTIONP function)
  (def-class-p COMPILED-FUNCTION-P compiled-function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 6.3 EQUALITY PREDICATES                                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgeneric EQUAL (x y))
(defmethod equal (x y) (eq x y))
(defmethod equal ((x number) (y number)) (eql x y))
(defmethod equal ((x cons) (y cons))
  (and (equal (car x) (car y))
       (equal (cdr x) (cdr y))))
(defmethod equal ((x string) (y string))
  (not (null (string-eq x y))))
(defmethod equal ((x bit-vector) (y bit-vector))
  (let ((lx (length x)))
    (and (= lx (length y))
	 (dotimes (n lx t)
	   (unless (= (bit x n) (bit y n))
	     (return nil))))))

;;; Hashing functions must always terminate.  For equal-hash, only
;;; circular lists could cause problems, and this is checked for using
;;; Steele's list-length strategy.

(defgeneric equal-hash (x))
(defmethod equal-hash (x) (eql-hash x))
(defmethod equal-hash ((x cons))
  (do* ((x x cddr)
	(n (equal-hash (car x))
	   (merge-hash-codes n (equal-hash (car x))))
	(cdr (cdr x) (cdr x))
	cddr
	(circlep nil (eq x slow))
	(slow x (cdr slow)))
      (circlep n)
    (declare (type fixnum n))
    (cond ((null cdr) (return n))
	  ((consp cdr) (setq n (merge-hash-codes n (equal-hash (car cdr)))
			     cddr (cdr cdr)))
	  (t (return (merge-hash-codes (equal-hash cdr) n))))
    (cond ((atom cddr) (if cddr
			   (return (merge-hash-codes (equal-hash cddr) n))
			   (return n)))
	  ((consp cdr) nil)
	  (t (return (merge-hash-codes (equal-hash cddr) n))))))

  
(macrolet
    ((hash-array (x hasher length accessor)
		 `(loop with code fixnum = #xAA
			for i from 0 below ,length
			do (setq code (merge-hash-codes
				       code (the fixnum (,hasher (,accessor ,x i)))))
			finally (return code))))
  #|(defmethod equal-hash ((x simple-base-string))
    (hash-array x equal-hash (vector-size x) base-schar))
  (defmethod equal-hash ((x simple-extended-string))
    (hash-array x equal-hash (vector-size x) extended-schar))
  (defmethod equal-hash ((x string))
    (hash-array x equal-hash (length x) char))|#
  (defmethod equal-hash ((x string)) (string-hash x))

  (defmethod equal-hash ((x simple-bit-vector))
    (hash-array x eq-hash
		(ceiling (vector-size x) digit-size)
		digitref))
  (defmethod equal-hash ((x bit-vector))
    (multiple-value-bind (vector offset) (get-simple-vector x 0)
      (macrolet ((ref (v i) `(digitref ,v (+ ,i offset))))
	(hash-array vector eq-hash
		    (ceiling (length x) digit-size)
		    ref)))))