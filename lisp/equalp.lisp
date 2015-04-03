;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 6.3 EQUALITY PREDICATES                                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgeneric EQUALP (x y))
(defmethod equalp (x y) (eq x y))
(defmethod equalp ((x number) (y number)) (= x y))
(defmethod equalp ((x character) (y character)) (char-equal x y))
(defmethod equalp ((x cons) (y cons))
  (and (equalp (car x) (car y))
       (equalp (cdr x) (cdr y))))
(defmethod equalp ((x array) (y array))
  (let ((rank-x (array-rank x))
	size)
    (and (= rank-x (array-rank y))
	 (if (= 1 rank-x)
	     (= (setq size (length x))
		(length y))
	     (dotimes (axis (array-rank x) t)
	       (unless (= (array-dimension x axis)
			  (array-dimension y axis))
		 (return nil))))
	 (dotimes (index (or size (array-total-size x)) t)
	   (let ((x-el (row-major-aref x index))
		 (y-el (row-major-aref y index)))
	     (unless (equalp x-el y-el)
	       (return nil)))))))
(defmethod equalp ((x hash-table) (y hash-table))
  (let ((n (hash-table-count x))
	(test (hash-table-test x)))
    (and (= n (hash-table-count y))
	 (eq test (hash-table-test y))
	 (with-hash-table-iterator (next-x x)
	   (loop
	    (multiple-value-bind (more key-x val-x) (next-x)
	      (if more
		  (multiple-value-bind (val-y foundp)
		      (gethash key-x y)
		    (unless (and foundp
				 (equalp val-x val-y))
		      (return nil)))
		  (return t))))))))
(defmethod equalp ((x structure-object) (y structure-object))
  (let ((class-x (class-of x)))
    (and (eq class-x (class-of y))
	 (dotimes (i (length (class-slots class-x)) t)
	   (unless (equalp (structref x i) (structref y i))
	     (return nil))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                          HASH CODES                          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  
(defparameter *equalp-circularity-hash-table* nil)
(defresource equalp-map ()
  :constructor (make-hash-table :test 'eq)
  :deinitializer (clrhash equalp-map))

(defun equalp-hash (x)
  (using-resource (*equalp-circularity-hash-table* equalp-map)
    (equalp-hash1 x)))

(defgeneric equalp-hash1 (x))
(defmethod equalp-hash1 (x) (equal-hash x))
(defmethod equalp-hash1 ((x character))
  (eq-hash (char-upcase x)))
(defmethod equalp-hash1 ((x number))
  (eq-hash (truncate x)))
(defmethod equalp-hash1 ((x complex))
  (let ((img (imagpart x))
	(realhash (equalp-hash1 (realpart x))))
    (if (zerop img)
	(equalp-hash1 realhash)
	(merge-hash-codes realhash (equalp-hash1 img)))))
(defmethod equalp-hash1 ((x hash-table))
  ;; If we had an ORDER INDEPENDENT merge-hash-codes, then we could
  ;; map over values using equalp-hash, and over keys using a hash
  ;; function based on the hash-table-test.
  (merge-hash-codes (eq-hash (hash-table-count x))
		    (eq-hash (hash-table-test x))))

(macrolet ((register (val)
		     `(or (gethash x *equalp-circularity-hash-table*)
			  (setf (gethash x *equalp-circularity-hash-table*)
				,val))))
  (defmethod equalp-hash1 ((x cons))
    (register
     (do ((x x next)
	  (n (equalp-hash1 (car x))
	     (merge-hash-codes n (equalp-hash1 (car x))))
	  (next (cdr x) (cdr next)))
	 (nil)
       (typecase next
	 (null (return n))
	 (cons)
	 (t (return (merge-hash-codes (equalp-hash1 next) n)))))))
  (defmethod equalp-hash1 ((x array))
    (register
     (loop for i from 1 below (size x)
	   with code = (equalp-hash1 (row-major-aref x 0)) 
	   do (setq code (merge-hash-codes
			  code (equalp-hash1 (row-major-aref x i))))
	   finally (return code))))
  (defmethod equalp-hash1 ((x structure-object))
    (register
     (loop with code = 0
	   with class =  (class-of x)
	   for slot in (class-slots class)
	   do (setq code
		    (merge-hash-codes code (equalp-hash1
					    (slot-value-using-class
					     class x slot))))))))


;;; java:hashCode = equals-hash (i.e. where equals is an open generic-function)

;;; Similar symbols must have the same sxhash in diferent images, so
;;; we can't use the address.
(defun SXHASH (object)
  (typecase object
    (symbol (merge-hash-codes
	     (equal-hash (symbol-package object))
	     (equal-hash (symbol-name object))))
    (hash-table (merge-hash-codes
		 (eq-hash (hash-table-count object))
		 (sxhash (hash-table-test object))))
    (t (equal-hash object))))
