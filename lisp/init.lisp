(defmethod similar (x y) (equal x y))
(defmethod similar ((x package) (y package))
  (similar (package-name x) (package-name y)))

(defmethod similar ((x null) (y null)) t)
(defmethod similar ((x symbol) (y symbol))
  (or (eq x y)
      (and (null (symbol-package x))
	   (null (symbol-package y))
	   (equal (symbol-name x) (symbol-name y)))))

(defmethod similar :around ((x array) (y array))
  (and (eq (array-element-type x) (array-element-type y))
       (call-next-method)))
	
(defmethod similar ((x vector) (y vector))
  (loop for ex across x
	and ey across y
	always (similar ex ey)))

(defmethod similar ((x array) (y array))
  (let ((rank (array-rank x)))
    (and (= rank (array-rank y))
	 (dotimes (i rank t)
	   (unless (= (array-dimension x i)
		      (array-dimension y i))
	     (return nil)))
	 (dotimes (i (array-total-size x) t)
	   (unless (similar (row-major-aref x i)
			    (row-major-aref y i))
	     (return nil))))))

;;; IWBNI we defined similarity on hash-tables and random-states, too.

(setq *constant-check-hook* #'similar)

(defgeneric compile-object (definition &optional name))
(defmethod compile-object ((definition compiled-funciton) &optional name)
  (declare (ignore name))
  definition)
