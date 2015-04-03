;;; defmacro describe-components is in eclipse-compile2.lisp
;;; Default is in describer.lisp

(defmethod describe-object ((object INTEGER) stream)
  (describe-components
   (object stream :type (if (fixnump object)
			    'fixnum
			    'bignum))
   ()
   (unless (fixnump object)
     (let ((big (integer-xint object)))
       (dotimes (i (xint-size big))
	 (format t "~%  ~d: ~x" i (digitref big i)))))))

(defmethod describe-object ((object FLOAT) stream)
  (multiple-value-bind (significand exponent sign)
      (integer-decode-float object)
    (describe-components
     (object stream)
     ((significand significand)
      (exponent exponent)
      (sign sign)))))

(defun describe-structure (object stream &optional
				  (slot-names
				   (mapcar #'slot-definition-name
					   (class-slots (class-of object)))))
				   
  (describe-components
   (object stream)
   ((class (class-of object)))
   (format stream "~%  SLOTS: ")
   (dolist (sn slot-names)
     (format stream "~%    ~s: ~:[UNBOUND~;~s~]"
	     sn
	     (slot-boundp object sn)
	     (and (slot-boundp object sn)
		  (slot-value object sn))))))

(defmethod describe-object ((object STANDARD-OBJECT) stream)
  (describe-structure object stream))
(defmethod describe-object ((object STRUCTURE-OBJECT) stream)
  (describe-structure object stream))
(defmethod describe-object ((object CLASS) stream)
  (describe-structure object stream
		      (remove 'wrapper
			       (mapcar #'slot-definition-name
				       (class-slots (class-of object))))))



(defmethod describe-object ((object OPEN-ADDRESS-HASH-TABLE) stream)
  (describe-components
   (object stream :type 'hash-table)
   ((test (hash-table-test object))
    (count (hash-table-count object))
    (size (hash-table-size object))
    (rehash-size (hash-table-rehash-size object))
    (rehash-threshold (hash-table-rehash-threshold object))
    (n-buckets (hash-table-n-buckets object)))
   (format stream "~%  COMPONENTS: ")
     (loop with limit = (hash-table-n-buckets object)
	   for index = 0 then (1+ this-index)
	   for this-index = (first-non-empty-index object index)
	   when (>= this-index limit) return nil
	   do (format stream "~%    ~d: ~s => ~s"
		      this-index
		      (hash-table-key object this-index)
		      (hash-table-value object this-index)))))


(defmethod describe-object ((object SIMPLE-ARRAY) stream)
  (describe-components
   (object stream)
   ((element-type (array-element-type object))
    (dimensions (array-dimensions object)))))

(defmethod describe-object ((object ARRAY) stream)
  (multiple-value-bind (displaced-to offset)
      (array-displacement object)
    (describe-components
     (object stream)
     ((element-type (array-element-type object))
      (dimensions (array-dimensions object))
      (displaced-to displaced-to)
      (displaced-index-offset offset))
     (when (array-has-fill-pointer-p object)
       (format t "~%  FILL-POINTER: ~d" (fill-pointer object))))))
     
(defmethod describe-object ((object SYMBOL) stream)
  (describe-components
   (object stream)
   ((name (symbol-name object))
    (package (symbol-package object))
    (value (if (boundp object) (symbol-value object) "<unbound>"))
    (function (if (fboundp object) (symbol-function object) "<unbound>"))
    (plist (symbol-plist object)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                          INSPECT                             ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Default is in describer.lisp

;;; Need to provide more interaction!!! eg. restarts that inspect each component.

