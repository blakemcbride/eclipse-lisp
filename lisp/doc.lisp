;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                 DOCUMENTATION                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; METAOBJECTS

(defmethod documentation ((x METAOBJECT) (doc-type (eql 't)))
  (declare (ignorable doc-type))
  (object-documentation x))
(defmethod (setf documentation) (new-value (x METAOBJECT) (doc-type (eql 't)))
  (declare (ignorable doc-type))
  (setf (object-documentation x) new-value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SPECIALIZED DOC-TYP FOR SPECIALIZED METAOBJECTS

(defmethod documentation ((x CLASS) (doc-type (eql 'type)))
  (declare (ignorable doc-type))
  (object-documentation x))
(defmethod (setf documentation) (new-value (x CLASS) (doc-type (eql 'type)))
  (declare (ignorable doc-type))
  (setf (object-documentation x) new-value))

(defmethod documentation ((x METHOD-COMBINATION) (doc-type (eql 'method-combination)))
  (declare (ignorable doc-type))
  (object-documentation x))
(defmethod (setf documentation) (new-value (x METHOD-COMBINATION)
					   (doc-type (eql 'method-combination)))
  (declare (ignorable doc-type))
  (setf (object-documentation x) new-value))

(defmethod documentation ((x GENERIC-FUNCTION) (doc-type (eql 'function)))
  (declare (ignorable doc-type))
  (object-documentation x))
(defmethod (setf documentation) (new-value (x GENERIC-FUNCTION)
					   (doc-type (eql 'function)))
  (declare (ignorable doc-type))
  (setf (object-documentation x) new-value))

(defmethod documentation ((x packaged-function) (doc-type (eql 'function)))
  (declare (ignorable doc-type))
  (let ((name (function-name x)))
    (when name (documentation name doc-type))))
(defmethod documentation ((x packaged-function) (doc-type (eql 't)))
  (declare (ignorable doc-type))
  (let ((name (function-name x)))
    (when name (documentation name doc-type))))

(defmethod (setf documentation) (new-value (x packaged-function)
					   (doc-type (eql 'function)))
  (declare (ignorable doc-type))
  (let ((name (function-name x)))
    (when name (setf (documentation name doc-type) new-value))))

(defmethod (setf documentation) (new-value (x packaged-function)
					   (doc-type (eql 't)))
  (declare (ignorable doc-type))
  (let ((name (function-name x)))
    (when name (setf (documentation name doc-type) new-value))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; INDIRECTION THROUGH SYMBOLS
(defun doc-data (name doc-type)
  (etypecase name
    (symbol (getf (get name 'doc-data) doc-type))
    (cons (getf (get (cdr name) 'doc-data) `(,(car name) ,doc-type)))))
(defun (setf doc-data) (doc name doc-type)
  (etypecase name
    (symbol (setf (getf (get name 'doc-data) doc-type) doc))
    (cons (setf (getf (get (cdr name) 'doc-data)
		      `(,(car name) ,doc-type))
		doc))))

(defmethod documentation ((x symbol) (doc-type (eql 'VARIABLE)))
  (doc-data x doc-type))
(defmethod documentation ((x symbol) (doc-type (eql 'SETF)))
  (doc-data x doc-type))
(defmethod documentation (x (doc-type (eql 'COMPILER-MACRO)))
  (doc-data x doc-type))
(defmethod documentation (x (doc-type (eql 'FUNCTION)))
  (doc-data x doc-type))
(defmethod (setf documentation) (new-value (x symbol) (doc-type (eql 'VARIABLE)))
  (setf (doc-data x doc-type) new-value))
(defmethod (setf documentation) (new-value (x symbol) (doc-type (eql 'SETF)))
  (setf (doc-data x doc-type) new-value))
(defmethod (setf documentation) (new-value x (doc-type (eql 'COMPILER-MACRO)))
  (setf (doc-data x doc-type) new-value))
(defmethod (setf documentation) (new-value x (doc-type (eql 'FUNCTION)))
  (setf (doc-data x doc-type) new-value))

(defmethod documentation ((x SYMBOL) (doc-type (eql 'type)))
  (let ((class (find-class x nil)))
    (if class
	(object-documentation  class)
      (doc-data x doc-type))))
;; N.B.: Calling (setf (documenation 'x 'type) "some doc") before there
;; is a class named x, and then defining a class named X, looses the
;; documenation defined for the type specifier.  This is consistent
;; with the idea that previously, X was a (possibly undefined) type
;; specifier, but now it is a class.
(defmethod (setf documentation) (new-value (x SYMBOL) (doc-type (eql 'type)))
  (let ((class (find-class x nil)))
    (if class
	(setf (object-documentation class) new-value)
      (setf (doc-data x doc-type) new-value))))

(defmethod documentation ((x SYMBOL) (doc-type (eql 'structure)))
  (declare (ignorable doc-type))
  (object-documentation (find-class x)))
(defmethod (setf documentation) (new-value (x SYMBOL) (doc-type (eql 'structure)))
  (declare (ignorable doc-type))
  (setf (object-documentation (find-class x)) new-value))

(defmethod documentation ((x SYMBOL) (doc-type (eql 'method-combination)))
  (declare (ignore doc-type))
  (object-documentation (find-method-combination-type x)))
(defmethod (setf documentation) (new-value (x SYMBOL)
					   (doc-type (eql 'method-combination)))
  (declare (ignore doc-type))
  (setf (object-documentation (find-method-combination-type x)) new-value))

