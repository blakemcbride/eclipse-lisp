(defparameter *root-class-list*
  '(T unknown-object SYMBOL CHARACTER NUMBER COMPLEX REAL FLOAT
      SINGLE-FLOAT DOUBLE-FLOAT RATIONAL RATIO INTEGER SEQUENCE LIST
      NULL CONS ARRAY VECTOR BIT-VECTOR STRING BASE-STRING extended-string
      General-Vector SIMPLE-ARRAY complex-array simple-basic-vector
      SIMPLE-BIT-VECTOR SIMPLE-STRING SIMPLE-BASE-STRING simple-extended-string SIMPLE-VECTOR
      complex-basic-vector COMPLEX-BIT-VECTOR COMPLEX-STRING
      COMPLEX-BASE-STRING complex-extended-string COMPLEX-VECTOR STANDARD-OBJECT FUNCTION
      macro-function COMPILED-FUNCTION built-in-function packaged-function
      INTERPRETED-FUNCTION BYTE-COMPILED-FUNCTION
      FUNCALLABLE-STANDARD-OBJECT HASH-TABLE Open-Address-Hash-Table
      Open-Address-Eq-Hash-Table Open-Address-Eql-Hash-Table
      Open-Address-Equal-Hash-Table Open-Address-Equalp-Hash-Table
      METAOBJECT SLOT-DEFINITION DIRECT-SLOT-DEFINITION
      EFFECTIVE-SLOT-DEFINITION STANDARD-SLOT-DEFINITION
      STANDARD-DIRECT-SLOT-DEFINITION
      STANDARD-EFFECTIVE-SLOT-DEFINITION TYPE SPECIALIZER CLASS
      BUILT-IN-CLASS FORWARD-REFERENCED-CLASS STANDARD-CLASS
      FUNCALLABLE-STANDARD-CLASS interned-standard-class
      standard-system-class funcallable-standard-system-class METHOD
      STANDARD-METHOD STANDARD-ACCESSOR-METHOD STANDARD-READER-METHOD
      STANDARD-WRITER-METHOD method-combination-type
      METHOD-COMBINATION GENERIC-FUNCTION STANDARD-GENERIC-FUNCTION
      standard-system-generic-function))

(defun class-var (name) (make-name "~a-CLASSOBJ" name))
(defun wrapper-var (name) (make-name "~a-WRAPPEROBJ" name))
(let ((*package* (find-package :eclipse)))
  (dolist (name *root-class-list*)
    (let ((wrapper (wrapper-var name))
	  (class (class-var name)))
      (proclaim `(special ,class ,wrapper))
      (setf (eclipse::global-declaration class 'eclipse::global-variable
					 'eclipse::c-external) t)
      (setf (eclipse::global-declaration wrapper 'eclipse::global-variable
					 'eclipse::c-external) t)
      (let ((wrapper-obj (eclipse::make-wrapper nil)))
	(setf (symbol-value wrapper) wrapper-obj
	      (symbol-value class)
	      (eclipse::make-static-standard-instance wrapper-obj))))))  