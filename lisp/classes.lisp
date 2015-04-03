#| *****************************************************************
NOTES:

0. This file describes the behavior of our MOP class system using Lisp
   forms as the documenation.  It won't actually compile in Eclipse or
   other Lisp because of circularities.  We compile this using at
   Elwood using a special elwood-compile-only definition for
   static-defclass. 

1. The MOP requires that the class of all metaobject classes to be
   standard-class.  We interpret that to allow these to be 
   indirect (rather than direct) instances of standard-class.  ANSI's
   wording for things like standard-object definately allows our
   interpretation for ANSI classes.  

2. Our standard-system-class inherits from standard-class so that we
   can satisify the constraints of Note 1.  Unfortunately this adds
   some baggage to standard-system-class that we really don't need.
   IWBNI the standards defined an abstract base-standard-class (or
   simply class) and a concrete standard-class so that
   standard-system-class could inherit only from base-standard-class.
   Oh well. 

***************************************************************** |#
(defparameter *standard-method-combination*
  ;; The metaclass is really "standard", but that isn't defined at
  ;; this point.  It will be reset later.
  (make-standard-instance 0 method-combination-type-classobj
			  method-combination-type-wrapperobj))

(macrolet ((defclass (&rest args) `(static-defclass ,@args)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                     BASIC CLASSES                            ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass T () ()
  (:abstract t)
  (:metaclass built-in-class))

(defclass unknown-object (t) ()
  (:abstract t)
  (:metaclass built-in-class))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass SYMBOL (t) ()
  (:metaclass built-in-class))

(defclass CHARACTER (t) ()
  (:metaclass built-in-class))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass NUMBER (t) ()
  (:abstract t)
  (:metaclass built-in-class))


(defclass COMPLEX (number) ()
  (:metaclass built-in-class))

(defclass REAL (number) ()
  (:abstract t)
  (:metaclass built-in-class))

(defclass FLOAT (real) ()
  (:abstract t)
  (:metaclass built-in-class))
(defclass SINGLE-FLOAT (float) ()
  (:metaclass built-in-class))
(defclass DOUBLE-FLOAT (float) ()
  (:metaclass built-in-class))

(defclass RATIONAL (real) ()
  (:abstract t)
  (:metaclass built-in-class))
(defclass RATIO (rational) ()
  (:metaclass built-in-class))
(defclass INTEGER (rational) ()
  (:metaclass built-in-class))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass SEQUENCE (t) ()
  (:abstract t)
  (:metaclass built-in-class))

(defclass LIST (sequence) ()
  (:abstract t)
  (:metaclass built-in-class))

(defclass NULL (symbol list) ()
  (:metaclass built-in-class))

(defclass CONS (list) ()
  (:metaclass built-in-class))

;;; In Dylan, <array> is abstract, and there are implementation defined
;;; concrete subclasses.  Also <array> is a SUBCLASS of <sequence>, and
;;; there isn't anything that corresponds to cl:sequence.
(defclass ARRAY (t) ()
  (:metaclass built-in-class))

(defclass VECTOR (array sequence) ()
  (:abstract t)
  (:metaclass built-in-class))

;;; In Dylan, these would inherit from a subclass of <vector> called
;;; <stretchy-vector>. 
(defclass BIT-VECTOR (vector) ()
  (:abstract t)
  (:metaclass built-in-class))
(defclass STRING (vector) ()
  (:abstract t)
  (:metaclass built-in-class))
(defclass BASE-STRING (string) ()
  (:abstract t)
  (:metaclass built-in-class))
(defclass EXTENDED-STRING (string) ()
  (:abstract t)
  (:metaclass built-in-class))
(defclass General-Vector (vector)
  (#+dylan(dylan-name :initform 'object-vector))
  (:abstract t)
  (:metaclass built-in-class))

(defclass SIMPLE-ARRAY (array) ()
  (:metaclass built-in-class))
(defclass complex-array (array) ()
  (:metaclass built-in-class))

(defclass simple-basic-vector (vector simple-array array sequence)
  (#+dylan(dylan-name :initform 'simple-vector))
  (:abstract t)
  (:metaclass built-in-class))
(defclass SIMPLE-BIT-VECTOR (bit-vector simple-basic-vector) ()
  (:metaclass built-in-class))
(defclass SIMPLE-STRING (string simple-basic-vector) ()
  (:metaclass built-in-class))
(defclass SIMPLE-BASE-STRING (base-string simple-string) ()
  (:metaclass built-in-class))
(defclass SIMPLE-EXTENDED-STRING (extended-string simple-string) ()
  (:metaclass built-in-class))
(defclass SIMPLE-VECTOR (general-vector simple-basic-vector)
  (#+dylan(dylan-name :initform 'simple-object-vector))
  (:metaclass built-in-class))

(defclass complex-basic-vector (vector complex-array array sequence) ()
  (:abstract t)
  (:metaclass built-in-class))
(defclass COMPLEX-BIT-VECTOR (bit-vector complex-basic-vector) ()
  (:metaclass built-in-class))
(defclass COMPLEX-STRING (string complex-basic-vector) ()
  (:metaclass built-in-class))
(defclass COMPLEX-BASE-STRING (base-string complex-string) ()
  (:metaclass built-in-class))
(defclass COMPLEX-EXTENDED-STRING (extended-string complex-string) ()
  (:metaclass built-in-class))
(defclass COMPLEX-VECTOR (general-vector complex-basic-vector) ()
  (:metaclass built-in-class))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass STANDARD-OBJECT (t) ()
  (:abstract t)
  (:metaclass standard-class))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass FUNCTION (t) ()
  #+probably-should-be (:abstract t)
  (:metaclass standard-system-class))
(defclass COMPILED-FUNCTION (function) ()
  #+probably-should-be (:abstract t)
  (:metaclass standard-system-class))
(defclass BUILT-IN-FUNCTION (compiled-function) ()
  #+sealing (:sealed t)
  (:metaclass standard-system-class))

(defclass FUNCALLABLE-STANDARD-OBJECT (standard-object function) ()
  (:abstract t)
  (:metaclass standard-class))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass HASH-TABLE (t)
  (#+dylan(dylan-name :initform 'table)) 
  (:abstract t)
  (:primary t))

(defclass Open-Address-Hash-Table (hash-table) ()
  #+abstractly
  ((keys :accessor open-address-hash-table-keys)
   (values :accessor open-address-hash-table-values)
   (n-buckets :reader open-address-hash-table-n-buckets :initarg :n-buckets)
   (size :reader open-address-hash-table-size :initarg :size)
   (count :initform 0 :reader open-address-hash-table-count)
   (rehash-size :reader open-address-hash-table-rehash-size :initarg :rehash-size))
  (:abstract t) (:primary t) #+sealing (:sealed t))

(defclass Open-Address-Eq-Hash-Table (open-address-hash-table) ()
  (:primary t) (:sealed t)
  #+nn(:metaclass built-in-class))
(defclass Open-Address-Eql-Hash-Table (open-address-hash-table) ()
  (:primary t) (:sealed t)
  #+nn(:metaclass built-in-class))
(defclass Open-Address-Equal-Hash-Table (open-address-hash-table) ()
  (:primary t) (:sealed t)
  #+nn(:metaclass built-in-class))
(defclass Open-Address-Equalp-Hash-Table (open-address-hash-table) ()
  (:primary t) (:sealed t)
  #+nn(:metaclass built-in-class))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                METAOBJECT CLASSES                            ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass METAOBJECT (standard-object) 
  ((dependents :initform nil :type list)
   (documentation :initform nil :initarg :documentation
		  :accessor object-documentation))
  (:abstract t)
  (:primary t)
  (:metaclass standard-system-class))

;;;; SLOTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass SLOT-DEFINITION (metaobject)
  ((name :initarg :name :reader slot-definition-name :type symbol)
   (allocation :initform :instance :initarg :allocation
	       :reader slot-definition-allocation :type symbol)
   (type :initform t :initarg :type
	 :reader slot-definition-type)
   (initform :initarg :initform :reader slot-definition-initform)
   (initfunction :initform nil :initarg :initfunction
		 :reader slot-definition-initfunction)
   (initargs :initform nil :initarg :initargs
	     :reader slot-definition-initargs :type list))
  (:abstract t)
  (:primary t)
  (:metaclass standard-system-class))

(defclass DIRECT-SLOT-DEFINITION (slot-definition)
  ((readers :initform nil :initarg :readers
	    :reader slot-definition-readers :type list)
   (writers :initform nil :initarg :writers
	    :reader slot-definition-writers :type list))
  (:abstract t)
  (:primary t)
  (:metaclass standard-system-class))

(defclass EFFECTIVE-SLOT-DEFINITION (slot-definition)
  ((location :accessor slot-definition-location))
  (:abstract t)
  (:primary t)
  (:metaclass standard-system-class))

(defclass STANDARD-SLOT-DEFINITION (slot-definition)
  ()
  (:abstract t)
  (:metaclass standard-system-class))

(defclass STANDARD-DIRECT-SLOT-DEFINITION (standard-slot-definition
					   direct-slot-definition)
  ()
  (:metaclass standard-system-class))

(defclass STANDARD-EFFECTIVE-SLOT-DEFINITION (standard-slot-definition
					      effective-slot-definition)
  ()
  (:metaclass standard-system-class))

;;;; SPECIALIZERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass TYPE (metaobject)
  ((name :initarg :name :reader type-name))
  (:abstract t)
  (:primarty t)
  (:metaclass standard-system-class))

(defclass SPECIALIZER (type)
  ;; This reader method may be overridden for class metaobjects if
  ;; add-direct-method, remove-direct-method and
  ;; specializer-direct-generic-functions are overridden as well.
  ((direct-methods :initform nil :type list
		   :reader specializer-direct-methods))
  (:abstract t)
  (:primary t)
  (:metaclass standard-system-class))

(defclass CLASS (specializer)
  ((name :initform nil :reader class-name)
   (direct-subclasses :initform nil :reader class-direct-subclasses
		      :type list)
   (direct-superclasses :initarg :direct-superclasses
			:reader class-direct-superclasses
			:type list)
   (class-precedence-list :reader class-precedence-list
			  :type list)
   (direct-slots :initform nil :initarg :direct-slots
		 :reader class-direct-slots
		 :type list)
   (effective-slots :reader class-slots :type list)
   (default-initargs :reader class-default-initargs :type list)
   (direct-default-initargs :initform nil
			    :initarg :direct-default-initargs
			    :reader class-direct-default-initargs
			    :type list)
   ;; Actually, the MOP specifies that class-finalized-p returns nil
   ;; for uninintialized classes as well, which this won't do.
   ;; Why does the MOP say this?
   (finalized-p :initform nil :type boolean
		:reader class-finalized-p)
   (prototype :reader class-prototype)
   ;; Eclipse specific...
   (primary :initform nil :initarg :primary :type boolean
	    :reader primary-class-p)
   (sealed :initform nil :initarg :sealed :type boolean
	   :reader sealed-class-p)
   (abstract :initform nil :initarg :abstract :type boolean
	     :reader abstract-class-p)
   (wrapper :accessor class-wrapper :type wrapper))
  (:default-initargs :direct-superclasses nil)
  (:abstract t)
  (:primary t)
  (:metaclass standard-system-class))

(defclass BUILT-IN-CLASS (class)
  ((effective-slots :initform nil)
   (default-initargs :initform nil)
   (sealed :initform t)
   (finalized-p :initform t))
  (:sealed t)
  (:metaclass standard-system-class))

(defclass FORWARD-REFERENCED-CLASS (class)
  ()
  (:metaclass standard-system-class))

(defclass STANDARD-CLASS (class)
  ((direct-shared-slots :initform nil :type list
			:accessor class-direct-shared-slots)
   (initargs :reader class-initargs :type list))
  (:default-initargs
   :direct-superclasses (list (find-type 'standard-object)))
  (:primary t)
  (:metaclass standard-system-class))

(defclass FUNCALLABLE-STANDARD-CLASS (standard-class) ;Our choice
  ()
  (:default-initargs
   :direct-superclasses (list (find-type 'funcallable-standard-object)))
  (:metaclass standard-system-class))

(defclass interned-standard-class (standard-class)
  ((test :initform 'eql :initarg :test :reader class-hash-test)
   (instances :reader class-instances))
  (:metaclass standard-system-class))

(defclass standard-system-class (standard-class)
  ((finalized-p :initform t))
  (:sealed t)
  (:metaclass standard-system-class))

#+not-used-any-more
(defclass interned-standard-system-class (interned-standard-class)
  ((finalized-p :initform t))
  (:sealed t)
  (:metaclass standard-system-class))

(defclass funcallable-standard-system-class (funcallable-standard-class)
  ((finalized-p :initform t))
  (:sealed t)
  (:metaclass standard-system-class))


;;;; METHODS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass METHOD (metaobject)
  ((qualifiers :initform nil :initarg :qualifiers
	       :reader method-qualifiers :type list)
   (lambda-list :initarg :lambda-list :reader method-lambda-list
		:type list)
   (specializers :initarg :specializers :reader method-specializers
		 :type list)
   (function :initarg :function :reader method-function
	     :type function)
   (generic-function :initform nil :type generic-function
		     :reader method-generic-function)
   (declarations :initform nil :initarg :declarations :type list
		 :reader method-declarations))
  (:abstract t)
  (:primary t)
  (:metaclass standard-system-class))

(defclass STANDARD-METHOD (method)
  ((keywords)
   (allow-other-keys-p))
  (:primary t)
  (:metaclass standard-system-class))

(defclass STANDARD-ACCESSOR-METHOD (standard-method)
  ((slot-definition :initarg :slot-definition
		    :reader accessor-method-slot-definition
		    :type slot-definition))
  (:abstract t)
  (:primarty t)
  (:metaclass standard-system-class))

(defclass STANDARD-READER-METHOD (standard-accessor-method)
  ()
  (:metaclass standard-system-class))

(defclass STANDARD-WRITER-METHOD (standard-accessor-method)
  ()
  (:metaclass standard-system-class))

;;;; METHOD COMBINATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass method-combination-type (interned-standard-class)
  ((function :initarg :function :type function
	     :reader method-combination-type-function)
   (declarations :initform nil :initarg :declarations :type list
		 :reader method-combination-type-declarations)
   (test :initform 'equal))
  (:default-initargs
   :direct-superclasses (list (find-type 'method-combination)))
  (:primary t)
  (:sealed t)
  (:metaclass standard-system-class))

(defclass METHOD-COMBINATION (metaobject)
  ((function :initarg :function :type function
	     :reader method-combination-function)
   (options :initform nil :initarg :options :type list
	    :reader method-combination-options))
  (:abstract t)
  (:primary t)
  (:metaclass method-combination-type))

;;;; GENERIC FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass GENERIC-FUNCTION (metaobject funcallable-standard-object)
  ((name :initform nil :initarg :name
	 :reader generic-function-name)
   ;; Reader may signal error if sealed!
   (methods :initform nil :type list
	    :reader generic-function-methods)
   (method-class :initform (find-type 'standard-method)
		 :initarg :method-class
		 :reader generic-function-method-class)
   (lambda-list :initarg :lambda-list :type list
		:reader generic-function-lambda-list)
   (method-combination :initform *standard-method-combination*
		       :initarg :method-combination
		       :reader generic-function-method-combination)
   (argument-precedence-order :initarg :argument-precedence-order
			      :type list
			      :reader generic-function-argument-precedence-order)
   (declarations :initform nil :initarg :declarations
		 :reader generic-function-declarations))
  (:abstract t)
  (:primary t)
  (:metaclass funcallable-standard-system-class))

(defclass STANDARD-GENERIC-FUNCTION (generic-function)
  (;; Eclipse specific...
   (gf-emf-table :initform (make-emf-table) :type emf-table)
   (attached-methods :initform nil :type list)
   (required-parameters :type list
			:reader generic-function-required-parameters)

   (keywords :initform nil :reader generic-function-keywords)
   (allow-other-keys-p :initform nil
		       :reader generic-function-allow-other-keys-p)
   (sealed :initform nil :reader generic-function-sealed-p))
  (:primary t)
  (:metaclass funcallable-standard-system-class))

(defclass standard-system-generic-function (standard-generic-function)
  ()
  (:primary t)
  (:sealed t)
  (:metaclass funcallable-standard-system-class))


;;; PACKAGED FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass packaged-function (funcallable-standard-object)
  ((lambda :initarg :lambda  :reader function-lambda :initform nil)
   (env :initarg :env :reader function-env :initform t)
   (name :initarg :name :reader function-name :initform nil))
  (:abstract t)
  (:primary t)
  (:metaclass funcallable-standard-system-class))

(defclass macro-function (packaged-function)
  ((function))
  (:metaclass funcallable-standard-system-class))

(defclass INTERPRETED-FUNCTION (packaged-function)
  ((lambda :reader interpreted-function-lambda)
   (env :reader interpreted-function-env)
   (name :reader interpreted-function-name)) ;Note that name is third slot, as for gf.
  (:metaclass funcallable-standard-system-class))

(defclass BYTE-COMPILED-FUNCTION (compiled-function packaged-function)
  ()
  (:metaclass funcallable-standard-system-class))

)
