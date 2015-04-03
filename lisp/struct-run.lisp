;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                   PRIMITIVES                                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun structref (struct location)
  (with-update (struct (object-class struct))
	       (get-slot (standard-instance-slots struct) location)))

(defun (setf structref) (val struct location)
  (with-update (struct (object-class struct))
	       (set-slot (standard-instance-slots struct) location val)))

;;; ! Potential optimization: Have defstruct define the functions -
;;; particularly constructors and predicate (and maybe accessors?) -
;;; within a LET from which which they close over the class and
;;; wrapper instances that they need.  Also, the length of slot-values
;;; can be computed at macroexpansion time as well.  Finally, let
;;; MAKE-STRUCTURE be defined in C so that it: 1) does not cons up a
;;; list of aguments, 2) allows UNBOUND as an initial value by using
;;; the explicit count to determine end of arguments.  If this is
;;; done, we should remove the ' from in front of unbound-variable in
;;; the initial value for binding-value (env.lisp) and when testing
;;; for for dirty lexical-targets in TARGET-VALUE. (Actually, the
;;; check should use UNBOUNDP)

(defun make-empty-structure (class length)
  (make-standard-instance length class
			  (host::static-access class wrapper class)))
  
(defun make-structure (name &rest slot-values)
  (declare (dynamic-extent slot-values))
  (let* ((class (find-type name))
	 (struct (make-empty-structure class (length slot-values)))
	 (slots (standard-instance-slots struct)))
    (declare (type slots slots))
    (loop for value in slot-values
	  and index from 0
	  do (set-slot slots index value))
    struct))

(defun abstract-structure-class-p (class)
  (and (classp class (find-type 'structure-class))
       (abstract-class-p class)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;               STRUCTURE METAOBJECT CLASSES                   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   

(defclass STRUCTURE-OBJECT (t) ()
  (:abstract t))

(defclass STRUCTURE-CLASS (standard-class)
  ((type :initform 'structure-object :initarg :type
		   :reader structure-type)
   (named :initarg :named :reader structure-named)
   (type-name-index)
   (initial-offset :initform 0 :initarg :initial-offset
		   :reader structure-initial-offset)
   (inherited-slots :initform nil :initarg :inherited-slots
		    :reader structure-inherited-slots)
   (conc-name :initarg :conc-name :reader structure-conc-name)
   (predicate :initarg :predicate :reader structure-predicate)
   (copier :initarg :copier :reader structure-copier)
   (constructors :initarg :constructors :reader
		 structure-constructors)
   (contributing-slots))
  (:default-initargs
   :direct-superclasses (list (find-type 'structure-object)))
  #+nn(:sealed t)
  #+nn(:metaclass standard-system-class))

;;; This function gives us a non-nil value to put in the initfunction
;;; slot of a structure direct slot definition, so that when
;;; finalize-inheritance creates an effective slot definition, it
;;; will correctly copy the initform.
(defun illegal-structure-initialization ()
  (error "Structure slot definitions are not initialized through initfunctions."))

(defclass Structure-Slot-Definition (slot-definition)
  ((read-only :initform nil :initarg :read-only
	      :reader slot-definition-read-only)
   (hidden :initform nil :initarg :hidden
	   :reader slot-definition-hidden)
   (initform :initform nil)
   (initfunction :initform #'illegal-structure-initialization))
  #+nn(:metaclass standard-system-class))
(defclass Structure-Direct-Slot-Definition (structure-slot-definition
					    direct-slot-definition)
  ()
  #+nn(:metaclass standard-system-class))
(defclass Structure-Effective-Slot-Definition (structure-slot-definition
					       effective-slot-definition)
  ()
  #+nn(:metaclass standard-system-class))

(defmethod make-load-form ((object structure-class) &optional environment)
  `(ensure-structure ',(proper-name object t environment)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;               STRUCTURE METHODS                              ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
(defmethod DIRECT-SLOT-DEFINITION-CLASS ((class STRUCTURE-CLASS)
					 &rest initargs)
  (declare (ignore initargs) (dynamic-extent initargs))
  (find-type 'structure-direct-slot-definition))

(defmethod EFFECTIVE-SLOT-DEFINITION-CLASS ((class STRUCTURE-CLASS)
					    &rest initargs)
  (declare (ignore initargs) (dynamic-extent initargs))
  (find-type 'structure-effective-slot-definition))

(defun make-hidden-slot (&optional initform)
  (make-instance 'structure-direct-slot-definition
		 :name (gensym (if initform "NAME" "PAD"))
		 :hidden t :read-only t
		 :initform initform))


(defmethod COMPUTE-EFFECTIVE-SLOT-DEFINITION ((class structure-class)
					      name
					      direct-slot-definitions)
  (let* ((first (first direct-slot-definitions))
	 (second (second direct-slot-definitions))
	 (plist `(:name ,name
			:initform ,(slot-definition-initform first)
			:type
			,(let ((type (slot-definition-type first))
			       (super (if second
					  (slot-definition-type second)
					  't)))
			   (if (subtypep type super)
			       type
			       (signal-program-error
				"Type ~s for slot ~s is not a subtype of ~s."
				type name super)))
			:hidden ,(slot-definition-hidden first)
			:read-only
			,(if (slot-definition-read-only first)
			     t
			     (when (and second
					(slot-definition-read-only second))
			       (signal-program-error
				"Cannot make read-only slot ~s readable."
				name))))))
    (make-effective-slot-definition class plist)))

(defmethod SHARED-INITIALIZE :AFTER ((structure STRUCTURE-CLASS)
				     slot-names &rest initargs)
  (with-slots (name abstract type named direct-superclasses
		    initial-offset direct-slots
		    inherited-slots contributing-slots)
      structure
    (let ((struct-name name))
      (setf abstract (not (eq type 'structure-object)))
      (when (and abstract
		 (car-eq direct-superclasses (find-type 'structure-object)))
	(setf direct-superclasses (list (find-type 't))))
      (macrolet
	  ((initialize (slot-name initarg form)
		       `(when (and (eq (getf initargs ,initarg 'none)
				       'none)
				   (or (eq slot-names 't)
				       (member ',slot-name slot-names)))
			  (setf (slot-value structure ',slot-name)
				,form))))
	(initialize named :named (not abstract))
	(initialize conc-name :conc-name (concatenate-names struct-name "-"))
	(initialize predicate :predicate
		    (when named (concatenate-names struct-name "-P")))
	(initialize copier :copier (concatenate-names "COPY-" struct-name)))
      (let ((slots direct-slots))
	(when abstract
	  (when named (push (make-hidden-slot `',name) slots))
	  (dotimes (n initial-offset)
	    (push (make-hidden-slot) slots)))
	(setf contributing-slots slots
	      direct-slots (dolist (spec inherited-slots slots)
			     (push (make-direct-slot-definition
				    structure spec)
				   slots)))))))

     
(defmethod FINALIZE-INHERITANCE :AFTER ((structure STRUCTURE-CLASS))
  (with-slots (abstract named predicate 
			initial-offset type-name-index
			contributing-slots effective-slots)
      structure
    (unless (or abstract named)
      (key-not-allowed :named "for untyped structure ~s." structure))
    (when (and predicate (not named))
      (key-not-allowed :predicate "for unamed structure ~s." structure))
    (cond
     (abstract
      (when named
	(setf type-name-index (+ initial-offset
				 (- (length effective-slots)
				    (length contributing-slots))))))
     (t (when (/= initial-offset 0)
	  (key-not-allowed :initial-offset "for untyped structure ~s."
			   structure))))))    

;;; CREATION AND ACCESS

(defmethod ALLOCATE-INSTANCE ((class structure-class) &key)
  (unless (class-finalized-p class) (finalize-inheritance class))
  (make-empty-structure class (length (class-slots class))))

(defmethod SLOT-VALUE-USING-CLASS ((class structure-class)
				   (object structure-object)
				   (slot
				    structure-effective-slot-definition))
  (structref object (slot-definition-location slot)))
(defmethod (setf SLOT-VALUE-USING-CLASS) (new-value
					  (class structure-class)
					  (object structure-object)
					  (slot structure-effective-slot-definition))
  (setf (structref object (slot-definition-location slot)) new-value))

;;; Used by MAKE-LOAD-FORM-SAVING-SLOTS.
(defmethod SLOT-BOUNDP-USING-CLASS ((class structure-class)
				    (object structure-object)
				    (slot structure-effective-slot-definition))
  t)

(defun ensure-structure (name &rest keys)
  (declare (dynamic-extent keys))
  (let ((class (apply #'ensure-class name
		      :metaclass 'structure-class
		      keys)))
    (finalize-inheritance class)
    class))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                    COPY PROTOCOL                             ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   

;;; These are not peculiar to structures, but they are only structures
;;; use them right now.  See also, alternatives/copy.lisp.
(defgeneric Type-For-Copy (object))
(defmethod type-for-copy (object) (class-of object))
(defgeneric Shallow-Copy (object))
(defmethod Shallow-Copy (object)
  (let* ((class (type-for-copy object))
	 (copy (allocate-instance class)))
    (dolist (slot (class-slots (class-of object)))
      (setf (slot-value-using-class
	     class copy
	     (find-slot-definition
	      class (slot-definition-name slot)))
	    (slot-value-using-class class object slot)))
    copy))

(defun COPY-STRUCTURE (structure)
  (etypecase structure
    (structure-object (shallow-copy structure))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                    MISCELLANEOUS                             ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun make-standard-structure-constructor-name (structure-name)
  (concatenate-names "MAKE-" structure-name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                    PRINTING                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Returns a keylist suitable for use in the printer.
(defun make-structure-keylist (instance)
  (loop with class = (class-of instance)
	for slot in (class-slots class)
	nconc (list (make-keyword (slot-definition-name slot))
		    (structref instance
			       (slot-definition-location slot)))
	into plist
	finally (return (cons (class-name class) plist))))

(defun level-structure-printer (func)
  (declare (special *current-level*))
  #'(lambda (object stream) (funcall func object stream *current-level*)))

