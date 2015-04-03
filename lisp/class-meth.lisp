(macrolet ((defgeneric (&rest args)
	     `(static-defgeneric ,@args))
	   (defmethod (&rest args)
	     `(static-defmethod ,@args)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                        CLASSES                               ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SPECIALIZED ACCESSORS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric (SETF CLASS-NAME) (new-name class))
(defmethod (SETF CLASS-NAME) (new-name class) 
  (reinitialize-instance class :name new-name))	;As per MOP.

;;; The method on class may be overridden if add-direct-method,
;;; remove-direct-method and specializer-direct-methods are also.

(defgeneric SPECIALIZER-DIRECT-GENERIC-FUNCTIONS (specializer))
;;; Consolidated from additional specified methods on
;;; class, eql-specializer.
(defmethod specializer-direct-generic-functions ((specializer specializer))
  (loop with gfs = nil
	for method in (specializer-direct-methods specializer)
	for gf = (method-generic-function method)
	when gf do (pushnew gf gfs)
	finally (return gfs)))


;;; MAINTENANCE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The methods for the following two generic functions, together with
;;; class-direct-subclasses, may be overridden as a set.

(defgeneric ADD-DIRECT-SUBCLASS (superclass subclass))
(defmethod add-direct-subclass ((class class) (subclass class))
  (if (sealed-class-p class)
      (error 'sealed-object-error :object class
	     :invader subclass)
      (with-slots (direct-subclasses) class
	(pushnew subclass direct-subclasses))))

(defgeneric REMOVE-DIRECT-SUBCLASS (superclass subclass))
(defmethod remove-direct-subclass ((class class) (subclass class))
  (with-slots (direct-subclasses) class
    (setf direct-subclasses (remove subclass direct-subclasses))))


(defgeneric VALIDATE-SUPERCLASS (class superclass))

;;; This method may be overridden.
;;; N.B.: This doesn't handle standard-system-classes, but those are
;;; built by hand anyway.
(defmethod validate-superclass ((class class) (superclass class))
  (or (eq superclass t-classobj)
      (eq (class-of class) (class-of superclass))
      (let ((coclass (class-of class))
	    (cosuper (class-of superclass)))
	(or (and (eq coclass standard-class-classobj)
		 (eq cosuper funcallable-standard-class-classobj))
	    (and (eq cosuper standard-class-classobj)
		 (eq coclass funcallable-standard-class-classobj))))))

(defmethod validate-superclass ((class standard-class)
				(superclass forward-referenced-class))
  t)

(defmethod validate-superclass ((class standard-class)
				(superclass standard-class))
  t)

(defgeneric COMPUTE-CLASS-PRECEDENCE-LIST (class))

;;; This method may be overridden.
;;; N.B.: If we forced each superclass to be finalized (which isn't
;;; necessary), then it would not be necessary to excplictly check for
;;; forward references, because finalize-inheritance of a
;;; forward-referenced-class signals an error.

(defmethod compute-class-precedence-list ((class class))
  (let ((classes-to-order (collect-superclasses* class)))
    (check-for-forward-references class classes-to-order)
    (topological-sort classes-to-order
		      (delete-duplicates
		       (mapcan #'local-precedence-ordering
			       classes-to-order))
		      #'std-tie-breaker-rule)))

(defgeneric COMPUTE-DEFAULT-INITARGS (class))
;;; These methods may be overridden.
;;; Consolidated from additional specified methods on
;;; funcallable-standard-class. 
(defmethod compute-default-initargs ((class standard-class))
  (let ((class-precedence-list (class-precedence-list class)))
    (check-for-forward-references class class-precedence-list)
    (loop for iclass in class-precedence-list
	  with keys = nil
	  nconc (loop for initarg in (class-direct-default-initargs
				      iclass)
		      for key = (car initarg)
		      unless (find-list-eq key keys)
		      do (push key keys)
		      and collect initarg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;             INITIALIZATION OF CLASS METAOBJECTS              ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The use of canonicalize-class here isn't strictly necessary, but
;;; it saves us a generic-function call in the usual case.

(defgeneric ENSURE-CLASS-USING-CLASS (class name &key
					    direct-default-initargs
					    direct-slots
					    direct-superclasses
					    metaclass
					    &allow-other-keys))

(defmethod ensure-class-using-class ((class null) name
				     &rest keys
				     &key (metaclass 'standard-class)
				     &allow-other-keys)
  (declare (dynamic-extent keys))
  (setf (find-type name)
	(apply #'make-instance (canonicalize-class metaclass t)
	       :name name (canonicalize-ensure-class-args keys))))

(defmethod ensure-class-using-class ((class forward-referenced-class) name
				     &rest keys
				     &key (metaclass 'standard-class)
				     &allow-other-keys)
  (declare (dynamic-extent keys))
  (apply #'change-class class (canonicalize-class metaclass t)
	 :name name (canonicalize-ensure-class-args keys)))

;;; This method may be overridden.
(defmethod ensure-class-using-class ((class class) name
				     &rest keys
				     &key (metaclass 'standard-class)
				     &allow-other-keys)
  (declare (dynamic-extent keys))
  (check-metaclass class metaclass)
  (setf (find-type name)		;maybe name changed
	(apply #'reinitialize-instance class
	       :name name (canonicalize-ensure-class-args keys))))
		

;;; Is this promoted too high?  i.e. should be on standard-class?
(defmethod SHARED-INITIALIZE ((class class) slot-names &rest initargs
			      &key (direct-slots nil direct-slots-p)
			      (name nil namep) direct-superclasses)
  (declare (dynamic-extent initargs))
  (let ((new-superclasses nil)
	(old-superclasses (and (slot-boundp class 'direct-superclasses)
			       (class-direct-superclasses class))))
    (unless direct-superclasses
      (setf initargs
	    `(:direct-superclasses
	      ,(setf direct-superclasses
		     (funcall (third (assoc :direct-superclasses
					    (class-default-initargs
					     (class-of class))))))
	      ,@initargs)))
    (when direct-slots-p
      (setf initargs
	    `(:direct-slots
	      ,(setf direct-slots
		     (loop for spec in direct-slots
			   for slot = (make-direct-slot-definition class spec)
			   with names = nil
			   for name = (slot-definition-name slot)
			   if (find-list-eq name names)
			   do (multiple-appearance-error name :direct-slots)
			   else do (push name names)
			   collect slot))
	      ,@initargs)))
    (loop for super in direct-superclasses
	  unless (member super old-superclasses)
	  do (if (validate-superclass class super)
		 (push super new-superclasses)
		 (illegal-superclass class super name namep)))
    (apply #'call-next-method class slot-names initargs)
    (dolist (super new-superclasses) (add-direct-subclass super class))
    (dolist (slot direct-slots)
      (add-accessor-methods class slot :readers
			    (slot-definition-readers slot))
      (add-accessor-methods class slot :writers
			    (slot-definition-writers slot)))
    class))

(defmethod SHARED-INITIALIZE :AFTER ((class interned-standard-class)
				      slot-names &key)
  (declare (ignore slot-names))
  (setf (slot-value class 'instances)
	(make-hash-table :test (class-hash-test class))))

(defmethod SHARED-INITIALIZE ((class built-in-class) slot-names &key)
  (declare (ignore slot-names))
  (illegal-initialization "initialize or reinitialize" class))

(defmethod REINITIALIZE-INSTANCE ((class standard-system-class) &key)
  (illegal-initialization "reinitialize" class))

;;; See 5.5.1 "Reinitialization of Class Metaobjects" subprotocol in MOP.
;;; and 4.3.6 "Redefining Classes" in ANSI.
(defmethod REINITIALIZE-INSTANCE ((class class) &rest initargs &key
				  (direct-slots nil
						direct-slots-p) 
				  (direct-superclasses
				   nil
				   direct-superclasses-p)
				  &aux (old-superclasses
					(class-direct-superclasses class)))
  (declare (ignore direct-slots direct-superclasses)
	   (dynamic-extent initargs))
  (when direct-slots-p
    (loop for slot in (class-direct-slots class)
	  do (remove-accessor-methods class slot :readers)
	  do (remove-accessor-methods class slot :writers)))
  (if (class-finalized-p class)
      (let ((old-slots (class-slots class))
	    (old-cpl  (class-precedence-list class)))
	(call-next-method)
	(finalize-inheritance class)
	(when (or (not (equal old-cpl (class-precedence-list class)))
		  ;; changed local slot location
		  (loop for old in old-slots
			with new-slots = (class-slots class)
			when (local-slot-p old)
			do (let ((new
				  ;; FIND is implemented using CLOS
				  (loop for slot in new-slots
					with name = (slot-definition-name old)
					when (eql name (slot-definition-name slot))
					return slot)))
			     (unless (and new (eql (slot-definition-location old)
						   (slot-definition-location new)))
			       (return t)))))
	  (make-instances-obsolete class))
	(map-dependents class
			#'(lambda (dep)
			    (apply #'update-dependent class dep
				   initargs))))
      (call-next-method))
  (mapc #'finalize-subclasses (class-direct-subclasses class))
  (when direct-superclasses-p
    (loop for super in old-superclasses
	  with new = (class-direct-superclasses class)
	  unless (member super new)
	  do (remove-direct-subclass super class)))
  class)

(defun finalize-subclasses (class)
  (when (class-finalized-p class) (finalize-inheritance class))
  (mapc #'finalize-subclasses (class-direct-subclasses class)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                 CLASS FINALIZATION PROTOCOL                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Interestingly, it is possible to finalize-inheritance for a class
;;; without finalizing inheritance of its superclasses!

(defgeneric FINALIZE-INHERITANCE (class))
(defmethod finalize-inheritance ((class forward-referenced-class))
  (illegal-forward-reference class))

;;; Consolidated from additional specified method on
;;; funcallable-standard-class.

;;; N.B.: MAKE-INSTANCES-OBSOLETE is called AFTER this, and it can
;;; only use the wrapper stored in the class.  Therefore, we'd better
;;; not assign a new wrapper now if it's already  bound.
;;; Make-instance-obsolete will supply a new class-wrapper after it
;;; modifies the old one. 

(defmethod finalize-inheritance ((standard-class standard-class))
  (with-slots (class-precedence-list direct-superclasses
	       effective-slots default-initargs finalized-p
	       initargs prototype wrapper) standard-class

    ;; This can't be checked during initialization, because abstract
    ;; properties of class or (forward-referenced) superclass might
    ;; not be determined at that point.
    (when (abstract-class-p standard-class)
      (dolist (superclass direct-superclasses)
	(unless (abstract-class-p superclass)
	  (illegal-superclass standard-class superclass))))
    
    ;; Standard MOP definition
    (setf class-precedence-list (compute-class-precedence-list
				 standard-class))
    (let ((slots (compute-slots standard-class)))
      (setf effective-slots slots
	    default-initargs (compute-default-initargs standard-class)
	    finalized-p t)
      
      ;; Eclipse specific
      (when (unboundp wrapper) (setf wrapper (make-wrapper slots)))
      (setf initargs (loop for slot in slots
			   append (slot-definition-initargs slot))
	    prototype (allocate-instance standard-class)))
    ;; Programs may not depend on the returned value, but this is a
    ;; reaonsable, non-ugly thing to return. 
    standard-class))  


)