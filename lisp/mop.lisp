(macrolet ((defgeneric (&rest args)
	     `(static-defgeneric ,@args))
	   (defmethod (&rest args)
	     `(static-defmethod ,@args)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         DEPENDENT MAINTENANCE PROTOCOL                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The methods for the following three generic functions may be
;;; overridden as a set.
;;; All three have been consolidated from specified methods
;;; standard-class, funcallable-standard-class
;;; standard-generic-function.   

(defgeneric ADD-DEPENDENT (metaobject dependent))
(defmethod add-dependent ((metaobject metaobject) dependent)
  (with-slots (dependents) metaobject
    (pushnew dependent dependents)))

(defgeneric REMOVE-DEPENDENT (metaobject dependent))
(defmethod remove-dependent ((metaobject metaobject) dependent)
  (with-slots (dependents) metaobject
    (setq dependents (remove dependent dependents))))

(defgeneric MAP-DEPENDENTS (metaobject function))
(defmethod map-dependents ((metaobject metaobject) function)
  (with-slots (dependents) metaobject
    (dolist (dependent dependents)
      (funcall function dependent))))

(defgeneric UPDATE-DEPENDENT (metaobject dependent &rest initargs))


(defgeneric MAKE-INSTANCES-OBSOLETE (class))

(defmethod make-instances-obsolete ((class symbol))
  (make-instances-obsolete (find-type class)))

;;; Sealing issues???
;;; Consolidated from additional specified method on
;;; funcallable-standard-class. 
(defmethod make-instances-obsolete ((standard-class standard-class))
  (with-slots (wrapper) standard-class
    (make-wrapper-obsolete wrapper)
    (setf wrapper (make-wrapper (class-slots standard-class))))
  (mapc #'flush-method-cache (specializer-direct-generic-functions standard-class))
  (mapc #'make-instances-obsolete (class-direct-subclasses standard-class))
  standard-class)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                 INSTANCE STRUCTURE PROTOCOL                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgeneric SLOT-MISSING (class object slot-name operation &optional new-value))
(defmethod slot-missing ((class t) object slot-name operation
			 &optional (new-value nil new-value-p))
  ;; IWBNI this used a defined condition.
  (error "No slot ~s in ~s, operation ~s~:[~;, new-value ~s~]."
	 slot-name object operation new-value-p new-value))

(defgeneric SLOT-UNBOUND (class instance slot-name))
(defmethod slot-unbound ((class t) instance slot-name)
  (error 'unbound-slot :name slot-name :instance instance))

(defgeneric SLOT-BOUNDP-USING-CLASS (class instance slot))
(defgeneric SLOT-VALUE-USING-CLASS (class instance slot))
(defgeneric (SETF SLOT-VALUE-USING-CLASS) (new-value class instance slot))
(defgeneric SLOT-MAKUNBOUND-USING-CLASS (class instance slot))

;;; Potential optimization: Define specialized methods for sealed
;;; classes.
;;; IWNI after initialization, (setf slot-value-using-class) for sealed
;;; classes signalled an error.
;;; These methods may be overridden.
(macrolet
    ((def-access (class accessor)
       `(progn
	  (defmethod SLOT-BOUNDP-USING-CLASS ((class ,class) object
					      (slot
					       standard-effective-slot-definition)) 
	    (let ((location (slot-definition-location slot)))
	      (not (unboundp
		    (if (consp location)
			(cdr location)
			(with-update (object class)
				     (,accessor object location)))))))
	  (defmethod SLOT-VALUE-USING-CLASS ((class ,class) object
					     (slot
					      standard-effective-slot-definition)) 
	    (let* ((location (slot-definition-location slot))
		   (val (if (consp location)
			    (cdr location)
			    (with-update (object class)
					 (,accessor object location)))))
	      (if (unboundp val)
		  (values (slot-unbound class object (slot-definition-name slot)))
		  val)))
	  (defmethod SLOT-MAKUNBOUND-USING-CLASS ((class ,class) object
						  (slot
						   standard-effective-slot-definition)) 
	    (let ((location (slot-definition-location slot)))
	      (if (consp location)
		  (setf (cdr location) (symbol-value-value 'unbound-flag))
		  (with-update (object class)
			       (setf (,accessor object location)
				     (symbol-value-value 'unbound-flag)))))
	    object)
	  (defmethod (SETF SLOT-VALUE-USING-CLASS) (new-value
						    (class ,class) object
						    (slot
						     standard-effective-slot-definition)) 
	    (let ((location (slot-definition-location slot)))
	      (if (consp location)
		  (setf (cdr location) new-value)
		  (with-update (object class)
			       (setf (,accessor object location) new-value))))))))
  (def-access standard-class standard-instance-access)
  (def-access funcallable-standard-class funcallable-standard-instance-access))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;              OBJECT CREATION AND INITIALIZATION              ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Potential optimization: Initialize the instance returned by
;;; class-prototype with those slot-definition-initforms that happen
;;; to be constant, and then have allocate-instance return a copy.

;;; The current static-classes.lisp initialization assumes this is done!!!
;;; Note that when we set a new wrapper for the class, we must also assign
;;; that same wrapper to the tagged-instance-wrapper of the prototype.

;;; Potential optimization: classes which do not have applicable user
;;; defined methods for make-instance, allocate-instance,
;;; initialize-instance and shared-initialize can have a streamlined
;;; constructor function cached in the class object. Note that the
;;; detection of user methods must include those specialized on
;;; anything in the class-precedence-list, so this must wait until
;;; finalization.
;;;  0. The streamlined constructors can check initargs against a
;;;     cached list of values (or iterate over keys instead of
;;;     slots).
;;;  1. It may be cleaner/safer to define the various system defined
;;;  :before/:after/:around initialization methods as part of the
;;;  cached constructor for the system classes.
;;;  2. The cached constructor need only evaluate :default-initarg
;;;  initforms when they are actually used (like slot :initforms). The
;;;  validity of :default-initarg keys could be checked at the time
;;;  the constructor is defined.
;;;  3. The cached constructor could use knowledge of the class layout
;;;  to initialize values rather than using slot-boundp/slot-value.
;;;  4. Defclass could generate a structure-style constructor (with
;;;  defaulted keyword arguments) which would be compiled by the
;;;  file-compiler.  Care must be taken to generate the constructor if
;;;  the class already uses a non-streamlined constructor at
;;;  macroexpansion time, and to not use such a constructor if the
;;;  class already uses a non-streamlined constructor at load-time.

(defgeneric ALLOCATE-INSTANCE (class &key &allow-other-keys))
(defgeneric MAKE-INSTANCE (class &key &allow-other-keys))
(defgeneric INITIALIZE-INSTANCE (instance &key &allow-other-keys))
(defgeneric REINITIALIZE-INSTANCE (instance &key &allow-other-keys))
(defgeneric SHARED-INITIALIZE (instance slot-names &key &allow-other-keys))

(defmethod ALLOCATE-INSTANCE ((class built-in-class) &key)
  (error "Cannot allocate ~s." class))

(defmethod ALLOCATE-INSTANCE ((standard-class standard-class) &key)
  (unless (class-finalized-p standard-class) (finalize-inheritance standard-class))
  (with-slots (wrapper) standard-class
    (make-standard-instance (n-instance-slots (class-slots standard-class))
			    standard-class wrapper)))

(defmethod ALLOCATE-INSTANCE ((standard-class funcallable-standard-class) &key)
  (unless (class-finalized-p standard-class) (finalize-inheritance standard-class))
  (with-slots (wrapper) standard-class
    (make-funcallable-standard-instance (n-instance-slots (class-slots standard-class))
					standard-class wrapper)))

(defmethod MAKE-INSTANCE ((class symbol) &rest initargs)
  (declare (dynamic-extent initargs))
  (apply #'make-instance (find-type class) initargs))

(defmethod MAKE-INSTANCE ((class built-in-class) &key)
  (error "Cannot make-instance ~s." class))

(defmethod MAKE-INSTANCE ((class standard-class) &rest initargs)
  (declare (dynamic-extent initargs))
  (unless (class-finalized-p class) (finalize-inheritance class))
  (when (abstract-class-p class) (error "~s is abstract." class))
  ;; Any default-initargs that don't already appear as explicit
  ;; initargs are evaluated and appended to the explicit initargs.
  (let* ((default-initargs (class-default-initargs class))
	 (defaults (loop for (key nil initfunc) in default-initargs
			 when (eq (getf initargs key 'none) 'none)
			 nconc (list key (funcall initfunc))))
	 (defaulted-initargs (if defaults (append initargs defaults)
				 initargs))
	 (prototype (class-prototype class)))
    (check-initargs class defaulted-initargs
		    (allocate-instance class initargs)
		    (initialize-instance prototype initargs)
		    (shared-initialize prototype t initargs))
    (let ((instance (apply #'allocate-instance class defaulted-initargs)))
      (apply #'initialize-instance instance defaulted-initargs)
      instance)))

(defmethod INITIALIZE-INSTANCE ((instance standard-object) &rest initargs)
  (declare (dynamic-extent initargs))
  (apply #'shared-initialize instance t initargs))

(defmethod REINITIALIZE-INSTANCE ((instance standard-object) &rest initargs)
  (declare (dynamic-extent initargs))
  (check-initargs (class-of instance) initargs
		  (reinitialize-instance instance initargs)
		  (shared-initialize instance nil initargs))
  (apply #'shared-initialize instance nil initargs))


;;; Set slots explicitly mentioned as initargs (regardless of
;;; whether they were already bound or listed in slot-names) and
;;; initialize those unbound slots which are in slot-names.
(defmethod SHARED-INITIALIZE ((instance standard-object) slot-names
			      &rest initargs &key)
  (declare (dynamic-extent initargs))
  (let ((class (class-of instance)))
    (dolist (slot (class-slots class))
      (let ((slot-name (slot-definition-name slot)))
	(multiple-value-bind (init-key init-value explicit-arg-p)
	    (get-properties initargs (slot-definition-initargs slot))
	  (declare (ignore init-key))
	  (if explicit-arg-p
	      (setf (slot-value-using-class class instance slot) init-value) 
	      (when (and (check-slot-initialization-p slot-name slot-names)
			 (not (slot-boundp-using-class class instance slot)))
		(let ((func (slot-definition-initfunction slot)))
		  (when func
		    (setf (slot-value-using-class class instance slot)
			  (funcall func)))))))))) 
  instance)


(defgeneric UPDATE-INSTANCE-FOR-REDEFINED-CLASS
  (instance added-slots discarded-slots property-list &key &allow-other-keys))

(defmethod update-instance-for-redefined-class
    ((instance standard-object) added-slots discarded-slots
     property-list &rest initargs)
  (declare (ignorable discarded-slots property-list)
	   (dynamic-extent initargs))
  (check-initargs (class-of instance) initargs
		  (update-instance-for-redefined-class
		   instance added-slots discarded-slots property-list
		   initargs) 
		  (shared-initialize instance added-slots initargs))
  (apply #'shared-initialize instance added-slots initargs))


(defgeneric UPDATE-INSTANCE-FOR-DIFFERENT-CLASS (previous current &key
							  &allow-other-keys))
(defmethod update-instance-for-different-class ((previous standard-object)
						(current standard-object)
						&rest initargs)
  (declare (dynamic-extent initargs))
  ;; Do we need to gather slot-names before check-initargs?
  (let ((new-class (class-of current)))
    (check-initargs new-class initargs
		    (update-instance-for-different-class previous current initargs)
		    (shared-initialize current t initargs))
    (apply #'shared-initialize
	   current
	   (loop for slot in (class-slots new-class)
		 with old-slots = (class-slots (class-of previous))
		 for slot-name = (slot-definition-name slot)
		 when (and (local-slot-p slot)
			   (not (find slot-name old-slots
				      :key #'slot-definition-name)))
		 collect slot-name)
	   initargs)))


(defgeneric CHANGE-CLASS (instance new-class &key &allow-other-keys))

(defmethod change-class (instance (new-class symbol) &rest initargs)
  (declare (dynamic-extent initargs))
  (apply #'change-class instance (find-type new-class) initargs))

(defmethod change-class (instance (new-class standard-system-class) &key)
  (illegal-change-class instance new-class))

(defmethod change-class ((instance metaobject) new-class &key)
  (illegal-change-class instance new-class))

(defmethod change-class ((instance forward-referenced-class) new-class
			 &rest initargs) 
  (declare (dynamic-extent initargs))
  (standard-change-class instance new-class initargs))    

(defmethod change-class ((instance standard-object)
			 (new-class standard-class)
			 &rest initargs)
  (declare (dynamic-extent initargs))
  (standard-change-class instance new-class initargs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                        SLOTS                                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod REINITIALIZE-INSTANCE ((instance SLOT-DEFINITION) &key)
  (illegal-initialization "reinitalize" instance))

(defgeneric READER-METHOD-CLASS (class direct-slot &rest initargs))
(defgeneric WRITER-METHOD-CLASS (class direct-slot &rest initargs))
;;; These methods may be overridden.
;;; Consolidated from additional specified methods on
;;; funcallable-standard-class. 
(defmethod reader-method-class ((class standard-class)
				(direct-slot standard-direct-slot-definition)
				&rest initargs)
  (declare (ignore initargs) (dynamic-extent initargs))
  standard-reader-method-classobj)
(defmethod writer-method-class ((class standard-class)
				(direct-slot standard-direct-slot-definition)
				&rest initargs)
  (declare (ignore initargs) (dynamic-extent initargs))
  standard-writer-method-classobj)

(defgeneric DIRECT-SLOT-DEFINITION-CLASS (class &rest initargs))
;;; These methods may be overridden.
;;; Consolidated from additional specified methods on
;;; funcallable-standard-class. 
(defmethod direct-slot-definition-class ((class standard-class)
					 &rest initargs)
  (declare (ignore initargs) (dynamic-extent initargs))
  (find-type 'standard-direct-slot-definition))

(defgeneric EFFECTIVE-SLOT-DEFINITION-CLASS (class &rest initargs))
;;; These methods may be overridden.
;;; Consolidated from additional specified methods on
;;; funcallable-standard-class. 
(defmethod effective-slot-definition-class ((class standard-class)
					    &rest initargs)
  (declare (ignore initargs) (dynamic-extent initargs))
  (find-type 'standard-effective-slot-definition))

(defgeneric COMPUTE-EFFECTIVE-SLOT-DEFINITION
  (class name direct-slot-definitions))
;;; Consolidated from additional specified methods on
;;; funcallable-standard-class.
(defmethod compute-effective-slot-definition ((class standard-class)
					      name
					      direct-slot-definitions)
  (let* ((initer (find-if-not #'null direct-slot-definitions
			      :key #'slot-definition-initfunction))
	 (plist `(;; Use value from first slot:
		  :name ,name
		  :allocation ,(slot-definition-allocation
				(car direct-slot-definitions))
		  ;; Use first value found:
  		  :initform ,(when initer
			       (slot-definition-initform initer))
		  :initfunction ,(when initer
				   (slot-definition-initfunction initer))
		  :documentation
		  ,(let ((doc (find-if-not #'null
					   direct-slot-definitions
					   :key #'object-documentation)))
		     (when doc (object-documentation doc)))
		  ;; Union of those found:
		  :type ,(intersect-types
			  (loop for slot in direct-slot-definitions
				for type = (slot-definition-type slot)
				unless (eq type 't)
				collect type)
			  nil)
		  :initargs
		  ,(loop for slot in direct-slot-definitions
			 nconc (loop for initarg in
				     (slot-definition-initargs slot)
				     unless (find-list-eq initarg initargs)
				     collect initarg) into initargs
			 finally (return initargs)))))
    (make-effective-slot-definition class plist)))

(defgeneric COMPUTE-SLOTS (class))
(defmethod compute-slots :around ((class standard-class))
  (compute-slot-locations class (call-next-method)))

;;; This primary method (but not the :around method,) may be overriden.

;;; The spec requires that the slots given to
;;; compute-effective-slot-definition are in class-precedence-order.

;;; In addition, we would like to preserve ordering as much as
;;; possible between classes (to promote access optimization).  We do
;;; this by yielding a result in REVERSE class-precedence-order.
;;; I.e. truly new slots are added to the end, and redefinitions of
;;; slots don't change their locations.

;;; Finally, we have also chosen to preserve new slot ordering within
;;; a class.  It isn't very Lisp-like to be adding new things to the
;;; END of a list, but nonetheless, I believe this is the behavior
;;; people expect.

;;; Ex: (defclass root () (a b c))			=> a b c
;;; (defclass subclass (root) (a d e))			=> a b c d e
;;; (defclass further-subclass (subclass) (b f d)) 	=> a b c d e f

(defmethod compute-slots ((class standard-class))
  (let ((all-slots nil))
    (dolist (iclass (class-precedence-list class))
      (setq all-slots (append (class-direct-slots iclass)
			      all-slots)))
    (loop for remaining-slots on all-slots
	  for slot = (car remaining-slots)
	  for name = (slot-definition-name slot)
	  unless (find-list-eq name names)
	  collect name into names
	  and collect
	  (compute-effective-slot-definition
	   class name
	   (let (slots)
	     (dolist (slot remaining-slots slots)
	       (when (eql name (slot-definition-name slot))
		 (push slot slots))))))))
)