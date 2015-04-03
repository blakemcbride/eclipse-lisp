(let ((x (make-symbol "UNBOUND")))
  (defun unbound () x))

(defun class-precedence-list1 (class)
  (unless (class-finalized-p class) (finalize-inheritance class))
  (class-precedence-list class))

;;; Finding a Class's Subclasses

(defun mapappend (fn list)
  (apply #'append (mapcar fn list)))

(defun subclasses* (class)
  (remove-duplicates
   (cons class
	 (mapappend #'subclasses*
		    (class-direct-subclasses class)))))

(defun subclasses (class) (remove class (subclasses* class)))


;;; Regenerating Class Definitions

(defun display-defclass (class-name)
  (pprint (generate-defclass (find-class class-name)))
  (values))

(defun generate-defclass (class)
  `(defclass ,(class-name class)
       ,(mapcar #'class-name (class-direct-superclasses class))
     ,(mapcar #'generate-slot-specification (class-direct-slots class))))

(defun generate-slot-specification (slot)
  `(,(slot-definition-name slot)
    ,@(when (slot-definition-initfunction slot)
	`(:initform ,(slot-definition-initform slot)))
    ,@(when (slot-definition-initargs slot)
	(mapappend #'(lambda (initarg) `(:intarg ,initarg))
		   (slot-definition-initargs slot)))
    ,@(when (slot-definition-readers slot)
	(mapappend #'(lambda (reader) `(:reader ,reader))
		   (slot-definition-readers slot)))
    ,@(when (slot-definition-writers slot)
	(mapappend #'(lambda (writer) `(:writer ,writer))
		   (slot-definition-writers slot)))))


;;; Displaying Inherited Information

(defun display-defclass* (class-name)
  (pprint (generate-defclass* (find-class class-name)))
  (values))

(defun generate-defclass* (class)
  `(defclass* ,(class-name class)
       ,(mapcar #'class-name (class-precedence-list1 class))
     ,(mapcar #'(lambda (slot)
		  (generate-inherited-slot-specification class slot))
	      (class-slots class))))


(defun good-specializer-name (spec)
  (if (eql-specializer-p spec)
      `(eql ,(eql-specializer-object spec))
      (class-name spec)))
  
(defun specializer-name-list (method)
  (loop for spec in (method-specializers method)
	collect (good-specializer-name spec)))

(defun silly-method-more-specific-p (gf method1 method2)
  (let ((args (generic-function-required-parameters gf))
	(order (generic-function-argument-precedence-order gf))
	(specs1 (method-specializers method1))
	(specs2 (method-specializers method2)))
    (dolist (name order nil)
      (let* ((position (position name args))
	     (spec1 (nth position specs1))
	     (spec2 (nth position specs2)))
	(unless (eq spec1 spec2)
	  (return
	   (cond ((eql-specializer-p spec1) t)
		 ((eql-specializer-p spec2) nil)
		 ((find spec2 (progn (unless (class-finalized-p spec1)
					 (finalize-inheritance spec1))
				     (class-precedence-list spec1))) t)
		 ((find spec1 (progn (unless (class-finalized-p spec2)
				       (finalize-inheritance spec2))
				     (class-precedence-list spec2))) nil)
		 (t (string< (class-name spec1) (class-name spec2))))))))))

(defun display-methods (class)
  (setq class (canonicalize-class class t))
  (finalize-inheritance class)
  (let ((methods nil)
	(cpl (class-precedence-list class))
	(so (find-type 'standard-object)))
    (loop for class in cpl
	  when (eq class so) return nil
	  do (loop for method in (specializer-direct-methods class)
		   for gf = (method-generic-function method)
		   for existing = (assoc gf methods)
		   if existing do (push method (cdr existing))
		   else do (setf methods (acons gf (list method) methods))))
    (setq methods
	  (sort methods #'(lambda (n1 n2)
			    (cond ((and (consp n1) (consp n2))
				   (string< (second n1) (second n2)))
				  ((consp n1) nil)
				  ((consp n2) t)
				  (t (string< n1 n2))))
		:key #'(lambda (entry) (generic-function-name (car entry)))))
    (cons (mapcar #'good-specializer-name cpl)
	  (loop for entry in methods
		collect
		(let* ((gf (car entry))
		       (meths (cdr entry))
		       (sorted 
			(sort meths
			      #'(lambda (m1 m2)
				  (silly-method-more-specific-p
				   gf m1 m2))))
		       (around (remove :around sorted :key #'method-qualifiers :test-not #'eql))
		       (primary (remove nil sorted :key #'method-qualifiers :test-not #'eql))
		       (before (remove :before sorted :key #'method-qualifiers :test-not #'eql))
		       (after (remove :after sorted :key #'method-qualifiers :test-not #'eql)))
		  `(,(generic-function-name gf)
		    ,@(when around
			`(:around ,(mapcar #'specializer-name-list around)))
		    ,@(when before
			`(:before ,(mapcar #'specializer-name-list before)))
		    (:primary ,(mapcar #'specializer-name-list primary))
		    ,@(when after
			`(:after ,(mapcar #'specializer-name-list after)))))))))
					   
	  

(defun generate-inherited-slot-specification (class slot)
  (let* (direct-slot
	 (source-class
	  (find-if #'(lambda (superclass)
		       (setf direct-slot 
			     (find (slot-definition-name slot)
				   (class-direct-slots superclass)
				   :key #'slot-definition-name)))
		   (class-precedence-list1 class)))
	 (generated-slot-spec
	  (generate-slot-specification direct-slot)))
    (if (eq source-class class)
	generated-slot-spec
      (append generated-slot-spec
	      `(:inherited-from ,(class-name source-class))))))


;;; Ordering of Classes in Multiple Inheritance

(defun common-subclasses* (class-1 class-2)
  (intersection (subclasses* class-1) (subclasses* class-2)))

(defun in-order-p (c1 c2)
  (flet ((in-order-at-subclass-p (sub)
	   (let ((cpl (class-precedence-list1 sub)))
	     (not (null (member c2 (cdr (member c1 cpl))))))))
    (or (eq c1 c2)
	(every #'in-order-at-subclass-p
	       (common-subclasses* c1 c2)))))


;;; Ordering of Slots in Multiple Inheritance

(defun has-diamond-p (class)
  (some #'(lambda (pair)
	    (not (null (common-subclasses* (car pair)
					   (cadr pair)))))
	(all-distinct-pairs (class-direct-subclasses class))))

(defun all-distinct-pairs (set)
  (if (null set) 
      ()
    (append (mapcar #'(lambda (rest)
			(list (car set) rest))
		    (cdr set))
	    (all-distinct-pairs (cdr set)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;               BROWSING GENERIC FUNCTIONS                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Regenerating Generic Function and Method Definitions

(defun generate-defgeneric (gf)
  `(defgeneric ,(generic-function-name gf)
       ,(generic-function-lambda-list gf)))

(defun generate-defmethod (method &key show-body)
  (declare (ignore show-body))		;Method-body is only in closette.
  `(defmethod ,(generic-function-name (method-generic-function method))
       ,@(method-qualifiers method)
     ,(generate-specialized-arglist method)
     #|,@(when show-body (method-body method))|#))

(defun generate-specialized-arglist (method)
  (let* ((specializers (method-specializers method))
	 (lambda-list (method-lambda-list method))
	 (number-required (length specializers)))
    (append (mapcar #'(lambda (arg class)
			(if (eq class (find-class 't))
			    arg
			  `(,arg ,(class-name class))))
		    (subseq lambda-list 0 number-required)
		    specializers)
	    (subseq lambda-list number-required))))


(defun generate-generic-function (gf-name &key show-body)
  (cons (generate-defgeneric (fdefinition gf-name))
	(mapcar #'(lambda (method)
		    (generate-defmethod method :show-body show-body))
		(generic-function-methods (fdefinition gf-name)))))

(defun display-generic-function (gf-name &key show-body)
  (display-defgeneric gf-name)
  (dolist (method (generic-function-methods (fdefinition gf-name)))
    (pprint (generate-defmethod method :show-body show-body)))
  (values))

(defun display-defgeneric (gf-name)
  (pprint (generate-defgeneric (fdefinition gf-name)))
  (values))

;;; Finding All Generic Functions

(defun all-generic-functions ()
  (remove-duplicates
   (mapappend #'class-direct-generic-functions
	      (subclasses* (find-class 't)))))

(defun class-direct-generic-functions (class)
  (remove-duplicates
   (mapcar #'method-generic-function
	   (specializer-direct-methods class))))


;;; Finding Relevant Generic Functions

(defun relevant-generic-functions (class ceiling &key elide-accessors-p)
  (remove-duplicates
   (mapcar #'method-generic-function
	   (remove-if
	    #'(lambda (m)
		(and elide-accessors-p
		     (or (reader-method-p m)
			 (writer-method-p m))))
	    (mapappend #'specializer-direct-methods
		       (set-difference (class-precedence-list1 class)
				       (class-precedence-list1 ceiling)))))))

(defun display-effective-method (gf args)
  (let ((applicable-methods
	 (compute-applicable-methods-using-classes
	  gf (mapcar #'class-of (required-portion gf args)))))
    (pprint
     (if (null applicable-methods)
	 '(error "No applicable methods.")
       (generate-effective-method gf applicable-methods)))))

(defun generate-effective-method (gf methods)
  (declare (ignore gf))
  (labels ((generate-method (method)
	     `(method ,@(cdr (generate-defmethod
			      method :show-body t))))
	   (generate-call-method (method next-methods)
	     `(call-method
	       ,(generate-method method)
	       ,(mapcar #'generate-method next-methods))))
    (let ((primaries (remove-if-not #'primary-method-p methods))
	  (befores (remove-if-not #'before-method-p methods))
	  (afters (remove-if-not #'after-method-p methods)))
      (if (null primaries)
	  '(error "No primary method")
	`(progn
	   ,@(mapcar
	      #'(lambda (method)
		  (generate-call-method method ()))
	      befores)
	   (multiple-value-prog1
	       ,(generate-call-method (car primaries)
				      (cdr primaries))
	     ,@(mapcar
		#'(lambda (method)
		    (generate-call-method method ()))
		(reverse afters))))))))


;;; Finding All Slot Accessors

(defun reader-method-p (method)
  (let ((specializers (method-specializers method)))
    (and (= (length specializers) 1)
	 (member (generic-function-name (method-generic-function method))
		 (mapappend #'slot-definition-readers
			    (class-direct-slots (car specializers)))
		 :test #'equal))))

(defun writer-method-p (method)
  (let ((specializers (method-specializers method)))
    (and (= (length specializers) 2)
	 (member (generic-function-name (method-generic-function method))
		 (mapappend #'slot-definition-writers
			    (class-direct-slots (cadr specializers)))
		 :test #'equal))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;             PROGRAMMATIC CREATION OF NEW CLASSES               ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-programmatic-instance (superclass-names &rest initargs)
  (apply #'make-instance
	 (find-programmatic-class
	  (mapcar #'find-class superclass-names))
	 initargs))

(defun find-programmatic-class (superclasses)
  (let ((class (find-if
		#'(lambda (class)
		    (equal superclasses
			   (class-direct-superclasses class)))
		(class-direct-subclasses (car superclasses)))))
    (if class
	class
      (make-programmatic-class superclasses))))

(defun make-programmatic-class (superclasses)
  (make-instance 'standard-class
    :name (mapcar #'class-name superclasses)
    :direct-superclasses superclasses
    :direct-slots ()))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                CLASS PRECEDENCE LISTS                          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass flavors-class (standard-class) ())
(defclass loops-class (standard-class) ())
(defmethod compute-class-precedence-list ((class flavors-class))
  (append (remove-duplicates
	   (depth-first-preorder-superclasses* class)
	   :from-end t)
	  (list (find-class 'standard-object)
		(find-class 't))))

;;; Needed in CLOS????
(defmethod compute-class-precedence-list ((class loops-class))
  (append (remove-duplicates
	   (depth-first-preorder-superclasses* class)
	   :from-end nil)
	  (list (find-class 'standard-object)
		(find-class 't))))

(defun depth-first-preorder-superclasses* (class)
  (if (eq class (find-class 'standard-object))
      ()
    (cons class (mapappend #'depth-first-preorder-superclasses*
			   (class-direct-superclasses class)))))


(defclass vanilla-flavor () ())
;;; This is better handled by using default initialization arguments!
(defmethod initialize-instance :around ((class flavors-class)
					&rest all-keys
					&key direct-superclasses)
  (apply #'call-next-method
	 class
	 :direct-superclasses (or direct-superclasses
				  (list (find-class 'vanilla-flavor)))
	 all-keys))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                  SLOT INHERITANCE                              ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Slot Attributes
(defclass attributes-class (standard-class) ())

(defclass attributes-slot-definition (slot-definition)
  ((attributes :initarg :attributes :initform nil :accessor slot-definition-attributes)))
(defclass attributes-direct-slot-definition
  (attributes-slot-definition standard-direct-slot-definition) ())
(defclass attributes-effective-slot-definition
  (attributes-slot-definition standard-effective-slot-definition) ())

(defmethod direct-slot-definition-class ((class attributes-class) &rest initargs)
  (declare (ignore initargs) (dynamic-extent initargs))
  (find-class 'attributes-direct-slot-definition))
(defmethod effective-slot-definition-class ((class attributes-class) &rest initargs)
  (declare (ignore initargs) (dynamic-extent initargs))
  (find-class 'attributes-effective-slot-definition))

(defmethod compute-effective-slot-definition ((class attributes-class) name direct-slots)
  (declare (ignore name))
  (let ((normal-slot (call-next-method)))
    (setf (slot-definition-attributes normal-slot)
      (remove-duplicates
       (mapappend #'slot-definition-attributes direct-slots)))
    normal-slot))

(defmethod compute-slots ((class attributes-class))
  (let* ((normal-slots (call-next-method))
	 (alist (mapcar		
		 #'(lambda (slot)
		     (cons (slot-definition-name slot)
			   (mapcar #'(lambda (attr) (cons attr nil))
				   (slot-definition-attributes slot))))
		 normal-slots)))
    (cons (make-instance 'standard-effective-slot-definition
			 :name 'all-attributes ; Should really use a gensym!
			 :initform `',alist
			 :initfunction #'(lambda () alist))
	  normal-slots)))

(defun slot-attribute (instance slot-name attribute)
  (cdr (slot-attribute-bucket instance slot-name attribute)))
(defun (setf slot-attribute) (new-value instance slot-name attribute)
  (setf (cdr (slot-attribute-bucket instance slot-name attribute))
    new-value))
(defun slot-attribute-bucket (instance slot-name attribute)
  (let* ((all-buckets (slot-value instance 'all-attributes))
	 ;; 'all-attributes should really be a gensym!
	 (slot-bucket (assoc slot-name all-buckets)))
    (unless slot-bucket
      (error "The slot named ~s of ~s has no attributes."
	     slot-name instance))
    (let ((attr-bucket (assoc attribute (cdr slot-bucket))))
      (unless attr-bucket
	(error "The slot named ~s of ~s has no attribute named ~s."
	       slot-name instance attribute))
      attr-bucket)))

;;; Slot Encapsulation
(defclass encapsulated-class (standard-class) ())
(defclass encapsulated-slot-definition (slot-definition)
  ((pretty-name :initarg :pretty-name :accessor slot-definition-pretty-name)))
(defclass encapsulated-direct-slot-definition
  (encapsulated-slot-definition standard-direct-slot-definition) ())

(defmethod direct-slot-definition-class ((class encapsulated-class) &rest initargs)
  (declare (ignore initargs) (dynamic-extent initargs))
  (find-class 'encapsulated-direct-slot-definition))

;; This doesn't work with class redefinitions.  We would need to preserve the
;; previous unique name for each slot!

(defun map-slots (direct-slots)
  (mapcar #'(lambda (slot-properties)
	      (let ((pretty-name (getf slot-properties ':name))
		    (new-properties (copy-list slot-properties)))
		(setf (getf new-properties ':name) (gensym))
		(setf (getf new-properties ':pretty-name) pretty-name)
		new-properties))
	  direct-slots))

(defmethod initialize-instance :around
	   ((class encapsulated-class) &rest all-keys &key direct-slots)
    (apply #'call-next-method class
	   :direct-slots (map-slots direct-slots)
	   all-keys))
(defmethod reinitialize-instance :around
	   ((class encapsulated-class) &rest all-keys &key direct-slots)
    (apply #'call-next-method class
	   :direct-slots (map-slots direct-slots)
	   all-keys))

(defun private-slot-value (instance slot-name class)
  (slot-value instance (private-slot-name slot-name class)))

(defun private-slot-name (slot-name class)
  (let ((slot (find slot-name (class-direct-slots class)
		    :key #'slot-definition-pretty-name)))
    (if slot
	(slot-definition-name slot)
      (error "The class ~s has no private slot named ~s."
	     class slot-name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                SLOT ACCESS                                     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Monitoring Slot Access

(defclass monitored-class (standard-class) ())

(defmethod slot-value-using-class :before
	   ((class monitored-class) instance slot)
  (note-operation instance slot 'slot-value))

(defmethod (setf slot-value-using-class) :before
	   (new-value (class monitored-class) instance slot)
  (declare (ignore new-value))
  (note-operation instance slot 'set-slot-value))

(defmethod slot-boundp-using-class :before
	   ((class monitored-class) instance slot)
  (note-operation instance slot 'slot-boundp))

(defmethod slot-makunbound-using-class :before
	   ((class monitored-class) instance slot)
  (note-operation instance slot 'slot-makunbound))

(let ((history-list ()))
  (defun note-operation (instance slot operation)
    (push `(,operation ,instance ,(slot-definition-name slot)) history-list)
    (values))
  (defun reset-slot-access-history ()
    (setq history-list ())
    (values))
  (defun slot-access-history ()
    (reverse history-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                     INSTANCE ALLOCATION                        ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass dynamic-slot-class (standard-class) ())

(defclass dynamic-slot-definition (slot-definition)
  ((allocation :initarg :dynamic)))
(defclass dynamic-direct-slot-definition
  (dynamic-slot-definition standard-direct-slot-definition) ())
(defclass dynamic-effective-slot-definition
  (dynamic-slot-definition standard-effective-slot-definition) ())

(defmethod direct-slot-definition-class ((class dynamic-slots-class) &rest initargs)
  (declare (ignore initargs) (dynamic-extent initargs))
  (find-class 'dynamic-direct-slot-definition))
(defmethod effective-slot-definition-class ((class dynamic-slots-class) &rest initargs)
  (declare (ignore initargs) (dynamic-extent initargs))
  (find-class 'dynamic-effective-slot-definition))

(defun dynamic-slot-p (slot)
  (eq (slot-definition-allocation slot) ':dynamic))

(defmethod allocate-instance ((class dynamic-slot-class) &key)
  (let ((instance (call-next-method)))
    (when (some #'dynamic-slot-p (class-slots class))
      (allocate-table-entry instance))
    instance))

(defmethod slot-value-using-class ((class dynamic-slot-class) instance slot)
  (if (and slot (dynamic-slot-p slot))
      (read-dynamic-slot-value instance (slot-definition-name slot))
      (call-next-method)))

(defmethod (setf slot-value-using-class) (new-value (class dynamic-slot-class) instance slot)
  (if (and slot (dynamic-slot-p slot))
      (write-dynamic-slot-value new-value instance (slot-definition-name slot))
      (call-next-method)))

(defmethod slot-boundp-using-class ((class dynamic-slot-class) instance slot)
  (if (and slot (dynamic-slot-p slot))
      (dynamic-slot-boundp instance (slot-definition-name slot))
      (call-next-method)))

(defmethod slot-makunbound-using-class ((class dynamic-slot-class) instance slot)
  (if (and slot (dynamic-slot-p slot))
      (dynamic-slot-makunbound instance (slot-definition-name slot))
      (call-next-method)))

;;; This implementation doesn't allow the garbage collection of instances
;;; since the table will contain pointers to all such instances.
(let ((table (cl:make-hash-table :test #'eq)))
  (defun allocate-table-entry (instance)
    (setf (cl:gethash instance table) ()))
  (defun read-dynamic-slot-value (instance slot-name)
    (let* ((alist (cl:gethash instance table))
	   (entry (assoc slot-name alist)))
      (if (null entry)
	  (error "The slot ~s is unbound in the object ~s."
		 slot-name instance))
      (cdr entry)))
  (defun write-dynamic-slot-value (new-value instance slot-name)
    (let* ((alist (cl:gethash instance table))
	   (entry (assoc slot-name alist)))
      (if (null entry)
	  (push `(,slot-name . ,new-value)
		(cl:gethash instance table))
	(setf (cdr entry) new-value))
      new-value))
  (defun dynamic-slot-boundp (instance slot-name)
    (let* ((alist (cl:gethash instance table))
	   (entry (assoc slot-name alist)))
      (not (null entry))))
  (defun dynamic-slot-makunbound (instance slot-name)
    (let* ((alist (cl:gethash instance table))
	   (entry (assoc slot-name alist)))
      (unless (null entry)
	(setf (cl:gethash instance table)
	  (delete entry alist))))
    instance))

;;; Class Slot Allocation
;;; This still does not support the CLOS behavior for change-class and
;;; update-instance-for-different-class!  In particular, the behavior when
;;; the allocation of a slot changes is wrong.

(defclass class-slot-class (standard-class)
  ((class-allocated-slots
    :initform ()
    :accessor class-allocated-slots)))

(defun class-slot-p (slot)
  (eq (slot-definition-allocation slot) ':xclass))

(defmethod shared-initialize :after ((class class-slot-class) slot-names &key)
  (declare (ignore slot-names))
  (setf (class-allocated-slots class)
    (mapcar 
     #'(lambda (slot)
	 (let ((initfunction (slot-definition-initfunction slot)))
	   (cons (slot-definition-name slot)
		 (if (not (null initfunction))
		     (funcall initfunction)
		     (unbound)))))
     (remove-if-not #'class-slot-p
		    (class-direct-slots class)))))

(defmethod slot-value-using-class ((class class-slot-class) instance slot)
  (declare (ignore instance))
  (if (and slot (class-slot-p slot))
      (class-slot-value class (slot-definition-name slot))
      (call-next-method)))

(defmethod (setf slot-value-using-class) (new-value (class class-slot-class) instance slot)
  (declare (ignore instance))
  (if (and slot (class-slot-p slot))
      (setf (class-slot-value class (slot-definition-name slot)) new-value)
      (call-next-method)))

(defmethod slot-boundp-using-class ((class class-slot-class) instance slot)
  (declare (ignore instance))
  (if (and slot (class-slot-p slot))
      (class-slot-boundp class (slot-definition-name slot))
      (call-next-method)))

(defmethod slot-makunbound-using-class ((class class-slot-class) instance slot)
  (declare (ignore instance))
  (if (and slot (class-slot-p slot))
      (class-slot-makunbound class (slot-definition-name slot))
      (call-next-method)))


(defun class-slot-value (class slot-name)
  (dolist (super (class-precedence-list class))
    (let ((slot (find slot-name
		      (class-direct-slots super)
		      :key #'slot-definition-name)))
      (when slot
	(let ((value (cdr (assoc slot-name
				 (class-allocated-slots super)))))
	  (when (eq value (unbound))
	    (error "The class slot ~s is unbound in the class ~s."
		   slot-name class))
	  (return-from class-slot-value value))))))

(defun (setf class-slot-value) (new-value class slot-name)
  (dolist (super (class-precedence-list class))
    (let ((slot (find slot-name
		      (class-direct-slots super)
		      :key #'slot-definition-name)))
      (when slot
	(setf (cdr (assoc slot-name (class-allocated-slots super)))
	  new-value)
	;; Note bad syntax here.  Allegro doesn't handle (return-from (setf
	;; foo) ...)
	(return-from class-slot-value new-value)))))

(defun class-slot-boundp (class slot-name)
  (dolist (super (class-precedence-list class))
    (let ((slot (find slot-name
		      (class-direct-slots super)
		      :key #'slot-definition-name)))
      (when slot
	(let ((value (cdr (assoc slot-name
				 (class-allocated-slots super)))))
	  (return-from class-slot-boundp
	    (not (eq value (unbound)))))))))

(defun class-slot-makunbound (class slot-name)
  (dolist (super (class-precedence-list class))
    (let ((slot (find slot-name
		      (class-direct-slots super)
		      :key #'slot-definition-name)))
      (when slot
	(setf (cdr (assoc slot-name (class-allocated-slots super)))
	      (unbound))
	(return-from class-slot-makunbound class)))))

