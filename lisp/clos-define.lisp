#| *****************************************************************
Definitions needed to define CLOS objects at run time.
***************************************************************** |#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                ERROR CHECKING                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun illegal-forward-reference (metaobject &optional dependent)
  (signal-program-error "~s~@[ (used by ~s)~] has not been defined."
			metaobject dependent))

(defun illegal-superclass (class super &optional name namep)
  (error "~:[S~;Abstract s~]uperclass ~s is not compatible with ~:[~;abstract ~]~s (metaclass ~s)."
	 (abstract-class-p super) super
	 (abstract-class-p class)
	 (if namep name (when (slot-boundp class 'name)
			  (class-name class)))
	 (class-of class)))

(defun check-congruent-lambda (gf method)
  (unless (congruent-lambda-p (generic-function-lambda-list gf)
			      (method-lambda-list method))
    (signal-program-error
     "~a method parameters ~s are not congruent to ~s."
     (format-method nil method (generic-function-name gf))
     (method-lambda-list method) (generic-function-lambda-list gf))))

(defun check-for-forward-references (class cpl)
  (let ((forward (find-if 'forward-referenced-class-p cpl)))
    (when forward (illegal-forward-reference forward class))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;              PREDICATES                                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun forward-referenced-class-p (class)
  (classp class forward-referenced-class-classobj))

(defun generic-function-p (x)
  (classp x generic-function-classobj))

(defun congruent-lambda-p (gf method)
  (do ((gf gf (cdr gf))
       (m method (cdr m)))
      ((endp gf) (or (null m) (eq (car m) '&aux)))
    (unless m (return nil))
    (case (car gf)
      (&optional (unless (eq (car m) '&optional)
		   (return nil)))
      ((&rest &key)
       (return (case (car m)
		 ((&rest &key)
		  (let* ((m-keys (member '&key m))
			 ;; POSITION is defined using CLOS
			 (m-end (loop for i from 0
				      and parameter in m
				      when (eq parameter '&aux)
				      return i)))
		    (cond ((find-list-eq-end '&allow-other-keys m-keys m-end)
			   t)
			  ((and (null m-keys) (eq (car m) '&rest))
			   t)
			  (t
			   (dolist (gfk (rest (member '&key gf)) t)
			     (when (eq gfk '&allow-other-keys)
			       (return t))
			     (unless (find-list-eq-end (bindingform-keyword gfk) m-keys
						       m-end #'bindingform-keyword) 
			       (return nil)))))))
		 (t nil))))
      (&aux (return t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;              ENSURE ...                                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ENSURE-CLASS (name &rest keys)
  (declare (dynamic-extent keys))
  (apply #'ensure-class-using-class
	 (find-type name nil) name keys))

(defun ENSURE-GENERIC-FUNCTION (function-name &rest keys)
  (declare (dynamic-extent keys))
  (apply #'ensure-generic-function-using-class
	 (when (fboundp function-name)
	   (let ((function (fdefinition function-name)))
	     (cond ((generic-function-p function) function)
		   (t (cerror "Discard old definition of ~s~*."
			      "~s names the non-generic-function ~s."
			      function-name function)
		      (fmakunbound function-name)
		      nil))))
	 function-name keys))

(defun ensure-method-using-class (gf requested-method-class keys)
  (let ((method (apply #'make-instance requested-method-class keys)))
    (add-method gf method)
    method))

(defun ensure-method (gf &rest keys)
  (declare (dynamic-extent keys))
  (ensure-method-using-class gf (generic-function-method-class gf) keys))

;;; Neither ANSI nor the MOP define behavior when method combination
;;; is redefined.  
(defun ensure-method-combination-type (name &rest keys)
  (let ((current (find-method-combination-type name nil)))
    (if current
	(apply #'reinitialize-instance current :name name keys)
	(setf (find-method-combination-type name)
	      (apply #'make-instance 'method-combination-type
		     :name name keys))))
  name)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;              CLASS DEFINITION                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-direct-slot-definition (class spec)
  (apply #'make-instance
	 (apply #'direct-slot-definition-class
		class spec)
	 spec))

(defun make-effective-slot-definition (class spec)
  (apply #'make-instance
	 (apply #'effective-slot-definition-class class spec)
	 spec))

;;; IWBNI the first specializer for writer-methods was
;;; (base-type (slot-definition-type slot)) rather than t.
;;; If/when all types are allowed as specializers, it should be
;;; (intern-type (slot-definition-type slot)).
(defun accessor-specializers (class key)
  (ecase key
    (:readers (list class))
    (:writers (list (find-type 't) class))))

(defun add-accessor-methods (class slot key accessors)
  (when accessors
    (let ((class-function (ecase key
			    (:writers #'writer-method-class)
			    (:readers #'reader-method-class)))
	  (plist `(:slot-definition ,slot
		   :specializers ,(accessor-specializers class key))))
      (dolist (accessor accessors)
	(ensure-method-using-class
	 (ensure-generic-function accessor)
	 (apply class-function class slot plist)
	 plist)))))


(defun remove-accessor-methods (class slot key)
  (loop for existing-accessor in (ecase key
				   (:readers (slot-definition-readers slot))
				   (:writers (slot-definition-writers slot)))
	with specializers = (accessor-specializers class key)
	do (let ((gf (fdefinition existing-accessor)))
	     (remove-method gf (get-method gf nil specializers t)))))


(defun standard-change-class (instance new-class initargs)
  (let ((copy (allocate-instance new-class))
	(old-class (class-of instance)))
    (declare (dynamic-extent copy))
    (when (sealed-class-p old-class)
      (illegal-change-class instance new-class))
    (loop for new-slot in (class-slots new-class)
	  with old-slots = (class-slots old-class)
	  when (local-slot-p new-slot)
	  do (let* ((slot-name (slot-definition-name new-slot))
		    (old-slot (find slot-name old-slots
				    :key #'slot-definition-name)))
	       (when (and old-slot
			  (slot-boundp-using-class old-class instance
						   old-slot)) 
		 (setf (slot-value-using-class new-class copy
					       new-slot) 
		       (slot-value-using-class old-class instance
					       old-slot)))))
    (macrolet ((rotate (get set type)
		       `(let ((temp (,get copy)))
			  (declare (type ,type temp))
			  (,set copy (,get instance))
			  (,set instance temp))))
      (rotate tagged-instance-class
	      set-tagged-instance-class
	      t)
      (rotate tagged-instance-wrapper
	      set-tagged-instance-wrapper
	      wrapper)
      (rotate standard-instance-slots
	      set-standard-instance-slots
	      slots))
    (apply #'update-instance-for-different-class
	   copy instance initargs)
    instance))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;              SLOT LOCATIONS                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; We extend the concept of slot location such that the "location" of
;;; a :class allocated slot is a cons of slot-definition-name and
;;; value.  The value can be updated by changing the cdr of this
;;; "location", and the effect will be seen by all classes that share the
;;; slot. 

;;; Each class maintains an (initially empty) list of direct shared
;;; "locations" in class-direct-shared-slots (an alist of name value
;;; pairs).  Compute-class-allocated-slot searches this list in each
;;; member of the class precedence list before creating and adding a new
;;; pair.  This serves two purposes:
;;; 1. We don't create new shared slot "locations" for shared slots
;;;    inherited from superclasses.
;;; 2. We don't create a new "location" (or reinitize an old one) if there
;;;    already exists one for the current class.  This comes about
;;;    because:
;;;    1. class is on the class-precedence-list of class.
;;;    2. reinitize-instance for the class does not reset
;;;       class-direct-shared-slots to nil.
;;;
;;; Note that since we never delete obsolete members of
;;; class-direct-shared-slots, the list contains all the direct shared
;;; slots that have EVER been defined.  The obsolete values can't be
;;; accessed by portable user code, but the values are still there so
;;; that if the same slot is later added again, it will not be
;;; reinitialized.  Issue: It is not clear if this is desirable
;;; behavior.  If not, than the last part of finalize-inheritance
;;; could delete any entries from direct-shared-slots that aren't in
;;; direct-slots.
;;;
;;; Note that the class t can legally be in our
;;; class-precedences-list, even though it is a built-in-class and
;;; therefore does not handle the class-direct-shared-slots reader.

(defun compute-class-allocated-slot (class slot)
  (let ((name (slot-definition-name slot)))
    (or (loop for super in (class-precedence-list class)
	      thereis (unless (eq super t-classobj)
			(assoc name (class-direct-shared-slots super))))
	(let* ((func (slot-definition-initfunction slot))
	       (new (cons name (if func
				   (funcall func)
				   (symbol-value-value 'unbound-flag)))))
	  (push new (class-direct-shared-slots class))
	  new))))

(defun compute-slot-locations (class effective-slot-definitions)
  (loop for definition in effective-slot-definitions
	with position = 0
	for location = (case (slot-definition-allocation definition) 
			 (:instance (let ((pos position))
				      (setq position (1+ pos))
				      pos))
			 (:class (compute-class-allocated-slot
				  class definition)))
	when location
	do (setf (slot-definition-location definition) location))
  effective-slot-definitions)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;              FUNCALLABLE INSTANCES                           ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun SET-FUNCALLABLE-INSTANCE-FUNCTION (funcallable-instance function)
  (set-funcallable-standard-instance-function
   funcallable-instance function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                 INTERNED INSTANCES                           ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun intern-instance (class &rest keys &key (key keys keyp) &allow-other-keys)
  (let* ((class (canonicalize-class class t))
	 (table (class-instances class)))
    (or (gethash key table)
	(progn (when keyp (setq keys (copy-list keys))
		     (remf keys :key))
	       (setf (gethash key table)
		     (apply #'make-instance class keys))))))

;;; We might want to define an interned-standard-class metaclass.
(defun INTERN-EQL-SPECIALIZER (object)
  (intern-instance 'eql-specializer :object object
		   :name `(eclipse:eql ,object)
		   :key object))

(defun intern-method-combination (&rest args)
  (let* ((name (car args))
	 (class (find-method-combination-type name))
	 (table (class-instances class)))
    (or (gethash args table)
	(let ((options (rest args)))
	  (setf (gethash args table)
		(make-instance
		 class
		 :function (apply (method-combination-type-function
				   class)
				  options)
		 :options options))))))
	       
;;; method-combination-type registry
(defun find-method-combination-type (name &optional (errorp t))
  (or (system-property name 'method-combination-types nil)
      (when errorp (illegal-forward-reference name))))
(defun (setf find-method-combination-type) (class name
						  &optional errorp)
  (declare (ignore errorp))
  (system-property-setter name 'method-combination-types class))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;          ACCESSORS AND OTHER EXTRACTION                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Some of these might arguably belong to clos-compile.lisp

(defun EXTRACT-SPECIALIZER-NAMES (specialized-lambda-list)
  (loop for parameter in specialized-lambda-list
	when (lambda-list-keyword-p parameter)
	do (return required)
	collect (if (consp parameter) (second parameter) 't)
	into required
	finally (return required)))

(defun EXTRACT-LAMBDA-LIST (specialized-lambda-list)
  (loop for parameters on specialized-lambda-list
	for parameter = (car parameters)
	when (lambda-list-keyword-p parameter)
	do (return (nconc required parameters))
	collect (bindingform-name parameter) into required
	finally (return required)))

(defun extract-generic-function-lambda-list (lambda-list)
  (loop for parameter in lambda-list
	with bailp = nil
	when bailp return list
	do (case parameter
	     ((&aux &allow-other-keys) (return list))
	     (&key (setq bailp t)))
	collect (bindingform-name parameter) into list
	finally (return list)))

;; Note that x ((:x a)) have congruent keys, but x ((x a)) do not.
(defun Bindingform-Keyword (key)
  (if (consp key)
      (let ((key (car key)))
	(if (consp key)
	    (car key)
	    (make-keyword key)))
      (make-keyword key)))

(defun Extract-Keywords (lambda-list)
  (let ((collectp nil) (keys nil))
    (dolist (parameter lambda-list keys)
      (cond ((or (eq parameter '&allow-other-keys)
		 (eq parameter '&aux)) (setq collectp nil))
	    (collectp (push (eclipse:bindingform-keyword parameter) keys))
	    ((eq parameter '&key) (setq collectp t))))))



;;; MAINTENANCE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Like find-method, but expects specializers to all be specializer
;;; metaobjects and does not signal an error if the length of
;;; specializers does not agree with the generic-function.

;;; Allowed to signal an error if generic-function is sealed.

(defun get-method (generic-function method-qualifiers specializers
				    errorp)
  (dolist (method (generic-function-methods generic-function)
		  (when errorp
		    (error "No method~@[~{ ~s~}~] ~s on ~s."
			   method-qualifiers specializers generic-function)))
    (when (and (list-eq method-qualifiers
			(method-qualifiers method))
	       (list-eq specializers
			(method-specializers method)))
      (return method))))


;;; Slot names might not be valid initargs, so we can't just pass them
;;; as initargs to initialize-instance.  Furthermore, unbound slots
;;; must not have initforms evaluated. (Even if we evaluate them and
;;; then call slot-makunbound, side-effects in initiforms might
;;; occur.)  If we really want to call initialize-instance, we could
;;; initialize unbound slots with a dummy value, then call
;;; initialize-instance, and then slot-makunbound, but it is not clear
;;; how to documunent references in an initialize-instance method to
;;; initargs or the dummy valued slots.  It is better to just let
;;; users call initialize-intstance explicitly if they need to.

;;; Although it isn't discussed in ANSI, we also don't initialize
;;; non-local slots, because this could have unintended load-order
;;; consequences.

(defun MAKE-LOAD-FORM-SAVING-SLOTS (object &key (slot-names nil namesp)
					   environment)
  (declare (ignore environment))
  (let* ((class (class-of object))
	 (names nil)
	 (forms (loop for slot in (class-slots class)
		      for name = (slot-definition-name slot)
		      when (and (or (null namesp)
				    (member name slot-names))
				(local-slot-p slot)
				(slot-boundp-using-class
				 class object slot))
		      collect `(setf ,name ',(slot-value-using-class
					      class object slot))
		      and do (push name names))))
    (values
     `(allocate-instance ',class)
     `(with-slots ,names ',object ,@forms))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                  CANONICALIZATION                            ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun canonicalize-class (class-or-name &optional (errorp t))
  (if (symbolp class-or-name)
      (find-type class-or-name errorp)
      class-or-name))

(defun make-forward-referenced-class (name)
  (ensure-class-using-class
   nil name :metaclass 'forward-referenced-class))

;;; Should this really accept specializer metaobjects as well as names?
(defun canonicalize-specializer (specializer-or-name)
  (if (symbolp specializer-or-name)
      (or (find-type specializer-or-name nil)
	  (make-forward-referenced-class specializer-or-name))
      specializer-or-name))

;;; These two convert keyword initargs presented to ensure-xxx to
;;; those suitable for initialization methods.

;;; Note that the use of ensure-class to create a
;;; forward-referenced-class makes sure that it has a proper-name.
;;; Then, when the class is later defined (using defclass,
;;; ensure-class, or change-class), the class will be properly updated.

(defun canonicalize-ensure-class-args (keys)
  (setq keys (copy-list keys))
  (remf keys :metaclass)
  (let ((direct-superclasses (getf keys :direct-superclasses 'not-found)))
    (unless (eq direct-superclasses 'not-found)
      (setf (getf keys :direct-superclasses)
	    (loop for super in direct-superclasses
		  collect (or (canonicalize-class super nil)
			      (make-forward-referenced-class super))))))
  keys)

(defun canonicalize-ensure-generic-function-args (keys)
  (setq keys (copy-list keys))
  (remf keys :generic-function-class)
  (remf keys :environment) ;ANSI allows this for ENSURE-GENERIC-FUNCTION
  (let ((requested-method-class (getf keys :method-class 'not-found)))
    (unless (eq requested-method-class 'not-found)
      (setf (getf keys :method-class)
	    (canonicalize-class requested-method-class t))))
  keys)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         CLASS PRECEDENCE LIST AND OTHER SORTING              ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This version of collect-superclasses* isn't bothered by cycles in the
;;; class hierarchy, which sometimes happens by accident.
(defun collect-superclasses* (class)
  (labels ((all-superclasses-loop (seen superclasses)
	     (let ((class-to-process
		    (dolist (super superclasses nil)
		      (unless (find-list-eq super seen)
			(return super)))))
	       (if class-to-process
		   (all-superclasses-loop
		    (cons class-to-process seen)
		    (dolist (super (class-direct-superclasses
				    class-to-process)
				   superclasses)
		      (pushnew super superclasses)))
		 superclasses))))
    (all-superclasses-loop nil (list class))))

;;; Topological-sort implements the standard algorithm for topologically
;;; sorting an arbitrary set of elements while honoring the precedence
;;; constraints given by a set of (X,Y) pairs that indicate that element X
;;; must precede element Y.  The tie-breaker procedure is called when it is
;;; necessary to choose from multiple minimal elements; both a list of
;;; candidates and the ordering so far are provided as arguments.

(defun topological-sort (elements constraints tie-breaker)
  (let ((remaining-constraints constraints)
	(remaining-elements elements)
	(result ()))
    (loop
      (let ((minimal-elements
	     (remove-if
	      #'(lambda (class)
		  (member class remaining-constraints
			  :key #'cadr))
	      remaining-elements)))
	(when (null minimal-elements)
	  (if (null remaining-elements)
	      (return-from topological-sort result)
	    (error "Inconsistent precedence graph.")))
	(let ((choice (if (null (cdr minimal-elements))
			  (car minimal-elements)
			(funcall tie-breaker
				 minimal-elements
				 result))))
	  (setq result (nconc result (list choice)))
	  (setq remaining-elements
	    (delete choice remaining-elements))
	  (setq remaining-constraints
	    (delete choice remaining-constraints
		    :test #'member)))))))

;;; In the event of a tie while topologically sorting class precedence
;;; lists, the CLOS Specification says to "select the one that has a direct
;;; subclass rightmost in the class precedence list computed so far."  The
;;; same result is obtained by inspecting the partially constructed class
;;; precedence list from right to left, looking for the first minimal
;;; element to show up among the direct superclasses of the class precedence
;;; list constituent.  (Kiczales, et. al. say that "There's a lemma that
;;; show that this rule yields a unique result.")

(defun std-tie-breaker-rule (minimal-elements cpl-so-far)
  (dolist (cpl-constituent (reverse cpl-so-far)) ;Cons alert!
    (dolist (element minimal-elements)
      (when (find-list-eq element (class-direct-superclasses cpl-constituent))
	(return-from std-tie-breaker-rule element)))))

;;; The local precedence ordering of a class C with direct superclasses C_1,
;;; C_2, .., C_n is the set ((C C_1) (C_1 C_2) ... (C_n-1 C_n)). 
(defun local-precedence-ordering (class)
  (let ((supers (class-direct-superclasses class)))
    (mapcar #'list			;Mapcar finishes when
	    (cons class supers)		;shorter list runs out.
	    supers)))



