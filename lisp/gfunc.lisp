
(macrolet ((defgeneric (&rest args)
	     `(static-defgeneric ,@args))
	   (defmethod (&rest args)
	     `(static-defmethod ,@args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                   METHOD-COMBINATION                         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgeneric FIND-METHOD-COMBINATION (generic-function
				     method-combination-type-name
				     method-combination-options))   
(defmethod find-method-combination ((generic-function
				     standard-generic-function)
				    (method-combination-type-name
				     symbol)
				    method-combination-options)
  (intern-method-combination (cons method-combination-type-name
				   method-combination-options)))


;;; This method may be overridden.
(defgeneric COMPUTE-EFFECTIVE-METHOD (generic-function
				      method-combination methods))
(defmethod compute-effective-method ((generic-function standard-generic-function)
				     *method-combination* methods)
  (let ((*generic-function* generic-function))
    (funcall (method-combination-function *method-combination*) methods)))

;;; The normal reinitialize-instance method for classes is careful to
;;; AVOID making instances obsolete if the class-precedence-list and
;;; local slot order remain unchanged.
(defmethod REINITIALIZE-INSTANCE :AFTER ((instance method-combination-type) &key)
  (when (class-finalized-p instance)
    (make-instances-obsolete instance)))

(defmethod UPDATE-INSTANCE-FOR-REDEFINED-CLASS :AFTER
    ((method-combination method-combination) added-slots discarded-slots
     property-list &rest initargs)
  (declare (ignore added-slots discarded-slots property-list initargs)
	   (dynamic-extent initargs))
  (with-slots (function options) method-combination
    (setf function
	  (apply (method-combination-type-function
		  (class-of method-combination))
		 options))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                  GENERIC FUNCTIONS                            ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SPECIALIZED ACCESSORS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric (SETF GENERIC-FUNCTION-NAME) (new-name gf))
(defmethod (SETF GENERIC-FUNCTION-NAME) (new-name gf) 
  (reinitialize-instance gf :name new-name))	;As per MOP.

;;; MAINTENANCE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgeneric add-attached-methods (generic-function methods))
(defmethod add-attached-methods ((standard-generic-function standard-generic-function)
				 methods)
  (with-slots (attached-methods) standard-generic-function
    (let ((attached attached-methods))
      (dolist (method methods)
	(add-method standard-generic-function method)
	(push method attached))
      (setf attached-methods attached))))

(defgeneric remove-attached-methods (generic-function))
(defmethod remove-attached-methods ((standard-generic-function
				     standard-generic-function))
  (with-slots (attached-methods) standard-generic-function
    (dolist (method attached-methods)
      (remove-method standard-generic-function method))
    (setf attached-methods nil)))
      

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;       INITIALIZATION OF GENERIC FUNCTION METAOBJECTS         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric ENSURE-GENERIC-FUNCTION-USING-CLASS
  (generic-function function-name &key argument-precedence-order
		    declarations documentation generic-function-class
		    lambda-list method-class method-combination 
		    &allow-other-keys))

(defmethod ensure-generic-function-using-class
  ((generic-function null) function-name
   &rest keys
   ;; Avoid name conflict with special var used during bootstrapping.
   &key ((:generic-function-class gf-class) 'standard-generic-function)
   &allow-other-keys)
  (declare (dynamic-extent keys))
  (setf (fdefinition function-name)
	(apply #'make-instance
	       (canonicalize-class gf-class t)
	       :name function-name
	       (canonicalize-ensure-generic-function-args keys))))

;;; This method may be overridden.
(defmethod ensure-generic-function-using-class
  ((generic-function generic-function) function-name
   &rest keys
   ;; Avoid name conflict with special var used during bootstrapping.
   &key ((:generic-function-class gf-class) (class-of generic-function)) 
   &allow-other-keys )
  (declare (dynamic-extent keys))
  (check-metaclass generic-function gf-class)
  (apply #'reinitialize-instance generic-function
	 :name function-name
	 (canonicalize-ensure-generic-function-args keys)))

;;; 1. Defaults value for/checks :argument-precedence-order.
;;; 2. Canonicalizes :method-combination.  This excedes the MOP (but
;;; follows standard practice) by canonicalizing symbol and list form
;;; method-combination specifications.
;;; 3. Sets required-parameters.
;;; Is this promoted too high?  i.e. should be on standard-generic-function?
(defmethod SHARED-INITIALIZE ((gf generic-function) slot-names
			      &rest initargs 
			      &key (lambda-list nil lambda-list-p)
			      (argument-precedence-order
			       nil argument-precedence-order-p)
			      (method-combination nil method-combination-p))
  (declare (dynamic-extent initargs))
  (cond (lambda-list-p
	 (set-gf-lambda-list gf lambda-list argument-precedence-order))
	(argument-precedence-order-p (missing-args :lambda-list)))
  (when (and method-combination-p
	     (or (consp method-combination)
		 (symbolp method-combination)))
    (let ((name (bindingform-name method-combination))
	  (options (when (consp method-combination)
		     (rest method-combination))))
      (setq initargs `(:method-combination
		       ,(find-method-combination gf name options)
		       ,@initargs))))
  (apply #'call-next-method gf slot-names initargs)
  ;; We're assuming that it is invalid to use a generic-function until
  ;; the lambda-list has been initialized either after or concurrent
  ;; with any other initializations.
  (when lambda-list-p (finalize-generic-function gf))
  gf)


(defmethod REINITIALIZE-INSTANCE :BEFORE ((gf generic-function) &key)
  (remove-attached-methods gf))
  
(defmethod REINITIALIZE-INSTANCE :AFTER ((gf generic-function)
					 &rest initargs) 
  (declare (dynamic-extent initargs))
  (map-dependents gf
		  #'(lambda (dep)
		      (apply #'update-dependent gf dep initargs))))

(defgeneric flush-method-cache (generic-function))
(defmethod flush-method-cache ((generic-function standard-generic-function))
  (clear-emf-table (emf-table generic-function))
  nil)
(defmethod flush-method-cache ((generic-function standard-system-generic-function))
  (clear-emf-table (emf-table generic-function))
  (initialize-critical-generics))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;       GENERIC FUNCTION INVOCATION PROTOCOL                   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; IWBNI these provided restarts.

(defgeneric NO-APPLICABLE-METHOD (generic-function &rest function-arguments))
(defmethod no-applicable-method (generic-function &rest function-arguments)
  (error "No applicable method for ~s, args:~{ ~s~}."
	 generic-function function-arguments))

(defgeneric NO-NEXT-METHOD (generic-function method &rest args))
(defmethod no-next-method ((generic-function standard-generic-function)
			   (method standard-method)
			   &rest args)
  (error "No next method for ~s, args ~s." generic-function args))

;; The methods on the following two generic functions may be overridden.

;;; It is a shame that this rebuilds a list of the classes of the
;;; required-arguments, even though the caller had already created one.
(defgeneric COMPUTE-APPLICABLE-METHODS (generic-function arguments)
  (:generic-function-class standard-system-generic-function))
(defmethod compute-applicable-methods ((generic-function standard-generic-function)
				       arguments)
  (let ((applicable-methods nil)
	(classes (loop for arg in arguments
		       ;; parameter used only to terminate loop.
		       and parameter on (generic-function-required-parameters
					 generic-function)
		       collect (class-of arg))))
    (dolist (method (generic-function-methods generic-function))
      (loop for specializer in (method-specializers method)
	    and arg in arguments
	    ;; class used only to terminate loop.
	    and class on classes
	    unless (specializerp arg specializer) do (return)
	    finally (push method applicable-methods)))
    (sort-applicable-methods generic-function applicable-methods classes)))

(defgeneric COMPUTE-APPLICABLE-METHODS-USING-CLASSES (generic-function classes)
  (:generic-function-class standard-system-generic-function))
(defmethod compute-applicable-methods-using-classes ((generic-function
						      standard-generic-function)
						     classes)
  (let ((applicable-methods nil))
    (with-slots (methods) generic-function
      (dolist (method methods)
	;; Check each specializer for EQL and against classes.
	(loop for specializer in (method-specializers method)
	      and class in classes
	      with eql-specializers-p = nil
	      when (eql-specializer-p specializer)
	      ;; 1. Don't return nil, nil yet, might be unapplicable.
	      ;; 2. Can't call call subclassp on eql specs.
	      do (setq eql-specializers-p t
		       specializer (class-of (eql-specializer-object
					      specializer)))
	      unless (subclassp class specializer) do (return)
	      finally			;Made it, its applicable...
	      (if eql-specializers-p	;...but it had EQL-specializers
		  (return-from compute-applicable-methods-using-classes
		    (values nil nil))
		  (return (push method applicable-methods)))))) ;...otherwise add it.
    (values (sort-applicable-methods
	     generic-function applicable-methods classes)
	    t)))

;;; Potential optimization: For each method who's specializers involve
;;; an eql specializer, create a specialization list which is
;;; identical, but uses the class of the eql-specializer-object
;;; instead.  Remove any duplicates.  Unless a user method with this
;;; specializer already exists, create an internal method with this
;;; specializer, which explicitly dispatches based on the eql
;;; specialized argument(s).  Then make sure that
;;; compute-applicable-methods-using-classes returns that method when
;;; appropriate, rather than returning a null second value.

;;; Potential optimization: When an applicable method is an
;;; accessor-method, it is possible to substitute a different
;;; (combined?) method into the effective method.  This is
;;; particularly advantagous to specializations of
;;; standard-accessor-method (such as for primary slots).  It is not
;;; clear whether this should be done in
;;; compute-discriminating-function or something it calls, like the
;;; apply-method macro.  Note though, that we want the optimization to
;;; take effect within compute-standard-effective-method-function as
;;; well.  A subprotocol might be necessary, in order to provide a
;;; general mechanism for optimizing a method call.

;;; This method may be overridden.
(defgeneric COMPUTE-DISCRIMINATING-FUNCTION (generic-function)
  (:generic-function-class standard-system-generic-function))
(defmethod compute-discriminating-function ((generic-function
					     standard-generic-function))
  (standard-discriminating-function
   generic-function
   (length (generic-function-required-parameters generic-function))))

)