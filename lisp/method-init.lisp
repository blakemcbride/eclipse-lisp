(macrolet ((defgeneric (&rest args)
	     `(static-defgeneric ,@args))
	   (defmethod (&rest args)
	     `(static-defmethod ,@args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                   LOAD FORMS                                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgeneric MAKE-LOAD-FORM (object &optional environment))

(defmethod make-load-form (object &optional environment)
  (declare (ignore environment))
  (error "There is no load-form defined for ~s,~@
          so it cannot be used as a compile-file literal." object))

(defmethod make-load-form ((object class) &optional environment)
  `(ensure-class ',(proper-name object t environment)
		 :metaclass ',(class-name-of object)))

(defmethod make-load-form ((object standard-generic-function)
			   &optional environment)
  (declare (ignore environment))
  (let ((name (generic-function-name object)))
    (if name
	`(fdefinition ',name)
	(error "~s is an anonymous generic function." object))))

;;; We check for unintialized methods because they might be used as
;;; literals in the implementation of make-method-lambda.

(defmethod make-load-form ((object standard-method) &optional environment)
  (declare (ignore environment))
  (if (and (slot-boundp object 'generic-function)
	   (slot-boundp object 'qualifiers)
	   (slot-boundp object 'specializers))
      `(find-method ,(method-generic-function object)
		    ',(method-qualifiers object)
		    ',(method-specializers object))
      `(class-prototype (find-class ',(class-name-of object)))))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                   ACCESSORS                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstatic-accessors metaobject)
(defstatic-accessors slot-definition)
(defstatic-accessors direct-slot-definition)
(defstatic-accessors effective-slot-definition)
(defstatic-accessors type)
(defstatic-accessors specializer)
(defstatic-accessors class)
(defstatic-accessors standard-class)
(defstatic-accessors interned-standard-class)
(defstatic-accessors method)
(defstatic-accessors standard-accessor-method)
(defstatic-accessors method-combination-type)
(defstatic-accessors method-combination)
(defstatic-accessors generic-function)
(defstatic-accessors standard-generic-function)
(defstatic-accessors packaged-function)
(defstatic-accessors interpreted-function)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;              INITALIZING CRITICAL EMF TABLES                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun initialize-critical-generics ()
  (macrolet
      ((def-emf (generic-function specializers function)
	 `(emf-table-set (emf-table (fdefinition ',generic-function))
			 ,(length specializers)
			 ,(host::make-specializer-forms1 specializers)
			 #'(lambda (&rest args)
			     (declare (dynamic-extent args))
			     (,function args nil)))))
    (def-emf METHOD-SPECIALIZERS (standard-method)
      method-specializers-method)
    (def-emf METHOD-QUALIFIERS (standard-method)
      method-qualifiers-method)
    (def-emf METHOD-FUNCTION (standard-method)
      method-function-method)

    (def-emf CLASS-PRECEDENCE-LIST (standard-system-class)
      class-precedence-list-class)
    (def-emf CLASS-PRECEDENCE-LIST (funcallable-standard-system-class)
      class-precedence-list-class)

    (def-emf GENERIC-FUNCTION-METHOD-COMBINATION (standard-generic-function)
      generic-function-method-combination-generic-function)
    (def-emf GENERIC-FUNCTION-KEYWORDS (standard-generic-function)
      generic-function-keywords-standard-generic-function)
    (def-emf GENERIC-FUNCTION-ALLOW-OTHER-KEYS-P (standard-generic-function)
      generic-function-allow-other-keys-p-standard-generic-function)

    (def-emf GENERIC-FUNCTION-METHOD-COMBINATION (standard-system-generic-function)
      generic-function-method-combination-generic-function)
    (def-emf GENERIC-FUNCTION-KEYWORDS (standard-system-generic-function)
      generic-function-keywords-standard-generic-function)
    (def-emf GENERIC-FUNCTION-ALLOW-OTHER-KEYS-P (standard-system-generic-function)
      generic-function-allow-other-keys-p-standard-generic-function)
  
    (def-emf COMPUTE-APPLICABLE-METHODS-USING-CLASSES
      (standard-generic-function cons)
      compute-applicable-methods-using-classes-standard-generic-function-t)
    (def-emf COMPUTE-APPLICABLE-METHODS-USING-CLASSES
      (standard-system-generic-function cons)
      compute-applicable-methods-using-classes-standard-generic-function-t)))

(initialize-critical-generics)