(defmacro DEFTYPE (name lambda-list &body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (ensure-deftype
      ',name :function (function
			,(parse-macro name lambda-list body :default ''*)))
     ',name))

(macrolet ((defgeneric (&rest args)
	     `(static-defgeneric ,@args))
	   (defmethod (&rest args)
	     `(static-defmethod ,@args)))


;;;    Notes: 1. We rely on the compiler to avoid generating code for
;;;    FLET function bodies that are ignorable and not actually used.
;;;    2. ANSI says we must check that cnm-args, if not nil, must have
;;;    the same ordered set of applicable methods for the original
;;;    arguments to the generic function.  We don't check this.
;;;    3. ANSI says call-next-method and next-method-p may not be used
;;;    within the lambda-list, but that it is undefined whether they
;;;    are defined in the global environment.  We actually don't have
;;;    them in the global environment, but do have them fbound during
;;;    the lambda-list binding.  This is because the lambda might
;;;    contain free ftype or notinline declarations for these
;;;    functions, so they must be fbound before these declarations are
;;;    introduced.  Since the other user declarations might also
;;;    contain BOUND declarations for the method parameters, the
;;;    declarations must be introduced at the time the method
;;;    parameters are bound.  (We could split up the declarations, but
;;;    that would be hairy.)

;;; This method may be overridden.
(defmethod make-method-lambda ((generic-function
				standard-generic-function)
			       (method standard-method) lambda-expression
			       environment)
  (destructuring-bind (lambda-list &body body)
      (rest lambda-expression)
    (let ((name (generic-function-name generic-function))
	  (args (cl:gensym "ARGS"))
	  (methods (cl:gensym "METHODS")))
      (multiple-value-bind (decl body doc) (find-declarations body nil)
	(values 
	 `(lambda (,args ,methods)
	    (flet ((CALL-NEXT-METHOD (&rest cnm-args)
				     (declare (dynamic-extent cnm-args))
				     #+cmu (declare (notinline car))
				     (let ((next-method (car ,methods)))
				       (if next-method
					   (apply-method next-method
							 (or cnm-args ,args)
							 (rest ,methods))
					   (apply #'no-next-method
						  ,(make-load-form
						    generic-function environment)
						  ,(make-stand-in-method
						     generic-function
						     method
						     environment)
						  cnm-args))))
		   (NEXT-METHOD-P ()
				  (not (null ,methods))))
	      (declare (inline call-next-method next-method-p)
		       (ignorable #'call-next-method #'next-method-p))
	      (destructuring-bind ,(make-method-lambda-list lambda-list)
		  ,args
		(declare ,@decl		;might reference call-next-method, etc.
			 ,@(specializer-declarations
			    lambda-list environment))
		(block ,(if (consp name) (second name) name)
		  ,@body))))
	 `(:declarations ',decl :documentation ,doc))))))

)