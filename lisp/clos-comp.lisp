#| *****************************************************************
Macros and other compile-time definitions for using CLOS.
***************************************************************** |#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                 CONSTRUCTORS                                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun eclipse::make-method-lambda-list (lambda-list)
  (let ((keys (member '&key lambda-list)))
    (if (and keys (not (find '&allow-other-keys keys)))
	(let ((aux (position '&aux lambda-list)))
	  (if aux
	      (append (subseq lambda-list 0 aux)
		      '(&allow-other-keys)
		      (subseq lambda-list aux))
	      (append lambda-list '(&allow-other-keys))))
	lambda-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;          ACCESSORS AND OTHER EXTRACTION                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun eclipse::extract-specialized-names (specialized-lambda-list)
  (loop for parameter in specialized-lambda-list
	when (eclipse:lambda-list-keyword-p parameter)
	do (return required)
	when (consp parameter)
	collect (first parameter)
	into required
	finally (return required)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                      WITH-...                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Potential Optimization: (further optimization for known class
;;; case of WITH-SLOTS, below):
;;; If a slot is declared constant, the value can be cached in a let.
;;; This probably isn't worth the hair, unless we find other
;;; compelling reasons to implement constant slots.

;;; Potential Optimization: for WITH-SLOTS, below:
;;; If the class of instance-form is known at compile-time AND:
;;;   - either
;;;     - the mentioned slot is either primary and the class is not
;;;       redefinable, or 
;;;     - for each of class and all its subclasses, the class is
;;;       sealed and either
;;;       - the class is abstract, or
;;;       - the location is the same as in all other non-abstract subclasses,
;;; then the references to find-slot-definition (and perhaps class-of)
;;; for the slot in question can be replaced with a load-time-value
;;; form or literal. 
;;; 
;;; Furthermore, if the above is applicable, and only the system defined
;;; method for slot-value-using-class is applicable for some slot, and
;;; either: 
;;;    - the domain of applicable methods on (setf)
;;;      slot-value-using-class is sealed for class and all
;;;      superclasses, or 
;;;    - slot-value-using-class is not declare inline and debug is not
;;;      declared high,
;;; then for the slot in question, we can use the instance access
;;; protocol routines directly.

;;; Potential optimization: When the above is not applicable, but a
;;; slot is known at compile-time to have an accessor which we trust
;;; will be present at run-time, it may be faster to use that accessor
;;; rather than going through find-slot/slot-value-using-class.  This
;;; is because accessors are safely optimized through the emf cache,
;;; so they may get optimized even if the above optimizations can't be
;;; made for with-slots.

(defmacro eclipse:WITH-SLOTS (slot-entries instance-form &body body)
  (let ((instance (gensym "INSTANCE"))
	(class (gensym "CLASS"))
	slot-definitions bindings)
    (loop for slot-entry in slot-entries
	  for var = (eclipse:bindingform-name slot-entry)
	  for name = (eclipse:bindingform-initform slot-entry slot-entry)
	  for def = (gensym (symbol-name name))
	  do (push `(,def (eclipse:find-slot-definition ,class ',name))
		   slot-definitions)
	  do (push `(,var (eclipse:slot-value-using-class ,class ,instance
						  ,def))
		   bindings))
    `(let* ((,instance ,instance-form)
	    (,class (eclipse:class-of ,instance))
	    ,@slot-definitions)
       (symbol-macrolet ,bindings
	 ,@body))))

(defmacro eclipse:WITH-ACCESSORS (slot-entries instance-form &body body)
  (let ((instance (gensym "INSTANCE")))
    `(let ((,instance ,instance-form))
       (symbol-macrolet
	   ,(loop for slot-entry in slot-entries
		  collect `(,(first slot-entry)
			    (,(second slot-entry) ,instance)))
	 ,@body))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;             PROCESSING OF THE USER INTERFACE MACROS          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;               METHODS AND GENERIC FUNCTIONS                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; IWBNI if these were stored in the environment, rather than bound
;;; in dynamic variables.  (It would be even better if they were
;;; simply passed as arguments to make-method-lambda, but the AMOP
;;; didn't set things up that way.)

(defparameter *specializer-names* nil)
(defparameter *specialized-names* nil)

;;; Given a lambda-list providing parameter names, this provides
;;; appropriate type declarations for the required parameters. It also
;;; provides suitable ignorable declarations for those parameters that
;;; are explicitly specialized.  (If *specializer-names* and
;;; *specialized-names* are nil, a no-op set of declarations is
;;; returned.)  We don't bother creating a type declaration for eql
;;; specializers that require evaluation.

(defun eclipse::specializer-declarations (lambda-list environment)
  #-not-yet (declare (ignore lambda-list environment))
  (cons `(ignorable ,@*specialized-names*)
	#-not-yet nil ;!!!
	#+not-yet
	(loop for parameter in lambda-list
	      and specializer-name in *specializer-names*
	      for type = (cond ((symbolp specializer-name)
				specializer-name)
			       ((eclipse:car-eq specializer-name 'eql)
				(let ((form (macroexpand
					     (second specializer-name)
					     environment)))
				  (unless (consp form)
				    `(member ,form)))))
	      when type collect (list 'type type parameter))))

(defun make-specializer-forms (specializer-names)
  (cons 'list
	(loop for name in specializer-names
	      collect (typecase name
			(symbol
			 `(eclipse:canonicalize-specializer ',name))
			(cons
			 (ecase (first name)
			   (eclipse:eql 
			    `(eclipse:intern-eql-specializer ,(second name)))))
			(t name)))))

(defun generate-method-form (gf-form gf function-name body env)
  ;; We don't use function-name now, but we might change
  ;; (function (lambda ...)) into an eclipse-specific special form
  ;; that lets the generated C function have a meaningful name.
  (let* ((qualifiers nil)
	 (specialized-lambda-list	;Pop qualifiers off body until
	  (do (lambda)			;we hit a lambda list.
	      ((listp (setq lambda (pop body))) lambda)
	    (push lambda qualifiers)))
	 (lambda-list (eclipse:extract-lambda-list specialized-lambda-list))
	 (*specialized-names* (eclipse::extract-specialized-names
			       specialized-lambda-list))
	 (*specializer-names* (eclipse:extract-specializer-names
			       specialized-lambda-list)))    
    (multiple-value-bind (method-lambda plist)
	(eclipse:make-method-lambda
	 gf
	 (eclipse:class-prototype (eclipse:generic-function-method-class gf))
	 `(lambda ,lambda-list ,@body) env)
      `(eclipse:ensure-method
	,gf-form
	:qualifiers ',(setq qualifiers (nreverse qualifiers))
	:lambda-list ',lambda-list
	;; Eql specializer names need to be evaluated.
	:specializers
	,(make-specializer-forms *specializer-names*)
	:function (eclipse:named-function
		   (,function-name ,@qualifiers ,@*specializer-names*)
		   ,@(cdr method-lambda))
	,@plist))))

;;; MAKE-STAND-IN-METHOD creates a compile-file literal suitable for
;;; representing the method on generic-function in environment.  This
;;; is called by the default method on make-method-lambda with a
;;; prototype of method (which must not be side-effectected if it is
;;; to serve effectively as a compile-file literal).  The result
;;; becomes the second argument to no-next-method.

;;; The second argument to no-next-method is supposed to be the method
;;; that called call-next-method, but the caller of make-method-lambda
;;; only supplies a prototype, which isn't really enough to fully
;;; identify the method.  For now, we just use the prototype and rely
;;; on there being a make-load-form for uninitialized methods that at
;;; least produces a method of the correct type.  If this isn't good
;;; enough, we could make a new instance of the same class,
;;; initializing :qualifiers, :specializers, and generic-function with
;;; information from the other arguments to make-stand-in-method.
;;; Issues are:
;;;    1. Environment does not currently store this information.
;;;    2. The generic-function slot does not have an initarg, it must
;;;       be (setf slot-value)'d.
;;;    3. The specializers might contain an eql specializer with a
;;;       form that must be evaluated, and which cannot be used as a
;;;       compile-file literal.

(defun eclipse::make-stand-in-method (generic-function method environment)
  (declare (ignore generic-function environment))
  (make-load-form method))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;               DEFCLASS, DEFGENERIC, DEFMETHOD                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SLOT OPTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun standard-slot-option (name value plist singlep)
  (loop for pair on plist by #'cddr
	for (key vals) = pair
	and tail = nil then (rest pair)
	when (eq (second key) name)
	return (let* ((pairs (rest vals))
		      (old (first pairs)))
		 (rplaca pairs (nconc (if singlep (list old) old)
				      (list value)))
		 nil)
	finally (progn (rplacd (or tail (last plist))
			       `(',name ',(if singlep value `(,value))))
		       (return singlep))))

(defun accumulated-slot-option (name)
  #'(lambda (old-name value specs singlep)
      (declare (ignore old-name singlep))
      (standard-slot-option name value specs nil))) 

(let ((option-table (make-hash-table)))
  (defun add-slot-option (key func)
    (setf (gethash key option-table) func))
  (defun slot-option (key &optional (default #'standard-slot-option))
    (gethash key option-table default)))

(add-slot-option :reader (accumulated-slot-option :readers))
(add-slot-option :writer (accumulated-slot-option :writers))
(add-slot-option :initarg (accumulated-slot-option :initargs))
(add-slot-option :accessor
		 #'(lambda (name value specs singlep)
		     (declare (ignore name singlep))
		     (standard-slot-option :readers value specs nil)
		     (standard-slot-option :writers `(eclipse:setf ,value) specs nil)))
(add-slot-option :initform
		 #'(lambda (name value specs singlep)
		     (let ((form (standard-slot-option name value specs singlep)))
		       (if (and (not form) singlep)
			   (eclipse::multiple-appearance-error name "slot options")
			   (let ((specs1 (rest specs)))
			     (rplacd specs1
				     `(':initfunction 
				       (function (lambda () ,value))
				       ,@(rest specs1)))))
		       form)))
		       
(defun canonicalize-mop-slot-specification (specs)
  (loop for (name value) on (when (consp specs) (rest specs)) by #'cddr
	with result = (list '':name `',(eclipse:bindingform-name specs))
	with seen = nil
	do (setf (getf seen name)
		 (funcall (slot-option name)
			  name value result (getf seen name t)))
	finally (return (cons 'list result))))

(defun canonicalize-mop-slot-specifications (direct-slots)
  `(list ,@(mapcar #'canonicalize-mop-slot-specification direct-slots)))


;;; DEFCLASS/DEGENERIC options ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun standard-option (name tail)
  `(',name ',tail))
(defun singleton-option (name tail)
  (destructuring-bind (val) tail
    (standard-option name val)))
   
(let ((option-table (make-hash-table)))
  (defun add-mop-option (key func)
    (setf (gethash key option-table) func))
  (defun mop-option (key &optional (default #'standard-option))
    (gethash key option-table default)))

(defun canonicalize-mop-options (options)
  (loop for (name . tail) in options
	with seen = nil
	if (member name seen)
	do (eclipse::multiple-appearance-error name "options")
	else do (push name seen)
	nconc (funcall (mop-option name) name tail)))

(add-mop-option :documentation #'singleton-option)
(add-mop-option :metaclass #'singleton-option)
(defun canonicalize-default-initargs (plist)
  `(list ,@(loop for (name form) on plist by #'cddr
		 with seen = nil
		 if (member name seen)
		 do (eclipse::multiple-appearance-error name :default-initargs)
		 else do (push name seen)
		 collect `(list ',name ',form
				(function (lambda () ,form))))))
(add-mop-option :default-initargs
  #'(lambda (name tail)
      (declare (ignore name))
      (list '':direct-default-initargs
	    (canonicalize-default-initargs tail))))

(add-mop-option :abstract #'singleton-option)
(add-mop-option :sealed #'singleton-option)
(add-mop-option :primary #'singleton-option)



;;; Rather than :compile-toplevel, this should probably do something
;;; using and &environment parameter.

(defmacro eclipse:DEFCLASS (name direct-superclasses direct-slots &rest options) 
  `(eclipse:eval-when (:compile-toplevel :load-toplevel :execute)
     (eclipse:ensure-class ',name
		   :direct-superclasses ',direct-superclasses
		   :direct-slots
		   ,(canonicalize-mop-slot-specifications direct-slots)
		   ,@(canonicalize-mop-options options))))


(add-mop-option :generic-function-class #'singleton-option)
(add-mop-option :method-class #'singleton-option)
(add-mop-option :method-combination
		#'(lambda (option tail)
		    (destructuring-bind (name . args) tail
		      (list option
			    `(eclipse::intern-method-combination
			      ',name ,@args)))))

;;; ANSI says that ENSURE-GENERIC-FUNCTION accepts an :environment
;;; keyword, though the MOP says nothing about this.  Certainly, we
;;; cannot include an environemnt as a compile-file literal in a form
;;; RETURNED from a macro expansion (because &environment values have
;;; dynamic extent), but perhaps we could pass it to calls to
;;; ENSURE-GENERIC-FUNCTION made DURING the macroexpansion of
;;; defgeneric and defmethod.

(defmacro eclipse:DEFGENERIC (function-name lambda-list &rest options
					    &environment env)
  (let* ((gf-name (gensym "GF"))
	 ;; Note that gf is ensured during macroexpansion!
	 ;; This preserves uniform compile-time behavior regardless of
	 ;; whether initial methods are present.
	 (gf (eclipse:ensure-generic-function function-name))
	 (methods (loop for (name . body) in options
			when (eq name :method)
			collect (generate-method-form
				 gf-name gf function-name body env)))
	 ;; IWBNI we added current OPTIMIZE quantities from env.
	 (declarations (loop for (name . decs) in options
			     when (eq name 'declare) nconc decs))
	 (gf-form `(eclipse:ensure-generic-function
		    ',function-name
		    ':lambda-list ',lambda-list
		    ,@(when declarations `(':declarations ',declarations))
		    ,@(canonicalize-mop-options
		       (delete 'declare
			       (delete :method options :key #'first)
			       :key #'first)))))
    (if methods
	`(let ((,gf-name ,gf-form))
	   (eclipse:add-attached-methods ,gf-name (list ,@methods))
	   ,gf-name)
	gf-form)))

		 
;;; IWBNI we found a way to keep from having to repeatedly
;;; ensure-generic-function in a generated file with lots of methods
;;; on the same generic-function.  Perhaps by making the
;;; generic-function a compile-file literal?

(defmacro eclipse:DEFMETHOD (function-name &rest args &environment env)
  (generate-method-form
   `(eclipse:ensure-generic-function ',function-name)
   (eclipse:ensure-generic-function function-name)
   function-name args env))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                   METHOD COMBINATION                         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Parse-method-combination is used by DEFINE-METHOD-COMBINATION.  It
;;; produces a lambda which takes method combination options.  We call
;;; this a method-combination-generator.  The arguments are defined by
;;; the lambda-list of the method-combination, and the actual
;;; parameters for a call are supplied by the :method-combination
;;; arguments in DEFGENERIC.  The method-combination-function returns
;;; another function which we call the method-combination-function.

;;; The method-combination-function is called by
;;; COMPUTE-EFFECTIVE-METHOD.  It takes a list of applicable methods
;;; as argument and returns two values: an effective method form and a
;;; list of effective-method options (:generic-function, :arguments,
;;; etc.).  These are the values returned by COMPUTE-EFFECTIVE-METHOD.
;;; The method-combination-function is evalutated in the lexical
;;; environment in which the method-combination-function was defined
;;; (i.e. the environment in which DEFINE-METHOD-COMBINATION was
;;; executed).  The variables defined by :generic-function and
;;; :arguments are NOT visible during execution of the
;;; method-combination-function.


(macrolet ((def-method-group-spec (key (spec) default)
	     `(defun ,(eclipse:make-name "METHOD-GROUP-SPEC-~a" key) (,spec)
		(do ((spec ,spec (cdr spec)))
		    ((endp spec) ,default)
		  (when (eq (car spec) ,key)
		    (return (second spec)))))))
  (def-method-group-spec :required (full-spec) nil)
  (def-method-group-spec :order (full-spec) :most-specific-first)
  (def-method-group-spec :description (full-spec)
    (format nil "~s~~*" (car full-spec))))

;;; qualifier-match is in clos-run.lisp

(defun parse-group-qualifier (options qualifier-var &aux patterns)
  (dolist (option (rest options))
    (typecase option
      ((or list (member *))
       (push `(eclipse:qualifier-match ',option ,qualifier-var) patterns))
      (keyword (return))
      (symbol
       (push `(,option ,qualifier-var) patterns))))
  (cond ((rest patterns) `(or ,@(nreverse patterns)))
	(patterns (car patterns))
	(t (eclipse:signal-program-error
	    "No qualifier-pattern or predicate for method-group-specifier ~s."
	    options))))
  
(defun parse-method-group-specifiers (group-specifiers methods)
  `((dolist (method ,methods)
      (let ((qualifiers (eclipse:method-qualifiers method)))
	(cond ,@(loop for spec in group-specifiers
		      collect `(,(parse-group-qualifier spec 'qualifiers)
				(push method ,(first spec))))
	      (t (eclipse:invalid-method-error
		  method
		  "Qualifier ~s does not match ~{~s~#[~; or ~:;, ~]~} group."
		  (eclipse:method-qualifiers method) ',(mapcar #'car group-specifiers))))))
    ;; Should also map over each group and signal error when any two
    ;; methods in a group have equal specializers but different
    ;; qualifiers!
    ,@(loop for spec in group-specifiers
	    for name = (first spec)
	    collect `(eclipse:ecase ,(method-group-spec-order spec)
		       (:most-specific-first (setq ,name (nreverse ,name)))
		       (:most-specific-last)))
    ,@(loop for spec in group-specifiers
	    for name = (first spec)
	    collect `(eclipse:when (eclipse:and ,(method-group-spec-required spec)
				(null ,name))
		       (eclipse:method-combination-error
			"No applicable ~@? methods."
			,(method-group-spec-description spec) nil)))))

;;; "Each parameter variable defined by [:arguments] lambda-list is
;;; bound to a form that can be inserted into the effective
;;; method. When this form is evaluated during execution of the
;;; effective method, its value is the corresponding argument to the
;;; generic function. ... Within the body forms, [:generic-function]
;;; generic-function-symbol is bound to the generic function object."

;;; The "form" we use is simply the quoted variable name, and then we
;;; evaluate the body in an environment in which the variable name is
;;; bound.  See MAKE-EFFECTIVE-METHOD-LAMBDA.

(defun parse-method-combination (lambda-list group-specifiers body)
  (let ((methods (gensym "METHODS")) options additional-parameters)
    (labels ((add-arg1 (name)
		       (push `(,name ',name) additional-parameters))
	     (add-arg (parameter-form)
		      (etypecase parameter-form
			(cons (destructuring-bind
				  (name &optional init
					(suppliedp nil spp))
				  parameter-form
				(declare (ignore init))
				(add-arg1 (eclipse:bindingform-initform name name))
				(when spp (add-arg1 suppliedp))))
			(symbol (add-arg1 parameter-form)))))
      ;; Pop options off body until we hit the real body.
      (dolist (form body)
	(case (and (consp form) (car form))
	  (:arguments (let ((lambda (rest form)))
			(loop for parameter in lambda
			      unless (eclipse:lambda-list-keyword-p parameter)
			      do (add-arg parameter))
			(push lambda options)
			(push :arguments options)
			(pop body)))
	  (:generic-function (let ((name (second form)))
			       (add-arg name)
			       (push name options)
			       (push :generic-function options)
			       (pop body)))
	  (t (return))))
      (when additional-parameters
	(setf body `((let ,additional-parameters ,@body))))
      (multiple-value-bind (decl body doc) (eclipse:find-declarations body nil)
	(values
	 `(lambda ,lambda-list
	    (declare ,@decl)
	    #'(lambda (,methods)
		(values
		 (let ,(mapcar #'car group-specifiers)
		   ,@(parse-method-group-specifiers group-specifiers methods)
		   ,@body)
		 ',options)))
	 doc decl)))))

(defun parse-short-method-combination (name &key documentation
					    identity-with-one-argument
					    (operator name))
  (values
   (parse-method-combination
    '(&optional (order :most-specific-first))
    `((eclipse::around (:around))
      (eclipse::primary (,operator) :order order :required t))
    `((let ((form ,(let ((all ``(,',operator
				 ,@(loop for method in eclipse::primary
					 collect `(eclipse:call-method ,method ())))))
		     (if identity-with-one-argument
			 `(if (rest eclipse::primary)
			      ,all
			    `(eclipse:call-method ,(first eclipse::primary) ()))
		       all))))
	(if eclipse::around
	    `(eclipse:call-method ,(first eclipse::around)
			  (,@(rest eclipse::around) (eclipse:make-method ,form)))
	  form))))
   documentation nil))

;;; Rather than :compile-toplevel, this should probably do something
;;; using and &environment parameter.

(defmacro eclipse:DEFINE-METHOD-COMBINATION (name &rest options)
  (multiple-value-bind (lambda doc decls)
      (if (and options (listp (first options)))	;long form
	  (destructuring-bind (lambda-list group-specifiers &body body)
	      options
	    (parse-method-combination
	     lambda-list group-specifiers body))
	  (apply #'parse-short-method-combination name options))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (eclipse:ensure-method-combination-type
	',name
	:function (function ,lambda)
	:documentation ,doc
	;; IWBNI we also appended the optimizations from the env.
	:declarations ',decls))))

