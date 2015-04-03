#| *****************************************************************
Utilities used by CLOS at run time.
***************************************************************** |#
;; This is set in type-metaobjects.lisp.  Fortunately, classp works
;; with the "class" metaobject being tested for is nil.
(defparameter *eql-specializer-type* nil)

;;; format-method, print-metaobject are defined in print-object.lisp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                ERROR CHECKING                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; N.B.: Initarg checking is at end of file.

(defun illegal-initialization (op class)
  (error "Cannot ~a ~s." op class))

(defun illegal-change-class (instance new-class)
  (error "Cannot change class of ~s from ~s to ~s."
	 instance (class-of instance) new-class))

(defun check-metaclass (class metaclass)
  (let ((old-metaclass (class-of class))
	(new-metaclass (canonicalize-class metaclass t)))
    (unless (eq old-metaclass new-metaclass)
      (illegal-change-class class new-metaclass))))


;;; ANSI specifically requires that method-combination-error and
;;; invalid-method-error can only be called within the dynamic-extent
;;; of a method combination function.  This allows us to assume that
;;; *generic-function* and *method-combination* will be bound.
(defparameter *generic-function* nil) 
(defparameter *method-combination* nil)

(defun METHOD-COMBINATION-ERROR (format-string &rest args)
  (error "~?~_ ~w: ~w." format-string args
	*generic-function* *method-combination*)) 

(defun INVALID-METHOD-ERROR (method format-string &rest args)
  (apply #'method-combination-error "Invalid method ~w:~_ ~@?" method
	 format-string args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                   PREDICATES                                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun local-slot-p (slot)
  (eq (slot-definition-allocation slot) :instance))

(defun subclassp (c1 c2)
  (if (eq c1 c2) t			;For speed.      
      (find-list-eq c2
		    (host::static-access c1 class-precedence-list class))))

(defun classp (object class)
  (subclassp (class-of object) class))
(defun eql-specializer-p (object)
  (classp object *eql-specializer-type*))
(defun specializerp (object specializer)
  (if (eql-specializer-p specializer)
      (eql object (eql-specializer-object specializer))
      (classp object specializer)))

(defun std-method-more-specific-p (gf method1 method2
				      required-classes)
  (let ((args (generic-function-required-parameters gf))
	(order (generic-function-argument-precedence-order gf))
	(specs1 (method-specializers method1))
	(specs2 (method-specializers method2)))
    (if (list-eq args order)
	(loop for spec1 in specs1 and spec2 in specs2
	      and class in required-classes
	      unless (eq spec1 spec2)
	      return (cond ((eql-specializer-p spec1) t)
			   ((eql-specializer-p spec2) nil)
			   (t (let ((cpl (class-precedence-list class)))
				(find-list-eq spec2 (cdr (member spec1
							 cpl))))))
	      finally (return nil))
	(dolist (name order nil)
	  (let* ((position (position name args))
		 (spec1 (nth position specs1))
		 (spec2 (nth position specs2)))
	    (unless (eq spec1 spec2)
	      (return
	       (cond ((eql-specializer-p spec1) t)
		     ((eql-specializer-p spec2) nil)
		     (t (let ((cpl (class-precedence-list
				    (nth position required-classes))))
			  (find-list-eq spec2 (cdr (member spec1
						   cpl)))))))))))))

;;; ANSI/MOP require name to be a symbol.
(defun check-slot-initialization-p (slot-name slot-names)
  (or (not (listp slot-names))
      (member slot-name slot-names :test #'eq)))

(defun initialize-slot-p (instance slot-name slot-names)
  (and (check-slot-initialization-p slot-name slot-names)
       (not (slot-boundp instance slot-name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;          ACCESSORS AND OTHER EXTRACTION                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun proper-name (class &optional (errorp t) environment)
  (let ((name (class-name class)))
    (if (and name (symbolp name)
	     (eq class (find-type name nil environment)))
	name
	(when errorp
	  (error "~s is not known by a proper name." class)))))

;;; Potential optimization: cache the number of local slots in the
;;; class object.
(defun n-instance-slots (slot-definitions)
  ;; COUNT-IF is defined using CLOS.
  (loop with n = 0
	for slot in slot-definitions
	when (local-slot-p slot) do (setq n (add-integer-integer n 1))
	finally (return n)))

(defun lambda-&rest-index (lambda-list)
  (loop for parameter in lambda-list
	with index = 0
	do (case parameter
	     ((&rest &key) (return index))
	     (&optional )
	     (&aux (return index))
	     (otherwise (incf index)))
	finally (return index)))

(defun generic-function-&rest-index (gf)
  (lambda-&rest-index (generic-function-lambda-list gf)))


(defun EQL-SPECIALIZER-OBJECT (eql-specializer)
  (with-slots (object) eql-specializer
    object))

;;; Used by emf-table-get to map a list of classes into a list of
;;; current wrappers.  emf-table-get is defined in the host
;;; environment, but it is easier do this part in Lisp than in C,
;;; etc.  Note however, that the reference to class-wrapper must be
;;; statically resolved to avoid looping, because this is used in
;;; defining the EMF for class-wrapper itself.
(locally (declare (inline static-class-wrapper))
  (defun static-class-wrapper (class)
    (host::static-access class wrapper class))
  (defun class-wrappers (class-list)
    (loop for class in class-list
	  collect (static-class-wrapper class))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                 INSTANCE STRUCTURE PROTOCOL                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Returns effective-slot-definition from class, or slot-name if not
;;; found.  IMHO this should be a gf so that it can be optimized for
;;; classes with huge numbers of slots.

;;; Potential optimization: Cache a plist of
;;; name/effective-slot-definition-pairs in the class.

;;; FIND is implemented using CLOS
;;; ANSI/MOP require name to be a symbol.
(defun find-slot-definition (class slot-name)
  (loop for slot in (class-slots class)
	when (eq slot-name (slot-definition-name slot))
	return slot))

(defun SLOT-EXISTS-P (instance slot-name)
  (let ((class (class-of instance)))
    (not (null (find-slot-definition class slot-name)))))

(defun SLOT-BOUNDP (instance slot-name)
  (let* ((class (class-of instance))
	 (slot-definition (find-slot-definition class slot-name)))
    (if slot-definition
	(slot-boundp-using-class class instance slot-definition)
	(values (slot-missing class instance slot-name 'slot-boundp)))))

(defun SLOT-VALUE (instance slot-name)
  (let* ((class (class-of instance))
	 (slot-definition (find-slot-definition class slot-name)))
    (if slot-definition
	(slot-value-using-class class instance slot-definition)
	(values (slot-missing class instance slot-name 'slot-value)))))

(defun SLOT-MAKUNBOUND (instance slot-name)
  (let* ((class (class-of instance))
	 (slot-definition (find-slot-definition class slot-name)))
    (if slot-definition
	(slot-makunbound-using-class class instance slot-definition)
	(progn (slot-missing class instance slot-name 'slot-makunbound)
	       instance))))

(defun (SETF SLOT-VALUE) (new-value instance slot-name)
  (let* ((class (class-of instance))
	 (slot-definition (find-slot-definition class slot-name)))
    (if slot-definition
	(setf (slot-value-using-class class instance slot-definition)
	      new-value)
	(progn (slot-missing class instance slot-name 'setf new-value)
	       new-value))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         DEPENDENT MAINTENANCE PROTOCOL                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; See 4.3.6 Redefining Classes in ANSI.
;;; N.B.: It is difficult to put more of the work for computing
;;; added/discarded names into make-instances-obsolete (and
;;; storing the results in the wrapper), because classes don't have
;;; access to old wrappers.  If the class is redefined TWICE, the
;;; previously collected and cached names will not be correct.

;;; Potential optimization: If there are no user-defined applicable
;;; methods on update-instance-for-redefined-class, we need not build
;;; discarded-names list and plist of discarded name/values, and we
;;; can call shared-initialize instead of
;;; update-instance-for-redefined-class.

(defun update-instance (instance wrapper class)
  (declare (type wrapper wrapper))
  (let* ((old-slot-definitions (wrapper-obsolete-slots wrapper))
	 (new-slot-definitions (class-slots class))
	 (old-slots (standard-instance-slots instance))
	 (new-slots (set-standard-instance-slots
		     instance
		     (make-slots (n-instance-slots new-slot-definitions)))))
    (set-tagged-instance-wrapper instance (class-wrapper class))
    ;; Gather lists of added-names, discard-names, plist.
    (flet ((old-value (old-slot)	;Includes old shared values.
		      (let ((location (slot-definition-location old-slot)))
			(if (consp location)
			    (cdr location)
			    (get-slot old-slots location)))))
      (let* (added-names discarded-names plist)
	;; Fill in new slots with old values or add to added-names.
	(loop for new-slot in new-slot-definitions
	      when (local-slot-p new-slot)
	      do (let* ((name (slot-definition-name new-slot))
			(old-slot (find name old-slot-definitions
					:key #'slot-definition-name)))
		   (if old-slot
		       (set-slot new-slots (slot-definition-location new-slot)
				 (old-value old-slot))
		       (push name added-names))))
	;; Build discarded-names and plist of discarded name/values.
	(loop for old-slot in old-slot-definitions
	      when (local-slot-p old-slot)
	      do (let* ((name (slot-definition-name old-slot))
			(new-slot (find name new-slot-definitions
					:key #'slot-definition-name)))
		   (unless new-slot 
		     (push name discarded-names)
		     (let ((value (old-value old-slot)))
		       (unless (unboundp value)
			 (setq plist (nconc (list name value) plist)))))))
	(update-instance-for-redefined-class
	 instance added-names discarded-names plist)))))

;;; Used by emf-table-get
(defun update-instances-if-needed (instances)
  (dolist (instance instances nil)
    (let ((wrapper (object-wrapper instance)))
      (declare (type wrapper wrapper))
      (when (wrapper-invalid-p wrapper)
	(update-instance instance wrapper
			 (tagged-instance-class instance)))))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;          GENERIC FUNCTION FINALIZATION                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Whenever generic-function-lambda-list is set, we also set a number
;;; of other parameters, and check the congruence of any methods.
(defun set-gf-lambda-list (gf lambda apo)
  (with-slots (argument-precedence-order
	       lambda-list 
	       required-parameters
	       keywords allow-other-keys-p)
      gf
    (let ((required
	   ;; SUBSEQ and POSITION-IF are defined using CLOS
	   (loop for parameter in lambda
		 when (lambda-list-keyword-p parameter)
		 return result
		 collect parameter into result
		 finally (return result)))
	  (keys (member '&key lambda)))
      (setf lambda-list lambda
	    required-parameters required
	    keywords (extract-keywords keys) 
	    allow-other-keys-p (or (null keys)
				   (find-list-eq '&allow-other-keys keys))
	    argument-precedence-order
	    (if apo
		(if (list-eq apo required)
		    required
		    (if (set-exclusive-or required apo)
			(signal-program-error
			 "Invalid :argument-predence-order ~s." apo)
			apo))
		required)))
    (when (slot-boundp gf 'methods)
      (dolist (method (generic-function-methods gf))
	(check-congruent-lambda gf method)))))

;;; Potential optimization: Set an initial discriminating function
;;; to a  closure over the generic-function which, when called, resets
;;; the discriminating function to the result of
;;; compute-discriminating-function, clears the method table, and then
;;; calls the discriminating function.

;;; Potential optimization: When a global cpl is used, we can keep the
;;; set of methods sorted.  Then compute-applicable-methods
;;; (-using-classes) need not resort the applicable methods.  Note
;;; that the call to clear-method-table in make-instances-obsolete
;;; would have to resort as well.

(defun finalize-generic-function (generic-function)
  (when (slot-boundp generic-function 'lambda-list)
    (set-funcallable-instance-function
     generic-function
     (compute-discriminating-function generic-function))
    (flush-method-cache generic-function))
  generic-function)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GENERIC FUNCTION APPLICATION

;;; MOP: "When a method is actually called by an effective mehtod,
;;; it's first argument will be a list for the arguments to the
;;; generic function.  Its remaining arguments will be all but the
;;; first argument passed to call-method.  By default, all method
;;; functions must accept two arguments: the list of arguments to the
;;; generic function and the list of next methods."

(defmacro apply-method (method args &rest other-data)
  `(funcall-function (method-function ,method) ,args ,@other-data))

(defun make-internal-method (function)
  (make-instance standard-method-classobj :function function))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EFFECTIVE-METHOD-FUNCTIONS

(defun sort-applicable-methods (gf applicable-methods arg-classes)
  (quicksort-list applicable-methods nil
		  #'(lambda (m1 m2)
		      (std-method-more-specific-p
		       gf m1 m2 arg-classes))
		  nil))


(defun call-method-macrolet (form args)
  `(macrolet ((call-method (m &rest d)
		(call-method-expansion m ',args d)))
     ,form))

(defun make-method-expansion (make-method-form args)
  (call-method-macrolet (second make-method-form) args))

(defun call-method-expansion (method args other-data)
  (if (car-eq method 'make-method)
      (make-method-expansion method args)
    `(apply-method
      ,method ,args
      ,@(if (consp (car other-data))
	    `(,(cons 'list
		     (loop for method in (car other-data)
			   collect 
			   (if (car-eq method 'make-method)
			       `(make-internal-method
				 #'(lambda (args &rest other-data)
				     (declare (ignore other-data)
					      (dynamic-extent other-data))
				     ,(make-method-expansion
				       method 'args)))
			       method)))
	      ,@(rest other-data))
	  other-data))))

(defun standard-discriminating-function (generic-function n)
  #'(lambda (&rest args)
      (declare (dynamic-extent args))
      (let* ((table (emf-table generic-function))
	     (emfun (emf-table-get table n args)))
	(if emfun
	    (apply-function emfun args)
	    (let ((classes (loop repeat n for arg in args
				 collect (class-of arg))))
	      (multiple-value-bind (applicable-methods by-class-p)
		  (compute-applicable-methods-using-classes
		   generic-function classes)
		(unless by-class-p
		  (setq applicable-methods
			(compute-applicable-methods
			 generic-function args)))
		(let ((emfun
		       (compute-effective-method-function
			generic-function
			(generic-function-method-combination
			 generic-function)
			applicable-methods classes)))
		  (when by-class-p
		    (emf-table-set table n classes emfun))
		  (apply-function emfun args))))))))

;;; This function gives us a place to catch some special cases of
;;; effective methods and return a closure rather than calling
;;; compute-effective-method and compiling the result.
;;;
;;; IWBNI this were a generic function so we could open this up to users.
;;; 
;;; We create (and cache) a call to no-applicable-methods here rather
;;; than in make-effective-method-lambda in order to avoid generating
;;; keyword checking code.

(defun compute-effective-method-function (generic-function
					  method-combination
					  applicable-methods
					  argument-classes)
  (if applicable-methods
      (if (and (eq method-combination *standard-method-combination*)
	       (let ((meta (class-of generic-function)))
		 (or (eq meta standard-generic-function-classobj)
		     (eq meta standard-system-generic-function-classobj))))
	  (compute-standard-effective-method-function
	   generic-function applicable-methods argument-classes)
	  (compile-object (make-effective-method-lambda
			   generic-function method-combination
			   applicable-methods)
			  (generic-function-name generic-function)))
      #'(lambda (&rest args)
	  (declare (dynamic-extent args))
	  (apply #'no-applicable-method generic-function args))))


;;; We use a specialized EMFs for accessors that never calls
;;; method-function. Note that the cached EMF will not be applicable
;;; if the argument class is redefined, so we can safely cache
;;; location.  We check to make sure their are CURRENTLY (i.e. at
;;; compute-effective-method-function time) no user defined methods on
;;; (setf) slot-value-using-class, but we don't uncache the EMF if any
;;; such methods are defined later.  We justify this by noting that
;;; ANSI says (setf) slot-value may be inlined.  IWBNI we respected
;;; notinline declarations, and/or made slot-value-using-class a
;;; generic function of a certain class, which, like make-instance and
;;; friends, caused this cache to be flushed when methods were added
;;; (i.e. the generic-function was reinitialized?).

;;; Potential Optimization: Even when there IS a method on
;;; SLOT-VALUE-USING-CLASS, we can still define a specialized method
;;; function that caches the main-class and slot rather than calling
;;; class-of and find-slot during execution of the method function.

(defun compute-method-function (method argument-classes)
  (let ((readerp (classp method standard-reader-method-classobj)))
    (or (when (or readerp (classp method standard-writer-method-classobj))
	  (let* ((direct-slot (accessor-method-slot-definition method))
		 (main-class (if readerp (first argument-classes)
				 (second argument-classes)))
		 ;; Only an EFFECTIVE slot has a location.
		 (slot (find-slot-definition main-class
					     (slot-definition-name direct-slot))))
	    ;; Make sure that no user methods are applicable for slot-value
	    (multiple-value-bind (methods by-classp)
		(compute-applicable-methods-using-classes
		 (fdefinition (if readerp 'slot-value-using-class
				  '(setf slot-value-using-class)))
		 (let ((value-classes `(,(class-of main-class) ,main-class
					,(class-of slot))))
		   (if readerp
		       value-classes
		       (cons (first argument-classes) value-classes))))
	      (when (and by-classp (null (rest methods)))
		(let ((location  (slot-definition-location slot)))
		  (macrolet ((def-func (lambda &body body)
			       `#'(lambda (args &rest ignore)
				    (declare (ignore ignore)
					     (dynamic-extent ignore))
				    (destructuring-bind ,lambda args
				      ,@body))))
		    (if readerp
			(if (consp location)
			    (def-func (instance)
			      (declare (ignore instance))
			      (cdr location))
			    (def-func (instance)
			      (standard-instance-access instance location)))
			;; else writer
			(if (consp location)
			    (def-func (new instance)
			      (declare (ignore instance))
			      (setf (cdr location) new))
			    (def-func (new instance)
			      (setf (standard-instance-access instance location) new))))))))))
	(method-function method))))
		      
;;; Potential Optimization: Create a constant-method class.  In analogous
;;; circumstances in which specialized EMFs are used for accessors,
;;; constant-methods create a specialized EMF that returns a constant
;;; value (This is computed at the time the EMF is created by running the
;;; method.  This might require passing the actual arguments to
;;; compute-standard-effective-method-function.)  This is useful for
;;; constant slot accessors and in implementing things like typep and
;;; subtypep. 

(defun compute-standard-effective-method-function (gf methods argument-classes)
  (macrolet ((with-method-error (form)
				`(let ((*generic-function* gf)
				       (*method-combination*
					(generic-function-method-combination gf)))
				   ,form)))
    ;; Categorize methods into groups and determine keycheck information.
    (let* ((keys (generic-function-keywords gf))
	   (check-keys-p (not (generic-function-allow-other-keys-p gf)))
	   arounds primaries befores afters)
      (loop for method in methods with qualifier
	    when check-keys-p
	    do (multiple-value-bind (method-keys allow-other-keys-p)
		   (function-keywords method)
		 (if allow-other-keys-p
		     (setq check-keys-p nil)
		     (setq keys (union keys method-keys))))
	    do (case (if (rest (setq qualifier (method-qualifiers method)))
			 qualifier
			 (car qualifier))
		 ((nil) (push method primaries))
		 (:around (push method arounds))
		 (:before (push method befores))
		 (:after (push method afters))
		 (t (with-method-error
		     (invalid-method-error
		      method "Illegal qualifier~{ ~s~}."
		      qualifier)))))
      (unless primaries
	(with-method-error
	 (method-combination-error "No primary methods applicable to classes ~s."
				   argument-classes)))
      ;; Don't reverse afters.  Arounds are rereversed later.
      (setq primaries (nreverse-list primaries)
	    befores (nreverse-list befores))
      ;; We will build closures for each special case out of the
      ;; following parts:
      (let* ((first-primary (car primaries))
	     (first-primary-function (compute-method-function
				      first-primary argument-classes))
	     (other-primaries (rest primaries)))
	(macrolet
	    ((defmeth (&body body)
	       `#'(lambda (&rest args)
		    ;; IWBNI we knew that the method-function didn't
		    ;; expect args to have indefinite extent, but we don't.
		    ;; (declare (dynamic-extent args))
		    ,@body))
	     (before ()
		     '(dolist (before befores)
			(apply-method before args nil)))
	     (main () '(funcall-function first-primary-function args other-primaries))
	     (combined-after ()
			     '(multiple-value-prog1
				  (main) 
				(dolist (after afters)
				  (apply-method after args nil))))
	     (check () '(check-keys (nthcdr n args) keys)))
	  (let* ((n (and check-keys-p (generic-function-&rest-index gf)))
		 (main			;Variable capture!	    
		  (if (or arounds (null check-keys-p))
		      (if afters
			  (if befores
			      (defmeth (before) (combined-after))
			      (defmeth (combined-after)))
			  (if befores
			      (defmeth (before) (main))
			      (defmeth (main))))
		      (if afters
			  (if befores
			      (defmeth (check) (before) (combined-after))
			      (defmeth (check) (combined-after)))
			  (if befores
			      (defmeth (check) (before) (main))
			      (defmeth (check) (main)))))))
	    (if arounds
		(let* ((all-around
			(when arounds
			  (nreverse-list
			   (cons
			    (make-internal-method
			     #'(lambda (args &rest ignore)
				 (declare (ignore ignore)
					  (dynamic-extent ignore))
				 (apply-function main args)))
			    arounds))))
		       (first-around-function (method-function
					       (first all-around)))
		       (other-arounds (rest all-around)))
		  (if check-keys-p
		      (defmeth (check)
			(funcall-function first-around-function args other-arounds))
		      (defmeth
			(funcall-function first-around-function args other-arounds))))
		main)))))))


;;; Notes for MAKE-EFFECTIVE-METHOD-LAMBDA:
;;; 1. Since the :arguments lambda-list may have default initforms
;;; that depend on the proper scope of the binding of parameters, it
;;; is important that we not split up the these parameters into a LET 
;;; (for missing values) and a separate DESTRUCTURING-BIND.  We only
;;; deal with the most general case (mismatched parameters) and do not
;;; try to optimize other cases.  If a programmer of method
;;; combination knows the actual arglist, the programmer can specify
;;; &whole args with an explicit destructuring-bind of args.
;;; 2. We are allowed to suppress keyword checking if the gf or any
;;; applicable method or the method-combination is unsafe, but we
;;; don't do that. 

(defun make-effective-method-lambda (gf method-combination methods)

  ;; COMPUTE EFFECTIVE METHOD
  (multiple-value-bind (form options)
	(compute-effective-method
	 gf method-combination methods)

    ;; Add bindings from method combination :arguments.
    (multiple-value-bind (bindings arg-var key-var)
	(adjusted-lambda-bindings
	 gf (rest (find :arguments options :key #'car)))

      ;; Add binding for method combination :generic-function.
      (let ((gf-var (find :generic-function options :key #'car)))
	(when gf-var
	  (setq bindings (nconc bindings `((,(second gf-var) ',gf))))))

      ;; Add call-method definition.
      (let ((body (list (call-method-macrolet form arg-var))))
	     
	;; Add keyword argument checking.
	(unless (generic-function-allow-other-keys-p gf)
	  (let ((allowed-keys (generic-function-keywords gf)))
	    (loop for method in methods
		  do (multiple-value-bind (keys allow-other-keys-p)
			 (function-keywords method)
		       (if allow-other-keys-p
			   (return)
			   (setq allowed-keys
				 (union keys allowed-keys))))
		  finally 
		  (push `(check-keys ,(or key-var
					  `(nthcdr
					    ,(generic-function-&rest-index gf)
					    ,arg-var))
				     ',allowed-keys) body))))
	`(lambda (&rest ,arg-var)
	   (declare (dynamic-extent ,arg-var))
	   ,@(if bindings
		 `((let* ,bindings ,@body))
		 body))))))

;;; Given an effective-method :arguments lambda-list, this returns:
;;; 1. A (possibly empty list) of binding forms (name value pairs) for
;;;    each argument variable used in the effective method.
;;; 2. A symbol to be used as the name of the variable that holds the
;;;    list of all the arguments to the generic function.  If this is
;;;    not specified in an &whole parameter in the :arguments
;;;    lambda-list, an internal symbol is used.
;;; 3. The name of the (possibly internal) parameter that holds the
;;;    set of keyword arguments (eg. the &rest parameter), if any.  

(defun adjusted-lambda-bindings (gf em-lambda)
  (let ((args (gensym "ARGS"))
	(rest (gensym "REST")))
    (if em-lambda
	(let* ((whole (if (eq (car em-lambda) '&whole)
			  (prog1 (second em-lambda)
			    (setq em-lambda (cddr em-lambda)))
			  args))
	       (bindings (when em-lambda `((,rest ,whole))))
	       (restvar nil))
	  (loop for parameter in em-lambda
		with index = 0 and cdring = 0
		and max = (length
			   (generic-function-required-parameters gf))
		and optional-p = nil and restp = nil and keyp = nil
		do (case parameter
		     (&optional
		      (setq cdring (- max index)
			    index max
			    max (generic-function-&rest-index gf)
			    optional-p t))
		     (&rest (unless optional-p
			      (setq max (generic-function-&rest-index gf)))
			    (setq cdring (- max index)
				  restp t))
		     (&key (unless restp
			     (unless optional-p
			       (setq max (generic-function-&rest-index gf)))
			     (setq cdring (- max index)))
			   (setq keyp t))
		     (&aux (setq index 0 max 0 restp nil keyp nil))
		     (t 
		      (let* ((name (bindingform-name parameter))
			     (sp (when (consp parameter) (third parameter)))
			     (advancep (< index max))
			     (check (or keyp restp advancep))
			     (rest (when check
				     (if (plusp cdring)
					 (prog1 
					     `(setq ,rest
						    (nthcdr ,cdring ,rest))
					   (setq cdring 0))
					 rest)))
			     (tmp (when (and check (or sp keyp) (not restp))
				    (gensym (symbol-name (or sp name))))))
			(when tmp (push `(,tmp ,(if keyp
						    `(member ',name ,rest)
						    rest))
					bindings))
			(push `(,name
				,(cond
				  (restp (setq restp nil restvar name) rest)
				  (keyp `(if ,tmp (car ,tmp)
					     ,(bindingform-initform parameter)))
				  (check `(if ,rest
					      (pop ,rest)
					      ,(if optional-p
						   (bindingform-initform parameter)
						   `(missing-args ',name))))
				  (t (bindingform-initform parameter))))
			      bindings)
			(when sp (push `(,sp ,(if check tmp nil)) bindings))
			(when advancep (incf index))))))
	  (values (nreverse-list bindings) whole restvar))
	(values nil args nil))))
			     

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; INITARG CHECKING

;;; Potential Optimization:
;;; If the instance constructor optimization is implemented
;;; (i.e. special case when no user-defined initialization methods),
;;; then we won't always need this hair at run time and it can be moved to
;;; clos-define. 


#| INITARG CHECKING is really gross.
1. Initarg checking is performed by four generic functions:
     MAKE-INSTANCE,
     REINITIALIZE-INSTANCE,
     UPDATE-INSTANCE-FOR-REDEFINED-CLASS,
     UPDATE-INSTANCE-FOR-DIFFERENT-CLASS.
2. Specifying :ALLOW-OTHER-KEYS explicitly to any of these four turns
   off initarg checking for that call.
3. The four methods accept method initargs based on the lambda-list of
   applicable methods for SHARED-INITIALIZE.  MAKE-INSTANCE also
   considers applicable methods for ALLOCATE-INSTANCE and
   INITIALIZE-INSTANCE.  The other three also consider applicable
   methods for the initarg checking generic function that is actually
   executing.  In all cases, the applicable methods are considered
   regardless of whether that method is actually called.
4. The presence of &allow-other keys in a considered method turns off
   initarg testing for that call.
5. When testing is performed, each initarg must be either declared as
   an initarg in an effective-slot-definition in the class or declared
   as a keyword argument to one of the considered methods.
|#

;;; Computes the applicable methods specified by call list, collecting
;;; up the &key arguments from each.  If any specify &allow-other-key,
;;; we quit.  Otherwise, we make sure that each initarg is named by a
;;; valid key.
(defun check-initargs-1 (class initargs valid-keys &rest call-list)
  (declare (dynamic-extent call-list))
  (dolist (call call-list)
    (dolist (method (compute-applicable-methods
		     (fdefinition (car call))
		     (rest call)))
      (multiple-value-bind (keys allow-other-keys-p)
	  (function-keywords method)
	(when allow-other-keys-p (return-from check-initargs-1))
	(setq valid-keys (append keys valid-keys)))))
  (let ((bad (find-bad-key initargs valid-keys)))
    (when bad (key-not-allowed bad "for class ~s" class))))
