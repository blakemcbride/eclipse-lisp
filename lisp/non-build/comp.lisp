
(defstruct (COMPILER (:conc-name "PROCESSOR-"))
  (undefined-types nil)
  (undefined-functions nil)
  (undefined-variables nil))

;;; Note that these might be bound by WITH-COMPILATION-UNIT to
;;; some general value such as t the value of (make-compiler).
;;; Functions such as COMPILE and COMPILE-FILE should check these
;;; values, and assign a suitable new value if necessarry.
;;; We need to define some semantics for this!!!
(defvar *PROCESSOR*)
(defvar *ENVIRONMENT*)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; COMPILATION UNITS

;;; Belongs in evaluation.lisp!!!
;;; Style-warnings are issued for:
;;; unused variable (not declared ignore) or used variable (declared ignore).
;;; unaesthetic, inefficient, or depracated code
;;; inconsistent arguments (a call vs. declaration, definition vs
;;;   previous declaration or definition)
;;; undefined functions or variables (our interpretation)

;;; WITH-COMPILER-HANDLERS implements the error trapping defined in
;;; "Exceptional Situations in the Compiler" in the ANSI spec.  
;;; It returns three values:
;;;   - primary value from body.
;;;   - whether or not there were ANY notices
;;;   - whether or not there were non-STYLE-WARNING notices.
;;; Note that if a user does not want compilation errors to be trapped
;;; and converted to warnings, they can bind *BREAK-ON-SIGNALS* to
;;; ERROR.

(defmacro xxWITH-COMPILER-HANDLERS (&body body)
  (with-unique-names (notices significant-notices)
    `(let ((,notices nil) (,significant-notices nil))
       (handler-bind ((style-warning #'(lambda (c)
					 (declare (ignore c))
					 (setq ,notices t)))
		      (warning #'(lambda (c)
				   (declare (ignore c))
				   (setq ,notices t
					 ,significant-notices t)))
		      (error #'(lambda (c)
				 (setq ,notices t
				       ,significant-notices t)
				 (report-condition c)
				 (use-value (runtime-error c
							   (current-form)
							   (current-continuation))))))
	 (values (progn ,@body) ,notices ,significant-notices)))))

(defun runtime-error (condition form continuation)
  (process `(execute-compile-time-error ,condition ',form)
	    *processor* *environment* continuation))

(defun execute-compile-time-error (condition form)
  (with-evaluation-frame (form *interpreter* nil nil)
			 (error condition)))
  

;;; Get rid of definition in evaluation.lisp!!!
;;; WITH-COMPILATION-UNIT facilitates sharing of top level processor
;;; and environment contexts across multiple compilation operations.
;;; All the values returned by the body are returned.

;;; *COMPILATION-UNITS* is a plist defining the valid keys for
;;; with-compilation-unit, and defining the name of the variable that
;;; is conditionally bound by with-compilation-unit (i.e. depending on
;;; :override and whether the variable is already bound).  It is
;;; assumed the variables are declarated special.

(defparameter *compilation-units*
  '(:processor *processor* :environment *environment*))


(define-condition SIMPLE-STYLE-WARNING (simple-condition style-warning) ())


;;; With-compilation-unit binds each of the special variables only if
;;; override is true or the variable is not yet bound globally.  When
;;; a variable does get bound by with-compilation-unit, the initial
;;; value will be that specified by as the option value in the
;;; with-compilation-unit form.  Before creating any new special
;;; bindings, all option names and values are evaluated in order,
;;; regardless of whether their values are used to create a new
;;; binding for a variable.

;;; For each special variable that is actually bound by
;;; with-compilation-unit, CLOSE-COMPILATION-UNIT is called after all
;;; the forms in the body have executed.  The current value of the
;;; special variable is passed as the only argument to
;;; close-compilation-unit.  The calls are made in the reverse order
;;; in which the corresponding keys are listed in the
;;; with-compilation-unit form.

;;; If no options are specified, or if only :override is specified,
;;; then the effect is as though :processor t :environment t appeared
;;; in the options.

(defmacro xxWITH-COMPILATION-UNIT ((&rest keys)
				   &body body)
  `(process-compilation-unit (list ,@keys) (lambda () ,@body)))

(defun process-compilation-unit (keys function)
  (loop for (key val) on (if (or (null keys)
				 (and (eq (car keys) ':override)
				      (null (cddr keys))))
			     '(:processor t :environment t)
			     keys)
	by #'cddr
	with symbols = nil and values = nil
	and override = (getf keys :override)
	and seen = '(:override)
	unless (member key seen)
	do (let ((var (or (getf *compilation-units* key)
			  (key-not-allowed key))))
	     (push key seen)
	     (when (or override (not (boundp var)))
	       (push var symbols)
	       (push val values)))
	finally (return (progv symbols values
			  (multiple-value-prog1 (funcall function)
			    (dolist (symbol (nreverse symbols))
			      (close-compilation-unit symbol)))))))

(defmethod CLOSE-COMPILATION-UNIT ((unit t)))

(defmethod CLOSE-COMPILATION-UNIT ((unit COMPILER))
  (flet ((show (id names)
	   (when names
	     (warn 'simple-style-warning
		   :format-control
		   "The following ~a were referenced, but not~
                   defined:~{ ~s~}."
		   :format-arguments (list id  (nreverse names))))))
    (show "special variables" (processor-undefined-variables unit))
    (show "functions" (processor-undefined-functions unit))
    (show "types" (processor-undefined-types unit))))
	

;;; Note that condition is WARNING unless the compilation-unit
;;; mechanism is used, in which case the (deferred) condition is
;;; style-warning.

(defmethod UNKNOWN-BINDING (name kind (processor COMPILER))
  (unless (when (boundp '*processor*)
	    (case kind
	      (:special (push name
			      (processor-undefined-variables *processor*)))
	      (:function (push name
			       (processor-undefined-functions *processor*)))
	      (:type (push name
			   (processor-undefined-types *processor*)))))
    (warn "Unknown ~a ~s." kind name)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LITERALS
;;; Temporary !!!, but make sure that constant-id's are subclasses of literal!
(defstruct (literal (:print-object (lambda (literal stream)
				     (write `',(literal-value literal)
					    :stream stream))))
  (value))
		    
(defmethod PROCESS-LITERAL (literal (processor COMPILER) environment
				     continuation)
  (declare (ignore environment))
  (resume continuation processor (make-literal :value literal)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BINDING ACCESS

;;; !!!

(defun binding-used-p (binding)
  (getf (binding-declarations binding) 'used))

(defun mark-binding-used (binding)
  (setf (getf (binding-declarations binding) 'used) t))

(defun binding-referenced-p (binding)
  (getf (binding-declarations binding) 'referenced))

(defun mark-binding-referenced (binding)
  (setf (getf (binding-declarations binding) 'referenced) t))

(defun binding-written-p (binding)
  (getf (binding-declarations binding) 'used))

(defun mark-binding-written (binding)
  (setf (runtime-value binding) 'UNBOUND-FLAG)
  (setf (getf (binding-declarations binding) 'written) t))

(defun binding-inlinable-p (binding)
  (unless (typep binding 'lexical-function) ;!!!
    (not (eq (runtime-value binding) 'UNBOUND-FLAG)))) ;!!!
  

(defmethod PROCESS-BINDING :BEFORE (name (binding NULL) kind
					 (processor COMPILER)
					 environment continuation)
  (declare (ignore environment continuation))
  (when name				;not reference to NIL
    (unknown-binding name kind processor)))

(defmethod PROCESS-ASSIGNMENT :BEFORE (name value-form
					    (binding NULL) kind
					    (processor COMPILER)
					    environment continuation)
  (declare (ignore value-form environment continuation))
  (unknown-binding name kind processor))


;;; This may not be the best way to do this!!! Alternatives:
;;; + When resuming a literal to a lexical-assignment-cont, store the
;;;   binding-value.  This way, we save the value even if there is a
;;;   complicated expression that ends up being a literal.
;;; + When reading a variable to lexical-assignment-cont, store the
;;;   binding representing the variable in the assignment-cont's
;;;   binding-value.  

(defmethod RESUME :BEFORE ((continuation LEXICAL-ASSIGNMENT-CONT)
			   (processor COMPILER)
			   (value LITERAL))
  (setf (runtime-value (assignment-cont-variable continuation))
	value))

(defmethod RESUME :BEFORE ((continuation LEXICAL-ASSIGNMENT-CONT)
			   (processor COMPILER) value)
  (declare (ignore value))
  (mark-binding-written (assignment-cont-variable continuation)))

(defmethod PROCESS-BINDING :AROUND ((name t) (binding RUNTIME-BINDING)
				     (kind t) (processor COMPILER)
				     environment continuation)
  (declare (ignore environment))
  (mark-binding-used binding)
  (if (binding-inlinable-p binding)
      ;; Binding-value must be a literal
      (resume continuation processor (runtime-value binding))
      (call-next-method)))


(defmethod PROCESS-BINDING :BEFORE ((name t) (binding RUNTIME-BINDING)
				     (kind t) (processor COMPILER)
				     environment continuation)
  (declare (ignore environment continuation))
  (mark-binding-referenced binding))


;;; Inlining function definitions.
;;; What about inlining global definitions???!!!

#+not-yet
(defmethod PROCESS-BINDING :AROUND ((name t)
				    (binding LEXICAL-FUNCTION)
				    (kind t)
				    (processor COMPILER)
				    environment continuation)
  (declare (ignore environment))
  (let ((closure (binding-value binding)))
    (if (and (functionp closure)
	     ;; not declared inline!!!
	     )
	(resume continuation processor
		xxxxx)
	(call-next-method))))
	  

;;; Note that free ignore/ignorable declarations are irrelevant for
;;; supressing warnings about unused bindings -- the binding has to be
;;; used somewhere in its scope, so the test can only be done at the
;;; end of its scope.  Any ignore/ignorable declaration that coincides
;;; with this scope would have to be a bound declaration.  This means
;;; that we can find the declaration in the binding-declarations.
(defmethod RESUME :BEFORE ((continuation RUNTIME-EXTENSION-CONT)
			   (compiler COMPILER) values)
  (declare (ignore values))
  (dolist (binding (runtime-extension-cont-bindings continuation))
    (unless (binding-used-p binding)
      (let ((decs (binding-declarations binding)))
	(unless (or (getf decs 'ignore)
		    (getf decs 'ignorable))
	  (warn 'simple-style-warning
		:format-control "~a is never used."
		:format-arguments (list (binding-name binding))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BASIC EVALUATION

;;; Because this is an around method that is applicable for all
;;; compilation of all forms, it takes precedence over any special
;;; processing defined for a pariticular form.  If you need to
;;; eliminate the possibility of compiler-macros, a specialized
;;; :around method is needed.

(defmethod PROCESS-COMBINATION :AROUND ((operator T) form (binding SYMBOL)
					 (kind t) (processor COMPILER)
					 environment continuation) 
  (let* ((expander (unless (car-eq operator 'lambda)
		     (compiler-macro-function operator)))
	 (expansion (if expander
			(expand-macro expander form environment)
			form)))
    (if (eql expansion form)
	(call-next-method)
	(process expansion processor environment continuation))))

;;; This ONLY does potential compiler-macro substitution, which must
;;; be done on the original form (i.e. before arguments are
;;; processed).  It does not perform other optimizations such as
;;; making direct calls, etc.  These are best handled by methods on
;;; INVOKE, because we want to have the arguments already processed.
(defspecial FUNCALL (func &rest args)
  :processor COMPILER
  :qualifiers (:AROUND)
  (declare (ignore args))
  (when (consp func)
    (destructuring-bind (key &optional op &rest extra) func
      (unless extra
	(let* ((expander
		(case key
		  ((function #-machine-compile cl:function)
		   (unless (car-eq op 'lambda)
		     (compiler-macro-function op environment)))
		  ;; It is not completely clear that ANSI really
		  ;; allows us to call the compiler-macro-function
		  ;; in this case, but we do.
		  (quote (compiler-macro-function op))))
	       (expansion (if expander
			      (expand-macro expander form environment)
			      form)))
	  (unless (eql expansion form)
	    (return-from process-combination
	      (process expansion processor environment continuation)))))))
  (call-next-method))
	

(defmethod RESUME ((continuation CATCH-CONT) (processor COMPILER) tag)  
  (let ((cont (make-labeled-cont (continuation-next continuation)
				 tag)))
    (process-sequence (catch-cont-forms continuation)
		      processor
		      (catch-cont-environment continuation)
		      cont)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; OTHER PROCESSING

#+wrong
(defmethod PROCESS-COMBINATION ((operator (eql 'LOAD-TIME-VALUE))
				 form (binding SYMBOL) (kind t)
				 (processor COMPILER)
				 environment continuation)
  (destructuring-bind (form &optional read-only-p) (cdr form)
    (declare (ignore read-only-p))
    (process-literal (process form *interpreter* nil nil)
		      processor environment continuation)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; RUN-TIME COMPILER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (BYTE-COMPILER (:include compiler)))
(defparameter *BYTE-COMPILER* (make-byte-compiler))

(defmethod COMPILE-OBJECT ((definition CONS) &optional name)
  (destructuring-bind (lambda-list . body) (cdr definition)
    (xxwith-compilation-unit (:processor (make-byte-compiler) ;!!!
		              :environment nil)
      (let ((closure (process-function-definition
		      name lambda-list body
		      *processor* *environment*
		      NULL-CONTINUATION)))
	(setf (slot-value closure 'lambda) definition)
	closure))))
	      
		     

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BASIC EVALUATION

(defmethod RESUME ((continuation IF-CONT) (processor BYTE-COMPILER)
		   values) 
  (let ((environment (if-cont-environment continuation)))
    (resume (continuation-next continuation) processor
	    `(if ,values
		 ,(process (if-cont-consequence continuation)
			    processor environment null-continuation)
		 ,(process (if-cont-alternate continuation)
			    processor environment null-continuation)))))

(defmethod RESUME ((continuation SEQUENCE-CONT) (processor BYTE-COMPILER)
		   value)
  (let ((forms (sequence-cont-forms continuation)))
    (resume (continuation-next continuation) processor
	    `(progn ,value
		    ,@(loop for form in forms
			    and env = (sequence-cont-environment
				       continuation)
			    collect (process form processor env
					     null-continuation))))))

(defmethod COMBINE-VALUES ((processor BYTE-COMPILER) values value)
  (nconc values (list value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BINDING ACCESS

(defmethod PROCESS-BINDING ((name t) (binding RUNTIME-BINDING) (kind t)
			    (processor BYTE-COMPILER) environment continuation)
  (declare (ignore environment))
  (resume continuation processor 
	  (if (binding-name binding)
	      binding
	      (multiple-value-bind (lambda fenv) ; an enclosed lambda expression
		  (function-lambda-expression (runtime-value binding))
		(destructuring-bind (lambda-list . body) (rest lambda)
		  (process-function-definition
		   name lambda-list body
		   processor fenv null-continuation))))))
				    

;;; :special, :constant
(defmethod PROCESS-BINDING (name (binding t) (kind t)
				 (processor BYTE-COMPILER) environment continuation)
  (declare (ignore environment))
  (resume continuation processor `(symbol-value ',name)))


(defmethod PROCESS-BINDING (name (binding T) (kind (eql :FUNCTION))
				 (processor BYTE-COMPILER)
				 environment continuation)
  (declare (ignore environment))
  (resume continuation processor `(fdefinition ',name)))


(defmethod RESUME ((continuation LEXICAL-ASSIGNMENT-CONT) (processor BYTE-COMPILER)
		   value)
  (resume (continuation-next continuation) processor
	  `(assign ,(assignment-cont-variable continuation)
		   ,value)))

(defmethod RESUME ((continuation DYNAMIC-ASSIGNMENT-CONT)
		   (processor BYTE-COMPILER) value) 
  (resume (continuation-next continuation) processor
	  `(set ',(assignment-cont-variable continuation)
		 ,value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FUNCTION DEFINITION

(defcont FUNCTION-CONT exit-cont ())

;;; Rationalize this with process-lambda!!!
;;; Rationalize how the funcallable-standard-instance-function is
;;; initialized for interpreted and byte compiled functions!!!  

(defmethod PROCESS-FUNCTION-DEFINITION (name lambda-list body
					     (processor BYTE-COMPILER)
					     environment
					     continuation)
  (multiple-value-bind (bindings checks decls2)
      (parse-lambda lambda-list *args-name*)
    (multiple-value-bind (decls1 body)
	(find-declarations body t)
      (let* ((env (make-function-frame environment 'unbound-flag))
	     (closure (make-byte-compiled-function
		       :name name :env env
		       :code (process 
			      `(let* ,bindings
				 (declare ,@decls1 ,@decls2)
				 ,@checks ,@body)
			      processor env
			      (make-function-cont
			       null-continuation)))))
	;; This should be the same for all packaged-functions (see
	;; enclose.lisp)!!! Perhaps MAKE-INTERPRETER should really be
	;; the COMPUTE-DISCRIMINATING-FUNCTION method for
	;; packaged-functions? 
	(set-funcallable-instance-function
	 closure (make-interpreter closure))
	(resume continuation processor
		closure)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EXITS

(defmethod RESUME ((continuation LABELED-CONT) (processor BYTE-COMPILER)
		   values)
  (resume (continuation-next continuation)
	  processor
	  `(catch ,(labeled-cont-tag continuation)
	      ,@(byte-compiler-body values))))

;;; IWBNI we processed form to a special throwing continuation so
;;; that, if the tag is known at compile time, we can skip the runtime
;;; search for the tag.  Be careful, though: if we cross a function
;;; boundary and the closure is funcallable in a different context,
;;; than there could be different intervening tags.
(defmethod RESUME ((continuation THROW-CONT) (processor BYTE-COMPILER)
		   tag)
  (resume (continuation-next continuation)
	  processor
	  `(throw ,tag
	      ,(process (throw-cont-form continuation)
			 processor
			 (throw-cont-environment continuation)
			 null-continuation))))

(defmethod RESUME ((continuation BLOCK-CONT) (processor BYTE-COMPILER)
		   values)
  (resume (continuation-next continuation)
	  processor
	  `(catch ,continuation
	     ,@(byte-compiler-body values))))
     
;;; If target-continuation is visible within the lexically known
;;; continuations, then we can unwind to that point manually.
;;; It might not be visible, though, because a lexical closure that
;;; causes this unwind might get funcalled from some unknown compiled
;;; function that it was passed to.  Unwinding would not get us to the
;;; correct place in the CALLER of this function.  In the general
;;; case, we have to throw the values to the target-continuation and
;;; let the dynamic unwinding mechanism do the right thing.

(defmethod PROCESS-UNWIND ((processor BYTE-COMPILER)
			    lexical-continuation values
			    (target-continuation BLOCK-CONT))
  (resume (continuation-next lexical-continuation)
	  processor
	  `(throw ,target-continuation
	     ,@(byte-compiler-body values))))

;;; Potential optimization: implement loop unrolling by resuming
;;; target-continuation! 
(defmethod PROCESS-UNWIND ((processor BYTE-COMPILER)
			    lexical-continuation values
			    (target-continuation JUMP-SET-CONT)) 
  (resume lexical-continuation processor
	  `(throw ,target-continuation ,values)))

(defmethod RESUME ((continuation TAGBODY-CONT) (processor byte-compiler)
		   values)
  (resume (continuation-next continuation)
	  processor `(jump-set ,(tagbody-cont-jump-set continuation)
			       ,@(byte-compiler-body values))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CONTEXT CONTINUATIONS

(defun restore-forms (key continuation processor value)
  (resume (continuation-next continuation) processor
	  `(,key ,value
		 ,@(byte-compiler-body
		    (process-sequence
		     (restore-values-cont-forms continuation)
		     processor
		     (restore-values-cont-environment continuation)
		     null-continuation)))))

(defmethod RESUME ((continuation RESTORE-VALUES-CONT)
		   (processor BYTE-COMPILER) value)
  (restore-forms 'multiple-value-prog1 continuation processor value))

(defmethod RESUME ((continuation CLEANUP-CONT)
		   (processor BYTE-COMPILER) value)
  (restore-forms 'UNWIND-PROTECT continuation processor value))


(defun byte-compiler-body (values)
  (if (car-eq values 'progn)
      (cdr values)
      (list values)))

(defmethod RESUME ((continuation RUNTIME-EXTENSION-CONT)
		   (processor BYTE-COMPILER)
		   values)
  (resume (continuation-next continuation)
	  processor
	  `(frame ,(runtime-extension-cont-bindings continuation)
	     ,@(runtime-extension-cont-inits continuation)
	     ,@(byte-compiler-body values))))


(defmethod RESUME ((continuation PROGV-CONT)
		   (processor BYTE-COMPILER) values)
  (destructuring-bind (symbols values) values
    (resume (continuation-next continuation) processor
	    `(progv ,symbols ,values
	       ,(process-sequence (progv-cont-forms continuation)
				   processor
				   (progv-cont-environment continuation)
				   null-continuation)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FUNCTIONS

(defmethod INVOKE ((function t) values (processor BYTE-COMPILER)
		   environment continuation)
  (declare (ignore environment))
  (resume continuation processor
	  ;; This really depends on optimization settings and notinline!!!
	  (let ((f1 (when (car-eq function 'fdefinition)
		      (second function))))
	    (if (car-eq f1 'quote)
		`(,(second f1) ,@values)
		`(funcall ,function ,@values)))))


(defmethod RESUME ((continuation MV-CALL-CONT) (processor BYTE-COMPILER) values)
  (destructuring-bind (f . arguments) values
    (resume (continuation-next continuation) processor
	    `(multiple-value-call ,f ,@arguments))))

(defspecial LOAD-TIME-VALUE (form &optional read-only-p)
  :processor BYTE-COMPILER
  (declare (ignore read-only-p))
  (process-literal (eval form)
		   processor nil continuation))

;;; Need special LOAD-TIME-VALUE, EVAL-WHEN for FILE-COMPILER.
