;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                OPERATIONS ON FORMS                          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun WALK-CONSTANT (form dynamic-env eval-targets declarations)
  (typecase dynamic-env			;before method
    (COMPILER-ENV
     (confirm-unconfirmed-types form #'typep declarations)))
  (make-result dynamic-env
	       (target (intern-constant dynamic-env form)
		       dynamic-env eval-targets declarations)))

(defun WALK1 (form lexical-env dynamic-env eval-targets declarations)	 
  (restart-case
      (typecase form
	(SYMBOL
	 (multiple-value-bind (binding-type binding variable-declarations)
	     (variable-information form lexical-env)
	   (walk-read binding binding-type form
		      lexical-env dynamic-env eval-targets
		      (known-type dynamic-env
				  (getf variable-declarations 'type)
				  (combine-declarations
				   declarations
				   variable-declarations
				   (or binding form) dynamic-env)))))
	(CONS
	 (let ((operator (car form)))
	   (if (or (symbolp operator) (car-eq operator 'lambda))
	       (multiple-value-bind (binding-type binding function-declarations)
		   (function-information operator lexical-env)
		 (walk-combination binding binding-type form lexical-env dynamic-env
				   eval-targets
				   (combine-declarations
				    function-declarations declarations
				    form dynamic-env)))
	       (signal-program-error
		"~s is not recognized in the function position of a form."
		operator))))
	(t				;literal
	 (walk-constant form dynamic-env eval-targets declarations)))
      (continue-processing (condition-or-value &optional interactivep)
	:interactive (lambda ()
		       (write-string "New form to be evaluated: " *debug-io*)
		       (list (walk (read *debug-io*) lexical-env
				    dynamic-env eval-targets
				    declarations)
			     t))
	:report (lambda (s)
		  (format s
			  "~@<Supply a new form to be evaluted instead of ~:_~:w~:>."
			  form))
        (if interactivep condition-or-value
	    (make-error dynamic-env form condition-or-value)))))


  
(defun WALK-SEQUENCE (forms lexical-env dynamic-env eval-targets
			    declarations)
  (labels ((walk-sequence (forms)
	     (if (cdr forms)
		 (combine-results
		  dynamic-env
		  (walk (first forms) lexical-env dynamic-env
			(make-targets) nil)
		  (walk-sequence (rest forms)))
	       (walk (first forms) lexical-env dynamic-env eval-targets
		     declarations))))
    (walk-sequence forms)))

;;; This represents assignment (i.e. not initial binding) to a
;;; "variable" that may, in fact, turn out to be symbol-macro,
;;; constant, etc.
(defun WALK-ASSIGNMENT (variable value lexical-env dynamic-env
				 eval-targets form-declarations)
  (multiple-value-bind (binding-type binding variable-declarations)
      (variable-information variable lexical-env)
    ;; The declarations code makes sure that SETQ expressions return only
    ;; a single value when their result is used in a multiple-values
    ;; collecting form.
    (walk-write (unless (eq binding-type :special) binding)
		binding-type variable value
		lexical-env dynamic-env eval-targets
		(combine-declarations
		 `(single-value t ,@variable-declarations)
		 form-declarations (or binding variable) dynamic-env))))


;;; Special forms are defined with specializations on NAME, which will
;;; take priority over specializations on key.  This means, for
;;; example, that a special form will be handled correctly, even if it
;;; happens to also be defined as a global macro.

(defun WALK-GLOBAL-COMBINATION (name key form
				     lexical-env dynamic-env
				     eval-targets declarations)
  (let ((special (and (symbolp name) (special-operator name))))
    (when special (setq key :special))
    (case key
      (:SPECIAL
       (funcall special form lexical-env dynamic-env
		eval-targets declarations))
      (:MACRO				;global macros
       (walk-macro (global-macro-function name) form lexical-env dynamic-env
		   eval-targets declarations))
      (t;; This default method handles normal function calls.
       (walk-funcall name (rest form) lexical-env dynamic-env eval-targets
		     declarations)))))

	 
(defun WALK-LAMBDA (args lambda-form lexical-env dynamic-env
			 eval-targets declarations)
  (destructuring-bind (lambda-list &rest body) (rest lambda-form)
    (let* ((evalp (listp args))
	   (name (if evalp (make-symbol "ARGS") args))
	   (env (add-variable name lexical-env dynamic-env))
	   (temp (get-binding name env))
	   (varargsp (file-compiler-env-p dynamic-env)))
      (when evalp (setf (binding-value temp) args))
      (multiple-value-bind (bindings checks decls1)
	  (parse-lambda lambda-list name :varargs varargsp)
	#+no-longer-needed (when varargsp (setq *varargs-binding* (caar bindings)))
	(multiple-value-bind (decls2 body doc) (find-declarations body nil)
	  (values
	   (walk-bindings bindings (append decls1 decls2) body
			  (unless evalp temp) checks
			  env dynamic-env eval-targets declarations) 
	   lexical-env doc))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                    MACROEXPANSION                            ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun WALK-MACRO (macro-function form lexical-env dynamic-env
				  eval-targets declarations)
  (walk (expand macro-function form lexical-env)
	lexical-env dynamic-env eval-targets declarations))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                     OPERATIONS ON VARIABLES                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun LITERAL-CONSTANT-P (variable declarations)
  (declare (ignore declarations))
  (or (keywordp variable)
      (eql variable 't) (eql variable 'nil)
      t))		;Should reference OPTIMIZE from declarations!!!

(defun UNKNOWN-VARIABLE (dynamic-env variable)
  (compiler-style-warn dynamic-env variable 'function
		       #"Variable ~s is unknown and assumed special."
		       variable))


(defun READ-VARIABLE (binding variable dynamic-env eval-targets
			      declarations)
  ;; around method for compiler
  (when (and (compiler-env-p dynamic-env) (eql binding :constant)
	     (literal-constant-p variable declarations))
    (return-from read-variable
      (walk-constant (symbol-value variable)
		     dynamic-env eval-targets declarations)))
  ;; before method for compiler
  (when (and (compiler-env-p dynamic-env) (null binding))
    (unknown-variable dynamic-env variable))
  (make-result
   dynamic-env
   (target (read-value dynamic-env binding variable)
	   dynamic-env eval-targets declarations)))

(defun WRITE-VARIABLE (binding variable value-form lexical-env
			       dynamic-env eval-targets declarations)
  ;; before method for compiler
  (when (and (compiler-env-p dynamic-env) (null binding))
    (unknown-variable dynamic-env variable))
  (walk value-form lexical-env dynamic-env
	(add-target
	 (if (not (binding-p binding))
	     (make-global-target :binding variable
				 :declarations declarations)
	   (make-lexical-target :binding binding
				:declarations declarations))
	 eval-targets)
	declarations))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;             OPERATIONS ON BINDINGS                           ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun WALK-READ (binding binding-type variable lexical-env dynamic-env
			  eval-targets declarations)
  (case binding-type
    (:SYMBOL-MACRO
     (walk-macro (get-symbol-macro-function binding variable)
		 variable lexical-env dynamic-env eval-targets
		 declarations))
    (t					;local or global variable
     (read-variable (or binding binding-type) variable 
		    dynamic-env eval-targets declarations))))


(defun WALK-WRITE (binding binding-type variable value-form
			   lexical-env dynamic-env
			   eval-targets declarations)
  (case binding-type
    (:SYMBOL-MACRO
     (walk `(setf ,(expand (get-symbol-macro-function binding variable)
				   variable lexical-env)
			  ,value-form) 
	   lexical-env dynamic-env eval-targets declarations))
    (:CONSTANT
     (error 'program-error
	    :format-control "Attempt to assign ~s to the constant ~s."
	    :format-arguments (list value-form variable)))
    (t (write-variable (or binding binding-type) variable value-form
		     lexical-env dynamic-env eval-targets declarations))))

;;; WALK-COMBINATION is specialized for local MACRO-BINDINGS and
;;; FUNCTION-BINDINGS, as these take precedence over compiler-macros,
;;; special forms, global macros or functions.  If these aren't
;;; applicable, then the default method for global definitions comes
;;; into play.
(defun WALK-COMBINATION (binding binding-type form
				 lexical-env dynamic-env eval-targets
				 declarations)
  (typecase binding
    (MACRO-BINDING
     (walk-macro (binding-value binding) form lexical-env dynamic-env
		 eval-targets declarations))
    (LEXICAL-FUNCTION
     (walk-funcall binding (rest form) lexical-env dynamic-env
		   eval-targets declarations))
    
    (NULL
     ;; Not a local function or local macro.  Compilers have an around
     ;; method to handle compiler-macros.
     (let ((fname (car form)))
       (when (compiler-env-p dynamic-env) ;:around method
	 ;; Handle compiler macros.  This takes priority over
	 ;; special-forms, global macros, and funcalls, but NOT over
	 ;; local functions and local macros.
	 (let* ((expander (and (not (car-eq fname 'lambda))
			       (not (eq (getf declarations 'inline)
					'notinline))
			       (compiler-macro-function fname)))
		(expansion (and expander
				(expand expander form lexical-env))))
	   (if (or (null expander) (eql expansion form))
	       nil			;(call-next-method)
	       (return-from walk-combination
		 (walk expansion lexical-env dynamic-env eval-targets
		       declarations)))))
       (walk-global-combination fname binding-type form lexical-env
				dynamic-env eval-targets declarations)))))

;;; Series bindings
(defun updated-declarations (exitp declarations lexical-env n-specials)
  (if exitp
      (let ((declarations (copy-list declarations)))
	(incf (getf declarations 'controls 0) n-specials)
	declarations)
      declarations))


(defun WALK-BINDINGS (bindings decls body parse checks lexical-env dynamic-env
			       eval-targets declarations)
  (let (vars init-envs (n-specials 0)
	     (inits (when parse (make-op dynamic-env 'begin-parse parse)))
	     (exitp (find-if #'exit-target-p eval-targets)))
    (multiple-value-bind (body-env init-env)
	(add-variables (mapcar #'bindingform-name bindings)
		       lexical-env dynamic-env decls t)
      (do ((bindings bindings (cdr bindings)) ;Reorder init-env
	   (env init-env (frame-env env)))
	  ((null bindings))
	(push env init-envs))
      (let ((results
	     (make-protected
	      dynamic-env
	      #'(lambda ()
		  (dolist (binding bindings)
		    (let* ((env (pop init-envs))
			   (var (get-binding (bindingform-name binding) env))
			   (initform (bindingform-initform binding)))
		      (if (special-binding-p var)
			  (incf n-specials)
			  (push var vars))
		      (if (eq initform 'unbound-flag)
			  (setf (getf (binding-declarations var) 'dirty) t)
			(setq inits
			      (combine-results
			       dynamic-env inits
			       (walk-binding var initform (frame-env env)
					     dynamic-env nil))))))
		  (setq inits (combine-results
			       dynamic-env inits
			       (walk-sequence checks body-env dynamic-env
					      (make-targets) nil)))
		  (combine-results
		   dynamic-env
		   (if parse
		       (make-block
			dynamic-env nil inits (make-op dynamic-env 'end-parse
						       parse))
		       inits)
		   (walk-sequence body body-env dynamic-env eval-targets
				  (updated-declarations exitp declarations
							body-env n-specials)))) 
	      #'(lambda ()
		  (unless exitp (make-unwind dynamic-env n-specials))))))
	(make-block dynamic-env vars nil results)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                  ARGUMENT/INIT PROCESSING                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Arguably, either HOST-FUNCTION-P or FTYPE-DECL should expand the
;;; type-specifier.  This would provide a limited in way in which
;;; programmers could extend the syntax for ftype specifiers.

(defun host-function-p (type)
  (or (eq type 'ec:macro) (eq type 'ec:function)))

(defparameter *default-ftype-declaration* '(function (&rest t) t))
;;; IWBNI this intersected all ftype declarations.  This
;;; implementation only returns the first (innermost) one.  This is
;;; legal, but could be better.
(defun FTYPE-DECL (declarations)
  (getf declarations 'ftype *default-ftype-declaration* ))

(defun KEY-TYPE (name keylist)
  (dolist (pair keylist)
    (when (and (not (eq pair '&allow-other-keys))
	       (eq (first pair) name))
      (return (second pair)))))



;;; Walk each argument form to a temporary binding, collecting results
;;; and bindings.  All the hair is for type checking the bindings
;;; against ftype declarations.  It might be better to compile to
;;; temporaries, and then use the type-check as a predicate to
;;; determine whether a c:function call is acceptable.  It depends on
;;; how we handle temporaries and what we do with inappropriate C
;;; calls.

;;; IWBNI we checked the types of arguments for lisp:functions, but
;;; DID NOT introduce run-time type checks in compiled-code that were
;;; repeated inside the function.  For now, we don't type-check
;;; lisp:functions, only c:functions and c:macros.

(defparameter *arg-names*
  '#.(loop for i to call-arguments-limit
	   collect (make-symbol (princ-to-string i))))
(defun WALK-ARGUMENTS (designator forms lexical-env dynamic-env
				  declarations)
  (destructuring-bind (&optional ftype argtypes returntypes
				 &rest ignore)
      (ftype-decl declarations)
    (declare (ignore ignore))
    (let ((check-types-p (host-function-p ftype))
	  (return-type (return-type returntypes))
	  inits optionalp restp)
      (labels ((warning (msg &rest args)
			(setq return-type nil)
			(apply #'compiler-style-warn dynamic-env designator 'function
			       msg args) (setq restp t))
	       (next-type () 
			  (cond (restp restp)
				(t (let ((type (pop argtypes)))
				     (case type
				       (&optional (setq optionalp t) (next-type))
				       (&key (setq restp t)
					     (return-from next-type))
				       (&rest (setq restp (next-type)))
				       ((nil) (warning
					       "Too many arguments for ~s." designator))
				       (t type)))))))
	(let* ((env (loop for name in *arg-names*
			  ;; FORM is used to terminate loop
			  and form on forms
			  with types = nil
			  collect name into names
			  do (let ((type (next-type)))
			       (when (and type check-types-p (not (eq type 't)))
				 (push (list 'type type name) types)))
			  finally (return (add-variables
					   names lexical-env dynamic-env
					   types))))
	       ;; N.B.: Assumes first frame contains bindings and that
	       ;; they are in forward order!
	       (bindings (frame-bindings env)))
	  ;; N.B.: subforms of a top-level form are NOT at top-level,
	  ;; so arguments must NOT be processed at top-level. The
	  ;; easiest way to ensure this is to process them in env,
	  ;; instead of lexical-env!
	  (dolist (form forms) 
	    (push (walk-binding (pop bindings) form env dynamic-env nil) inits))
	  (when (and argtypes (not (or restp optionalp))
		     (not (lambda-list-keyword-p (pop argtypes))))
	    (warning "Not enough arguments for ~s." designator))
	  (values (nreverse inits) (frame-bindings env) return-type))))))

;;; This represents initial binding (i.e. not general assignment) to a
;;; true binding (i.e. not a symbol-macro or constant).
;;; 
;;; Similar to WALK-ASSIGNMENT, but takes a VARIABLE BINDING rather
;;; than a symbol naming the variable.  In addition, if binding is a
;;; special, then the right thing happens.  For serial bound specials,
;;; this means first DBINDing the variable.  For parallel bound
;;; specials, this means assigning the value as a lexical binding, and
;;; returning this binding as a second value to indicate that the
;;; caller must later arrange to DBIND the symbol and assign the value
;;; of the binding to the symbol-value, after all the normal parallel
;;; bindings have been made.  
(defun walk-binding (binding valueform lexical-env dynamic-env
			     parallel-p) 
  (let ((name (binding-name binding))
	(variable-declarations
	 (combine-declarations (binding-declarations binding) nil
			       binding dynamic-env)) 
	(targets (make-targets)))
    (if (getf variable-declarations 'special)
	(if parallel-p
	    (values (write-variable binding name valueform lexical-env
				    dynamic-env targets variable-declarations)
		    binding)
	    (values
	     (combine-results
	      dynamic-env
	      (make-op dynamic-env 'dbind (intern-constant dynamic-env name))
	      (write-variable :special name valueform lexical-env
			      dynamic-env targets variable-declarations))
	     nil))
	(values (write-variable binding name valueform lexical-env
				dynamic-env targets variable-declarations)
		nil))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                            FUNCTIONS                         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun make-byte-compiled-function (&rest keys)
  (declare (dynamic-extent keys))
  (apply #'make-instance 'byte-compiled-function keys))

(defmethod print-object ((object packaged-function) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (write (or (function-name object)
	       (function-lambda object))
	   :stream stream)))

;;; The implementation for machine-compiled functions is arguably
;;; wrong.  It is possible that the source for some function could be
;;; recorded without the definition being installed.
;;; IWBNI we used a common representation for non-machine-compiled
;;; functions. 
(defmethod FUNCTION-LAMBDA-EXPRESSION ((fn packaged-function))
  (values (function-lambda fn)
	  (function-env fn)
	  (function-name fn)))
(defmethod FUNCTION-LAMBDA-EXPRESSION ((fn compiled-function))
  (let* ((name nil)			;!!!
	 (source (function-source name)))
    (if source
	(values source nil name)
	(values nil t name))))

;;; FUNCTION NAMES, etc:
;;; In Lisp generally:
;;;   Environment Specific:
;;;     Function-Name: a symbol or a list.
;;;     Lambda-Expression: a list begining with lambda,
;;;         used like a function-name.
;;;     Function-Name-Block-Name: a symbol. Either:
;;;         1. a symbol function name
;;;         2. the cadr of a setf function name
;;;     Function-Form: a list of function-name and arguments.
;;;     Lambda-Form: a list of lambda-expression and arguments.
;;;   Global:
;;;     Function-Designator: a symbol or a function object.
;;;     Extended-Function-Designator: a function-name or a function
;;;         object.
;;; In Eclipse:
;;;   Environment Specific:
;;;     fname: a function-name or #:lambda.
;;;   Global:
;;;     extended-function-name: a list of fnames, most specific last.
;;;         NB: 1. always a list of at least one element.
;;;             2. globally identifies a function.
;;;             3. used in reporting, but not accepted by fdefinition.

(defun MACHINE-FUNCTION-P (dynamic-env function-name declarations)
  (declare (ignore function-name))
  (typecase dynamic-env
    (compiler-env (host-function-p (first (ftype-decl declarations))))))

(defun DIRECT-FUNCTION-P (dynamic-env function-name declarations)
  (declare (ignore declarations))	;Should check speed, etc.
  (typecase dynamic-env
    (compiler-env
     (and (member (symbol-package (function-name-key function-name))
		  (load-time-value
		   (list (find-package :eclipse)
			 (find-package :cl)
			 #+lisp-host (find-package :host))))
	  (fboundp function-name)
	  (not (typep (fdefinition function-name) 'generic-function))))))

(defun VALUES-FUNCTION-P (dynamic-env function-name declarations)
  (declare (ignore function-name))
  (typecase dynamic-env
    (compiler-env (car-eq (third (getf declarations 'ftype)) 'values))))

;;; General ANSI "functional evaluation" converts a
;;; function-name/lambda-expression into a function-value, using the
;;; environment if necessary to check for local definitions.  The
;;; evaluation occurs implicitly for the heads of function-forms or
;;; explicitly by the FUNCTION special form.

;;; This function converts an extended-function-designator or
;;; lambda-expression to a function reference -- i.e. to one of:
;;; - a local function binding holding a closure.
;;; - the result of forming a closure (over a lambda-expression).
;;; - the result of a call to fdefinition to obtain a closure.
;;; - a reference to a machine (i.e. C) function.
;;; Up to three values are returned:
;;;  1. The reference.
;;;  2. The call type: :machine/:direct/nil
;;;  3. A no-values flag.  A non-null value indicates that the call does
;;;  not perform the usual side-effects on the multiple-values machinery.

(defun make-used (obj)
  (setf (getf (binding-declarations obj) 'used) t)
  (when (captured-function-p obj)
    (make-used (captured-function-binding obj))))

(defun FUNCTION-REFERENCE (dynamic-env obj lexical-env declarations
				     &optional check-machine-p)
  (cond ((lexical-function-p obj)
	 (typecase dynamic-env
	   (compiler-env (make-used obj)
			 obj)
	   (t (binding-value obj))))
	((car-eq obj 'lambda)
	 (closure-value dynamic-env nil obj lexical-env declarations t))
	((and check-machine-p
	      (setq check-machine-p (compiler-env-p dynamic-env))
	      (machine-function-p dynamic-env obj declarations))
	 (values obj :machine
		 (not (values-function-p dynamic-env obj declarations))))
	((and check-machine-p
	      (direct-function-p dynamic-env obj declarations))
	 (values obj :direct nil))
	(t (make-fdefinition dynamic-env obj))))
  

(defun WALK-FUNCTION-DEFINITION (function-name lambda-list body privatep
					       lexical-env dynamic-env
					       eval-targets
					       declarations
					       &optional fdef-set)
  (make-result dynamic-env
	       (target (closure-value dynamic-env function-name
				      `(lambda ,lambda-list ,@body)
				      lexical-env declarations privatep
				      fdef-set)
		       dynamic-env eval-targets
		       (known-type dynamic-env 'function declarations))))

(defun CLOSURE-VALUE (dynamic-env name lambda-form lexical-env declarations
				  &optional privatep fdef-set)
  (typecase dynamic-env
    (COMPILER-ENV
     (let* ((boundary (find-boundary-env lexical-env))
	    (enclosing-ids
	     (if boundary
		 (boundary-env-functions boundary)
		 (error "No function closure environment found in ~a." lexical-env)))
	    (this-id (or name 'lambda))
	    (id (cons this-id (boundary-env-id boundary)))
	    (count (count this-id enclosing-ids :test #'name-clash))
	    ;; This should be passed in, to properly reflect flet!!!
	    #+old-code
	    (declarations (nth-value 2 (function-information
					name lexical-env)))
	    (ftype (getf declarations 'ftype))
	    (return-type (return-type (third ftype)))
	    (decs (when return-type
		    (add-type-to-declarations
		     return-type nil lambda-form dynamic-env))))
       (unless (zerop count) (push count id))
       (setf (boundary-env-functions boundary)
	     (cons this-id enclosing-ids))
       (let* ((new-boundary (make-boundary-env :id id :env
					       lexical-env))
	      (targ (make-function-return :binding id))
	      (targs (add-target targ)))
	 (multiple-value-bind (result arg-bindings)
	     (case (first ftype)
	       (ec:function
		(walk-c-function (second lambda-form)
				 (second ftype)
				 (cddr lambda-form)
				 new-boundary dynamic-env targs decs))
	       (t (values (walk-lambda 'ec::ap lambda-form new-boundary
				       dynamic-env targs decs)
			  nil)))
	   (let* ((id (cons 'function (reverse id)))
		  (bindings (boundary-env-enclosed-bindings
			     new-boundary))
		  (n (length bindings)))
	     (let ((def `(,privatep
			  ,(implementation-type dynamic-env return-type)
			  ,id ,arg-bindings ,bindings
			  ,@result)))
	       (if fdef-set
		   (push def (cdr fdef-set))
		   (push def (compiler-env-functions dynamic-env))))
	     `(make-closure ,n ,id
			    ,(if (zerop n)
				 null-hook-flag
				 `(address (env-hook ,id)))
			    ,@(mapcar #'(lambda (b) `(address ,b))
				      bindings)))))))
    (t					;INTERPRETER
     ;; This hack removes superflous environment bindings that may
     ;; have been present because the closure was an argument to
     ;; something. 
     (block hack
       (do ((frame lexical-env (frame-env frame)))
	   ((null frame) (setq lexical-env nil))
	 (typecase frame
	   (variable-env
	    (loop for binding in (frame-bindings frame)
		  unless (member (binding-name binding) *arg-names* :test 'eq)
		  do (return-from hack t)))
	   (t (return-from hack t)))))
     (enclose lambda-form lexical-env name))))

(defun WALK-C-FUNCTION (args arg-types body lexical-env dynamic-env
			     eval-targets declarations)
  (multiple-value-bind (decls body) (find-declarations body nil)
    (let* ((body-env (augment-environment
		      lexical-env 
		      :variable args
		      :declare
		      (append decls
			      (mapcar #'list arg-types args))))
	   (bs (frame-bindings body-env)))
      (setf (getf declarations 'supplies-values) t)
      (values 
       `((compound ,@(walk-sequence body body-env dynamic-env eval-targets
				    declarations)))
       (list bs (make-declare dynamic-env bs))))))
  

;;; MAKE-CALL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Once upon a time, we tried to grovel over this to eliminate
;;; temporaries where possible.  Now we use the general temporary
;;; variable elimination machinery.
(defun simple-val (binding init)
  (destructuring-bind (first-instruction &rest more-instructions) init
    (when (and (not more-instructions)
	       (consp first-instruction))
      (destructuring-bind (&optional assignment var val &rest more)
	  first-instruction
	(when (and (not more)
		   (eq assignment 'setq)
		   (eq var binding))
	  val)))))

(defun pretty-arguments (bindings inits)
  (let ((tail (last bindings)))
    (when tail
      (let* ((binding (first tail))
	     (val (simple-val binding (first (last inits)))))
	(when (and val (typecase val
			 ((or symbol constant-id ec:void) )
			 (binding )
			 (t t)))
	  (setf (getf (binding-declarations (car tail))
		      'call-eliminable) t)))))
  ;; In addition to being to fancy, the following is WRONG because
  ;; the real last-call to appear at this point to be just a reference
  ;; to a non-dirty temporary variable computed earlier.  The
  ;; following code would skip over the temporary variable reference,
  ;; and make some call to the left of it be the last-call.  This
  ;; "earlier" call would then get eliminated (i.e. appear inline in
  ;; this call) and so we would have the side-effects for the left
  ;; call after the side-effects for the right call.
  #+wrong				
  (loop with last-call = nil
	for binding in bindings
	and init in inits
	do (destructuring-bind (first-instruction &rest more-instructions) init
	     (when (and (not more-instructions)
			(consp first-instruction))
	       (destructuring-bind (&optional assignment var val &rest more)
		   first-instruction
		 (when (and (not more)
			    (eq assignment 'setq)
			    (eq var binding))
		   (typecase val
		     ((or symbol constant-id ec:void) )
		     (binding (setq last-call nil))
		     (t (setq last-call binding)))))))
	finally (when last-call
		  (setf (getf (binding-declarations last-call)
			      'call-eliminable) t)))
  (values bindings
	  (loop for init in inits nconc init)
	  bindings))

(defun MAKE-CALL (dynamic-env function arg-bindings arg-inits
			      eval-targets declarations
			      &optional machine-call-p)
  (typecase dynamic-env
    (COMPILER-ENV
     (multiple-value-bind (bindings inits references)
	 (pretty-arguments arg-bindings arg-inits)
       (when (and machine-call-p (consp function))
	 (setq function `(function ,function)))
       (make-block dynamic-env bindings inits
		   (make-result dynamic-env
		     (target (case machine-call-p
			       (:machine `(,function ,@references))
			       (:direct `(,function ,@references ,eoa-flag))
			       (t `(call ,function ,@references)))
			     dynamic-env eval-targets declarations)))))
    (t					;INTERPRETER
     (interpreted-call
      function arg-inits dynamic-env eval-targets declarations))))

(defun INTERPRETED-CALL (function args dynamic-env eval-targets
				  declarations &optional resetp)
  (typecase function
    (INTERPRETED-FUNCTION
     (values
      (walk-lambda args
		   (interpreted-function-lambda function)
		   (interpreted-function-env function)
		   dynamic-env eval-targets declarations)))
    (t					;COMPILED-FUNCTION
     (let ((mv-target (find-if #'multiple-values-target-p
			       eval-targets))
	   (values (multiple-value-list (apply function args))))
       (when mv-target
	 (when resetp
	   ;; It is possible that some multiple-values-prog1 has
	   ;; already filled the buffer. This resets it.
	   (setf (multiple-values-target-values mv-target) 'all))
	 (fill-multiple-values values declarations mv-target))
       (target (first values) dynamic-env eval-targets declarations)))))

(defun WALK-FUNCALL (extended-function-designator
		     args lexical-env dynamic-env eval-targets
		     declarations)
  (multiple-value-bind (arg-inits arg-vars return-type)
      (walk-arguments extended-function-designator args lexical-env
		      dynamic-env declarations)
    (multiple-value-bind (func machine-call-p single-value-p)
	(function-reference dynamic-env extended-function-designator
			    lexical-env declarations t)
      (unless (getf declarations 'supplies-values)
	(setf (getf declarations 'supplies-values) (not single-value-p)))
      (when machine-call-p
	(register-call dynamic-env func declarations))
      (make-call dynamic-env
	func arg-vars arg-inits eval-targets
	(known-type dynamic-env return-type declarations)
	machine-call-p))))


(defparameter *preregistered-c-operators*
  '(fdefinition funcall-function symbol-value system-property
		system-property-setter make-closure)) 

(defmethod REGISTER-CALL ((dynamic-env t) (function t) (declarations t)))
