;;; See interp.lisp, comp.lisp, byte.lisp, fcomp.lisp, comp.doc
;;;  meta-eval.lisp, expand.lisp


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ERROR CONTEXT
;;;
;;; It is not clear at this point how we want to implement the
;;; following necessary capabilites:
;;; 1. DEBUGGER can produce a BACKTRACE of frames (eg. at minimum,
;;;    forms being interpreted).
;;; 2. DEBUGGER can indicate innermost (interpreted?) FRAME in
;;;    which error occured.
;;; 3. COMPILER can indicate CONTEXT in which error occured (eg. an
;;;    abbreviated backtrace).
;;; 4. DEBUGGER can invoke a (use-value?) RESTART to return a value
;;;    from any frame.  Optionally, IWBNI each frame could also be
;;;    restarted, possibly with different values.
;;; 5. DEBUGGER can report and modify ENVIRONMENT in a given frame.
;;;
;;; This code defines the interface to this mechanism.

;;; Would it make sense to store the evaluation stack in the processor
;;; object (eg. the object pointed by *interpreter*) rather than
;;; within a global variable named *evaluation-frames*??? This might
;;; simplify multi-threading, and allow greater code sharing with Java
;;; process/thread objects.

(defvar *evaluation-frames* nil)

;;; Processes body within a frame in which the given information is
;;; known to the debugger.
(defmacro with-evaluation-frame ((form processor environment continuation)
				 &body body)
  (rebinding (form processor environment continuation)
    `(restart-case (let ((*evaluation-frames* (cons (list ,form ,processor
							  ,environment ,continuation)
						    *evaluation-frames*)))
		     ,@body)
       (use-value (value)
		  :interactive (lambda ()
				 (write-string "New form to be evaluated: " *debug-io*)
				 (list (process (read *debug-io*)
						,processor
						,environment
						,continuation)))
		  :report (lambda (s)
			    (format s
				    (formatter
				     "Supply a new form to be evaluted instead of ~w.")
				    ,form))
		  value))))

(defun current-form ()
  (caar *evaluation-frames*))
(defun current-continuation ()
  (fourth (car *evaluation-frames*)))

;;; :verbose:
;;;   t: full form regardless of length, each on a separate line.
;;;   :default: form with limited printing, each on a separate line.
;;;   nil: "frame name" only, separated by <-.
;;; IWBNI we filtered out macroexpansions, or at least known
;;; macroexpansion junk from defun/defmacro, etc.

;;; IWBNI particular *print-xxx* values should came from documented
;;; globals!!! 

(defun backtrace (stream &key (verbose :default) &aux linep)
  (when *evaluation-frames*
    (flet ((report (frames &aux (form (caar frames)))
		   (princ (if verbose
			      form
			      (bindingform-name form))
			  stream)))
      (progv '(*print-lines* *print-length* *print-level*)
	  (case verbose
	    ((t) (setq linep t) '(20 100 nil))
	    (:default (setq linep t) '(1 6 2))
	    #+(and excl (not machine-compile)) ;excl bug
	    ((nil) '(nil nil nil)))
	(do ((frames *evaluation-frames* next)
	     (next (cdr *evaluation-frames*) (cdr next)))
	    ((null next) (report frames) (values))
	  (report frames)
	  (cond (linep (terpri stream))
		(t (pprint-newline :fill stream)
		   (write-string " <- " stream))))))))

;;; Should be called by report-condition!!!

(defun report-context (stream &optional (verbose nil))
  (when *evaluation-frames*
    (write-string " within " stream)
    (if verbose
	(backtrace stream :verbose nil)
	(pprint-logical-block (stream nil) (write (current-form) :stream stream)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; UTILITIES

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun sequence-constructor1 (name)
    (make-name "MAKE-~a" name)))
  
(defmacro defcont (name include slots)
  `(defstruct (,name (:include ,include)
		     (:constructor ,(sequence-constructor1 name)
				   (,@(mapcar #'slot-definition-name
					      (class-slots (find-class include)))
				      ,@slots)))
     ,@slots))

;;; Makes it easier to see where a null-continuation is being introduced.
(defconstant NULL-CONTINUATION nil)

(defstruct PROCEDURE)			;IWBNI FUNCTION subclassed this!
(defcont CONTINUATION procedure (next))
(defcont EFFECT-CONT continuation ())
(defcont CONTEXT-CONT continuation ())
(defcont SEQUENCE-CONT continuation (forms environment))

(defmethod RESUME ((continuation NULL) (processor T) values)
  values)

(defmethod RESUME ((continuation CONTINUATION) processor values)
  (resume (continuation-next continuation) processor values)) 


;;; DEFSPECIAL defines OPERATOR as a special operator, with the given
;;; LAMBDA-LIST.  Within BODY, the variables FORM, PROCESSOR
;;; ENVIRONMENT and CONTINUATION are bound (i.e. variable-capture is
;;; used). In addition, the variables BINDING and KIND are bound to
;;; the corresponding values returned by FUNCTION-BINDING.  BODY can
;;; optionally begin  with:
;;;   :processor <processor-class-name>
;;;      Makes operator special only for the given processor-class.
;;;   :qualifiers <list-of-qualifiers>
;;;      Defines the corresponding auxiliary method rather than
;;;      a primary method.
;;; See notes on defining special operator methods at the start of the
;;; "FUNCTION REFERENCES" section, below.

(defmacro DEFSPECIAL (operator lambda-list &body body)
  (let ((processor T)
	(qualifiers NIL))
    (do ((bod body (cdr bod)))
	((null bod) (setq body nil))
      (case (car bod)
	(:processor (pop bod) (setq processor (car bod)))
	(:qualifiers (pop bod) (setq qualifiers (car bod)))
	(t (return (setq body bod)))))
    `(defmethod PROCESS-COMBINATION ,@qualifiers
       ((operator (eql ',operator)) form (binding SYMBOL) (kind t) 
	(processor ,processor) environment continuation)
       (declare (ignorable environment))
       (destructuring-bind ,lambda-list (cdr form)
	 ,@body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FORMS
(defmethod PROCESS :AROUND (form processor environment continuation)
  (with-evaluation-frame (form processor environment continuation)
			 (call-next-method)))

(defmethod PROCESS ((form T) processor environment continuation)
  (process-literal form processor environment continuation))

(defmethod PROCESS ((form SYMBOL) processor environment continuation)
  (multiple-value-bind (kind binding)
      (variable-binding processor form environment)
    (process-binding form binding kind
		     processor environment continuation)))

(defmethod PROCESS ((form CONS) processor environment continuation)
  (let ((operator (car form)))
    (multiple-value-bind (kind binding)
	(function-binding processor operator environment)
      (process-combination operator form binding kind			    
			   processor environment continuation))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ENVIRONMENT INFORMATION
;;;
;;; Used by PROCESS-BINDING and PROCESS-COMBINATION.
;;; This use differs slightly from VARIABLE-INFORMATION and
;;; FUNCTION-INFORMATION as defined by Steele:
;;;   KIND identifies kind of binding desired in lisp-n.  Can't be
;;;     nil.  For example, in lisp-1 systems, names can refer to only
;;;     one kind of binding.  In lisp-2, etc., there are :variable,
;;;     :function, :special, :constant, :macro, :symbol-macro, ...
;;;   BINDING represents the binding in the processing environment.
;;;     It might be:
;;;     - an environment binding,
;;;     - a name (i.e. symbol or list), 
;;;     - :internal or some other symbol to indicate that the binding
;;;       is not user accessible: special operators, c:macros, etc.
;;;     - nil if the binding is unknown in env.
;;;   NAMED-FUNCTION is a :special for function-binding.
;;;   User-written code walkers need to be able to count on Steele's
;;;   FUNCTION-INFORMATION returning :special only for ANSI special
;;;   operators.

;;; !!! Issue: Does it make sense to get rid of KIND and encapsulate
;;; "bindings" like NIL and names within a true binding object that
;;; identify their class?  We should examine the impact on
;;; process-binding and other methods that might specialize on kind.
;;;
;;; If this happens, than process-binding might not need the name
;;; argument, and can just be a specialization of PROCESS, rather than
;;; a separate function.  The latter definitions of expand-macro also
;;; go away.  Note that in order to make special forms (eql methods on
;;; process-combination), we need to have undefined-global-function be
;;; a subclass of global-function.  We may need something similar for
;;; constants??? UNKNOWN-BINDING changes in compiler.

(defmethod FUNCTION-BINDING ((processor t) name env)
  (if (car-eq name 'lambda)
      ;; We don't call process-function-definition here, because:
      ;; 1. Function-binding should be side effect free -- it should
      ;;    not create new closure definitions in the processor.
      ;; 2. The closure may be inlined, in which case
      ;;    process-function-definition is unecessary.
      ;; Note that :name is nil, which cannot normally happen.
      (values :function
	      (make-lexical-function :name nil
				     :value (enclose name env))
	      nil)
      (case name
	(named-function (values :special :special nil))
	(t (multiple-value-bind (kind binding declarations)
	       (function-information name env)
	     (values (or kind :function)
		     (case kind
		       (:special :special)
		       ((nil) (case (car (getf declarations 'ftype))
				    ((ec:function ec:macro) :internal)
				    (t (or binding name))))
		       (t (or binding name)))
		     declarations))))))

;;; Issue: Might the caller want to know that a variable is special
;;; but bound locally?

(defmethod VARIABLE-BINDING ((processor t) name env)
  (multiple-value-bind (kind binding declarations)
      (variable-information name env)
    (values (or kind :special)
	    (when kind
	      (or (unless (eq kind :special) binding)
		  name))
	    declarations)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; REFERENCING BINDINGS
;;;
;;; Each processor must define methods for:
;;; 
;;; PROCESS-BINDING (name binding kind processor environment continuation)
;;; Processes the value of the binding named by name.
;;;   Name examples: X, T, NIL, LIST, (setf <name>), (lambda ....)
;;;   Binding examples:
;;;     an environment binding of various kinds,
;;;     a name through which the top-level binding can be extracted,
;;;     NIL if undefined.
;;;   Kind examples: :lexical, :special, :constant, :function, :macro,
;;;     :symbol-macro (Never NIL in Common Lisp.)

;;; Find root binding in a captured chain.
(defmethod root-binding ((binding binding)) binding)
;; IWBNI binding used multiple inheritance so this was just one method.
(macrolet ((defcaptured (name accessor)
  `(defmethod root-binding ((binding ,name))
     (root-binding (,accessor binding)))))
  (defcaptured captured-var captured-var-binding)
  (defcaptured captured-function captured-function-binding)
  (defcaptured captured-block captured-block-binding)
  (defcaptured captured-tag captured-tag-binding))

;;; Like binding-value, but follows captured binding chains.
(defun runtime-value (binding)
  (binding-value (root-binding binding)))
(defun (setf runtime-value) (new-value binding)
  (setf (binding-value (root-binding binding)) new-value))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ASSIGNMENT
;;;
;;; PROCESS-ASSIGNMENT process the value-form with the binding named
;;; by name added to the current continuation.  The arguments are
;;; similar to PROCESS-BINDING except for the addition of
;;; value-form. 
;;; PROCESS-INITIALIZATION is the same as PROCESS-ASSIGNMENT, but is
;;; used when first initializing a binding (eg. in a LET binding form).

(defcont ASSIGNMENT-CONT effect-cont (variable))
(defcont DYNAMIC-ASSIGNMENT-CONT assignment-cont ())
(defcont LEXICAL-ASSIGNMENT-CONT assignment-cont ())
(defcont INITIALIZATION-CONT lexical-assignment-cont ())

(defmethod PROCESS-ASSIGNMENT ((name t) value-form
			       (binding RUNTIME-BINDING) (kind T)
			       processor environment continuation)
  (process value-form processor environment
	   (make-lexical-assignment-cont continuation binding)))

(defmethod PROCESS-INITIALIZATION (value-form
				   (binding RUNTIME-BINDING) 
				   processor environment continuation)
  (process value-form processor environment
	   (make-initialization-cont continuation binding)))

(defmethod PROCESS-ASSIGNMENT (name value-form
				    (binding T) (kind (eql :SPECIAL))
				    processor environment
				    continuation)
  (process value-form processor environment
	   (make-dynamic-assignment-cont continuation name)))

(defmethod PROCESS-ASSIGNMENT (name value-form
				    binding (kind (eql :CONSTANT))
				    processor environment continuation)
  (declare (ignore binding processor environment continuation))
  (error 'control-error :format-control
	 "Attempt to assign the value of ~s to the constant ~s."
	 :format-arguments (list value-form name)))

;;; We must do syntactical analysis of NAME here because the
;;; syntactically known variable-information may effect how VALUE is
;;; processed.  For example, if NAME is a symbol-macro for a form that
;;; has a setf-expansion with multiple store forms, then we will need
;;; to know this BEFORE the processing of VALUE begins.

(defspecial SETQ (&rest pairs)
  (if pairs
      (destructuring-bind (name value . more) pairs
	(if more
	    (process-sequence `((setq ,name ,value)
				(setq ,@more))
			      processor environment continuation)
	    (multiple-value-bind (kind binding)
		(variable-binding processor name environment)
	      (process-assignment name value binding kind
				  processor environment
				  continuation))))  
      (process-literal nil processor environment continuation)))


;;; VALUE AND VALUES ASSIGNMENT

(defcont VALUES-CONT effect-cont ())
(defcont VALUE-CONT values-cont ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SEQUENCES OF OPERATIONS
;;;

(defmethod PROCESS-SEQUENCE (forms processor environment continuation)
  (process (car forms) processor environment
	   (let ((rest (cdr forms)))
	     (if rest
		 (make-sequence-cont continuation rest environment)
		 continuation))))

(defspecial PROGN (&body body)
  (process-sequence body processor environment continuation))

;;; RESUME SEQUENCE-CONT must be defined by each processor.

(defcont COLLECTION-CONT sequence-cont ())

(defmethod RESUME ((continuation COLLECTION-CONT) processor value)
  (let* ((env (sequence-cont-environment continuation))
	 (next (continuation-next continuation))
	 (cont (collection-cont next processor)))
    (setq value (combine-values processor nil value))
    (resume next processor
	    (dolist (form (sequence-cont-forms continuation)
			  value)
	      (setq value
		    (combine-values processor value 
				    (process form processor env
					     cont)))))))

(defmethod collection-cont ((continuation t) (processor t))
  (make-value-cont null-continuation))
(defmethod collection-cont ((continuation MV-CALL-cont) (processor t))
  (make-values-cont null-continuation))


;;; RESUME RESTORE-VALUES-CONT must be defined by each processor.
(defcont RESTORE-VALUES-CONT context-cont (forms environment))
(defcont CLEANUP-CONT restore-values-cont ())

(defspecial MULTIPLE-VALUE-PROG1 (form &body more-forms)
  (process form processor environment
	   (if more-forms
	       (make-values-cont
		(make-restore-values-cont
		 continuation more-forms environment))
	       continuation)))

(defspecial UNWIND-PROTECT (form &body more-forms)
  (process form processor environment
	   (if more-forms
	       (make-values-cont
		(make-cleanup-cont
		 continuation more-forms environment))
	       continuation)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MACROS
(defmethod PROCESS-BINDING (name binding
				 (kind (eql :SYMBOL-MACRO))
				 processor environment continuation)
  (process (expand-macro binding name environment)
	   processor environment continuation))

(defmethod PROCESS-COMBINATION ((operator t) form
				binding (kind (eql :MACRO))
				processor environment
				continuation)
  (process (expand-macro binding form environment)
	   processor environment continuation))

(defmethod PROCESS-ASSIGNMENT (name value-form
				    binding  (kind (eql :SYMBOL-MACRO))
				    processor environment
				    continuation)
  (process `(setf ,(expand-macro binding name environment)
		  ,value-form)
	   processor environment continuation))

;;; Should these take PROCESSOR so they can be specialized?

(defmethod EXPAND-MACRO ((def FUNCTION) form environment)
  (expand def form environment))

(defmethod EXPAND-MACRO ((def BINDING) form environment)
  (expand (binding-value def) form environment))

(defmethod EXPAND-MACRO ((def T) (form CONS) environment)
  (expand (global-macro-function (car form)) form environment))

(defmethod EXPAND-MACRO ((def T) (form SYMBOL) environment)
  (expand (global-symbol-macro-function form) form environment))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CONTROLLING EVALUATION

(defspecial QUOTE (form)
  (process-literal form processor environment continuation))

(defcont IF-CONT continuation (consequence alternate environment))
(defspecial IF (predicate consequence &optional alternate)
  (process predicate processor environment
	   (make-if-cont continuation
			 consequence alternate
			 environment)))

(defspecial EVAL-WHEN (situations . body)
  (dolist (situation situations
		     (process-literal nil processor environment
				      continuation))
    (ecase situation
      ((:compile-toplevel compile #-machine-compile cl:compile))
      ((:load-toplevel load #-machine-compile cl:load))
      ((:execute eval #-machine-compile cl:eval)
       (return (process-sequence body processor
				 environment continuation))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MODIFYING THE PROCESSING-TIME ENVIRONMENT

(defspecial LOCALLY (&body body)
  (multiple-value-bind (decls body) (find-declarations body t)
    (let ((new-env (augment-environment environment :declare decls)))
      (process-sequence body processor
			(if (and (top-level-p environment)
				 (not (top-level-p new-env)))
			    (make-top-level-env :env new-env)
			    new-env)
			continuation))))

(defspecial SYMBOL-MACROLET (bindings . body)
  (multiple-value-bind (decls body) (find-declarations body t)
    (process-sequence body processor
		      (augment-environment environment
					   :declare decls
					   :symbol-macro bindings)
		      continuation)))

(defspecial MACROLET (bindings . body)
  (multiple-value-bind (decls body) (find-declarations body t)
    (process-sequence
     body processor
     (augment-environment
      environment :declare decls
      :macro (mapcar
	      #'(lambda (b)
		  (destructuring-bind (name lambda-list . body) b
		    (list name
			  (enclose	;!!!
			   (parse-macro name lambda-list body)
			   environment))))
	      bindings))
     continuation)))

(defspecial THE (value-type form)
  (declare (ignore value-type))	;!!!
  (process form processor environment continuation))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FUNCTION REFERENCES

;;; N.B.: To define a processor-specific method on
;;; process-combination that will always be applicable for a
;;; particular operator, regardless of whether the operator is defined
;;; as a lexical function or macro, the method should specialize on
;;; operator and processor only.
;;;
;;; However, to override global functional evaluation for a pariticlar
;;; oeprator, but not override local functions or local macros, let
;;; the specialized lambda-list be:
;;;   ((operator (eql '<NAME>)) form (binding SYMBOL) (kind t)
;;;    (processor <PROCESSOR-TYPE>) environment continuation)
;;; Note that this will work regardless of whether the function
;;; happens to be defined at compile time, and thus whether
;;; function-binding returns the name or NIL.  For special
;;; handling of setf functions, use the above, but specialize binding
;;; on LIST.
;;;
;;; Note that defining such specialized processing does NOT override
;;; compiler-macros.  You need an around method to do that.

;;; Default case of process-combination: i.e. a function call.
;;; This accidentally allows ((setf name) value athing arg)!
(defmethod PROCESS-COMBINATION ((operator T) form (binding t)
				(kind (eql :FUNCTION))
				processor environment continuation)
  (process-binding operator binding kind processor environment
		   (make-collection-cont (make-call-cont continuation)
					 (cdr form) environment)))

(defspecial MULTIPLE-VALUE-CALL (function &rest args)
  (process function processor environment
	   (make-value-cont
	    (make-collection-cont (make-mv-call-cont continuation)
				  args environment))))

;;; !!! Issue: How do we handle flet/labels that are only used inline???

(defspecial NAMED-FUNCTION (name lambda-list . body) 
  (process-function-definition name lambda-list body
			       processor environment continuation))
  
(defspecial FUNCTION (function-name)
  (multiple-value-bind (kind binding)
      (function-binding processor function-name environment)
    (if (eq kind :function)
	(process-binding function-name binding kind
			 processor environment continuation)
	(signal-program-error "~a is a ~a operator."
			      function-name kind))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EXITS

;;; Any walker might use these to create a private dynamic catch
;;; tag.  A compiler might also use these to optimize jumps that turn out
;;; to be lexically visible exits. 
(defcont EXIT-CONT context-cont ())

(defcont CATCH-CONT context-cont (forms environment))
(defcont LABELED-CONT exit-cont (tag))
(defcont BLOCK-CONT exit-cont (label))

(defspecial BLOCK (label &body body)
  (let ((cont (make-block-cont continuation label)))
    (process-sequence body
		      processor
		      (make-block-env
		       :env environment
		       :bindings (list (make-named-block
					:name label
					:value cont)))
		      cont)))

(defspecial CATCH (tag . forms)
  (process tag processor environment
	   (make-catch-cont continuation forms environment)))

(defcont THROW-CONT continuation (form environment))
(defcont RETURN-FROM-CONT continuation (label environment))

(defspecial THROW (tag value)
  (process tag processor environment
	   (make-throw-cont continuation value environment)))

(defspecial RETURN-FROM (label value)
  (process value processor environment
	   (make-return-from-cont continuation label environment)))

(defmethod RESUME ((continuation RETURN-FROM-CONT) processor values)
  (let* ((name (return-from-cont-label continuation))
	 (binding (control-information
		   name 'block-env
		   (return-from-cont-environment continuation) 
		   #'make-captured-block)))
    (if binding 
	(process-unwind processor continuation values (runtime-value binding))
	(signal-program-error "Attempt to return-from unknown block ~s." name))))

(defspecial GO (label)
  (let ((binding (control-information label 'tagbody-env
				      environment
				      #'make-captured-tag)))
    (if binding
	(process-unwind processor continuation binding
			(runtime-value binding))
	(signal-program-error
	 "Attempt to go to unknown label ~s." label))))


;;; TAGBODY ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 1. Define tags in environment for each label. This can't be done
;;; on the fly, because there may be forward references to labels.
;;; 
;;; 2. Make each of these go-tags available within the evaluation of
;;; any of them.  This is done by having each go-tag use the
;;; jump-set-cont and environment during its evaluation.

(defun tagp (form) (typecase form ((or symbol integer) t)))
(defcont jump-set-cont exit-cont (environment))
(defcont tagbody-cont exit-cont (bindings jump-set))

(defspecial TAGBODY (&rest statements)
  (let* ((counter 0) bindings forms
	 (tagbody-env (make-tagbody-env :env environment))
	 (tagbody-cont (make-tagbody-cont continuation nil nil))
	 (jump-set-cont (make-jump-set-cont tagbody-cont tagbody-env)))
    (dolist (statement (reverse statements))
      (cond ((tagp statement)
	     (push (make-go-tag :name statement
				:key (incf counter)
				:declarations `(forms ,forms)
				:value jump-set-cont)
		   bindings)
	     (push `(label ,statement) forms))
	    (t (push statement forms))))
    (setf (frame-bindings tagbody-env) bindings)
    (setf (tagbody-cont-bindings tagbody-cont) bindings)
    (setf (tagbody-cont-jump-set tagbody-cont) jump-set-cont)
    (resume jump-set-cont processor forms)))

;; Can this be rationalized with sequence-cont???
(defmethod RESUME ((continuation JUMP-SET-CONT) processor forms)
  (process-sequence forms processor
		    (jump-set-cont-environment continuation)
		    (continuation-next continuation)))

;;; Introduced by TAGBODY, above.
(defspecial LABEL (label)
  (declare (ignore label))
  (resume continuation processor form))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FUNCTIONS

(defvar *args-name* (make-symbol "ARGS"))
(let* ((names (list *args-name*)))
  (defun make-function-frame (env arguments)
    (let ((frame (augment-environment (make-boundary-env :env env)
				      :variable names)))
      (setf (binding-value (get-binding *args-name* frame)) arguments)
      frame)))

;; Is there a reason to have this separate from INVOKE???
(defmethod PROCESS-LAMBDA ((f interpreted-function) arguments processor environment continuation)
  (declare (ignore environment))
  (multiple-value-bind (lambda fenv)
      (function-lambda-expression f)
    (destructuring-bind (lambda-list . body) (rest lambda)
      (multiple-value-bind (ignore checks decls1 bindings)
	  (parse-lambda lambda-list *args-name*)
	(declare (ignore ignore))
	(multiple-value-bind (decls2 body)
	    (find-declarations body nil)
	  (process `(let* ,bindings
		      (declare ,@decls1 ,@decls2)
		      ,@checks ,@body)
		   processor
		   (make-function-frame fenv arguments)
		   continuation))))))

(defmethod INVOKE ((function INTERPRETED-FUNCTION) values
		   processor environment continuation)
  (process-lambda function values processor environment continuation))


(defcont call-cont context-cont ())
(defcont mv-call-cont call-cont ())
(defmethod RESUME ((continuation CALL-CONT) processor values)
  (destructuring-bind (f . arguments) values
    (invoke f arguments processor
	    ;; Environment could conceivably be used for debugging...
	    nil
	    (continuation-next continuation))))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; EXTENDING THE RUN-TIME BINDING ENVIRONMENT

(defcont RUNTIME-EXTENSION-CONT context-cont (bindings inits))

(defspecial FLET (binding-forms &body body)
  (process-local-functions binding-forms body processor environment
			   continuation nil))

(defspecial LABELS (binding-forms &body body)
  (process-local-functions binding-forms body processor environment
			   continuation t))

(defun process-local-functions (binding-forms body processor environment
				     continuation recursivep) 
  (multiple-value-bind (decls body) (find-declarations body t)
    (let* ((new-env (augment-environment
		     environment :declare decls
		     :bound-declarations-p recursivep
		     :function (mapcar #'car binding-forms)))
	   (def-env (if recursivep new-env environment))
	   (bindings nil)
	   (inits
	    (loop for (name lambda . body) in binding-forms
		  for binding = (get-binding name new-env)
		  do (push binding bindings)
		  collect
		  (multiple-value-bind (decls body)
		      (find-declarations body t)
		    (process-initialization
		     `(named-function ,name ,lambda
				      (declare ,@decls)
				      (block ,(function-name-key name)
					,@body))
		     binding processor def-env null-continuation)))))
      (process-sequence body processor new-env
			(make-runtime-extension-cont
			 continuation bindings inits)))))

(defspecial LET (binding-forms &body body)
  (multiple-value-bind (decls body) (find-declarations body t)
    (let* ((new-env (augment-environment
		     environment :declare decls
		     :variable (mapcar #'bindingform-name
				       binding-forms)))
	   ;; Treat all variables as lexical until all initforms are
	   ;; processed.
	   (specials nil)
	   (bindings nil)
	   (inits
	    (loop for def in binding-forms
		  for name = (bindingform-name def)
		  for binding = (get-binding name new-env)
		  when (special-binding-p binding)
		  do (push binding specials)
		  do (push binding bindings)
		  collect (process-initialization
			   (bindingform-initform def)
			   binding processor environment null-continuation))))
      (setq bindings (nreverse bindings)
	    specials (nreverse specials))
      (let ((next (make-runtime-extension-cont
		   continuation bindings inits)))
	(if specials
	    (bind-specials body specials processor new-env next)
	    (process-sequence body processor new-env next))))))

;;; This is wrong because specials must be dynamically bound during
;;; the evaluation of following initforms!!!
(defspecial LET* (binding-forms &body body)
  (multiple-value-bind (decls body) (find-declarations body t)
    (multiple-value-bind (body-env init-env)
	(augment-environment environment :declare decls
			     :bound-declarations-p t
			     :variable (mapcar #'bindingform-name
					       binding-forms))
      (let ((init-envs nil))		;Reorder init-env
	(do ((bindings binding-forms (cdr bindings))
	     (env init-env (frame-env env)))
	    ((null bindings))
	  (push env init-envs))
	(let* ((specials nil)
	       (bindings nil)
	       (inits
		(loop for def in binding-forms
		      and env in init-envs
		      for init-env = (frame-env env)
		      for name = (bindingform-name def)
		      and form = (bindingform-initform def)
		      for binding = (get-binding name env)
		      when (eq form 'unbound-flag) ;TEMPORARY kludge 
		      do (setq form nil) ;to deal with arg parsing!!!
		      if (special-binding-p binding)
		      do (push binding specials)
		      do (push binding bindings)
		      collect (process-initialization
			       form binding
			       processor init-env null-continuation))))
	  (let ((next (make-runtime-extension-cont
		       continuation bindings inits)))
	    (if specials
		(bind-specials body specials processor body-env next)
		(process-sequence body processor body-env next))))))))

;;; Temporary!!!  It will be easier to combine these after we make
;;; process-binding a method of process by eliminating KIND so that
;;; bindings can appear as literals in code to be processed.

(defmethod BIND-SPECIALS (body specials (processor LISP-INTERPRETER) env cont)
  (resume (make-progv-cont cont body env) processor
	  (list (mapcar #'binding-name specials)
		(mapcar #'binding-value specials))))

(defmethod BIND-SPECIALS (body specials (processor BYTE-COMPILER) env cont)
  (resume (make-progv-cont cont body env) processor
	  (list (process-literal (mapcar #'binding-name specials)
				 processor env null-continuation)
		(resume (make-call-cont null-continuation)
			processor
			`((fdefinition 'list)
			  ,@(loop for b in specials
				  collect (process-binding
					   (binding-name b)
					   b :variable processor
					   env null-continuation)))))))

(defcont PROGV-CONT context-cont (forms environment))

(defspecial PROGV (symbols-form values-form . body)
  (process symbols-form processor environment
	   (make-collection-cont (make-progv-cont continuation 
						  body environment)
				 (list values-form)
				 environment)))
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#-machine-compile
(macrolet ((def-host (host-name eclipse-name)
	     `(defspecial ,host-name (&rest args)
		(process (cons ',eclipse-name args)
			  processor environment continuation))))
  (def-host cl:function function)
  (def-host cl:setf setf)
  (def-host cl:funcall funcall)

  #|(def-host cl:the the)
  (def-host cl:find-symbol find-symbol)
  (def-host cl:gensym gensym)
  (def-host cl:intern intern)
  (def-host cl:member member)
  
  (def-host cl:MULTIPLE-VALUE-BIND  MULTIPLE-VALUE-BIND )
  (def-host cl:DO do)
  (def-host cl:MULTIPLE-VALUE-SETQ MULTIPLE-VALUE-SETQ )
  (def-host cl:OR OR)
  (def-host cl:PROG1 PROG1)
  (def-host cl:PROG2 PROG2)
  (def-host cl:PROG* PROG*)
  (def-host cl:PROG PROG)
  (def-host cl:PSETQ PSETQ)
  (def-host cl:DOLIST DOLIST)
  (def-host cl:COND COND)
  (def-host cl:DOTIMES DOTIMES)
  (def-host cl:RETURN RETURN)
  (def-host cl:AND AND)
  (def-host cl:MULTIPLE-VALUE-LIST MULTIPLE-VALUE-LIST)
  (def-host cl:DO* do*)|#
  )
#-machine-comile
(macrolet ((def-host-noop (host-name)
	     `(defspecial ,host-name (&rest args) (declare (ignore args))
		(process-literal nil processor environment continuation))))
  (def-host-noop cl:declaim)
  (def-host-noop eclipse::host-declaim))