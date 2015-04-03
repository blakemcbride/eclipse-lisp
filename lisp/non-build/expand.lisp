(defmacro xf (form)			;temporary for development
  `(xxwith-compiler-handlers
    (let* ((*processor* eclipse::*byte-compiler*)
	   (*environment* nil)
	   (results (eclipse::process ',form *processor* *environment*
				      nil)))
      (xp results nil)
      results)))
(defmacro ff (form) `(progn (xf ,form) (ef ,form)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FULL MACROEXPANDER EXAMPLE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (EXPANDER (:include compiler)))
(defparameter *EXPANDER* (make-expander))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BASIC EVALUATION

(defmethod RESUME ((continuation IF-CONT) (processor EXPANDER)
		   values) 
  (let ((environment (if-cont-environment continuation)))
    (resume (continuation-next continuation) processor
	    `(if ,values
		 ,(process (if-cont-consequence continuation)
			    processor environment nil) ;!!!
		 ,(process (if-cont-alternate continuation)
			    processor environment nil))))) ;!!!

(defmethod RESUME ((continuation SEQUENCE-CONT) (processor EXPANDER)
		   value)
  (let ((forms (sequence-cont-forms continuation)))
    (resume (continuation-next continuation) processor
	    `(progn ,value
		    ,@(loop for form in forms
			    and env = (sequence-cont-environment
				       continuation)
			    collect (process form processor env
					     null-continuation))))))

(defmethod COMBINE-VALUES ((processor EXPANDER) values value)
  (nconc values (list value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BINDING ACCESS

(defmethod PROCESS-BINDING ((name t) (binding RUNTIME-BINDING) (kind t)
			    (processor EXPANDER) environment continuation)
  (declare (ignore environment))
  (resume continuation processor 
	  (if (eq name (binding-name binding))
	      binding
	      (multiple-value-bind (lambda fenv)
		  (function-lambda-expression (binding-value binding))
		(destructuring-bind (lambda-list . body) (rest lambda)
		  (process-function-definition
		   name lambda-list body
		   processor fenv nil)))))) ;!!!
				    

;;; :special, :constant
(defmethod PROCESS-BINDING (name (binding t) (kind t)
				 (processor EXPANDER) environment continuation)
  (declare (ignore environment))
  (resume continuation processor `(symbol-value ,name)))


(defmethod PROCESS-BINDING (name (binding T) (kind (eql :FUNCTION))
				 (processor EXPANDER)
				 environment continuation)
  (declare (ignore environment))
  (resume continuation processor `(fdefinition ',name)))


(defmethod RESUME ((continuation LEXICAL-ASSIGNMENT-CONT) (processor EXPANDER)
		   value)
  (resume (continuation-next continuation) processor
	  `(setq ,(assignment-cont-variable continuation)
		 ,value)))

(defmethod RESUME ((continuation DYNAMIC-ASSIGNMENT-CONT)
		   (processor EXPANDER) value) 
  (resume (continuation-next continuation) processor
	  `(set ',(assignment-cont-variable continuation)
		 ,value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FUNCTION DEFINITION

(defcont FUNCTION-CONT exit-cont ())

;;; Rationalize this with process-lambda!!!

(defvar *dummy-arg* (make-symbol "ARGS"))
(defvar *dummy-arglist* (list *dummy-arg*))
(defmethod PROCESS-FUNCTION-DEFINITION (name lambda-list body
					     (processor EXPANDER)
					     environment
					     continuation)
  (multiple-value-bind (bindings checks decls2)
      (parse-lambda lambda-list *dummy-arg*)
    (multiple-value-bind (decls1 body)
	(find-declarations body t)
      (let ((def `((&rest ,*dummy-arg*)
		   ,@(multiple-value-list
		      (process 
		       `(let* ,bindings
			  (declare ,@decls1 ,@decls2)
			  ,@checks ,@body)
		       processor
		       (augment-environment environment
					    :variable *dummy-arglist*)
		       (make-function-cont nil)))))) ;!!!
	(resume continuation processor
		(if (car-eq name 'lambda)
		    `(lambda ,@def)
		    `(named-function ,name ,@def)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EXITS

(defmethod RESUME ((continuation LABELED-CONT) (processor EXPANDER)
		   values)
  (resume (continuation-next continuation)
	  processor
	  `(catch ,(labeled-cont-tag continuation)
	      ,@(expander-body values))))

;;; IWBNI we processed form to a special throwing continuation so
;;; that, if the tag is known at compile time, we can skip the runtime
;;; search for the tag.  Be careful, though: if we cross a function
;;; boundary and the closure is funcallable in a different context,
;;; than there could be different intervening tags.
(defmethod RESUME ((continuation THROW-CONT) (processor EXPANDER)
		   tag)
  (resume (continuation-next continuation)
	  processor
	  `(throw ,tag
	      ,(process (throw-cont-form continuation)
			 processor
			 (throw-cont-environment continuation)
			 nil))))	;!!!

(defmethod RESUME ((continuation BLOCK-CONT) (processor EXPANDER)
		   values)
  (resume (continuation-next continuation)
	  processor
	  `(block ,(block-cont-label continuation)
	     ,@(expander-body values))))
     
;;; If target-continuation is visible within the lexically known
;;; continuations, then we can unwind to that point manually.
;;; It might not be visible, though, because a lexical closure that
;;; causes this unwind might get funcalled from some unknown compiled
;;; function that it was passed to.  Unwinding would not get us to the
;;; correct place in the CALLER of this function.  In the general
;;; case, we have to throw the values to the target-continuation and
;;; let the dynamic unwinding mechanism do the right thing.

(defmethod PROCESS-UNWIND ((processor EXPANDER)
			    lexical-continuation values
			    (target-continuation BLOCK-CONT))
  (resume (continuation-next lexical-continuation)
	  processor
	  `(return-from ,(block-cont-label target-continuation)
	     ,@(expander-body values))))

;;; Potential optimization: implement loop unrolling by resuming
;;; target-continuation! 
(defmethod PROCESS-UNWIND ((processor EXPANDER)
			    lexical-continuation values
			    (target-continuation JUMP-SET-CONT)) 
  (resume lexical-continuation processor
	  `(go ,values)))

(defmethod RESUME ((continuation TAGBODY-CONT) (processor expander)
		   values)
  (resume (continuation-next continuation)
	  processor `(tagbody ,@(expander-body values))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CONTEXT CONTINUATIONS

(defun restore-forms (key continuation processor value)
  (resume (continuation-next continuation) processor
	  `(,key ,value
		 ,@(process-sequence
		    (restore-values-cont-forms continuation)
		    processor
		    (restore-values-cont-environment continuation)
		    null-continuation))))

(defmethod RESUME ((continuation RESTORE-VALUES-CONT)
		   (processor EXPANDER) value)
  (restore-forms 'multiple-value-prog1 continuation processor value))

(defmethod RESUME ((continuation CLEANUP-CONT)
		   (processor EXPANDER) value)
  (restore-forms 'UNWIND-PROTECT continuation processor value))


(defun expander-body (values)
  (if (car-eq values 'progn)
      (cdr values)
      (list values)))

(defmethod RESUME ((continuation RUNTIME-EXTENSION-CONT)
		   (processor EXPANDER)
		   values)
  (resume (continuation-next continuation)
	  processor
	  `(let ,(runtime-extension-cont-bindings continuation)
	     ,@(runtime-extension-cont-inits continuation)
	     ,@(expander-body values))))


(defmethod RESUME ((continuation PROGV-CONT)
		   (processor EXPANDER) values)
  (destructuring-bind (symbols values) values
    (resume (continuation-next continuation) processor
	    `(progv ,symbols ,values
	       (process-sequence (progv-cont-forms continuation)
				 processor
				 (progv-cont-environment continuation)
				 null-continuation)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FUNCTIONS

(defmethod INVOKE ((function t) values (processor EXPANDER)
		   environment continuation)
  (declare (ignore environment))
  (resume continuation processor
	  ;; This really depends on optimization settings and notinline!!!
	  (let ((f1 (when (car-eq function 'fdefinition)
		      (second function))))
	    (if (car-eq f1 'quote)
		`(,(second f1) ,@values)
		`(funcall ,function ,@values)))))


(defmethod RESUME ((continuation MV-CALL-CONT) (processor EXPANDER) values)
  (destructuring-bind (f . arguments) values
    (resume (continuation-next continuation) processor
	    `(multiple-value-call ,f ,@arguments))))

;;; Need special LOAD-TIME-VALUE, EVAL-WHEN for FILE-COMPILER.
