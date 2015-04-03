(defmacro ef (form)			;temporary for development!!!
  `(values-list
    (eclipse::process ',form eclipse::*interpreter* nil nil)))

(defstruct LISP-INTERPRETER)
(defparameter *INTERPRETER* (make-lisp-interpreter))

;; Belongs in evaluation.lisp!!!
(defun EVAL-ENV (form &optional environment (processor *interpreter*))
  (values-list (process form processor environment null-continuation)))

(defstruct (stepper (:include lisp-interpreter))
  (level 0))

(defmacro STEP (form &environment env)
  `(eval-env ',form ,env (make-stepper)))

(defmethod PROCESS (form (processor LISP-STEPPER)
			 environment continuation)
  (declare (ignore environment continuation))
  (let ((level (stepper-level processor)))
    (break #"~%~v@tStep ~d Form: ~s" (* level 2) level form)
    (setf (stepper-level processor) (1+ level))
    (let ((values (unwind-protect (call-next-method)
		    (setf (stepper-level processor) level))))
      (format *debug-io* #"~%~v@tStep ~d Values: ~s~%"
	      (* level 2) level values)
      values)))

;;; Redefined from evaluation.lisp!!!
(defun apply-interpreted-function (closure args)
  (declare (special *byte-engine*))
  (let ((processor (typecase closure	;fix this!!
		     (byte-compiled-function *byte-engine*)
		     (t *interpreter*)))) 
    (values-list (process-lambda closure args processor nil null-continuation))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BASIC EVALUATION


(defmethod PROCESS-LITERAL (literal (processor LISP-INTERPRETER)
				    environment continuation)		
  (declare (ignore environment))
  (resume continuation processor (list literal)))

(defmethod RESUME ((continuation IF-CONT) (processor LISP-INTERPRETER) values) 
  (process (if (car values)
	       (if-cont-consequence continuation)
	       (if-cont-alternate continuation))
	   processor
	   (if-cont-environment continuation)
	   (continuation-next continuation)))

(defmethod RESUME ((continuation VALUE-CONT)
		   (processor LISP-INTERPRETER) value)
  (call-next-method continuation processor (list (car value))))

(defmethod RESUME ((continuation SEQUENCE-CONT)
		   (processor LISP-INTERPRETER) values)
  (let ((env (sequence-cont-environment continuation)))
    (dolist (form (sequence-cont-forms continuation)
		  (resume (continuation-next continuation) processor
			  values))
      (setq values (process form processor env null-continuation)))))

(defmethod COMBINE-VALUES ((processor LISP-INTERPRETER) values value)
  (nconc values value)) 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BINDING ACCESS

(defmethod PROCESS-BINDING ((name t) (binding RUNTIME-BINDING) (kind t)
			     (processor LISP-INTERPRETER)
			     environment continuation)
  (declare (ignore environment))
  (resume continuation processor (list (runtime-value binding))))

;;; :SPECIAL, :CONSTANT
(defmethod PROCESS-BINDING (name (binding T) (kind T)
				  (processor LISP-INTERPRETER)
				  environment continuation)
  (declare (ignore environment))
  (resume continuation processor (list (symbol-value name))))

;; Sanity check: Make sure this is not called by ((lambda (x) x) 3)
(defmethod PROCESS-BINDING (name (binding T) (kind (eql :FUNCTION))
				  (processor LISP-INTERPRETER)
				  environment continuation)
  (declare (ignore environment))
  (resume continuation processor (list (fdefinition name))))

;; Rationalize these two!!!
(defmethod RESUME ((continuation LEXICAL-ASSIGNMENT-CONT) (processor LISP-INTERPRETER)
		   values)
  (resume (continuation-next continuation) processor
	  (list (setf (runtime-value (assignment-cont-variable continuation))
		      (first values)))))

(defmethod RESUME ((continuation DYNAMIC-ASSIGNMENT-CONT)
		   (processor LISP-INTERPRETER)
		   values) 
  (resume (continuation-next continuation) processor
	  (list (setf (symbol-value (assignment-cont-variable continuation))
		      (first values)))))

 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FUNCTION DEFINITION

(defmethod PROCESS-FUNCTION-DEFINITION (name lambda-list body
					      (processor LISP-INTERPRETER)
					      environment continuation)
  (resume continuation processor
	  (list (enclose `(lambda ,lambda-list ,@body) environment name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EXITS
;;; There are several places where the interpreter needs to PROCESS or
;;; PROCESS-SEQUENCE in a context which wraps some dynamic protection
;;; around that evaluation, but NOT its CONTINUATIONS.  This includes
;;; block, catch, unwind-protect and progv.  We need a better way to
;;; unify this!!! The usual CPS style doesn't need this because all
;;; invocations have access to the (dynamic) current continuation:
;;;  1. Resuming that invocation is guaranteed to "return" back to the
;;; right place.  In our case, however, there may be arbitrary
;;; intervening (C) functions which do not look at the current
;;; continuation.  Resuming a continuation through one of these will
;;; only cause the intervening function to return a particular value,
;;; but will not cause us to continue unwinding. Perhaps we could fix
;;; this by having invoke (compile-function) dynamically bind the
;;; curent continuation, and checking for a special '#:unwinding
;;; return value which would indicate we should continue manually unwinding
;;; rather than simply returning that value.
;;;  2. Compiled throws must pick up interpreted
;;; unwind-protect/progv's, and compiled unwind-protect/progv's must
;;; be picked up by interpreted throw, return-from and go.


;;; The nested values listing is because the catch could pick up
;;; multiple values through an interpreted or compiled throw, or could
;;; pick up a list of values if process-sequence returns normally.
;;; Either way, the values must be packaged into a list for resuming.
(defun interpret-catch (tag body processor environment continuation)
  (resume (continuation-next continuation) processor
	  (multiple-value-list
	   (catch tag
	     (values-list
	      (process-sequence body processor environment null-continuation))))))
			     

(defmethod RESUME ((continuation CATCH-CONT) (processor LISP-INTERPRETER)
		   tags)
  (let ((tag (car tags)))
    (interpret-catch tag (catch-cont-forms continuation)
		     processor
		     (catch-cont-environment continuation)
		     continuation)))

(defspecial BLOCK (label &body body)
  :processor LISP-INTERPRETER
  (let ((cont (make-block-cont continuation label)))
    (interpret-catch cont body processor
		     (make-block-env
		      :env environment
		      :bindings (list (make-named-block
				       :name label
				       :value cont)))
		     cont)))

(defmethod RESUME ((continuation JUMP-SET-CONT) (processor LISP-INTERPRETER)
		   forms)
  (declare (ignore values))
  (resume (continuation-next continuation) processor
	  (catch continuation
	    (process-sequence forms processor
			      (jump-set-cont-environment continuation)
			      null-continuation))))


(defmethod RESUME ((continuation TAGBODY-CONT) (processor LISP-INTERPRETER)
		   (values GO-TAG))
  (resume (tagbody-cont-jump-set continuation) processor
	  (getf (binding-declarations values) 'forms)))

(defmethod RESUME ((continuation TAGBODY-CONT) (processor LISP-INTERPRETER)
		   (values t))
  (process-literal nil processor nil (continuation-next continuation)))


#+old
(defmethod RESUME ((continuation JUMP-SET-CONT) (processor LISP-INTERPRETER)
		   values)
  (declare (ignore values))
  (let ((result (catch continuation (call-next-method))))
    (typecase result  
      (go-tag (resume continuation processor
		      (getf (binding-declarations result) 'forms)))
      (t (process-literal nil processor nil (continuation-next continuation))))))


(defmethod PROCESS-UNWIND ((processor LISP-INTERPRETER)
			    lexical-continuation values target-continuation)
  (declare (ignore lexical-continuation))
  ;; Listp check is for things like go-tags, which aren't wrapped up
  ;; in a list.
  (throw target-continuation (if (listp values)
				 (values-list values)
				 values)))

(defmethod RESUME ((continuation THROW-CONT) (processor LISP-INTERPRETER)
		   tags)
  (throw (car tags)
    (values-list 
     (process (throw-cont-form continuation)
	      processor
	      (throw-cont-environment continuation)
	      null-continuation))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CONTEXT CONTINUATIONS

(defmethod RESUME ((continuation RESTORE-VALUES-CONT)
		   (processor LISP-INTERPRETER) values)
  (process-sequence (restore-values-cont-forms continuation)
		    processor
		    (restore-values-cont-environment continuation)
		    null-continuation)
  (resume (continuation-next continuation) processor values))

(defspecial UNWIND-PROTECT (form &rest other-forms)
  :processor LISP-INTERPRETER
  (resume continuation processor
	  (unwind-protect
	      (process form processor environment null-continuation) 
	    (process-sequence other-forms processor environment null-continuation))))


(defmethod RESUME ((continuation PROGV-CONT)
		   (processor LISP-INTERPRETER) values)
  (destructuring-bind (symbols values) values
    (resume (continuation-next continuation)
	    processor
	    (progv symbols values
	      (process-sequence (progv-cont-forms continuation)
				processor
				(progv-cont-environment continuation)
				null-continuation)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; OTHER PROCESSING

;;; IWBNI the environment kept an eq table of load-time-value forms
;;; (the entire form, including the LOAD-TIME-VALUE part).  The result
;;; of processing the form is cached in the table, and reused.
;;; Similarly an equal table would cache the results that are
;;; read-only-p.  (A compiler could also check load-forms against this
;;; latter table.)

;;; It is not clear if macro references within form should be resolved
;;; using the current lexical environment or NIL.

(defspecial LOAD-TIME-VALUE (form &optional read-only-p)
  :processor LISP-INTERPRETER
  (declare (ignore read-only-p))
  (let ((var '#:load-time-value))
    (process `(let ((,var ,form)) ,var)
	     processor nil continuation)))

(defmethod INVOKE ((function-values t) values (processor LISP-INTERPRETER)
		   environment continuation)
  (declare (ignore environment))
  (resume continuation processor
	  (multiple-value-list (apply function-values values))))
