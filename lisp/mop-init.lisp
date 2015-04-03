;;; These functions are used in initializing metaobjects.
;;; Thus, they might be needed at runtime generally, but would not be
;;; needed for applications compiled in a way which statically
;;; initialized any needed classes.

;;; Used by shared-initialize structure-class
(defun eclipse::concatenate-names (&rest names)
  (intern (apply #'concatenate
		 #+machine-compile 'string
		 #-machine-compile 'cl:string
		 (mapcar #'cl:string names))))


;; These two are needed so that dotimes can be used in interpreted
;; code: add-integer-integer and ge-integer-integer are c:functions,
;; so they can't be called directly in interpreted code.  After the
;; compiler gets smarter, we could replace the calls to these in
;; dotimes with ordinary +/add and <=/ge, as long as we also include
;; declarations.  Note, though, that dotimes is used so often, simply
;; using add,ge will be too slow.
(defun eclipse::add-integer (x y) (eclipse::add-integer-integer x y))
(defun eclipse::ge-integer (x y) (eclipse::ge-integer-integer x y))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MACROS                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This stuff is in the runtime library because applications might
;;; have defmacro and similar code interspersed with the truly
;;; "run-time" source.  Any code generated form this should still be
;;; loadable (i.e. the init funciton run) using just the runtime
;;; library.

;;; Of course, their won't be anything around to use the definitions,
;;; but that's ok.  Programmers can edit their code to get rid of the
;;; waste, or use a future application-builder product to
;;; automatically separate the compile-time definitions.

;;; ERROR CHECKING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun eclipse::check-for-global-macro-env (name env)
  (when env
    (eclipse:signal-program-error
     "Attempt to install ~a in non-null lexical environment." name)))

;;; CONSTRUCTORS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+machine-compile
(progn
(defun eclipse::make-macro-function (mname mfunc)
  ;; Create a real function (a closure that signals control-error),
  ;; and then modify its type and slots to conform to a macro-function.
  (let (func)
    (flet ((illegal-macro-call (&rest args)
			       (error 'control-error
				      :format-control "Cannot funcall ~s on args: ~s."
				      :format-arguments (list func args))))
      (setf func #'illegal-macro-call)
      (set-tagged-instance-class func macro-function-classobj)
      (set-standard-instance-slots func (make-slots 4))
      (if (and (fboundp 'function-lambda-expression)
	       ;; Might be used by function-lambda-expression
	       (fboundp 'function-source))
	  ;; Might not be bootstrapped yet.
	  ;; We can get rid of the above guard if the compiler
	  ;; can access methods directly!!!
	  (multiple-value-bind (mlambda menv)
	      (function-lambda-expression mfunc)
	    (with-slots (name lambda env function) func
	      (setf name mname
		    function mfunc
		    lambda mlambda
		    env menv)))
	  (setf (host::static-access func host::name
				     macro-function) mname
		(host::static-access func function
				     macro-function) mfunc
                (host::static-access func host::lambda
				      macro-function) nil
                (host::static-access func host::env
				      macro-function) nil)))
    func))
(defun eclipse::macro-function-p (x)
  (eclipse::typep x eclipse::macro-function-classobj))
(defun eclipse::macro-function-function (name macro-function)
  (declare (ignore name))
  (slot-value macro-function 'function))
)

(defun eclipse:make-symbol-macro-function (expansion)
  #'(lambda (form env)
      (declare (ignore form env))
      expansion))


;;; ACCESSORS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Note that expander argument to (setf macro-function) cannot be
;;; nil.  Use fmakunbound to remove a global macro.

(defun (SETF eclipse:MACRO-FUNCTION) (expander name &optional env)
  (eclipse::check-for-global-macro-env name env)
  (eclipse:set-symbol-function-value
   name (eclipse::make-macro-function name expander)))

;;; Expander can be nil for these three.
(defun (setf eclipse:symbol-macro-function) (expander symbol &optional env)
  (eclipse::check-for-global-macro-env symbol env)
  (eclipse::system-property-setter symbol 'symbol-macro expander))
  
(defun (SETF eclipse:COMPILER-MACRO-FUNCTION) (def function-name &optional env)
  (eclipse::check-for-global-macro-env function-name env)
  (let ((setfp (eclipse::setf-function-name-p function-name)))
    (eclipse::system-property-setter
     (eclipse::function-name-key function-name setfp)
     (if setfp 'setf-compiler-macro 'compiler-macro)
     def)))

;;; This can't be a defsetf because it is used within generated code
;;; that appears within EVAL-WHEN :COMPILE-TOPLEVEL, and the
;;; interpreter can't evaluate c:macros such as SET-SETF-EXPANDER.
(defun (setf eclipse:SETF-EXPANDER) (expander key)
  (eclipse:set-setf-expander key expander))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 8.2 MACRO EXPANSION                                          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter eclipse:*MACROEXPAND-HOOK* #'funcall)

;;; get-setf-expansion is required to generate new setf methods with
;;; new, unique temporary variables each time it is called.
(defun generate-setf-method (form inverse setf-function-p)
  (destructuring-bind (accessor . subforms) form
    (let ((new-var (gensym "VALUE"))
	  (vars (eclipse::make-symbols subforms "ARG")))
      (values vars subforms (list new-var)
	      (if setf-function-p
		  `(,@inverse ,new-var ,@vars)
		  `(,@inverse ,@vars ,new-var))
	      `(,accessor ,@vars)))))
