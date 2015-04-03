;;; IWBNI this evaluated subforms of place only once, but the spec
;;; doesn't require this, and no one else does the fancy way, so we
;;; won't either.  Similarly for assert, ctypecase and ccase.
(defmacro CHECK-TYPE (place type &optional type-string)
  (let ((var (gensym "VALUE"))
	(block (gensym "CHECK-TYPE")))
    `(block ,block
       (loop
	(let ((,var ,place))
	  (when (typep ,var ',type) (return-from ,block nil))
	  (setf ,place
		(check-type-error ',place ,var ',type ,type-string)))))))

(defmacro DEFINE-CONDITION (name superclasses slot-specs
				 &rest options)
  (let ((report-clause (find :report options :key #'first))
	(options (delete :report options :key #'first)))
    `(progn
       (defclass ,name ,(or superclasses '(condition))
	 ,slot-specs ,@options)
       ,@(when report-clause
	   (let ((report (second report-clause))
		 (condition (gensym "CONDITION"))
		 (stream (gensym "STREAM")))
	     `((defmethod print-object ((,condition ,name) ,stream)
		 (if *print-escape*
		     (call-next-method)
		     ,(if (stringp report)
			  `(write-string ,report ,stream)
			  `(,report ,condition ,stream)))))))
       ',name)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 29.4 PROGRAM INTERFACE TO THE CONDITION SYSTEM               ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 29.4.2 ASSERTIONS
(defmacro ASSERT (test-form &optional places datum &rest arguments)
  (let ((block (gensym "ASSERT")))
    `(block ,block
       (loop
	(when ,test-form (return-from ,block nil))
	(assert-error ',test-form ',places ,datum ,@arguments)
	,@(mapcar #'(lambda (place)
		      `(setf ,place (assert-prompt ',place ,place)))
		  places)))))


;;; 29.4.3 EXHAUSTIVE CASE ANALYSIS
(defmacro CTYPECASE (keyform &rest clauses)
  (let ((block (gensym "CTYPECASE")))
    `(block ,block
       (tagbody
	,block
	(return-from ,block
	  (typecase ,keyform
	    ,@clauses
	    (otherwise
	     (setf ,keyform
		   (check-type-error
		    ',keyform ,keyform
		    '(or ,@(mapcar #'first clauses)) nil))
	     (go ,block))))))))


(defmacro CCASE (&environment env keyform &rest clauses)
  (let ((block (gensym "CCASE"))
	(types (case-keys clauses env)))
    `(block ,block
       (tagbody
	,block
	(return-from ,block
	  (case ,keyform
	    ,@clauses
	    (otherwise
	     (setf ,keyform
		   (check-type-error
		    ',keyform ,keyform '(member ,@types) nil))
	     (go ,block))))))))
	       
;;; 29.4.4 HANDLING CONDITIONS
(defmacro IGNORE-ERRORS (&rest forms)
  `(handler-case (progn ,@forms)
     (error (condition) (values nil condition))))


;;; BINDING FORMS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun make-condition-bindings (bindings constructor)
  (loop for (name function . args) in bindings
	collect `(,constructor ',name ,function ,@args)))

(defmacro HANDLER-BIND (bindings &rest body)
  `(let ((*handlers*
	  (cons (list ,@(make-condition-bindings bindings 'make-handler))
		*handlers*)))
     (declare (dynamic-extent *handlers*))
     ,@body))

;;; 29.4.7 ESTABLISHING RESTARTS
(defmacro RESTART-BIND (restarts &rest body)
  `(let ((*restarts*
	  (cons (list ,@(make-condition-bindings restarts 'make-restart))
		*restarts*)))
     (declare (dynamic-extent *restarts*))
     ,@body))

(defmacro WITH-SIMPLE-RESTART ((restart-name format-control
				&rest format-arguments)
			       &body forms)
  `(restart-case (progn ,@forms)
     (,restart-name ()
	 :report (lambda (s)
		   (format s ,format-control ,@format-arguments))
       (values nil t))))

(defmacro WITH-CONDITION-RESTARTS (condition-form restarts-form
						  &body body)
  (let ((old-associations (gensym "OLD-ASSOCIATIONS")))
    `(let ((,old-associations (associate-conditions
			       ,condition-form ,restarts-form)))
       (unwind-protect (progn ,@body)
	 (disassociate-conditions ,old-associations)))))

;;; CASE FORMS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun handler-case-body (temp vars body)
  `(let ,(when vars `((,(first vars) ,temp)))
     ,@body))

(defun restart-case-body (temp vars body)
  (loop (if (member (car body) '(:test :report :interactive))
	    (setq body (cddr body))
	  (return)))
  `(destructuring-bind ,vars ,temp ,@body))

(defun bind-form-from-case (binder expression bindings
			    options-fn body-fn args-restp
			    options-fn-extra-arg
			    &aux (block (gensym (symbol-name binder)))
				 (temp (gensym "VALUES"))
				 (labels nil))
  `(block ,block
     (let ((,temp nil))
       #+cmu (declare (special ,temp))	;Major cmu bug
       (tagbody
	 (,binder
	  ,(mapcar #'(lambda (binding)
		       (destructuring-bind (name vars . body) binding
			 (let ((label (gensym)))
			   (setq labels
				 `(,@labels ,label
					    (return-from ,block
					      ,(funcall body-fn temp vars body))))
			   `(,name
			     #'(lambda (,@args-restp temp)
				 (setq ,temp temp)
				 (go ,label))
			     ,@(when options-fn
				 (funcall options-fn name body
					  options-fn-extra-arg))))))
		   bindings)
	  (return-from ,block ,expression))
	 ,@labels))))

(defmacro HANDLER-CASE (expression &rest bindings)
  (let ((last-clause (car (last bindings))))
    (if (eq (car last-clause) :no-error)
	(let ((error-return (gensym "NO-ERROR"))
	      (normal-return (gensym "NORMAL-RETURN")))
	  `(block ,error-return
	     (multiple-value-call #'(lambda ,@(rest last-clause))
	       (block ,normal-return
		 (return-from ,error-return
		   (handler-case (return-from ,normal-return ,expression)
		     ,@(butlast bindings)))))))
      (bind-form-from-case
       'handler-bind expression bindings nil #'handler-case-body nil nil))))

(defmacro RESTART-CASE (&environment env expression &rest bindings)
  #+lisp-host (declare (ignore env))
  (let* ((expression (macroexpand expression #-lisp-host env))
	 (signaller (and (consp expression) (car expression)))
	 (condition-var (when (member signaller
				    '(signal error cerror warn))
			  (gensym "CONDITION")))
	 (exp (if condition-var
		  `(,signaller ,condition-var)
		expression))
	 (form (bind-form-from-case
		'restart-bind exp bindings #'restart-case-options
		#'restart-case-body '(&rest) condition-var)))
    (if condition-var
	`(let ((,condition-var
		,`(canonicalize-condition
		   ,(second expression)
		   (list ,@(cl:cddr expression))
		   ',(case signaller
		      (warn 'simple-warning)
		      (signal 'simple-condition)
		      ((error cerror) 'simple-error)))))
	   ,form)
      form)))

    
(defun restart-case-options (name body condition
				  &aux options reportp)
  (flet ((add-opt (name val)
		  (setq options `(,@options ,name (function ,val))))
	 (done ()
	       (unless (or name reportp)
		 (warn "No report option specified for anonymous restart ~s."
		       body))
	       (return-from restart-case-options
		 (if condition
		     `(:associated-conditions (list ,condition)
					      ,@options)
		     options))))
    (loop for (key val) on body by #'cddr
	  do (case key
	       (:report (setq reportp t)
			(add-opt :report-function
				 (if (stringp val)
				     `(lambda (s) (write-string ,val s))
				     val)))
	       (:interactive (add-opt :interactive-function val))
	       (:test (add-opt :test-function val))
	       (t (done)))
	  finally (done))))
