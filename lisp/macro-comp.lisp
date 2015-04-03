(defparameter macro-args (make-symbol "FORM"))
(defun eclipse:PARSE-MACRO (name lambda-list body
				 &key default compiler-p &aux decl env)
  ;; Note that lambda-list might be dotted, so we can't use MEMBER!
  (do ((rest lambda-list (cdr rest)))
      ((not (consp rest))
       (setq env (make-symbol "IGNORED")
	     decl `((ignore ,env))))
    (when (eq (car rest) '&environment)
      (return
       (setq lambda-list (nconc (ldiff lambda-list rest) (cddr rest))
	     env (second rest)))))
  (multiple-value-bind (bindings decl0 body doc real-bindings)
      (eclipse::destructure-lambda lambda-list body macro-args default nil)
    (setq decl (append decl decl0))
    (let* ((n (if (eclipse:car-eq lambda-list '&whole) 1 0))
	   (popper1 `(setq ,macro-args (cdr ,macro-args)))
	   (popper (if compiler-p
		       `(when (eq (pop ,macro-args) 'eclipse:funcall)
			  ,popper1)
		       popper1)))
      (let ((first-real-binding (nth n real-bindings)))
	(if first-real-binding
	    (setf (second first-real-binding)
		  `(progn ,popper ,(second first-real-binding)))
	    (setf body (cons popper body)))))
    (values `(lambda (,macro-args ,env &aux ,@bindings)
	       ,@(when decl `((declare ,@decl)))
	       (block ,name ,@body))
	    doc)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 8.1 MACRO DEFINITION                                         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro eclipse:DEFINE-SYMBOL-MACRO (symbol expansion)
  (if (eclipse::globally-special-p symbol)
      (eclipse:signal-program-error
       "Special variable ~s cannot be redefined as a symbol-macro."
       symbol)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (eclipse:symbol-macro-function ',symbol)
	     (eclipse:make-symbol-macro-function ',expansion))
       ',symbol)))

(defmacro eclipse:DEFINE-SETF-EXPANDER (access-fn lambda-list &body body)
  (multiple-value-bind (f doc)
      (eclipse::parse-macro access-fn lambda-list body)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (eclipse:setf-expander ',access-fn) (function ,f))
       ,(when doc `(setf (eclipse:documentation ',access-fn 'eclipse:setf) ,doc))
       ',access-fn)))

(defmacro eclipse:DEFMACRO (name lambda-list &body body)
  (multiple-value-bind (lambda doc)
      (eclipse::parse-macro name lambda-list body)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (eclipse:macro-function ',name)
	     (eclipse:named-function
	      #-cmu ,name
	      #+cmu (eclipse::macro ,name)
	      ,@(rest lambda)))
       ,(when doc `(setf (eclipse:documentation ',name 'eclipse:function) ,doc))       
       ',name)))

(defmacro eclipse:DEFUN (name lambda-list &body body)
  (multiple-value-bind (decl body doc) (eclipse:find-declarations body nil)
    (let* ((setfp (eclipse::setf-function-name-p name))
	   (key (eclipse::function-name-key name setfp)))
      `(progn
	 (,(if setfp
	       'eclipse:set-symbol-setf-function-value
	       'eclipse:set-symbol-function-value)
	  ',key
	  (eclipse:named-function ,name ,lambda-list
				  ,@(when decl `((declare ,@decl)))
				  (block ,key ,@body)))
	 ,(when doc `(setf (eclipse:documentation ',name 'eclipse:function) ,doc))
	 ',name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 8.2 MACRO EXPANSION                                          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Examines lambda-list and returns:
;;; 1. The parameter names used, in REVERSE order.
;;; 2. The environment parameter name, or nil.
;;; 3. The lambda-list, possibly bashed to remove (&environment name).
(defun lambda-vars (lambda-list &aux vars env)
  (do ((parameters lambda-list (cdr parameters)))
      ((endp parameters) (values vars
				 (cadr env)
				 (if env
				     (nconc (ldiff lambda-list env) (cddr env))
				   lambda-list)))
    (let ((parameter (car parameters)))
      (cond ((eq parameter '&environment)
	     (setq env parameters
		   parameters (cdr parameters)))
	    ((eclipse:lambda-list-keyword-p parameter))
	    ((symbolp parameter) (push parameter vars))
	    ((consp parameter)
	     (destructuring-bind (parameter
				  &optional default (suplied nil sp))
		 parameter
	       (declare (ignore default))
	       (push (if (consp parameter)
			 (second parameter)
		       parameter)
		     vars)
	       (when sp (push suplied vars))))))))

(defmacro eclipse:DEFSETF (access-fn update &rest body)
  (if (consp update)
      (let* ((lambda-list update)
	     (stores (pop body)))
	(if (or (find '&key lambda-list)
		(find '&rest lambda-list))
	    (multiple-value-bind (locals env lambda-list)
	        (lambda-vars lambda-list)
  	      (multiple-value-bind (decl body doc) (eclipse:find-declarations body nil)
	        `(eclipse:define-setf-expander ,access-fn
		   (&rest subforms
			  ,@(when env `(&environment ,env)))
		   ,doc				   
		   (let ((temps (eclipse::make-symbols subforms "ARG"))
			 (store-temps (eclipse::make-symbols ',stores)))
		     (values temps subforms store-temps
			     `((lambda ,',lambda-list
				 ,(eclipse:apply #'(lambda ,`(,@stores ,@locals)
					     ,`(declare ,@decl)
					     (block ,access-fn
					       ,@body))
					 (append store-temps ',locals)))
			       ,@temps)
			     (cons ',access-fn temps))))))
	    ;; An optimization of the above that makes prettier code.
	    (let ((locals (reverse (lambda-vars lambda-list))))
	      `(eclipse:define-setf-expander ,access-fn ,lambda-list
		 (let* ((temps (eclipse::make-symbols ',locals))
			(store-temps (eclipse::make-symbols ',stores))
			(setter (eclipse:apply #'(lambda (,@locals ,@stores)
					   ,@body)
				       (append temps store-temps))))
		   (values temps (list ,@locals) store-temps setter
			   (cons ',access-fn temps)))))))
      `(eclipse:define-setf-expander ,access-fn (&whole form &rest args)
	 ,@(when body `(,(car body)))
	 (declare (ignore args))
	 (generate-setf-method
	  form '(,update) nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;n
;;; 8.4 COMPILER MACROS                                          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro eclipse:DEFINE-COMPILER-MACRO (name lambda-list &body body)
  (multiple-value-bind (lambda doc)
      (eclipse::parse-macro name lambda-list body :compiler-p t)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       ,(when doc `(setf (eclipse:documentation ',name 'eclipse:compiler-macro) ,doc))
       (setf (eclipse:compiler-macro-function ',name) (function ,lambda))
       ',name)))