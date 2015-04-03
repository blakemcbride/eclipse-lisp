
(defun eclipse::symbol-function-value (name)
  ;;(unless (member (first (eclipse:global-declaration name 'eclipse::global-function 'ftype))
  ;;		  '(ec:macro ec:function) :test #'eq)
    (or (get name 'gf)
	#+cmu (macro-function name) 
	(when (fboundp name)
	  (symbol-function name)));;)
    )

(defun eclipse::symbol-setf-function-value (name)
  (or (get name 'setf nil)
      (let ((name `(setf ,name)))
	(when (fboundp name)
	  (fdefinition name)))))

(defun update-host-fdefinition-from-backpointer (instance)
  (let ((backpointer (funcallable-standard-instance-backpointer
		      instance))
	(function (funcallable-standard-instance-function instance)))
    (when (and backpointer function)
      #+not-now
      (progn
	(format t "backpointer type is ~s, function type is ~s.~%"
		(type-of backpointer) (type-of function))
	(format t "backpointer is ~s, function is ~s.~%" backpointer function)
	(format t "(and backpointer function) => ~s~%" (and backpointer function))
	(format t "(when (and backpointer function) 33) => ~s~%"
		(when (and backpointer function) 33)))
      #-cmu (setf (fdefinition backpointer) function)
      #+cmu (c::%%defun backpointer function nil))
    #+not-now
    (format t "Returning from update.~%")))

(defun eclipse:set-symbol-function-value (name function)
  (etypecase function
    #+allegro-v4.1
    (macro-function (setf (macro-function name)
			  (eclipse::macro-function-function name function)))    
    (function (if (eclipse::macro-function-p function)
		  (setf (macro-function name)
			(eclipse::macro-function-function name function))
		  (setf (symbol-function name) function)))
    #+(and excl (not allegro-v4.1))
    (excl::closure (setf (macro-function name)
			  (eclipse::macro-function-function name function)))
    (eclipse::funcallable-standard-instance
     (setf (get name 'gf) function)
     (setf (funcallable-standard-instance-backpointer function) name)
     (update-host-fdefinition-from-backpointer function)
     function)
    #+was
    (eclipse::funcallable-standard-instance
     (setf (funcallable-standard-instance-backpointer function)
	   name)
     (setf (get name 'gf) function))
    #+was
    (eclipse::standard-instance		;no longer used?
     (setf (symbol-function name) (host-closure function)))
    (null (remprop name 'gf)
	  (fmakunbound name)
	  function)))
      

(defun eclipse:set-symbol-setf-function-value (name function)
  (let ((key `(setf ,name)))
    (etypecase function
      (function (setf (fdefinition key) function))
      (eclipse::funcallable-standard-instance
       (setf (get name 'setf) function)
       (setf (funcallable-standard-instance-backpointer function) key)
       (update-host-fdefinition-from-backpointer function)
       function)
      #+was
      (eclipse::funcallable-standard-instance
       (setf (funcallable-standard-instance-backpointer function) key
	     (get name 'setf) function))
      #+was
      (eclipse::standard-instance	;no longer used?
       (setf (fdefinition key) (host-closure function)))
      (null (remprop name 'setf)
	    (fmakunbound key)
	    function))))


(defun eclipse:setf-expander (name)
  (or #+excl (get name 'EXCL::SETF-METHOD-EXPANDER)
      #+cmu (lisp::info lisp::setf lisp::expander name)
      (let ((inverse #+excl (get name 'EXCL::SETF-INVERSE)
		     #+cmu (lisp::info lisp::setf lisp::inverse name)))
	(when inverse
	  #'(lambda (place environment)
	      (declare (ignore environment))
	      (generate-setf-method place `(,inverse) nil))))))

	

(defun eclipse:set-setf-expander (name f)
  (let ((f (etypecase f
	     (function f)
	     (eclipse::standard-instance (host-closure f)))))
    #+excl (setf (get name 'EXCL::SETF-METHOD-EXPANDER) f)
    #+cmu (cl::%define-setf-macro name f nil nil)))

(defun function-name (f)
  #+cmu
  (if (eval:interpreted-function-p f)
	    (nth-value 2 (eval:interpreted-function-lambda-expression f))
	    (lisp::%function-name (lisp::%function-self f)))
  #-cmu (nth-value 2 (cl:function-lambda-expression f)))

(defmacro eclipse:named-function (name lambda-list &body body)
  (let ((name (cond ((eclipse::setf-function-name-p name)
		     `(setf ,(eclipse::function-name-key name t)))
		    ((consp name)
		     #+excl name
		     #+cmu (intern (princ-to-string name)))
		    (t name))))
    #+cmu
    `(progn
       (c::%defun ',name (function (lambda ,lambda-list ,@body)) nil nil)
       (prog1 (fdefinition ',name) (fmakunbound ',name)))
    #+excl
    `(excl:named-function ,name (lambda ,lambda-list ,@body))))

(defun eclipse::macro-function-p (f)
  #+allegro-v4.1 (typep f 'macro-function)
  #+(and excl (not allegro-v4.1)) (excl::macro-closure-object-P f)
  #+cmu (let ((name (and (functionp f) (function-name f))))
	  (and (stringp name) (search "DEFMACRO" name))))
  
(defun eclipse::make-macro-function (name f)
  (fmakunbound name)
  (setf (macro-function name)
	(etypecase f
	  (function f)
	  (eclipse::standard-instance (host-closure f))))
  (symbol-function name))
  

(defun eclipse::macro-function-function (name f)
  (declare (ignorable name f))
  (macro-function name))

(defmacro eclipse::funcall-function (f &rest args)
  `(locally-pretty-fast
     (funcall (the function ,f) ,@args)))

(defmacro eclipse::apply-function (f &rest args)
  `(locally-pretty-fast
     (apply (the function ,f) ,@args)))

  