;;; There is an asymmetry between setting and getting macro functions.

;;; MACRO-FUNCTION and COMPILER-MACRO-FUNCTION (there is no
;;; symbol-macro-function,) all take a name and an optional
;;; environment, and use xxx-information as necessary to return the
;;; right expansion function from the environment.  They use
;;; global-macro-function, global-compiler-macro-function and
;;; global-symbol-macro-function, if necessary, to get the global
;;; value.

;;; (SETF MACRO-FUNCTION), (SETF COMPILER-MACRO-FUNCTION), and (SETF
;;; SYMBOL-MACRO-FUNCTION) set only the global value.
;;; To remove a global definition:
;;; macro-function: use fmakunbound. (Note that expander arg to (setf
;;;    macro-function) must be a  function!)
;;; compiler-macro-function: Use NIL as expander arg to (setf
;;;    compiler-macro-functin) 
;;; symbol-macro-function: Not defined by ANSI. (Use NIL as expander
;;;    arg to (setf symbol-macro-function)).


;;; ACCESSORS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
(defun eclipse::global-macro-function (name)
  (eclipse::macro-function-function name (symbol-function name)))

;;; global-symbol-macro-function is in env.lisp

(defun global-compiler-macro-function (function-name)
  (let ((setfp (eclipse::setf-function-name-p function-name)))
    (eclipse::system-property
     (eclipse::function-name-key function-name setfp)
     (if setfp 'setf-compiler-macro 'compiler-macro)
     nil)))


;;; These idioms use the local-definition, if appropriate, otherwise
;;; the global one.
(defun eclipse::get-macro-function (local-def name)
  (if local-def
      (eclipse::binding-value local-def)
      (eclipse::global-macro-function name)))

(defun eclipse::get-symbol-macro-function (local-def name)
  (if local-def
      (eclipse::binding-value local-def)
      (eclipse::global-symbol-macro-function name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 8.1 MACRO DEFINITION                                         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun eclipse:MACRO-FUNCTION (symbol &optional env)
  (multiple-value-bind (function-type local-def)
      (eclipse:function-information (eclipse::check-typef symbol 'symbol) env)
    (case function-type
      (:macro (eclipse::get-macro-function local-def symbol)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 8.2 MACRO EXPANSION                                          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun eclipse::expand (func form env)
  (funcall eclipse:*macroexpand-hook* func form env))

(defun form-information (form environment)
  (let* ((consp (consp form))
	 (key (if consp (car form) form)))
    (when (or (symbolp key) (eclipse:car-eq key 'lambda))
      (multiple-value-bind (type binding)
	  (if consp
	      (eclipse:function-information key environment)
	      (eclipse:variable-information key environment))
	(values
	 (or type (if consp :function :special))
	 binding key)))))

(defun eclipse:MACROEXPAND-1 (form &optional env)
  (multiple-value-bind (function-type binding key)
      (form-information form env)
    (case function-type
      (:macro
       (values (eclipse::expand (eclipse::get-macro-function binding key) form env)
	       t))
      (:symbol-macro
       (values (eclipse::expand (eclipse::get-symbol-macro-function binding key) form env)
	       t))
      (t (values form nil)))))

(defun eclipse:MACROEXPAND (form &optional env)
  (multiple-value-bind (form expanded-p) (eclipse:macroexpand-1 form env)
    (if expanded-p
	(values (eclipse:macroexpand form env) t)
	(values form nil))))

;;; generate-setf-method is defined in mop-init.lisp

(defun eclipse:GET-SETF-EXPANSION (place &optional environment)
  #-machine-compile
  (unless (eclipse::tagged-instance-p environment)
    (return-from eclipse:get-setf-expansion
      (host::get-setf-expansion place environment)))
  (multiple-value-bind (type binding key)
      (form-information place environment)
    ;; Expansion functions are global and are shadowed by local
    ;; bindings.
    (let ((expander (unless (or binding (eq key place))
		      (eclipse:setf-expander key))))
      (if expander
	  (eclipse:funcall expander place environment) ; Should this use EXPAND?
	  (ecase type
	    (:macro
	     (eclipse:get-setf-expansion
	      (eclipse::expand (eclipse::get-macro-function binding key) place environment)
	      environment))
	    (:symbol-macro
	     (eclipse:get-setf-expansion
	      (eclipse::expand (eclipse::get-symbol-macro-function binding key) place environment)
	      environment))
	    ((:special :lexical)
	     (let ((var (gensym "VALUE")))
	       (values nil nil `(,var) `(setq ,place ,var) place)))
	    (:function
	     (generate-setf-method
	      place `(funcall (function (setf ,key))) t)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;n
;;; 8.4 COMPILER MACROS                                          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun eclipse:COMPILER-MACRO-FUNCTION (name &optional env)
  (multiple-value-bind (function-type local-def declarations)
      (eclipse:function-information name env)
    (declare (ignore function-type))
    (unless (or local-def
		(eql (getf declarations 'inline) 'notinline))
      (global-compiler-macro-function name))))



;;; COMPILER-MACROEXPAND and COMPILER-MACROEXPAND-1 do not appear to
;;; be defined by ANSI.  

