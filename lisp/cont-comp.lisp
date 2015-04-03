;;; Some of the macros here will be circular in totally
;;; interpreted code (i.e. code which macroexpands during
;;; interpetation of the body).  Therefore, when machine-compiling, we
;;; make these not be at top-level so that we don't install an
;;; intepreted macro-function.
(macrolet
    ((xdefmacro (&rest forms)
		#+machine-compile
		`(let () (defmacro ,@forms))
		#-machine-compile
		`(defmacro ,@forms)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun eclipse::DESTRUCTURE-LAMBDA (lambda-list body form-var default ignore-doc-p)
  (multiple-value-bind (decl body doc)
      (eclipse:find-declarations body ignore-doc-p)
    (multiple-value-bind (bindings checks decl2 real-bindings)
	(eclipse::parse-lambda lambda-list form-var :macro t :default default)
      (values bindings (nconc decl decl2) (nconc checks body) doc real-bindings))))

(xdefmacro eclipse:DESTRUCTURING-BIND (lambda-list expression &rest body)
  (let ((form-var (gensym "FORM")))
    (multiple-value-bind (bindings decs body)
	(eclipse::destructure-lambda lambda-list body form-var nil t)
      `(let* ((,form-var ,expression)
	      ,@bindings)
	 (declare ,@decs)
	 ,@body))))


;;; IWBNI this took a check-lock-p argument which signals a
;;; continuable error if the package is locked.
;;; Should this check to see if the package is deleted?
(xdefmacro eclipse:IN-PACKAGE (name)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     #+cmu (cl:in-package ,name)
     (setq *package* (eclipse::pkg ,name))))


(xdefmacro eclipse:SETF (&rest args &environment env)
  (loop for (place value) on args by #'cddr
	collect (multiple-value-bind (temps vals stores store-form value-form)
		    (eclipse:get-setf-expansion place env)
		  (if (and (eq place value-form) (symbolp place))
		      `(setq ,place ,value)
		      (let ((bindings (eclipse::make-binding-list temps vals)))
			(if (null (cdr stores))
			    `(let* (,@bindings (,(car stores) ,value))
			       ,store-form)
			    `(let* ,bindings
			       (multiple-value-bind ,stores ,value
				 ,store-form))))))
	into results
	finally (return (if (cdr results)
			    (cons 'progn results)
			    (car results)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 5.3.1 Declaring Global Variables and Named Constants

(xdefmacro eclipse:DEFPARAMETER (name &optional initial-value
				     (documentation nil docp))
  `(progn
     #+lisp-host (eclipse::host-declaim (special ,name))
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (eclipse:global-declaration
	      ',name 'eclipse::global-variable 'special) t))
     ,(when docp
	`(setf (eclipse:documentation ',name 'variable) ,documentation))
     (setq ,name ,initial-value)
     ',name))

(xdefmacro eclipse:DEFVAR (name &optional (initial-value nil init-p)
			       (documentation nil docp))
  `(progn
     #+lisp-host (eclipse::host-declaim (special ,name))
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (eclipse:global-declaration
	      ',name 'eclipse::global-variable 'special) t))
     ,(when docp
	`(setf (eclipse:documentation ',name 'variable) ,documentation))
     ,(when init-p
	`(unless (eclipse:boundp ',name) (setq ,name ,initial-value)))
     ',name))

(xdefmacro eclipse:DEFCONSTANT (name &optional initial-value
				    (documentation nil docp))
  `(progn
     #+lisp-host (eclipse::host-declaim (special ,name))
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (eclipse:global-declaration
	      ',name 'eclipse::global-variable 'special) t)
       (if (eclipse:boundp ',name)
	   (when eclipse::*constant-check-hook*
	     (eclipse::redefine-constant ',name ,initial-value))
	   (eclipse::set-symbol-value-value ',name ,initial-value))
       (setf (eclipse:global-declaration
	      ',name 'eclipse::global-variable 'eclipse::constant) t))
     ,(when docp
	`(setf (eclipse:documentation ',name 'variable) ,documentation))
     ',name))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 6.4 LOGICAL OPERATORS                                        ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(xdefmacro eclipse:AND (&rest forms)
  (if forms
      (destructuring-bind (form &rest rest) forms
	(if rest
	    `(if ,form (eclipse:and ,@rest) nil)
	    form))
      t))

(xdefmacro eclipse:OR (&rest forms)
  (destructuring-bind (&optional form &rest rest) forms
    (if rest
	(let ((var (gensym)))
	  `(let ((,var ,form))
	     (if ,var ,var (eclipse:or ,@rest))))
      form)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 7.1 CONSTANTS AND VARIABLES                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defsetf eclipse:SYMBOL-VALUE eclipse:set)

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun generate-pset (args env &optional atomic-operator &aux setters)
  (labels ((pset1 (bindings pairs)
		  (if pairs
		      (destructuring-bind (name value . more) pairs
			(when (and atomic-operator (consp name))
			  (eclipse:signal-program-error
			   "Compound variable form ~a in ~a."
			   name atomic-operator))
			(multiple-value-bind (subvars subvals vals setter)
			    (eclipse:get-setf-expansion name env)
			  (push setter setters)
			  (setq bindings
				(append bindings
					(eclipse::make-binding-list subvars subvals)))
			  (if (cdr vals)
			      `(let ,bindings
				 (eclipse:multiple-value-bind ,vals ,value
				   ,(pset1 nil more)))
			      (pset1 (append bindings `((,(car vals) ,value)))
				     more))))
		      (let ((forms (nreverse (cons nil setters))))
			(if bindings `(let ,bindings ,@forms) forms)))))
    (when args (pset1 nil args)))))

(xdefmacro eclipse:PSETQ (&rest args &environment env)
  (generate-pset args env 'psetq))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 7.2 GENERALIZED VARIABLES                                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Properly handles multiple store forms, but "optimizes" the
;;; single-store-form and variable-as-place cases.
(defun eclipse::modify-body (place env form)
  (if (symbolp place)
      `(setq ,place ,form)
      (multiple-value-bind (vars vals stores store-form access-form)
	  (eclipse:get-setf-expansion place env)
	(if (cdr stores)
	    (let ((place1 (car stores)))
	      `(let* ,(eclipse::make-binding-list vars vals)
		 (eclipse:multiple-value-bind ,stores ,access-form
		   (setq ,place1 ,(subst place1 place form))
		   ,store-form)))
	    `(let* ,(eclipse::make-binding-list
		     `(,@vars ,@stores)
		     `(,@vals ,(subst access-form place form)))
	       ,store-form)))))

(xdefmacro eclipse:DEFINE-MODIFY-MACRO (name lambda-list function &optional
					    doc-string)
  (let* ((parameters nil)
	 (rest
	  (do ((lambdas lambda-list (cdr lambdas)))
	      ((null lambdas))
	    (let ((parameter (car lambdas)))
	      (case parameter
		(&rest (return (cadr lambdas)))
		(&optional)
		(t (push (eclipse:bindingform-name parameter) parameters)))))))
    (let ((form `(append (list ',function place ,@parameters) ,rest)))
      `(eclipse:defmacro ,name (place ,@lambda-list &environment env)
	 ,doc-string
	 (eclipse::modify-body place env ,form)))))

(xdefmacro eclipse:PSETF (&rest args &environment env)
  (generate-pset args env))

(xdefmacro eclipse:SHIFTF (&rest args &environment env)
  (let (let*-bindings forms)
    (do ((first t nil)
	 (a args (cdr a))
	 (prev-store-vars)
	 (prev-setter))
	((endp (cdr a))
	 (push `(eclipse:multiple-value-bind ,prev-store-vars ,(car a)
		  ,prev-setter) forms))
      (multiple-value-bind
	  (temps exprs store-vars setter getter)
	  (eclipse:get-setf-expansion (car a) env)
	(mapc #'(lambda (temp expr)
		  (push `(,temp ,expr) let*-bindings))
	      temps exprs)
	(push (if first
		  getter
		`(eclipse:multiple-value-bind ,prev-store-vars ,getter
		   ,prev-setter)) forms)
	(setf prev-store-vars store-vars)
	(setf prev-setter setter)))
    `(let* ,(nreverse let*-bindings)
       (eclipse:multiple-value-prog1
	   ,@(nreverse forms)))))

(xdefmacro eclipse:ROTATEF (&rest args &environment env)
  (when args
    (let* (let*-bindings mv-bindings setters
	   (getters (cons nil nil))
	   (tail getters))
      (dolist (arg args)
	(multiple-value-bind (temps subforms store-vars setter getter)
	    (eclipse:get-setf-expansion arg env)
          (mapc #'(lambda (temp subform)
		    (push (list temp subform) let*-bindings))
		temps subforms)
	  (push store-vars mv-bindings)
	  (push setter setters)
	  (setf (cdr tail) (cons getter nil)
		tail (cdr tail))))
      (push nil setters)
      (setq setters (nreverse setters))
      (pop getters)
      (setf (cdr tail) (cons (pop getters) nil))
      (labels ((thunk (mv-bindings getters)
		 (if mv-bindings
		     `((eclipse:multiple-value-bind
			   ,(car mv-bindings)
			   ,(car getters)
			 ,@(thunk (eclipse:cdr mv-bindings) (eclipse:cdr getters))))
		   setters)))
	`(let* ,(nreverse let*-bindings)
	   ,@(thunk (nreverse mv-bindings) getters))))))

(define-setf-expander eclipse:THE (typespec place &environment env)
  (multiple-value-bind (temps values stores set get)
      (eclipse:get-setf-expansion place env)
    (let ((new-value (first stores)))
      (values temps
	      values
	      stores
	      (subst `(eclipse:the ,typespec ,new-value) new-value set)
	      get))))

(define-setf-expander eclipse:VALUES (&rest places &environment env)
  (let (temps values stores null-stores set get)
    (dolist (place (reverse places))
      (multiple-value-bind (ptemps pvalues pstores pset pget)
	  (eclipse:get-setf-expansion place env)
	(setq temps (append ptemps temps)
	      values (append pvalues values)
	      stores (cons (first pstores) stores)
	      null-stores (append null-stores (rest pstores))
	      set (cons pset set)
	      get (cons pget get))))
    (values (append temps null-stores)
	    (append values (make-list (length null-stores)))
	    stores
	    `(eclipse:values ,@set)
	    `(eclipse:values ,@get))))

(define-setf-expander eclipse:APPLY (function &rest args)
  (let ((setter (if (eclipse:car-eq function 'function)
		   `(function (setf ,(second function)))
		   (eclipse:signal-program-error
		    "~s is not of the form (function <name>)." function)))
	(temps (eclipse::make-symbols args "ARG"))
	(store (gensym "VALUE")))
    (values temps args (list store)
	    `(eclipse:apply ,setter ,store ,@temps)
	    `(eclipse:apply ,function ,@temps))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 7.4 SIMPLE SEQUENCING                                            ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(xdefmacro eclipse:PROG1 (first &rest forms)
  (let ((var (gensym "FIRST-VAL")))
    `(let ((,var ,first))
       ,@forms
       ,var)))

(xdefmacro eclipse:PROG2 (first second &rest forms)
  (let ((var (gensym "SECONDVAL")))
    `(progn ,first
	    (let ((,var ,second))
	      ,@forms
	      ,var))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 7.6 CONDITIONALS                                                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(xdefmacro eclipse:WHEN (test &rest forms)
  `(if ,test (progn ,@forms)))

(xdefmacro eclipse:UNLESS (test &rest forms)
  `(if (not ,test) (progn ,@forms) nil))

(xdefmacro eclipse:COND (&rest clauses)
  (if clauses
      (destructuring-bind (test &rest forms) (car clauses)
	(if forms
	    (case test
	      ((t) `(progn ,@forms))
	      ((nil) `(eclipse:cond ,@(cdr clauses)))
	      (t `(if ,test
		      (progn ,@forms)
		      (eclipse:cond ,@(cdr clauses)))))
	    (let ((n-result (gensym "TEST")))
	      `(let ((,n-result ,test))
		 (if ,n-result
		     ,n-result
		     (eclipse:cond ,@(cdr clauses)))))))
      nil))

;;; Potential optimization: The compiler should treat case as a
;;; special form.  Consider all the keys which are numbers, characters
;;; or symbols.  If there are more than two, we should examine their
;;; SXHASH, removing from condideration any that have matching sxhash
;;; values.  If there are still more than two such keys being
;;; condidered, than these keys should be tested in a SWITCH based on
;;; SXHASH of the key value.  Any remaining clauses get tested in a
;;; COND in the usual way.  For example:
;;;  (case (some-form) (1 'x) (sym 'y) (t 'z))
;;;  => (let ((#:g1 (some-form)))
;;;       (switch (sxhash #:g1) (1 'x) (987654 'y) (t 'z)))
;;; Instead of SXHASH, we should use some even faster primitive
;;; function that hashes base-chars to a values above 256.  We might
;;; even want to store the hash key within symbols.

;;; In our implementation, only non-fixnum numbers behave differently
;;; under eq and eql. 
(defun eclipse::eq-test-p (x)
  (or (eclipse:fixnump x) (not (numberp x))))

(xdefmacro eclipse:CASE (keyform &rest clauses)
  (loop with keyplace = (gensym "KEY")
	for (keys . forms)  in clauses
	unless forms
	do (setq forms '(nil))
	when (and (consp keys) (null (cdr keys)) (not (eq (car keys) 't)))
	do (setq keys (car keys))
	collect (cond ((or (eq keys 't) (eq keys 'otherwise))
		       `(t ,@forms))
		      ((consp keys)
		       `((member ,keyplace ',keys
				 :test ',(if (every #'eclipse::eq-test-p keys)
					     'eq 'eclipse:eql)) ,@forms))
		      (t
		       `((,(if (eclipse::eq-test-p keys) 'eq 'eclipse:eql)
			  ,keyplace ',keys) ,@forms)))
	into results
	finally (return (if results
			    `(let ((,keyplace ,keyform))
			       (eclipse:cond ,@results))
			    `(progn ,keyform nil)))))

;;; Sketch:
;;; + Types which are classes for which each (indirect) subtype
;;;   (including the class itself) is sealed and has a proper name,
;;;   can be tested in a CASE keyed off the (CLASS-NAME-OF keyform).
;;;   A case clause label consists of the proper-name if the class has
;;;   no subclasses, or a list of all the (indirect) subclasses 
;;;   (including class itself) which are not abstract.
;;; + Any clauses that are not case-testable, plus the default, are
;;;   handled in the default clause of the case, using a COND form.
;;;   If this forms the only branch of the CASE, then the case is
;;;   ommitted, leaving only the COND.
;;; + When processing the entire typecase or a cond group within it, the
;;;   first case-testable clause marks the end of the current cond (if
;;;   any), and the start of a new case.  The new case is handled
;;;   within the default clause of the COND.  Similarly, the first
;;;   non-case-testable clause marks the end of the current case (if
;;;   any), and the start of a new cond.
;;; + When processing a type for a cond clause that cannot be
;;;   case-tested, the following tests are used:
;;;     1. classes are tested for by (FIND class cpl), where cpl is a
;;;        local variable holding the CLASS-PRECEDENCE-LIST of the
;;;        CLASS-OF the form.  This variable only appears if it is
;;;        used.
;;;    2. fixnum and bignum are tested with the FIXNUMP predicate.
;;;       (We should have a BIGNUMP predicate, too!!!)
;;;    3. Everything else is tested with (TYPEP key type), where type
;;;       is the intersection of the specified type for this clause,
;;;       and the complement of all classes which precededed it.

;;; Potential optimization:
;;; When a case-testable clause follows a non-case-testable clause,
;;; but can be proven that the non-case-testable type is not a subtype
;;; of the case-testable one, then it can appear in a CASE that
;;; precedes the COND.  This can be used to avoid having multiple CASE
;;; forms.

;;; Potential optimization:
;;; Fixnum can be handled as a case keyed off of TYPE-NAME-OF instead
;;; of CLASS-NAME-OF, provided that no other case is INTEGER. This can
;;; be used to avoid having both a CASE and a COND, and cuts down on
;;; the number of CASES broken up by intervening two-branch CONDs.

(defun eclipse::case-keys (class env)
  (when (eclipse:typep class 'eclipse:class)
    (do ((classes (list class))
	 (keys nil))
	((null classes) keys)
      (let ((class (first classes)))
	(unless (or (eclipse:sealed-class-p class)
		    (eclipse:typep class 'eclipse:built-in-class))
	  (return nil))
	(unless (eclipse:abstract-class-p class)
	  (let ((name (eclipse::proper-name class nil env)))
	    (if name
		(pushnew name keys)
		(return nil))))
	(setq classes (append (rest classes)
			      (eclipse:class-direct-subclasses class)))))))

(xdefmacro eclipse:TYPECASE (keyform &rest clauses &environment env)
  (let ((key (gensym "OBJECT"))
	(name (gensym "CLASS-NAME"))
	(cpl (gensym "CPL"))
	namep cplp default-forms groups all-case-keys
	current-case-clauses current-cond-clauses)
    (flet ((close-case ()
		       (when current-case-clauses
			 (push 'case current-case-clauses)
			 (push current-case-clauses groups)
			 (setq current-case-clauses nil)))
	   (close-cond ()
		       (when current-cond-clauses
			 (push 'cond current-cond-clauses)
			 (push current-cond-clauses groups)
			 (setq current-cond-clauses nil))))
      (do ((clauses clauses (cdr clauses)))
	  ((endp clauses))
	(destructuring-bind (this-typespec . forms) (car clauses)
	  (when (or (eq this-typespec 't) (eq this-typespec 'otherwise))
	    (if (rest clauses)
		(eclipse:signal-program-error
		 "~s follows default clause in TYPECASE." (rest clauses))
		(return (setq default-forms forms))))
	  (let* ((these-types1 (eclipse::expand-type this-typespec nil env)) 
		 (these-types (if (eclipse:typep these-types1 'eclipse::type-union)
				  (eclipse::component-types these-types1)
				  (list these-types1)))
		 (case-keys
		  (loop for type in these-types
			for key = (eclipse::case-keys type env)
			if key nconc key
			else return nil)))
	    (cond
	     (case-keys (setq namep t)
			(close-cond)
			;; Make sure we don't have duplicates
			(setq case-keys
			      (nset-difference case-keys all-case-keys))
			(setq all-case-keys
			      (append all-case-keys case-keys))
			(push `(,case-keys ,@forms) current-case-clauses))
	     (t (close-case)
		(let* ((this-type (if (cdr these-types)
				      (eclipse::union-types
				       these-types nil env)
				      (car these-types)))
		       (type-name (eclipse:type-name this-type))
		       (type-object
			#+lisp-host
			`(eclipse:load-time-value
			  (eclipse::expand-type ',type-name nil))
			#-lisp-host this-type))
		  (push `(,(cond ((eclipse:typep this-type 'eclipse:class)
				  (setq cplp t)
				  `(eclipse::find-list-eq ,type-object ,cpl))
				 ((equal type-name
					 `(eclipse:integer ,eclipse:most-negative-fixnum
							   ,eclipse:most-positive-fixnum))
				  `(eclipse:fixnump ,key))
				 (t `(eclipse:typep ,key ,type-object)))
			  ,@(or forms '(nil)))
			current-cond-clauses)))))))
      (close-case) (close-cond)
      (dolist (group groups)
	(setq default-forms
	      (destructuring-bind (key . clauses) group
		(when default-forms
		  (push `(t ,@default-forms) clauses))
		(case key
		  (case `((eclipse:case ,name ,@(nreverse clauses))))
		  (cond `((eclipse:cond ,@(nreverse clauses))))))))
      (if groups
	  `(let* ((,key ,keyform)
		  ,@(when namep `((,name (eclipse:class-name-of ,key))))
		  ,@(when cplp `((,cpl (eclipse:class-precedence-list-of ,key)))))
	     ,@default-forms)
	  `(progn ,keyform ,@default-forms)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 7.7 BLOCKS AND EXITS                                         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(xdefmacro eclipse:RETURN (&optional result)
  `(return-from nil ,result))

)