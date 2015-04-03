;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                    SPECIAL FORMS                             ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; N.B.: SPECIAL-OPERATOR-P is true only for a fixed set of
;;; operators.  Special-Operator returns the walker function for
;;; operators that are handled specially in this implementation.  It
;;; is possible for Special-Operator to return one function (a walker)
;;; and for SYMBOL-FUNCTION to return a different one (the normal
;;; funcallable function).

;;; This whole thing is probabably better handled by defining methods
;;; on WALK-GLOBAL-COMBINATION that are eql specialized on name.  This
;;; would provide the added benefit of making it easy to provide
;;; special forms in some dynamic environments, but not others.

(defun special-operator (name)
  (system-property name 'special-operator nil))
(defun (setf special-operator) (walker name)
  (system-property-setter name 'special-operator walker))

(defmacro DEFSPECIAL (name lambda-list
			   (&optional (dynamic-env 'dynamic-env)
				      (lexical-env 'lexical-env)
				      (eval-targets 'eval-targets)
				      (declarations 'declarations))
			   &body body)
  (let ((form (gensym "FORM")))
    (multiple-value-bind (decls body) (find-declarations body t)
      `(progn (setf (special-operator ',name)
		    #'(lambda (,form ,lexical-env ,dynamic-env ,eval-targets
				     ,declarations)
			(declare ,@decls)
			(destructuring-bind ,lambda-list (rest ,form)
			  (block ,name ,@body))))
	      ',name))))

(defspecial PROGN (&rest forms) ()
  (walk-sequence forms lexical-env dynamic-env eval-targets declarations))

(defspecial QUOTE (form) ()
  (declare (ignore lexical-env))
  (walk-constant form dynamic-env eval-targets declarations))

(defspecial THE (value-type form) ()
  (walk form lexical-env dynamic-env eval-targets
	    (add-type-to-declarations
	     (return-type value-type)		 
	     declarations form dynamic-env)))

(defspecial LOCALLY (&body body) ()
  (multiple-value-bind (decls body) (find-declarations body t)
    (let ((new-env (augment-environment lexical-env :declare decls)))
      (when (and (top-level-p lexical-env)
		 (not (top-level-p new-env)))
	(setf new-env (make-top-level-env :env new-env)))
      (walk-sequence body new-env dynamic-env eval-targets
		     declarations)))) 

(defspecial LOAD-TIME-VALUE (form &optional read-only-p) ()
  (if (and (compiler-env-p dynamic-env)
	     (not (constantp form)))
      (let* ((name (make-symbol "LTV"))
	     (id (intern-constant dynamic-env name)))
	(add-init-code dynamic-env
		       (walk form nil dynamic-env
			     (add-target (make-constant-target :binding id))
			     declarations)
		       read-only-p)
	;; IWBNI we extracted the dynamic type of the result and added it
	;; to the declarations.
	(target-special id dynamic-env eval-targets declarations))
      (walk form lexical-env dynamic-env eval-targets
	    `(single-value t ,@declarations))))

;;; IWBNI macros inside a top-level (eval-when (compile load) ...)
;;; weren't expanded twice.
(defspecial EVAL-WHEN (situations &body body) ()
  (let (compile load execute)
    (dolist (situation situations)
      (ecase situation
	((:compile-toplevel compile) (setf compile t))
	((:load-toplevel load) (setf load t))
	((:execute eval) (setf execute t))))
    (when (and (file-compiler-env-p dynamic-env)
	       (top-level-p lexical-env))
      (when compile
	(dolist (form body)
	  #+machine-compile (eval-env form lexical-env)
	  #-machine-compile
	  (case user::*eval*
	    ((nil) nil)
	    (trace (cl:format t "*** eval-env ~s ~s.~%" form lexical-env)
		   (eval-env form lexical-env))
	    (t (eval-env form lexical-env))))
	(setq lexical-env		;body not at toplevel any more
	      (make-variable-env :env lexical-env)))
      (setq execute load))
    (if execute
	(walk-sequence body lexical-env dynamic-env eval-targets
		       declarations) 
	(walk-constant nil dynamic-env eval-targets declarations))))

(defparameter *testvar* (make-symbol "TEST"))
(defspecial IF (predicate consequent &optional alternative) ()
  (let* ((name *testvar*)
	 (lexical-env (add-variable name lexical-env dynamic-env))
	 (predicate-binding (get-binding name lexical-env))
	 (predicate-result (walk-binding predicate-binding predicate
					 lexical-env dynamic-env nil))) 
    (typecase dynamic-env
      (COMPILER-ENV
       (let* ((cons (walk consequent lexical-env dynamic-env eval-targets
			  declarations))
	      (alt (walk alternative lexical-env dynamic-env eval-targets
			 declarations))
	      (complexp (or (null eval-targets)
			    (statements-p cons) (statements-p alt)))
	      (operator (if complexp 'if 'cond))
	      (assignment-p (assignment-p predicate-binding
					  predicate-result))
	      (pred (if assignment-p
			(assignment-value predicate-result)
			(read-value dynamic-env
				    predicate-binding
				    (binding-name predicate-binding))))
	      ;; If both branches set same variable we can move it outside.
	      (binding (unless complexp
			 (let* ((last-cons (last cons))
				(b (when (assignment-p nil last-cons)
				     (cadar last-cons))))
			   (when b
			     (let ((last-alt (last alt)))
			       (when (and (assignment-p nil last-alt)
					  (eql b (cadar last-alt)))
				 (rplaca last-cons (caddar last-cons))
				 (rplaca last-alt (caddar last-alt))
				 b))))))
	      (form (list operator pred cons alt))
	      (result (make-result dynamic-env
			(if binding `(setq ,binding ,form) form))))
	 (if assignment-p result
	     (make-block dynamic-env (list predicate-binding)
			 predicate-result result))))
      (t				;INTERPRETER
       (if predicate-result
	   (walk consequent lexical-env dynamic-env eval-targets
		 declarations)
	   (walk alternative lexical-env dynamic-env eval-targets
		 declarations))))))

(defspecial SETQ (&rest pairs) ()
  (if pairs
      (labels ((do-pairs (pairs)
		 (destructuring-bind (variable value . more) pairs
		   (if more
		       (combine-results
			dynamic-env
			(walk-assignment variable value lexical-env
					 dynamic-env (make-targets)
					 nil)
			(do-pairs more))
		     (walk-assignment
		      variable value lexical-env dynamic-env
		      eval-targets declarations))))) 
	(do-pairs pairs))
    (walk 'nil lexical-env dynamic-env eval-targets declarations)))

(defspecial LET* (bindings &body body) ()
  (multiple-value-bind (decls body) (find-declarations body t)
    (walk-bindings bindings decls body nil nil lexical-env dynamic-env
		   eval-targets declarations)))

(defspecial LET (bindings &body body) ()
  (let (vars parallel-specials inits (n-specials 0)
	     (exitp (find-if #'exit-target-p eval-targets)))
    (multiple-value-bind (decls body) (find-declarations body t)
      (multiple-value-bind (body-env init-env)
	  (add-variables (mapcar #'bindingform-name bindings)
			 lexical-env dynamic-env decls nil)
	(dolist (binding bindings)
	  (let ((var (get-binding (bindingform-name binding) body-env)))
	    (multiple-value-bind (init special)
		(walk-binding var (bindingform-initform binding)
			      init-env dynamic-env t)
	      (push var vars)
	      (when special
		(push special parallel-specials)
		(incf n-specials))
	      (setq inits (combine-results dynamic-env inits init)))))
	(make-block
	 dynamic-env vars inits
	 (make-protected
	  dynamic-env
	  #'(lambda (&aux inits)
	      (combine-results
	       dynamic-env
	       (dolist (binding parallel-specials inits)
		 (let ((name (binding-name binding)))
		   (setf inits
			 (combine-results
			  dynamic-env
			  inits
			  (combine-results
			   dynamic-env
			   (make-op dynamic-env 'dbind
				    (intern-constant dynamic-env name))
			   (make-result dynamic-env 
			     (target-value
			      dynamic-env
			      (make-global-target :binding name)
			      (if (compiler-env-p dynamic-env)
				  binding
				  (variable-value binding name)) nil)))))))
	       (walk-sequence body body-env dynamic-env eval-targets
			      (updated-declarations exitp declarations
						    body-env n-specials))))
	  #'(lambda ()
	      (unless exitp (make-unwind dynamic-env n-specials)))))))))

(defspecial SYMBOL-MACROLET (bindings &body body) ()
  (multiple-value-bind (decls body) (find-declarations body t)
    (walk-sequence
     body
     (augment-environment
      lexical-env :declare decls
      :symbol-macro bindings)
     dynamic-env eval-targets declarations)))

(defspecial MACROLET (bindings &body body) ()
  (multiple-value-bind (decls body) (find-declarations body t)
    (walk-sequence
     body
     (augment-environment
      lexical-env :declare decls
      :macro (mapcar #'(lambda (b)
			 (destructuring-bind (name lambda-list . body) b
			   (list name
				 (enclose
				  (parse-macro name lambda-list body)
				  lexical-env))))
		     bindings))
     dynamic-env eval-targets declarations)))


(defspecial FLET (bindings &body body) ()
  (flet-labels bindings body lexical-env dynamic-env eval-targets
	       declarations t))

(defspecial LABELS (bindings &body body) ()
  (flet-labels bindings body lexical-env dynamic-env eval-targets
	       declarations nil))

(defun flet-labels (bindings body lexical-env dynamic-env eval-targets
			     declarations parallel-p)
  (multiple-value-bind (decls body) (find-declarations body t)
    (multiple-value-bind (body-env definition-env)
	(augment-environment
	 lexical-env :declare decls
	 :function (mapcar #'bindingform-name bindings)
	 :index-p (file-compiler-env-p dynamic-env)
	 :bound-declarations-p (not parallel-p))
      (let ((ebindings (frame-bindings body-env))
	    (inits nil)
	    (fdef-set (when (compiler-env-p dynamic-env) (cons nil nil))))
	(dolist (binding ebindings)
	  (setf inits
		(combine-results
		 dynamic-env inits
		 (destructuring-bind (name lambda-list &body body)
		     (pop bindings)
		   (walk-function-definition
		    name lambda-list
		    (multiple-value-bind (decls body)
			(find-declarations body t)
		      `((declare ,@decls)
			(block ,(function-name-key name)
			  ,@body)))
		    t definition-env dynamic-env
		    (add-target
		     (make-lexical-target :binding binding))
		    (binding-declarations binding)
		    fdef-set)))))
	(let ((body-results
	       (walk-sequence body body-env dynamic-env eval-targets
			      declarations)))
	  (when fdef-set
	    (loop for binding in ebindings
		  and init in inits
		  and fdef in (nreverse (cdr fdef-set))
		  for decs = (binding-declarations binding)
		  if (getf decs 'used)
		  collect binding into used-bindings
		  and collect init into used-inits
		  and do (push fdef (compiler-env-functions dynamic-env))
		  and do (when (getf decs 'ignore)
			   (warn-used dynamic-env (binding-name binding) "Function"))
		  else unless (or (getf decs 'ignorable)
				  (getf decs 'ignore))
		  do (warn-unused dynamic-env (binding-name binding) "Function")
		  finally (setq ebindings used-bindings inits used-inits)))
	  (make-block
	   dynamic-env ebindings inits
	   body-results))))))


(defspecial NAMED-FUNCTION (function-name lambda-list &body body) ()
  (walk-function-definition function-name lambda-list body nil
			    lexical-env dynamic-env eval-targets declarations))

;;; Name is a function-name or lambda-expression.
(defspecial FUNCTION (name) ()
  (multiple-value-bind (binding-type binding function-declarations)
      (function-information name lexical-env)
    (ecase binding-type
      ((nil :function)
       (make-result
	dynamic-env
	(target (function-reference dynamic-env (or binding name)
				    lexical-env declarations)
		dynamic-env eval-targets
		(known-type dynamic-env 'function 
			    (combine-declarations
			     function-declarations
			     declarations (or binding name) dynamic-env))))))))


(defspecial PROGV (symbols values &body body) ()
  (let* ((mv-target (retain-values-target eval-targets))
	 (syms 'symbols)
	 (vals 'vals)
	 (id (make-tag :name 'progv :status mv-target))
	 (env1 (add-variables (list syms vals) lexical-env dynamic-env))
	 (env (make-control-env :bindings (list id) :env env1))
	 (symbol-binding (get-binding syms env1))
	 (value-binding (get-binding vals env1))
	 (null-targets (make-targets)))
    (index-tag id env1)
    (make-block
     dynamic-env (list symbol-binding value-binding)
     (combine-results
      dynamic-env
      (walk-binding symbol-binding symbols env dynamic-env nil)
      (walk-binding value-binding values env dynamic-env nil))
     (make-block
      dynamic-env nil
      (make-op dynamic-env 'progv-start id symbol-binding value-binding)
      (combine-results
       dynamic-env
       (make-protected dynamic-env
		       #'(lambda ()
			   (WALK-SEQUENCE body env dynamic-env
					  (add-target mv-target null-targets)
					  declarations))
		       #'(lambda ()
			   (make-op dynamic-env 'progv-end id)))
       (walk-saved-value id dynamic-env eval-targets declarations))))))

#+not-yet
(defspecial get-value (index) ()	;only used in compiler!
  (target `(get-value ,(if (fixnump index)
			   index
			   `(integer-int ,index)))
	  dynamic-env eval-targets declarations))

;;; A special form for performance reasons.
#+not-yet
(defspecial NTH-VALUE (n form) ()
  (typecase dynamic-env
    (compiler-env
     (let* ((var '#:index)
	    (env (add-variable var lexical-env dynamic-env))
	    (index-binding (get-binding var env))
	    (index-result (walk-binding index-binding n lexical-env dynamic-env nil))) 
       (make-block
	dynamic-env (frame-bindings env)
	index-result
	(walk form lexical-env dynamic-env targets nil)
	(walk `(get-value ,index-binding) dynamic-env eval-targets
	      declarations))))
    (t ;; i.e. call next method
     (walk-macro (global-macro-function 'nth-value)
		 `(nth-value ,n ,form) lexical-env dynamic-env
		 eval-targets declarations))))


;;; A special form for performance reasons.
#+not-yet
(defspecial MULTIPLE-VALUE-BIND (vars values-form &rest forms) ()
  (typecase dynamic-env
    (compiler-env
     (multiple-value-bind (decls body) (find-declarations body t)
       (let* ((target (make-multiple-values-target))
	      (targets (add-target target))
	      (body-env (add-variables vars lexical-env dynamic-env decls nil))
	      (exitp (find-if #'exit-target-p eval-targets))
	      (n-specials 0)
	      locals inits)
	 (make-block
	  dynamic-env locals
	  (combine-results
	   dynamic-env
	   (walk values-form lexical-env dynamic-env targets nil)
	   (loop for var in vars
		 and count from 0
		 for binding = (get-binding var body-env)
		 do (unless (ignored-binding-p binding)
		      (if (special-binding-p var)
			  (incf n-specials)
			  (push binding locals))
		      (setq inits
			    (combine-results
			     dynamic-env inits
			     (walk-binding binding `(get-value ,count)
					   lexical-env dynamic-env nil))))
		 finally (return inits)))
	  (walk-sequence body body-env dynamic-env eval-targets
			 (updated-declarations exitp declarations
					       body-env n-specials))))))
    (t					;i.e. call-next-method
     (walk-macro (global-macro-function 'multiple-value-bind)
		 `(multiple-value-bind ,vars ,values-form ,@forms)
		 lexical-env dynamic-env eval-targets declarations))))
      

(defspecial MULTIPLE-VALUE-CALL (function &rest arg-forms) ()
  (let* ((target (make-multiple-values-target))
	 (targets (add-target target))
	 (id (make-tag :name 'mvc :status target))
	 (env (make-values-env :bindings (list id) :env lexical-env)))
    (index-tag id lexical-env)
    (labels ((walk-args (args)
		(when args
		  (combine-many-results
		   dynamic-env
		   (walk (first args) env dynamic-env targets nil)
		   (make-op dynamic-env 'accumulate-values id)
		   (walk-args (rest args))))))
      (make-block
       dynamic-env nil
       (combine-results
	dynamic-env (make-op dynamic-env 'multiple-value-call id)
	(walk-tag function id lexical-env dynamic-env))
       (combine-results
	dynamic-env
	(if arg-forms
	    (walk-args arg-forms)
	    (walk-funcall 'values nil env dynamic-env targets nil))
	(typecase dynamic-env
	  (COMPILER-ENV
	   (setf (getf declarations 'supplies-values) t)
	   (target-special `(multiple-value-funcall ,id)
			   dynamic-env eval-targets declarations))
	  (t (interpreted-call (binding-value id)
			       (getf (binding-declarations id)
				     'accumulation)
			     dynamic-env eval-targets declarations))))))))


;;; Don't get too fancy optimizing for non-multiple-values.
;;; Consider, for example: 
;;; (setq x (multiple-value-prog1 3 (setq x 9) (print x)))
;;;  => x is 3 after printing 9.
;;; We need to store the primary value of form in a place that won't
;;; get clobbered by later assignments.  The current multiple values
;;; buffer is as as good a place as any.

(defspecial MULTIPLE-VALUE-PROG1 (form &body more-forms) ()
  (mvp/protect form more-forms nil lexical-env dynamic-env
	       eval-targets declarations)) 

(defspecial UNWIND-PROTECT (form &body cleanups) ()
  (mvp/protect form cleanups t lexical-env dynamic-env eval-targets
	       declarations))

(defun mvp/protect (form cleanups protectp lexical-env dynamic-env
			 eval-targets declarations)
  (if cleanups
      (let* ((name (if protectp 'exception 'mvp1))
	     (header (if protectp 'unwind-protect 'multiple-value-prog1))
	     (start (if protectp 'cleanup-start 'save-values))
	     (end (if protectp 'cleanup-end 'restore-values))
	     (values-target (retain-values-target eval-targets))
	     (binding (make-values-buffer
		       :name name
		       :declarations '(dynamic-extent t)
		       :status values-target))
	     (env (funcall
		   (if protectp
		       #'make-control-env
		       #'make-values-env)
		   :env lexical-env :bindings (list binding)))
	     (null-target (make-targets)))
	;; Because unwind-protect may cause control flow to shift
	;; around, we must use volatile markings regardless of whether
	;; we have an exit target.
	(index-tag binding lexical-env protectp)
	(make-block
	 dynamic-env nil
	 (make-op dynamic-env header binding)
	 (combine-results
	  dynamic-env
	  ;; For the most part, form-thunk needs to see only lexical-env, not
	  ;; our new-env, but a return-from still needs to know its
	  ;; crossing a control-env.
	  (let ((form-thunk #'(lambda ()
				(walk form env dynamic-env
				      (add-target values-target null-target)
				      declarations)))
		(other-thunk #'(lambda ()
				 (combine-many-results
				  dynamic-env
				  (make-op dynamic-env start binding)
				  (walk-sequence cleanups env dynamic-env
						 null-target nil)
				  (make-op dynamic-env end binding)))))
	    (if protectp
		(make-protected dynamic-env form-thunk other-thunk)
		(combine-results dynamic-env (funcall form-thunk)
				 (funcall other-thunk))))	  
	  (when eval-targets
	    (walk-saved-value binding dynamic-env eval-targets
			      declarations))))) 
    (walk form lexical-env dynamic-env eval-targets declarations)))

  

(defun WALK-SAVED-VALUE (id dynamic-env eval-targets declarations)
  (setf (getf declarations 'supplies-values) t)
  (target-special
   (typecase dynamic-env
     (COMPILER-ENV `(saved-value ,id))
     (t (let ((target (binding-status id)))
	  (when target (first (multiple-values-target-values target))))))
   dynamic-env eval-targets declarations))

(defun INTERPRETED-CATCH (tag forms lexical-env dynamic-env
			      eval-targets declarations)
  (let* ((thrownp t)
	 (values (multiple-value-list
		  (catch tag
		    (multiple-value-prog1
			(walk-sequence forms lexical-env dynamic-env
				       eval-targets declarations)
		      (setq thrownp nil))))))
    (if thrownp
	(interpreted-call #'values values dynamic-env eval-targets
			  declarations t)
	(car values))))

;;; Compile sequence in an environment which defines new block.
;;; If block is used (i.e. nonlocal, marked by find-binding) then use
;;; block/if-returned.  If block is locally used, introduce an block-end
;;; to act as a goto label.
(defspecial BLOCK (label &body forms) ()
  (let* ((original-declarations (copy-list declarations))
	 (binding (make-named-block
		  :name label
		  :eval-targets eval-targets
		  :declarations (append '(dynamic-extent t) declarations)))
	 (env (make-block-env :env lexical-env
			      :bindings (list binding))))
    (typecase dynamic-env
      (COMPILER-ENV
       (let* ((body (walk-sequence
		     forms env dynamic-env eval-targets declarations))
	      (bdeclarations (binding-declarations binding))
	      (localp (getf bdeclarations 'local))
	      (enclosedp (or (getf bdeclarations 'enclosed)
			     (getf bdeclarations 'control)))
	      (nexitp (not (find-if #'exit-target-p eval-targets))))
	 (when (and localp nexitp)
	   (setf body (combine-results
		       dynamic-env body
		       (make-op dynamic-env 'block-end binding))))
	 (when (or localp enclosedp)
	   (index-tag binding lexical-env (and nexitp enclosedp)))
	 (remf declarations 'types)	;Don't check twice.
	 (if enclosedp
	     (make-block
	      dynamic-env nil
	      (make-op dynamic-env 'block binding)
	      (make-result dynamic-env
		`(if (returned ,binding)
		     ,(walk-saved-value
		       binding dynamic-env eval-targets
		       original-declarations) 
		     ,body)))
	     body)))
      (t				;INTERPRETER
       (interpreted-catch binding forms env dynamic-env eval-targets declarations)))))

;;; Either all tags can be indexed (which preserves outside-in sequence)
;;; or tags can be indexed only at end of block when actually used
;;; (which numbers from inside-out).

(defun index-tag (binding lexical-env &optional mark-volatile-p)
  (let ((boundary (find-boundary-env lexical-env mark-volatile-p)))
    (when boundary
      (let ((tags (boundary-env-bindings boundary)))
	(index-binding binding tags boundary nil)
	(setf (boundary-env-bindings boundary)
	      (cons binding tags))))))

;;; Makes C compiler happy in that an non-local exit that happens to
;;; be in tail position will be followed by a return statement, so
;;; it doesn't look like we're falling off the end of a function.
(defun WALK-EXIT (form exit-target lexical-env dynamic-env
		       eval-targets declarations)
  (let ((exit-set (member-if #'function-return-p eval-targets))
	(result (walk form lexical-env dynamic-env
		      (add-target exit-target) declarations)))
    (if exit-set
	(combine-results dynamic-env
			 result
			 (walk-constant nil dynamic-env exit-set nil))
	result)))


(defspecial RETURN-FROM (name &optional result) ()
  (declare (ignore declarations))
  (multiple-value-bind (block block-declarations)
      (control-information name 'block-env lexical-env
			   #'make-captured-block)
    (unless block
      (error 'program-error
	     :format-control "Attempt to RETURN-FROM unknown block ~s."
	     :format-arguments (list name)))
    ;; IWBNI we just unequivocally packaged up the block into a
    ;; block-return and walked the result form -- letting TARGET-VALUE
    ;; take care of the details.  Unfortunately, unlike GO,
    ;; RETURN-FROM produces a value that may involve other targets.
    ;; When dealing with multiple targets, our (C) mechanism requires that
    ;; each assignment, etc., be an expression, so there is no way to
    ;; introduce a there is no way for us to return a sequence
    ;; expression that has a goto.
    (typecase dynamic-env
      (COMPILER-ENV
       ;; If any control forms are crossed, we use the big longjmp hammer.
       ;; The non-control/captured branch is actually now smart enough to
       ;; handle everything but unwind-protect crossings, but we don't
       ;; utilitize this yet.
       (let ((controlp (when (plusp (getf block-declarations 'controls 0))
			 (setf (getf (binding-declarations block) 'control)
			       t))))
	 (if (or controlp (captured-block-p block))
	     (walk-exit result
			(make-block-return :binding block
					   :declarations block-declarations)
			lexical-env dynamic-env eval-targets
			block-declarations) 
	     (let* ((targets (named-block-eval-targets block))
		    (val (walk result lexical-env dynamic-env targets
			       block-declarations))
		    (count (getf block-declarations 'controls 0))
		    (values-id (getf block-declarations 'unwind-values)))
	       (combine-many-results
		dynamic-env
		(unless (zerop count) `((unwind ,count)))
		(when values-id `((restore-values ,values-id)))
		val
		(unless (find-if #'exit-target-p targets)
		  (setf (getf (binding-declarations block)
			      'local) t)
		  (make-op dynamic-env 'local-return block)))))))
      (t				;INTERPETER
       (walk result lexical-env dynamic-env
	     (add-target (make-block-return :binding block))
	     block-declarations)))))

    
(defun WALK-TAG (form binding lexical-env dynamic-env)
  (walk form lexical-env dynamic-env
	(add-target (make-catch-tag-target :binding binding))
	nil))

(defspecial CATCH (tag &body forms) ()
  (let* ((id (make-tag :name 'catch))
	 (env #+was lexical-env
	      ;; Helps us when we are in a tagbody and do a
	      ;; return-from that ends up going out of the function.
	      (make-control-env :bindings (list id) :env lexical-env))
	 (exitp (find-if #'exit-target-p eval-targets)))
    (index-tag id lexical-env (not exitp))
    (make-block
     dynamic-env nil
     (make-op dynamic-env 'catch id)
     (combine-results
      dynamic-env
      (walk-tag tag id env dynamic-env)
      ;; IWBNI this used a single, unified mechanism!
      (typecase dynamic-env
	(COMPILER-ENV
	 (make-result dynamic-env
	   `(if (thrown ,id)
		,(target-special
		  `(returned-value ,id) dynamic-env eval-targets
		  (append '(supplies-values t) declarations) )
		,(combine-results
		  dynamic-env
		  (walk-sequence forms env dynamic-env eval-targets
				 (if exitp
				     (let ((decls (copy-list declarations)))
				       (incf (getf decls 'controls 0))
				       decls)
				     declarations))
		  (unless (find-if #'exit-target-p eval-targets)
		    (make-op dynamic-env 'end-catch id))))))
	 (t				;INTEPRETER
	  (interpreted-catch (binding-value id) forms env dynamic-env
			    eval-targets declarations)))))))

(defspecial THROW (tag result) ()
  (let* ((id (make-tag :name 'throw))
	 (env lexical-env
	      #+not-really-appropriate
	      (make-control-env :bindings (list id) :env lexical-env)))
    (index-tag id lexical-env)
    (make-block
     dynamic-env nil
     (make-op dynamic-env 'throw id)
     (combine-many-results
      dynamic-env
      (walk-tag tag id env dynamic-env)
      (make-op dynamic-env 'find-catcher id)
      (walk-exit result (make-catch-exit :binding id) env dynamic-env
		 eval-targets declarations)))))

(defun tag? (statement) (or (symbolp statement) (integerp statement)))
(defspecial TAGBODY (&body forms) ()
  (let ((counter 0) bindings (compilep (compiler-env-p dynamic-env))
	(tagbody (make-tag :name 'TAGBODY)))
    (do ((forms forms remaining-forms)
	 (remaining-forms (rest forms) (rest remaining-forms)))
	((null forms))
      (let ((form (first forms)))
	(when (tag? form)
	  (let ((tag (make-go-tag :name form
				  :key (incf counter)
				  :status tagbody
				  :declarations
				  (if compilep '(dynamic-extent t)
				      `(code ,remaining-forms)))))
	    (index-tag tag lexical-env)
	    (push tag bindings)))))
    (let ((results nil) (targets (make-targets))
	  (tags (setq bindings (nreverse bindings)))
	  (env (make-tagbody-env :env lexical-env
				 :bindings bindings))
	  (last nil))
      (typecase dynamic-env
	(COMPILER-ENV
	 (dolist (form forms)
	   (setq last form)
	   (setq results (combine-results
			  dynamic-env results
			  (if (tag? form)
			      (make-op dynamic-env 'label (pop tags))
			      (walk form env dynamic-env targets
				    nil)))))
	 ;; Avoids unreachble statements in only the most obvious cases.
	 (when (and eval-targets (not (car-eq last 'go)))
	   (setq results
		 (combine-results
		  dynamic-env results
		  (walk-constant nil dynamic-env eval-targets declarations))))
	 (let ((nonlocals (remove-if
			   #'(lambda (binding)
			       (let ((decs (binding-declarations binding)))
				 (not (or (getf decs 'enclosed)
					  (getf decs 'control)))))
			   bindings)))
	   (if nonlocals
	       (progn
		 ;; Because we can go backwards, we must always
		 ;; consider existing stuff volatile, regardless of
		 ;; whether or not this is an exit target.
		 (index-tag tagbody lexical-env t)
		 (make-tagblock
		  dynamic-env tagbody nonlocals results))
	       results)))
	(t				;INTERPRETER
	 (flet ((eval-code (forms)
		  (catch tagbody
		    (dolist (form forms
				  (walk-constant
				   nil dynamic-env eval-targets declarations))
		      (unless (tag? form)
			(walk form env dynamic-env targets nil))))))
	   (loop
	    (let ((result (eval-code forms)))
	      (if (go-tag-p result)
		  (setq forms (getf (binding-declarations result)
				    'code))
		  (return result))))))))))
	     

(defun make-tagblock (dynamic-env block nonlocals results)
  (make-block
   dynamic-env nil
   (make-op dynamic-env 'tagbody block)
   (combine-many-results
    dynamic-env
    (let (results)
      (dolist (tag nonlocals results)
	(setq results
	      (combine-results dynamic-env results
			       (make-op dynamic-env 'define-label
					tag (go-tag-key tag))))))
    (make-op dynamic-env 'tagbody-start block)
    results
    (make-op dynamic-env 'tagbody-end block))))

(defspecial GO (name) ()
  (multiple-value-bind (b tag-declarations)
      (control-information name 'tagbody-env lexical-env
			   #'make-captured-tag)
    (unless b
      (error 'program-error
	     :format-control "Attempt to GO to unknown tag ~s."
	     :format-arguments (list name)))
    ;; If any control forms are crossed, we use the big longjmp
    ;; hammer.  See RETURN-FROM.
    (when (plusp (getf tag-declarations 'controls 0))
      (setf (getf (binding-declarations b) 'control) t))
    (walk-exit nil (make-go-exit :binding b
				 :declarations tag-declarations)
	       lexical-env dynamic-env eval-targets declarations)))

;;; Handles forms generated by setf expansions.
;;; IWBNI this handled lexical functions, too.

;;; !!! We also need to check for a compiler-macro on (setf <name>)
;;; when processing (funcall (function (setf <name>)) ...).

(defspecial FUNCALL (function &rest args) ()
  (when (and (consp function)
	     (member (first function) '(function quote #+lisp-host cl:function)))
    (let ((designator (second function)))
      (unless (nth-value 1 (function-information designator))
	(return-from funcall
	  (walk-funcall designator args lexical-env dynamic-env
			eval-targets declarations)))))
  (walk-funcall 'funcall
		`(,function ,@args) lexical-env dynamic-env
		eval-targets declarations))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                  INTERPRETER-MACROS                          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#+not-used
(defmacro def-interpreter (name lambda-list &body body)
  (let ((form (gensym "FORM")))
    (multiple-value-bind (decls body) (find-declarations body t)
      `(progn (setf (special-operator ',name)
		    #'(lambda (,form lexical-env dynamic-env eval-targets
				     declarations)
			(destructuring-bind ,lambda-list (rest ,form)
			  (declare ,@decls)
			  (typecase dynamic-env
			    (compiler-env
			     (walk-funcall name (rest ,form)
					   lexical-env dynamic-env
					   eval-targets declarations))
			    (t (block ,name ,@body))))))
	      ',name))))


	  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                  COMPILER-MACROS                             ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#-machine-compile
(progn
(define-compiler-macro cl:DEFUN (&rest args) `(defun ,@args))
(define-compiler-macro cl:DEFMACRO (&rest args) `(defmacro ,@args))
(define-compiler-macro cl:DEFVAR (&rest args) `(defvar ,@args))
(define-compiler-macro cl:DEFSETF (&rest args) `(defsetf ,@args))
(define-compiler-macro cl:DESTRUCTURING-BIND (&rest args)
			       `(destructuring-bind ,@args))
(define-compiler-macro cl:PRINT-UNREADABLE-OBJECT (&rest args)
			       `(print-unreadable-object ,@args))
(define-compiler-macro cl:IN-PACKAGE (name)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setq cl:*package* (pkg ,name))))

(define-compiler-macro cl:WHEN (&rest args) `(when ,@args))
(define-compiler-macro cl:UNLESS (&rest args) `(unless ,@args))
(define-compiler-macro cl:PUSH (&rest args) `(push ,@args))
(define-compiler-macro cl:DOLIST (&rest args) `(dolist ,@args))
(define-compiler-macro cl:DOTIMES (&rest args) `(dotimes ,@args))
(define-compiler-macro cl:CASE (&rest args) `(case ,@args))
(define-compiler-macro cl:ECASE (&rest args) `(ecase ,@args))
(define-compiler-macro cl:TYPECASE (&rest args) `(typecase ,@args))
(define-compiler-macro cl:ETYPECASE (&rest args) `(etypecase ,@args))
(define-compiler-macro cl:LOOP (&rest args) `(loop ,@args))

;;; These are needed for embedded eval-env, so a compiler-macro won't
;;; do it.  Some of these may be special forms in the host, which
;;; louses up our function-information results.  Making them specials
;;; for us fixes that.
(macrolet ((def-host-special (host-name eclipse-name)
	     `(defspecial ,host-name (&rest forms) ()
		(walk `(,',eclipse-name ,@forms)
		      lexical-env dynamic-env eval-targets declarations))))
  (def-host-special cl:the the)
  (def-host-special cl:setf setf)
  (def-host-special cl:funcall funcall)
  (def-host-special cl:find-symbol find-symbol)
  (def-host-special cl:gensym gensym)
  (def-host-special cl:intern intern)
  (def-host-special cl:function function)
  (def-host-special cl:member member)
  (def-host-special cl:pop pop)
  (def-host-special cl:flet flet)
  
  (def-host-special cl:MULTIPLE-VALUE-BIND  MULTIPLE-VALUE-BIND )
  (def-host-special cl:DO do)
  (def-host-special cl:MULTIPLE-VALUE-SETQ MULTIPLE-VALUE-SETQ )
  (def-host-special cl:OR OR)
  ;;(def-host-special cl:DECLARE DECLARE)
  ;;(def-host-special cl:TAGBODY tagbody)
  (def-host-special cl:PROG1 PROG1)
  (def-host-special cl:PROG2 PROG2)
  (def-host-special cl:PROG* PROG*)
  (def-host-special cl:PROG PROG)
  (def-host-special cl:PSETQ PSETQ)
  (def-host-special cl:DOLIST DOLIST)
  (def-host-special cl:COND COND)
  (def-host-special cl:DOTIMES DOTIMES)
  (def-host-special cl:RETURN RETURN)
  (def-host-special cl:AND AND)
  (def-host-special cl:MULTIPLE-VALUE-LIST MULTIPLE-VALUE-LIST)
  (def-host-special cl:DO* do*)
  #+cmu
  (progn
    (def-host-special cl::BACKQ-LIST* list*)
    (def-host-special cl::BACKQ-LIST list)
    (def-host-special cl::BACKQ-CONS cons)
    (def-host-special cl::BACKQ-NCONC nconc)
    (def-host-special cl::BACKQ-APPEND append)
    (def-host-special cl::BACKQ-QUOTE quote)))
  
  

(macrolet ((def-host-noop (host-name)
	     `(defspecial ,host-name (&rest forms) ()
		(declare (ignore lexical-env))
		forms
		(walk-constant nil dynamic-env eval-targets declarations))))
  (def-host-noop cl:declaim)
  (def-host-noop eclipse::host-declaim))


#|
;;; These are generated for bootstrap purposes by some Eclipse macros.
(define-compiler-macro DEFTYPE (&rest ignore)
  (declare (ignore ignore)))
#+cmu ;Appears in bootstrap expansion of in-package.
(define-compiler-macro kernel:%in-package (name)
  (declare (ignore name)))
(define-compiler-macro FUNCALL (&rest args)
  `(funcall ,@args))

;; Not a compiler-macro because we need it for EVAL-ENV.
(defspecial FUNCTION (name) ()
  (walk `(function ,name) lexical-env dynamic-env eval-targets
	declarations))
|#
)
