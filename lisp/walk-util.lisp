;;; Major Sections:
;;; Introduction to Walker
;;;   Lexical Environments
;;;   Dynamic Environments
;;;     Notifications
;;;   Targetting
;;;   Type checking and Declarations
;;;   Primitive Results
;;; Primitive Values
;;; Primitive Processing
;;; Operations on Forms
;;;   Macroexpansions
;;; Operations on Variables
;;; Operations on Bindings
;;; Argument/Init Processing
;;; Functions
;;; Special Forms
;;; Top Level User Interface
;;; Compiler-macros

#|
To Do:
- Need a thorough analysis of side-effects on environment and target structures.
- C Function declarations before use.
- Retain macro file declarations for use in include statement.
- There are a number of places noted as:
   ;Should reference OPTIMIZE from declarations!!!
  We need to see how to most efficiently pass optimization data around.
- WITH-COMPILATION-UNIT!!!  Need deferrable-warning function.
  Undefined type and undefined function warnings need to be deferred.
  Block compilation should be provided for, which passes around
  certain data from dynamic-env within a unit (for example:
  function-warnings, known-functions, compiler environment
  declarations, compiler environment source code)
|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                    INTRODUCTION TO WALKER                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
The last four arguments to walker functions are:
  DYNAMIC-ENV: A particular compiler-env, file-compiler-env, etc., or
nil, indicating the Common Lisp interpreter.  This stores information
about and for the processing environment.  It is usually side-effected
with definitions, external references, etc.
  LEXICAL-ENV: A representation suitable for &environment arguments.
This stores information about the lexical context.
  EVAL-TARGETS: An indication of what will be done with the value
being processed (eg. stored in a binding, returned from a function,
etc.)
  DECLARATIONS: A plist of accumulated declarations representing what
is known so far about the form being processed.  A new plist is
created for each top level form and added to for each subform.  Be
careful with side-effects!

It should be possible to wrap all this information into a single
environment representation so that only one argument need be passed
around.  This would also allow compiler-macros to make use of
targeting information and accumulated declarations.

We separate them into four parameters for several reasons:

1. Many operations need to dispatch based on the class of the
dynamic-env.  Making this a separate argument saves us from having to
either search through the lexical-env or duplicating the lexical-env
hierarchy into parallel hierarchies for different dynamic-env's.  (It
might be nice, however, to dynamically create parallel class
hierarchies as needed, as is done in CLIM.)

2. Eval-targets are lexical until one reaches a throw, at which point
they're dynamic.  For now, it seems cleaner to separate this
information rather than trying to keep it in the lexical-env.  In the
future, we might also want to change the representation of eval-target
sets so that they can participate in dispatching.

3. Some operations (such as exits) need to use declarations from
earlier points in the computation at the expense of, or in addition
to, the current lexical context.  However, the current lexical
environment might also be used in such cases.  Separating declarations
from the lexical-env facilitates this.

4. Separating declarations from the lexical-env makes the
implementation of THE forms easier.

5. Some declarations need to be repeatedly examined: gathering these
into a declarations plist is more efficient than repeatedly searching
through the lexical-env and reconstructing them.

There should be special lexical environment created to represent the
environment in which the top-level forms, the load-time-values, and
the load-forms will be "evalated" in.  This would be stored in the
dynamic-env, and side-effected as we go along.  It would be passed
to MAKE-LOAD-FORM, the walker used by MAKE-LOAD-FORM and
WALK-LITERAL.  Perhaps it should be used as the initial lexical-env
for all forms, too, so that, for example, macroexpansion of
top-level forms would see this environment.

We also cheat and store some clearly dynamic things in the
lexical-environment: see uses of make-control-env.

A RESULT is not really a data type, but it is the result of walking
a form.  For an interpreter, this is the returned value.  For
a compiler, it is the instruction sequence.
|#  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                  LEXICAL ENVIRONMENTS                        ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; See env.lisp

(defstruct (top-level-env (:include env)))

(defun TOP-LEVEL-P (env)
  (or (null env)
      (top-level-env-p env)
      (boundary-env-p env)
      (and (or (macro-variable-env-p env)
	       (macro-function-env-p env)
	       (declaration-env-p env))
	   (top-level-p (frame-env env)))))

(defun ADD-VARIABLE (name lexical-env dynamic-env &optional declarations)
  (augment-environment lexical-env :variable (list name)
		       :declare declarations
		       :index-p (file-compiler-env-p dynamic-env)))

(defun ADD-VARIABLES (names lexical-env dynamic-env
			    &optional declarations bound-declarations-p)
  (augment-environment lexical-env :variable names
		       :bound-declarations-p bound-declarations-p
		       :declare declarations
		       :index-p (file-compiler-env-p dynamic-env)))

;;; The mark-volatile-p flag tells us to mark all existing variables
;;; found in the current closure as volatile.  This is done on the
;;; lexical environment in which UNWIND-PROTECT, CATCH, and closed
;;; over BLOCK and TAGBODY are processed.  It is safe, but too big a
;;; hammer to declare all such marked variables as volatile in C.
;;; IWBNI we only marked variables which are WRITTEN INSIDE the
;;; processed body, and then READ AFTER the processed body.

(defun FIND-BOUNDARY-ENV (lexical-env &optional mark-volatile-p)
  (doframes (frame lexical-env 'boundary-env nil)
	    (when (and mark-volatile-p (variable-env-p frame))
	      (dolist (binding (frame-bindings frame))
		(setf (getf (binding-declarations binding) 'volatile) t)))
    (return frame)))

;;; Uses of this can be replaced by FIRST or POP of a list of
;;; bindings, but that depends on knowledge of environments, which
;;; could change.
(defun GET-BINDING (name frame)
  (or (car (member name (frame-bindings frame) :key #'binding-name :test #'eq))
      (get-binding name (frame-env frame))))

(defun ignored-binding-p (binding)
  (setf (binding-declarations binding) 'ignore))

;;; Perhaps dirty should be relative to another binding.
;;; Consider: (let ((tmp x)) (setq x (cdr tmp)))
;;; Here we could really eliminate tmp even thought its value is x and
;;; x is dirty.  
(defun DIRTY-BINDING-P (binding)
  (let ((decs (binding-declarations binding)))
    (or (getf decs 'dirty)
	(getf decs 'enclosed))))


(defun VARIABLE-VALUE (binding variable)
  (if (symbolp binding)
      (symbol-value variable)
      (binding-value binding)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                  DYNAMIC ENVIRONMENTS                        ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defstruct COMPILER-ENV
  (functions nil)
  (inits nil)
  (function-warnings (make-hash-table :size 27 :test #'equal)))

(defun add-init-code (dynamic-env results &optional (read-only-p t))
  (declare (ignore read-only-p))
  (setf (compiler-env-inits dynamic-env)
	(combine-results dynamic-env
			 (compiler-env-inits dynamic-env)
			 results)))

(defstruct STEPPER-ENV
  (level 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                      NOTIFICATIONS                           ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-condition SIMPLE-STYLE-WARNING (style-warning simple-condition)
  ())

(defun WARNED-P (key scope dynamic-env)
  (when key
    (let ((table (and dynamic-env
		      (ecase scope
			(function (compiler-env-function-warnings
				   dynamic-env))))))
      (if table
	  (or (gethash key table)
	      (progn (setf (gethash key table) t)
		     nil))
	t))))

(defun COMPILER-WARN (dynamic-env key scope format &rest args)
  (unless (warned-p key scope dynamic-env)
    (let ((*print-lines* 50)
	  (*print-length* 10)
	  (*print-level* 4)
	  (*print-circle* t))
      (apply #'warn format args))))

(defun COMPILER-STYLE-WARN (dynamic-env key scope format &rest args)
  (unless (warned-p key scope dynamic-env)
    (let ((*print-lines* 50)
	  (*print-length* 50)
	  (*print-level* 15)
	  (*print-circle* t))
      (warn 'simple-style-warning :format-control format :format-arguments args))))

(defun warn-used (dynamic-env name class)
  (compiler-style-warn dynamic-env name 'function
		       #"~a ~s is used but it was declared ignored."
		       class name))
(defun warn-unused (dynamic-env name class)
  (compiler-style-warn dynamic-env name 'function
		       #"~a ~s is not used."
		       class name))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                      TARGETTING                              ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defstruct (TARGET
	    (:print-function
	     (lambda (b stream level)
	       (declare (ignore level))
	       (print-unreadable-object (b stream :type t)
		 (write (target-binding b) :stream stream)))))
  binding declarations)

(defstruct (LEXICAL-TARGET (:include target)))
(defstruct (CATCH-TAG-TARGET (:include lexical-target)))
(defstruct (GLOBAL-TARGET (:include target)))
(defstruct (CONSTANT-TARGET (:include target)))

(defstruct (MULTIPLE-VALUES-TARGET (:include target))
  (values 'all))
(defstruct (EXIT-TARGET (:include multiple-values-target)))
(defstruct (FUNCTION-RETURN (:include exit-target)))
(defstruct (BLOCK-RETURN (:include exit-target)))
(defstruct (CATCH-EXIT (:include exit-target)))

(defstruct (GO-EXIT (:include target)))

(defun RETAIN-VALUES-TARGET (targets)
  (let ((mv (find-if #'multiple-values-target-p targets)))
    (or (unless (exit-target-p mv) mv)
	(when targets
	  (make-multiple-values-target)))))

(defun RETAIN-BLOCK-TARGET (binding targets)
  (if (find-if #'block-return-p targets)
      targets
      (nconc (remove-if #'function-return-p targets)
	     (list (make-block-return :binding binding)))))


;;; Currently, target sets are just an ordered lists of targets.
;;; IWBNI we arranged them so that multiple assignment of convertable
;;; data performed only one conversion.
(defun MAKE-TARGETS () nil)
(defun ADD-TARGET (target &optional targets)
  (if target (cons target targets) targets))

(defun TARGET (thing dynamic-env eval-targets declarations)
  (let* ((type-set (getf declarations 'types))
	 (types (and type-set (type-set-types type-set))))
    (labels ((walk-targets1 (thing eval-targets)
	       (if eval-targets
		   (walk-targets1
		    (target-value dynamic-env (first eval-targets)
				  thing declarations)
		    (rest eval-targets))
		 thing)))
      (walk-targets1
       (if types
	   (run-time-type-check dynamic-env thing types)
	 thing)
       eval-targets))))

(defun TARGET-SPECIAL (form dynamic-env eval-targets declarations)
  (make-result dynamic-env
    (target form dynamic-env eval-targets declarations)))


(defun FILL-MULTIPLE-VALUES (values declarations target)
  (let ((value (first values)))
    (case (multiple-values-target-values target)
      (all (setf (multiple-values-target-values target)
		 (if (getf declarations 'single-value)
		     (list value)
		     values))))
    value))

(defun PROPER-VALUE (value declarations target)
  (declare (ignore target))
  (if (and (getf declarations 'supplies-values)
	   (not (getf declarations 'single-value)))
      value
    `(values1 ,value)))

(defun PROPER-UNWIND (value declarations)
  (let ((count (getf declarations 'controls 0))
	(values-id (getf declarations 'unwind-values)))
    (if (and (zerop count) (null values-id))
	value
	`(,(if (statement-p value)
	       'compound 'sequence)
	  ,@(unless (zerop count) `((unwind ,count)))
	  ,@(when values-id `((restore-values ,values-id)))
	  ,value))))

(defun set-variable-value (var val)
  (when (captured-var-p var)
    (set-variable-value (captured-var-binding var) val))
  (setf (binding-value var) val))

;;; This kludge is so that we can find out the name of the current
;;; (gensymed) parse-lambda helper binding, which is used in a special
;;; way and need not ever be considered dirty.  This helps eliminate a
;;; temporary variable during argument checking.
;;(defparameter *varargs-binding* nil)

(defun TARGET-VALUE (dynamic-env target thing declarations)
  (typecase dynamic-env
    (COMPILER-ENV
     (typecase target
       (LEXICAL-TARGET
	;; This is where we will eventually store partially evaluated
	;; values.
	(let ((binding (target-binding target)))
	  (unless (eq (binding-value binding) 'unbound-flag)
	    (setf (getf (binding-declarations binding) 'dirty) t))
	  ;; IWBNI we had a more general mechanism for indicating that
	  ;; a function does not return!  
	  (unless (and (consp thing)
		       (member (car thing)
			       '(missing-args error)))
	    (set-variable-value binding thing)))
	`(setq ,(target-binding target) ,thing))
       (GLOBAL-TARGET
	`(set-symbol-value-value ,(intern-constant dynamic-env
						   (target-binding target))
				 ,thing))
       (CONSTANT-TARGET
	`(initialize ,(target-binding target) ,thing))
       (FUNCTION-RETURN
	(let ((count (getf declarations 'controls 0))
	      (val (proper-value thing declarations target)))
	  (if (zerop count)
	      `(ec:return ,val)
	      `(unwind-return ,val ,count))))
       (BLOCK-RETURN
	`(return-from ,(block-return-binding target)
	   ,(proper-value thing declarations target)))
       (CATCH-EXIT
	`(exit-throw ,(catch-exit-binding target)
		     ,(proper-value thing declarations target)))
       (MULTIPLE-VALUES-TARGET
	(proper-value thing declarations target))
       (GO-EXIT
	(let ((b (target-binding target)))
	  (cond ((captured-tag-p b)
		 `(go ,b ,(go-tag-key (captured-tag-binding b))))
		((getf (binding-declarations b) 'control)
		 `(go ,(binding-status b) ,(go-tag-key b)))
		(t (proper-unwind
		    `(goto ,b) (target-declarations target))))))))
    (t					;INTERPETER
     (typecase target
       (LEXICAL-TARGET
	(set-variable-value (target-binding target) thing))
       (GLOBAL-TARGET
	(set (target-binding target) thing))
       (BLOCK-RETURN
	(fill-multiple-values (list thing) declarations target)
	(throw (block-return-binding target)
	  (values-list (multiple-values-target-values target)))) 
       (CATCH-EXIT
	(fill-multiple-values (list thing) declarations target)
	(throw (binding-value (catch-exit-binding target))
	  (values-list (multiple-values-target-values target))))
       (MULTIPLE-VALUES-TARGET
	(fill-multiple-values (list thing) declarations target))
       (GO-EXIT
	(let ((b (target-binding target)))
	  (throw (binding-status b) b)))))))

(defmethod INTERN-CONSTANT ((dynamic-env t) obj) obj)

;;; References to some symbols appear within macros in eclipse.h and
;;; these won't ever be seen by the compiler -- therefore eclipse.h
;;; must declare them as extern's.   We don't want to redeclare them.
;;; As long as we have this mechanism, we might as well do this for a
;;; bunch of common objects.

(defvar null-hook-flag (intern "NULL-HOOK" :eclipse))
(defvar eoa-flag (intern "EOA" :eclipse))
(defparameter *common-interned-objects*
  `(nil t *package* *readtable*
	*load-pathname* *load-truename*
	SETF-EXPANDER
    ,eoa-flag
    ,null-hook-flag
    ,(intern "UNBOUND" :eclipse)
    #|,(find-package :eclipse)
    ,(find-package :common-lisp-user)
    ,(find-package :keyword)|#))
(defvar not-yet-interned (make-symbol "NOT-YET-INTERNED"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;              TYPE CHECKING AND DECLARATIONS                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
Claims as to the type of a form being walked are built up as the form
is being walked.  For example, a THE special form adds a type claim,
and the (possibly free) declarations for a variable add type claims
for forms assigned to these variables.  These claims are stored in a
type-set in the DECLARATIONS argument which is passed to successively
deeper WALKER methods.

When a new type is added to a type-set object, it is compared with the
existing types: 
 - If it is a supertype of an existing type, then it is not added.
 - If it is a subtype of an existing type, then the existing type is
   replaced with the new type.
 - Otherwise, it is added to the type-set.  In addition, if it is
   disjoint from some existing type, a warning is issued.

Ultimately, there are are three situations in which these claims can
be resolved:
- LITERALS: The compiler can test the claims against the actual value. 
- READING A VARIABLE: The compiler can test the claims against the
  CURRENT-TYPES for the variable, if known.
- CALLING A FUNCTION: A compiler may choose to trust available type
  declarations for the function, which may be used to test the claims.
Any remaining unsatisfied claims can be tested at run-time.

The targetting mechanism in each of these three cases can store the
entire set of claims as the CURRENT-TYPES of each variable in the
target-set.  (Actually, for literals, it is better to just use the
precise-type of the literal value.)  

Note: this mechanism does not alter the TYPES of a binding based on what
ends up stored there.  However, for compilers, it DOES alter the
CURRENT-TYPES.  This might be useful to allow temporary bindings to
become c:int, c:float, etc.

|#
;;; nil - New-type is definately a subtype of existing type.
;;; :disjoint - Definately disjoint
;;; :super - New-type is a supertype of existing-type, might be ok.
;;; :unknown - Could not determine relationship.
(defun COMPARE-TYPES (new-type existing-type)
  (multiple-value-bind (super sure)
      (subtypep existing-type new-type)
    (if super
	:super
      (multiple-value-bind (sub sure2)
	  (subtypep new-type existing-type)
	(cond (sub :sub)
	      ((and sure sure2) :disjoint)
	      (t :unknown))))))
      
(defstruct (TYPE-SET (:copier nil)) types)
(defun COPY-TYPE-SET (type-set)
  (make-type-set :types (copy-list (type-set-types type-set))))

;;; ADD-TYPE(type-spec type-set form dynamic-env) => type-set or nil
;;; MODIFIES: type-set
;;; SIGNALS: If type-spec is disjoint from an existing type in
;;; type-set, a warning is printed using form and dynamic-env. 
;;; EFFECTS: Adds type-spec to type-set unless it is a supertype of a
;;; type already included.  Returns type-set if type-set was modified,
;;; otherwise nil. 
(defun ADD-TYPE (new-type type-set form dynamic-env)
  (do ((types (type-set-types type-set) (cdr types))
       (tail nil types))
      ((null types)
       (if tail
	   (setf (cdr tail) (list new-type))
	 (setf (type-set-types type-set) (list new-type)))
       type-set)
    (let* ((existing-type (car types))
	   (relation (compare-types new-type existing-type)))
      (case relation
	(:super (return nil))
	(:sub (setf (car types) new-type)
	       (return type-set))
	(t (when (eq relation :disjoint)
	     (compiler-warn
	      dynamic-env (list form new-type existing-type) 'function
	      "~s cannot be both ~s and ~s."
	      form new-type existing-type))))))) 

;;; ADD-TYPE-TO-DECLARATIONS(type-spec declarations form dynamic-env)
;;;    => declarations
;;; MODIFIES: declarations
;;; SIGNALS: If type-spec is disjoint from an existing type in
;;; declarations, a warning is printed using form and dynamic-env. 
;;; EFFECTS: Adds type-spec to the type-set in declarations, unless it
;;; is a supertype of a type already included.  (A new type-set is
;;; created if declarations does not yet include a type-set).  Returns
;;; the new, modified declarations. 
(defun ADD-TYPE-TO-DECLARATIONS (new-type declarations form
					  dynamic-env)
  (let ((type-set (getf declarations 'types)))
    (cond (type-set
	   (add-type new-type type-set form dynamic-env))
	  ((or (eq new-type 't)
	       (subtypep 't new-type)) )
	  (t (setq declarations
		   `(types ,(make-type-set :types (list new-type))
			   ,@declarations))))
    declarations))

;;; COMBINE-DECLARATIONS(new-declarations old-declarations
;;;                      form dynamic-env) => declarations
;;; MODIFIES: old-declarations
;;; EFFECTS: Combines items from new-declarations with
;;; old-declarations (which may be nil), and returns the combined
;;; declarations.  Type declarations are added using
;;; ADD-TYPE-TO-DECLARATIONS and the form and dynamic-env arguments.
;;; Other declarations are included directly.  
(defun COMBINE-DECLARATIONS (new-declarations
			     old-declarations
			     form dynamic-env)
  (loop for (key value) on new-declarations by #'cddr
	do (setq old-declarations
		 (case key
		   (type (add-type-to-declarations
			  value old-declarations form dynamic-env))
		   (t `(,key ,value ,@old-declarations)))))
  old-declarations)


;;; Returns a new (side-effected) declarations list by removing
;;; unconfirmed types in the typeset for which (funcall test type
;;; unconfirmed-type) is true. 
(defun CONFIRM-UNCONFIRMED-TYPES (type test declarations)
  (let ((unconfirmed-types (getf declarations 'types)))
    (when unconfirmed-types
      (setf (type-set-types unconfirmed-types)
	    (delete type (type-set-types unconfirmed-types)
		    :test test))))
  declarations)

(defun check-typefs (value list-of-types)
  (loop for type in list-of-types
	unless (typep value type)
	do (error 'type-error :datum value :expected-type type))
  value)
	

(defun RUN-TIME-TYPE-CHECK (dynamic-env value types)
  (typecase dynamic-env
    (COMPILER-ENV
     #+run-time-type-checking
     (compiler-style-warn dynamic-env nil 'function
			  "Could not confirm that ~s was of type ~s."
			  value (if (cdr types) `(and ,@types) (car
								types)))
     ;;Should reference OPTIMIZE from declarations!!!
     value #+run-time-type-checking
     (cond ((some #'(lambda (x) (implementation-primitive-p dynamic-env x))
		  types)			;Is this necessary?
	    value)
	   ((rest types)
	    `(check-typefs ,value ,(intern-constant dynamic-env
						    types)))
	   (t `(check-typef ,value ,(intern-constant dynamic-env
						     (first types))))))
    (t					;INTERPRETER
     (check-typefs value types))))

(defun KNOWN-TYPE (dynamic-env type declarations)
  (when type
    (typecase dynamic-env
      (compiler-env 
       (confirm-unconfirmed-types type #'subtypep declarations))))
  declarations)

;; IWBNI we actually did something with the others here.
(defun RETURN-TYPE (typespec)
  (if (car-eq typespec 'values)
      (if (eq (second typespec) '&rest)
	  (third typespec)
	  (second typespec))
      typespec))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                     PRIMITIVE RESULTS                        ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MAKE-RESULT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun MAKE-RESULT (dynamic-env value)
  (typecase dynamic-env
    (COMPILER-ENV (and value (list value)))
    (t value)))

(defun COMBINE-RESULTS (dynamic-env result1 result2)
  (typecase dynamic-env
    (COMPILER-ENV (nconc result1 result2))
    (t result2)))

(defun COMBINE-MANY-RESULTS (dynamic-env &rest results)
  (let ((res (pop results)))
    (dolist (r results res)
      (setq res (combine-results dynamic-env res r)))))

(defun SINGLETON-RESULT-P (result)
  (and result (null (cdr result))))

(defun statement-p (subresult)
  (and (consp subresult)
       (member (car subresult)
	       '(if compound
		    begin-parse ec:return unwind-return
		    local-return return-from
		    multiple-value-call multiple-value-prog1
		    progv-start unwind-protect 
		    block block-end tagbody ec::go goto throw))))

(defun STATEMENTS-P (result) (some #'statement-p result))

(defun ASSIGNMENT-P (binding result)
  (and (singleton-result-p result)
       (let ((statement (first result)))
	 (and (car-eq statement 'setq)
	      (or (null binding) (eql binding (second statement)))))))

;;; Requires: (assignment-p x result) => true
(defun ASSIGNMENT-VALUE (result)
  (third (car result)))

(defparameter *bound-specials* nil)
(defun make-op1 (dynamic-env code args)
  (make-result dynamic-env
    (typecase dynamic-env
      (compiler-env (cons code args))
      (t (case code
	   (dbind (let ((variable-name (first args)))
		    (push (cons variable-name
				(if (boundp variable-name)
				    (symbol-value variable-name)
				    'unbound-flag))
			  *bound-specials*)))
	   (progv-end (make-unwind dynamic-env 1))
	   (progv-start
	    (destructuring-bind (symbol-binding value-binding)
		(rest args)
	      (do ((n 0 (1+ n))
		   (symbols (binding-value symbol-binding) (cdr symbols))
		   (values (binding-value value-binding) (cdr values)))
		  ((endp symbols) (push n *bound-specials*))
		(let ((symbol (car symbols)))
		  (make-op dynamic-env 'dbind symbol)
		  (if values
		      (set symbol (car values))
		      (makunbound symbol))))))
	   (accumulate-values
	    (let* ((binding (first args))
		   (target (binding-status binding))
		   (old (getf (binding-declarations binding)
			      'accumulation))
		   (new (multiple-values-target-values target)))
	      (setf (getf (binding-declarations binding) 'accumulation)
		    (nconc old new))
	      (setf (multiple-values-target-values target) 'all)))
	   (t nil))))))
(defun MAKE-OP (dynamic-env code &rest args)
  (make-op1 dynamic-env code args))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                     PRIMITIVE PROCESSING                     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod maybe-add-external-variable ((dynamic-env t) (variable t)))
  
(defun READ-VALUE (dynamic-env binding variable)
  (when (and (binding-p binding) (special-binding-p binding))
    (setq binding (binding-name binding)))
  (typecase dynamic-env
    (COMPILER-ENV
     (cond ((binding-p binding) binding)
	   ((c-symbol-p variable) variable)
	   ((global-declaration variable 'global-variable 'c-external)
	    (maybe-add-external-variable dynamic-env variable)
	    variable)
	   (t
	    `(symbol-value ,(intern-constant dynamic-env variable) ,eoa-flag))))
    (t					;INTERPRETER
     (variable-value binding variable))))

(defun PROCESSING-ERROR (form condition)
  (declare (ignore form))
  (error condition))
   
(defun MAKE-ERROR (dynamic-env form condition)
  (typecase dynamic-env
    (COMPILER-ENV
     (compiler-warn dynamic-env nil nil
		    "Error compiling ~_~s:~%~a"
		    form condition)
     (walk1 `(processing-error
	      ',form 
	      ,(let ((cond (gensym "CONDITION")))
		 (multiple-value-bind (create init)
		     (make-load-form-saving-slots condition)
		   `(let ((,cond ,create))
		      ,(subst cond condition init)
		      ,cond))))
	    nil dynamic-env nil nil))
    (t					;Intepreter
     (make-result dynamic-env
		  (processing-error form condition)))))

(defun MAKE-FDEFINITION (dynamic-env function-name)
  (typecase dynamic-env
    (COMPILER-ENV
     (let ((name (intern-constant dynamic-env function-name)))
       (if (and (symbolp function-name)
		(let ((package (symbol-package function-name)))
		  (or (eq package #.(find-package :eclipse))
		      #+lisp-host (eq package #.(find-package :cl)))))
	   `(symbol-function-value ,name)
	   `(fdefinition ,name ,eoa-flag))))
    (t (fdefinition function-name))))

(defun MAKE-UNWIND (dynamic-env n-specials)
  (typecase dynamic-env
    (COMPILER-ENV
     (unless (zerop n-specials)
       (make-result dynamic-env `(unwind ,n-specials))))
    (t					;INTERPRETER
     (dotimes (ignore n-specials)
       (declare (ignorable ignore))
       (when *bound-specials*		;null stack should be error!!!
	 (let ((entry (pop *bound-specials*)))
	   (etypecase entry
	     ;; destructuring-bind of dotted list broken in CMUCL!!!
	     ;; In Eclipse, destructuring-bind would produce NIL for unbound entry.
	     (cons (let ((var (car entry)) (val (cdr entry)))
		     (if (eq val 'unbound-flag)
			 (makunbound var)
			 (set var val))))
	     (fixnum (make-unwind dynamic-env entry)))))))))

(defun MAKE-PROTECTED (dynamic-env thunk cleanup-thunk)
  (typecase dynamic-env
    (compiler-env (combine-results dynamic-env (funcall thunk)
				   (funcall cleanup-thunk)))
    (t (unwind-protect (funcall thunk)
	 (funcall cleanup-thunk)))))
      

;;; MAKE-BLOCK ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun implementation-type (dynamic-env lisp-type)
  (if (implementation-primitive-p dynamic-env lisp-type)
      lisp-type
      'object))

(defun binding-implementation-type (dynamic-env binding)
  (implementation-type dynamic-env
		       (getf (binding-declarations binding) 'type)))

(defun make-declare (dynamic-env bindings)
  (typecase dynamic-env
    (COMPILER-ENV
     (let (types vtypes result)
       (dolist (binding bindings)
	 (let ((type (binding-implementation-type dynamic-env binding)))
	   (if (getf (binding-declarations binding) 'volatile)
	       (push binding (getf vtypes type))
	       (push binding (getf types type)))))
       (flet ((make-decl (volatilep type bindings)
		(setq result (combine-results
			      dynamic-env result
			      ;; Make sure we don't exceed
			      ;; call-arguments-limit, 
			      (make-op1 dynamic-env 'declare
					(cons volatilep
					      (cons type
						    bindings)))))))
	 (loop for (type bindings) on types by #'cddr
	       do (make-decl nil type bindings))
	 (loop for (type bindings) on vtypes by #'cddr
	       do (make-decl t type bindings))
	 result)))))

;;; ELIMINABLE-P and MAKE-BLOCK incorporate temporary variable
;;; elimination.  They are conservative in that they do not eliminate
;;; variables if the variable itself or the variable that it refers to
;;; have EVER been dirty at this point in the compilation.  For
;;; example, a temporary that refers to another variable could really
;;; be eliminated if it was made dirty outside the variable being
;;; considered for elimination.  We don't eliminate it.

(defvar not-eliminable (make-symbol "NOT-ELIMINABLE"))
(defun eliminable-p (binding)
  (if (or (dirty-binding-p binding)
	  (eq (binding-value binding) 'unbound-flag))
      not-eliminable
      (let* ((name (binding-name binding))
	     (val (if (or (eq name 'ec::ap)
			  (null (symbol-package name)))
		      (binding-value binding)
		      '((compound not-eliminable)))))
	(typecase val
	  (binding (if (dirty-binding-p val)
		       not-eliminable
		       val))
	  (constant-id val)
	  (symbol val)
	  (ec:void val)
	  (cons (if (or (statements-p val)
			(not (getf (binding-declarations binding)
				   'call-eliminable nil))
			(not (eliminable-form-p val)))
		    not-eliminable
		    val))))))

(defun eliminable-form-p (form)
  (cond ((consp form) (every #'eliminable-form-p form))
	((binding-p form) (or (not (dirty-binding-p form))
			      #+no-longer-needed
			      (eq (binding-name form) *varargs-binding*)))
	(t t)))

(defun defined-in-p (var form)
  (labels ((test (x) (defined-in-p1 var x))
	   (defined-in-p1 (var form)
	     (when (consp form)
	       (case (first form)
		 (declare (member var (cddr form) :test #'eq))
		 (compound (some #'test (cdr form)))
		 (if (or (some #'test (third form))
			 (some #'test (fourth form))))))))
    (defined-in-p1 var form)))

(defun form-contains-dependent (form result)
  (cond ((consp form)
	 (loop for sub in form
	       thereis (form-contains-dependent sub result)))
	((binding-p form)
	 (loop for instruction in result
	       thereis (defined-in-p form instruction)))))

(defun safely-eliminable-p (binding result)
  (let ((substitution (eliminable-p binding)))
    (cond ((eq substitution not-eliminable)
	   substitution)
	  ((form-contains-dependent substitution result)
	   ;; IWBNI rather than giving up on the elimination, we
	   ;; shuffled the result so that the binding reference was
	   ;; within the scope of variable(s) on which substitution
	   ;; dependend. 
	   not-eliminable)
	  (t substitution))))
	
	  
;;; IWBNI this did not cons up new vars!
;;; When a variable is eliminable, we do three things:
;;; 1. Substutute in its replacement value in the result.
;;; 2. Delete expressions of the form (setq <eliminated-var> ...) from
;;;    the result.  Since the variable will not appear, and references
;;;    will have been replaced by its value, there's not point in
;;;    initializing the variable.
;;; 3. Substitute in its replacement value in FORMS that are the value
;;;    of other variables.  This only occurs (I think) when a function
;;;    call assigned to a temporary gets eliminated within another
;;;    function call.  Consider: (setq z (foo (bar x))) =>
;;;      <0.1> <= x
;;;      <0> <= (bar <0.1>)
;;;    <0.1> will be eliminated. Rule 3 is implemented by substituting
;;;    the <x> for <0.1> in the binding-value of <0>, where <0> was
;;;    found because there is a top-level result of the form (setq <0>
;;;    ...).
;;;      <z> <= (foo <0>)
;;;    The current value of <0>, (bar <x>), will be substituted for
;;;    <0> in the value for <z>.  If it weren't for rule 3, the
;;;    current value of <0> would still be (bar <0.1>).
(defun MAKE-BLOCK (dynamic-env vars inits results)
  (typecase dynamic-env
    (COMPILER-ENV
     (make-result dynamic-env
       (let* ((result (combine-results dynamic-env inits results))
	      (vars (loop for var in vars
			  for substitution = (safely-eliminable-p var result)
			  if (eq substitution not-eliminable)
			  collect var
			  else do
			  (setf result (eliminate var substitution result)))))
	 (cond ((and (singleton-result-p result) (null vars))
		(first result))
	       ((or vars (statements-p result))
		`(compound
		  ,@(make-declare dynamic-env vars)
		  ,@result))
	       (t `(sequence ,@result))))))
    (t					;INTERPRETER
     results)))

(defun eliminate (var val forms)
  (cond ((consp forms)
	 (loop for form in forms
	       for new = (eliminate var val form)
	       unless (when (car-eq form 'setq)
			(let ((b (second form)))
			  (cond ((eq b var) t)
				((binding-p b)
				 (setf (binding-value b)
				       (eliminate var val
						  (binding-value b)))
				 nil))))
	       collect new))
	((eq forms var) val)
	(t forms)))
             	

