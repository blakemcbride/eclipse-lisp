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

(defun SPECIAL-BINDING-P (binding)
  (getf (binding-declarations binding) 'special))

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
	  (unless (or (eq (binding-value binding) 'unbound-flag)
		      ;; IWBNI we had a more general mechanism for
		      ;; indicating that a function does not return!
		      (and (consp thing)
			   (member (car thing)
				   '(missing-args error)))
		      #+broken-kludge
		      (eq (binding-name binding) *varargs-binding*))
	    (setf (getf (binding-declarations binding) 'dirty) t))
	  (set-variable-value binding thing))
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
  (if (dirty-binding-p binding)
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
	   ;; within the scope of varible(s) on which substitution
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
             	


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                OPERATIONS ON FORMS                          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun WALK-CONSTANT (form dynamic-env eval-targets declarations)
  (typecase dynamic-env			;before method
    (COMPILER-ENV
     (confirm-unconfirmed-types form #'typep declarations)))
  (make-result dynamic-env
	       (target (intern-constant dynamic-env form)
		       dynamic-env eval-targets declarations)))

(defun WALK1 (form lexical-env dynamic-env eval-targets declarations)	 
  (restart-case
      (typecase form
	(SYMBOL
	 (multiple-value-bind (binding-type binding variable-declarations)
	     (variable-information form lexical-env)
	   (walk-read binding binding-type form
		      lexical-env dynamic-env eval-targets
		      (known-type dynamic-env
				  (getf variable-declarations 'type)
				  (combine-declarations
				   declarations
				   variable-declarations
				   (or binding form) dynamic-env)))))
	(CONS
	 (let ((operator (car form)))
	   (if (or (symbolp operator) (car-eq operator 'lambda))
	       (multiple-value-bind (binding-type binding function-declarations)
		   (function-information operator lexical-env)
		 (walk-combination binding binding-type form lexical-env dynamic-env
				   eval-targets
				   (combine-declarations
				    function-declarations declarations
				    form dynamic-env)))
	       (signal-program-error
		"~s is not recognized in the function position of a form."
		operator))))
	(t				;literal
	 (walk-constant form dynamic-env eval-targets declarations)))
      (continue-processing (condition-or-value &optional interactivep)
	:interactive (lambda ()
		       (write-string "New form to be evaluated: " *debug-io*)
		       (list (walk (read *debug-io*) lexical-env
				    dynamic-env eval-targets
				    declarations)
			     t))
	:report (lambda (s)
		  (format s
			  "~@<Supply a new form to be evaluted instead of ~:_~:w~:>."
			  form))
        (if interactivep condition-or-value
	    (make-error dynamic-env form condition-or-value)))))


  
(defun WALK-SEQUENCE (forms lexical-env dynamic-env eval-targets
			    declarations)
  (labels ((walk-sequence (forms)
	     (if (cdr forms)
		 (combine-results
		  dynamic-env
		  (walk (first forms) lexical-env dynamic-env
			(make-targets) nil)
		  (walk-sequence (rest forms)))
	       (walk (first forms) lexical-env dynamic-env eval-targets
		     declarations))))
    (walk-sequence forms)))

;;; This represents assignment (i.e. not initial binding) to a
;;; "variable" that may, in fact, turn out to be symbol-macro,
;;; constant, etc.
(defun WALK-ASSIGNMENT (variable value lexical-env dynamic-env
				 eval-targets form-declarations)
  (multiple-value-bind (binding-type binding variable-declarations)
      (variable-information variable lexical-env)
    ;; The declarations code makes sure that SETQ expressions return only
    ;; a single value when their result is used in a multiple-values
    ;; collecting form.
    (walk-write (unless (eq binding-type :special) binding)
		binding-type variable value
		lexical-env dynamic-env eval-targets
		(combine-declarations
		 `(single-value t ,@variable-declarations)
		 form-declarations (or binding variable) dynamic-env))))


;;; Special forms are defined with specializations on NAME, which will
;;; take priority over specializations on key.  This means, for
;;; example, that a special form will be handled correctly, even if it
;;; happens to also be defined as a global macro.

(defun WALK-GLOBAL-COMBINATION (name key form
				     lexical-env dynamic-env
				     eval-targets declarations)
  (let ((special (and (symbolp name) (special-operator name))))
    (when special (setq key :special))
    (case key
      (:SPECIAL
       (funcall special form lexical-env dynamic-env
		eval-targets declarations))
      (:MACRO				;global macros
       (walk-macro (global-macro-function name) form lexical-env dynamic-env
		   eval-targets declarations))
      (t;; This default method handles normal function calls.
       (walk-funcall name (rest form) lexical-env dynamic-env eval-targets
		     declarations)))))

	 
(defun WALK-LAMBDA (args lambda-form lexical-env dynamic-env
			 eval-targets declarations)
  (destructuring-bind (lambda-list &rest body) (rest lambda-form)
    (let* ((evalp (listp args))
	   (name (if evalp (make-symbol "ARGS") args))
	   (env (add-variable name lexical-env dynamic-env))
	   (temp (get-binding name env))
	   (varargsp (file-compiler-env-p dynamic-env)))
      (when evalp (setf (binding-value temp) args))
      (multiple-value-bind (bindings checks decls1)
	  (parse-lambda lambda-list name :varargs varargsp)
	#+no-longer-needed (when varargsp (setq *varargs-binding* (caar bindings)))
	(multiple-value-bind (decls2 body doc) (find-declarations body nil)
	  (values
	   (walk-bindings bindings (append decls1 decls2) body
			  (unless evalp temp) checks
			  env dynamic-env eval-targets declarations) 
	   lexical-env doc))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                    MACROEXPANSION                            ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun WALK-MACRO (macro-function form lexical-env dynamic-env
				  eval-targets declarations)
  (walk (expand macro-function form lexical-env)
	lexical-env dynamic-env eval-targets declarations))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                     OPERATIONS ON VARIABLES                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun LITERAL-CONSTANT-P (variable declarations)
  (declare (ignore declarations))
  (or (keywordp variable)
      (eql variable 't) (eql variable 'nil)
      t))		;Should reference OPTIMIZE from declarations!!!

(defun UNKNOWN-VARIABLE (dynamic-env variable)
  (compiler-style-warn dynamic-env variable 'function
		       #"Variable ~s is unknown and assumed special."
		       variable))


(defun READ-VARIABLE (binding variable dynamic-env eval-targets
			      declarations)
  ;; around method for compiler
  (when (and (compiler-env-p dynamic-env) (eql binding :constant)
	     (literal-constant-p variable declarations))
    (return-from read-variable
      (walk-constant (symbol-value variable)
		     dynamic-env eval-targets declarations)))
  ;; before method for compiler
  (when (and (compiler-env-p dynamic-env) (null binding))
    (unknown-variable dynamic-env variable))
  (make-result
   dynamic-env
   (target (read-value dynamic-env binding variable)
	   dynamic-env eval-targets declarations)))

(defun WRITE-VARIABLE (binding variable value-form lexical-env
			       dynamic-env eval-targets declarations)
  ;; before method for compiler
  (when (and (compiler-env-p dynamic-env) (null binding))
    (unknown-variable dynamic-env variable))
  (walk value-form lexical-env dynamic-env
	(add-target
	 (if (not (binding-p binding))
	     (make-global-target :binding variable
				 :declarations declarations)
	   (make-lexical-target :binding binding
				:declarations declarations))
	 eval-targets)
	declarations))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;             OPERATIONS ON BINDINGS                           ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun WALK-READ (binding binding-type variable lexical-env dynamic-env
			  eval-targets declarations)
  (case binding-type
    (:SYMBOL-MACRO
     (walk-macro (get-symbol-macro-function binding variable)
		 variable lexical-env dynamic-env eval-targets
		 declarations))
    (t					;local or global variable
     (read-variable (or binding binding-type) variable 
		    dynamic-env eval-targets declarations))))


(defun WALK-WRITE (binding binding-type variable value-form
			   lexical-env dynamic-env
			   eval-targets declarations)
  (case binding-type
    (:SYMBOL-MACRO
     (walk `(setf ,(expand (get-symbol-macro-function binding variable)
				   variable lexical-env)
			  ,value-form) 
	   lexical-env dynamic-env eval-targets declarations))
    (:CONSTANT
     (error 'program-error
	    :format-control "Attempt to assign ~s to the constant ~s."
	    :format-arguments (list value-form variable)))
    (t (write-variable (or binding binding-type) variable value-form
		     lexical-env dynamic-env eval-targets declarations))))

;;; WALK-COMBINATION is specialized for local MACRO-BINDINGS and
;;; FUNCTION-BINDINGS, as these take precedence over compiler-macros,
;;; special forms, global macros or functions.  If these aren't
;;; applicable, then the default method for global definitions comes
;;; into play.
(defun WALK-COMBINATION (binding binding-type form
				 lexical-env dynamic-env eval-targets
				 declarations)
  (typecase binding
    (MACRO-BINDING
     (walk-macro (binding-value binding) form lexical-env dynamic-env
		 eval-targets declarations))
    (LEXICAL-FUNCTION
     (walk-funcall binding (rest form) lexical-env dynamic-env
		   eval-targets declarations))
    
    (NULL
     ;; Not a local function or local macro.  Compilers have an around
     ;; method to handle compiler-macros.
     (let ((fname (car form)))
       (when (compiler-env-p dynamic-env) ;:around method
	 ;; Handle compiler macros.  This takes priority over
	 ;; special-forms, global macros, and funcalls, but NOT over
	 ;; local functions and local macros.
	 (let* ((expander (and (not (car-eq fname 'lambda))
			       (not (eq (getf declarations 'inline)
					'notinline))
			       (compiler-macro-function fname)))
		(expansion (and expander
				(expand expander form lexical-env))))
	   (if (or (null expander) (eql expansion form))
	       nil			;(call-next-method)
	       (return-from walk-combination
		 (walk expansion lexical-env dynamic-env eval-targets
		       declarations)))))
       (walk-global-combination fname binding-type form lexical-env
				dynamic-env eval-targets declarations)))))

;;; Series bindings
(defun updated-declarations (exitp declarations lexical-env n-specials)
  (if exitp
      (let ((declarations (copy-list declarations)))
	(incf (getf declarations 'controls 0) n-specials)
	declarations)
      declarations))


(defun WALK-BINDINGS (bindings decls body parse checks lexical-env dynamic-env
			       eval-targets declarations)
  (let (vars init-envs (n-specials 0)
	     (inits (when parse (make-op dynamic-env 'begin-parse parse)))
	     (exitp (find-if #'exit-target-p eval-targets)))
    (multiple-value-bind (body-env init-env)
	(add-variables (mapcar #'bindingform-name bindings)
		       lexical-env dynamic-env decls t)
      (do ((bindings bindings (cdr bindings)) ;Reorder init-env
	   (env init-env (frame-env env)))
	  ((null bindings))
	(push env init-envs))
      (let ((results
	     (make-protected
	      dynamic-env
	      #'(lambda ()
		  (dolist (binding bindings)
		    (let* ((env (pop init-envs))
			   (var (get-binding (bindingform-name binding) env))
			   (initform (bindingform-initform binding)))
		      (if (special-binding-p var)
			  (incf n-specials)
			  (push var vars))
		      (if (eq initform 'unbound-flag)
			  (setf (getf (binding-declarations var) 'dirty) t)
			(setq inits
			      (combine-results
			       dynamic-env inits
			       (walk-binding var initform (frame-env env)
					     dynamic-env nil))))))
		  (setq inits (combine-results
			       dynamic-env inits
			       (walk-sequence checks body-env dynamic-env
					      (make-targets) nil)))
		  (combine-results
		   dynamic-env
		   (if parse
		       (make-block
			dynamic-env nil inits (make-op dynamic-env 'end-parse
						       parse))
		       inits)
		   (walk-sequence body body-env dynamic-env eval-targets
				  (updated-declarations exitp declarations
							body-env n-specials)))) 
	      #'(lambda ()
		  (unless exitp (make-unwind dynamic-env n-specials))))))
	(make-block dynamic-env vars nil results)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                  ARGUMENT/INIT PROCESSING                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Arguably, either HOST-FUNCTION-P or FTYPE-DECL should expand the
;;; type-specifier.  This would provide a limited in way in which
;;; programmers could extend the syntax for ftype specifiers.

(defun host-function-p (type)
  (or (eq type 'ec:macro) (eq type 'ec:function)))

(defparameter *default-ftype-declaration* '(function (&rest t) t))
;;; IWBNI this intersected all ftype declarations.  This
;;; implementation only returns the first (innermost) one.  This is
;;; legal, but could be better.
(defun FTYPE-DECL (declarations)
  (getf declarations 'ftype *default-ftype-declaration* ))

(defun KEY-TYPE (name keylist)
  (dolist (pair keylist)
    (when (and (not (eq pair '&allow-other-keys))
	       (eq (first pair) name))
      (return (second pair)))))



;;; Walk each argument form to a temporary binding, collecting results
;;; and bindings.  All the hair is for type checking the bindings
;;; against ftype declarations.  It might be better to compile to
;;; temporaries, and then use the type-check as a predicate to
;;; determine whether a c:function call is acceptable.  It depends on
;;; how we handle temporaries and what we do with inappropriate C
;;; calls.

;;; IWBNI we checked the types of arguments for lisp:functions, but
;;; DID NOT introduce run-time type checks in compiled-code that were
;;; repeated inside the function.  For now, we don't type-check
;;; lisp:functions, only c:functions and c:macros.

(defparameter *arg-names*
  '#.(loop for i to call-arguments-limit
	   collect (make-symbol (princ-to-string i))))
(defun WALK-ARGUMENTS (designator forms lexical-env dynamic-env
				  declarations)
  (destructuring-bind (&optional ftype argtypes returntypes
				 &rest ignore)
      (ftype-decl declarations)
    (declare (ignore ignore))
    (let ((check-types-p (host-function-p ftype))
	  (return-type (return-type returntypes))
	  inits optionalp restp)
      (labels ((warning (msg &rest args)
			(setq return-type nil)
			(apply #'compiler-style-warn dynamic-env designator 'function
			       msg args) (setq restp t))
	       (next-type () 
			  (cond (restp restp)
				(t (let ((type (pop argtypes)))
				     (case type
				       (&optional (setq optionalp t) (next-type))
				       (&key (setq restp t)
					     (return-from next-type))
				       (&rest (setq restp (next-type)))
				       ((nil) (warning
					       "Too many arguments for ~s." designator))
				       (t type)))))))
	(let* ((env (loop for name in *arg-names*
			  ;; FORM is used to terminate loop
			  and form on forms
			  with types = nil
			  collect name into names
			  do (let ((type (next-type)))
			       (when (and type check-types-p (not (eq type 't)))
				 (push (list 'type type name) types)))
			  finally (return (add-variables
					   names lexical-env dynamic-env
					   types))))
	       ;; N.B.: Assumes first frame contains bindings and that
	       ;; they are in forward order!
	       (bindings (frame-bindings env)))
	  ;; N.B.: subforms of a top-level form are NOT at top-level,
	  ;; so arguments must NOT be processed at top-level. The
	  ;; easiest way to ensure this is to process them in env,
	  ;; instead of lexical-env!
	  (dolist (form forms) 
	    (push (walk-binding (pop bindings) form env dynamic-env nil) inits))
	  (when (and argtypes (not (or restp optionalp))
		     (not (lambda-list-keyword-p (pop argtypes))))
	    (warning "Not enough arguments for ~s." designator))
	  (values (nreverse inits) (frame-bindings env) return-type))))))

;;; This represents initial binding (i.e. not general assignment) to a
;;; true binding (i.e. not a symbol-macro or constant).
;;; 
;;; Similar to WALK-ASSIGNMENT, but takes a VARIABLE BINDING rather
;;; than a symbol naming the variable.  In addition, if binding is a
;;; special, then the right thing happens.  For serial bound specials,
;;; this means first DBINDing the variable.  For parallel bound
;;; specials, this means assigning the value as a lexical binding, and
;;; returning this binding as a second value to indicate that the
;;; caller must later arrange to DBIND the symbol and assign the value
;;; of the binding to the symbol-value, after all the normal parallel
;;; bindings have been made.  
(defun walk-binding (binding valueform lexical-env dynamic-env
			     parallel-p) 
  (let ((name (binding-name binding))
	(variable-declarations
	 (combine-declarations (binding-declarations binding) nil
			       binding dynamic-env)) 
	(targets (make-targets)))
    (if (getf variable-declarations 'special)
	(if parallel-p
	    (values (write-variable binding name valueform lexical-env
				    dynamic-env targets variable-declarations)
		    binding)
	    (values
	     (combine-results
	      dynamic-env
	      (make-op dynamic-env 'dbind (intern-constant dynamic-env name))
	      (write-variable :special name valueform lexical-env
			      dynamic-env targets variable-declarations))
	     nil))
	(values (write-variable binding name valueform lexical-env
				dynamic-env targets variable-declarations)
		nil))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                            FUNCTIONS                         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ENCLOSE (lambda &optional env name)
  (let ((closure (make-instance 'interpreted-function
				:lambda lambda :env env :name name)))
    ;;#+machine-compile ???!!!
    (set-funcallable-standard-instance-function
     closure (make-interpreter closure))
    closure))
(defun make-byte-compiled-function (&rest keys)
  (declare (dynamic-extent keys))
  (apply #'make-instance 'byte-compiled-function keys))

(defmethod print-object ((object packaged-function) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (write (or (function-name object)
	       (function-lambda object))
	   :stream stream)))

;;; The implementation for machine-compiled functions is arguably
;;; wrong.  It is possible that the source for some function could be
;;; recorded without the definition being installed.
;;; IWBNI we used a common representation for non-machine-compiled
;;; functions. 
(defmethod FUNCTION-LAMBDA-EXPRESSION ((fn packaged-function))
  (values (function-lambda fn)
	  (function-env fn)
	  (function-name fn)))
(defmethod FUNCTION-LAMBDA-EXPRESSION ((fn compiled-function))
  (let* ((name nil)			;!!!
	 (source (function-source name)))
    (if source
	(values source nil name)
	(values nil t name))))

;;; FUNCTION NAMES, etc:
;;; In Lisp generally:
;;;   Environment Specific:
;;;     Function-Name: a symbol or a list.
;;;     Lambda-Expression: a list begining with lambda,
;;;         used like a function-name.
;;;     Function-Name-Block-Name: a symbol. Either:
;;;         1. a symbol function name
;;;         2. the cadr of a setf function name
;;;     Function-Form: a list of function-name and arguments.
;;;     Lambda-Form: a list of lambda-expression and arguments.
;;;   Global:
;;;     Function-Designator: a symbol or a function object.
;;;     Extended-Function-Designator: a function-name or a function
;;;         object.
;;; In Eclipse:
;;;   Environment Specific:
;;;     fname: a function-name or #:lambda.
;;;   Global:
;;;     extended-function-name: a list of fnames, most specific last.
;;;         NB: 1. always a list of at least one element.
;;;             2. globally identifies a function.
;;;             3. used in reporting, but not accepted by fdefinition.

(defun MACHINE-FUNCTION-P (dynamic-env function-name declarations)
  (declare (ignore function-name))
  (typecase dynamic-env
    (compiler-env (host-function-p (first (ftype-decl declarations))))))

(defun DIRECT-FUNCTION-P (dynamic-env function-name declarations)
  (declare (ignore declarations))	;Should check speed, etc.
  (typecase dynamic-env
    (compiler-env
     (and (member (symbol-package (function-name-key function-name))
		  (load-time-value
		   (list (find-package :eclipse)
			 (find-package :cl)
			 #+lisp-host (find-package :host))))
	  (fboundp function-name)
	  (not (typep (fdefinition function-name) 'generic-function))))))

(defun VALUES-FUNCTION-P (dynamic-env function-name declarations)
  (declare (ignore function-name))
  (typecase dynamic-env
    (compiler-env (car-eq (third (getf declarations 'ftype)) 'values))))

;;; General ANSI "functional evaluation" converts a
;;; function-name/lambda-expression into a function-value, using the
;;; environment if necessary to check for local definitions.  The
;;; evaluation occurs implicitly for the heads of function-forms or
;;; explicitly by the FUNCTION special form.

;;; This function converts an extended-function-designator or
;;; lambda-expression to a function reference -- i.e. to one of:
;;; - a local function binding holding a closure.
;;; - the result of forming a closure (over a lambda-expression).
;;; - the result of a call to fdefinition to obtain a closure.
;;; - a reference to a machine (i.e. C) function.
;;; Up to three values are returned:
;;;  1. The reference.
;;;  2. The call type: :machine/:direct/nil
;;;  3. A no-values flag.  A non-null value indicates that the call does
;;;  not perform the usual side-effects on the multiple-values machinery.

(defun make-used (obj)
  (setf (getf (binding-declarations obj) 'used) t)
  (when (captured-function-p obj)
    (make-used (captured-function-binding obj))))

(defun FUNCTION-REFERENCE (dynamic-env obj lexical-env declarations
				     &optional check-machine-p)
  (cond ((lexical-function-p obj)
	 (typecase dynamic-env
	   (compiler-env (make-used obj)
			 obj)
	   (t (binding-value obj))))
	((car-eq obj 'lambda)
	 (closure-value dynamic-env nil obj lexical-env declarations t))
	((and check-machine-p
	      (setq check-machine-p (compiler-env-p dynamic-env))
	      (machine-function-p dynamic-env obj declarations))
	 (values obj :machine
		 (not (values-function-p dynamic-env obj declarations))))
	((and check-machine-p
	      (direct-function-p dynamic-env obj declarations))
	 (values obj :direct nil))
	(t (make-fdefinition dynamic-env obj))))
  

(defun WALK-FUNCTION-DEFINITION (function-name lambda-list body privatep
					       lexical-env dynamic-env
					       eval-targets
					       declarations
					       &optional fdef-set)
  (make-result dynamic-env
	       (target (closure-value dynamic-env function-name
				      `(lambda ,lambda-list ,@body)
				      lexical-env declarations privatep
				      fdef-set)
		       dynamic-env eval-targets
		       (known-type dynamic-env 'function declarations))))

(defun CLOSURE-VALUE (dynamic-env name lambda-form lexical-env declarations
				  &optional privatep fdef-set)
  (typecase dynamic-env
    (COMPILER-ENV
     (let* ((boundary (find-boundary-env lexical-env))
	    (enclosing-ids
	     (if boundary
		 (boundary-env-functions boundary)
		 (error "No function closure environment found in ~a." lexical-env)))
	    (this-id (or name 'lambda))
	    (id (cons this-id (boundary-env-id boundary)))
	    (count (count this-id enclosing-ids :test #'name-clash))
	    ;; This should be passed in, to properly reflect flet!!!
	    #+old-code
	    (declarations (nth-value 2 (function-information
					name lexical-env)))
	    (ftype (getf declarations 'ftype))
	    (return-type (return-type (third ftype)))
	    (decs (when return-type
		    (add-type-to-declarations
		     return-type nil lambda-form dynamic-env))))
       (unless (zerop count) (push count id))
       (setf (boundary-env-functions boundary)
	     (cons this-id enclosing-ids))
       (let* ((new-boundary (make-boundary-env :id id :env
					       lexical-env))
	      (targ (make-function-return :binding id))
	      (targs (add-target targ)))
	 (multiple-value-bind (result arg-bindings)
	     (case (first ftype)
	       (ec:function
		(walk-c-function (second lambda-form)
				 (second ftype)
				 (cddr lambda-form)
				 new-boundary dynamic-env targs decs))
	       (t (values (walk-lambda 'ec::ap lambda-form new-boundary
				       dynamic-env targs decs)
			  nil)))
	   (let* ((id (cons 'function (reverse id)))
		  (bindings (boundary-env-enclosed-bindings
			     new-boundary))
		  (n (length bindings)))
	     (let ((def `(,privatep
			  ,(implementation-type dynamic-env return-type)
			  ,id ,arg-bindings ,bindings
			  ,@result)))
	       (if fdef-set
		   (push def (cdr fdef-set))
		   (push def (compiler-env-functions dynamic-env))))
	     `(make-closure ,n ,id
			    ,(if (zerop n)
				 null-hook-flag
				 `(address (env-hook ,id)))
			    ,@(mapcar #'(lambda (b) `(address ,b))
				      bindings)))))))
    (t					;INTERPRETER
     ;; This hack removes superflous environment bindings that may
     ;; have been present because the closure was an argument to
     ;; something. 
     (block hack
       (do ((frame lexical-env (frame-env frame)))
	   ((null frame) (setq lexical-env nil))
	 (typecase frame
	   (variable-env
	    (loop for binding in (frame-bindings frame)
		  unless (member (binding-name binding) *arg-names* :test 'eq)
		  do (return-from hack t)))
	   (t (return-from hack t)))))
     (enclose lambda-form lexical-env name))))

(defun WALK-C-FUNCTION (args arg-types body lexical-env dynamic-env
			     eval-targets declarations)
  (multiple-value-bind (decls body) (find-declarations body nil)
    (let* ((body-env (augment-environment
		      lexical-env 
		      :variable args
		      :declare
		      (append decls
			      (mapcar #'list arg-types args))))
	   (bs (frame-bindings body-env)))
      (setf (getf declarations 'supplies-values) t)
      (values 
       `((compound ,@(walk-sequence body body-env dynamic-env eval-targets
				    declarations)))
       (list bs (make-declare dynamic-env bs))))))
  

;;; MAKE-CALL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Once upon a time, we tried to grovel over this to eliminate
;;; temporaries where possible.  Now we use the general temporary
;;; variable elimination machinery.
(defun simple-val (binding init)
  (destructuring-bind (first-instruction &rest more-instructions) init
    (when (and (not more-instructions)
	       (consp first-instruction))
      (destructuring-bind (&optional assignment var val &rest more)
	  first-instruction
	(when (and (not more)
		   (eq assignment 'setq)
		   (eq var binding))
	  val)))))

(defun pretty-arguments (bindings inits)
  (let ((tail (last bindings)))
    (when tail
      (let* ((binding (first tail))
	     (val (simple-val binding (first (last inits)))))
	(when (and val (typecase val
			 ((or symbol constant-id ec:void) )
			 (binding )
			 (t t)))
	  (setf (getf (binding-declarations (car tail))
		      'call-eliminable) t)))))
  ;; In addition to being to fancy, the following is WRONG because
  ;; the real last-call to appear at this point to be just a reference
  ;; to a non-dirty temporary variable computed earlier.  The
  ;; following code would skip over the temporary variable reference,
  ;; and make some call to the left of it be the last-call.  This
  ;; "earlier" call would then get eliminated (i.e. appear inline in
  ;; this call) and so we would have the side-effects for the left
  ;; call after the side-effects for the right call.
  #+wrong				
  (loop with last-call = nil
	for binding in bindings
	and init in inits
	do (destructuring-bind (first-instruction &rest more-instructions) init
	     (when (and (not more-instructions)
			(consp first-instruction))
	       (destructuring-bind (&optional assignment var val &rest more)
		   first-instruction
		 (when (and (not more)
			    (eq assignment 'setq)
			    (eq var binding))
		   (typecase val
		     ((or symbol constant-id ec:void) )
		     (binding (setq last-call nil))
		     (t (setq last-call binding)))))))
	finally (when last-call
		  (setf (getf (binding-declarations last-call)
			      'call-eliminable) t)))
  (values bindings
	  (loop for init in inits nconc init)
	  bindings))

(defun MAKE-CALL (dynamic-env function arg-bindings arg-inits
			      eval-targets declarations
			      &optional machine-call-p)
  (typecase dynamic-env
    (COMPILER-ENV
     (multiple-value-bind (bindings inits references)
	 (pretty-arguments arg-bindings arg-inits)
       (when (and machine-call-p (consp function))
	 (setq function `(function ,function)))
       (make-block dynamic-env bindings inits
		   (make-result dynamic-env
		     (target (case machine-call-p
			       (:machine `(,function ,@references))
			       (:direct `(,function ,@references ,eoa-flag))
			       (t `(call ,function ,@references)))
			     dynamic-env eval-targets declarations)))))
    (t					;INTERPRETER
     (interpreted-call
      function arg-inits dynamic-env eval-targets declarations))))

(defun INTERPRETED-CALL (function args dynamic-env eval-targets
				  declarations &optional resetp)
  (typecase function
    (INTERPRETED-FUNCTION
     (values
      (walk-lambda args
		   (interpreted-function-lambda function)
		   (interpreted-function-env function)
		   dynamic-env eval-targets declarations)))
    (t					;COMPILED-FUNCTION
     (let ((mv-target (find-if #'multiple-values-target-p
			       eval-targets))
	   (values (multiple-value-list (apply function args))))
       (when mv-target
	 (when resetp
	   ;; It is possible that some multiple-values-prog1 has
	   ;; already filled the buffer. This resets it.
	   (setf (multiple-values-target-values mv-target) 'all))
	 (fill-multiple-values values declarations mv-target))
       (target (first values) dynamic-env eval-targets declarations)))))

(defun WALK-FUNCALL (extended-function-designator
		     args lexical-env dynamic-env eval-targets
		     declarations)
  (multiple-value-bind (arg-inits arg-vars return-type)
      (walk-arguments extended-function-designator args lexical-env
		      dynamic-env declarations)
    (multiple-value-bind (func machine-call-p single-value-p)
	(function-reference dynamic-env extended-function-designator
			    lexical-env declarations t)
      (unless (getf declarations 'supplies-values)
	(setf (getf declarations 'supplies-values) (not single-value-p)))
      (when machine-call-p
	(register-call dynamic-env func declarations))
      (make-call dynamic-env
	func arg-vars arg-inits eval-targets
	(known-type dynamic-env return-type declarations)
	machine-call-p))))


(defparameter *preregistered-c-operators*
  '(fdefinition funcall-function symbol-value system-property
		system-property-setter make-closure)) 

(defmethod REGISTER-CALL ((dynamic-env t) (function t) (declarations t)))



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
