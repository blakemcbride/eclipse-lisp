#| temporary!!!  See macro.lisp
(defun make-symbol-macro-function (expansion)
  #'(lambda (form env)
      (declare (ignore form env))
      expansion))
(defun macro-function-p (f)
  #+excl (cl:typep f 'macro-function)
  #+cmu (eq f (symbol-function 'cl:defmacro)))
(defun global-symbol-macro-function (name) nil)
|#


;;; xxx-information returns typed bindings.  If it has to cross one
;;; or more function-boundaries, the function boundaries will be
;;; side-effected to include the appropriate reference, and the
;;; captured-binding returned.
#| ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GOLDEN RULE OF DECLARATIONS:
Whenever a new binding is created, any existing outer declarations
should be ignored within the scope of the new binding.  This is
true even for: 
  - type definitions (nesting only applies for declarations
    refering to the same binding).
  - a special binding shadowing a local binding
  - a NEW special binding shadowing an existing special binding.

The only exception is that a local binding of a variable proclaimed
special must still be special.  
  
One consequence of this is that the compiler does not intersect
information about different bindings of the same special variable.

We implement this by adding declarations to each frame in which the
declaration appears -- to the binding itself if there is one in
the same frame, otherwise to a "dummy" binding.  When collecting
variable information, we append all the applicable declarations
from "dummy" bindings we encounter as well as the from the "real" 
bindings.  The innermost declarations are listed first, so these
appear earlier in the plist.

LOCALLY SPECIAL RULE:
We further assume that a special declaration which causes
references within the declaration to refer to a variable which
would OTHERWISE BE LEXICAL introduces a new scope in which
declarations for the special variable are accumulated as though a
new binding were being used.  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; |#

;;; See macro.lisp.  Its defined here because macro.lisp is not
;;; present in runtime library.
(defun eclipse::global-symbol-macro-function (name)
  (eclipse::system-property name 'symbol-macro nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                 DATA STRUCTURES                              ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defstruct (BINDING
	    (:print-function print-binding)
	    (:constructor make-env-binding))
  (name nil :read-only t)
  (value 'unbound-flag)
  (status t)	;A null status indicates a dummy (declaration only) binding.
  declarations)

(defstruct (RUNTIME-BINDING (:include binding)))
(defstruct (VAR (:include runtime-binding)))
(defstruct (LEXICAL-FUNCTION (:include runtime-binding)))
(defstruct (CAPTURED-VAR (:include var)) boundaries index binding)
(defstruct (CAPTURED-FUNCTION (:include lexical-function)) boundaries index binding)

(defstruct (COMPILETIME-BINDING (:include binding)))
(defstruct (SYMBOL-MACRO-BINDING (:include compiletime-binding)))
(defstruct (MACRO-BINDING (:include compiletime-binding)))

(defstruct (TAG (:include binding)))
(defstruct (NAMED-BLOCK (:include tag))
  (eval-targets nil))
(defstruct (GO-TAG (:include tag)) key)
(defstruct (VALUES-BUFFER (:include tag)))

(defstruct (CAPTURED-BLOCK (:include named-block)) boundaries index binding)
(defstruct (CAPTURED-TAG (:include go-tag)) boundaries index binding)

(defun print-binding (b stream level)
  (declare (ignore level))
  (print-unreadable-object (b stream :type t)
    (write (binding-name b) :stream stream)
    (let ((index (getf (binding-declarations b) 'index 0)))
      (unless (zerop index)
	(write-char #\space stream)
	(princ index stream)))))

(defun SPECIAL-BINDING-P (binding)
  (getf (binding-declarations binding) 'special))

(defstruct (ENV (:print-function print-env))
  (env nil))

(defstruct (BINDING-ENV (:include env) (:conc-name frame-))
  (bindings nil))
(defstruct (basic-variable-env (:include binding-env)))
(defstruct (VARIABLE-ENV (:include basic-variable-env)))
(defstruct (MACRO-VARIABLE-ENV (:include basic-variable-env)))
(defstruct (basic-function-env (:include binding-env)))
(defstruct (FUNCTION-ENV (:include basic-function-env)))
(defstruct (MACRO-FUNCTION-ENV (:include basic-function-env)))
(defstruct (DECLARATION-ENV (:include binding-env)))
(defstruct (BLOCK-ENV (:include binding-env)))
(defstruct (TAGBODY-ENV (:include binding-env)))
(defstruct (VALUES-ENV (:include binding-env)))
(defstruct (CONTROL-ENV (:include values-env)))
(defstruct (BOUNDARY-ENV (:include binding-env))
  (id nil)
  (labels nil)
  (functions nil)
  (enclosed-bindings nil))

(defgeneric print-env-data (env stream))
(defmethod print-env-data (env stream)
  (declare (ignore env stream)) nil)
(defmethod print-env-data ((env binding-env) stream)
  (write (mapcar #'binding-name (frame-bindings env))
	 :stream stream)
  t)
(defun print-env (env stream depth)
  (print-unreadable-object (env stream :type t :identity t)
    (when (or (null *print-level*) (<= depth *print-level*))
      (when (and (print-env-data env stream)
		 (env-env env))
	(write-char #\space stream))
      (when (env-env env)
	(write (env-env env) :stream stream)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                 DECLARATION HANDLERS                         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NOTE: The :variable and :function handlers created by
;;; define-declaration are supposed to return a list of 
;;; 	(<binding-name> <key> <value>)
;;; elements.  To support (dynamic-extent #'foo), (ignorable #'foo),
;;; etc., we also allow <binding-name> to be of the form (function
;;; <name>), even though the first value returned by the declaration
;;; handler in such cases will be :VARIABLE.  

(defparameter *declaration-handlers* (make-hash-table))

(defun unknown-declaration (form env)
  (declare (ignore env))
  (warn "Ignoring unrecognized declaration ~s." form)
  nil)

;;; DEFINE-DECLARATION and the actual predefined declaration
;;; definitions are in more-compile.lisp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                 "PUBLIC" INTERFACE                           ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Returns the environment in which the given environment was built.
;;; Note that AUGMENT-ENVIRONMENT may introduce several layered
;;; environments so that (enclosing-environment (augment-environment
;;; X)) is not necessarilly X.
(defun enclosing-environment (env) (and env (frame-env env)))

;;; Processes declarations in an environment and returns three values:
;;; - A plist of variable-name/declaration-plist pairs
;;; - A plist of function-name/declaration-plist pairs
;;; - A plist of declaration-name/values pairs
;;; where declaration-plist is a plist of key/value pairs.
(defun canonicalize-declarations (declarations env &aux vbindings fbindings dbindings)
  (dolist (decl declarations)
    (multiple-value-bind (returned-declaration-type items-to-add)
	(let ((key (car decl)))
	  (funcall (or (gethash key *declaration-handlers*)
		       (and (find-type key nil) ;i.e. a type specifier
			    (gethash 'type-abbreviation *declaration-handlers*))
		       #'unknown-declaration)
		   decl env))
      (dolist (spec (if (eq returned-declaration-type :declare)
			(list items-to-add)
		      items-to-add))
	(let* ((returned-name (car spec))
	       (data (cdr spec))
	       (function-name-p (or (car-eq returned-name 'function)
				    #+lisp-host (car-eq returned-name 'cl:function)))
	       (declaration-type
		(if (eq returned-declaration-type :variable)
		    (if function-name-p :function :variable)
		  returned-declaration-type))
	       (name (if function-name-p (second returned-name) returned-name)))
	  (macrolet ((add-to (bindings)
		     ;; Can't use setf/getf because name may be list.
	             `(loop for (fname . rest) on ,bindings by #'cddr
			    when (equal name fname)
			    return (setf (car rest) (append (car rest) data))
			    finally (setq ,bindings
					  (list* name data ,bindings)))))
	    (case declaration-type
	      (:variable (add-to vbindings))
	      (:function (add-to fbindings))
	      (:declare (add-to dbindings))))))))
  (values vbindings fbindings dbindings))

;;; An additional keyword argument, :bound-declarations-p indicates
;;; that bound declarations will apply to initialization forms
;;; appearing at the head of a body being represented by the new
;;; environment.  This is used for LABELS, LET*, and LAMBDA.  The
;;; default is nil.

;;; In all cases, two environments are returned.  The first is as
;;; described in CLtL2, and is an appropriate environment for the BODY
;;; of a Common Lisp special form.  The second value is an environment
;;; that is appropriate for the INITITIALIZATION forms of a Common
;;; Lisp special form.  When :bound-declarations-p is nil, the second
;;; environment is equivalent to the environment passed as an
;;; argument to augment-environment.

;;; When :bound-declarations-p is non-nil, the second argument
;;; includes the :function and :variable bindings, and only those
;;; declarations which refer to these bindings (i.e. bound
;;; declarations).  In addition, the :variable bindings and their
;;; bound declarations are added in sequence and successively earlier
;;; bindings can be accessed using ENCLOSING-ENVIRONMENT.  For
;;; example, one might represent the environment formed by:
;;;	(let* ((a 1) (b a) (c b))
;;;       (declare (special b z) (fixnum c))...)
;;; as (augment-environment x   :variable '(a b c)
;;;				:declare '((special b z) (fixnum c))
;;;				:bound-declarations-p t)
;;; The first environment returned is suitable for the body of the
;;; LET* and includes variable bindings for A, B, and C, special
;;; declarations, for B and Z, and a fixnum declarations for C.

;;; The second environment returned does not include free declarations.
;;; The ENCLOSING-ENVIRONMENT of this is suitable for the
;;; initialization form for the last variable, C.  It includes the
;;; original environment argument to augment-environment, plus
;;; bindings for A and B, and a special declarations for B.  

;;; ENCLOSING-ENVIRONMENT of this environment is suitable for the
;;; initialization form of the preceding variable, B, and includes the
;;; original environment and a binding for A. 
	
;;; ENCLOSING-ENVIRONMENT of this environment is suitable for the
;;; initialization form of the first variable, A, and is equivalent to
;;; the environment which was originally passed to
;;; augment-environment.

;;; Note that no Common Lisp form introduces both :function and
;;; :variable bindings, and that :function bindings are never
;;; introduced in series.  The scope of :function bindings in LABELS
;;; includes ALL the function definitions forms, so the second
;;; environment value returned by augment-environment may be used for
;;; all the LABELS function definitions and one need not descend
;;; through the environments using ENCLOSING-ENVIRONMENT.  The
;;; second returned value is undefined if one combines non-nil values
;;; for all of :variable, :function and :bound-declarations-p.

;;; In order to support indexing (i.e. renaming of similarly named
;;; variables with overlapping scope) the initialization environment
;;; may include a dummy "declaration-only" binding for the new
;;; variables defined in the body environment.  This is because the C
;;; scope of the initialization forms includes the new bindings, and
;;; these forms may introduce newer bindings with the same name.
;;; Indexing looks for bindings (declaration-only or otherwsise) with
;;; the same name.  

(defun AUGMENT-ENVIRONMENT (env
			    &key variable symbol-macro
				 function macro
				 declare
				 bound-declarations-p
				 index-p
			    &aux (init-env env)
				 (body-env env))
  (macrolet ((grab (decs name)
		   ;; Can't use getf/remf because name may be a list
		   `(loop for (bname . bdecs) on ,decs by #'cddr
			  and last = nil then bdecs
			  when (equal bname ,name)
			  do (if last
				 (setf (cdr last) (cdr bdecs))
				 (setf ,decs (cdr bdecs)))
			  and return (car bdecs))))
    (flet ((add-free-declarations (declarations frame-constructor type)
	     (when (and declarations (or (eq body-env init-env)
					 (not (typep body-env type))))
	       (setf body-env (funcall frame-constructor :env body-env)))
	     (loop for (name decls) on declarations by #'cddr
		   do (push (make-env-binding :name name
					      :declarations decls
					      :status nil)
			    (frame-bindings body-env)))))
      ;; This processes declarations in the original environment, so the
      ;; declaration handlers can't tell whether a new declaration will
      ;; be bound or free.  (sigh.)
      (multiple-value-bind (vars funs decs)
	  (canonicalize-declarations declare env)
	
	(when variable
	  (let* ((dummies nil)
		 (bindings
		  (loop for name in variable
			with binding
			when (constantp name env)
			do (error "Cannot bind constant ~s." name)
			do (setq binding
				 (make-var
				  :name name
				  :declarations
				  (append (grab vars name)
					  (when (globally-special-p name)
					    ;; IWBNI we also picked up (type)
					    ;; declarations from corresponding
					    ;; enclosing (and global) specials. 
					    '(special t)))))
			when index-p
			do (index-binding binding bindings body-env 'variable-env)
			and do (push (make-var :name name :status nil) dummies)
			collect binding into bindings
			finally (return bindings))))
	    (cond (bound-declarations-p
		   (when dummies
		     (setq body-env (make-variable-env :bindings dummies
						       :env body-env)))
		   (dolist (b bindings)
		     (setq body-env (make-variable-env :bindings (list b)
						       :env body-env)
			   init-env body-env)))
		  (t
		   (when dummies
		     (setq init-env (make-variable-env :bindings dummies
						       :env init-env)))
		   (setq body-env (make-variable-env :bindings bindings
						     :env body-env))))))
	(add-free-declarations vars #'make-variable-env 'variable-env)
      
	(when function
	  (setq body-env
		(make-function-env
		 :bindings
		 (loop for name in function
		       with binding
		       do (setq binding (make-lexical-function
					 :name name
					 :status 'function
					 :declarations
					 (grab funs name)))
		       when index-p
		       do (index-binding binding bindings body-env 'function-env)
		       collect binding into bindings
		       finally (return bindings))
		 :env body-env))
	  (when bound-declarations-p (setq init-env body-env)))
	(add-free-declarations funs #'make-function-env 'function-env)

	(add-free-declarations decs #'make-declaration-env 'declaration-env)

	(macrolet ((do-macros (defs macro-constructor binding-constructor
				bad-definition-check error-message
				&optional key)
		     `(when ,defs
			(setf body-env (,macro-constructor :env body-env))
			(dolist (pair ,defs)
			  (destructuring-bind (name definition) pair
			    (when ,bad-definition-check
			      (error 'program-error
				     :format-control ,error-message
				     :format-arguments `(,name)))
			    (push (,binding-constructor
				   :name name
				   :value ,(if key
					       `(,key definition)
					       'definition))
				  (frame-bindings body-env)))))))
	  (do-macros symbol-macro make-macro-variable-env make-symbol-macro-binding
		     ;; It is not clear if we must also check for
		     ;; enclosing special declarations. ???
		     (or (find name variable)
			 (getf (getf decs name) 'special)
			 (globally-special-p name))
		     "~s is defined as both a symbol-macro and a variable."
		     make-symbol-macro-function)
	  (do-macros macro make-macro-function-env make-macro-binding
		     (find name function)
		     "~s is defined as both a function and a macro."))
	;; Make sure we enforce non-toplevel qualities of LET.
	(when (eq body-env env)
	  (setq body-env (make-variable-env :env env)
		init-env body-env))
	(values body-env init-env)))))

;;; This is correct, but silly.  The hair is associated with indirect
;;; closures where we have a set of bindings in the environment which,
;;; when closed over, all share a single "real" binding.  This
;;; happens, for example, with tagbody labels.  In implementation
;;; here, a DOUBLY closed over binding that uses two "fake" bindings
;;; from the same tagbody will use one environment variable in the
;;; first closure (representing the tagbody, desired) but two in the
;;; second closure (one for each tag, not really desirable).
(defun make-captured-binding (binding constructor boundaries)
  (dolist (boundary boundaries binding)
    (let* ((status (binding-status binding))
	   (indirectp (binding-p status))
	   (key (if indirectp status binding))
	   (bindings (boundary-env-enclosed-bindings boundary))
	   (found (find key bindings))
	   (found2 (when found
		     (getf (getf (binding-declarations binding) 'enclosed) binding))))
      (if found2
	  (setq binding found2)
	  (let* ((index (if found
			    (getf (binding-declarations key) 'captured-index)
			    (setf (getf (binding-declarations key) 'captured-index)
				  (length bindings))))
		 (captured (funcall constructor
				    :name (binding-name binding)
				    :declarations (binding-declarations binding)
				    :binding binding
				    :index index
				    :boundaries boundaries)))
	    (setf (getf (getf (binding-declarations binding) 'enclosed) binding)
		  captured)
	    (unless found
	      (setf (boundary-env-enclosed-bindings boundary)
		    (nconc bindings (list key))))
	    (setq binding captured))))))
			      
(defun control-information (name type env captured-constructor
				 &optional (test #'eq))
  (do ((declarations nil)
       (boundaries nil)
       (frame env (env-env frame)))
      ((null frame) (values nil declarations))
    (typecase frame
      (BOUNDARY-ENV
       (push frame boundaries))
      (CONTROL-ENV
       (incf (getf declarations 'controls 0)))
      (VALUES-ENV
       ;; We only want the outermost id.
       (setf (getf declarations 'unwind-values)
	     (first (frame-bindings frame))))
      (VARIABLE-ENV
       (incf (getf declarations 'controls 0)
	     (count-if #'(lambda (b)
			   (and (binding-status b)
				(special-binding-p b)))
		       (frame-bindings frame))))
      (t
       (when (typep frame type)
	 (let ((b (find name (frame-bindings frame)
			:test test
			:key #'binding-name)))
	   (when b
	     (return
	      (values
	       (if boundaries
		   (make-captured-binding b captured-constructor boundaries)
		   b)
	       (nconc declarations (binding-declarations b)))))))))))
	   
(defmacro doframes ((var env type
			 &optional result-form)
		    general
		    &body body)
  (let ((type-var (gensym "TYPE")))
    `(do ((,type-var ,type)
	  (,var ,env (env-env ,var)))
	 ((not (env-p ,var)) ,result-form)
       ,@(when general `(,general))
       (when (typep ,var ,type-var)
	 ,@body))))

(defun name-clash (symbol-a symbol-b)
  (or (eq symbol-a symbol-b)
      (and (consp symbol-a) (consp symbol-b)
	   (eql (car symbol-a) (car symbol-b))
	   (every #'name-clash (cdr symbol-a) (cdr symbol-b)))
      (and (symbolp symbol-a) (null (symbol-package symbol-a))
	   (symbolp symbol-b) (null (symbol-package symbol-b))
	   (string= symbol-a symbol-b))))

(defun index-binding (binding bindings env frame-type &aux (name (binding-name binding)))
  (flet ((check (name bindings)
	   (let ((match (find name bindings
			      :key #'binding-name :test #'name-clash)))
	     (when match 
	       (setf (getf (binding-declarations binding) 'index)
		     (1+ (getf (binding-declarations match) 'index 0)))
	       (return-from index-binding)))))
    (check name bindings)
    (doframes (frame env frame-type)
      (when (boundary-env-p frame) (return))
      (check name (frame-bindings frame)))))

;;; NOTE: We return the pair (special t) in the declarations alist
;;; when it is appropriate.
;;; NOTE COMPILER DEPENDENCY: For either local macros or
;;; symbol-macros, we return the definition as the second value.
(macrolet
    ;; Beware of variable capture here!
    ((information ((frame-type end-type &optional global-declarations)
		   &body binding-case)
       `(let ((declarations nil) (boundaries nil) binding)
	  (doframes (frame env ,frame-type
			   (let ((declarations
				  ,(if global-declarations
				       `(append declarations
						,global-declarations)
				       'declarations)))
			     (remf declarations 'enclosed)
			     (values ,end-type nil declarations)))  
		    (when (boundary-env-p frame)
		      (push frame boundaries)) 
		    (when (setf binding (find name (frame-bindings frame)
					      :test #'equal
					      :key #'binding-name))
		      (setf declarations (append declarations
						 (binding-declarations binding)))
		      ,@binding-case)))))
  
  (defun VARIABLE-INFORMATION (name &optional env)
    (information ((find-type 'basic-variable-env)
		  (cond ((constantp name env) :constant)
			((globally-special-p name) :special)
			((getf declarations 'special) :special)
			((global-symbol-macro-function name) :symbol-macro)
			(t nil))
		  (global-declaration name 'global-variable nil))
     (when (binding-status binding)
       (let ((kind (cond
		    ((getf declarations 'special) :special)
		    ((macro-variable-env-p frame) :symbol-macro)
		    (t :lexical))))
	 (return (values kind
			 (if (and (eq kind :lexical) boundaries)
			     (make-captured-binding
			      binding #'make-captured-var boundaries)
			     binding)
			 declarations))))))
  
  ;; NOTE COMPILER DEPENDENCY: Steele says that this "returns
  ;; information about the interpretation of the function-name [name]
  ;; when it appears in the functional position" of a form.  We define
  ;; it for names "when it appears as the second argument to the
  ;; FUNCTION special operator." 

  ;; We take the position that FTYPE, INLINE/NOTINLINE and
  ;; IGNORE/IGNORABLE declarations for NAME are not automatically
  ;; applicable to (SETF NAME) - i.e. they must be defined separately
  ;; by the programmer.
  (defun FUNCTION-INFORMATION (name &optional env)
    (if (car-eq name 'lambda)
	;; Maybe this should return an indexed anonymous binding?
	(values :function nil nil)
	(information ((find-type 'basic-function-env)
		      (if (special-operator-p name)
			  :special	;Steele returns :special-operator!!!
			  (let ((def (fdefinition1 name))
				(ftype (first (getf declarations 'ftype))))
			    (cond ((eq ftype 'ec:function) nil)
				  ((eq ftype 'ec:macro) nil)
				  ((and (not (eq ftype 'function))
					(or (macro-function-p def)
					    #-machine-compile
					    (and (symbolp name) (cl:macro-function name))))
				   :macro)
				  (def :function))))
		      (let ((setfp (setf-function-name-p name)))
			(global-declaration
			 (function-name-key name setfp)
			 (if setfp 'global-setf-function 'global-function)
			 nil)))
		     (when (binding-status binding)
		       (let ((kind (if (macro-function-env-p frame)
				       :macro :function)))
			 (return (values kind
					 (if (and (eq kind :function) boundaries)
					     (make-captured-binding
					      binding #'make-captured-function
					      boundaries)
					     binding)
					 declarations)))))))

  (defun DECLARATION-INFORMATION (name &optional env)
    (information ((find-type 'declaration-env)
		  (append declarations (global-declaration name 'global-declaration nil)))
      (return (append declarations (global-declaration name 'global-declaration nil)))))
  )
