(defmacro eclipse:FUNCTION (name) `(function ,name))

(defparameter user::*declare-c-macros-p* t)
(defmacro eclipse::host-declaim (&rest specs)
  `(cl:declaim ,@(if user::*declare-c-macros-p*
		     specs
		     (loop for spec in specs
			   for (spectype . args) = spec
			   collect (if (eq spectype 'ftype)
				       (destructuring-bind ((kind argt returnt &optional file)
							    &rest names)
					   args
					 (declare (ignore kind file))
					 `(ftype (cl:function ,argt ,returnt)
						 ,@(remove-if #'macro-function names)))
				       spec)))))
(declaim
 (inline eclipse::make-slots 
	 eclipse::make-wrapper-obsolete

	 eclipse::set-tagged-instance-class
	 eclipse::set-tagged-instance-wrapper
	 eclipse::set-standard-instance-slots
	 eclipse::set-funcallable-standard-instance-function

	 eclipse::wrapper-obsolete-slots
	 eclipse::wrapper-invalid-p 
	 eclipse::funcallable-standard-instance-function
	 )
 #-cmu
 (inline eclipse::object-class 
	 eclipse::symbol-function-value eclipse::symbol-setf-function-value
	 eclipse::set-symbol-function-value eclipse::set-symbol-setf-function-value
	 ))

(defvar eclipse::not-found (make-symbol "NOT-FOUND"))
(defvar eclipse::*keyword-package* (find-package :keyword))
(defun eclipse:Make-Keyword (x) (intern (string x) eclipse::*keyword-package*))

;; Mostly, this is a (casting) identity operation.
;; + Sometimes we cheat and treat a long as an object.  In the host
;;   this must return an ec:int.
;; + Other times, we use it get the address as an ec:int.  The host
;;   must take the address.
(defun eclipse::object-word (x)
  (typecase x
    (integer x)
    (t (eclipse::object-address x))))
      
(defun eclipse::word-object (x) x)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                     DEFTYPES                                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The following types are SHADOWED IN ECLIPSE:
(deftype eclipse:bit () 'bit)
(deftype eclipse:list () 'list)
(deftype eclipse:vector () '(or vector eclipse::basic-vector))
(deftype eclipse:generic-function ()
  `(and eclipse::funcallable-standard-instance
	(satisfies eclipse::generic-function-p)))
(deftype eclipse:hash-table ()
  `(and built-in-instance (satisfies eclipse:hash-table-p)))
(defun structurep (x) (eclipse:typep x 'eclipse:structure-object))
(defun xxpathnamep (x) (eclipse:typep x 'eclipse:pathname))
(defun slot-definition-p (x) (eclipse:typep x 'eclipse:slot-definition))
(deftype eclipse:structure-object ()
  `(and eclipse::standard-instance (satisfies structurep)))
(deftype eclipse::pathname ()
  `(and eclipse::standard-instance (satisfies xxpathnamep)))
(deftype eclipse::slot-definition ()
  `(and eclipse::standard-instance (satisfies slot-definition-p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                        CHARACTERS                             ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun eclipse::char-character (char) char)
(defun eclipse::character-char (base-char)
  #+(or cmu excl) base-char
  #-(or cmu excl)
  (if (>= (char-code base-char) 256)
      (error 'type-error :datum base-char :expected-type 'base-char)
      base-char))

(defun eclipse::wchar-character (wchar) wchar)
(defun eclipse::character-wchar (character) character)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                     CONSES                                   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; These must be defined as functions because we shadow car/cdr so as
;;; to be able to define defsetfs for them.
(declaim (inline eclipse:car eclipse:cdr))
(locally-pretty-fast
  (defun eclipse:car (x) (car x))
  (defun eclipse:cdr (x) (cdr x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                     CONTROL                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun host-closure (f)
  #'(lambda (&rest args)
      (eclipse::apply-interpreted-function f args)))


#-cmu
(declaim (inline eclipse:values eclipse::apply))
(locally-pretty-fast
  (defun eclipse:values (&rest args)
    (declare (dynamic-extent args))
    (apply #'values args))
  (defun eclipse:apply (f &rest args)
    (declare (dynamic-extent args))
    (apply (let ((f (etypecase f
		      (function f)
		      (eclipse::funcallable-standard-instance f)
		      (eclipse::standard-instance (host-closure f))
		      (symbol (eclipse::symbol-function-value f))
		      (cons (ecase (first f)
			      ((eclipse:setf setf)
			       (eclipse::symbol-setf-function-value (second f))))))))
	     (etypecase f
	       (eclipse::funcallable-standard-instance
		(the function (funcallable-standard-instance-function f)))
	       (function f)))
	   (apply #'list* args))))

(defmacro eclipse:THE (type form)
  (when (eclipse:car-eq type 'eclipse:values)
    (setq type `(values ,@(cdr type))))
  `(the ,type ,form))

;; These are redefined later by our loop.lisp
(defmacro eclipse:LOOP (&body body) `(loop ,@body))
(defmacro eclipse:LOOP-FINISH (&body body) `(loop-finish ,@body))

#+excl-old
(defun GET-SETF-EXPANSION (x &optional env)
  (declare (ignore env))
  (get-setf-method x nil))

#+excl-old
(defmacro DEFINE-SETF-EXPANDER (access-fn lambda-list &body forms)
  `(define-setf-method ,access-fn ,lambda-list ,@forms))
  
(defmacro eclipse:fixnum-index (x) x)
(defun eclipse:integer-index (x)
  (the (unsigned-byte 32) x))

(defun eclipse::test (int) (not (eql int 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                     SYMBOL                                   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(declaim (inline eclipse:symbol-plist eclipse::set-symbol-plist eclipse:make-symbol))
(defun eclipse:make-symbol (s) (make-symbol (string s)))
(defun eclipse:symbol-plist (x) (symbol-plist x))
(defun eclipse::set-symbol-plist (x list) (setf (symbol-plist x) list))

(defun eclipse::unboundp (x) (eq x eclipse::unbound-flag))
(defun eclipse::symbol-value-value (name)
  (if (boundp name)
      (symbol-value name)
      eclipse::unbound-flag))
(defun eclipse::set-symbol-value-value (name val)
  (if (eclipse::unboundp val)
      (makunbound name)
      (if (constantp name)
	  (eval `(defconstant ,name ,val))
	  (set name val))))

(defun eclipse::symbol-package-value (symbol)
  (let ((eclipse (get symbol 'eclipse-package 'not-defined)))
    (if (eq eclipse 'not-defined)
	(symbol-package symbol)
	eclipse)))
(defun eclipse::symbol-package-setter (pkg symbol)
  (setf (get symbol 'eclipse-package) pkg))

(defun eclipse::system-property (symbol indicator default)
  (getf (get symbol 'system) indicator default))

(defun eclipse::system-property-setter (symbol indicator val)
  (case indicator
    (compiler-macro
     #+excl
     (COMPILER::COMPILER-PUTPROP symbol val 'COMPILER::.COMPILER-MACRO.)
     #+cmu
     (setf (compiler-macro-function symbol) val))
    (setf-compiler-macro
     #+excl
     (COMPILER::COMPILER-PUTPROP `(setf ,symbol) val 'COMPILER::.COMPILER-MACRO.)
     #+cmu
     (setf (compiler-macro-function `(setf ,symbol)) val)))
  (setf (getf (get symbol 'system) indicator) val))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                     CHARACTER                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun eclipse::CHARACTER-INT (character) (char-int character))
(defun eclipse::INT-CHARACTER (code)
  (if (< code char-code-limit)
      (code-char code)
      (code-char 0)))
(defun eclipse::CHARACTER-WINT (character) (char-int character))
(defun eclipse::WINT-CHARACTER (code) (eclipse::int-character code))

(macrolet
    ((def-char-pred (name lispf)
	 `(defun ,name (char)
	    (if (,lispf (code-char char))
		1
		0))))
  (def-char-pred ec:ISALPHA alpha-char-p)
  (def-char-pred ec:ISALNUM alphanumericp)
  (def-char-pred ec:ISPRINT graphic-char-p)
  (def-char-pred ec:ISUPPER upper-case-p)
  (def-char-pred ec:ISLOWER lower-case-p)

  (def-char-pred ec:ISWALPHA alpha-char-p)
  (def-char-pred ec:ISWALNUM alphanumericp)
  (def-char-pred ec:ISWPRINT graphic-char-p)
  (def-char-pred ec:ISWUPPER upper-case-p)
  (def-char-pred ec:ISWLOWER lower-case-p))  

    
(macrolet
    ((def-char-convt (name lispf)
	 `(defun ,name (char)
	    (eclipse::character-int (,lispf (code-char (eclipse:int-integer char)))))))
  (def-char-convt ec:TOUPPER char-upcase)
  (def-char-convt ec:TOLOWER char-downcase)
  (def-char-convt ec:TOWUPPER char-upcase)
  (def-char-convt ec:TOWLOWER char-downcase))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun eclipse:index-fixnum (x) x)


(defun drain () (finish-output *standard-output*)
  (finish-output *error-output*)
  (finish-output *trace-output*)
  (finish-output *debug-io*))
  
(locally-fast
	      
  (defun eclipse::make-slots (n)
    (declare (type eclipse:index n))
    (make-array n :initial-element eclipse::unbound-flag))
  (defmacro eclipse::get-slot (slots n)
    `(svref (the eclipse::slots ,slots) (the eclipse:index ,n)))
  (defmacro eclipse::set-slot (slots n new)
    `(setf (svref (the eclipse::slots ,slots) (the eclipse:index ,n)) ,new))
  ;; For startup only.
  (defun eclipse::make-static-slots (&rest args)
    (declare (type list args) (dynamic-extent args))
    (make-array (length args)
		:initial-contents args))

  (defun eclipse::wrapper-invalid-p (wrapper)
    (zerop (wrapper-hash-key wrapper)))
  (defun eclipse::make-wrapper-obsolete (wrapper)
    (setf (wrapper-hash-key wrapper) 0))
  (defmacro eclipse::wrapper-hash-key (wrapper)
    `(wrapper-hash-key ,wrapper))
  (defun eclipse::wrapper-obsolete-slots (wrapper)
    (wrapper-obsolete-slots wrapper))

  (defmacro eclipse::tagged-instance-class (tagged-instance)
    `(tagged-instance-class (the eclipse::tagged-instance ,tagged-instance)))
  (defmacro eclipse::tagged-instance-wrapper (tagged-instance)
    `(tagged-instance-wrapper (the eclipse::tagged-instance ,tagged-instance)))
  (defun eclipse::set-tagged-instance-class (instance class)
    (setf (tagged-instance-class instance) class))  
  (defun eclipse::set-tagged-instance-wrapper (instance wrapper)
    (setf (tagged-instance-wrapper instance) wrapper))

  (defun eclipse::set-standard-instance-slots (instance slots)
	  (setf (standard-instance-slots instance) slots))  
  (defmacro eclipse::standard-instance-slots (standard-instance)
    `(standard-instance-slots (the eclipse::standard-instance ,standard-instance)))
  (defun eclipse::funcallable-standard-instance-function (instance)
    (funcallable-standard-instance-function instance))
  (defun eclipse::set-funcallable-standard-instance-function (instance function)
    (setf (funcallable-standard-instance-function instance) function)
    (update-host-fdefinition-from-backpointer instance)
    function
    #+was
    (let ((backpointer (funcallable-standard-instance-backpointer
			instance)))
      (unless backpointer
	(setf backpointer
	      (setf (funcallable-standard-instance-backpointer
		     instance)
		    (eclipse:slot-value instance 'eclipse::name))))
      (when backpointer
	(when (eclipse:car-eq backpointer 'eclipse:setf)
	  (setq backpointer `(setf ,(second backpointer))))
	#-cmu (setf (fdefinition backpointer) function)
	#+cmu (c::%%defun backpointer function nil)
	))))

(locally-fast
 (defun eclipse::object-class (object)
   (typecase object
     ;; Detectable by pointer tag in C.
     (integer eclipse::integer-classobj)
     (character eclipse::character-classobj)

     ;; Used only when embedded in a host Lisp.
     (null eclipse::null-classobj)
     (symbol eclipse::symbol-classobj)
     (cons eclipse::cons-classobj)
     (simple-base-string eclipse::simple-base-string-classobj)
     (simple-vector eclipse::simple-vector-classobj)
     (simple-bit-vector eclipse::simple-bit-vector-classobj)
     ;; i.e. any host function is considered compiled (opaque) to us
     (function eclipse::compiled-function-classobj)
     (simple-string eclipse::simple-base-string-classobj)
     (simple-bit-vector eclipse::simple-bit-vector-classobj)
     (simple-vector eclipse::simple-vector-classobj)
     (simple-array eclipse::simple-array-classobj)
     (cl:string eclipse::complex-base-string-classobj)
     (bit-vector eclipse::complex-bit-vector-classobj)
     (vector eclipse::complex-vector-classobj)
     (array eclipse::complex-array-classobj)

     ;; Detectable by pointer tag in C.
     (eclipse::tagged-instance (tagged-instance-class object))

     ;; More stuff used only when embedded in a host Lisp
     (complex eclipse::complex-classobj)
     (package (eclipse:find-type 'eclipse:package))

     ;; Ideally, this hair is never used. Most of it is is historical.
     (t (or (eclipse:find-type
	     (let ((type (type-of object)))
	       (if (consp type)
		   (destructuring-bind (class &optional element-type
					      dims &rest ignore)
		       type
		     (declare (ignore ignore) (dynamic-extent ignore))
		     (case class
		       (simple-array
			(if (= 1 (length (the list dims)))
			    (case element-type
			      ((base-char character)
			       'eclipse:simple-base-string)
			      (bit 'eclipse:simple-bit-vector)
			      (t 'eclipse:simple-vector))
			    'eclipse:simple-array))
		       (array
			(if (= 1 (length (the list dims)))
			    (case element-type
			      ((base-char character)
			       'eclipse::complex-base-string)
			      (bit 'eclipse::complex-bit-vector)
			      (t 'eclipse::complex-vector))
			    (if (typep object 'simple-array)
				'eclipse:simple-array ;Broken host
				'eclipse::complex-array)))
		       ;; CMU
		       (function 'eclipse:compiled-function)
		       (t class)))
		   (case type
		     (function 'eclipse::interpreted-function)
		     (t type)))) nil)
	    ;; Really just this part...
	    eclipse::unknown-object-classobj))))
  (defmacro eclipse::object-wrapper (object)
    `(let ((object ,object))
       (typecase object
	 ;; Detectable by pointer tag in C.
	 (eclipse::tagged-instance (tagged-instance-wrapper object))
	 (integer eclipse::integer-wrapperobj)
	 (character eclipse::character-wrapperobj)
	 (cons eclipse::cons-wrapperobj)
	 (simple-base-string eclipse::simple-base-string-wrapperobj)

	 ;; Used only when embedded in a host Lisp
	 (null eclipse::null-wrapperobj)
	 (symbol eclipse::symbol-wrapperobj)
	 (t (static-access (eclipse::object-class object) eclipse::wrapper eclipse::class)
	    #+really
	    (eclipse::unknown-object-wrapperobj)))))

  (defun eclipse:fixnump (x)
    ;;#-excl
    (typep x 'fixnum)
    #+nn ;;#+excl
    (typecase x
      (fixnum t)
      (integer (<= (integer-length x) eclipse::fixnum-size)))))


;;; These three could be defined using STATIC-ACCESS... 
(defmacro eclipse::instance-tag (x)
  `(locally-pretty-fast
     ;; Depends on (host::static-access x eclipse::name eclipse:standard-class)!!!
     (svref (standard-instance-slots (eclipse::object-class ,x)) 2)))
(defmacro eclipse::instance-cpl (x)
  `(locally-pretty-fast
     ;; (host::static-access gf eclipse::class-precedence-list eclipse:standard-class)!!!
     (svref (standard-instance-slots (eclipse::object-class ,x)) 6)))
(defmacro eclipse::emf-table (gf)
  `(locally-pretty-fast
  ;; depends on (host::static-access gf eclipse::gf-emf-table eclipse:standard-generic-function)!!!
     (svref (funcallable-standard-instance-slots ,gf) 9)))

(defmacro eclipse::fast-find (item list)
  `(let ((item ,item)
	 (list ,list))
     (locally-pretty-fast
      (loop for elt in list
	    when (eq elt item) return item))))

(defmacro eclipse::fast-find-slot (class slot-name)
  `(let ((class ,class)
	 (slot-name ,slot-name))
     (declare (type eclipse::standard-instance class))
     (locally-pretty-fast
      (loop for slot in
	    ;; depends on (host::static-access class eclipse::effective-slots eclipse:standard-class)!!!
	    (svref (standard-instance-slots class) 8)
	    when (eq slot-name
		     ;; depends on (host::static-access slot eclipse::name eclipse:slot-definition)!!!
		     (svref (standard-instance-slots slot) 2))
	    return slot))))

(defun instance-printer (object stream level)
  (declare (type fixnum level))
  (when (or (null *print-level*)
	    (< level *print-level*))
    #+debug-classes
    (print-unreadable-object (object stream :identity t)
      (format stream "~:(~s~) ~s"
	      (eclipse::get-slot (standard-instance-slots (eclipse:class-of object)) 2)
	      (eclipse::get-slot (standard-instance-slots object) 2)))
    #-debug-classes
    (eclipse::print-object object stream)))

(defmethod make-load-form ((object eclipse::tagged-instance)
			   &optional environment)
  (declare (ignore environment))
  (eclipse:make-load-form object))


;;;; ROW-MAJOR-AREF
#+excl-old
(defun (setf row-major-aref) (val array n)
  (do ((i (array-rank array))
       d ni indices)
      ((zerop i) (setf (apply #'aref array indices) val))
    (setq d (array-dimension array (decf i)))
    (multiple-value-setq (n ni) (floor n d))
    (push ni indices)))

#+excl-old
(defun row-major-aref (array n)
  (do ((i (array-rank array))
       d ni indices)
      ((zerop i) (apply #'aref array indices))
    (setq d (array-dimension array (decf i)))
    (multiple-value-setq (n ni) (floor n d))
    (push ni indices)))


;;; Some hosts always create flet bindings, even when they are not
;;; used and declared ignorable.  We pay a terrible perfmance penalty
;;; for this in our implemenation of methods.  This macro gets rid of
;;; this special case.
(defmacro eclipse:FLET (bindings &body body &environment env)
  (if (and (eq (caar bindings) 'eclipse:call-next-method)
	   (eclipse:car-eq (first body) 'eclipse:declare)
	   (not (uses-functions-p '(eclipse:call-next-method eclipse:next-method-p)
				  (rest body) env)))
      (let ((dummy (gensym "DUMMY")))
	`(let ((,dummy ,(second (second (third (second bindings))))))
	   (declare (ignore ,dummy))
	   ,@(rest body)))
      `(flet ,bindings ,@body)))

;;; This isn't really right -- a real code walk is necessary to make
;;; sure that we check all expandable subforms -- but this suites our
;;; purposes in an embedded lisp. 
(defun uses-functions-p (function-list body &optional env)
  (loop for form in (case (when (consp body) (car body))
		      ((eclipse:macrolet cl:macrolet)  (cddr body))
		      (t body))
	for expanded = (case (when (consp form) (car form))
			 ((eclipse:pprint-pop eclipse::pprint-exit-if-list-exhausted)
			  form)
			 (t (macroexpand form env)))
	thereis (if (consp expanded)
		    (case (car expanded)
		      (quote nil)
		      ;; Can't macroexpand (setf foo) - wrong # of args.
		      (function
		       (let ((f (second expanded)))
			 (if (symbolp f)
			     (find f function-list)
			     (uses-functions-p function-list (cddr f) env)))
		       #+old (find (second expanded) function-list))
		      (t (uses-functions-p function-list expanded env)))
		    (find expanded function-list))))

#| WRONG !!!
;;; This is a based on of the version in type-metaobjects.lisp
(define-compiler-macro TYPEP (&whole form object type &optional environment)
  (if (or environment (not (constantp type)))
      form
      (let ((expansion (expand-type (eval type) nil))
	    (symbol (gensym "TYPE")))
	`(typep ,object (if (boundp ',symbol)
			    (symbol-value ',symbol)
			    (setf (symbol-value ',symbol)
				  (expand-type ',(type-name expansion))))))))
|#

