#| *****************************************************************
Macros and other compile-time definitions needed for compiling
Eclipse, or running it within a host Lisp, but not needed for
compiling user code. 
***************************************************************** |#

(defconstant eclipse::float-mantissa (1- (ash 1 eclipse::float-signif)))
(defconstant eclipse::double-mantissa (1- (ash 1 eclipse::double-signif)))


;; For debugging
(defmacro eclipse::VARS (&rest variables)
  `(format *trace-output*
	   ,(loop with result = "~&"
		  for var in variables
		  do
		  (setf result
			(if (and (consp var)
				 (eq (first var) 'quote))
			    (concatenate 'cl:string result " ~S ")
			    (concatenate 'cl:string result (string-downcase var) " = ~S ")))
		  finally (return (concatenate 'cl:string result "~%")))
	   ,@variables))


(defun eclipse::xp (x &optional (print-circle? t))
  (let ((*print-circle* print-circle?)
	(*print-length* nil)
	(*print-level* nil))
    (pprint x t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;        XP STRUCTURES, AND THE INTERNAL ALGORITHM             ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-when (:execute :compile-toplevel :load-toplevel) ;not used at run time!!!
  (defvar eclipse::block-stack-entry-size 1)
  (defvar eclipse::prefix-stack-entry-size 5)
  (defvar eclipse::queue-entry-size 7)
  (defvar eclipse::buffer-entry-size 1)
  (defvar eclipse::prefix-entry-size 1)
  (defvar eclipse::suffix-entry-size 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                   ARITHMETIC                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro eclipse::WITH-RATIO ((n d x) &body body &aux (x-var (gensym "X")))
  `(let ((,x-var ,x))
     (let ((,n (eclipse:numerator ,x-var))
	   (,d (eclipse:denominator ,x-var)))
       (declare (type integer ,n ,d))
       ,@body)))

(defmacro eclipse::WITH-RATIOS ((nx dx x) (ny dy y) &body body)
  `(eclipse::with-ratio (,nx ,dx ,x)
	       (eclipse::with-ratio (,ny ,dy ,y)
			   ,@body)))

(defmacro eclipse::WITH-COMPLEX ((r i x &optional (type 'real))
			&body body &aux (x-var (gensym "X")))
  `(let ((,x-var ,x))
     (let ((,r (eclipse:realpart ,x-var))
	   (,i (eclipse:imagpart ,x-var)))
       (declare (type ,type ,r ,i))
       ,@body)))

(defmacro eclipse::WITH-COMPLEXES ((rx ix x) (ry iy y) &body body)
  `(eclipse::with-complex (,rx ,ix ,x)
		 (eclipse::with-complex (,ry ,iy ,y)
			       ,@body)))

(defmacro eclipse::defmethod-math2 (name (type1 type2) &optional (op name))
  (let ((op (make-name "~a-~a-~a" op type1 type2)))
    `(eclipse:defmethod ,name ((x ,type1) (y ,type2))
			(declare (inline ,op))
			(,op x y))))

(defmacro eclipse::defmethod-math1 (name (type) &optional (op name))
  (let ((op (make-name "~a-~a" op type)))
    `(eclipse:defmethod ,name ((x ,type))
			(declare (inline ,op))
			(,op x))))
      

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                   LISTS                                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro eclipse::defassoc (name accessor)
  `(eclipse:defun ,name (item a-list test test-not key)
     (eclipse:dolist (pair a-list nil)
       (eclipse:when (eclipse::satisfies-the-test item (,accessor pair)
						 test test-not key)
		     (eclipse:return pair)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                   VECTORS                                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro eclipse::with-simple-vector ((vector start end
				      &optional
				      (length-var (gensym "LENGTH"))
				      (offset-var (gensym "OFFSET"))
				      offset-start-var
				      offset-end-var)
			      &body body)
  (let* ((vector-var (eclipse:bindingform-name vector))
	 (vector (eclipse:bindingform-initform vector vector))
	 (start-var (eclipse:bindingform-name start))
	 (start (eclipse:bindingform-initform start start))
	 (end-var (eclipse:bindingform-name end))
	 (end (eclipse:bindingform-initform end end))
	 (offset-start-var (or offset-start-var start-var))
	 (offset-end-var (or offset-end-var end-var))
	 (cpl-var (gensym "CPL"))
	 (complex-class (class-var 'eclipse::complex-basic-vector))
	 (complexp (gensym "COMPLEXP")))
    `(let* ((,vector-var ,vector)
	    (,cpl-var (eclipse:class-precedence-list-of ,vector-var))
	    (,complexp (eclipse::find-list-eq ,complex-class ,cpl-var))
	    (,length-var (if ,complexp
			     (locally (declare (type eclipse::complex-basic-vector ,vector-var))
			       (eclipse::complex-array-fill-pointer ,vector-var))
			     (locally (declare (type eclipse::simple-basic-vector ,vector-var))
			       (eclipse::vector-size ,vector-var))))
	    (,start-var ,start)
	    (,end-var (eclipse::check-both-bounds ,start-var ,end ,length-var))
	    (,offset-var 0))
       (when ,complexp
	 (loop
	  (cond (,complexp
		 (setq ,offset-var (+ ,offset-var (eclipse::complex-array-offset ,vector-var))
		       ,vector-var (eclipse::simple-array-contents ,vector-var)
		       ,cpl-var (eclipse:class-precedence-list-of ,vector-var)
		       ,complexp (eclipse::find-list-eq ,complex-class ,cpl-var)))
		((eclipse::find-list-eq ,(class-var 'eclipse::simple-basic-vector) ,cpl-var)
		 (return))
		(t (return (setq ,vector-var (eclipse::simple-array-contents ,vector-var)))))))
       (let ((,offset-start-var (+ ,offset-var ,start-var))
	     (,offset-end-var (+ ,offset-var ,end-var)))
	 ,@body))))


;;; EXCL and CMU have broken file compilers that do not call
;;; make-load-form when they should.  Format uses subseq within code
;;; generation.  If that code contains Eclipse string literals, they
;;; won't compile in a EXCL or CMU.

(defmacro eclipse::safe-subseq (string start end)
  #+lisp-host `(host::string (eclipse:subseq ,string ,start ,end))
  #-lisp-host `(eclipse:subseq ,string ,start ,end))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                        CLOS                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; STATIC INITIALIZATIONS
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun static-location (name class)
    #+really (slot-definition-location
	      (find-slot-definition (find-type class)
				    name))
    #-really
    (or (ecase class
	  ((eclipse:class eclipse:standard-class
			  eclipse:standard-system-class
			  eclipse:method-combination-type
			  eclipse:metaobject eclipse:specializer eclipse:type
			  eclipse:interned-standard-system-class
			  eclipse:interned-standard-class)
	   (position (symbol-name name)
		     '#.(mapcar #'symbol-name
				'(dependents
				  documentation
				  name direct-methods
				  direct-subclasses direct-superclasses
				  class-precedence-list 
				  direct-slots effective-slots default-initargs
				  direct-default-initargs finalized-p prototype 
				  primary sealed abstract wrapper
				  direct-shared-slots initargs 
				  test instances eclipse:function declarations))
		     :test #'equal))
	  ((eclipse:method eclipse:standard-method eclipse:standard-accessor-method)
	   (position (symbol-name name)
		     '#.(mapcar #'symbol-name
				'(dependents
				  documentation
				  qualifiers lambda-list specializers
				  eclipse:function generic-function declarations  
				  keywords allow-other-keys-p slot-definition))
		     :test #'equal))
	  ((eclipse:generic-function eclipse:standard-generic-function
				     eclipse:standard-system-generic-function)
	   (position (symbol-name name)
		     '#.(mapcar #'symbol-name
				'(dependents
				  documentation
				  name methods method-class lambda-list
				  method-combination
				  argument-precedence-order declarations
				  gf-emf-table attached-methods
				  required-parameters keywords
				  allow-other-keys-p sealed))
		     :test #'equal))
	  ((eclipse:slot-definition eclipse:effective-slot-definition)
	   (position (symbol-name name)
		     '#.(mapcar #'symbol-name
				'(dependents
				  documentation
				  name allocation type initform initfunction
				  initargs location)) 
		     :test #'equal))
	  (eclipse:direct-slot-definition
	   (position (symbol-name name)
		     '#.(mapcar #'symbol-name
				'(dependents
				  documentation
				  name allocation type initform initfunction
				  initargs readers writers)) 
		     :test #'equal))
	  (eclipse:method-combination
	   (position (symbol-name name)
		     '#.(mapcar #'symbol-name
				'(dependents
				  documentation
				  eclipse:function options)) 
		     :test #'equal))
	  ((eclipse::packaged-function eclipse::interpreted-function
				       eclipse::macro-function)
	   (position (symbol-name name)
		     '#.(mapcar #'symbol-name
				'(lambda
				  env
				  name
				  eclipse:function)) 
		     :test #'equal)))
	(error "Could not determine location for ~s in ~s." name class))))


(defmacro static-access (object name class)
  `(,(if (member class '(eclipse:generic-function
			 eclipse:standard-generic-function))
	 'eclipse:funcallable-standard-instance-access
	 'eclipse:standard-instance-access)
    ,object ,(static-location name class)))


;;; Potential optimization:
;;; When instance is known at compile-time to be a
;;; standard-system-class or funcallable-standard-system-class, then
;;; this reduces to just (progn ,@body).
(defmacro eclipse::with-update ((instance class) &body body)
  `(let ((wrapper (eclipse::tagged-instance-wrapper ,instance)))
     (declare (type eclipse::wrapper wrapper))
     (when (eclipse::wrapper-invalid-p wrapper)
       (eclipse::update-instance ,instance wrapper ,class)) 
     ,@body))

;;; This assumes all the optimizations specified as comments below with-slots,
;;; plus it assumes that no slot is shared, and that instance-form is
;;; a PLACE which also happens to name the class.
(defmacro with-class-slots (slot-entries instance-form &body body)
  (let ((instance (gensym "INSTANCE"))
	(class (gensym "CLASS"))
	bindings)
    (loop for slot-entry in slot-entries
	  for var = (eclipse:bindingform-name slot-entry)
	  for name = (eclipse:bindingform-initform slot-entry slot-entry)
	  do (push `(,var (static-access ,instance ,name ,instance-form))
		   bindings))
    `(let* ((,instance ,instance-form)
	    (,class (class-of ,instance)))
       (eclipse::with-update (,instance ,class)
		    (symbol-macrolet ,bindings ,@body)))))

(defun fix-call-next (args methods body)
  (cond ((atom body) body)
	((equal body '(eclipse:call-next-method))
	 (values `(funcall (eclipse:method-function (car ,methods)) ,args
			   (rest ,methods))
		 t))
	((and (eq (first body) 'eclipse:apply)
	      (member (second body) '((eclipse:function eclipse:call-next-method)
				      (function eclipse:call-next-method))
		      :test #'equal))
	 (values `(funcall (eclipse:method-function (car ,methods))
			   (list* ,@(cddr body))
			   (rest ,methods))
		 t))
	((member (first body) '(ignorable quote eclipse::bq-quote))
	 (values body nil))
	(t (multiple-value-bind (a a-methodp)
	       (fix-call-next args methods (first body))
	     (multiple-value-bind (b b-methodp)
	       (fix-call-next args methods (rest body))
	       (values (cons a b)
		       (or a-methodp b-methodp)))))))

(defparameter *registry* nil)
(defun find-class1 (name) (getf *registry* name))
(defun (setf find-class1) (class name) (setf (getf *registry* name) class))


(defun make-specializer-forms1 (specializer-names)
  (cons 'list
	(loop for name in specializer-names
	      collect (class-var name))))

(defun make-direct-slot-form (name &key (initform nil initformp)
				   initarg reader writer
				   accessor (type t)
				   dependents documentation
				   (allocation :instance))
  `(eclipse::make-standard-instance-from-slots
    ,(class-var 'eclipse:standard-direct-slot-definition)
    ,(wrapper-var 'eclipse:standard-direct-slot-definition)
    ;; Order must be correct!
    (eclipse::make-static-slots
     ',dependents ',documentation ',name ',allocation ',type
     ',initform ,(when initformp `#'(lambda () ,initform))
     ',(when initarg (list initarg))
     ',(let ((r (or reader accessor))) (when r (list r)))
     ',(when (or writer accessor) (list (or writer `(eclipse:setf ,accessor)))))))

;;; Uses of this, combined with NTH, must use the correct order!
(defun slot-data (form) (rest (fourth form)))

(defun make-effective-slot-forms (superclass direct-slot-specs)
  (let* ((forms (when superclass
		  (copy-tree
		   (rest (static-access superclass
					eclipse::effective-slots 
					eclipse:standard-class)))))
	 (location (1- (length forms))))
    (dolist (spec direct-slot-specs)
      (destructuring-bind (name &key (initform nil initformp)
				initarg dependents documentation
				(allocation :instance) (type t) &allow-other-keys)
	  spec
	(let ((form (find name forms :key #'(lambda (form)
					      (second (nth 2 (slot-data form)))))))
	  (cond (form
		 (when initformp
		   (setf (nth 5 (slot-data form)) `',initform
			 (nth 6 (slot-data form)) `#'(lambda () ,initform)))
		 (when initarg
		   (push initarg (second (nth 7 (slot-data form))))))
		(t (setq forms
			 (nconc forms
				`((eclipse::make-standard-instance-from-slots
				   ,(class-var 'eclipse:standard-effective-slot-definition)
				   ,(wrapper-var 'eclipse:standard-effective-slot-definition)
				   ;; Order must be correct!
				   (eclipse::make-static-slots
				    ',dependents ',documentation ',name ',allocation ',type
				    ',initform ,(when initformp `#'(lambda () ,initform))
				    ',(when initarg (list initarg))
				    ,(incf location)))))))))))
    (cons 'list forms)))
	      

(defun get-initargs-from-specs (forms)
  (loop for spec in (rest forms)
	append (second (nth 7 (slot-data spec)))))


;;; Order must be correct!
(defmacro make-class-slots (&key dependents documentation name
				 direct-methods direct-subclasses
				 direct-superclasses cpl direct-slots
				 effective-slots default-initargs
				 direct-default-initargs
				 (finalized-p t) prototype primary
				 sealed abstract direct-shared-slots
				 initargs wrapper
				 (instances nil instancesp) test
				 (function nil functionp) declarations)
  `(eclipse::make-static-slots
    ,dependents ,documentation ,name ,direct-methods
    ,direct-subclasses ,direct-superclasses ,cpl
    ,direct-slots ,effective-slots ,default-initargs
    ,direct-default-initargs ,finalized-p ,prototype
    ,primary ,sealed ,abstract ,wrapper ,direct-shared-slots ,initargs
    ,@(when instancesp `(,test ,instances))
    ,@(when functionp `(,function ,declarations))))

;;#+debug
(defparameter host::*dummy*
  (let ((x (eclipse::make-standard-instance-from-slots
	    nil (eclipse::make-wrapper nil)
	    (host::make-class-slots :name 'eclipse::undefined-metaobject))))
    (eclipse::set-tagged-instance-class x x)
    x))

(defun make-prototype-form (name length)
  (case name
    (eclipse:character #\X)
    ((eclipse:integer eclipse:rational eclipse:real eclipse:number) 0)
    (eclipse:complex #c(0.0f0 0.0f0))
    ((eclipse:single-float eclipse:float) 0.0f0)
    (eclipse:double-float 0.0d0)
    (eclipse:ratio 1/2)
    ((eclipse:null eclipse:list eclipse:sequence eclipse:symbol
		   eclipse:t eclipse::built-in-class) nil)
    (eclipse:cons '(cons nil nil))

    ((eclipse:simple-bit-vector eclipse:bit-vector)
     '(eclipse::make-bit-vector 0))
    ((eclipse:simple-base-string eclipse:base-string
				 eclipse:simple-string eclipse:string)
     '(eclipse::make-base-char-vector 0))
    ((eclipse::simple-extended-string eclipse::extended-string)
     '(eclipse::make-extended-char-vector 0))
    ((eclipse:simple-vector eclipse::general-vector
			    eclipse::simple-basic-vector eclipse:vector)
     '(eclipse::make-general-vector 0))

    (eclipse::complex-bit-vector
     `(eclipse::make-complex-array ,(class-var 'eclipse::complex-bit-vector)
				   (eclipse:fixnum-index 1) '(0)
				   (eclipse::make-bit-vector 0)
				   0 0))
    ((eclipse::complex-base-string eclipse::complex-string)
     `(eclipse::make-complex-array ,(class-var 'eclipse::complex-base-string)
				   (eclipse:fixnum-index 1) '(0)
				   (eclipse::make-base-char-vector 0)
				   0 0))
    (eclipse::complex-extended-string
     `(eclipse::make-complex-array ,(class-var 'eclipse::complex-extended-string)
				   (eclipse:fixnum-index 1) '(0)
				   (eclipse::make-extended-char-vector 0)
				   0 0))
    ((eclipse::complex-vector eclipse::complex-basic-vector)
     `(eclipse::make-complex-array ,(class-var 'eclipse::complex-vector)
				   (eclipse:fixnum-index 1) '(0)
				   (eclipse::make-general-vector 0)
				   0 0))

    ((eclipse:simple-array eclipse:array)
     `(eclipse::make-simple-array (eclipse:fixnum-index 0) nil
				  (eclipse::make-general-vector 1)))
    (eclipse::complex-array 
     `(eclipse::make-complex-array ,(class-var 'eclipse::complex-array)
				   (eclipse:fixnum-index 0) nil
				   (eclipse::make-general-vector 1) 0 0))
     
    (t `(eclipse::make-standard-instance-from-slots
	     class wrapper (eclipse::make-slots ,length)))))
  
(defmacro eclipse::static-DEFCLASS (name direct-superclasses slots
					 &rest options)
  (let* ((metaclass (or (second (assoc :metaclass options))
			'eclipse::standard-class))
	 (super (car (last direct-superclasses)))
	 ;;; Not generally true, but true for us!
	 (cpl0 (append (mapcar #'find-class1
			       (butlast direct-superclasses))
		       (when super
			 (static-access
			  (find-class1 super)
			  eclipse::class-precedence-list
			  eclipse:standard-class))))
	 (direct-slots
	  `(list ,@(loop for spec in slots
			 collect (apply #'make-direct-slot-form
					spec))))
	 (effective-slots
	  (make-effective-slot-forms
	   ;; Fragile !!!
	   (if (member name '(eclipse:standard-direct-slot-definition
			      eclipse:standard-effective-slot-definition
			      eclipse::byte-compiled-function))
	       (second cpl0) (first cpl0))
	   slots))
	 (direct-default-initargs
	  (canonicalize-default-initargs
	   (rest (assoc :default-initargs options))))
	 ;; Not generally true, but true for us!
	 (default-initargs
	   (if (rest direct-default-initargs)
	       direct-default-initargs
	       (when cpl0
		 (static-access (first cpl0) eclipse::default-initargs
				eclipse:standard-class)))))
    (let ((class (eclipse::make-standard-instance-from-slots
		  *dummy* (eclipse::make-wrapper nil)
		  (make-class-slots
		   :default-initargs default-initargs
		   :name name :cpl cpl0 :direct-slots direct-slots
		   :effective-slots effective-slots))))
      (print name)
      (setf (find-class1 name) class)
      (push class (static-access
		   class eclipse::class-precedence-list
		   eclipse:standard-class)))
    `(let* ((meta ,(class-var metaclass))
	    (class ,(class-var name)) 
	    (supers ,(make-specializer-forms1 direct-superclasses))
	    (cpl1 (list ,@(loop for class in cpl0
				collect (class-var
					  (static-access
					     class eclipse::name
					     eclipse:standard-class)))))
	    (effective ,effective-slots)
	    (wrapper ,(wrapper-var name)))
       (eclipse::set-tagged-instance-class class meta)
       (eclipse::set-tagged-instance-wrapper class ,(wrapper-var metaclass))
       (eclipse::set-standard-instance-slots
	class (make-class-slots
	       :name ',name :direct-superclasses supers
	       :cpl (cons class cpl1)
	       :direct-slots ,direct-slots
	       :effective-slots effective
	       :direct-default-initargs ,direct-default-initargs
	       :default-initargs ,default-initargs
	       :prototype ,(make-prototype-form name (length (rest effective-slots)))
	       :wrapper wrapper
	       :initargs ',(get-initargs-from-specs effective-slots)
	       :primary ,(second (assoc :primary options))
	       :sealed ,(second (assoc :sealed options))
	       :abstract ,(second (assoc :abstract options))
	       ,@(case metaclass
		   #+not-used-any-more
		   (eclipse::interned-standard-system-class
		    `(:instances (make-hash-table :test 'eql)
				 :test 'eclipse:eql))
		   (eclipse::method-combination-type
		    `(:instances nil #+theoretically (make-hash-table :test 'equal)
				 :test 'eclipse:equal)))
	       ,@(when (eq metaclass 'eclipse::method-combination-type)
		   `(:function (eclipse::symbol-value-value 'eclipse::unbound-flag)
			       :declarations
			       (eclipse::symbol-value-value 'eclipse::unbound-flag)))))
       ,@(loop for super in direct-superclasses
	       collect `(push class
			      (static-access
			       ,(class-var super)
			       eclipse::direct-subclasses
			       eclipse:standard-class)))
       (setf (eclipse:find-type ',name) class)
       class)))


(defmacro eclipse::static-DEFGENERIC (name lambda-list &rest options)
  (let* ((gf-class (or (second (assoc :generic-function-class
				      options))
		       'eclipse:standard-generic-function))
	 (required-parameters
	  (subseq lambda-list 0
		  (position-if #'eclipse:lambda-list-keyword-p
			       lambda-list)))
	 (keys (member '&key lambda-list))
	 (keywords (eclipse:extract-keywords keys))
	 (allow-other-keys-p (or (null keys)
				 (find '&allow-other-keys keys)))
	 (argument-precedence-order required-parameters)
	 (dependents nil)
	 (documentation nil)
	 (declarations nil)
	 (attached-methods nil)
	 (methods nil) (sealed nil))
    `(let* ((meta (eclipse:find-type ',gf-class))
	    (gf (eclipse::make-funcallable-standard-instance-from-slots
		meta
		(static-access meta eclipse::wrapper eclipse:class)
		;; Order must be correct!
		(eclipse::make-static-slots
		 ,dependents ,documentation
		 ',name ,methods
		 (eclipse:find-type 'eclipse:standard-method)
		 ',lambda-list eclipse::*standard-method-combination*
		 ',argument-precedence-order
		 ',declarations
		 (eclipse::make-emf-table) ,attached-methods ',required-parameters
		 ',keywords ',allow-other-keys-p ',sealed))))
       (setf (eclipse:fdefinition ',name) gf)
       (eclipse:set-funcallable-instance-function
	gf (eclipse::standard-discriminating-function
	    gf ,(length required-parameters)))
       ',name)))

(defmacro eclipse::static-DEFMETHOD (name specialized-lambda-list
					  &body body &aux qualifiers)
  (unless (consp specialized-lambda-list)
    (setf qualifiers (list specialized-lambda-list)
	  specialized-lambda-list (pop body)))
  (multiple-value-bind (body call-next-p)
      (fix-call-next '.args. '.methods. body)
    (let* ((name1 (if (consp name)
		      (eclipse:make-name "SETF-~a" (second name))
		      name))
	   (lambda-list (eclipse:extract-lambda-list
			 specialized-lambda-list))
	   (specializer-names (eclipse:extract-specializer-names
			       specialized-lambda-list))
	   (specializers (make-specializer-forms1 specializer-names))
	   (function-name
	    (eclipse:make-name "~a~{-~a~}~{-~a~}" name1 qualifiers specializer-names))
	   (keys (member '&key lambda-list))
	   (keywords (eclipse:extract-keywords keys))
	   (allow-other-keys-p (find '&allow-other-keys keys))
	   (function `#',function-name)
	   (dependents nil)
	   (documentation nil)
	   (declarations nil))
      `(progn
	 (eclipse:defun ,function-name ,(if call-next-p
				    '(.args. .methods.)
				    '(.args. &rest ignore))
	   ,@(unless call-next-p
	       `((declare (ignore ignore) (dynamic-extent ignore))))
	   (macrolet ((eclipse:with-slots (&rest body)
			  `(with-class-slots ,@body)))
	     (block ,(if (consp name) (second name) name)
	       (eclipse:destructuring-bind ,(eclipse::make-method-lambda-list lambda-list)
		   .args.
		 (declare (ignorable ,@(eclipse::extract-specialized-names
					specialized-lambda-list)))
		 ,@body))))
	 (let* ((gf (eclipse:fdefinition ',name))
		(meta (eclipse:find-type 'eclipse:standard-method))
		(method (eclipse::make-standard-instance-from-slots
			 meta
			 (static-access meta eclipse::wrapper eclipse:class)
			 ;; Order must be correct!
			 (eclipse::make-static-slots
			  ,dependents ,documentation
			  ',qualifiers ',lambda-list
			  ,specializers ,function
			  gf ,declarations
			  ',keywords ',allow-other-keys-p))))
	   ,@(loop for specializer in specializer-names
		   collect `(push method
				  (static-access
				   (eclipse:find-type ',specializer)
				   eclipse::direct-methods eclipse:specializer)))
	   (push method
		 (static-access gf eclipse::methods eclipse:generic-function)))
	 nil))))

(defmacro eclipse::defstatic-accessors (class-name)
  (let (forms (class (eclipse:find-type class-name)))
    (dolist (slot (static-access
		   class eclipse::direct-slots eclipse:standard-class)) 
      (let* ((slot-name (static-access
			 slot eclipse::name eclipse:direct-slot-definition))
	     ;; Check for conflicts with the specials that name classes (ex. method-class)
	     (conflictp (boundp slot-name))
	     (key (if conflictp (gensym (symbol-name slot-name)) slot-name))
	     (binding (if conflictp (list key slot-name) slot-name)))
	(dolist (reader (static-access
			 slot eclipse::readers eclipse:direct-slot-definition))
	  (push `(eclipse::static-defmethod
		  ,reader ((,class-name ,class-name))
		  (eclipse:with-slots (,binding) ,class-name
				      (let ((val ,key))
					(if (eclipse::unboundp val)
					    (eclipse:slot-unbound
					     (eclipse:class-of ,class-name)
					     ,class-name ',slot-name)
					    val))))
		forms)
	  (push `(eclipse::static-defgeneric
		  ,reader (,class-name)
		  (:generic-function-class
		   eclipse:standard-system-generic-function))
		forms))
	(dolist (writer (static-access
			 slot eclipse::writers eclipse:direct-slot-definition))
	  (push `(eclipse::static-defmethod
		  ,writer (new (,class-name ,class-name))
		  (eclipse:with-slots (,binding) ,class-name
				      (eclipse:setf ,key new)))
		forms)
	  (push `(eclipse::static-defgeneric
		  ,writer (new ,class-name)
		  (:generic-function-class
		   eclipse:standard-system-generic-function))
		forms))))
    `(progn ,@forms)))



;;; CLOS INITARG CHECKING
;;; Unless :allow-other-keys is specified, this collects up the names
;;; and argument lists of the functions which will have applicable
;;; methods considered.  It passes this and the class initargs to
;;; check-initargs-1. 
(defmacro eclipse::check-initargs (class initargs &rest call-lists)
  `(eclipse:when (eclipse:and ,initargs (not (getf ,initargs :allow-other-keys)))
     (eclipse::check-initargs-1
      ,class ,initargs (eclipse::class-initargs ,class)
      ,@(loop for call in call-lists
	      collect `(list* ',(first call) ,@(rest call))))))
