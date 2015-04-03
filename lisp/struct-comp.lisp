
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CONSTRUCTOR BUILDERS                                         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   

(defun make-constructor (class constructor-name lambda-list args)
  (with-slots (name type) class
    `(defun ,constructor-name ,lambda-list
       ,(case type
	  (structure-object `(make-structure ',name ,@args))
	  (list `(list ,@args))
	  (t `(vectorize ',type ,@args))))))

;;; This not only finds the variable name from the lambda list, but it
;;; SIDE-EFFECTS the lambda-list to use the initform from the
;;; slot-definition rather than nil.
(defun find-lambda-var (slot lambda-list
			&aux (name (slot-definition-name slot)))
  (do ((parameters lambda-list (cdr parameters))
       (optionalp nil))
      ((null parameters) nil)
    (let ((parameter (car parameters)))
      (case parameter
	((&rest &aux &allow-other-keys) (setq optionalp nil))
	((&optional &key) (setq optionalp t))
	(t (let ((key (if (consp parameter)
			  (let ((keyword (car parameter)))
			    (if (consp keyword)
				(second keyword)
			      keyword))
			parameter)))
	     (when (eq key name)
	       (when (and optionalp
			  (or (not (consp parameter))
			      (not (cdr parameter))));i.e. (foo) or ((:why y))
		 (rplaca parameters
			 `(,(if (consp parameter) (car parameter) parameter)
			      ,(slot-definition-initform slot))))
	       (return key))))))))

;;; N.B. Both the following rely on finding effective-slots to be in
;;; reverse order!
(defun make-boa-constructor (class constructor-name lambda slots
			     &aux (args nil)
			     (lambda-list (copy-tree lambda)))
  (dolist (slot slots)
    (push (or (unless (slot-definition-hidden slot)
		(find-lambda-var slot lambda-list))
	      (slot-definition-initform slot))
	  args))
  (make-constructor class constructor-name lambda-list args))

(defun make-keyword-constructor (class constructor-name slots
				 &aux (lambda-list nil) (args nil))
  ;; Use uninterned symbols for parameters to avoid potental clash
  ;; with defconstants. 
  (dolist (slot slots)
    (if (slot-definition-hidden slot)
	(push (slot-definition-initform slot) args)
      (let* ((name (slot-definition-name slot))
	     (var (make-symbol (symbol-name name))))
	(push var args)
	(push `((,(make-keyword name) ,var)
		,(slot-definition-initform slot)) lambda-list))))
  (make-constructor class constructor-name
		    (cons '&key lambda-list) args))

(defun generate-defstruct-constructors (class constructors effective-slots)
  (let ((functions nil))
    (dolist (constructor constructors functions)
      (destructuring-bind (name &optional (lambda nil lambdap))
	  constructor
	(when name
	  (push
	   (if lambdap
	       (make-boa-constructor class name lambda effective-slots)
	       (make-keyword-constructor class name effective-slots))
	   functions))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; OTHER FUNCTION FORM GENERATORS                               ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
(defun generate-defstruct-accessors (conc-name effective-slots type)
  (let ((accessors nil)
	(prefix conc-name)
	(struct `(the ,type structure)))
    (dolist (slot effective-slots accessors)
      (unless (slot-definition-hidden slot)
	(with-slots ((slot-name name)
		     (slot-type type)
		     location read-only) slot
	  (let* ((form (case type
			 (structure-object
			  `(structref ,struct ,location))
			 (list `(nth ,location ,struct))
			 (t `(elt ,struct ,location))))
		 (name (if prefix
			   (make-name "~a~a" prefix slot-name)
			   slot-name))
		 (accessor (if (eq slot-type 't) form
			       `(the ,slot-type ,form))))
	    (unless read-only
	      (push `(defun (setf ,name) (new structure)
		       (setf ,accessor new))
		    accessors))
	    (push `(defun ,name (structure) ,accessor) accessors)))))))

(defun generate-defstruct-functions (class print-object class-var)
  (with-slots (predicate copier constructors
			 type type-name-index abstract
			 effective-slots name conc-name) 
      class
    (let ((slot-definitions (reverse effective-slots)))
      `(,@(when predicate
	    `((defun ,predicate (object)
		,(case type
		   (structure-object `(classp object ,class-var))
		   (list `(when (listp object)
			    (eq (nth ,type-name-index object)
				',name)))
		   (t `(when (typep object ',type)
			 (and (= (length object) ,(length effective-slots))
			      (eq (elt object ,type-name-index)
				  ',name))))))))
	  ,@(when copier
	      `((defun ,copier (struct)
		  ,(if abstract
		       `(copy-seq struct)
		       `(copy-structure struct)))))
	  ,@(when print-object
	      (let ((object (gensym "OBJECT"))
		    (stream (gensym "STREAM")))
		`((defmethod print-object ((,object ,name) ,stream)
		    (funcall ,print-object ,object ,stream)))))
	  ,@(generate-defstruct-constructors class constructors slot-definitions)
	  ,@(generate-defstruct-accessors
	     conc-name slot-definitions type)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;               OPTION PARSING                                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
(defun canonicalize-defstruct-slot-specification (spec)
  (if (symbolp spec)
      `(:name ,spec)
      (destructuring-bind (name &optional initform
				&key (type t) read-only)
	  spec
	`(:name ,name :initform ,initform
		:type ,type :read-only ,read-only))))

(defun canonicalize-defstruct-slot-specifications (specs)
  (mapcar #'canonicalize-defstruct-slot-specification specs))

(defparameter *structure-options* (make-hash-table))
(defun add-defstruct-option (key func)
  (setf (gethash key *structure-options*) func))
(defun defstruct-option (key)
  (or (gethash key *structure-options*)
      (Signal-program-error "Unrecognized ~s option for defstruct." key)))

(defun simple-defstruct-option (default)
  #'(lambda (structure-name name tail)
      (destructuring-bind (&optional (val (when default
					    (make-name default structure-name))))
	  tail
	`(,name ,val))))

(add-defstruct-option :conc-name (simple-defstruct-option nil))
(add-defstruct-option :copier (simple-defstruct-option "COPY-~A"))
(add-defstruct-option :predicate (simple-defstruct-option "~A-P"))
(add-defstruct-option
 :type #'(lambda (structure-name name tail)
	   (declare (ignore structure-name name))
	   (destructuring-bind (&optional (val 'structure-object)) tail
	     `(:type ,val))))
(add-defstruct-option
 :initial-offset #'(lambda (structure-name name tail)
		     (declare (ignore structure-name name))
		     (destructuring-bind (&optional (val 0)) tail
		       `(:initial-offset ,val))))
(add-defstruct-option :named #'(lambda (structure-name name tail)
				 (declare (ignore structure-name name tail))
				 '(:named t)))
(add-defstruct-option
 :print-object #'(lambda (structure-name name tail)
		   (declare (ignore structure-name name))
		   (destructuring-bind (&optional (func 'print-structure))
		       tail
		     `(:print-object #',func))))

(add-defstruct-option
 :print-function #'(lambda (structure-name name tail)
		     (declare (ignore structure-name name))
		     (destructuring-bind (&optional (func nil funcp))
			 tail
		       `(:print-object
			 ,(if funcp
			      `(level-structure-printer #',func)
			      '#'print-structure)))))

(add-defstruct-option
 :include #'(lambda (structure-name name tail)
	      (declare (ignore structure-name name))
	      (destructuring-bind (&optional (super 'structure-object)
					     &rest slots) tail
		`(:direct-superclasses
		  (,super) :inherited-slots
		  ,(canonicalize-defstruct-slot-specifications slots)))))

;;; :constructor is handled specially.
(defun canonicalize-defstruct-options (structure-name options)
  (loop with seen = nil and constructors = nil
	for opt in options
	for name = (bindingform-name opt)
	if (eq name :constructor)
	do (push (or (when (consp opt) (rest opt))
		     (list (make-standard-structure-constructor-name
			    structure-name)))
		 constructors)
	else append
	(let* ((result-pair (funcall (defstruct-option name)
				     structure-name name
				     (when (consp opt) (rest opt))))
	       (real-name (car result-pair)))
	  (if (member real-name seen)
	      (eclipse::multiple-appearance-error real-name "options")
	      (push real-name seen))
	  result-pair) into initargs
	finally (return `(:constructors
			  ,(or constructors
			       `((,(make-standard-structure-constructor-name
				    structure-name))))
			  ,@initargs))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                    DEFSTRUCT                                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro DEFSTRUCT (options &body slots)
  (let ((name (bindingform-name options))
	(options (when (consp options) (rest options))))
    (let* ((opts (append (canonicalize-defstruct-options name options)
			 (when (stringp (first slots))
			   `(:documentation ,(pop slots)))
			 `(:direct-slots
			   ,(canonicalize-defstruct-slot-specifications
			     slots))))
	   (printer (prog1 (getf opts :print-object)
		      (remf opts :print-object)))
	   (class (apply #'ensure-structure name opts))
	   (class-var (gensym name)))
      `(let ((,class-var (apply #'ensure-structure ',name ',opts)))
	 (declare (ignorable ,class-var))
	 ,@(generate-defstruct-functions class printer class-var)
	 ',name))))

#+example	
(defstruct (foo )
	    a (b 2) (c 3 :type fixnum :read-only t))
