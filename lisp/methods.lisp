#| *****************************************************************
For class-meth, methods, gfunc, method-init:
 
Some of the definitions involving method construction
(and to some extent, effective method construction) are not needed to
USE CLOS at runtime, but are used at compile time.

Others, such as those involved with defining or redefining objects, or
the various maintance protocols, are needed only when defining new
classes or methods.

Such operators could be moved to the clos-define or clos-compile
modules. 
***************************************************************** |#
(macrolet ((defgeneric (&rest args)
	     `(static-defgeneric ,@args))
	   (defmethod (&rest args)
	     `(static-defmethod ,@args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                        METHODS                               ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SPECIALIZED ACCESSORS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric FUNCTION-KEYWORDS (method))
(defmethod function-keywords ((standard-method standard-method))
  (with-slots (keywords allow-other-keys-p) standard-method
    (values keywords allow-other-keys-p)))

;;; MAINTENANCE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric FIND-METHOD (generic-function
			 method-qualifiers specializers
			 &optional (errorp t)))
(defmethod FIND-METHOD ((generic-function standard-generic-function)
			method-qualifiers specializers
			&optional (errorp t))
  (unless (= (length specializers)
	     (length
	      (generic-function-required-parameters generic-function)))
    (error "Specializers ~s does not match required arguments for ~s."
	   specializers generic-function))
  (when (find-if #'consp specializers)
    (setq specializers
	  (loop for specializer in specializers
		collect (if (car-eq specializer 'eql)
			    (intern-eql-specializer (second specializer))
			    specializer))))
  (get-method generic-function method-qualifiers specializers errorp))
					  

;;; The methods on class for the following two generic functions may
;;; be overridden if specializer-direct-generic-functions and
;;; specializer-direct-methods is overridden as well.
;;; These methods were consolidated from specified methods
;;; on class and eql-specializer.

(defgeneric ADD-DIRECT-METHOD (specializer method))
(defmethod add-direct-method ((specializer specializer) (method method))
  (with-slots (direct-methods) specializer
    (pushnew method direct-methods)))

(defgeneric REMOVE-DIRECT-METHOD (specializer method))
(defmethod remove-direct-method ((specializer specializer) (method method))
  (with-slots (direct-methods) specializer
    (setq direct-methods (remove method direct-methods))))

;;; Allowed to signal sealing violation error.

(defgeneric ADD-METHOD (generic-function method))
(defmethod add-method ((standard-generic-function standard-generic-function)
		       (method standard-method))
  (with-slots ((method-gf generic-function)) method
    (with-slots (methods allow-other-keys-p) standard-generic-function
      (let* ((qualifiers (method-qualifiers method))
	     (specializers (method-specializers method))
	     (lambda-list (method-lambda-list method))
	     (eql-specializer-p nil)
	     (existing (get-method standard-generic-function qualifiers
				   specializers nil)))
	(cond ((slot-boundp standard-generic-function 'lambda-list)
	       (check-congruent-lambda standard-generic-function method)
	       (when (and (find-list-eq '&key lambda-list)
			  allow-other-keys-p
			  (not (find-list-eq '&allow-other-keys
				     (generic-function-lambda-list
				      standard-generic-function))))
		 (setf allow-other-keys-p nil)))
	      (t (set-gf-lambda-list
		  standard-generic-function
		  (extract-generic-function-lambda-list lambda-list)
		  nil)))
	(when method-gf
	  (error "~s is already associated with ~s." method method-gf))
	(when existing
	  (remove-method standard-generic-function existing))
	(dolist (specializer specializers)
	  (when (eql-specializer-p specializer) (setq eql-specializer-p t))
	  (add-direct-method specializer method))
	;; Add method to generic-function-methods.  By placing methods
	;; with eql specializers first, we make
	;; compute-applicable-methods-using-classes return nil, nil
	;; faster.
	(let ((current methods))
	  (setf methods
		(if (and current (not eql-specializer-p)
			 ;; FIND-IF is defined using CLOS
			 (loop for spec in (method-specializers (car current))
			       when (eql-specializer-p spec) return t))
		    (append current (list method))
		    (cons method current))))
	(setf method-gf standard-generic-function))))
  (finalize-generic-function standard-generic-function)
  (map-dependents standard-generic-function
		  #'(lambda (dep)
		      (update-dependent standard-generic-function dep
					'add-method method)))
  standard-generic-function)


;;; Should signal sealed-object-error if generic-function is sealed or
;;; if method is in an intert domain!!!

(defgeneric REMOVE-METHOD (generic-function method))
(defmethod remove-method ((generic-function standard-generic-function)
			  (method standard-method))
  (with-slots (methods) generic-function
    (setf methods (remove method methods)))
  (with-slots ((method-gf generic-function) specializers) method
    (setf method-gf nil)
    (dolist (specializer specializers)
      (remove-direct-method specializer method)))
  (finalize-generic-function generic-function)
  (map-dependents generic-function
		  #'(lambda (dep)
		      (update-dependent generic-function dep
					'remove-method method)))
  generic-function)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;             INITIALIZATION OF METHOD METAOBJECTS             ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; IWBNI this did a lexical analysis and told us whether
;;; next-method-p was used.  If not, then we could return
;;; :no-next-method-p t in the second value plist.  This could be used
;;; to do the "direct single method" optimization.  Alternatively,
;;; IWBNI we could design a protocol by which CALL-NEXT-METHOD and
;;; NEXT-METHOD-P could be automatically coordinated with CALL-METHOD
;;; and make-effective-method.

(defgeneric MAKE-METHOD-LAMBDA (generic-function method lambda-epxression
						 environment))


(defmethod REINITIALIZE-INSTANCE ((instance method) &key)
  (illegal-initialization "reinitalize" instance))

;;; These can't be called by user code.
(defmethod INITIALIZE-INSTANCE ((standard-method standard-method) &key
				lambda-list)
  (with-slots (keywords allow-other-keys-p) standard-method
    (setf allow-other-keys-p (find-list-eq '&allow-other-keys lambda-list)
	  keywords (extract-keywords lambda-list)))
  (call-next-method))
    

;;; Potential optimization:  After finalization, accessor methods 
;;; can be defined more efficiently iff either:
;;;  A. The slot is primary. We don't have to worry about
;;;     redefinitions changing the slot properties because this will
;;;     remove old accessor methods. 
;;;  B. The location of the slot is the same in each subclass and either:
;;;     1. The class and each subclass is sealed, or
;;;     2. We make arrangements such that adding or redefining a
;;;     (direct or indirect) subclass with the same slot in a
;;;     different location causes this method to be undo the optimization.
;;;  C. The class is sealed and we automatically add accessor methods
;;;      for each subclass. To make this work, we would have to
;;;      automatically remove those accessor methods when this
;;;      accessor was removed.
;;; The :function closure can either:
;;;  A. Continue to use (setf) slot-value-using-class, but close
;;;     over an effective-slot-definition computed at finalization, or
;;;  B. If the applicable (setf) slot-value-using-class is sealed and
;;;     the only applicable method is the standard one, then we can
;;;     use the instance-access-protocol directly. 

(defmethod INITIALIZE-INSTANCE ((instance standard-reader-method)
				&rest initargs
				&key slot-definition)
  (declare (dynamic-extent initargs))
  (let ((name (slot-definition-name slot-definition)))
    (apply #'call-next-method instance
	   :lambda-list '(instance)
	   :function 
	   #'(lambda (args &rest ignore)
			 (declare (ignore ignore) (dynamic-extent ignore))
			 (destructuring-bind (instance) args
			   (let ((class (class-of instance)))
			     (slot-value-using-class
			      class instance
			      (find-slot-definition class name)))))
	   initargs)))
		       
(defmethod INITIALIZE-INSTANCE ((instance standard-writer-method)
				&rest initargs
				&key slot-definition)
  (declare (dynamic-extent initargs))
  (let ((name (slot-definition-name slot-definition)))
    (apply #'call-next-method instance
	   :lambda-list '(new-value instance) 
	   :function #'(lambda (args &rest ignore)
			 (declare (ignore ignore) (dynamic-extent ignore))
			 (destructuring-bind (val instance) args
			   (let ((class (class-of instance)))
			     (setf (slot-value-using-class
				    class instance
				    (find-slot-definition class name))
				   val))))
	   initargs))) 


)
