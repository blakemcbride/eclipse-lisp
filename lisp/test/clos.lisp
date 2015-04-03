;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLASSES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DEFCLASS !!!
;;; FIND-CLASS !!!
;;; CLASS-NAME, (SETF CLASS-NAME)  !!!

;;; CLASS-OF
(deftest class-of-symbol (eq (class-of 'fred) (find-class 'symbol)) t)
(deftest class-of-ratio (eq (class-of 2/3) (find-class 'ratio)) t)
(defclass cobook () ())
(deftest class-of-book
  (eq (class-of (make-instance 'cobook)) (find-class 'cobook)) t)
(defclass conovel (cobook) ())
(deftest class-of-novel
  (eq (class-of (make-instance 'conovel)) (find-class 'conovel)) t)

;;;; SLOT DEFINITION ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SLOT INITFORM ENVIRONMENT
(let ((x 3))
  (defun set-x1-binding (new) (setq x new))
  (defclass slot-environment-example ()
    ((slot :initform x))))
(let ((x 5))
  (defun set-x2-binding (new) (setq x new))
  ;; CMUCL BUG: define a closure over a variable which is written to,
  ;; but not read.  Write the variable.  You're screwed.
  #+cmu (defun report-x2-binding () x)
  (defclass slot-environment-example2 (slot-environment-example)
    (slot)))

(deftest slot-environment
  (progn (set-x1-binding 4)
	 (set-x2-binding 6)
	 (slot-value (make-instance 'slot-environment-example2) 'slot))
  4)

;;; SLOT ALLOCATION
(defclass slot-allocation-example0 ()
  ((local :allocation :class		;overriden
	  :accessor allocation-example-local)
   (shared :allocation :instance	;overridden
	   :accessor allocation-example-shared)
   (default :allocation :instance	;overridden
     :accessor allocation-example-default)))
  
(defclass slot-allocation-example (slot-allocation-example0)
  ((local :allocation :instance)
   (shared :allocation :class)
   (default)))

(deftest slot-allocation-local
  (let ((ex1 (make-instance 'slot-allocation-example))
	(ex2 (make-instance 'slot-allocation-example)))
    (setf (allocation-example-local ex1) 1)
    (setf (allocation-example-local ex2) 2)
    (allocation-example-local ex1))
  1)
(deftest slot-allocation-default
  (let ((ex1 (make-instance 'slot-allocation-example))
	(ex2 (make-instance 'slot-allocation-example)))
    (setf (allocation-example-default ex1) 1)
    (setf (allocation-example-default ex2) 2)
    (allocation-example-default ex1))
  1)
(deftest slot-allocation-shared
  (let ((ex1 (make-instance 'slot-allocation-example))
	(ex2 (make-instance 'slot-allocation-example)))
    (setf (allocation-example-shared ex1) 1)
    (setf (allocation-example-shared ex2) 2)
    (allocation-example-shared ex1))
  2)

;;;; SLOT ACCESS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass slot-example ()
  ((slot :initform 3)
   (trapped-unbound-slot)))

(defmethod slot-unbound (class (instance slot-example)
			 (name (eql 'trapped-unbound-slot)))
  (declare (ignore class))
  (values 42 'never-seen))
(defmethod slot-missing (class (instance slot-example)
			 (slot-name (eql 'trapped-missing-slot))
			 operation &optional new-value)
  (declare (ignore class))
  (values operation instance new-value))
;;; SLOT-UNBOUND
(deftest slot-unbound-trapped
  (slot-value (make-instance 'slot-example) 'trapped-unbound-slot)
  42)					;note: just one value
;;; SLOT-MAKUNBOUND, SLOT-BOUNDP, SLOT-UNBOUND default,
;;; UNBOUND-SLOT-INSTANCE, CELL-ERROR-NAME
(deftest slot-makunbound
  (let ((ex (make-instance 'slot-example)))
    (and (= (slot-value ex 'slot) 3)
	 (eq ex (slot-makunbound ex 'slot))
	 (not (slot-boundp ex 'slot))
	 (handler-case (slot-value ex 'slot)
	   (unbound-slot (c)
             (when (eq ex (unbound-slot-instance c))
	       (cell-error-name c))))))
  slot)
;;; SLOT-EXISTS-P
(deftest slot-exists-p-true
  (slot-exists-p (make-instance 'slot-example) 'slot)
  t)
(deftest slot-exists-p-false
  (slot-exists-p (make-instance 'slot-example) 'undefined)
  nil)
;;; SLOT-MISSING
(deftest slot-missing-trapped
  (slot-value (make-instance 'slot-example) 'trapped-missing-slot)
  slot-value)

;;; SLOT-VALUE
(defclass svfoo () 
  ((a :accessor svfoo-a :initarg :a :initform 1)
   (b :accessor svfoo-b :initarg :b)
   (c :accessor svfoo-c :initform 3)))
(defmethod svfoo-method ((x svfoo))
  (slot-value x 'a))

(deftest SLOT-VALUE
 (let ((svfoo1 (make-instance 'svfoo :a 'one :b 'two)))
   (values 
    (slot-value svfoo1 'a)
    (slot-value svfoo1 'b)
    (slot-value svfoo1 'c)
    (setf (slot-value svfoo1 'a) 'uno)
    (slot-value svfoo1 'a)
    (svfoo-method svfoo1)))
 ONE TWO 3 UNO UNO UNO)

(defclass wasthing ()
  ((x :initarg :x :accessor wasthing-x)
   (y :initarg :y :accessor wasthing-y)))
(defparameter *with-accessors-accesses* nil)
(defmethod (setf wasthing-x) :before (new-x (wasthing wasthing))
  (push (list (wasthing-x wasthing) new-x) *with-accessors-accesses*))

;;; WITH-ACCESSORS
(deftest with-accessors
  (let ((wasthing1 (make-instance 'wasthing :x 1 :y 2))
	(wasthing2 (make-instance 'wasthing :x 7 :y 8))
	(*with-accessors-accesses* nil))
    (with-accessors ((x1 wasthing-x) (y1 wasthing-y))
	wasthing1
      (with-accessors ((x2 wasthing-x) (y2 wasthing-y))
	  wasthing2
	(list (list x1 (wasthing-x wasthing1) y1 (wasthing-y wasthing1)
		    x2 (wasthing-x wasthing2) y2 (wasthing-y wasthing2))
	      (setq x1 (+ y1 x2))
	      (list x1 (wasthing-x wasthing1) y1 (wasthing-y wasthing1)
		    x2 (wasthing-x wasthing2) y2 (wasthing-y wasthing2))
	      (setf (wasthing-x wasthing2) (list x1))
	      (list x1 (wasthing-x wasthing1) y1 (wasthing-y wasthing1)
		    x2 (wasthing-x wasthing2) y2 (wasthing-y wasthing2))
	      (reverse *with-accessors-accesses*)))))
  ((1 1 2 2 7 7 8 8)
   9
   (9 9 2 2 7 7 8 8) 
   (9)
   (9 9 2 2 (9) (9) 8 8)
   ((1 9) (7 (9)))))

;;; WITH-SLOTS
(deftest with-slots1
  (let ((thing (make-instance 'wasthing :x 0 :y 1)))
    (values (with-slots (x y) thing (incf x) (incf y))
	    (list (wasthing-x thing) (wasthing-y thing))))
  2 (1 2))

(deftest with-slots
  (let ((thing1 (make-instance 'wasthing :x 1 :y 2))
	(thing2 (make-instance 'wasthing :x 7 :y 8))
	(*with-accessors-accesses* nil))
    (with-slots ((x1 x) (y1 y))
	thing1
      (with-slots ((x2 x) (y2 y))
	  thing2
	(list (list x1 (wasthing-x thing1) y1 (wasthing-y thing1)
		    x2 (wasthing-x thing2) y2 (wasthing-y thing2))
	      (setq x1 (+ y1 x2))
	      (list x1 (wasthing-x thing1) y1 (wasthing-y thing1)
		    x2 (wasthing-x thing2) y2 (wasthing-y thing2))
	      (setf (wasthing-x thing2) (list x1))
	      (list x1 (wasthing-x thing1) y1 (wasthing-y thing1)
		    x2 (wasthing-x thing2) y2 (wasthing-y thing2))
	      (reverse *with-accessors-accesses*))))) 
  ((1 1 2 2 7 7 8 8)
   9
   (9 9 2 2 7 7 8 8) 
   (9)
   (9 9 2 2 (9) (9) 8 8)
   ((7 (9)))))

;;; READERS, WRITERS and ACCESSORS
;;; Note order requires forward references.
#-(and cmu (not eclipse))
(progn
  (defclass pastry (cinnamon apple) ((y :initform 1)))
  (defclass pie (apple cinnamon) ((x :initform 1)))
  (defclass apple (fruit) ((x :initform 2)
			   (x2 :initform 2)
			   (y :initform 4)
			   (y2 :initform 4)
			   (y3 :initform 4)
			   (y4 :initform 4)))
  (defclass cinnamon (spice) ((x :initform 4)
			      (x2 :initform 4)
			      (x3 :initform 4)
			      (x4 :initform 4)
			      (y :initform 2)
			      (y2 :initform 2)))
  (defclass fruit (food) ((x :initform 3)
			  (x2 :initform 3)
			  (x3 :initform 3)
			  (y :initform 5)
			  (y2 :initform 5)
			  (y3 :initform 5)
			  (y4 :initform 5)
			  (y5 :initform 5)))
  (defclass spice (food) ((x :initform 5)
			  (x2 :initform 5)
			  (x3 :initform 5)
			  (x4 :initform 5)
			  (x5 :initform 5)
			  (y :initform 3)
			  (y2 :initform 3)
			  (y3 :initform 3))))
(defclass food () ((x :initform 6)
		   (x2 :initform 6)
		   (x3 :initform 6)
		   (x4 :initform 6)
		   (x5 :initform 6)
		   (x6 :initform 6)
		   (y :initform 6)
		   (y2 :initform 6)
		   (y3 :initform 6)
		   (y4 :initform 6)
		   (y5 :initform 6)
		   (y6 :initform 6)))

#+(and cmu (not eclipse))		;cmu can't handle forward references
(progn
  (defclass fruit (food) ((x :initform 3)
			  (x2 :initform 3)
			  (x3 :initform 3)
			  (y :initform 5)
			  (y2 :initform 5)
			  (y3 :initform 5)
			  (y4 :initform 5)
			  (y5 :initform 5)))
  (defclass spice (food) ((x :initform 5)
			  (x2 :initform 5)
			  (x3 :initform 5)
			  (x4 :initform 5)
			  (x5 :initform 5)
			  (y :initform 3)
			  (y2 :initform 3)
			  (y3 :initform 3)))
  (defclass apple (fruit) ((x :initform 2)
			   (x2 :initform 2)
			   (y :initform 4)
			   (y2 :initform 4)
			   (y3 :initform 4)
			   (y4 :initform 4)))
  (defclass cinnamon (spice) ((x :initform 4)
			      (x2 :initform 4)
			      (x3 :initform 4)
			      (x4 :initform 4)
			      (y :initform 2)
			      (y2 :initform 2)))
  (defclass pastry (cinnamon apple) ((y :initform 1)))
  (defclass pie (apple cinnamon) ((x :initform 1))))

(deftest pie-x (slot-value (make-instance 'pie) 'x) 1)
(deftest pie-x2 (slot-value (make-instance 'pie) 'x2) 2)
(deftest pie-x3 (slot-value (make-instance 'pie) 'x3) 3)
(deftest pie-x4 (slot-value (make-instance 'pie) 'x4) 4)
(deftest pie-x5 (slot-value (make-instance 'pie) 'x5) 5)
(deftest pie-x6 (slot-value (make-instance 'pie) 'x6) 6)

(deftest pastry-y (slot-value (make-instance 'pastry) 'y) 1)
(deftest pastry-y2 (slot-value (make-instance 'pastry) 'y2) 2)
(deftest pastry-y3 (slot-value (make-instance 'pastry) 'y3) 3)
(deftest pastry-y4 (slot-value (make-instance 'pastry) 'y4) 4)
(deftest pastry-y5 (slot-value (make-instance 'pastry) 'y5) 5)
(deftest pastry-y6 (slot-value (make-instance 'pastry) 'y6) 6)



(defclass accessor-dummy ()
  ((unused-slot)))
(defclass accessor-example0 ()
  ((x :reader ae-x :initform 1)
   (y :writer ae-y)
   (z :accessor ae-z)))
(defclass accessor-example (accessor-dummy accessor-example0)
  ((another-unused-slot)
   (x :reader ae-x2)
   (y :writer ae-y2)
   (z :accessor ae-z2)))
(defparameter *accessor-example* (make-instance 'accessor-example))
(deftest slot-reader-name
  (and (fboundp 'ae-x) (not (fboundp '(setf ae-x)))
       (= (ae-x *accessor-example*)
	  (ae-x2 *accessor-example*)
	  1))
  t)
(deftest slot-writer-name
  (and (fboundp 'ae-y) (not (fboundp '(setf ae-y)))
       (= (ae-y 2 *accessor-example*)
	  (slot-value *accessor-example* 'y)
	  2)
       (= (ae-y2 22 *accessor-example*)
	  (slot-value *accessor-example* 'y)
	  22))
  t)
(deftest slot-accessor-name
  (and (= (setf (ae-z *accessor-example*) 3)
	  (ae-z2 *accessor-example*)
	  3)
       (= (setf (ae-z2 *accessor-example*) 33)
	  (ae-z *accessor-example*)
	  33))
  t)

;;;; INITIALIZATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;; MAKE-INSTANCE 
(defclass miq () ((x :initarg a)))
(defclass mir (miq) ((x :initarg b))
  (:default-initargs a 1 b 2))

(deftest slot-initialization0 (slot-value (make-instance 'mir) 'x) 1) 
(deftest slot-initialization1 (slot-value (make-instance (find-class 'mir) 'a 3) 'x) 3)
(deftest slot-initialization2 (slot-value (make-instance 'mir 'b 4) 'x) 4)
(deftest slot-initialization3 (slot-value (make-instance 'mir 'a 1 'a 2) 'x) 1)

(defparameter *dia-evalled* nil)
(defclass dia ()
  ((x :initarg :x))
  (:default-initargs :x (setq *dia-evalled* t)))
(deftest dia-evaluation
  (let ((*dia-evalled* nil))
    (values (slot-value (make-instance 'dia :x 2) 'x)
	    *dia-evalled*
	    (slot-value (make-instance 'dia) 'x)
	    *dia-evalled*))
  2 nil t t)

#-(and cmu (not eclipse))
(progn
  (defclass no-class (standard-class) ())
  (defclass make-instance-dummy ()
    ((x :initarg a :initarg b))
    (:metaclass no-class))
  (defmethod make-instance ((class no-class) &rest initargs)
    initargs)
  (deftest make-instance
    (make-instance 'make-instance-dummy 'a 1 'b 2)
    (a 1 b 2)))
  

;;; ALLOCATE-INSTANCE !!!
;;; SHARED-INITIALIZE !!!
;;; INITIALIZE-INSTANCE !!!
;;; REINITIALIZE-INSTANCE !!!
;;; MAKE-INSTANCES-OBSOLETE !!!

;;; CHANGE-CLASS
;;; UPDATE-INSTANCE-FOR-DIFFERENT-CLASS !!!
;;; UPDATE-INSTANCE-FOR-REDEFINED-CLASS
;;; N.B.: Allegro claims that defining a type named position is in
;;; violation of the ANSI prohibition on defining anything, including
;;; type, for a symbol in the CL package.  This might be defensible.
;;; Thus we have modified the ANSI examples to use base-position
;;; rather than position.
(defclass base-position () ())
(defclass x-y-position (base-position)
  ((x :initform 0 :accessor position-x :initarg x)
   (y :initform 0 :accessor position-y :initarg y)))
(defclass rho-theta-position (base-position)
     ((rho :initform 0 :accessor position-rho)
      (theta :initform 0 :accessor position-theta)))
  
(defmethod update-instance-for-different-class :before ((old x-y-position) 
							(new rho-theta-position)
							&key)
  (let ((x (slot-value old 'x))
	(y (slot-value old 'y)))
    (setf (slot-value new 'rho) (sqrt (+ (* x x) (* y y)))
	  (slot-value new 'theta) (atan y x))))

(deftest change-class
  (let ((p1 (make-instance 'x-y-position 'x 2 'y 0)))
    (and (eq p1 (change-class p1 'rho-theta-position))
	 (list (position-rho p1)
	       (position-theta p1))))
  (2.0f0 0.0f0))

(defmethod update-instance-for-redefined-class :before
  ((pos x-y-position) added deleted plist &key)
  ;; Transform the x-y coordinates to polar coordinates
  ;; and store into the new slots.
  (declare (ignore added deleted))
  (let ((x (getf plist 'x))
	(y (getf plist 'y)))
    (setf (position-rho pos) (sqrt (+ (* x x) (* y y)))
	  (position-theta pos) (atan y x))))

(deftest update-instance-for-refined-class
  (progn
    (defclass x-y-position (base-position)
      ((x :initform 0 :accessor position-x :initarg x)
       (y :initform 0 :accessor position-y :initarg y)))
    (let ((x-y (make-instance 'x-y-position)))
      (setf (position-x x-y) 2)
      (setf (position-y x-y) 0)
      (defclass x-y-position (base-position)
	((rho :initform 0 :accessor position-rho)
	 (theta :initform 0 :accessor position-theta)))
      (values (position-rho x-y)
	      (position-theta x-y)
	      (null (defclass x-y-position (base-position)
		      ((x :initform 0 :accessor position-x :initarg x)
		       (y :initform 0 :accessor position-y :initarg y)))))))
  2.0f0 0.0f0 nil)
    
;;;; GENERIC FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
;;; DEFGENERIC !!!
;;; ENSURE-GENERIC-FUNCTION!!!
;;; DEFMETHOD !!!
;;; ADD-METHOD !!!
;;; REMOVE-METHOD !!!

;;; FIND-METHOD
(deftest find-method-found
  (eq (defmethod fmsome-operation ((a integer) (b float)) (list a b))
      (find-method (fdefinition 'fmsome-operation) '() (mapcar #'find-class
						  '(integer float))))
  t)
(deftest find-method-not-found
  (find-method (fdefinition 'fmsome-operation) '() (mapcar #'find-class '(integer integer)) nil)
  nil)


;;; METHOD-QUALIFIERS
(deftest method-qualifiers
  (method-qualifiers (defmethod mqgf :before ((a integer)) a))
  (:BEFORE))

;;; FUNCTION-KEYWORDS
(defmethod fkgf1 ((a integer) &optional (b 2)
		&key (c 3) ((:dee d) 4) e ((eff f)))
  (list a b c d e f))
(defmethod fkgf2 ((a integer))
  (list a))

(deftest function-keywords1
  (multiple-value-bind (keys allow)
      (function-keywords (find-method (fdefinition 'fkgf1) '()
				      (list (find-class 'integer))))
    (values (set-exclusive-or keys '(:C :DEE :E EFF)) allow))
  nil nil)

(deftest function-keywords2
  (function-keywords (find-method (fdefinition 'fkgf2) '() (list (find-class 'integer))))
  () nil)

(deftest function-keywords3
  (multiple-value-bind (keys allow)
      (function-keywords
       (defmethod fkgf3 ((a integer) &key b c d &allow-other-keys)
	 (list a b c d)))
    (values (set-exclusive-or keys '(:B :C :D)) (not (null allow))))
  nil t)


;;; :argument-precedence-order
(defgeneric apo (x y) (:argument-precedence-order y x))
(defmethod apo ((a integer) (b number)) (list (1+ a) b))
(defmethod apo ((c number) (d integer)) (list (1- c) d))
(deftest apo (apo 5 10) (4 10))

;;; COMPUTE-APPLICABLE-METHODS !!!
;;; NO-APPLICABLE-METHOD !!!
;;; CALL-METHOD, MAKE-METHOD !!!
;;; NEXT-METHOD-P !!!
;;; NO-NEXT-METHOD !!!
;;; CALL-NEXT-METHOD !!!

;;; DEFINE-METHOD-COMBINATION
(define-method-combination and1 :identity-with-one-argument t) 
(define-method-combination and2
  (&optional (order :most-specific-first))
  ((around (:around))
   (primary (and) :order order :required t))
  (let ((form (if (rest primary)
		  `(and ,@(mapcar #'(lambda (method)
				      `(call-method ,method))
				  primary))
		`(call-method ,(first primary)))))
    (if around
	`(call-method ,(first around)
		      (,@(rest around)
			 (make-method ,form)))
      form)))
(define-method-combination or1 :identity-with-one-argument t)
(define-method-combination or2
  (&optional (order ':most-specific-first))
  ((around (:around))
   (primary (or)))
  ;; Process the order argument
  (case order
    (:most-specific-first)
    (:most-specific-last (setq primary (reverse primary)))
    (otherwise (method-combination-error "~S is an invalid order.~@
     :most-specific-first and :most-specific-last are the possible values."
					 order)))
  ;; Must have a primary method
  (unless primary
    (method-combination-error "A primary method is required."))
  ;; Construct the form that calls the primary methods
  (let ((form (if (rest primary)
		  `(or ,@(mapcar #'(lambda (method)
				     `(call-method ,method))
				 primary))
		`(call-method ,(first primary)))))
    ;; Wrap the around methods around that form
    (if around
	`(call-method ,(first around)
		      (,@(rest around)
			 (make-method ,form)))
      form)))

;(defmethod func and ((x class1) y) ...)
;Order methods by positive integer qualifiers
;;:around methods are disallowed to keep the example small
(define-method-combination example-method-combination ()
  ((methods positive-integer-qualifier-p))
  `(progn ,@(mapcar #'(lambda (method)
			`(call-method ,method))
		    (stable-sort methods #'<
				 :key #'(lambda (method)
					  (first (method-qualifiers method)))))))
 
(defun positive-integer-qualifier-p (method-qualifiers)
  (and (= (length method-qualifiers) 1)
       (typep (first method-qualifiers) '(integer 0 *))))
  
;;; Example of the use of :arguments
(define-method-combination progn-with-lock () ;!!!
  ((methods ()))
  (:arguments object)
  `(unwind-protect
       (progn (lock (object-lock ,object))
	      ,@(mapcar #'(lambda (method)
			    `(call-method ,method))
			methods))
     (unlock (object-lock ,object))))

;;; INVALID-METHOD-ERROR !!!
;;; METHOD-COMBINATION-ERROR !!!

;;;; LOAD FORMS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; MAKE-LOAD-FORM
(defclass mlfobj ()
  ((x :initarg :x :reader mlfobj-x)
   (y :initarg :y :reader mlfobj-y)
   (dist :accessor mlfobj-dist)))
(defmethod shared-initialize :after ((self mlfobj) slot-names &rest keys)
  (declare (ignore slot-names keys))
  (unless (slot-boundp self 'dist)
    (setf (mlfobj-dist self)
	  (sqrt (+ (expt (mlfobj-x self) 2) (expt (mlfobj-y self) 2))))))
(defmethod make-load-form ((self mlfobj) &optional environment)
  (declare (ignore environment))
  `(make-instance ',(class-name (class-of self))
		  :x ',(mlfobj-x self) :y ',(mlfobj-y self)))

(deftest make-load-form1
  (let ((mlfobj1 (make-instance 'mlfobj :x 3.0 :y 4.0)))
    (values (mlfobj-dist mlfobj1)
	    (make-load-form mlfobj1)))
  5.0 (MAKE-INSTANCE 'MLFOBJ :X '3.0 :Y '4.0))

(let ((x 1))
  (defmethod sample-gf (tag) (list tag (1+ x)))
  (defmethod (setf sample-gf) (new-value tag) (list tag (setf x new-value))))
(deftest eql-gf-function
  (eql (ensure-generic-function 'sample-gf)
       (function sample-gf)) t)
(deftest eql-gf-setf-function
  (eql (ensure-generic-function '(setf sample-gf))
       (function (cl:setf sample-gf))) t)
(deftest eql-gf-fdefinition
  (eql (ensure-generic-function 'sample-gf)
       (fdefinition 'sample-gf)) t)
(deftest eql-gf-setf-fdefinition
  (eql (ensure-generic-function '(setf sample-gf))
       (fdefinition '(setf sample-gf))) t)
(deftest eql-gf-symbol-function
  (eql (ensure-generic-function 'sample-gf)
       (symbol-function 'sample-gf)) t)
;#+not-yet
(deftest apply-gf
  (values (apply (fdefinition '(setf sample-gf)) 3 'foo nil)
	  (apply (fdefinition 'sample-gf) 'foo nil))
  (foo 3) (foo 4))
;#+not-yet
(deftest funcall-gf
  (values (funcall (fdefinition '(setf sample-gf)) 2 'foo)
	  (funcall (fdefinition 'sample-gf) 'foo))
  (foo 2) (foo 3))
(deftest apply-gf2
  (values (apply (function (cl:setf sample-gf)) 3 'foo nil)
	  (apply (function sample-gf) 'foo nil))
  (foo 3) (foo 4))
(deftest funcall-gf2
  (values (funcall (function (cl:setf sample-gf)) 2 'foo)
	  (funcall (function sample-gf) 'foo))
  (foo 2) (foo 3))

;;; TYPE-OF
(deftest class-type-of (type-of (make-instance 'cobook)) cobook)

;;; TYPEP
(deftest class-typep (typep (make-instance 'cobook) 'cobook) t)
(deftest class-typep2 (typep (make-instance 'cobook) (find-class 'cobook)) t)

;;; SUBTYPEP
(deftest class-subtypep1 (subtypep (find-class 'pie) (find-class 'food)) t t)
(deftest class-subtypep2 (subtypep (find-class 'pie) 'food) t t)
(deftest class-subtypep3 (subtypep 'pie (find-class 'food)) t t)
(deftest class-subtypep4 (subtypep 'pie 'food) t t)
(deftest class-subtypep1n (subtypep (find-class 'pie) (find-class 'pastry)) nil t)
(deftest class-subtypep2n (subtypep (find-class 'pie) 'pastry) nil t)
(deftest class-subtypep3n (subtypep 'pie (find-class 'pastry)) nil t)
(deftest class-subtypep4n (subtypep 'pie 'pastry) nil t)
