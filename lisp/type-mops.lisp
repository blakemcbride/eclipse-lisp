;;; N.B.: FIXNUM, BIGNUM are not very efficient for use with TYPEP,
;;; because they involve range checks.  The Eclipse FIXNUMP predicate
;;; should be used instead.
;;; 
;;; Note that although the specialized ARRAY type specifiers can
;;; involve computation, they expand into class objects when possible,
;;; which allow the efficient classp operation to be used rather than
;;; the more general (array ... ...) form.

(defmethod TYPE-NAME (object) object)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                  TYPE METACLASSES                            ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq *eql-specializer-type*
      (defclass EQL-SPECIALIZER (specializer)
	((object :initarg :object))
	(:primary t)
	(:sealed t)
	(:metaclass interned-standard-class)))

(defclass Type-Combination (type)
  ((types :initarg :types :type list :accessor component-types)))

(defclass Type-Intersection (type-combination) ()
  (:metaclass interned-standard-class))

(defclass Type-Union (type-combination) ()
  (:metaclass interned-standard-class))

(defclass Type-Complement (type)
  ((type :initarg :type :accessor type-complement-type))
  (:metaclass interned-standard-class))

(defclass Ambiguous-Type (type) ())

(defclass Type-Satisfies (ambiguous-type)
  ((predicate :initarg :predicate :accessor type-predicate))
  (:metaclass interned-standard-class))

(defclass Forward-Referenced-Type (ambiguous-type) ())

(defmethod print-object ((object forward-referenced-type) stream)
  (print-unreadable-object (object stream)
    (format stream "Undefined-Type ~s" (type-name object))))

(defun expand-to-forward-reference (type-specifier environment)
  (declare (ignore environment))
  (make-instance 'forward-referenced-type
		 :name (if (and (consp type-specifier)
				(cdr type-specifier))
			   type-specifier
			   (car type-specifier))))
		 
(defclass Derived-Type (interned-standard-class)
  ((function :initarg :function
	     :initform #'expand-to-forward-reference
	     :accessor derived-type-expansion-function)
   (test :initform 'equal))
  (:default-initargs
   :direct-superclasses (list (find-type 'type))))

;;; ISSUE:
;;; When a derived-type is redefined, expand-deftype won't return the
;;; old expansions because the cache is cleared (by
;;; interned-standard-class).  However, the old instances might
;;; still be cached within some composite type.  
					      
(defclass Limited-Type (type)
  ((base-type :initarg :base-type :accessor base-type)
   (test :initform 'equal)))

(defclass Limited-Numeric-Type (Limited-Type)
  ((lower-limit :initform nil :initarg :min :accessor limited-type-min)
   (upper-limit :initform nil :initarg :max :accessor limited-type-max)
   (lower-limit-inclusive-p :initform t :initarg :inclusive-min
			    :accessor limited-type-inclusive-min)
   (upper-limit-inclusive-p :initform t :initarg :inclusive-max
			    :accessor limited-type-inclusive-max))
  (:metaclass interned-standard-class))

(defclass Limited-Character-Type (limited-numeric-type) ()
  (:metaclass interned-standard-class))

(defclass Limited-Complex-Type (Limited-Type)
  ((part-type :initarg :part-type
	      :reader complex-part-type)
   (base-type :initform (find-type 'complex)))
  (:metaclass interned-standard-class))

(defclass Limited-Cons-Type (Limited-Type)
  ((car-type :initarg :car-type
	     :reader cons-car-type)
   (cdr-type :initarg :cdr-type
	     :reader cons-cdr-type)
   (base-type :initform cons-classobj))
  (:metaclass interned-standard-class))

(defclass Limited-Array-Type (Limited-Type)
  ((element-type :initarg :element-type
		 :reader limited-array-element-type)
   (dimensions :initarg :dimensions
	       :reader limited-array-dimensions))
  (:metaclass interned-standard-class))


(defclass Declaration-Type (type) ())
(defclass Limited-Function-Type (Declaration-Type Limited-Type)
  ((argument-types :initarg :argument-types
		   :reader function-argument-types)
   (return-type :initarg :return-type
		:reader function-return-types)
   (base-type :initform (find-type 'function)))
  (:metaclass interned-standard-class))
(defclass Limited-Values-Type (Declaration-Type Limited-Type)
  ((base-type :initarg :primary-type
	      :reader primary-type)
   (secondary-types :initarg :secondary-types
		    :reader secondary-types))
  (:metaclass interned-standard-class))
  

(defmethod MAKE-LOAD-FORM ((object TYPE) &optional environment)
  (declare (ignore environment))
  `(expand-type ',(type-name object) nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                    DEFTYPE                                   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftype KEYWORD () '(and symbol (not null) (satisfies keywordp)))
(deftype ATOM () '(not cons))
(deftype BOOLEAN () '(or (eql t) (eql nil)))
(deftype NIL () '(or))
  
(deftype STANDARD-CHAR () '(or (character 32 126)
			       (character 10 10)))
(deftype BASE-CHAR () `(character 0 (,base-char-code-limit)))
(deftype EXTENDED-CHAR () '(and character (not base-char)))
(deftype ASCII-Char () '(character 0 #x7F))
(deftype Latin-Char () 'base-char)
(deftype UCS-2-Char () '(character 0 #xFFFF))
(deftype Unicode-Char () '(character 0 #x10ffff))

(deftype FIXNUM () `(integer ,most-negative-fixnum
			     ,most-positive-fixnum))
(deftype BIGNUM () '(and integer (not fixnum)))
(deftype BIT () '(integer 0 1))
(deftype MOD (n) `(integer 0 ,(1- n)))
(deftype SIGNED-BYTE (&optional (s '*))
  (if (eq s '*)
      'integer
      (let ((bound (ash 1 (1- s))))
	`(integer ,(- bound) ,(1- bound)))))
(deftype UNSIGNED-BYTE (&optional (s '*))
  (if (eq s '*)
      '(integer 0)
      `(integer 0 ,(1- (ash 1 s)))))

(deftype SHORT-FLOAT (&optional low high) `(single-float ,low ,high))
(deftype LONG-FLOAT  (&optional low high) `(double-float ,low ,high))

;; Used in the implementation.
(deftype function-name () '(or symbol (cons (eql setf) (cons symbol null))))
(deftype function-designator () '(or function (and symbol (satisfies fboundp))))
(deftype format-control () '(or string function))
(deftype type-specifier () '(or type symbol (cons symbol)))
				 
(deftype sequence-type-specifier () '(satisfies sequence-type-specifier-p))
(deftype character-designator ()
  '(or character (string 1) (and symbol (satisfies character-designator-name-p))))

(defun character-designator-name-p (symbol)
  (typep (symbol-name symbol) 'character-designator))
#|
(deftype Simple-Specialized-Vector ()
	'(SIMPLE-ARRAY * 1))
(deftype Long () 
	'(integer #.(signed-low wordsize)
		  #.(signed-high wordsize)))
(deftype Short ()
	'(integer #.(signed-low digitsize)
		  #.(signed-high digitsize)))
(deftype Dimension ()
	'(integer 0 #.(cl:1- array-dimension-limit)))
(deftype Rank ()
	'(integer 0 #.(cl:1- array-rank-limit)))
  |#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                    OPERATIONS                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                    COMPLEMENT                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgeneric Type-Complement (type &optional environment))

(defmethod type-complement (type &optional environment)
  (type-complement (expand-type type t environment) environment))

(defmethod type-complement ((type TYPE) &optional environment)
  (declare (ignore environment))
  (if (eq type t-classobj)
      (intern-type-union)
      (intern-type-complement type)))

(defmethod type-complement ((type TYPE-COMPLEMENT) &optional environment)
  (declare (ignore environment))
  (type-complement-type type))

(defmethod type-complement ((type TYPE-UNION) &optional environment)
  (intersect-types (mapcar #'type-complement (component-types type))
		   t environment))

(defmethod type-complement ((type TYPE-INTERSECTION) &optional environment)
  (union-types (mapcar #'type-complement (component-types type))
	       environment))

(defun new-limit (val inclusivep)
  (if val
      (if inclusivep val (list val))
      '*))

(defmethod type-complement ((type LIMITED-NUMERIC-TYPE) &optional environment)
  (let* ((base (base-type type))
	 (low (limited-type-min type))
	 (high (limited-type-max type))
	 (new (list (type-complement base))))
    (when high
      (push (intern-numeric-type
	     (list base  (new-limit (limited-type-max type)
				    (not (limited-type-inclusive-max type))))
	     t environment)
	    new))
    (when low
      (push (intern-numeric-type
	     (list base '*
		   (new-limit (limited-type-min type)
			      (not (limited-type-inclusive-min type))))
	     t environment)
	    new))
    (union-types new t environment)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                    UNION                                     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; In general it is only legal to combine multiple overlapping
;;; limited types appearing within a union into a single limited type
;;; with larger scope IFF they are adjacent.  Otherwise, the failure
;;; to observe the ordering might result in wrong answers.  We avoid
;;; the issue altogether and do no merging, which makes subtypep more
;;; difficult.  The same is true for intersections.

;;; Imagine TYPE-UNION (TYPE TYPE-UNION), where the type-unin contains
;;; a satisfies clause.  The type might be a subtype of the union, but
;;; we still might have to add the new type specifier on to the front
;;; of the union in order to preserve evaluation order filtering for
;;; typep.  This prevents us from doing subtype/supertype tests in an
;;; around method for all TYPE-UNION (TYPE TYPE).

(defgeneric type-union (type-1 type-2 &optional environment))
(defmethod type-union (type-1 type-2  &optional environment)
  (type-union (expand-type type-1 t environment)
	      (expand-type type-2 t environment)
	      environment))

(defmethod type-union ((type-1 TYPE) (type-2 TYPE) &optional environment)
  (cond ((subtypep type-1 type-2 environment) type-2)
	((subtypep type-2 type-1 environment) type-1)
	(t (intern-type-union type-1 type-2))))

(defmethod type-union ((type-1 TYPE-COMPLEMENT) (type-2 TYPE)
		       &optional environment)
  (declare (ignore environment))
  (or (combine-complement type-1 type-2 t)
      (call-next-method)))
(defmethod type-union ((type-1 TYPE) (type-2 TYPE-COMPLEMENT)
		       &optional environment)
  (declare (ignore environment))
  (or (combine-complement type-2 type-1 t)
      (call-next-method)))

;;; When the union of two types is a single, broader type (eg. a
;;; limited-type with a wider range), then we only include the
;;; combined type in place of the narrower component type, and not the
;;; broader new type that was asked for.

(defmethod type-union ((combination TYPE-UNION) (type TYPE)
		       &optional environment)
  (let ((components nil) (new-type type))
    (dolist (component (component-types combination))
      (unless (subtypep component type environment)
	(let ((new (type-union component type environment)))
	  (when (eq new t-classobj) (return-from type-union new))
	  (cond ((typep new 'type-union environment)
		 (push (first (component-types new)) components))
		(t (push new components)
		   (setq new-type nil))))))
    (when new-type
      (if (typep new-type 'type-union environment)
	  (dolist (component (component-types new-type))
	    (push component components))
	  (push new-type components)))
    (apply #'intern-type-union (nreverse components))))


;;; Here we still replace all narrower types with a broader type when
;;; possible.  However, the new type might be used to filter before a
;;; satisfies clause, so we must include the new type at the begining
;;; if the only place in which it is merged to a broader type is after
;;; the satisifies clause.

(defmethod type-union ((type TYPE) (combination TYPE-UNION)
		       &optional environment)
  (let ((components nil) (combine t) (new-type type))
    (dolist (component (component-types combination))
      (unless (subtypep component type environment)
	(let ((new (type-union type component environment)))
	  (when (eq new t-classobj) (return-from type-union new))
	  (cond ((typep new 'type-union environment)
		 (push (second (component-types new)) components))
		(t (push new components)
		   (when combine (setq new-type nil))))
	  (when (ambiguous-type-p component) (setq combine nil)))))
    (setq components (nreverse components))      
    (when new-type (push type components))
    (apply #'intern-type-union components)))

(let ((integral-types (list (find-type 'integer) (find-type 'character))))
  (defmethod type-union ((type-1 LIMITED-NUMERIC-TYPE)
			 (type-2 LIMITED-NUMERIC-TYPE) &optional environment)
    (let ((base (base-type type-1)))
      (cond ((not (eq (base-type type-2) base))
	     (intern-type-union type-1 type-2))
	    ((subtypep type-1 type-2 environment) type-2)
	    ((subtypep type-2 type-1 environment) type-1)
	    (t
	     (let ((lower (limited-type-min type-1))
		   (upper (limited-type-max type-1))
		   (lower-exclusive (not (limited-type-inclusive-min type-1)))
		   (upper-exclusive (not (limited-type-inclusive-max type-1))))
	       (when (member base integral-types)
		 (when lower (decf lower) (setf lower-exclusive t))
		 (when upper (incf upper) (setf upper-exclusive t)))
	       (let* ((new-low-p (and lower
				      (meets-low lower type-2 lower-exclusive)
				      (meets-high lower type-2
						  lower-exclusive)))
		      (new-high-p (and upper
				       (meets-low upper type-2 upper-exclusive)
				       (meets-high upper type-2 upper-exclusive)))
		      (new-low (when new-low-p (limited-type-min type-2)))
		      (new-high (when new-high-p (limited-type-max type-2))))
		 (if (or new-low-p new-high-p)
		     (intern-numeric-type
		      `(,base ,(new-limit (if new-low-p new-low lower)
					  (if new-low-p
					      (limited-type-inclusive-min type-1)
					      (not lower-exclusive)))
			      ,(new-limit (if new-high-p new-high upper)
					  (if new-high-p
					      (limited-type-inclusive-max type-1)
					      (not upper-exclusive))))
		      t nil)
		     (intern-type-union type-1 type-2)))))))))


(defmethod type-union ((type-1 LIMITED-CONS-TYPE)
		       (type-2 LIMITED-CONS-TYPE) &optional environment)
  (intern-cons-type (list (type-union (cons-car-type type-1)
				      (cons-car-type type-2)
				      environment)
			  (type-union (cons-cdr-type type-1)
				      (cons-cdr-type type-2)
				      environment))
		    t environment))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                    INTERSECTION                              ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgeneric type-intersection (type-1 type-2 &optional environment))
(defmethod type-intersection (type-1 type-2 &optional environment)
  (type-intersection (expand-type type-1 t environment)
		     (expand-type type-2 t environment)
		     environment))

(defmethod type-intersection ((type-1 TYPE) (type-2 TYPE)
			      &optional environment)
  (cond ((subtypep type-1 type-2 environment) type-1)
	((subtypep type-2 type-1 environment) type-2)
	(t (intern-type-intersection type-1 type-2))))

(defmethod type-intersection ((type-1 LIMITED-NUMERIC-TYPE)
			      (type-2 CLASS) &optional environment)
  (if (subtypep type-1 type-2 environment)
      type-1
      (intern-type-union)))
(defmethod type-intersection ((type-1 CLASS)
			      (type-2 LIMITED-NUMERIC-TYPE)
			      &optional environment)
  (declare (ignore environment))
  (type-intersection type-2 type-1))

(defmethod type-intersection ((type-1 TYPE-COMPLEMENT)
			      (type-2 TYPE) &optional environment)
  (declare (ignore environment))
  (or (combine-complement type-1 type-2 nil)
      (call-next-method)))
(defmethod type-intersection ((type-1 TYPE)
			      (type-2 TYPE-COMPLEMENT)
			      &optional environment)
  (declare (ignore environment))
  (or (combine-complement type-2 type-1 nil)
      (call-next-method)))

(defmethod type-intersection ((type-1 CLASS)
			      (type-2 TYPE-COMPLEMENT) &optional environment)
  (cond ((subtypep type-1 type-2 environment) type-1)
	((eq (type-name type-1) t) type-2)
	(t
      (let ((common (type-intersection
		     type-1 (type-complement-type type-2)
		      environment)))
	(union-types
	 (when (subtypep common type-1 environment)
	   (let ((subs
		  (loop with complement = (type-complement common environment)
			for sub in (class-direct-subclasses type-1)
			collect (type-intersection sub complement environment))))
	     (unless (or (abstract-class-p type-1)
			 (subtypep type-1 common environment))
	       (push (intern-type-intersection type-1 type-2) subs))
	     subs))
	 t environment)))))
	   
(defmethod type-intersection ((type-1 TYPE-COMPLEMENT) (type-2 CLASS)
			      &optional environment)
  (type-intersection type-2 type-1 environment))
	     
;;; In general, we can't predict whether we it will be faster to
;;; cruise down the subclass chain of type-1 or type-2.  However,
;;; INTERSECT-TYPES calls (type-intersection 'T x), so we know that
;;; looking at the subclasses of type-1 is not a win.
(defmethod type-intersection ((type-1 CLASS) (type-2 CLASS)
			      &optional environment)
  (cond ((subtypep type-1 type-2 environment) type-1)
	((subtypep type-2 type-1 environment) type-2)
	(t (union-types
	    (loop for sub in (class-direct-subclasses type-2)
		  collect (type-intersection type-1 sub environment))
	    t environment))))

(defmethod type-intersection ((type-1 TYPE) (type-2 TYPE-UNION)
			      &optional environment)
  (union-types
   (loop for type in (component-types type-2)
	 collect (type-intersection type-1 type environment))
   t environment))

(defmethod type-intersection ((type-1 TYPE-UNION) (type-2 TYPE)
			      &optional environment)
  (union-types
   (loop for type in (component-types type-1)
	 collect (type-intersection type type-2 environment))
   t environment))

;;; When a component type specifers should be replaced by some more
;;; restricted combination, that more restricted result is not used
;;; immediately, but instead used for all further testing and then
;;; placed on the front.  If the more restricted specifier ever
;;; becomes #<type NIL>, then we return it.

(let ((null-type (intern-type-union)))
  (defmethod type-intersection ((type TYPE) (combination TYPE-INTERSECTION)
				&optional environment)
    (let ((components nil))
      (dolist (component (component-types combination))
	(let ((new (type-intersection type component environment)))
	  (when (eq new null-type) (return-from type-intersection new))
	  (if (typep new 'type-intersection environment)
	      (push (second (component-types new)) components)
	      ;; We should really go back over components here because
	      ;; the new clause might further reduce them.
	      (setq type new))))
      (setq components (nreverse components))
      (push type components)
      (apply #'intern-type-intersection components))))

(defmethod type-intersection ((combination TYPE-INTERSECTION)
			      (type TYPE)
			      &optional environment)
  (dolist (component (reverse (component-types combination))
		     type)
    (setq type (type-intersection component type environment))))
  

(defmethod type-intersection ((type-1 LIMITED-NUMERIC-TYPE)
			      (type-2 LIMITED-NUMERIC-TYPE)
			      &optional  environment)
  (let ((base (base-type type-1)))
    (cond ((not (eq (base-type type-2) base))
	   (intern-type-intersection type-1 type-2))
	  ((subtypep type-1 type-2 environment) type-1)
	  ((subtypep type-2 type-1 environment) type-2)
	  (t
	   (let ((upper-1 (limited-type-max type-1))
		 (upper-2 (limited-type-max type-2))
		 (lower-1 (limited-type-min type-1))
		 (lower-2 (limited-type-min type-2))
		 (inclusive-upper-1 (limited-type-inclusive-max type-1))
		 (inclusive-upper-2 (limited-type-inclusive-max type-2))
		 (inclusive-lower-1 (limited-type-inclusive-min type-1))
		 (inclusive-lower-2 (limited-type-inclusive-min type-2)))
	     (intern-numeric-type
	      `(,base ,(cond ((null lower-1)
			      (new-limit lower-2 inclusive-lower-2))
			     ((null lower-2)
			      (new-limit lower-1 inclusive-lower-1))
			     (t (new-limit (max lower-1 lower-2)
					   (and inclusive-lower-1
						inclusive-lower-2))))
		      ,(cond ((null upper-1)
			      (new-limit upper-2 inclusive-upper-2))
			     ((null upper-2)
			      (new-limit upper-1 inclusive-upper-1))
			     (t (new-limit (min upper-1 upper-2)
					   (and inclusive-upper-1
						inclusive-upper-2)))))
	      t  environment))))))


(defmethod type-intersection ((type-1 LIMITED-CONS-TYPE)
			      (type-2 LIMITED-CONS-TYPE)
			      &optional environment)
  (intern-cons-type (list (type-intersection (cons-car-type type-1)
					     (cons-car-type type-2)
					     environment)
			  (type-intersection (cons-cdr-type type-1)
					     (cons-cdr-type type-2)
					     environment))
		    t  environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                   PREDICATES                                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                 AMBIGUOUS-TYPE-P                             ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod ambiguous-type-p ((type TYPE)) nil)
(defmethod ambiguous-type-p ((type ambiguous-type)) t)
(defmethod ambiguous-type-p ((type forward-referenced-class)) t)
(defmethod ambiguous-type-p ((type type-complement))
  (ambiguous-type-p (type-complement-type type)))
(defmethod ambiguous-type-p ((type type-combination))
  (some #'ambiguous-type-p (component-types type)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                    TYPEP                                     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Potential Optimization:  If the constant-method class is
;;; implemented, typep and subtypep could be redefined to use them.
;;; Note though, that only some methods are constant; specifically,
;;; those with class specializers.

(defgeneric TYPEP (object type-specifier &optional environment))

(defmethod typep (object type &optional environment)
  (typep object (expand-type type t environment)))

;;; There are two ways to think of this:
;;; 1. An object that exists can't be an instance of a type that has
;;;    not yet been defined.  Return NIL.
;;; 2. The type could be defined later to be a synonym for the type of
;;;    the object, so the real answer is "I don't know yet."  Signal
;;;    error.
;;; We do (1), because typep is used within SUBTYPEP, and we don't
;;; want to deal with errors.  Potential bug???
(defmethod typep (object (type FORWARD-REFERENCED-TYPE) &optional environment)
  (declare (ignore environment object))
  nil)

(defmethod typep (object (type DECLARATION-TYPE) &optional environment)
  (declare (ignore environment object))
  (illegal-discrimination-error type))

(defmethod typep (object (type CLASS) &optional environment)
  (declare (ignore environment))
  (not (null (classp object type))))

(defmethod typep (object (type EQL-SPECIALIZER) &optional environment)
  (declare (ignore environment))
  (eql object (eql-specializer-object type)))

(defmethod typep (object (type TYPE-UNION) &optional environment)
  (dolist (type (component-types type) nil)
    (when (typep object type environment) (return t))))

(defmethod typep (object (type TYPE-INTERSECTION) &optional environment)
  (dolist (type (component-types type) t)
    (unless (typep object type environment) (return nil))))

(defmethod typep (object (type TYPE-COMPLEMENT) &optional environment)
  (not (typep object (type-complement-type type) environment)))

(defmethod typep (object (type TYPE-SATISFIES) &optional environment)
  (declare (ignore environment))
  (funcall (type-predicate type) object))

(defmethod typep (object (type LIMITED-NUMERIC-TYPE) &optional environment)
  (and (typep object (base-type type) environment)
       (meets-low object type)
       (meets-high object type)))
(defmethod typep (object (type LIMITED-CHARACTER-TYPE) &optional environment)
  (when (typep object (base-type type) environment)
    (let ((code (char-code object)))
      (and (meets-low code type)
	   (meets-high code type)))))
		 
(defmethod typep (object (type LIMITED-COMPLEX-TYPE) &optional environment)
  (and (typep object (base-type type) environment)
       (typep (realpart object) (complex-part-type type) environment)))

(defmethod typep (object (type LIMITED-CONS-TYPE) &optional environment)
  (and (typep object (base-type type) environment)
       (typep (car object) (cons-car-type type) environment)
       (typep (cdr object) (cons-cdr-type type) environment)))

(defun dimensions-match-p (dims spec)
  (cond ((eq spec '*) t)
	((eq dims '*) nil)
	(t
	 (dolist (dim spec (null dims))
	   (cond ((eq dim '*) (unless (pop dims)
				(return nil)))
		 ((eql dim (pop dims)))
		 (t (return nil)))))))

(defmethod typep (object (type LIMITED-ARRAY-TYPE)
			 &optional environment)
  (and (typep object (base-type type) environment)
       (eq (array-element-type object)
	   (limited-array-element-type type))
       (dimensions-match-p (array-dimensions object)
			   (limited-array-dimensions type))))
       

#+not-yet
(define-compiler-macro TYPEP (&whole form object type &optional environment)
  (if (constantp type env)
      (let ((expansion (expand-type (eval type) nil)))
	(if (ambiguous-type-p expansion)
	    form
	    `(typep ,object ,expansion)))
      form))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                    SUBTYPEP                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgeneric SUBTYPEP (type-1 type-2 &optional environment))
  
(defmethod subtypep (type-1 type-2 &optional environment)
  (let ((t1 (expand-type type-1 nil environment))
	(t2 (expand-type type-2 nil environment)))
    (if (and t1 t2)
	(subtypep t1 t2 environment)
	(values nil nil))))

(defmethod subtypep ((type-1 DECLARATION-TYPE) (type-2 TYPE)
		     &optional environment)
  (declare (ignore environment type-2))
  (illegal-discrimination-error type-1))
(defmethod subtypep ((type-1 TYPE) (type-2 DECLARATION-TYPE)
		     &optional environment)
  (declare (ignore environment type-1))
  (illegal-discrimination-error type-2))

(defun ambiguous-subtypep (type-1 type-2)
  (if (or (eq type-2 t-classobj)
	  (eq type-1 type-2))
      (values t t)
      (values nil nil)))
(defmethod subtypep ((type-1 AMBIGUOUS-TYPE) (type-2 TYPE) &optional environment)
  (declare (ignore environment))
  (ambiguous-subtypep type-1 type-2))
(defmethod subtypep ((type-1 TYPE) (type-2 AMBIGUOUS-TYPE) &optional environment)
  (declare (ignore environment))
  (values nil nil))

(defmethod subtypep ((type-1 FORWARD-REFERENCED-CLASS) (type-2 TYPE) &optional environment)
  (declare (ignore environment))
  (ambiguous-subtypep type-1 type-2))


(defmethod subtypep ((type-1 CLASS) (type-2 FORWARD-REFERENCED-CLASS) &optional environment)
  (declare (ignore environment))
  (values nil nil))

(defmethod subtypep ((type-1 LIMITED-NUMERIC-TYPE) (type-2 TYPE-SATISFIES)
		     &optional environment)
  (let ((val (limited-type-min type-1)))
    (if (and val (eql val (limited-type-max type-1)))
	(values (typep val type-2 environment) t)
	(values nil nil))))
(defmethod subtypep ((type-1 LIMITED-CHARACTER-TYPE) (type-2 TYPE-SATISFIES)
		     &optional environment)
  (let ((val (limited-type-min type-1)))
    (if (and val (eql val (limited-type-max type-1)))
	(values (typep (code-char val) type-2 environment) t)
	(values nil nil))))


(defmethod subtypep ((type-1 TYPE) (type-2 TYPE) &optional environment)
  (declare (ignore environment type-1))
  (values nil t))

(defmethod subtypep ((type-1 CLASS) (type-2 CLASS) &optional environment)
  (declare (ignore environment))
  (unless (class-finalized-p type-1) (finalize-inheritance type-1))
  (values (not (null (subclassp type-1 type-2))) t))

(defmethod subtypep ((type-1 EQL-SPECIALIZER) (type-2 TYPE) &optional environment)
  (values (typep (eql-specializer-object type-1) type-2 environment) t))

;;; Yuck!  type-1 might be some unreduced type that is equivalent to
;;; the eql-specializer-object.
(defmethod subtypep ((type-1 TYPE) (type-2 EQL-SPECIALIZER) &optional environment)
  (let ((object (eql-specializer-object type-2)))
    (values (when (typep object type-1 environment)
	      (when (null object)
		(eq type-1 (find-type 'null))))
	    t)))

(defmethod subtypep ((type-1 TYPE-COMPLEMENT) (type-2 TYPE) &optional environment)
  (declare (ignore environment))
  (values (eq type-2 t-classobj)
	  (not (ambiguous-type-p type-1))))

(defmethod subtypep ((type-1 TYPE-COMPLEMENT) (type-2 TYPE-COMBINATION) &optional environment)
  (subtypep (type-complement type-2)
	    (type-complement-type type-1)
	    environment))
	

(defmethod subtypep ((type-1 CLASS) (type-2 TYPE-COMPLEMENT) &optional environment)
  (let ((comp (type-complement-type type-2)))
    (multiple-value-bind (sub sure) (subtypep type-1 comp)
      (if sub
	  (values nil t)
	  (multiple-value-bind (super sure-super)
	      (subtypep comp type-1)
	    (cond (super (values nil t))
		  (t
		   (setq sure (and sure sure-super))
		   (dolist (sub (class-direct-subclasses type-1)
				(values sure sure))
		     (multiple-value-bind (subp surep)
			 (subtypep sub type-2 environment)
		       (unless subp
			 (if surep
			     (return (values nil t))
			     (setq sure nil))))))))))))
  
(defmethod subtypep ((type-1 TYPE) (type-2 TYPE-COMPLEMENT) &optional environment)
  (let ((type (type-complement-type type-2)))
    (multiple-value-bind (sub sub-sure)
	(subtypep type type-1 environment)
      (multiple-value-bind (super super-sure)
	  (subtypep type-1 type environment)
	(cond ((or sub super) (values nil t))
	      ((and sub-sure super-sure) (values t t))
	      (t (values nil nil)))))))

(defmethod subtypep ((type-1 TYPE-COMPLEMENT) (type-2 TYPE-COMPLEMENT)
		     &optional environment)
  (subtypep (type-complement-type type-2)
	    (type-complement-type type-1)
	    environment))


;;; When we encounter an undecidable member (unknown type or
;;; satisfies), we could return nil, nil immediately.  
;;; We have made the design decision to continue with the other
;;; members in the hope that we can return xxx, t instead.
(macrolet ((do-subs ((type type-1 type-2 result) &body body)
		    `(let ((surep t))
		       (dolist (type ,type (values ,result surep))
			 (multiple-value-bind (sub sure)
			     (subtypep ,type-1 ,type-2 environment)
			   ,@body))))
	   (unless-sub (type type-1 type-2)
		       `(do-subs (,type ,type-1 ,type-2 surep)
				 (unless sub
				   (if sure
				       (return (values nil t))
				       (setq surep nil)))))
	   (when-sub (type type-1 type-2 &optional second-chance)
		     `(do-subs (,type ,type-1 ,type-2 ,second-chance)
			       (when sub (return (values t t)))
			       (unless sure (setq surep nil)))))
  (defmethod subtypep ((type-1 TYPE-UNION) (type-2 TYPE) &optional environment)
    (unless-sub (component-types type-1) type type-2))
  (defmethod subtypep ((type-1 TYPE) (type-2 TYPE-UNION) &optional environment)
    (when-sub (component-types type-2) type-1 type))       

  (defmethod subtypep ((type-1 TYPE-INTERSECTION) (type-2 TYPE) &optional environment)
    (when-sub (component-types type-1) type type-2))
  (defmethod subtypep ((type-1 TYPE-INTERSECTION) (type-2 TYPE-INTERSECTION)
		       &optional environment)
    (unless-sub (component-types type-2) type-1 type))
  (defmethod subtypep ((type-1 TYPE) (type-2 TYPE-INTERSECTION) &optional environment)
    (unless-sub (component-types type-2) type-1 type))
  )


(defmethod subtypep ((type-1 CLASS) (type-2 TYPE-UNION) &optional environment)
  (let ((subs (when (abstract-class-p type-1)
		(class-direct-subclasses type-1))))
    (if subs
	(let ((surep t))
	  (dolist (subclass subs (values surep surep))
	    (multiple-value-bind (sub sure)
		(subtypep subclass type-2 environment)
	      (unless sub
		(if sure
		    (return (values nil t))
		    (setq surep nil))))))
	(call-next-method))))
      
  
(defmethod subtypep ((type-1 LIMITED-TYPE) (type-2 CLASS)
		     &optional environment)
  (subtypep (base-type type-1) type-2 environment))

;; Oops.  Array base-types are different.
(defmethod subtypep ((type-1 LIMITED-ARRAY-TYPE) (type-2 CLASS)
		     &optional environment)
  (let ((base (base-type type-1))
	(dims (limited-array-dimensions type-1)))
    (subtypep (if (and (listp dims) (= 1 (length dims)))
		  (element-vector-type (class-name base)
				       (limited-array-element-type
					type-1))
		  base)
	      type-2 environment)))
(defmethod subtypep ((type-1 CLASS) (type-2 LIMITED-ARRAY-TYPE)
		     &optional environment)
  (values (and (eq (limited-array-dimensions type-2) '*)
	       (subtypep type-1
			 (element-vector-type
			  (class-name (base-type type-2))
			  (limited-array-element-type type-2))
			 environment))
	  t))
			 

(defmethod subtypep ((type-1 LIMITED-COMPLEX-TYPE) (type-2 LIMITED-COMPLEX-TYPE)
		     &optional environment)
  (subtypep (complex-part-type type-1)
	    (complex-part-type type-2) environment))
(defmethod subtypep ((type-1 LIMITED-CONS-TYPE) (type-2 LIMITED-CONS-TYPE)
		     &optional environment)
  (multiple-value-bind (car-sub car-sure)
      (subtypep (cons-car-type type-1)
		(cons-car-type type-2) environment)
    (multiple-value-bind (cdr-sub cdr-sure)
	(subtypep (cons-cdr-type type-1)
		  (cons-cdr-type type-2) environment)
      (if (and car-sub cdr-sub)
	  (values t t)
	  (values nil (or (and car-sure (not car-sub))
			  (and cdr-sure (not cdr-sub))))))))
(defmethod subtypep ((type-1 LIMITED-ARRAY-TYPE) (type-2 LIMITED-ARRAY-TYPE)
		     &optional environment)
  (values (and (subtypep (base-type type-1) (base-type type-2) environment)
	       (eq (limited-array-element-type type-1)
		   (limited-array-element-type type-2))
	       (dimensions-match-p
		(limited-array-dimensions type-1)
		(limited-array-dimensions type-2)))
	  t))    

(defmethod subtypep ((type-1 LIMITED-NUMERIC-TYPE) (type-2 LIMITED-NUMERIC-TYPE)
		     &optional environment)
  (multiple-value-bind (sub sure)
      (subtypep (base-type type-1) (base-type type-2) environment)
    (if sure
	(values
	 (and sub 
	      (let ((upper (limited-type-max type-1)))
		(if upper
		    (meets-high upper type-2 (limited-type-inclusive-max type-1))
		    (null (limited-type-max type-2))))
	      (let ((lower (limited-type-min type-1)))
		(if lower
		    (meets-low lower type-2 (limited-type-inclusive-min type-1))
		    (null (limited-type-min type-2)))))
	 t)
	(values nil nil))))

