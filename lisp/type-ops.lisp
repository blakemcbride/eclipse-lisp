;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                  INTERNED STORES                             ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(macrolet ((with-table ((type-var type key) form)
		       `(let* ((,type-var (find-type ,type))
			       (table (class-instances ,type-var)))
			  (or (gethash ,key table)
			      (setf (gethash ,key table)
				    ,form)))))
  
  (defun intern-type-union (&rest types)
    (with-table (class 'type-union types)
		(cond ((null types)
		       (make-instance class
				      :types types
				      :name 'nil))
		      ((rest types)
		       (make-instance class
				      :types types
				      :name `(or ,@(mapcar #'type-name types))))
		      (t (first types)))))

  (defun intern-type-intersection (&rest types)
    (with-table (class 'type-intersection types)
		(cond ((null types) t-classobj)
		      ((rest types)
		       (make-instance class
				      :types types
				      :name `(and ,@(mapcar #'type-name types))))
		      (t (first types)))))

  (defun intern-type-complement (type)
    (with-table (class 'type-complement type)
		(make-instance class
			       :type type
			       :name `(not ,(type-name type)))))


  (defun intern-type-predicate (symbol)
    (with-table (class 'type-satisfies symbol)
		(make-instance class :predicate symbol
			       :name `(satisfies ,symbol))))

  ;; These two need to expand component types!!!
  (defun intern-function-type (type-spec errorp environment)
    (declare (ignore errorp environment))
    (with-table (class 'limited-function-type type-spec)
		(make-instance class
			       :argument-types (car type-spec)
			       :return-type (cdr type-spec))))

  (defun intern-values-type (type-spec errorp environment)
    (declare (ignore errorp environment))
    (with-table (class 'limited-values-type type-spec)
		(make-instance class
			       :primary-type (car type-spec)
			       :secondary-types (cdr type-spec))))

  (defun intern-cons-type (type-spec errorp environment)
    (with-table (class 'limited-cons-type type-spec)
		(destructuring-bind (&optional (car1 '*) (cdr1 '*)) type-spec
		  (let ((car (expand-type (option-type car1)
					  errorp environment))
			(cdr (expand-type (option-type cdr1)
					  errorp environment))
			(nil-type (intern-type-union)))
		    (cond ((and (eq car t-classobj) (eq cdr t-classobj))
			   cons-classobj)
			  ((or (eq car nil-type) (eq cdr nil-type))
			   nil-type)
			  (t (make-instance class
					    :name `(cons ,(type-name car)
							 ,(type-name cdr))
					    :car-type car
					    :cdr-type cdr)))))))


  (defun intern-complex-type (type-spec errorp environment)
    (with-table (class 'limited-complex-type type-spec)
		(let* ((spec (or (first type-spec) '*))
		       (part-type (if (eq spec '*)
				      't
				      (upgraded-complex-part-type
				       spec environment))))
		  (if (eq part-type 't)
		      (find-type 'complex)
		      (make-instance
		       class
		       :name `(complex ,part-type)
		       :part-type (expand-type part-type
					       errorp
					       environment))))))

  (defun intern-array-type (spec errorp environment)
    (with-table (class 'limited-array-type spec)
		(destructuring-bind (type &optional (element-type '*) (dimensions '*))
		    spec
		  (let ((base (find-type (if (eq type 'simple-array)
					     'simple-array
					     'array))))
		    (cond
		     ((and (eq element-type '*) (eq dimensions '*))
		      base)
		     ((eq element-type '*)
		      (expand-type `(or (,type bit ,dimensions)
					(,type base-char ,dimensions)
					(,type character ,dimensions)
					(,type t ,dimensions))
				   errorp environment))
		     (t
		      (let ((element-type (upgraded-array-element-type
					   element-type environment))
			    (dimensions
			     (cond ((eq dimensions '*) '*)
				   ((listp dimensions) dimensions)
				   (t (make-list dimensions
						 :initial-element '*)))))
			(if (equal dimensions '(*))
			    (find-type
			     (element-vector-type type element-type))
			    (make-instance
			     class
			     :base-type base
			     :element-type element-type
			     :dimensions dimensions
			     :name `(,type ,element-type ,dimensions))))))))))

  (defun intern-numeric-type (spec errorp environment)
    (let* ((base (car spec))
	   (base-type (expand-type base errorp environment))
	   (base-name (type-name base-type)))
      (with-table (class (if (eq base-name 'character)
			     'limited-character-type
			     'limited-numeric-type)
			 spec)
		  (destructuring-bind (&optional (low '*) (high '*))
		      (cdr spec)
		    (let ((min1 (bindingform-name low))
			  (max1 (bindingform-name high)))
		      (let ((min (unless (eq min1 '*) min1))
			    (max (unless (eq max1 '*) max1))
			    (inclusive-min (not (consp low)))
			    (inclusive-max (not (consp high))))
			(when (member base-name '(integer character))
			  (unless inclusive-min 
			    (setf inclusive-min t
				  min (1+ min)))
			  (unless inclusive-max
			    (setf inclusive-max t
				  max (1- max)))
			  (setf spec `(integer ,(or min '*) ,(or max '*))))
			(when (and (eq base-name 'character)
				   (or (and min (< min 0))
				       (and max (< max 0))))
			  (setf min max
				inclusive-min nil))
			(macrolet ((code-val (val)
					     `(if (eq base-name 'character)
						  (code-char ,val)
						  ,val)))
			  (cond ((and (null min) (null max))
				 base-type)
				((and min max
				      (or (< max min)
					  (and (= min max)
					       (not (and inclusive-min
							 inclusive-max)))))
				 (union-types nil))
				(t
				 (make-instance
				  class
				  :base-type base-type
				  :min min
				  :max max
				  :inclusive-min inclusive-min
				  :inclusive-max inclusive-max
				  :name `(,base-name
					  ,@(rest spec)))))))))))))

;;; (eql <number>) or (eql <character>) become limited-numeric-types,
;;; otherwise, use intern-eql-specializer.
(defun intern-eql-specializer1 (object)
  (if (or (realp object) (characterp object))
      (let* ((name (class-name (class-of object)))
	     (obj (if (eq name 'character)
		      (char-code object)
		      object)))
	(intern-numeric-type
	 (list name obj obj)
	 t nil))
      (intern-eql-specializer object)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;               UTILITIES                                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun illegal-discrimination-error (type)
  (errorp "Type specifier ~s cannot be used for discrimination."
	  (type-name (type-name type))))

(defun union-types (type-specifiers &optional (errorp t) environment)
  (let ((union (intern-type-union)))
    (dolist (type type-specifiers union)
      (setq union (type-union union
			      (expand-type type errorp environment)
			      environment)))))



;;; We could intersect against a combination starting with #<Class T>,
;;; but that might involve all sorts of long searches through subclasses.

;;; The special cases for ()=>t and (x)=>x is to avoid doing work
;;; before the type metaobjects are defined.
(defun intersect-types (type-specifiers &optional (errorp t) environment)
  (cond ((null type-specifiers) t-classobj)
	((rest type-specifiers)
	 (let ((intersection (expand-type (car type-specifiers) errorp environment)))
	   (dolist (type (cdr type-specifiers) intersection)
	     (setq intersection (type-intersection
				 intersection
				 (expand-type type errorp environment)
				 environment)))))
	(t (first type-specifiers))))
	 


(defun option-type (option)
  (if (eq option '*) 't option))

(defun meets-low (x type &optional (inclusive-x t))
  (let ((lower (limited-type-min type)))
    (if lower
	(or (and (or (not inclusive-x)
		     (limited-type-inclusive-min type))
		 (= x lower))
	    (> x lower))
	t)))
(defun meets-high (x type &optional (inclusive-x t))
  (let ((upper (limited-type-max type)))
    (if upper
	(or (and (or (not inclusive-x)
		     (limited-type-inclusive-max type))
		 (= x upper))
	    (< x upper))
	t)))

(defun integerize-limit (x &optional up)
  (if (eq x '*) x
    (let ((n (if up
		 (ceiling (bindingform-name x))
		 (floor (bindingform-name x)))))
      (if (consp x) (list n) n))))

(defun rationalize-limit (x)
  (if (eq x '*) x
    (let ((n (rationalize (bindingform-name x))))
      (if (consp x) (list n) n))))
(defun float-limit (x &optional double)
  (if (eq x '*) x
    (let ((n (coerce (bindingform-name x)
		     (if double 'double-float 'single-float))))
      (if (consp x) (list n) n))))

(defun complemented-pair-p (type potential-complement known-complement-p)
  (when (or known-complement-p
	    (typep potential-complement 'type-complement)
	    (when (typep type 'type-complement)
	      (rotatef type potential-complement)
	      t))
    (let ((complement (type-complement-type potential-complement)))
      (and (subtypep complement type)
	   (subtypep type complement)))))
  

(defun combine-complement (complement type resultp)
  (when (complemented-pair-p type complement t)
    (if resultp
	t-classobj
	(intern-type-union))))

;;; The pseudo deftypes could be defined using deftype if they were not
;;; also the names of classes.

(defun Expand-Type (type-specifier &optional (errorp t) environment)
  (macrolet ((deftype1 (lambda-list &body body)
	       `(destructuring-bind
		    ,(loop for parameter in lambda-list
			   with optp = nil
			   when (eq parameter '&optional)
			   do (setq optp t)
			   collect (if (and optp (symbolp parameter)
					    (not (lambda-list-keyword-p
						  parameter)))
				       `(,parameter '*)
				       parameter))
		    type-specifiers
		  ,@body))
	     (deftype (lambda-list &body body)
	       `(expand-type (deftype1 ,lambda-list ,@body)
			     errorp environment)))
    ;; Hand expansion of TYPECASE:
    (case (class-name-of type-specifier)
      (NULL (union-types nil))
      (SYMBOL (expand-deftype type-specifier errorp environment))
      (CONS (let ((type-specifiers type-specifier))
	      (case (pop type-specifiers)
		(NOT (deftype1 (type) (type-complement type environment)))
		(EQL (deftype1 (object) (intern-eql-specializer1 object)))
		(SATISFIES
		 (deftype1 (predicate) (intern-type-predicate predicate)))
		(MEMBER
		 (union-types (mapcar #'intern-eql-specializer1 type-specifiers)))
		(OR (union-types type-specifiers errorp environment))
		(AND (intersect-types type-specifiers errorp environment))

		((INTEGER RATIO CHARACTER SINGLE-FLOAT DOUBLE-FLOAT
			  short-float long-float)
		 (intern-numeric-type type-specifier errorp environment))
		((ARRAY SIMPLE-ARRAY)
		 (intern-array-type type-specifier errorp environment))
		(CONS (intern-cons-type type-specifiers errorp environment))
		(COMPLEX (intern-complex-type type-specifiers errorp environment))
		(FUNCTION (intern-function-type type-specifiers errorp environment))
		(VALUES (intern-values-type type-specifiers errorp environment))

		(RATIONAL
		 (deftype  (&optional low high)
		   `(or (integer ,(integerize-limit low t) ,(integerize-limit high nil))
			(ratio ,low ,high))))
		(FLOAT
		 (deftype (&optional low high)
		   `(or (single-float ,(float-limit low) ,(float-limit high))
			(double-float ,(float-limit low t) ,(float-limit high t)))))
		(REAL
		 (deftype (&optional low high)
		   `(or (rational ,(rationalize-limit low) ,(rationalize-limit high))
			(float ,low ,high))))

		(SIMPLE-BASE-STRING
		 (deftype (&optional s) `(simple-array base-char (,s))))
		(BASE-STRING
		 (deftype (&optional s) `(array base-char (,s))))
		(SIMPLE-STRING
		 (deftype (&optional s)
		   `(or (simple-array base-char (,s))
			(simple-array character (,s)))))
		(STRING
		 (deftype (&optional s)
		   `(or (array character (,s)) (array base-char (,s)))))
		(SIMPLE-BIT-VECTOR
		 (deftype (&optional s) `(simple-array bit (,s))))
		(BIT-VECTOR
		 (deftype (&optional s) `(array bit (,s))))
		(SIMPLE-VECTOR
		 (deftype (&optional s) `(simple-array t (,s))))
		(VECTOR
		 (deftype (&optional element-type s)
		   `(array ,element-type (,s))))

		(t (expand-deftype type-specifier errorp environment)))))
      (t (if (typep type-specifier 'type)
	     type-specifier
	     (error 'type-error :datum type-specifier
		    :expected-type 'type-specifier))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                    DEFTYPE                                   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun derived-type-p (class)
  (classp class (find-type 'derived-type)))

(defun expand-deftype1 (function type-specifier errorp environment)
  (expand-type (funcall function
		      (if (consp type-specifier)
			  type-specifier
			  (list type-specifier))
		      environment)
	     errorp environment))
  
(defun expand-deftype (type-specifier &optional (errorp t)
				      environment)
  (let* ((name (bindingform-name type-specifier))
	 (deftype (or (find-type name errorp environment)
		      (if (consp type-specifier)
			  (ensure-deftype name)
			  (make-forward-referenced-class name)))))
    (if (derived-type-p deftype)
	(let ((table (class-instances deftype)))
	  (or (gethash type-specifier table)
	      (setf (gethash type-specifier table)
		    (expand-deftype1
		     (derived-type-expansion-function deftype)
		     type-specifier errorp environment))))
	(if (consp type-specifier)
	    (error
 "The list-form type specifier ~s cannot be used because ~s is not a derived type."
 type-specifier deftype)
	    deftype))))

(defun ensure-deftype (name &rest keys)
  (apply #'ensure-class name :metaclass 'derived-type keys))

;;; DEFTYPE is in dev-meth.lisp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                    TYPE-OF                                   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; All the cases except the last try to accommodate our strict
;;; interpretation of the ridiculous specification that elements of
;;; the extensive lists of built-in types return (ideally portable)
;;; recognizable subtypes.
(defun TYPE-OF (object)
  (let* ((class (class-of object))
	 (name (or (proper-name class nil) class)))
    (case name
      (integer `(integer ,object ,object))
      (character (cond ((standard-char-p object) 'standard-char)
		       ((<= (char-code object) #xff) 'base-char)
		       (t 'extended-char)))
      (symbol (if (keywordp object) 'keyword name))
      #+old (general-vector '(vector t))
      (complex-vector '(vector t))
      (complex-base-string 'base-string)
      (complex-extended-string 'string)
      (complex-bit-vector 'bit-vector)
      (complex-array `(array ,(array-element-type object)))
      ((simple-array array) `(,name ,(array-element-type object)))
      (t name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                    UPGRADING                                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun UPGRADED-COMPLEX-PART-TYPE (typespec &optional environment)
  (let ((type (type-intersection typespec 'real environment)))
    (cond ((subtypep type 'rational environment) 'rational)
	  ((subtypep type 'single-float environment) 'single-float)
	  ((subtypep type 'double-float environment) 'double-float)
	  ((subtypep type 'real environment) 't)
	  (t (error "~s is not a subtype of REAL." typespec)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 4.8 TYPE CONVERSION FUNCTION                                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((sequence (find-type 'sequence)))
  (defun COERCE (object result-type)
    (let ((type (expand-type result-type)))
      (cond ((typep object type) object)
	    ((subtypep type sequence)
	     (coerce-to-sequence1 object type))
	    ;; Here we take a (fairly) strict interpretation of the spec.
	    ;; (Expanded) type-specifier must be exact, not a subtype.
	    (t (case (type-name type)
		 (character (character object))
		 (complex (complex object))
		 ((float single-float short-float) (float object 1.0f0))
		 ((double-float long-float) (float object 1.0d0))
		 (function
		  (let ((potential (when (and (not (car-eq object 'lambda))
					      (fboundp object))
				     (fdefinition object))))
		    ;; ANSI forbids special operators and macros.
		    ;; + Special-operators are not fbound in Eclipse.
		    ;; + We don't call macro-function because that
		    ;;   might not be available in a runtime library.
		    (if (and potential
			     (not (macro-function-p potential)))
			potential
			(enclose object))))
		 (t (error 'type-error :datum object :expected-type result-type))))))))

