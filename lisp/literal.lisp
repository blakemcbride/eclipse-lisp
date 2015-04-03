;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; C CONTAINERS

;;; These are real tagged Lisp objects that serve as containers for C
;;; objects.  They might go away later.

(defstruct (ec:void (:copier nil) (:predicate nil)
		    (:constructor nil)))

(defstruct (ec:long (:copier nil) (:predicate nil)
		    (:constructor make-long (value))
		    (:conc-name long-)
		    (:include ec:void))
  (value 0 :type integer :read-only t))

(defstruct (ec:int (:copier nil) (:predicate nil)
		   (:constructor make-int (value))
		   (:conc-name int-)
		   (:include ec:long))
  (value 0 :type integer :read-only t))

(defstruct (ec:wint_t (:copier nil) (:predicate nil)
		      (:constructor make-wint (value))
		      (:conc-name wint-)
		      (:include ec:long))
  (value 0 :type integer :read-only t))

(defstruct (ec:char (:copier nil) (:predicate nil)
		    (:constructor make-char
				  (char-value &aux (value (char-int char-value))))
		    (:conc-name nil)
		    (:include ec:int))
  (char-value #\null :type base-char :read-only t))

(defstruct (ec:wchar_t (:copier nil) (:predicate nil)
		       (:constructor make-wchar
				     (wchar-value &aux (value (char-int wchar-value))))
		       (:conc-name nil)
		       (:include ec:int))
  (wchar-value #\null :type character :read-only t))

(defstruct (ec:float (:copier nil) (:predicate nil)
		      (:constructor make-float (value))
		      (:conc-name float-)
		    (:include ec:void))
  (value 0.0f0 :type single-float :read-only t))

(defstruct (ec:double (:copier nil) (:predicate nil)
		      (:constructor make-double (value))
		      (:conc-name double-)
		    (:include ec:void))
  (value 0.0d0 :type double-float :read-only t))

(defstruct (charp (:copier nil) (:predicate nil)
		      (:constructor make-charp (value))
		    (:include ec:void))
  (value "" :type simple-base-string :read-only t))

#+not-yet
(defstruct (ec:wide-string (:copier nil) (:predicate nil)
			   (:constructor make-wcharp (value))
			   (:conc-name wcharp-value)
			   (:include ec:void))
  (value "" :type simple-extended-string :read-only t))

(let ((void (find-type 'ec:void)))
  (defmethod implementation-primitive-p ((dynamic-env t) lisp-type)
    (when lisp-type
      (subtypep lisp-type void))))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Load Forms For Lisp Primitives

(defmethod make-load-form ((obj CHARACTER) &optional environment)
  (declare (ignore environment))
  `(int-character ,(make-char obj)))

(defmethod make-load-form ((obj SIMPLE-BASE-STRING) &optional environment)
  (declare (ignore environment))
  `(charp-simple-base-string ,(make-charp obj)))

;; We use c:double literals in order to avoid conflicts between the
;; way ANSI and K&R C handle float arguments to functions.
(defmethod make-load-form ((obj SINGLE-FLOAT) &optional environment)
  (declare (ignore environment))
  `(double-single-float ,(make-double (float obj 1.0d0))))

(defmethod make-load-form ((obj DOUBLE-FLOAT) &optional environment)
  (declare (ignore environment))
  `(double-double-float ,(make-double obj)))

(defmethod make-load-form ((obj RATIO) &optional environment)
  (declare (ignore environment))
  `(make-ratio ,(numerator obj) ,(denominator obj)))

(defmethod make-load-form ((obj COMPLEX) &optional environment)
  (declare (ignore environment))
  `(make-complex ,(realpart obj) ,(imagpart obj)))

;; Convert string to base-string when possible.
(defun compactify-element-type (string)
  (if (eq (array-element-type string) 'character)
      (loop for i from 0 below (length string)
	    when (>= (char-int (char string i)) base-char-code-limit)
	    return string
	    finally (return (concatenate 'base-string string)))
      string))

;;; See HyperSpec/Issues/iss063-writeup.html
;;; This is the "home-package" model, as opposed to the
;;; "current-package" model. 
(defmethod make-load-form ((obj SYMBOL)
			   &optional environment)
  (declare (ignore environment))
  (let ((pkg (symbol-package obj))
	(name (compactify-element-type (symbol-name obj))))
    (cond ((eq pkg *keyword-package*) `(make-keyword ,name))
	  (pkg `(intern ,name ,pkg))
	  (t `(make-symbol ,name)))))

(defun xint-forms (obj xint size &optional environment)
  (declare (ignore environment))
  `(progn ,@(loop for i from 0 below size
		  collect `(set-digitref ',obj ,i 
					 ,(digitref xint i)))))

(defmethod make-load-form ((obj INTEGER) &optional environment)
  (declare (ignore environment))
  (if #+machine-compile (fixnump obj)
      #-machine-compile (typep obj 'fixnum)
      `(int-fixnum ,(make-int obj))
      (let* ((xint (bignum-xint obj))
	     (size (xint-size xint)))
	(values `(xint-bignum (make-xint ,size)) 
		(xint-forms obj xint size)))))

(defmethod make-load-form ((obj BIT-VECTOR) &optional environment)
  (declare (ignore environment))
  (let* ((length (length obj)))
    (values `(make-bit-vector ,length)
	    (xint-forms obj obj (ceiling length digit-size)))))

(defmethod make-load-form ((obj SIMPLE-VECTOR) &optional environment)
  (declare (ignore environment))
  (let ((length (length obj)))
    (values `(make-general-vector ,(fixnum-index length))
	    `(progn ,@(loop for i from 0 below length
			    and elt across obj
			    collect `(setf (svref ,obj ,i) ',elt))))))

;;; Ideally, this is never used, as each specialized simple-vector
;;; should have its own load-form mechanism.
(defmethod make-load-form ((obj SIMPLE-BASIC-VECTOR) &optional environment)
  (declare (ignore environment))
  (let ((length (length obj)))
    (values `(make-array ,(fixnum-index length)
			 :element-type ',(array-element-type obj))
	    `(progn ,@(loop for i from 0 below length
			    and elt across obj
			    collect `(setf (elt ,obj ,i) ',elt))))))


(defmethod MAKE-LOAD-FORM ((obj SIMPLE-ARRAY) &optional environment)
  (declare (ignore environment))
  `(make-simple-array ,(class-of obj)
		      ,(simple-array-rank obj)
		      ',(array-dimensions obj)
		      ',(simple-array-contents obj)))

;;; This is not required by ANSI.
(defmethod MAKE-LOAD-FORM ((obj ARRAY) &optional environment)
  (declare (ignore environment)) 
  `(make-complex-array ,(class-of obj)
		       ,(simple-array-rank obj)
		       ',(array-dimensions obj)
		       ',(simple-array-contents obj)
		       ,(complex-array-offset obj)
		       ,(complex-array-fill-pointer obj)))

;;; IWBNI we defined a specialized load-form for
;;; Open-Address-Hash-Table that handled the key and values tables as
;;; a unit.  
(defmethod MAKE-LOAD-FORM ((object HASH-TABLE) &optional environment)
  (declare (ignore environment)) 
  (values
   `(make-hash-table :test ',(hash-table-test object)
		     :size ,(hash-table-size object)
		     :rehash-size ,(hash-table-rehash-size object)
		     :rehash-threshold ,(hash-table-rehash-threshold))
   `(progn
      ,@(loop for key being each hash-key of object
	      using (hash-value value)
	      collect `(setf (gethash ,key ,object) ',value)))))


;;; Note that this won't terminate on circular obj's which do NOT
;;; contain ref!  This method is too picky because once a branch is
;;; ciruclar, the sub branches are ciruclar, too (even though we break
;;; the branch at the top).
(defun circularp (ref obj)
  (or (eq ref obj)
      (typecase obj
	(cons (or (circularp ref (car obj))
		  (circularp ref (cdr obj))))
	((or structure-object standard-object)
	 (let ((class (class-of obj)))
	   (with-update
	    (obj class)
	    (loop with slots = (standard-instance-slots obj)
		  for i from 0 below (n-instance-slots
				      (class-slots class))
		  thereis (circularp ref (get-slot slots i))))))
	(t nil))))

(defmethod make-load-form ((obj CONS) &optional environment
			   &aux car-code cdr-code)
  (declare (ignore environment))
  (destructuring-bind (car . cdr) obj
    (when (circularp obj car)
      (setf car-code `((rplaca ',obj ',car))
	    car 'nil))
    (when (circularp obj cdr)
      (setf cdr-code `((rplacd ',obj ',cdr))
	    cdr 'nil))
    (values `(cons ',car ',cdr)
	    (when (or car-code cdr-code)
	      `(progn ,@car-code ,@cdr-code)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                     CONSTANTS                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Names for constants
(defstruct (constant-id
	    (:print-function (lambda (x s l)
			       (declare (ignore l))
			       (let ((*print-circle* nil))
				 (print-unreadable-object (x s)
				   (format s #"~a ~a"
					   (constant-id-identifier x)
					   (constant-id-string x))))))
	    (:constructor make-constant-id
			  (identifier format
			   &rest args
			   &aux (string
				 (make-constant-string format args)))))
  identifier string value) 

(defstruct (numeric-constant-id
	    (:include constant-id)
	    (:constructor make-numeric-constant-id
			  (identifier format
				      &rest args
				      &aux (string
					    (let ((*read-default-float-format*
						   'single-float))
					      (make-constant-string format args)))))))
	    	    			       			      

(defparameter c-package (find-package :eclipse-c))
(defun C-SYMBOL-P (symbol)
  (and (symbolp symbol) (eql (symbol-package symbol) c-package)))


(defun make-constant-name (dynamic-env obj)
  (etypecase obj
    (symbol
     (let ((cp (c-symbol-p obj)))
       (if (and (symbol-package obj) (not cp))
	   obj
	   (make-constant-id (if cp "CC" "SYM") #"~d-~a"
			     (count-gensyms dynamic-env (symbol-name obj))
			     (symbol-name obj)))))
    (character (make-constant-id "C" (or (char-name obj) obj)))
    (integer (make-numeric-constant-id "I" obj))
    (float (make-numeric-constant-id (if (typep obj 'single-float) "F" "D") obj))
    (ratio (make-numeric-constant-id "R" #"~d/~d" (numerator obj) (denominator obj)))
    (complex (make-numeric-constant-id "X" #"~aplus~aI" (realpart obj) (imagpart obj)))
    (package (make-constant-id "PKG" (package-name obj)))
    (cons (make-constant-id "CONS" (count-constants dynamic-env 'cons)))
    (string
     (if (< (length obj) 20)
	 ;; In order to handle strings that look like format control
	 ;; strings, we need to pass them through ~a.  
	 (make-constant-id "STR" #"~a_~d" obj
			   (count-constants dynamic-env 'string))
	 (make-constant-id "STRn" (count-constants dynamic-env 'string))))
    (array (if (typep obj 'bit-vector)
	       (make-constant-id "BV" (count-constants dynamic-env 'bit-vector))
	       (make-constant-id "A" (count-constants dynamic-env 'array))))
    (hash-table (make-constant-id "H" (count-constants dynamic-env 'hash-table)))
    (pathname (make-constant-id "P" (count-constants dynamic-env 'pathname)))
    (random-state (make-constant-id "RAND" (count-constants dynamic-env 'random-state)))
    (structure-object (make-constant-id "STRUCT" #"~d-~a"
					(count-constants dynamic-env (type-of obj))
					(type-of obj)))
    (standard-object (let ((class-name (class-name (class-of obj))))
		       (make-constant-id "INST"
					 #"~d-~a"
					 (count-constants dynamic-env (type-of obj))
					 class-name)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun initialize-common-interned-objects (dynamic-env)
  (loop for obj in *common-interned-objects*
	nconc (list obj (make-constant-name dynamic-env obj))))
	
;;; Public interface
#+old-code ;see walk.lisp
(setq *common-interned-objects*
      (loop for obj in `(nil		;t *package*
			 ,unbound
			 ,eoa-flag
			 ,null-hook-flag
			 #|,(find-package :common-lisp)
			  ,(find-package :common-lisp-user)
			  ,(find-package :keyword)
			  ,(find-package :eclipse-internals)|#)
	    append (list obj (make-constant-name nil obj))))

;;; While compiling the initialization form, the constant-name stores not
;;; only the name, but the creation code as well.  If we don't try to
;;; intern ourself while compiling our initialization form, then there is
;;; no circular reference and we add both the creation and initialization
;;; code together, and reset the constant-name properly.  However, if we
;;; do intern ourself while compiling our initialization form, then we
;;; must add the creation code right away.  This gives the required ANSI
;;; behavior. 

(defvar *current-object* nil)

#+lisp-host
(defun Cl-SYMBOL-P (symbol)
  (when (symbolp symbol)
    (let* ((name (symbol-name symbol))
	   (cl (or (find-symbol name :host)
		   #+cmu
		   (find-symbol name :extensions)))
	   (ecl (when (and cl (eql cl symbol))
		  (find-symbol name :eclipse))))
      (when ecl (not (eq cl ecl))))))


(defun WALK-LITERAL (dynamic-env obj &optional lexical-env)
  #+lisp-host
  (when (cl-symbol-p obj)
    (return-from walk-literal
      (walk-literal dynamic-env (intern (symbol-name obj) :eclipse)
		    lexical-env)))
  (let ((ref (get-constant-name dynamic-env obj)))
    (cond ((eq ref not-yet-interned)
	   (multiple-value-bind (create init) (make-load-form obj lexical-env)
	     (let ((*current-object* obj)
		   (name (make-constant-name dynamic-env obj)))
	       (set-constant-name dynamic-env name obj)
	       (let ((create-code (walk create nil dynamic-env
					(add-target (make-constant-target :binding name)
						    (make-targets))
					nil))
		     (init-code nil))
		 (when init
		   (set-constant-name dynamic-env (list name create-code) obj)
		   (setf init-code (walk init nil dynamic-env
					 (make-targets) nil))
		   (unless (consp (get-constant-name dynamic-env obj))
		     (setf create-code nil)))
		 (add-init-code dynamic-env create-code)
		 (when init-code (add-init-code dynamic-env init-code))
		 (set-constant-name dynamic-env name obj)))))
	  ((consp ref)
	   (destructuring-bind (name create-code) ref
	     (unless (eql obj *current-object*)
	       (add-init-code dynamic-env create-code)
	       (set-constant-name dynamic-env name obj))
	     name))
	  (t ref))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EXAMINING INTERNED CONSTANTS

(defun get-constant-name (dynamic-env obj)
  (let ((basic (getf (file-compiler-env-basic-constants dynamic-env)
		     obj not-yet-interned)))
    (if (eql basic not-yet-interned)
	(or (get-external-constant-name dynamic-env obj)
	    (gethash obj (file-compiler-env-constants dynamic-env)
		     not-yet-interned))
      basic)))

(defun add-external-variable (dynamic-env obj)
  (setf (gethash obj (file-compiler-env-external-constants dynamic-env))
	(make-constant-name dynamic-env obj)))

;;; ISSUE: make-constant-name only looks in
;;; FILE-COMPILER-ENV-CONSTANTS, and not
;;; C-FILE-COMPILER-ENV-EXTERNAL-CONSTANTS when counting constants.
;;; This is a problem if we want get-external-constant-name to return
;;; non-nil values for counted literals.
(defun get-external-constant-name (dynamic-env obj)
  (when (and (symbolp obj)
	     (member (symbol-package obj)
		     (load-time-value `(,(find-package :eclipse)
					,(find-package :cl)))))
    (or (gethash obj (file-compiler-env-external-constants dynamic-env))
	(add-external-variable dynamic-env obj))))
		   
(defun set-constant-name (dynamic-env name obj)
  (setf (gethash obj (file-compiler-env-constants dynamic-env)) name))

(defun count-constants (dynamic-env type &aux (i 0))
  (maphash #'(lambda (key value)
	       (declare (ignore value))
	       (when (typep key type) (incf i)))
	   (file-compiler-env-constants dynamic-env))
  i)

(defun count-gensyms (dynamic-env name &aux (i 0))
  (maphash #'(lambda (key value)
	       (declare (ignore value))
	       (when (and (symbolp key)
			  (string= (symbol-name key) name))
		 (incf i)))
	   (file-compiler-env-constants dynamic-env))
  i)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LOAD FORMS  See loaders.lisp
;;; IWBNI this:
;;; - Created data in the data segment rather than the heap, where
;;;   possible. 
;;; - Kept like objects together so they could be declared together.
;;; - Allowed a more efficient implementation of
;;;   count-constants/count-gensyms. 
;;; - coalesced all the objects permissible by ANSI.  We currently
;;;   coalesce the following only when they are equal:
;;;    - hash tables - can be coalesced if equalp
;;;    - general arrays - can be coalesced if equalp
;;;    - random-state 
