;; We know cmu doesn't implement it, and we don't want warnings.
#+cmu (declaim (declaration dynamic-extent))


;;; C primitives or true lisp object (yet to be determined) ;;;;;;;;

#+cmu (deftype boolean () t)
#+cmu (deftype eclipse:boolean () t)
(deftype eclipse:null () 'null)

(deftype eclipse:index ()
  #+cmu `(mod ,most-positive-fixnum)
  #+excl 'fixnum)

(defvar eclipse::unbound-flag 'eclipse::unbound)

;;; C primitives ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype ec:void () 't)
(deftype ec:int () '(signed-byte 32))
(deftype ec:wint_t () '(signed-byte 32))
(deftype ec:char () 'character)
(deftype ec:wchar_t () 'character)
(deftype ec:long () '(signed-byte 32))
(deftype ec:float () 'single-float)
(deftype ec:double () 'double-float)
(deftype eclipse:charp () '(or simple-base-string (member 0)))
(deftype eclipse::slots () 'simple-vector)
(deftype eclipse:object () 't)

(declaim (inline eclipse::make-wrapper
		 eclipse::make-standard-instance
		 eclipse::make-funcallable-standard-instance
		 eclipse::standard-instance-p
		 eclipse::tagged-instance-p)) 

(defmacro locally-fast (&body body)
  `(locally (declare (optimize (speed 3) (debug 0) (safety 0)))
     ,@body))

(defmacro locally-pretty-fast (&body body)
  `(locally (declare (optimize (speed 3) (debug 1) (safety 1)))
     ,@body))
  

(locally-fast
  (defstruct (eclipse::wrapper
	      (:predicate nil)
	      (:copier nil)
	      (:constructor eclipse::make-wrapper
			    (obsolete-slots
			     &aux (hash-key (1+ (random (1- most-positive-fixnum)))))))
    (hash-key nil :type eclipse:index)
    (obsolete-slots nil :type list))

  ;; Anything other than immediate data.
  ;; In C, these are distinguished from immediate data because the low
  ;; order three bits are always clear.

  (defstruct (eclipse::tagged-instance
	      (:predicate eclipse::tagged-instance-p)
	      (:copier nil)
	      (:print-function instance-printer)
	      (:constructor nil))
    class
    (wrapper nil :type eclipse::wrapper))
  #+excl (excl:add-typep-transformer 'eclipse::tagged-instance 'eclipse::tagged-instance-p)

  ;; In C, the constructors for things that include built-in-instance
  ;; have a class slot that is is a pointer to a class object with the
  ;; low order bit turned on.  Object-class uses that to
  ;; distinguish between built-in-instance and standard-instance.
  ;; TAGGED-INSTANCE-CLASS always masks off that bit. 

  (defstruct (built-in-instance
	      (:include eclipse::tagged-instance)
	      (:predicate nil)
	      (:copier nil)
	      (:constructor nil)))

  (defstruct (eclipse::standard-instance
	      (:include eclipse::tagged-instance)
	      (:predicate eclipse::standard-instance-p)
	      (:copier nil)
	      (:constructor eclipse::make-standard-instance
			    (size class wrapper &aux
				  (slots (eclipse::make-slots size))))
	      (:constructor eclipse::make-static-standard-instance
			    (wrapper))
	      (:constructor eclipse::make-standard-instance-from-slots
			    (class wrapper slots))) ;Startup only
    (slots nil :type eclipse::slots))

  (defstruct (eclipse::funcallable-standard-instance
	      (:include eclipse::standard-instance)
	      (:predicate finp)
	      (:copier nil)
	      (:constructor eclipse::make-funcallable-standard-instance
			    (size class wrapper &aux
				  (slots (eclipse::make-slots size))))
	      (:constructor eclipse::make-funcallable-standard-instance-from-slots
			    (class wrapper slots))) ;startup only
    (function nil :type (or function null))
    (backpointer nil)))


;;; True Lisp objects ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype eclipse:character () 'character)
(deftype eclipse:rational () 'rational)
(deftype eclipse:float () 'float)
(deftype eclipse:complex () 'complex)
(deftype eclipse:stream () 'eclipse::standard-instance)
(deftype eclipse::Open-Address-Hash-Table () 'hash-structure)
(deftype eclipse:package () '(or package eclipse::standard-instance))
(deftype eclipse:string ()
  '(or cl:string eclipse::base-char-vector eclipse::extended-char-vector
       eclipse::complex-array))

(deftype host::string () 'eclipse:string)
(deftype eclipse::simple-extended-string () 'eclipse::extended-char-vector)
(deftype eclipse::values (&rest args) `(values ,@args))
(deftype eclipse::or (&rest args) `(or ,@args))


(deftype eclipse:function (&rest ignore)
  (declare (ignore ignore))
  '(or function eclipse::standard-instance))
(deftype eclipse::function-designator ()
  '(or eclipse:function (and symbol (satisfies fboundp))))

(deftype ec:function (&rest args) `(function ,@args))
(deftype ec:macro (args value &optional file)
  (declare (ignore file))
  `(function ,args ,value))


;;; We need the classes defined here because their types are used in
;;; declarations and such, but their real constructors can't be
;;; defined until later.
(locally-pretty-fast
 
(defstruct (eclipse::basic-vector
	    (:include built-in-instance)
	    (:copier nil)
	    (:predicate nil)
	    (:constructor nil))
  (size 0 :type eclipse:index))
(defstruct (eclipse::base-char-vector
	    (:include eclipse::basic-vector)
	    (:copier nil)
	    (:predicate nil)
	    (:constructor make-base-char-vector (class wrapper size contents)))
  (contents "" :type simple-base-string))
(defstruct (eclipse::extended-char-vector
	    (:include eclipse::basic-vector)
	    (:copier nil)
	    (:predicate nil)
	    (:constructor make-extended-char-vector (class wrapper size contents)))
  (contents "" :type simple-string))
(defstruct (eclipse::general-vector
	    (:include eclipse::basic-vector)
	    (:copier nil)
	    (:predicate nil)
	    (:constructor make-general-vector (class wrapper size contents)))
  (contents #() :type simple-vector))
(defstruct (eclipse::digit-vector
	    (:include eclipse::basic-vector)
	    (:copier nil)
	    (:print-function
	     (lambda (object stream level)
	       (when (or (null *print-level*)
			 (< level *print-level*))
		 (if (eq (tagged-instance-class object)
			 (eclipse::find-type 'eclipse::integer))
		     (print-unreadable-object (object stream :type t :identity t)
		       (let ((contents (digit-vector-contents
					object)))
			 (dotimes (i (length contents))
			   (format stream "~4,'0x " (elt contents i)))))
		     (instance-printer object stream level)))))
	    (:predicate nil)
	    (:constructor make-digit-vector (class wrapper size contents)))
  (contents nil :type (cl:simple-array eclipse:index (*))))

(defstruct (eclipse::simple-nd-array
	    (:include built-in-instance)
	    (:copier nil)
	    (:predicate nil)
	    (:constructor make-simple-nd-array (class wrapper rank
						      dimensions contents)))
  (rank 0 :type eclipse:index)
  (dimensions nil :type (cl:simple-array eclipse:index (*)))
  (contents nil :type (or built-in-instance vector)))

(defstruct (eclipse::complex-array
	    (:include eclipse::simple-nd-array)
	    (:copier nil)
	    (:predicate nil)
	    (:constructor make-complex-array (class wrapper rank
						    dimensions
						    contents
						    offset fill-pointer)))
  (offset 0 :type fixnum)
  (fill-pointer 0 :type fixnum))
)


;;; This really wants to be defined after defstructs/deftypes in
;;; host-array.lisp, but it gets used by something in host-classes.lisp.
(defun string (s)
  (typecase s
    (cl:string s)
    (symbol (symbol-name s))
    (character (cl:string s))
    (eclipse::base-char-vector (base-char-vector-contents s))
    (eclipse::extended-char-vector (extended-char-vector-contents s))
    (eclipse::complex-array (subseq (string (simple-nd-array-contents s))
				    (complex-array-offset s)
				    (complex-array-fill-pointer s)))))

