;;; DECLARATIONS
(declaim
 (inline eclipse:xor
	 eclipse::enclosing-environment
	 eclipse:1+ eclipse:1-
	 ))

;;; These are macros and functions used by macros that are necessary to
;;; compile even the earliest files.

;;; When compiling the system using a host Lisp compiler, this file is
;;; compiled first, in the :host package.

;;; When compiling the system using our compiler to generate C, etc.,
;;; this file is compiled first, in the :eclipse package.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                 MACRO HYGENE                                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro eclipse::WITH-UNIQUE-NAMES (vars &body body)
  `(let ,(loop for var in vars
	       collect `(,var (make-symbol ,(symbol-name var))))
     ,@body))

(defmacro eclipse::REBINDING (vars &body body)	;difficult code here!..
  (loop for var in vars
	for name = (make-symbol (symbol-name var))
	collect `(,name ,var) into renames
	collect ``(,,var ,,name) into temps
	finally (return `(let ,renames
			   (eclipse::with-unique-names ,vars
						       `(let (,,@temps)
							  ,,@body))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                 CONSTRUCTORS                                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun eclipse:Make-Name (format &rest args)
  (declare (dynamic-extent args))
  (intern (apply #'format nil (string format) args)))

(defun eclipse::make-name-in-package (pkg format &rest args)
  (declare (dynamic-extent args))
  (let ((*package* (find-package pkg)))
    (apply #'eclipse:make-name format args)))

(defun eclipse::make-binding-list (names inits)
  (mapcar #'list names inits))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;          ACCESSORS AND OTHER EXTRACTION                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Returns 3 values: declaration specs, body and documentation-string.
;;; This assumes doc is a literal string, which Steele specifically
;;; says it must be for defvar, etc., but which is not explicitly
;;; stated for defun/defmacro.  We assume it must always be a string.
;;; Also, Steele does not say whether doc can be the result of
;;; macro-expansion.  We assume that, like declarations, it may not be.

(defun eclipse:Find-Declarations (body ignore-doc?)
  (if (endp body)
      ;; Explicitly returning 3 nil values makes CMU compiler happy.
      (values nil nil nil)
      (let ((d (car body)))
	(cond ((and (stringp d) (not ignore-doc?) (cdr body))
	       (multiple-value-bind (decls b)
		   (eclipse:find-declarations (cdr body) t)
		 (values decls b d)))
	      ((eclipse:car-eq d 'declare)
	       (multiple-value-bind (decls b doc)
		   (eclipse:find-declarations (cdr body) ignore-doc?)
		 (values (append (cdr d) decls) b doc)))
	      (t (values nil body nil))))))		   

;;; These are setf expanders rather than setf functions because value
;;; could be eoa/unbound and a setf function would signal an error for
;;; this during argument parsing.  SET-SLOT is an ec:function, which
;;; does no argument checking.
(defsetf eclipse:standard-instance-access (instance location) (value)
  `(eclipse::set-slot (eclipse::standard-instance-slots ,instance)
		      (eclipse::fixnum-index ,location)
		      ,value))
(defsetf eclipse:funcallable-standard-instance-access (instance location) (value)
  `(eclipse::set-slot (eclipse::standard-instance-slots ,instance)
		      (eclipse::fixnum-index ,location)
		      ,value))

