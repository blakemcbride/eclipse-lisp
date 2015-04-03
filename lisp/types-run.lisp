;;; Things the type system needs at runtime.
;;; The only way an application could do without this is if every
;;; typecase, type operation, and generic-function were decided at run
;;; time.  (And the I/O system is not designed to even try to do
;;; that.)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;n
;;;              CLASS-OF                                        ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun CLASS-OF (object) (object-class object))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                  REGISTRATION                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun Find-Type (name &optional (errorp t) environment)
  (declare (ignore environment))
  (or (system-property name 'type nil)
      (when errorp (error "No type named ~s." name))))
(defun (setf Find-Type) (class name &optional
			       errorp environment)
  (declare (ignore errorp environment))
  (system-property-setter name 'type class))


;;; These two should never be used by system (or good user) code.
;;; They are provided only for compatibility with a poorly designed
;;; part of the ANSI spec.  Use FIND-TYPE instead.
(defun FIND-CLASS (symbol &optional (errorp t) environment)
  (let ((type (find-type symbol nil environment)))
    (if (and type (not (or (derived-type-p type)
			   (abstract-structure-class-p type))))
	type
	(when errorp (error "No class named ~s." symbol)))))
(defun (SETF FIND-CLASS) (new-value symbol
				    &optional errorp environment)
  (let ((type (find-type symbol nil environment)))
    (when type
      (cond ((derived-type-p type)
	     (cerror "Delete derived type specifier ~s."
		     "~s names a derived type specifier." symbol))
	    ((abstract-structure-class-p type)
	     (cerror "Delete abstract structure ~s."
		     "~s names an abstract structure." symbol)))))
  (setf (find-type symbol errorp environment) new-value))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;              INSTANCE ACCESS PROTOCOL                        ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The MOP documents these to be undefined if location is not "a
;;; location" of directly accessible slot. The MOP defines location as
;;; being assigned as consecutive non-negative integers based on the
;;; order of the directly accessible slots in the list of effective
;;; slots.  Finally, the MOP syas portable programs are permitted only
;;; to reorder the list of effective slots, not to assign slot
;;; locations directly.  Thus we should be allowed to insist that
;;; location be a fixnum.

(defun STANDARD-INSTANCE-ACCESS (instance location)
  (get-slot (standard-instance-slots instance) (fixnum-index location)))
(defun FUNCALLABLE-STANDARD-INSTANCE-ACCESS (instance location)
  (get-slot (standard-instance-slots instance) (fixnum-index location)))

