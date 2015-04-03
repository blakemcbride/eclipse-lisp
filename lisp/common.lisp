;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                ERROR CHECKING                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun eclipse:missing-args (v)
  (eclipse:signal-program-error
   "Too few arguments.~@[ Argument ~s not supplied.~]" v))

(defun eclipse:extra-args (args)
  (eclipse:signal-program-error
   "Too many arguments.~@[ Extra arguments are ~s.~]" args))

(defun eclipse:key-not-allowed (key &optional format &rest args)
  (eclipse:signal-program-error
   "Illegal key ~s~@[ ~?~]." key format args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                   PREDICATES                                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconstant eclipse:LAMBDA-LIST-KEYWORDS
  '(&allow-other-keys &aux &body &environment &key &optional &rest &whole))

(defun eclipse:Lambda-List-Keyword-P (parameter-spec)
  (member parameter-spec eclipse:lambda-list-keywords))

(defun eclipse:CAR-EQ (x expected-car)
  (if (consp x)
      (eq (car x) expected-car)
      nil))

(defun eclipse:XOR (a b) (if a (not b) b))

(defun eclipse::setf-function-name-p (name)
  (when (consp name)
    (destructuring-bind (&optional key block &rest more) name
      (and (null more) (symbolp block) (member key '(eclipse:setf cl:setf))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;             ACCESSORS                                        ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun eclipse:key-arg (item plist)
  (do ((list plist (cddr list)))
      ((endp list) nil)
    (when (eq (car list) item) (return (cdr list)))))

(defun eclipse:Bindingform-Name (bindingform)
  (if (consp bindingform) (car bindingform) bindingform))

(defun eclipse:Bindingform-Initform (bindingform &optional default)
  (if (consp bindingform) (cadr bindingform) default))

(defun eclipse::function-name-key (name &optional
					(setfp (eclipse::setf-function-name-p name)))
  (cond (setfp (second name))
	((symbolp name) name)
	(t (error 'type-error :expected-type 'function-name :datum name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;             OTHER UTILITIES                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Return a list suitable for mapping over.
;;; This works through recursive calls because the inner call can only
;;; occur after the car has already been extracted.  No matter how
;;; many recursive calls are then made, the cdr will still be nil.
(let ((*symbol-list* (list nil)))
  (defun eclipse:LIST-ARG (list)
    (if (listp list) list
      (rplaca *symbol-list* list))))


;;; From DEFCONSTANT spec: "The consequences are undefined if there
;;; are any bindings of the variable named by name at the time
;;; defconstant is executed or if the value is not EQL to the value of
;;; initial-value. ... An implementation may choose to evaluate the
;;; value-form at compile time, load time, or both. Therefore, users
;;; must ensure that the initial-value can be evaluated at compile
;;; time ... and that it always evaluates to the same value."

;;; If the symbol named by a defconstant form is unbound (as is
;;; usually the case when the defconstanst form is first evaluated)
;;; the new value is assigned.

;;; If *constant-check-hook* is NIL and the symbol is already bound,
;;; the existing value is left in place and the initial-value form is
;;; not evaluated again.  This analogous to the behavior of defvar,
;;; and is consistent with the ANSI requirement that defconstant not
;;; be used to CHANGE constant values.

;;; Otherwise, the value of *constant-check-hook* is called on the old
;;; and new value.  The symbol value is changed (after printing a
;;; warning) only when the old and new value are different according
;;; to this predicate (i.e. if it returns nil).  The default value for
;;; *constant-check-hook* is #'equal.

(defparameter eclipse::*constant-check-hook* nil)
(defun eclipse::redefine-constant (name new &aux (old (symbol-value name)))
  (unless (funcall eclipse::*constant-check-hook* old new)
    (when (eclipse:constantp name)
      (warn "Constant ~S being redefined from ~s to ~s." name old new))
    (eclipse::set-symbol-value-value name new)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                 GLOBAL DECLARATIONS                          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Categories are global-varible, global-function,
;;; global-setf-function, or global-declaration.
;;; Keys are such things as special, inline, optimize, or nil, which
;;; means to return a plist of all the global declarations of the
;;; specified category.

(defun eclipse:Global-Declaration (name category key)
  (let ((decs (get name category)))
    (if key (getf decs key) decs)))

(defun (setf eclipse:Global-Declaration) (value variable category key)
  #+lisp-host
  (case category
    (eclipse::global-variable
     (case key
       (special
	#+cmu (cl:proclaim `(special ,variable))
	#+excl (compiler::compiler-putprop variable 't 'excl::.globally-special.))
       (eclipse::constant
	#+cmu (unless (or (constantp variable)
			  (null variable)
			  (eq variable t))
		(c::%%defconstant variable (cl:symbol-value variable) nil))
	#+excl (common-lisp:remprop variable 'excl::.constant.)
	#+excl (excl::.inv-get variable 'excl::.constant. t)))))
  (setf (getf (get variable category) key) value))



