;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 5.2 FUNCTIONS                                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 5.2.2 Lambda-Expressions

;;; Limit on number of lambda VARIABLES.  Large numbers could run
;;; afoul of C compiler local variable limits.  A reasonable number
;;; would be 255, but ANSI requires that it be no greater than
;;; call-arguments-limit. 
(defconstant LAMBDA-PARAMETERS-LIMIT 50)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 7.1 CONSTANTS AND VARIABLES                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun SYMBOL-VALUE (symbol)
  (if (boundp symbol)
      (symbol-value-value symbol)
      (error 'unbound-variable :name symbol)))

#| ;; No.  See defsetf in control-compile.lisp
(defun (SETF SYMBOL-VALUE) (value symbol)
  (etypecase symbol
    (symbol (set-symbol-value-value symbol value)))) |#

(defun SYMBOL-FUNCTION (symbol)
  (or (etypecase symbol
	(symbol (symbol-function-value symbol)))
      (error 'undefined-function :name symbol)))

(defun (SETF SYMBOL-FUNCTION) (value symbol)
  (etypecase symbol
    (symbol (set-symbol-function-value symbol value))))

;;; 7.1.2 ASSIGNMENT
(defun SET (symbol value)
  (etypecase symbol
    (symbol (if (constantp symbol)
		(error "Attempt to assign value to constant ~s." symbol)
		(set-symbol-value-value symbol value)))))

(defun MAKUNBOUND (symbol)
  (etypecase symbol
    (symbol (set-symbol-value-value symbol (symbol-value-value 'unbound-flag))))
  symbol)

;;; ANSI says this returns true for SPECIAL OPERATORS, which are
;;; defined in the ANSI glossary as one of a fixed set of symbols,
;;; enumerated in Figure 3-2.  See also, special-operator.
(defun SPECIAL-OPERATOR-P (name)
  (member name '(block let* return-from catch load-time-value setq
		       eval-when locally symbol-macrolet flet macrolet
		       function multiple-value-call the tagbody go
		       multiple-value-prog1 throw if progn
		       unwind-protect labels progv let quote)))


;;; This must be defined as a macro so that user-written program
;;; walkers can expand it to do something reasonable.
#+machine-compile
(defmacro NAMED-FUNCTION (name lambda-list &body body)
  (declare (ignore name))
  `(lambda ,lambda-list ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 7.3 FUNCTION INVOCATION                                          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This number effects how funcall, apply and multiple-value handling
;;; are written.
(defconstant CALL-ARGUMENTS-LIMIT 50)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 7.8 ITERATION                                                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 7.8.4 Mapping

(defun MAPL (function &rest lists)
  (declare (dynamic-extent lists))
  (map1 function lists nil nil))

(defun MAPLIST (function &rest lists)
  (declare (dynamic-extent lists))
  (map1 function lists :list nil))

(defun MAPCON (function &rest lists)
  (declare (dynamic-extent lists))
  (map1 function lists :nconc nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 7.10 MULTIPLE VALUES                                             ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun VALUES-LIST (list) (apply #'values list))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DECLARATIONS                                                     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Use (setf global-declaration)???!!!
(setf (get 'optimize 'global-declaration)
      '((speed 1) (safety 1) (space 1) (debug 1) (compilation-speed 1)))

(defun PROCLAIM (decl-spec)
  (multiple-value-bind (vars funcs decs)
      (canonicalize-declarations (list decl-spec) nil)
    (flet ((def-decs (decs category &optional adjustp)  
	     (loop for (symbol plist) on decs by #'cddr
		   for setfp = (and adjustp (setf-function-name-p symbol))
		   for name = (function-name-key symbol setfp)
		   for cat = (if setfp 'global-setf-function category)
		   do (loop for (prop val) on plist by #'cddr
			    do (setf (global-declaration name cat prop) val)))))
      (def-decs vars 'global-variable)
      (def-decs funcs 'global-function t)
      (def-decs decs 'global-declaration)
      'nil)))

(defmacro DECLAIM (&rest decl-specs)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     #+lisp-host (host-declaim ,@decl-specs)
     (mapc #'proclaim ',decl-specs)
     nil))


(defun globally-special-p (variable)
  (or #+excl (get variable 'EXCL::.GLOBALLY-SPECIAL.)
      #+cmu (member (ext:info variable kind variable) '(:special :constant))
      (global-declaration variable 'global-variable 'special)))

(defun CONSTANTP (form &optional env)
  (declare (ignore env))
  (typecase form
    (symbol (or #+lisp-host (cl:constantp form)
		(global-declaration form 'global-variable 'constant)
		(keywordp form)))
    ;; We are allowed to recognize other forms as well, but why bother?
    (cons (eq (car form) 'quote))
    (t t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 7.10 MULTIPLE VALUES                                             ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconstant MULTIPLE-VALUES-LIMIT 20)

