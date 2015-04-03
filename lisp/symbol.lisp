;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 10.1 THE PROPERTY LIST                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SYMBOL-PLIST is embedded

(defun GET (symbol indicator &optional default)
  (getf (symbol-plist symbol) indicator default))

(defun (SETF GET) (new-value symbol indicator &optional default)
  (declare (ignore default))
  (setf (getf (symbol-plist symbol) indicator) new-value))

(defun REMPROP (symbol indicator)
  (remf (symbol-plist symbol) indicator))


(defun GETF (place indicator &optional default)
  (do ((list place (cdr list)))
      ((endp list) default)
    (let ((item (pop list)))
      (unless list (error "~s is a malformed property list." place))
      (when (eq item indicator) (return (car list))))))

(defun putf (place indicator new-value)
  (do ((list place (cddr list)))
      ((endp list) (list* indicator new-value place))
    (locally (declare (type cons list))
      (when (eq (car list) indicator)
	(setf (cadr list) new-value)
	(return place)))))

(defun GET-PROPERTIES (place indicator-list)
  (do ((list place (cddr list)))
      ((endp list) (values nil nil nil))
    (let ((key (car list)))
      (when (member key indicator-list :test #'eq)
	(return (values key (cadr list) list))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 10.2 THE PRINT-NAME                                          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SYMBOL-NAME is embedded

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 10.3 CREATING SYMBOLS                                        ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MAKE-SYMBOL is embedded.  Note that it must signal type-error if
;;; argument is not a string!

(defun COPY-SYMBOL (sym &optional copy-props)
  (let ((new (make-symbol (symbol-name sym))))
    (when copy-props
      (setf (symbol-value new) (symbol-value sym))
      (setf (symbol-function new) (symbol-function sym))
      (setf (symbol-plist new) (copy-list (symbol-plist sym))))
    new))

(defun genstring (string n)
  (let ((*print-base* 10)
	(*print-radix* nil)
	(*print-pretty* nil))
    (declare (special *print-base* *print-radix* *print-pretty*))
    (concatenate 'string string (cl:princ-to-string n))))

(defparameter *GENSYM-COUNTER* 0)
(defun GENSYM (&optional x)
  (let ((prefix (if (stringp x) x "G"))
	(suffix (cond ((integerp x) x)
		      (t (let ((n *gensym-counter*))
			   (setq *gensym-counter* (1+ n))
			   n)))))
    (make-symbol (genstring prefix suffix))))

(defparameter *gentemp-counter* 0)
(defun GENTEMP (&optional (prefix "T") (package *package*))
  (loop
    (multiple-value-bind (sym existedp)
	(intern (genstring prefix (incf *gentemp-counter*)) package)
      (unless existedp (return sym)))))


(defun SYMBOL-PACKAGE (symbol)
  (if (symbolp symbol)
      (symbol-package-value symbol)
      (error 'type-error :expected-type 'symbol :datum symbol)))

(defun KEYWORDP (sym)
  (and (symbolp sym)
       (eq (symbol-package-value sym)
	   *keyword-package*)))

;;; This was in common-comp.lisp.  
(defun eclipse::Make-Symbols (list-of-symbols &optional root)
  (mapcar #'(lambda (arg)
	      (cl:gensym (or root (host::string arg))))
	  list-of-symbols))