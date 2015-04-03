;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                           CONDITIONS                         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; These are not required by CLIM.  They are based on Chris Vincent's
;;; w3p system.

;;; Issue: It is not clear which of these should really be separate
;;; errors, and which should, for example, be an ANSI condition (or
;;; subtype), for example type-error, parse-error, etc.???
 
(define-condition presentation-condition (condition) ())

(define-condition presentation-type-not-found (presentation-condition) 
  ((name :initarg :name :reader presentation-type-not-found-name))
  (:report (lambda (condition stream)
	     (format stream
		     "The presentation type ~A could not be found."
		     (presentation-type-not-found-name condition)))))
(define-condition presentation-type-class-not-found (presentation-type-not-found)
  ())
  
(define-condition presentation-generic-function-not-found (presentation-condition) ())

(define-condition presentation-input-condition (presentation-condition) ())
(define-condition presentation-parse-error (presentation-input-condition) ())
(define-condition handle-input-error (presentation-parse-error) 
  ((object :initarg :object :reader handle-input-error-object)
   (type :initarg :type :reader handle-input-error-type)
   (stream :initarg :stream :reader handle-input-error-stream)
   (view :initarg :view :reader handle-input-error-view)
   (args ::initarg :args :reader handle-input-error-args))
  (:report (lambda (condition stream)
             (let ((string (write-to-string (handle-input-error-object condition)))
                   (type (handle-input-error-type condition)))
               (format stream "The input ~A is not of the required type, ~S" string type)))))
   
(define-condition input-not-of-required-type (presentation-parse-error)
  ((string :initarg :string :reader input-not-of-required-type-string)
   (type :initarg :type :reader input-not-of-required-type-type))
  (:report (lambda (condition stream)
             (let ((string (input-not-of-required-type-string condition))
                   (type (input-not-of-required-type-type condition)))
               (format stream "The input ~A is not of the required type, ~S" string type)))))

;; Change to signal-bad-type!!!
(defun input-not-of-required-type (object type)
  "this function only signals an error, does not return."
  (error 'input-not-of-required-type
         :string (write-to-string object)
         :type type))

;;;------------------------------------------------------------------- 
;;;
;;; UTILITIES
;;;

(defun pretty-symbol-name (symbol &optional strip-prefix)
  (let ((string (format t "~(~a~)" (symbol-name symbol))))
    (nsubstitute #\space #\-
		 (subseq string (or (when strip-prefix (search strip-prefix string))
				    0)))))

;;; Don't actually declare ignore, just make sure args get called.
(defmacro ignore-lambda-list (lambda-list)
  (flet ((arg-name (arg)
           (typecase arg
             (cons (car arg))
             (t arg))))
    (declare (inline arg-name))
    `(progn ,@(loop for item in lambda-list
		    unless (lambda-list-keyword-p (arg-name item))
		    collect (arg-name item)))))

;;; Determine if a symbol is an argument of a lambda list.
(defun member-lambda-list (symbol lambda-list)  
  (loop for item in lambda-list
        when (cond ((atom item) (eql symbol item))
                   (t (eql symbol (first item))))
	return t))


;;;------------------------------------------------------------------- 
;;;
;;; PRESENTATION TYPE CLASSES
;;;

;;; Internal classes built on presentation-type-class allow method
;;; dispatch on presentation-types.
(defclass PRESENTATION-TYPE-CLASS ()	;Should be subclass of standard-class!!!
  ())

(defmethod MAKE-LOAD-FORM ((presentation-type-class presentation-type-class)
			   &optional environment)
  (declare (ignore environment))
  (make-load-form-saving-slots presentation-type-class))


;;; An internal presentation-type-class structure is used for method dispatch.
(defvar *presentation-type-class-table* (make-hash-table))

;;; get an exact match for a presentation-type-class, accepts a
;;; presentation-type name. 
(defun get-presentation-type-class (name &optional (errorp t))
  (or (gethash name *presentation-type-class-table*)
      (when errorp
	(error 'presentation-type-class-not-found :name name))))

;;; Use of this is probably better handled by initform/initialize-instance!!!
;;; Generate a class prototype for method dispatch on a
;;; presentation-type-class, accepts a presentation-type name.
(defun generate-presentation-type-class-prototype (name)
  (make-instance (class-name (get-presentation-type-class name t))))

;;; Change this to ensure-xxx style!!!
;;; Returns a presentation-class name for a presentation-type, accepts
;;; presentation-type name and inheritance.
;;; Should class-name should be `(presentation-type ,name)!!!
(defmacro define-presentation-type-class (name &key superiors)  
  (flet ((get-class-name (name)
			 (class-name (get-presentation-type-class name t))))
    (let ((superior-names (or (mapcar #'get-class-name superiors) '(presentation-type-class)))
          (class-name (concatenate-names name "-presentation-class")))
      `(progn 
         (defclass ,class-name ,superior-names ()
           (:documentation ,(format nil "Class for presentation-type ~S." name)))
         (setf (gethash ',name *presentation-type-class-table*)
               (find-class ',class-name))
         ',class-name))))


;;;------------------------------------------------------------------- 
;;;
;;; PRESENTATION TYPES
;;;

;;; The class for presentation-type:
;;; OPTION-ACCEPT-ARGS is an alist matching options to
;;;    presentation-types and arguments to accept 
;;; SUPERIORS is a list of presentation-type instances to inherit from
;;; CLASS-PROTOTYPE provides an instance to use in generic function dispatch"
(defclass presentation-type ()
  ((name :initarg :name :accessor PRESENTATION-TYPE-NAME)
   (class :initarg :class :accessor presentation-type-class)
   (parameters :initarg :parameters :initform nil
	       :accessor PRESENTATION-TYPE-PARAMETERS)
   (options :initarg :options :initform nil
	    :accessor PRESENTATION-TYPE-OPTIONS)
   (option-accept-args :initarg :option-accept-args :initform nil
		       :accessor presentation-type-option-accept-args)
   (superiors :initarg :superiors :initform nil
	      :accessor presentation-type-superiors) ;presentation-type-direct-supertypes???
   (inherit-from :initarg :inherit-from :initform nil
		 :accessor presentation-type-inherit-from)
   (description :initarg :description :initform nil
		:accessor presentation-type-description)
   (class-prototype :initarg :class-prototype :initform nil
		    :accessor presentation-type-class-prototype)))

(defmethod PRINT-OBJECT ((object presentation-type) stream)
  (print-metaobject object stream (presentation-type-name object) t))

;;; This should use ensure-xxx protocol.
(defmethod MAKE-LOAD-FORM ((presentation-type presentation-type)
			   &optional environment)
  (declare (ignore environment))
  (let ((name (presentation-type-name presentation-type))
        (superior-symbols (mapcar #'presentation-type-name
                                  (presentation-type-superiors presentation-type))))
    (values
      `(make-instance ',(class-name (find-class 'presentation-type))
                      :name ',name
                      :class ',(slot-value presentation-type 'class)
                      :parameters ',(slot-value presentation-type 'parameters)
                      :options ',(slot-value presentation-type 'options)
                      :option-accept-args ',(slot-value presentation-type 'option-accept-args)
                      :superiors nil
                      :inherit-from ',(slot-value presentation-type 'inherit-from)
                      :description ,(slot-value presentation-type 'description)
                      :class-key ,(slot-value presentation-type 'class-prototype))
      `(setf (presentation-type-superiors (get-presentation-type ',name))
             (mapcar #'get-presentation-type ',superior-symbols)))))


;;; Presentation-types stored in hash table are keyed on symbols.
(defvar *presentation-type-table* (make-hash-table))

;;; Exact match.
(defmethod get-presentation-type ((name symbol) &optional (errorp t))
  (or (gethash name *presentation-type-table*)
      (when errorp
	(error 'presentation-type-not-found :name name))))

(defmethod get-presentation-type ((class class) &optional (errorp t))
  (get-presentation-type (class-name class) errorp))

(defun find-first-presentation-type (class &optional (errorp t))
  (or (loop for class in (class-precedence-list class)
	    for presentation-type = (get-presentation-type class nil)
	    thereis presentation-type)
      (when errorp
	(error 'presentation-type-not-found :name class))))

(defmethod find-presentation-type ((class class) &optional (errorp t) environment)
  (declare (ignore environment))
  (find-first-presentation-type class errorp))

(defmethod find-presentation-type ((name symbol) &optional (errorp t) environment)
  (find-first-presentation-type (find-class name t environment) errorp environment))


(defmethod get-presentation-type-parameters ((type presentation-type)
					     &optional (errorp t))
  (declare (ignore errorp))
  (presentation-type-parameters type))

(defmethod get-presentation-type-parameters ((name symbol) &optional (errorp t))
  (let ((match (get-presentation-type name errorp)))
    (when match (presentation-type-parameters match))))


(defmethod get-presentation-type-options ((type presentation-type)
					  &optional (errorp t))
  (declare (ignore errorp))
  (presentation-type-options type))

(defmethod get-presentation-type-options ((name symbol) &optional (errorp t))
  (let ((match (get-presentation-type name errorp)))
    (when match (presentation-type-options match))))


;;; Find the presentation-type precedence list according to the slot
;;; values of superiors. 
(defmethod presentation-type-precedence-list ((presentation-type presentation-type)
					      &optional (errorp t))
  
  (declare (ignore errorp))
  (labels ((build-precedence-list (type-list)
             (let ((superiors 
                     (loop with result-list = nil
                           for type in type-list
                           for type-superiors = (presentation-type-superiors type)
                           when type-superiors
                             do (loop for superior in type-superiors
                                      unless (or (member superior result-list) (null superior))
                                        do (setq result-list (nconc result-list (list superior))))
                           finally (return result-list))))
               (cond ((null superiors) nil)
                     (t (concatenate 'list superiors (build-precedence-list superiors)))))))
    (nconc (list presentation-type) (build-precedence-list (list presentation-type)))))

(defmethod presentation-type-precedence-list ((name symbol) &optional (errorp t))
  (let ((match (get-presentation-type name errorp)))
    (when match (presentation-type-precedence-list match errorp))))

;;; Find the most specific defined presentation-type for an object by
;;; class name. Used by present as a best guess, but only works for
;;; types with no required parameters.
(defun PRESENTATION-TYPE-OF (object &optional (errorp t) environment)
  (let* ((class (class-of object))
         (match (find-presentation-type class errorp environment)))
    (if match
	(presentation-type-name match)
	(when errorp 
	  (error 'presentation-type-not-found :name class)))))

;;; Get the class-prototype for a presentation-type, used for method dispatch.
(defmethod get-presentation-type-class-prototype ((presentation-type presentation-type)
						  &optional (errorp t))
  (declare (ignore errorp))
  (presentation-type-class-prototype presentation-type))

(defmethod get-presentation-type-class-prototype ((name symbol) &optional (errorp t))
  (let ((match (get-presentation-type name errorp)))
    (when match (get-presentation-type-class-prototype match errorp))))

(defun get-superiors-from-inherit-from (inherit-from parameters options)
  (let ((form (eval `(let 
  ;; kludge to get around meta-evaluating inherit-from, just grab what we need
  ;; second style known to work for Allegro, should really make this
  ;; portable sometime !!!
  (vars inherit-from)
  (flet ((extract-name (backquote)
		       (vars backquote)
           (cond ((atom backquote) backquote)
                 ((atom (caadr backquote)) (caadr backquote))
                 (t (caaaar (second backquote))))))
    (declare (inline extract-name))
    (when (car-eq inherit-from 'quote)
      (setq inherit-from (second inherit-from)))
    (cond ((atom inherit-from) (list inherit-from))
          ((eql 'and (car inherit-from))
	   (mapcar #'extract-name (cdr inherit-from)))
          (t (list (extract-name inherit-from))))))

;;; Return a lambda-list from the parameters arg to
;;; define-presentation-type, ensures &rest arg. 
(defun decode-parameters-list (parameters-list)
  (cond ((member '&rest parameters-list) parameters-list)
        (t (let ((insert-pos (position-if #'(lambda (item)
					      (member item '(&key &aux)))
					  parameters-list)))
             (concatenate 'list 
                          (subseq parameters-list 0 insert-pos)
                          '(&rest rest-args)
                          (when insert-pos (subseq parameters-list insert-pos)))))))

;;; Return a lambda-list and an accept-args alist from the options arg
;;; to define-presentation-type. 
(defun decode-options-list (options-list)
  (loop for item in options-list
        when (atom item) collect item into lambda-list
        else when (<= (length item) 3) collect item into lambda-list
        else collect (subseq item 0 2) into lambda-list
             and collect (cons (make-keyword (car item))
			       (subseq item 3))
	     into options-list
        finally (return (values (concatenate 'list 
                                             '(&key) 
                                             lambda-list 
                                             (unless (member-lambda-list 'description lambda-list)
                                               '(description))
                                             '(&allow-other-keys))
                                options-list))))

(defmacro DEFINE-PRESENTATION-TYPE (name parameters
					 &key options inherit-from
					 (description
					  (pretty-symbol-name name))
					 ;; history, parameters-are-types!!!
					 &environment environment)
  (let ((class (find-class name nil environment))
        (parameters-lambda-list (decode-parameters-list parameters))
        (superiors (cond (inherit-from
			  (get-superiors-from-inherit-from
			   inherit-from))
			 ;; IWBNI we hand-bootstrapped type t and
			 ;; didn't check for it here!!!
                         ((eql name t) nil)
			 ;; CLIM specifies something more complicated!!!
                         (t '(t)))))
    (multiple-value-bind (options-lambda-list option-accept-args)
        (decode-options-list options)
      `(eval-when (:compile-toplevel :load-toplevel :execute)
	 (define-presentation-type-class ,name :superiors ,superiors)
	 (setf (gethash ',name *presentation-type-table*)
	       (make-instance 'presentation-type
			      :name ',name
			      :class (find-class ',name nil)
			      :parameters ',parameters-lambda-list
			      :options ',options-lambda-list
			      :option-accept-args ',option-accept-args
			      :superiors (mapcar #'get-presentation-type ',superiors)
			      :inherit-from ',inherit-from
			      :description ,description
			      :class-prototype
			      (generate-presentation-type-class-prototype ',name)))))))

;;; Define-presentation-type should use ensure-xxx find-xxx protocol,
;;; so (setf find-xxx) should be useable to remove it!!!
(defmacro remove-presentation-type (name)
  (check-type name symbol)
  (get-presentation-type name)
  `(remhash ',name *presentation-type-table*))


;;;------------------------------------------------------------------- 
;;;
;;; PRESENTATION TYPE SPECIFIERS
;;;

;;; Change to PRESENTATION-TYPE-SPECIFIER-P???
(defgeneric acceptable-presentation-type-specifier (specifier))
(defmethod acceptable-presentation-type-specifier ((specifier symbol))
  (when specifier t))
(defmethod acceptable-presentation-type-specifier ((specifier cons)) 
  (or (symbolp (car specifier))
      (symbolp (caar specifier))))
(defmethod acceptable-presentation-type-specifier (specifier) 
  (declare (ignore specifier))
  nil)

(deftype presentation-type-specifier ()
  `(satisfies acceptable-presentation-type-specifier))

(defmacro WITH-PRESENTATION-TYPE-DECODED ((name-var &optional parameters-var options-var)
                                          type &body body)
  `(progn 
     (check-type ,type presentation-type-specifier)
     (multiple-value-bind (,.(when name-var `(,name-var))
                           ,.(when parameters-var `(,parameters-var))
                           ,.(when options-var `(,options-var)))
         (cond ((atom ,type)
                (values ,.(when name-var `(,type))
                        ,@(when parameters-var `(nil))
                        ,@(when options-var `(nil))))
               ((atom (car ,type))
                (values ,.(when name-var `((car ,type)))
                        ,.(when parameters-var `((cdr ,type)))
                        ,@(when options-var `(nil))))
               (t (values ,.(when name-var `((caar ,type)))
                          ,.(when parameters-var `((cdar ,type)))
                          ,.(when options-var `((cdr ,type))))))
       ,@body)))

(defmacro WITH-PRESENTATION-TYPE-PARAMETERS ((name type) &body body)
  (let ((parameter-lambda-list (get-presentation-type-parameters name t)))
    `(with-presentation-type-decoded (nil parameter-args) ,type
       (destructuring-bind ,parameter-lambda-list parameter-args
         (ignore-lambda-list ,parameter-lambda-list)
         ,@body))))

(defmacro with-presentation-type-options ((name type) &body body)
  (let ((option-lambda-list (get-presentation-type-options name t)))
    `(with-presentation-type-decoded (nil nil option-args) ,type
       (destructuring-bind ,option-lambda-list option-args
         (ignore-lambda-list ,option-lambda-list)
         ,@body))))

         
;;;------------------------------------------------------------------- 
;;;
;;; PRESENTATION VIEWS
;;;

(defmacro define-presentation-view-class (name &key inherits-from
					       (description
						(format nil "The view class ~S." name)))
  `(defclass ,name ,inherits-from () (:documentation ,description)))


(defmacro define-presentation-view (name class &optional (errorp t) &environment environment)
  (let ((found-class (find-class class errorp environment)))
    (when found-class
      `(defparameter ,name (make-instance ',class)))))


 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 23.3 PRESENTATION METHODS

;;; Hash table keyed on function name that user sees, stores actual
;;; generic function name with lambda list.
(defvar *presentation-generic-function-table* (make-hash-table))

;;; Retrieve the generic function name and lambda-list for presentation-function-name.
(defun get-presentation-generic-function-info (presentation-function-name &optional (errorp t))
  (check-type presentation-function-name symbol)
  (let ((info (gethash presentation-function-name *presentation-generic-function-table*)))
    (unless (or info (not errorp))
      (error 'presentation-generic-function-not-found))
    info))

;;; Call a presentation-generic-function on a list of arguments.
(defmacro call-presentation-generic-function (presentation-function-name args)
  (check-type presentation-function-name symbol)
  (check-type args list)
  (let ((generic-function-name (first
				(get-presentation-generic-function-info
				 presentation-function-name t)))
        (type-name (car args)))
    `(funcall ',generic-function-name
	      (get-presentation-type-class-prototype ,type-name)
	      ,@(cdr args))))

(defmacro FUNCALL-PRESENTATION-GENERIC-FUNCTION (presentation-function-name &rest args)
  `(call-presentation-generic-function ,presentation-function-name
				       ,args))

;;; apply-presentation-generic-function!!!

(defmacro DEFINE-PRESENTATION-GENERIC-FUNCTION (generic-function-name 
                                                presentation-function-name lambda-list
						&rest options)
  (let ((generic-info (cons generic-function-name lambda-list)))
    `(progn (defgeneric ,generic-function-name ,lambda-list ,@options)
            (setf (gethash ',presentation-function-name
			   *presentation-generic-function-table*)
		  ',generic-info))))
    
(defmacro remove-presentation-generic-function (presentation-function-name)
  (check-type presentation-function-name symbol)
  (let ((generic-function-name (first
				(get-presentation-generic-function-info
				 presentation-function-name t))))
    `(progn (fmakunbound ',generic-function-name)
            (remhash ',presentation-function-name
		     *presentation-generic-function-table*))))


(defmacro DEFINE-PRESENTATION-METHOD (presentation-function-name &rest body)
  (let* ((generic-function-info
	  (get-presentation-generic-function-info
	   presentation-function-name t))
         (generic-function-name (first generic-function-info))
         (generic-lambda-list (cdr generic-function-info))
         (qualifiers (loop for item in body
                           while (atom item)
                           collect item into result
                           do (pop body)
                           finally (return result)))
         (specialized-lambda-list (pop body))
         (documentation (when (stringp (first body)) (list (pop body))))
         (declarations (loop for item in body
                             while (consp item) 
                             while (eq 'declare (first item))
                             collect item into result
                             do (pop body)
                             finally (return result)))
         (options-var (when (member 'options generic-lambda-list) '(options)))
         (parameters-var (when (member 'parameters generic-lambda-list) '(parameters)))
         (method-lambda-list `(,@parameters-var ,@options-var ,@specialized-lambda-list))
         (type-pos (1- (position 'type generic-lambda-list)))
         (method-type (nth type-pos method-lambda-list))
         (type-var (typecase method-type 
                     (cons (first method-type))
                     (t method-type)))
         (type-name (when (consp method-type) (second method-type)))
         (type-key (typecase method-type
                     (cons `(,(first generic-lambda-list)
			     ,(class-name (get-presentation-type-class type-name t))))
                     (t (first generic-lambda-list)))))
    (setf (nth type-pos method-lambda-list) type-var)
    (push type-key method-lambda-list)
    (unless (consp type-key)
      (push type-key body))
    (when parameters-var
      (cond (type-name (let ((parameters-lambda-list (get-presentation-type-parameters type-name)))
                         (setq body `((destructuring-bind ,parameters-lambda-list
                                          ,@parameters-var
                                        ,`(ignore-lambda-list ,parameters-lambda-list)
                                        ,@body)))))
            (t (push (car parameters-var) body))))
    (when options-var
      (cond (type-name (let ((options-lambda-list (get-presentation-type-options type-name)))
                         (setq body `((destructuring-bind ,options-lambda-list
                                          ,@options-var
                                        ,`(ignore-lambda-list ,options-lambda-list)
                                        ,@body)))))
            (t (push (car options-var) body))))
    `(defmethod ,generic-function-name ,@qualifiers ,method-lambda-list
                ,@documentation
                ,@declarations
                ,type-var
                ,@body)))

(defmacro DEFINE-DEFAULT-PRESENTATION-METHOD (presentation-function-name &body body)  
  `(define-presentation-method ,presentation-function-name ,@body))


(defmethod presentation-type-superior-p ((presentation-type
					  presentation-type)
					 (putative-superior presentation-type))
  (member putative-superior (presentation-type-precedence-list presentation-type)))

(defmethod presentation-type-superior-p ((name symbol) (putative-superior-name symbol))
  (presentation-type-superior-p (get-presentation-type name t)
                                (get-presentation-type putative-superior-name t)))


;;; MAP-OVER-PRESENTATION-TYPE-SUPERTYPES!!!
;;; FIND-PRESENTATION-TYPE-CLASS!!!
;;; CLASS-PRESENTATION-TYPE-NAME!!!
;;; DEFAULT-DESCRIBE-PRESENTATION-TYPE!!!
;;; MAKE-PRESENTATION-TYPE-SPECIFIER!!!