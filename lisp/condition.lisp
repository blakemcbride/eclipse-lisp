(defun MAKE-CONDITION (type &rest slot-initializations)
  (apply #'make-instance type slot-initializations))

(defun simple-condition-printer (condition stream)
  (apply #'format stream (simple-condition-format-control condition)
	 		 (simple-condition-format-arguments condition)))

(define-condition CONDITION (standard-object) ())
(define-condition SIMPLE-CONDITION ()
  ((format-control :reader simple-condition-format-control
		   :initarg :format-control)
   (format-arguments :reader simple-condition-format-arguments
		     :initarg :format-arguments
		     :initform '()))
  (:report simple-condition-printer))
(define-condition WARNING (condition) ())
(define-condition STYLE-WARNING (warning) ()) ;for unused variables
(define-condition SIMPLE-WARNING (simple-condition warning) ())
(define-condition BREAK (simple-condition) ())
(define-condition SERIOUS-CONDITION (condition) ())
(define-condition STORAGE-CONDITION (serious-condition) ())
(define-condition ERROR (serious-condition) ())
(define-condition SIMPLE-ERROR (simple-condition error) ())
(define-condition CELL-ERROR (error)
  ((name :reader cell-error-name :initarg :name)))
(define-condition UNBOUND-VARIABLE (cell-error) ()
  (:report
   (lambda (condition stream)
     (format stream "The variable ~S is unbound."
	     (cell-error-name condition)))))
(define-condition UNBOUND-SLOT (cell-error)
  ((instance :reader unbound-slot-instance
	     :initarg :instance))
  (:report
   (lambda (condition stream)
     (format stream "~s is unbound in ~s."
	     (cell-error-name condition)
	     (unbound-slot-instance condition)))))
(define-condition UNDEFINED-FUNCTION (cell-error) ()
  (:report
   (lambda (condition stream)
     (format stream "The function ~S is undefined."
	     (cell-error-name condition)))))
(define-condition PARSE-ERROR (simple-error) ())
(define-condition PRINT-NOT-READABLE (error)
  ((object :reader print-not-readable-object :initarg :object))
  (:report
   (lambda (condition stream)
    (format stream "~S cannot be printed readably."
	    (print-not-readable-object condition)))))

(define-condition TYPE-ERROR (error)
  ((datum :reader type-error-datum :initarg :datum)
   (expected-type :reader type-error-expected-type :initarg :expected-type))
  (:report
   (lambda (condition stream)
     (format stream "~s is not of type ~s."
	     (type-error-datum condition)
	     (type-error-expected-type condition)))))
(define-condition OUT-OF-BOUNDS-ERROR (type-error)
  ((length :reader out-of-bounds-error-length :initarg :length))
  (:report
   (lambda (condition stream)
     (format stream "~s is out of bounds for sequence of length ~s."
	     (type-error-datum condition)
	     (out-of-bounds-error-length condition)))))
(defmethod INITIALIZE-INSTANCE :AFTER ((condition OUT-OF-BOUNDS-ERROR) &key length)
  (setf (slot-value condition 'expected-type) `(integer 0 ,length)))
(define-condition SIMPLE-TYPE-ERROR (simple-condition type-error) ())

(define-condition ARITHMETIC-ERROR (error)
  ((operation :reader arithmetic-error-operation :initarg :operation)
   (operands :reader arithmetic-error-operands :initarg :operands))
  (:report (lambda (condition stream)
	     (format stream "~s."
		     (class-name (class-of condition))))))
(define-condition DIVISION-BY-ZERO         (arithmetic-error)
  ((operation :initform '/)
   (operands :initform '(0))))
(define-condition FLOATING-POINT-OVERFLOW  (arithmetic-error) ())
(define-condition FLOATING-POINT-UNDERFLOW (arithmetic-error) ())
(define-condition FLOATING-POINT-INEXACT   (arithmetic-error) ())
(define-condition FLOATING-POINT-INVALID-OPERATION   (arithmetic-error) ())


;;; Simple-error is our doing. Spec specifies just error.
(define-condition PROGRAM-ERROR (simple-error) ())
(define-condition CONTROL-ERROR (simple-error) ())

(defclass auto-arguments-error (simple-error)
  ((format-arguments :initform 'not-initialized)))
(defmethod initialize-instance :after ((condition auto-arguments-error)
				       &rest initargs)
  (declare (ignore initargs))
  (let ((class (class-of condition)))
    (when (eq (slot-value condition 'format-arguments) 'not-initialized)
      (setf (slot-value condition 'format-arguments)
	    (loop for slot in (class-slots class)
		  for name = (slot-definition-name slot)
		  unless (or (eq name 'format-control)
			     (eq name 'format-arguments))
		  collect (slot-value-using-class class condition slot))))))

(define-condition STREAM-ERROR (auto-arguments-error)
  ((stream :reader stream-error-stream :initarg :stream)))
(define-condition END-OF-FILE (stream-error)
  ((format-control :initform "End of file encountered on ~s.")))
(define-condition CLOSED-STREAM (stream-error)
  ((format-control :initform "~s is closed.")))

(define-condition READER-ERROR (parse-error stream-error) ())

(define-condition FILE-ERROR (auto-arguments-error)
  ((pathname :reader file-error-pathname :initarg :pathname)))
(define-condition file-not-found-error (file-error)
  ((format-control :initform "~a was not found.")))

(define-condition PACKAGE-ERROR (auto-arguments-error)
  ((package :initarg :package :reader package-error-package)))
(define-condition INACCESSIBLE-SYMBOL-ERROR (package-error)
  ((symbol :initarg :symbol :reader inaccessible-symbol-error-symbol)
   (format-control :initform "~s does not have ~s accessible.")))

(define-condition system-error (auto-arguments-error)
  ((function :reader system-error-function :initarg :function)
   (number :reader system-error-number :initarg :errno)
   (description :reader system-error-description :initarg
		:description)
   (format-control :initform "~a => ~a~@[: ~a~]")))

(defparameter *HANDLERS* nil)
(defparameter *RESTARTS* nil)

;;; *HANDLERS* is a list of HANDLER-FRAMES.  Each binding in
;;; handler-frame is a HANDLER.  
(defstruct (HANDLER
	    (:copier nil)
	    (:type list)
	    (:predicate nil)
	    (:constructor MAKE-HANDLER (condition-type function)))
  (condition-type nil :read-only t)
  (function nil :read-only t))

;;; *RESTARTS* is a list of RESTART-FRAMES.  Each binding in
;;; restart-frame is a RESTART.  
(defstruct (RESTART
	    (:copier nil)
	    (:constructor MAKE-RESTART
			  (name function
				&key interactive-function
				report-function associated-conditions
				test-function))
	    (:print-object
	     (lambda (restart stream)
	       (if *print-escape*
		   (call-next-method)	
		   (let ((f (restart-report-function restart)))
		     (if f (funcall f stream)
			 (write (restart-name restart) :stream stream)))))))
  (name nil :type symbol :read-only t)
  (function nil :read-only t)
  (interactive-function nil :read-only t)
  (report-function nil :read-only t)
  (associated-conditions nil)
  (test-function nil :read-only t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 29.4 PROGRAM INTERFACE TO THE CONDITION SYSTEM               ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 29.4.1 SIGNALING CONDITIONS
(defun canonicalize-condition (datum arguments default-type)
  (etypecase datum
    (symbol (apply #'make-condition datum arguments))
    (condition 
     (if arguments
	 ;; Required by WARN, merely permitted by others.
	 (error 'type-error :datum arguments :expected-type 'null)
       datum))
    (format-control (make-condition default-type
				    :format-control datum
				    :format-arguments arguments))))

(defun ERROR (datum &rest arguments)
  (let ((c (canonicalize-condition datum arguments 'simple-error))) 
    (signal c)
    (invoke-debugger c)))

(defun CERROR (continue-string datum &rest arguments)
  (restart-case (error (canonicalize-condition
			datum (unless (typep datum 'condition) arguments)
			'simple-error))
    (continue ()
	      :report (lambda (s)
			(apply #'format s continue-string
			       arguments))
	      nil)))

(defparameter *BREAK-ON-SIGNALS* nil)
(defun SIGNAL (datum &rest arguments)
  (let ((c (canonicalize-condition datum arguments 'simple-condition)))
    (let ((bos *break-on-signals*)
	  (*break-on-signals* nil))
      (when (typep c bos) (break "Signal break: ~a" c)))
    (do ((frames *handlers* (cdr frames)))
	((null frames) nil)
      (let ((*handlers* (cdr frames)))
	(dolist (handler (car frames))
	  (when (typep c (handler-condition-type handler))
	    (funcall (handler-function handler) c)
	    (return)))))))

;;; 29.4.9 WARNINGS
(defun WARN (datum &rest arguments)
  (let ((c (canonicalize-condition datum arguments 'simple-warning)))
    (unless (typep c 'warning)
      (error 'type-error :datum datum :expected-type 'warning))
    (restart-case (signal c)
      (muffle-warning ()
	:report "Skip warning."
	(return-from warn nil)))
    (report-condition c)
    nil))


;;; 29.4.2 ASSERTIONS

(defun read-evaluated-form ()
  (declare (notinline eval))
  (format *debug-io* "~&New value to be evaluated: ")
  (funcall (symbol-function 'eval) (read *debug-io*)))
(defun interactive-evaluated-form ()
  (list (read-evaluated-form)))


(defun check-type-error (place value type type-string)
  (let ((cond (make-condition
	       'simple-type-error
	       :datum value :expected-type type
	       :format-control "The value of ~s, ~s, is not ~a."
	       :format-arguments
	       (list place value
		     (or type-string
			 (format nil "of type ~s" type))))))
    (restart-case (error cond)
      (store-value (value)
	:report (lambda (stream)
		  (format stream "Supply a new value for ~S."
			  place))
	:interactive interactive-evaluated-form
	value))))


(defun assert-error (assertion places
			       &optional (datum "~s is not true." datump)
			       &rest arguments)
  (let ((cond (canonicalize-condition
	       datum (if datump arguments (list assertion))
	       'simple-error))) 
    (restart-case (error cond)
      (continue ()
	:report (lambda (stream)
		  (if places
		      (format stream "Retry with new value~P for ~{~S~^, ~}."
			      (length places) places)
		    (format stream "Retry.")))
	nil))))



(defun assert-prompt (name value)
  (if (y-or-n-p "The old value of ~S is ~S.~@
		 Do you want to supply a new value? "
		name value)
      (read-evaluated-form)
    value))

(defun associate-conditions (condition restarts &aux data)
  (dolist (restart restarts data)
    (push restart data)
    (push condition (restart-associated-conditions restart))))

(defun disassociate-conditions (data)
  (dolist (restart data)
    (pop (restart-associated-conditions restart))))
  

;;; 29.4.8 FINDING AND MANIPULATING RESTARTS
(defun applicable-restart (restart name condition)
  (and (or (null name)
	   (if (restart-p name)
	       (eq name restart)
	     (eql name (restart-name restart))))
       (or (null condition)
	   (let ((conds (restart-associated-conditions restart)))
	     (or (null conds)
		 (member condition conds))))
       (let ((test (restart-test-function restart)))
	 (or (null test)
	     (funcall test condition)))
       restart))
    
  
(defmacro do-restarts ((var name condition &optional resultform
			    (framesvar (gensym "RESTART-FRAMES"))
			    (restartvar (gensym "RESTART")))				      
		       &rest tagbody)
  `(do ((,framesvar *restarts* (cdr ,framesvar)))
       ((null ,framesvar) ,resultform)
     (dolist (,restartvar (car ,framesvar))
       (let ((,var (applicable-restart ,restartvar ,name ,condition)))
	 ,@tagbody))))

(defun COMPUTE-RESTARTS (&optional condition &aux results)
  (do-restarts (restart nil condition (nreverse results))
    (when restart (push restart results))))

(defun FIND-RESTART (restart-identifier &optional condition)
  (do-restarts (restart restart-identifier condition nil)
    (when restart (return-from find-restart restart))))

(defun invoke-applicable-restart (condition restart-identifier
					    arguments interactivep errorp) 
  (do-restarts (restart restart-identifier condition
			(when errorp
			  (error 'control-error
				 :format-control "Restart ~s is not active."
				 :format-arguments (list restart-identifier)))
			frames)
     (when restart
       (return-from invoke-applicable-restart
	 (let ((*restarts* (cdr frames)))
	   (apply (restart-function restart)
		  (if interactivep
		      (let ((fn (restart-interactive-function restart)))
			(when fn (funcall fn)))
		    arguments)))))))

(defun INVOKE-RESTART (restart-identifier &rest arguments)
  (invoke-applicable-restart nil restart-identifier arguments nil t))

(defun INVOKE-RESTART-INTERACTIVELY (restart-identifier)
  (invoke-applicable-restart nil restart-identifier nil t t))

;;; 29.4.10 RESTART FUNCTIONS
(defun ABORT (&optional condition)
  (invoke-applicable-restart condition 'abort nil nil t))
(defun MUFFLE-WARNING (&optional condition)
  (invoke-applicable-restart condition 'muffle-warning nil nil t))
(defun CONTINUE (&optional condition)
  (invoke-applicable-restart condition 'continue nil nil nil))
(defun STORE-VALUE (value &optional condition)
  (invoke-applicable-restart condition 'store-value
			     (list value) nil nil)) 
(defun USE-VALUE (value &optional condition)
  (invoke-applicable-restart condition 'use-value
			     (list value) nil nil)) 


;;; 29.4.11 DEBUGGING UTILITIES
(defparameter *DEBUGGER-HOOK* nil)
(defun BREAK (&optional (format-string "Break")
			&rest format-arguments)
  (with-simple-restart (continue "Return from BREAK.")
    (let ((*debugger-hook* nil))
      (invoke-debugger
       (make-condition 'break
		       :format-control format-string
		       :format-arguments format-arguments))))
  nil)

(defun Exit (&optional status condition)
  (invoke-applicable-restart condition 'exit `(,status) nil nil)
  (ec:exit (fixnum-int 1))
  nil)  

(defgeneric INVOKE-DEBUGGER (condition))
(defmethod INVOKE-DEBUGGER :AROUND (condition)
  (let ((hook *debugger-hook*)
	(*debugger-hook* nil))
    (when hook (funcall hook condition hook))
    (call-next-method)))

;; This is the default method used in runtime-only applications.
;; The "real" method is defined in debug.lisp
(defmethod INVOKE-DEBUGGER (condition)
  (report-condition condition)
  (exit t condition))

;;; Realy belongs in symbol.lisp or string.lisp!
;;; Used for condition display in debugger and by CLIM COMMAND-NAME-FROM-SYMBOL. 
(defun pretty-symbol-name (symbol &optional prefix)
  (let* ((name (symbol-name symbol))
	 (index (when prefix
		  (search prefix name))))
    (nstring-capitalize (nsubstitute #\space #\-
				     (subseq name
					     (if (when index (zerop index))
						 (length prefix)
						 0))))))

;;; Invokes the "condition reporter."
(defun report-condition (condition &optional (stream *error-output*))
  (terpri stream) (terpri stream)
  (let ((class-name (class-name-of condition)))
    (unless (eq class-name 'simple-condition)
      (write-string (pretty-symbol-name class-name "SIMPLE-")
		    stream)
      ;; (report-context stream nil)
      (write-char #\: stream)
      (terpri stream)))
  (pprint-logical-block (stream nil :per-line-prefix "  ")
    ;; This should be write (not wrap), but not all our error messages
    ;; include conditional newlines.
    (wrap condition :stream stream
	  :readably nil			;t would loop on print errors
	  :escape nil))			;to invoke condition reporter
  (terpri stream))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                OPERATING SYSTEM SIGNALS                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-condition asynchronous-interrupt (condition)
  ((signal :reader asynchronous-interrupt-signal
	   :initarg :signal)
   (code :reader asynchronous-interrupt-code
	 :initarg :code
	 :initform nil)
   (name :reader asynchronous-interrupt-name
	 :initarg :name
	 :initform nil))
  (:report (lambda (condition stream)
	     (format stream "~@[~s ~]Signal ~d~@[ (code ~s)~]"
		     (asynchronous-interrupt-name condition)
		     (asynchronous-interrupt-signal condition)
		     (asynchronous-interrupt-code condition)))))

(define-condition serious-asynchronous-interrupt
  (asynchronous-interrupt serious-condition)
  ())


(defparameter *interrupts* (make-array 32))
(defun set-interrupt (signal-number function)
  (setf (elt *interrupts* signal-number) function)
  (catch-interrupts (fixnum-int signal-number))
  function)

(defun interrupt-handler (signal-number code)
  (cerror "Return from interrupt."
	  (make-condition 'asynchronous-interrupt
			  :signal signal-number
			  :code code
			  :name :interrupt)))

(defun fpe-handler (signal-number code)
  (declare (ignore signal-number))
  (error (fpe-class (fixnum-int code))))

;;; IWBNI we handled:
;;; - SIGQUIT, SIGTERM by calling EXIT (maybe one should call ABORT?)
;;; - SIGHUP by signalling end-of-file on *standard-input*
;;;   (assuming that we handled end-of-file on *standard-input* properly)

(defun illegal-instruction-handler (signal-number code)
  (error 'serious-asynchronous-interrupt
	 :signal signal-number
	 :code code
	 :name :illegal-instruction))

(defun bus-error-handler (signal-number code)
  (error 'serious-asynchronous-interrupt
	 :signal signal-number
	 :code code
	 :name :bus-error))

(defun segv-handler (signal-number code)
  (error 'serious-asynchronous-interrupt
	 :signal signal-number
	 :code code
	 :name :segmentation-violation))

(defun sys-error-handler (signal-number code)
  (error 'serious-asynchronous-interrupt
	 :signal signal-number
	 :code code
	 :name :system-error))
