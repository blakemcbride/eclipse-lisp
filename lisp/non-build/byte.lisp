(defmacro bcf (form)			;temporary for development
  `(xxwith-compilation-unit (:processor (make-byte-compiler) ;!!!
					:environment nil)
     (process ',form *processor* *environment* nil)))

(defmacro cef (form)			;temporary for development
  `(values-list (process (bcf ,form) *byte-engine* nil nil)))

(defun walk-test (group &rest methods)
  (let (files (byte-engine (symbol-value '*byte-engine*)))
    (case group
      (:first (push "test/control.lisp" files))
      (:second (push "test/control2.lisp" files))
      (:both (push "test/control2.lisp" files)
	     (push "test/control.lisp" files)))
    (when (or (null methods) (find :interp methods))
      (rem-all-tests)
      (flet ((interp (file) 
		     (cl:with-open-file (f file)
					(cl:loop for form = (cl:read f nil 'eof)
						 until (eq form 'eof)
						 do (cl:print form)
						 do (process form
							     *interpreter* nil
							     nil)))))
	(mapc #'interp files)
	(do-tests)))
    (when (or (null methods) (find :byte methods))
      (rem-all-tests)
      (flet ((comp (file)
		   (cl:with-open-file (f file)
                        (xxwith-compilation-unit (:processor (make-byte-compiler) ;!!!
							     :environment nil)
			   (cl:loop for form = (cl:read f nil 'eof)
				    until (eq form 'eof)
				    do (cl:print form)
				    do (process (process form *processor* *environment* nil)
						byte-engine nil nil))))))
	(mapc #'comp files)
	(do-tests)))))

(defclass BYTE-COMPILED-FUNCTION (compiled-function packaged-function)
  ((code :initarg :code :accessor function-code))
  (:metaclass funcallable-standard-system-class))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Extending the interpreter to handle byte-compiled code.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Normally, the interpeter handles Lisp code.  Currently, our
;;; byte-compiler "cheats" and creates list structure that looks more
;;; or less like Lisp code that could be interpreted, except that it
;;; has already been preprocessed, and make use of some special forms
;;; and literal data should never appear in conforming Lisp source
;;; code. 
;;;
;;; The definitions here extend the interpreter to handle these small
;;; difference between our pseudo-byte-compiled code and real Lisp
;;; source.  (The first one might go away if xxx-binding gets rid of
;;; kind result and process-binding becomes a specialization of
;;; process for bindings "forms".)

(defstruct (BYTE-ENGINE (:include lisp-interpreter)))

(defmethod PROCESS ((form BINDING) (processor BYTE-ENGINE)
		    environment continuation)
   (declare (ignore environment))
   (resume continuation processor (list (runtime-value form))))

(defmethod PROCESS ((form LITERAL) (processor BYTE-ENGINE)
		    environment continuation)
   (declare (ignore environment))
   (resume continuation processor (list (literal-value form))))

(defspecial FRAME (bindings &body body)
  :processor BYTE-ENGINE
  (process-sequence body processor
		    (make-variable-env :bindings bindings :env environment)
		    continuation))
  
(defspecial ASSIGN (binding value)
  :processor BYTE-ENGINE
  (process value processor environment
	   (make-lexical-assignment-cont continuation binding)))

;;; Because interpreter expects evaluation of a throw form to produce
;;; a list of values....
(defmethod PROCESS ((form GO-TAG) (processor BYTE-ENGINE)
		    environment continuation)
   (declare (ignore environment))
   (resume continuation processor (list form)))

(defspecial JUMP-SET (tag &rest forms)
  :processor BYTE-ENGINE
  (resume (make-tagbody-cont continuation nil tag)
	  processor
	  (catch tag
	    (process-sequence forms processor
			      (jump-set-cont-environment tag)
			      null-continuation))))

;; Introduced by compiler-macro for CLASS-NAME-OF
(defspecial INSTANCE-TAG (&rest object)
  :processor BYTE-ENGINE
  (resume (make-collection-cont (make-call-cont continuation)
				object environment)
	  processor (list #'class-name-of)))


;;; !!! Must make sure that function-lambda-expression works on byte
;;; compiled functions:
;;;   shuffle superlclass order?
;;;   specialized method?
;;; Maybe we should have an UNCOMPILE-OBJECT method that returns the
;;; cached LAMBDA, if known, otherwise it walks the CODE in a way
;;; similar to generating C/JVM, but produces a lambda-expression
;;; list instead of just writing to a file???

(defparameter *byte-engine* (make-byte-engine))
;;; Rationalize these with interpreter!!!,
;;; apply-interpreted-function and process-lambda should be renamed!!!
(defmethod INVOKE ((function BYTE-COMPILED-FUNCTION) values
		   (processor LISP-INTERPRETER) environment continuation)
  (process-lambda function values processor environment continuation))


;;; Is there a reason to have this separate from INVOKE???
(defmethod PROCESS-LAMBDA ((closure BYTE-COMPILED-FUNCTION) args
			   (processor LISP-INTERPRETER)
			   environment continuation)
  (declare (ignore environment))
  (let ((env (function-env closure)))
    (setf (binding-value (get-binding *args-name* env)) args)
    (resume continuation processor
	    (byte-engine (function-code closure)
			 env))))

(defun byte-engine (code env)
  (process code *byte-engine* env nil))



;;;; Belongs in evaluation.lisp!!!

(defmacro xxwith-file-processing-restarts ((operation filespec keys
						      &optional abort-value-form)
					   &body body)
  `(restart-case (progn ,@body)
     (retry ()
	    :report (lambda (s) (format s #"Retry ~a ~s." ',operation ,filespec))
	    (apply (function ,operation) ,filespec ,keys))
     (abort ()
	    :report (lambda (s)
		      (format s #"Return NIL from ~a ~s." ',operation
			      ,filespec))
	    ,abort-value-form)))

;;; Evaluates body in a loop in which form-var names successive forms
;;; from input-stream.  *package*, *readtable*, and truename-var
;;; (eg. *load-truename*) are bound appropriately.  pathname-var is
;;; assumed to be already bound (to NIL if necessary).

(defmacro xxWITH-SOURCE ((form-var input-stream pathname-var truename-var message)
			 &body body)
  (rebinding (message input-stream)
    `(when ,input-stream
       (when ,message 
	 (evaluation-progress (or ,pathname-var ,input-stream) ,message))
       (do ((,truename-var (when ,pathname-var (truename ,input-stream)))
	    (*package* *package*)
	    (*readtable* *readtable*)
	    ,form-var)
	   ((eq (setq ,form-var (read ,input-stream nil 'eof)) 'eof)
	    (or ,truename-var ,input-stream))
	 ,@body))))

(defmethod STREAM-LOAD ((stream FUNDAMENTAL-CHARACTER-INPUT-STREAM)
			verbose print)
  (let ((element-type (stream-element-type stream))
	(external-format (stream-external-format stream)))
    (vars element-type external-format stream))
  (xxwith-source (form stream *load-pathname* *load-truename*
		       (when verbose "Loading"))
    (let ((values (process form *interpreter* nil
			   null-continuation))) 
      (when print			;IWBNI we printed multiple values
	(evaluation-progress (first values))))))

(defun xxLOAD (filespec &rest keys
			&key (verbose *load-verbose*)
			(print *load-print*)
			(if-does-not-exist :error)
			(external-format :default))
  (xxwith-file-processing-restarts (load filespec keys)
    (if (streamp filespec)
	(let ((*load-pathname* nil))
	  (stream-load filespec verbose print))
	(let ((*load-pathname* (load-pathname filespec nil)))
	  (with-open-file (stream *load-pathname*
				  :if-does-not-exist if-does-not-exist
				  ;; get most restritive type for format...
				  :element-type :default
				  :external-format external-format)
	    (stream-load stream verbose print))))))

(defun xxCOMPILE-FILE (input-file &rest keys
				  &key output-file
				  (#-cmu (output-format output-format)
					 #+cmu output-format)
				  (#-cmu (loader-name loader-name)
					 #+cmu loader-name)
				  (if-exists :supersede) ;non-ANSI???
				  (verbose *compile-verbose*) 
				  (print *compile-print*)
				  (external-format :default)
				  &aux processor)
  (declare (ignorable output-file))	;Used by ;compile-file-pathnames.
  (xxwith-file-processing-restarts (compile-file input-file keys
					       (values nil t t))
    (ecase if-exists;; Make sure if-exists is not  nil, :append
      ((:error :supersede :rename :rename-and-delete :overwrite)))
    (multiple-value-bind (output-pathname *compile-file-pathname*)
	(apply #'compile-file-pathnames input-file keys)
      (multiple-value-bind (input-truename notices warnings)
	  (with-open-file (input *compile-file-pathname*
				 :external-format external-format
				 :element-type
				 (if (eq external-format :ascii)
				     'base-char 'character))
	    (xxwith-compilation-unit
		(:processor
		 (ecase (or output-format
			    (make-keyword (pathname-type output-pathname
							 :case :common)))
		   (:c (make-c-file-compiler))
		   ((:default :bin :byte) (make-byte-file-compiler)))
		 :environment (make-boundary-env))
	      (setq processor *processor*)
	      (xxwith-compiler-handlers
	       (xxwith-source (form input *compile-file-pathname*
				  *compile-file-truename*
				  (when verbose "Compiling"))
		 (when print
		   (let ((*print-length* 3) (*print-level* 2)
			 (*print-lines* 1) (*print-pretty* t))
		     (evaluation-progress form)))
		 (add-load-instruction
		  *processor*
		  (process form *processor* *environment*
			   null-continuation))))))
	(when verbose
	  (evaluation-progress output-pathname "Writing"))
	(with-open-file (output output-pathname
				:direction :output
				:element-type 'base-char
				:external-format :ascii
				:if-exists if-exists)
	  (let ((output-truename (truename output)))
	    (unless (equal (pathname-version input-truename)
			   (pathname-version output-truename))
	      (setq notices t)
	      (warn 'simple-style-warning
		    :format-control
		    "Versions do not match between ~s and ~s."
		    :format-arguments
		    (list input-truename output-truename)))
	    (compiler-dump processor output loader-name)
	    (values output-truename notices warnings)))))))


