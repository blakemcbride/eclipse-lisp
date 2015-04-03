
(defparameter *compile-verbose* t)
(defparameter *compile-print* t)
;;; *load-verbose* is set in debug.lisp
(defparameter *load-print* nil)
(defparameter *compile-file-truename* nil)
(defparameter *compile-file-pathname* nil)
(defparameter *load-truename* nil)
(defparameter *load-pathname* nil)

(defun make-interpreter (closure)
  #'(lambda (&rest args)
      (apply-interpreted-function closure args)))

(defun apply-interpreted-function (closure args)
  (let ((target (make-multiple-values-target)))
    (interpreted-call closure args nil (add-target target) nil)
    (values-list (multiple-values-target-values target))))

(let ((functions (make-hash-table)))
  (defun function-source (designator)
    (gethash designator functions))
  (defun (setf function-source) (lambda-form designator)
    (setf (gethash designator functions) lambda-form)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; UTILITES for top level user interface                        ;;;

;;; Evaluates protected-body in a dynamic context where conditions of
;;; type WARNING and ERROR are trapped:
;;;  - WARNINGs are noted and declined (i.e. signalling continues)
;;;  - ERRORs are noted and the CONTINUE-PROCESSING restart is invoked
;;;    (if available).
;;; Evaluates unprotected-body with variables named by warnings-p and
;;; style-warnings-p bound to a boolean indicating whether
;;; non-style-warnings/errors or style-warnings were signalled.

(defmacro WITH-COMPILER-HANDLERS (((warnings-p style-warnings-p)
				   &body protected-body)
				  &body unprotected-body)
  `(let (,warnings-p ,style-warnings-p)
     (handler-bind ((warning #'(lambda (c)
				 (if (typep c 'style-warning)
				     (setq ,style-warnings-p t)
				   (setq ,warnings-p t))))
		    (error #'(lambda (c)
			       (setq ,warnings-p t)
			       (let ((restart (find-restart
					       'continue-processing
					       c)))
				 (when restart
				   (invoke-restart restart c))))))
       ,@protected-body)
     ,@unprotected-body))

(defun evaluation-progress (arg &optional message (stream *standard-output*))
  (fresh-line stream)
  (if message
      (format stream #"; ~a ~s" message arg)
      (format stream #";  ~s" arg))
  (terpri stream))

;;; Evaluates body in a loop in which form-var names successive forms
;;; from merged-filespec.  *package*, *readtable*, and truename-var
;;; (eg. *compile-file-truename*) are bound appropriately.
;;; Returns truename, unless OPEN returned nil (in which case we
;;; return nil).

(defmacro WITH-SOURCE ((form-var merged-filespec truename-var
				 message
				 external-format
				 &optional (if-does-not-exist :error))
		       &body body)
  (let ((input (gensym "INPUT"))
	(msg (gensym "MSG")))
    `(with-open-file (,input ,merged-filespec
			     :if-does-not-exist ,if-does-not-exist
			     :element-type (if (eq ,external-format :ascii)
					       'base-char 'character)
			     :external-format ,external-format)
       (when ,input
	 (let ((,msg ,message))
	   (when ,msg
	     (evaluation-progress (pathname ,input) ,msg)))
	 (do ((,truename-var (truename ,input))
	      (*package* *package*)
	      (*readtable* *readtable*)
	      ,form-var)
	     ((eq (setq ,form-var (read ,input nil 'eof)) 'eof)
	      ,truename-var)
	   ,@body)))))

(defmacro with-file-processing-restarts ((operation filespec keys) &body body)
  `(restart-case (progn ,@body)
     (retry ()
	    :report (lambda (s) (format s #"Retry ~a ~s." ',operation ,filespec))
	    (apply (function ,operation) ,filespec ,keys))
     (abort ()
	    :report (lambda (s)
		      (format s #"Return NIL from ~a ~s." ',operation ,filespec)))))


(defmacro WITH-COMPILATION-UNIT ((&key override) &rest body)
  `(progn ,override
     ,@body))

(defun load-pathname (input-file &optional (type "LISP"))
  (let ((path (merge-pathnames input-file)))
    (if (pathname-type path :case :common)
	path
	(make-pathname :type
		       (or type
			   (dolist (p (directory path :links nil
						 :if-does-not-exist nil)
				      "LISP")
			     (when (string= "BIN"
					    (pathname-type p :case :common))
			       (return "BIN"))))
		       :case :common
		       :defaults path))))

;;; Returns merged output-pathname and input-pathname as values.
;;; Note that logical-pathnames are NOT translated by this function
;;; (but will be by with-open-file).
;;; Note that a :newest input-file combined with an unsupplied
;;; output-file version will result in a :newest output-file -- even
;;; if the highest existing versions of the files in the file system
;;; have different numbers!
(defun compile-file-pathnames (input-file &key output-file
					  (#-cmu (output-format output-format)
						 #+cmu output-format
						 :default)
					  &allow-other-keys)
  (let* ((input-pathname (load-pathname input-file))
	 (output-defaults
	  (make-pathname :type (if (eq output-format :c)
				   "C" "BIN")
			 :case :common
			 :defaults input-pathname)))
    (values
     (if output-file
	 (merge-pathnames output-file output-defaults)
	 output-defaults)
     input-pathname)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PUBLIC top level user interface                              ;;;

(defparameter *walkhook* nil)
(defun WALKHOOK (form walkhookfn &rest environment-args)
  (let ((*walkhook* walkhookfn))
    (apply #'walk1 form environment-args)))

(defun WALK (form lexical-env dynamic-env eval-targets declarations)
  (if *walkhook*
      (let ((hook *walkhook*) (*walkhook* nil))
	(funcall hook form lexical-env dynamic-env eval-targets
		 declarations)) 
      (walk1 form lexical-env dynamic-env eval-targets declarations)))

(defparameter *evalhook* nil)
(defun EVAL-ENV (form &optional lexical-env dynamic-env)
  (if *evalhook*
      (let ((hook *evalhook*) (*evalhook* nil))
	(funcall hook form lexical-env dynamic-env))
      (let ((target (make-multiple-values-target)))
	(walk form lexical-env dynamic-env (add-target target) nil)
	(values-list (multiple-values-target-values target)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 20.1 Run-Time Evaluation of Forms
(defun EVAL (form) (eval-env form))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 25.3 DEBUGGING TOOLS                                         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *steplevel* 0)
(defun STEP-WALK (form lexical-env dynamic-env eval-targets declarations)
  (let* ((level *steplevel*)
	 (*steplevel* (+ level 1)))
    (break #"~%~v@tStep ~d Form: ~s" (* level 2) level form)
    (let ((primary 
	   (walkhook form #'step-walk lexical-env dynamic-env
		     eval-targets declarations)))  
      (format *debug-io* #"~%~v@tStep ~d Primary value: ~s~%"
	      (* level 2) level primary)
      primary)))


(defmacro STEP (form &environment env)
  `(let ((*walkhook* #'step-walk)) (eval-env ',form ,env)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 23.4 LOADING FILES                                           ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Currently only handles source files!!!
(defun LOAD (filespec &rest keys
		      &key (verbose *load-verbose*)
		      (if-does-not-exist :error)
		      (print *load-print*)
		      (external-format :default))
  (with-file-processing-restarts (load filespec keys)
   (let ((*load-pathname* (load-pathname filespec nil))
	 (targets (make-targets)))
     (with-source (form *load-pathname* *load-truename*
			(when verbose "Loading") 
			external-format if-does-not-exist)
		  (let ((value (walk form nil nil targets nil)))
		    (when print
		      (evaluation-progress value)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 25.1 THE COMPILER                                            ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun DISASSEMBLE (fn)
  (multiple-value-bind (lambda env name)
      (if (car-eq fn 'lambda)
	  (values fn nil nil)
	  (function-lambda-expression
	   (if (or (symbolp fn) (consp fn)) (fdefinition fn) fn)))
    (when lambda
      (let ((dynamic-env (make-c-file-compiler-env)))
	(walk-instructions dynamic-env
			   (make-result dynamic-env
			     (closure-value dynamic-env name lambda
					    (make-boundary-env :env env)
					    nil))
			   t 'loader))))
  nil)

;;; Disassembles arbitrary forms.
(defun File-Compile (output-format &rest forms)
  (let* ((dynamic-env (ecase output-format
			(:c (make-c-file-compiler-env))
			(:default (make-byte-file-compiler-env))))
	 (lexical-env (make-boundary-env))
	 (instructions (make-result dynamic-env nil)))
    (with-compiler-handlers
     ((warnings-p style-warnings-p)
      (dolist (form forms)
	(setq instructions
	      (combine-results
	       dynamic-env instructions
	       (walk form lexical-env dynamic-env (make-targets) nil)))))
     (values (walk-instructions dynamic-env instructions t 'loader)
	     (or warnings-p style-warnings-p)
	     warnings-p))))


;;; Name is either a global function-name or nil.
(defmethod compile-object ((definition interpreted-function)
			   &optional (name (interpreted-function-name definition)))
  (let ((dynamic-env #+should-be	;!!!
		     (make-compiler-env)))
    (closure-value dynamic-env
		   name
		   (interpreted-function-lambda definition)
		   (interpreted-function-env definition)
		   nil)))

(defmethod compile-object ((definition cons) &optional name)
  ;; better be a lambda-expression
  (let ((dynamic-env #+should-be	;!!!
		     (make-compiler-env)))
    (closure-value dynamic-env name definition nil nil)))
			   

(defun COMPILE (name &optional (definition (fdefinition name)))
  (with-compiler-handlers
   ((warnings-p style-warnings-p)
    (setq definition (compile-object definition name)))
   (when name
     (if (macro-function name)
	 (setf (macro-function name) definition)
	 (setf (fdefinition name) definition))
     (setq definition name))
   (values definition
	   (or warnings-p style-warnings-p)
	   warnings-p)))

(defun COMPILE-FILE-PATHNAME (&rest args)
  (declare (dynamic-extent args))
  (values (apply #'compile-file-pathnames args)))

(defun COMPILE-FILE (input-file &rest keys
				&key output-file
				(#-cmu (output-format output-format)
				       #+cmu output-format)
				(#-cmu (loader-name loader-name)
				       #+cmu loader-name)
				(if-exists :supersede) ;non-ANSI???
				(verbose *compile-verbose*) 
				(print *compile-print*)
				(external-format :default))
  (declare (ignorable output-file))	;Used by compile-file-pathnames.
  (with-file-processing-restarts (compile-file input-file keys)
    (ecase if-exists			; Make sure if-exists is legal and NON-NIL.
      ((:error :supersede :rename :rename-and-delete :overwrite)))
    (multiple-value-bind (output-pathname *compile-file-pathname*)
	(apply #'compile-file-pathnames input-file keys)
      (let* ((format (or output-format
			 (make-keyword (pathname-type output-pathname
						      :case :common))))
	     (dynamic-env (ecase format
			    (:c (make-c-file-compiler-env))
			    ((:default :bin :byte)
			     (make-byte-file-compiler-env))))
	     (lexical-env (make-boundary-env))
	     (instructions (make-result dynamic-env nil))
	     (targets (make-targets))
	     input-truename)
	(with-compiler-handlers
	 ((warnings-p style-warnings-p)
	  (setq input-truename
		(with-source (form *compile-file-pathname*
				   *compile-file-truename*
				   (when verbose "Compiling")
				   external-format)
			     (when print
			       (let ((*print-length* 3) (*print-level* 2)
				     (*print-lines* 1) (*print-pretty* nil))
				 (evaluation-progress form)))
			     (setq instructions
				   (combine-results
				    dynamic-env instructions
				    (walk form lexical-env
					  dynamic-env targets
					  nil))))))
	 (when verbose
	   (evaluation-progress output-pathname "Writing"))
	 (with-open-file (output output-pathname
				 :direction :output
				 :element-type 'base-char
				 :external-format :ascii
				 :if-exists if-exists)
	   (unless (equal (pathname-version input-truename)
			  (pathname-version (truename output)))
	     (warn "Versions do not match between ~s and ~s."
		   input-truename (truename output)))
	   (walk-instructions
	    dynamic-env instructions output
	    (or loader-name
		(intern (pathname-name *compile-file-pathname*
				       :case :common))))
	   (values (truename output)
		   (or warnings-p style-warnings-p)
		   warnings-p)))))))

