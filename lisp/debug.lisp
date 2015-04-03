(defparameter *read-eval-print-help*
  "Type a form to evaluate that form.
Type an integer corresponding to one of the listed options to
  perform that action.
Type a keyword (eg. :exit) corresponding to the bracketed name
  (eg [:EXIT]) in the listed options to perform the first such
  action listed.")

(defun print-help (&optional stream)
  (let ((stream (output-stream stream)))
    (write-string *read-eval-print-help* stream)
    (terpri stream)
    (finish-output stream))
  (values))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                   COMMAND LOOPS                              ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *load-verbose* t)
(defun LISP-TOP-LEVEL (verbose init)
  (when (setq *load-verbose* verbose)
    (write-string (lisp-implementation-type))
    (write-string " Version ")
    (write-string (lisp-implementation-version))
    (terpri)
    (write-string "Copyright (c) 1997, 1998 Elwood Corporation, Oak Creek, WI, USA.")
    (finish-output))
  (restart-bind ((help #'print-help
		       :report-function
		       #'(lambda (s) (write-string "Print help." s)))
		 (exit #'(lambda (&optional status (exitp t))
			   (when exitp
			     (return-from lisp-top-level status)))
		       :interactive-function
		       #'(lambda ()
			   (list nil (yes-or-no-p "Exit Lisp?")))
		       :report-function
		       #'(lambda (s) (write-string "Exit Lisp." s))))
    (typecase init
      (null)
      (string (load init))
      (t (load (make-pathname :name "INIT" :case :common)
	       :if-does-not-exist nil)
	 (load (make-pathname :host "SYS" :directory "SITE" :name "INIT"
			      :case :common)
	       :if-does-not-exist nil)
	 (load (make-pathname :name "INIT" :case :common
			      :type nil :version :newest
			      :defaults (user-homedir-pathname))
	       :if-does-not-exist nil)))
    (read-eval-print-loop
     0 (make-condition 'simple-condition
		       :format-control "Lisp Top Level.")
     *standard-input* *standard-output*
     "Return to top level.~*" verbose
     (not (interactive-stream-p *standard-output*)))))
      

(defparameter - nil)
(defparameter + nil) (defparameter ++ nil) (defparameter +++ nil)
(defparameter * nil) (defparameter ** nil) (defparameter *** nil)
(defparameter / nil) (defparameter // nil) (defparameter /// nil)

;;; The spec does not say whether * and friends should be rebound by
;;; recursive debugger levels.  We do NOT rebind them, so that
;;; information can be retained after poping out of the debugger.

;;; Maybe abort restarts which bring us back a pariticular debug level
;;; should be associated with the condition which sent us there?
;;; Issues:
;;; 1. How could this actually be used by conforming programs?
;;; 2. This would make aborts which return us to earlier
;;; read-eval-print loops invisible.  Either the user would have to
;;; pop out one at a time or we would have to play games to make them
;;; associated.

(defun std-prompt (stream level condition)
  (declare (ignore condition))
  (fresh-line stream)
  (unless (zerop level) (princ level stream))
  (write-string "> " stream))

(defparameter *prompt-hook*  #'std-prompt)

(defun READ-EVAL-PRINT-LOOP (level condition input-stream output-stream
				   &optional (abort-format-control
					      "Return to debug level ~d.")
				   (verbose t) unmatched-streams-p)
  (handler-bind ((end-of-file #'(lambda (c)
				  (when (eq input-stream (stream-error-stream c))
				    (return-from read-eval-print-loop nil)))))
    (let ((restarts (compute-restarts condition)))
      (loop
       (when verbose
	 (report-condition condition)
	 (display-restarts restarts output-stream))
       (with-simple-restart
	   (abort abort-format-control level)
	 (loop
	  (when verbose
	    (funcall *prompt-hook* output-stream level condition))
	  (finish-output output-stream)
	  (shiftf +++ ++ + - (read input-stream))

	  ;; Make dribble output prettier.
	  ;; #\) is a terminating character, so #\return following a
	  ;; list form is not normally sucked up until the next read.
	  (when unmatched-streams-p
	    (loop for char = (read-char-no-hang input-stream nil)
		  while char
		  unless (whitespace-char-p char)
		  return (unread-char char input-stream)))
	  (setf (stream-start-line-p output-stream) t)
	  
	  (shiftf /// // / (multiple-value-list
			    (eval-or-command - restarts)))
	  (shiftf *** ** * (first /))
	  (dolist (val /)
	    (write-toplevel val output-stream)
	    (terpri output-stream))))))))

(defmethod INVOKE-DEBUGGER ((condition condition))
  (let ((*debug-level* (1+ *debug-level*)))
    (clear-input *debug-io*)
    (read-eval-print-loop *debug-level* condition
			  *debug-io* *debug-io*)))

(defparameter *debug-level* 0)
(defun DRIBBLE (&optional (pathname nil pathnamep))
  (if pathnamep
      (with-open-file (file pathname :direction :output)
	(with-open-stream (input (make-echo-stream
				  *standard-input* file))
	  (with-open-stream (output (make-broadcast-stream
				     *standard-output* file))
	    (with-open-stream (io (make-two-way-stream
				   input output))
	      (let ((*standard-input* input)
		    (*standard-output* output)
		    (*error-output* output)
		    (*trace-output* output)
		    (*debug-io* io)
		    (*query-io* io)
		    (*terminal-io* io)
		    (*debug-level* (1+ *debug-level*)))
		(with-simple-restart (dribble
				      "Stop recording session to ~s."
				      pathname)
		  (read-eval-print-loop
		   *debug-level*
		   (make-condition 'break
				   :format-control
				   "Recording session to ~s."
				   :format-arguments (list pathname))
		   input output
		   "Return to dribble level ~d." t t))))))
	(truename file))
      (let ((restart (find-restart 'dribble)))
	(when restart (invoke-restart restart)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                   UTILITIES                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun display-restarts (restarts stream &aux (counter -1))
  (when restarts
    (terpri stream)
    (write-line "Options:" stream)
    (dolist (restart restarts)
      (format stream
	      (formatter "~&~3d:~:[~*~; ~a~]~@[ [:~a]~]")
	      (incf counter)
	      (restart-report-function restart) restart
	      (restart-name restart)))
    (terpri stream)
    (finish-output stream))) 


(defun EVAL-OR-COMMAND (form restarts)
  (flet ((maybe-invoke (restart)
		       (if restart (invoke-restart-interactively restart) (eval form))))
    (typecase form
      (unsigned-byte (maybe-invoke (nth form restarts)))
      (keyword (maybe-invoke (find form restarts
				   :key #'restart-name
				   :test #'string=)))
      (t (eval form)))))

