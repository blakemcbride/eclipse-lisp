
(defun PRINT-EVAL (form stream object)
  (when (and *print-readably* (not *read-eval*))
    (let ((*print-readably* nil))
      (error 'print-not-readable :object object)))
  (write-string "#." stream)
  (print-object form stream))

;;; See different method on pretty-printing-stream
(defmethod maybe-force-output ((stream stream))
  (force-output stream))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CORE INTERFACE                                               ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The specification for PRINT-OBJECT:
;;;   The printer's decision of whether an object has components (and
;;;   therefore should not be printed when the printing depth is not
;;;   less than *print-level*) is implementation-dependent.
;;;
;;; Meanwhile, the specification of PPRINT-LOGICAL-BLOCK says that
;;; *print-level* refers to the dynamic nesting depth of logical
;;; blocks -- it doesn't say anything about calls to WRITE or
;;; PRINT-OBJECT.  The pprint-logical-block notes also point out that
;;; one use of PPRINT-LOGICAL-BLOCK is to peform *print-level*
;;; checking.
;;;
;;; We will take the position that only pprint-logical-block descends
;;; a level, but that both pprint-logical-block and write check -- but
;;; only for non-atomicp objects, where non-atomicp objects are those
;;; that do circularity checking.  (Print-object-aborted-p checks
;;; level and circularity.)

;;; Any non-atomicp object might dispatch to something that calls
;;; pprint-logcial-block, which also checks for level and, for conses
;;; only, circularity.  This second circularity test will screw things
;;; up, so we recons conses to avoid the second test.  Alternatively,
;;; we could take the attitude that anyone writing their own print
;;; function for lists must must do their own level and circularity
;;; checking (eg. by using pprint-logical-block), and thus we could
;;; avoid the test here and not have to recons.

(defun WRITE-TOPLEVEL (object stream)
  (let ((circlep *print-circle*))
    (if (and circlep (null *circularity-map*))
	(with-two-circularity-passes (stream)
	  (write-toplevel object stream))
	(unless (and (not (atomicp object))
		     (print-object-aborted-p object stream))
	  (dispatching-print (if (and circlep (consp object))
				 (cons (car object)
				       (cdr object))
				 object)
			     stream))))
  object) 


;;; Implements Section 22.1.2 PRINTER DISPATCHING.
;;; When *print-pretty*, this more or less identical to
;;;   (funcall (pprint-dispatch object *print-pprint-dispatch*) s object)
(defparameter *print-error* nil)
(defun DISPATCHING-PRINT (object s)
  (handler-case
      (let ((printer (when *print-pretty*
		       (get-printer object *print-pprint-dispatch*))))
	(if printer
	    (funcall printer s object)
	    (print-object object s)))
    (error (c)
	   (if (or *print-readably* *print-error*)
	       (let (*print-readably* *print-error*) (error c))
	       (print-unreadable-object (object s :identity t)
		 (write-string "Unprintable Data" s))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 22.1.6  WHAT THE PRINT FUNCTION PRODUCES                     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Since conforming programs are not allowed to modify the
;;; standard-readtable, it is permissible, though not very robust, for
;;; this to use a direct reference to *standard-readtable*, rather than
;;; a copy.

;;; IWBNI we implemented the readtable accessors to accept a null
;;; value for *readtable* as indicating a copy of the
;;; *standard-readtable* is to be assumed. For reads, this would just
;;; use the standard value. For writes, a copy would be created and
;;; installed in the *readtable* symbol-value before modification.

(defmacro WITH-STANDARD-IO-SYNTAX (&body body)
  `(let ((*package* (find-package :USER))
	 (*print-array* t)
	 (*print-base* 10)
	 (*print-case* :upcase)
	 (*print-circle* nil)
	 (*print-escape* t)
	 (*print-gensym* t)
	 (*print-length* nil)
	 (*print-level* nil)
	 (*print-lines* nil)
	 (*print-miser-width* nil)
	 (*print-pprint-dispatch* *ipd*)
	 (*print-pretty* nil)
	 (*print-radix* nil)
	 (*print-readably* t)
	 (*print-right-margin* nil)
	 (*read-base* 10)
	 (*read-default-float-format* 'single-float)
	 (*read-eval* t)
	 (*read-suppress* nil)
	 (*readtable* *standard-readtable*))
     ,@body))


(defun print-unreadable-object-header (object stream type identity body)
  (when *print-readably*
    (let ((*print-readably* nil))
      (error 'print-not-readable :object object)))
  (write-string "#<" stream)
  (when type
    (let ((*print-level* nil) (*print-length* nil) (*print-circle* nil))
      #+ugly-but-fast
      (write-toplevel (class-name-of object) stream)
      #-pretty-but-slow 
      (format stream (formatter "~:(~s~)") (class-name-of object)))
    (when (or body identity)
      (write-char #\space stream))))

(defun print-unreadable-object-trailer (object stream identity body)
  (when identity
    (when body
      (write-char #\space stream))
    (write (int-integer (object-word object))
	   :stream stream :radix nil :base 16))
  (write-char #\> stream) (maybe-force-output stream)
  nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 22.3 OUTPUT FUNCTIONS                                        ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 22.3.1 OUTPUT TO CHARACTER STREAMS

(defun WRITE (object
	      &key
	      (stream *standard-output*)
	      ((:array *print-array*) *print-array*)
	      ((:base *print-base*) *print-base*)
	      ((:case *print-case*) *print-case*)
	      ((:circle *print-circle*) *print-circle*)
	      ((:escape *print-escape*) *print-escape*)
	      ((:gensym *print-gensym*) *print-gensym*)
	      ((:length *print-length*) *print-length*)
	      ((:level *print-level*) *print-level*)
	      ((:lines *print-lines*) *print-lines*)
	      ((:miser-width *print-miser-width*) *print-miser-width*)
	      ((:pprint-dispatch *print-pprint-dispatch*) *print-pprint-dispatch*)
	      ((:pretty *print-pretty*) *print-pretty*)
	      ((:radix *print-radix*) *print-radix*)
	      ((:readably *print-readably*) *print-readably*)
	      ((:right-margin *print-right-margin*) *print-right-margin*))
  (write-toplevel object stream))

(defun PRIN1 (object &optional (stream *standard-output*))
  (let ((*print-escape* t)
	(stream (output-stream stream)))
    (write-toplevel object stream)
    (maybe-force-output stream)
    object))
  
(defun PRINC (object &optional (stream *standard-output*))
  (let ((*print-escape* nil)
	(*print-readably* nil)
	(stream (output-stream stream)))
    (write-toplevel object stream)
    (maybe-force-output stream)
    object))


(defun PRINT (object &optional (stream *standard-output*))
  (setq stream (output-stream stream))
  (terpri stream)
  (prin1 object stream)
  (write-char #\space stream)
  (maybe-force-output stream)
  object)

(defun PPRINT (object &optional (stream *standard-output*))
  (let ((*print-pretty* t)
	(*print-escape* t)
	(stream (output-stream stream)))
    (terpri stream)
    (write-toplevel object stream)
    (maybe-force-output stream))
  (values))

(defun WRITE-TO-STRING (object &rest keys &key &allow-other-keys)
  (declare (dynamic-extent keys))
  (with-output-to-string (s)
    (apply #'write object :stream s keys)))

(defun PRIN1-TO-STRING (object)
  (with-output-to-string (s)
    (prin1 object s)))

(defun PRINC-TO-STRING (object)
  (with-output-to-string (s)
    (princ object s)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 22.4 QUERYING THE USER                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun Y-OR-N-P (&optional format-string &rest arguments)
  (when format-string
    (fresh-line *query-io*)
    (format *query-io* format-string arguments))
  (write-string " (Y or N) " *query-io*)
  (loop (let ((line (read-line *query-io*)))
	  (cond ((string-equal "y" line) (return t))
		((string-equal "yes" line) (return t))
		((string-equal "n" line) (return nil))
		((string-equal "no" line) (return nil))))
    (write-string "Type \"y\" for yes or \"n\" for no. " *query-io*)))

(defun YES-OR-NO-P (&optional format-string &rest arguments)
  (write-char #\bell *query-io*)
  (when format-string
    (fresh-line *query-io*)
    (format *query-io* format-string arguments))
  (write-string " (Yes or No) " *query-io*)
  (loop (let ((line (read-line *query-io*)))
	  (cond ((string-equal "yes" line) (return t))
		((string-equal "no" line) (return nil))))
    (write-string "Type \"yes\" for yes or \"no\" for no. " *query-io*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parameters that need to be defined before reading pretty.lisp
(eval-when (:compile-toplevel :load-toplevel :execute) ;used at run time
  (defvar block-stack-min-size #.(* 35. block-stack-entry-size))
  (defvar prefix-stack-min-size #.(* 30. prefix-stack-entry-size))
  (defvar queue-min-size #.(* 75. queue-entry-size))
  (defvar buffer-min-size 256.)
  (defvar prefix-min-size 256.)
  (defvar suffix-min-size 256.)) 
