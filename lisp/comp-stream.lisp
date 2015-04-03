
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; COMPOSITE STREAMS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; "a stream that is composed of one or more other streams."
(defclass composite-stream (constructed-stream) ())

(defmethod stream-input-stream :before ((stream COMPOSITE-STREAM))
  (unless (open-stream-p stream)
    (error 'closed-stream :stream stream)))
(defmethod stream-output-stream :before ((stream COMPOSITE-STREAM))
  (unless (open-stream-p stream)
    (error 'closed-stream :stream stream)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ENCAPSULATING STREAMS

(defclass ENCAPSULATING-STREAM (composite-stream)
  ((stream :initarg :stream
	   :accessor ENCAPSULATING-STREAM-STREAM)))

(macrolet ((def-pred (name class)
	     `(progn (defmethod ,name ((stream t)) nil)
		     (defmethod ,name ((stream ,class)) t))))
  (def-pred ENCAPSULATING-STREAM-P encapsulating-stream))

(defmethod stream-input-stream ((stream ENCAPSULATING-STREAM))
  (encapsulating-stream-stream stream))
(defmethod stream-output-stream ((stream ENCAPSULATING-STREAM))
  (encapsulating-stream-stream stream))

(macrolet
    ((def-tramp (op lambda stream &body body)
       `(defmethod ,op ,lambda
	  (let ((*original-stream* ,stream)) ,@body)))
     (def-tramp1 (op accessor remaining-lambda remaining-args)
       `(def-tramp ,op ((stream ENCAPSULATING-STREAM) ,@remaining-lambda)
	  stream
	  (,op (,accessor stream) ,@remaining-args)))
     (def-trampoline (op &optional lambda (args lambda))
       `(def-tramp1 ,op encapsulating-stream-stream ,lambda ,args))
     (def-tramp-input (op &optional lambda (args lambda))
       `(def-tramp1 ,op stream-input-stream ,lambda ,args))
     (def-tramp-output (op &optional lambda (args lambda))
       `(def-tramp1 ,op stream-output-stream ,lambda ,args)))
	  
  (def-trampoline INTERACTIVE-STREAM-P)
  (def-trampoline STREAM-ELEMENT-TYPE)
  (def-trampoline PATHNAME)
  (def-trampoline TRUENAME)
  
  (def-tramp-input INPUT-STREAM-P)
  (def-tramp-input stream-READ-CHAR)
  (def-tramp-input stream-UNREAD-CHAR (character))
  (def-tramp-input stream-READ-CHAR-NO-HANG)
  (def-tramp-input stream-PEEK-CHAR)
  (def-tramp-input stream-READ-LINE)
  (def-tramp-input stream-LISTEN)
  (def-tramp-input stream-CLEAR-INPUT)
  (def-tramp-input stream-READ-SEQUENCE
    ((sequence SEQUENCE) &key (start 0) end)
    (sequence :start start :end end))

  (def-tramp-output OUTPUT-STREAM-P)
  (def-tramp-output stream-WRITE-CHAR (character))
  (def-tramp-output stream-START-LINE-P)
  (def-tramp-output stream-LINE-COLUMN)
  (def-tramp-output stream-LINE-LENGTH)
  (def-tramp-output stream-TERPRI)
  (def-tramp-output stream-FRESH-LINE)
  (def-tramp-output stream-CLEAR-OUTPUT)
  (def-tramp-output stream-FINISH-OUTPUT)
  (def-tramp-output stream-FORCE-OUTPUT)
  (def-tramp-output stream-ADVANCE-TO-COLUMN (column))
  (def-tramp-output stream-WRITE-STRING
    ((string STRING) &optional (start 0) end)
    (string start end))
  (def-tramp-output stream-WRITE-SEQUENCE
    ((sequence SEQUENCE) &key (start 0) end)
    (sequence :start start :end end)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SYNONYM STREAMS

(defclass SYNONYM-STREAM (encapsulating-stream)
  ((symbol :initarg :symbol
	   :reader SYNONYM-STREAM-SYMBOL)))

(defmethod ENCAPSULATING-STREAM-STREAM ((stream SYNONYM-STREAM))
  (symbol-value (synonym-stream-symbol stream)))

(defun MAKE-SYNONYM-STREAM (symbol)
  (make-instance (find-type 'synonym-stream) :symbol symbol))

(defmethod PRINT-OBJECT ((object synonym-stream) s)
  (print-unreadable-object (object s :identity t :type t)
    (princ (synonym-stream-symbol object) s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BROADCAST STREAMS

(defclass BROADCAST-STREAM (output-stream encapsulating-stream)
  ((streams :initarg :streams :reader BROADCAST-STREAM-STREAMS)))

(defun MAKE-BROADCAST-STREAM (&rest streams)
  (every #'ensure-output-stream streams)
  (make-instance (find-type 'broadcast-stream)
		 :streams streams
		 :stream (first (last streams))))

;;; These might also be handled by defining a default method on NULL,
;;; but then we wouldn't get the correct type-error behavior.
(macrolet ((def-broadcast (op val)
	     `(defmethod ,op ((stream BROADCAST-STREAM))
		(if (encapsulating-stream-stream stream)
		    (call-next-method)
		    ,val))))
  (def-broadcast STREAM-ELEMENT-TYPE t)
  (def-broadcast stream-line-column nil)
  (def-broadcast stream-line-length nil)
  #|;!!!(def-broadcast FILE-LENGTH 0)
  (def-broadcast FILE-POSITION 0)
  (def-broadcast FILE-STRING-LENGTH 1)
  (def-broadcast STREAM-EXTERNAL-FORMAT :default)|#)

(defmethod INPUT-STREAM-P ((stream BROADCAST-STREAM)) nil)
(defmethod INTERACTIVE-STREAM-P ((stream BROADCAST-STREAM)) nil)

(macrolet ((def-broadcast (op val &optional lambda (args lambda))
	     `(defmethod ,op ((stream BROADCAST-STREAM) ,@lambda)
		(let ((last ,val))
		  (dolist (stream (broadcast-stream-streams stream) last)
		    (setq last (,op stream ,@args)))))))
  (def-broadcast stream-WRITE-CHAR character (character))
  (def-broadcast stream-TERPRI nil)
  (def-broadcast stream-FRESH-LINE nil)
  (def-broadcast stream-FINISH-OUTPUT nil)
  (def-broadcast stream-FORCE-OUTPUT nil)
  (def-broadcast stream-CLEAR-OUTPUT nil)
  (def-broadcast stream-ADVANCE-TO-COLUMN nil (column)))
  
(defmethod stream-WRITE-SEQUENCE ((stream BROADCAST-STREAM)
				  (sequence SEQUENCE)
				  &key (start 0) end)
  (dolist (stream (broadcast-stream-streams stream) sequence)
    (stream-write-sequence stream sequence :start start :end end)))

(defmethod STREAM-WRITE-STRING ((stream BROADCAST-STREAM)
				(sequence string)
				&optional (start 0) end)
  (dolist (stream (broadcast-stream-streams stream) sequence)
    (stream-write-string stream sequence start end)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CONCATENATED STREAMS

(defclass CONCATENATED-STREAM (fillable-character-input-stream encapsulating-stream)
  ((streams :initarg :streams :reader CONCATANATED-STREAM-STREAMS)))

(defun MAKE-CONCATENATED-STREAM (&rest streams
				       &aux (buffer (first streams)))
  (every #'ensure-input-stream streams)
  (make-instance (find-type 'concatenated-stream)
		 :streams streams :stream buffer))

;;; Ensures that no reference to the input-stream will see :EOF unless
;;; we're on the last stream.
(defmethod stream-input-stream :before ((stream CONCATENATED-STREAM))
  (with-slots ((input stream)) stream
    (when (eq :eof (stream-peek-char input))
      (stream-fill-input stream))))

;;; Oops - unread-char uses stream-input-stream, which, without this
;;; method, might advance to the next stream before we have a chance
;;; to unread the character back into the old stream.
(defmethod stream-UNREAD-CHAR ((stream CONCATENATED-STREAM)
			       (character CHARACTER))
  (let ((*original-stream* stream))
    (stream-unread-char (encapsulating-stream-stream stream)
			character)))

(defmethod stream-CLEAR-INPUT ((stream CONCATENATED-STREAM))
  (let ((input (stream-input-stream stream)))
    (if (or (interactive-stream-p input)
	    (null (stream-fill-input stream)))
	(stream-clear-input input)
	(stream-clear-input stream))))

(defmethod stream-fill-input ((stream CONCATENATED-STREAM) &optional ignore)
  (declare (ignore ignore))
  (with-slots (streams stream) stream
    (let ((more (rest streams)))
      (when more
	(let ((new (first more)))
	  (setf streams more
		stream new))
	stream))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BIDIRECTIONAL STREAMS

(defun make-bidirectional-stream (class-name input-stream output-stream)
  (make-instance (find-type class-name)
		 :interactive-p (interactive-stream-p input-stream)
		 :element-type (ensure-element-type-compatibility
				(list input-stream output-stream))
		 :input-stream (ensure-input-stream input-stream)
		 :output-stream (ensure-output-stream output-stream)))


(defclass BIDIRECTIONAL-ENCAPSULATING-STREAM
  (encapsulating-stream bidirectional-stream)
  ((input-stream :initarg :input-stream
		 :reader stream-input-stream)
   (output-stream :initarg :output-stream
		  :reader stream-output-stream)
   (interactive-p :initform nil :reader INTERACTIVE-STREAM-P
		  :initarg :interactive-p)
   (element-type :reader STREAM-ELEMENT-TYPE
		 :initarg :element-type
		 :initform *default-element-type*)))

(defmethod stream-input-stream :BEFORE ((stream BIDIRECTIONAL-ENCAPSULATING-STREAM))
  (finish-output (stream-output-stream stream)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TWO-WAY STREAMS

(defclass TWO-WAY-STREAM (bidirectional-encapsulating-stream)
  ((input-stream :reader TWO-WAY-STREAM-INPUT-STREAM)
   (output-stream :reader TWO-WAY-STREAM-OUTPUT-STREAM)))

(defun MAKE-TWO-WAY-STREAM (input-stream output-stream)
  (make-bidirectional-stream 'two-way-stream input-stream output-stream))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ECHO STREAMS

;;; Note that echo-stream and two-way-stream are disjoint.
(defclass ECHO-STREAM (bidirectional-encapsulating-stream)
  ((input-stream :reader ECHO-STREAM-INPUT-STREAM)
   (output-stream :reader ECHO-STREAM-OUTPUT-STREAM)
   (unread :initform nil :accessor echo-stream-unread)))

(defun MAKE-ECHO-STREAM (input-stream output-stream)
  (make-bidirectional-stream 'echo-stream input-stream output-stream))

(defmethod stream-UNREAD-CHAR ((stream ECHO-STREAM) (character CHARACTER))
  (setf (echo-stream-unread stream) character)
  nil)

(defmethod stream-CLEAR-INPUT :AFTER ((stream ECHO-STREAM))
  (setf (echo-stream-unread stream) nil))

(macrolet
    ((def-read (name)
       `(defmethod ,name ((stream ECHO-STREAM))
	  (let ((unread (echo-stream-unread stream)))
	    (cond (unread (setf (echo-stream-unread stream) nil)
			  unread)
		  (t (let ((char (,name (stream-input-stream stream))))
		       ;; In our implementation, char might be :eof,
		       ;; or in the case of read-char-no-hang, nil to
		       ;; indicate no character is ready.  We allow
		       ;; users to define other NON-CHARACTER results
		       ;; which are not echoed.
		       (when (characterp char)
			 (stream-write-char (stream-output-stream stream) char))
		       char)))))))
  (def-read stream-READ-CHAR)
  (def-read stream-READ-CHAR-NO-HANG))

(defmethod stream-PEEK-CHAR ((stream ECHO-STREAM))
  (or (echo-stream-unread stream)
      (stream-peek-char (stream-input-stream stream))))

(defmethod stream-LISTEN ((stream ECHO-STREAM))
  (if (echo-stream-unread stream) t
      (stream-listen (stream-input-stream stream))))

(defmethod stream-READ-LINE ((stream ECHO-STREAM))
  (let ((char (echo-stream-unread stream)))
    (when char (setf (echo-stream-unread stream) nil))
    (when (eql char #\newline)
      (return-from stream-read-line (values "" nil)))
    (multiple-value-bind (string eofp)
	(stream-read-line (stream-input-stream stream))
      (let ((output (stream-output-stream stream)))
	(stream-write-sequence output string)
	(unless eofp (stream-terpri output)))
      (when char
	(setf string (replace (make-array (1+ (length string))
					  :element-type
					  (array-element-type string))
			      string :start1 1)
	      (char string 0) char))
      (values string eofp))))

(defmethod stream-READ-SEQUENCE ((stream ECHO-STREAM) sequence
				 &key (start 0) end)
  (if (and end (>= start end))
      (setq end start)
      (let ((char (echo-stream-unread stream)))
	(when char
	  (setf (elt sequence start) char
		(echo-stream-unread stream) nil)
	  (incf start))
	(setq end (stream-read-sequence
		   (stream-input-stream stream) sequence
		   :start start :end end))
	(stream-write-sequence
	 (stream-output-stream stream) sequence
	 :start start :end end)))
  end)


(defun initilize-stream-variables (input-fd output-fd error-fd)
  (setq *standard-input*
	(make-instance 'ascii-file-input-stream
		       :pathname "stdin"
		       :interactive-p (interactivep input-fd)
		       :input-fd input-fd))
  (setq *standard-output*
	(make-instance 'ascii-file-output-stream
		       :pathname "stdout"
		       :interactive-p (interactivep input-fd)
		       :output-fd output-fd))
  (setq *error-output*
	(make-instance 'ascii-file-output-stream
		       :pathname "stderr"
		       :interactive-p (interactivep input-fd)
		       :output-fd error-fd))
  (setq *terminal-io*
	(make-two-way-stream *standard-input* *standard-output*))
  (setq *debug-io* (make-synonym-stream '*terminal-io*)
	*query-io* (make-synonym-stream '*terminal-io*)
	*trace-output* *standard-output*
	*null-output* (make-broadcast-stream))
  *terminal-io*)
(initilize-stream-variables 0 1 2)

