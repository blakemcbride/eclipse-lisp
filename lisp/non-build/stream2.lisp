;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EXTERNAL STREAMS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Connected to some non-lisp entity, usually through a Lisp buffer.

(defclass external-stream (fundamental-stream)
  ((external-format :initarg :external-format
		    :initform :default
		    :reader stream-external-format)))
(defclass buffered-external-input-stream (buffered-input-stream
					  external-stream) ())
(defclass buffered-external-output-stream (buffered-output-stream
					   external-stream) ())

(defmethod SHARED-INITIALIZE :AFTER
  ((stream BUFFERED-EXTERNAL-INPUT-STREAM) slots &rest ignore)
  (declare (ignore ignore))
  (when (initialize-slot-p stream 'input-buffer slots)
    (let ((buffer (make-array *default-buffer-size*
			      :element-type (stream-element-type stream))))
      (setf (slot-value stream 'original-input-buffer) buffer
	    (slot-value stream 'end) 0))))

(defmethod SHARED-INITIALIZE :AFTER
  ((stream BUFFERED-EXTERNAL-OUTPUT-STREAM) slots &rest ignore)
  (declare (ignore ignore))
  (when (initialize-slot-p stream 'output-buffer slots)
    (setf (slot-value stream 'output-buffer)
	  (make-buffer (stream-element-type stream)))))


(defmethod check-buffer-size ((stream BUFFERED-EXTERNAL-OUTPUT-STREAM)
			      n-additional-items)
  (with-slots (output-buffer) stream
    (let* ((buf output-buffer)
	   (current (array-dimension buf 0))
	   (pos (fill-pointer buf))
	   (needed (loop for needed = (+ pos n-additional-items)
			 until (>= current needed)
			 do (stream-force-output stream)
			 do (setf pos (fill-pointer buf))
			 finally (return needed))))
      ;; Replace won't go beyond fill-pointer
      (setf (fill-pointer buf) needed)
      pos)))

(defmethod stream-FINISH-OUTPUT :BEFORE ((stream buffered-external-output-stream))
  (stream-force-output stream))


(defclass buffered-external-character-input-stream
  (buffered-character-input-stream
   buffered-external-input-stream)
  ())
(defclass buffered-external-character-output-stream
  (buffered-character-output-stream
   buffered-external-output-stream)
  ((buffer-line-column :initform 0 :accessor buffer-line-column)))

(defclass buffered-external-binary-input-stream
  (buffered-binary-input-stream buffered-external-input-stream)
  ())
(defclass buffered-external-binary-output-stream
  (buffered-binary-output-stream buffered-external-output-stream)
  ())


(defmethod stream-TERPRI :AFTER
  ((stream BUFFERED-EXTERNAL-CHARACTER-OUTPUT-STREAM))
  (stream-force-output stream))

(defmethod stream-LINE-COLUMN ((stream
				buffered-external-character-output-stream))  
  (let* ((string (output-buffer stream))
	 (pos (fill-pointer string))
	 (start (position #\newline string :end pos :from-end t)))
    (if start
	(- pos start 1)
	(+ (buffer-line-column stream) pos))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FILE STREAMS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass FILE-STREAM (external-stream)
  ((pathname :initarg :pathname
	     :reader pathname
	     :reader stream-pathname)
   (truename :initarg :truename
	     :reader stream-truename)))
(defclass FILE-INPUT-STREAM (file-stream input-stream)
  ((input-fd :initarg :input-fd :reader input-fd)))
(defclass FILE-OUTPUT-STREAM (file-stream output-stream)
  ((output-fd :initarg :output-fd :reader output-fd)
   (renamed :initarg :renamed :initform nil)
   (cleanup :initarg :cleanup :initform nil)))

(defmethod PRINT-OBJECT ((object file-stream) s)
  (print-unreadable-object (object s :identity t :type t)
    (princ (pathname object) s)))

;;; For all bidrectional file-streams that Eclipse creates,
;;; input-fd = output-fd.  Anyone who defines a file-stream for which
;;; this is not true must define the close, file-lenghth and file-stream behavior.
(defmethod file-stream-descriptor ((stream file-input-stream)) (input-fd stream))
(defmethod file-stream-descriptor ((stream file-output-stream)) (output-fd stream))

(defmethod CLOSE :BEFORE ((stream FILE-OUTPUT-STREAM) &key abort)
  (if abort
      (clear-output stream)
      (finish-output stream)))

(defmethod CLOSE :AFTER ((stream FILE-STREAM) &key abort)
  (declare (ignore abort))
  (file-descriptor-close (file-stream-descriptor stream)))

(defmethod CLOSE :AFTER ((stream FILE-OUTPUT-STREAM) &key abort)
  (with-slots (truename renamed cleanup) stream
    (cond (abort (delete-file truename)
		 (when renamed (rename-file renamed truename)))
	  (t (case cleanup
	       (:expunge (expunge-file renamed))
	       (:delete (delete-file renamed)))))))

(defmethod stream-CLEAR-INPUT :BEFORE ((stream FILE-INPUT-STREAM))
  ;; tcflush(fd, TCIFLUSH), or ioctl(fd, TCFLSH, TCIFLUSH)
  (flush-input (input-fd stream)))

(defmethod stream-CLEAR-OUTPUT :BEFORE ((stream FILE-OUTPUT-STREAM))
  ;; tcflush(fd, TCOFLUSH), or ioctl(fd, TCFLSH, TCOFLUSH)
  (flush-output (output-fd stream)))

(defmethod stream-FINISH-OUTPUT ((stream FILE-OUTPUT-STREAM))
  ;; tcdrain(fd), or loop until ioctl(fd, TIOCOUTQ, &n) gives n==0
  (drain (output-fd stream))
  nil)

(defclass buffered-file-stream (file-stream) ())
(defclass buffered-file-input-stream (buffered-file-stream
				      fillable-input-stream
				      buffered-external-input-stream
				      file-input-stream)
  ())
(defclass buffered-file-output-stream (buffered-file-stream
				       buffered-external-output-stream
				       file-output-stream)
  ())
(defclass buffered-file-bidirectional-stream (buffered-file-stream
					      fillable-input-stream
					      buffered-external-input-stream
					      buffered-external-output-stream
					      file-input-stream
					      file-output-stream
					      bidirectional-stream)
  ())

  

(defmethod stream-FORCE-OUTPUT ((stream BUFFERED-FILE-OUTPUT-STREAM))
  (with-slots (output-buffer) stream
    (let* ((buffer output-buffer)
	   (n (stream-write stream (length buffer))))
      (replace buffer buffer :start2 n)
      (decf (fill-pointer buffer) n)
      nil)))

;;; Returns the number of characters read, or nil if at end of file.
;;; Unless told not to, we wait until at least one character has been
;;; read.
;;; N.B. When not waiting on an interactive stream, eof is
;;; indistinguishable from having no characters available.
(defmethod stream-fill-input ((stream BUFFERED-FILE-INPUT-STREAM)
			      &optional (wait t))
  (when (and (not wait) (interactive-stream-p stream)
	     (not (poll (input-fd stream))))
    (return-from stream-fill-input 0))
  (with-slots (input-buffer position end offset) stream
    (let ((n (stream-read stream (length input-buffer))))
      (cond ((plusp n) 
	     (setf position offset
		   end (+ offset n))
	     n)
	    #+old-code
	    ((interactive-stream-p stream)
	     (cond (wait (let ((fd (input-fd stream)))
			   (block-stream fd)
			   (unwind-protect (when (plusp (stream-read stream 1))
					     (setf position offset
						   end (1+ offset))
					     1)
			     (unblock-stream fd))))
		   (t 0)))))))

  
(defclass buffered-character-file-input-stream
  (fillable-character-input-stream
   buffered-file-input-stream
   buffered-external-character-input-stream) ())
(defclass buffered-character-file-output-stream
  (buffered-file-output-stream buffered-external-character-output-stream) ())
(defclass buffered-character-file-bidirectional-stream
  (fillable-character-input-stream
   buffered-file-input-stream
   buffered-file-output-stream
   buffered-external-character-input-stream
   buffered-external-character-output-stream
   buffered-file-bidirectional-stream) ())

(defclass buffered-binary-file-input-stream
  (buffered-file-input-stream buffered-external-binary-input-stream) ())
(defclass buffered-binary-file-output-stream
  (buffered-file-output-stream buffered-external-binary-output-stream) ())

(defmethod stream-READ-CHAR :BEFORE ((stream BUFFERED-CHARACTER-FILE-INPUT-STREAM))
  (with-slots (position end) stream
    (when (>= position end) (stream-fill-input stream))))

(defmethod stream-PEEK-CHAR :BEFORE ((stream BUFFERED-CHARACTER-FILE-INPUT-STREAM))
  (with-slots (position end) stream
    (when (>= position end) (stream-fill-input stream))))

(defmethod stream-READ-CHAR-NO-HANG ((stream BUFFERED-CHARACTER-FILE-INPUT-STREAM))
  (with-slots (input-buffer position end) stream
    (let ((pos position))
      (when (>= pos end)
	(case (stream-fill-input stream nil)
	  (nil (return-from stream-read-char-no-hang :eof))
	  (0 (return-from stream-read-char-no-hang nil))
	  (t (setq pos position))))
      (setf position (1+ pos))
      (char input-buffer pos))))


(defmethod stream-FORCE-OUTPUT ((stream BUFFERED-CHARACTER-FILE-OUTPUT-STREAM))
  (with-slots (output-buffer buffer-line-column) stream
    (let* ((buffer output-buffer)
	   (n (stream-write stream (length buffer)))
	   (pos (position #\newline buffer :end n :from-end t)))
      (setf buffer-line-column
	    (if pos
		(- n pos 1)
		(+ buffer-line-column n)))
      (replace buffer buffer :start2 n)
      (decf (fill-pointer buffer) n)
      nil)))


;;; The aux string in mb-file-input-stream is used to store shift state.  
(macrolet
    ((def-file-stream (root external-format element-type &rest aux)
       (let ((base (make-name "~a-FILE-STREAM" root))
	     (input (make-name "~a-FILE-INPUT-STREAM" root))
	     (output (make-name "~a-FILE-OUTPUT-STREAM" root)))
	 `(progn
	    (defclass ,base (fundamental-character-stream)
	      ((external-format :initform ,external-format)
	       (element-type :initform ,element-type)))
	    (defclass ,input (,base buffered-character-file-input-stream)
	      (,@aux))
	    (defclass ,output (,base buffered-character-file-output-stream)
	      ())
	    (defclass ,(make-name "~a-FILE-BIDIRECTIONAL-STREAM" root)
	      (,input ,output buffered-character-file-bidirectional-stream)
	      ())))))
  ;; CHARACTER STREAMS
  (def-file-stream ASCII :ascii 'base-char)
  (def-file-stream UCS-2 :ucs 'ucs-2-char)
  (def-file-stream UCS-4 :ucs 'unicode-char)
  (def-file-stream MB :multi-byte (if (member :ucs-4-io *features*)
				      'unicode-char
				      'ucs-2-char)
    (aux :initform (make-array 6
			       :element-type 'base-char
			       :fill-pointer 0)))
  ;; BINARY STREAMS 
  (def-file-stream beu8 :big-endian '(unsigned-byte 8))
  (def-file-stream beu16 :big-endian '(unsigned-byte 16))
  (def-file-stream beu32 :big-endian '(unsigned-byte 32))
  (def-file-stream bes8 :big-endian '(signed-byte 8))
  (def-file-stream bes16 :big-endian '(signed-byte 16))
  (def-file-stream bes32 :big-endian '(signed-byte 32))
  (def-file-stream leu8 :little-endian '(unsigned-byte 8))
  (def-file-stream leu16 :little-endian '(unsigned-byte 16))
  (def-file-stream leu32 :little-endian '(unsigned-byte 32))
  (def-file-stream les8 :little-endian '(signed-byte 8))
  (def-file-stream les16 :little-endian '(signed-byte 16))
  (def-file-stream les32 :little-endian '(signed-byte 32)))

(macrolet ((def-stream-read (class function &rest aux)
	     `(defmethod stream-read ((stream ,class) n)
		(with-slots (input-fd input-buffer offset ,@aux) stream
		    (,function input-fd input-buffer offset n ,@aux)))))
  (def-stream-read ascii-file-input-stream read-ascii)
  (def-stream-read ucs-2-file-input-stream read-ucs2)
  (def-stream-read ucs-4-file-input-stream read-ucs4)
  (def-stream-read mb-file-input-stream read-mb aux)
  (def-stream-read beu8-file-input-stream read-beu8)
  (def-stream-read beu16-file-input-stream read-beu16)
  (def-stream-read beu32-file-input-stream read-beu32)
  (def-stream-read leu8-file-input-stream read-leu8)
  (def-stream-read leu16-file-input-stream read-leu16)
  (def-stream-read leu32-file-input-stream read-leu32)
  (def-stream-read bes8-file-input-stream read-bes8)
  (def-stream-read bes16-file-input-stream read-bes16)
  (def-stream-read bes32-file-input-stream read-bes32)
  (def-stream-read les8-file-input-stream read-les8)
  (def-stream-read les16-file-input-stream read-les16)
  (def-stream-read les32-file-input-stream read-les32))

(macrolet ((def-stream-write (class function)
	     `(defmethod stream-write ((stream ,class) n)
		(with-slots (output-fd output-buffer) stream
		  (multiple-value-bind (vector offset)
		      (get-simple-vector output-buffer 0)
		    (,function output-fd vector offset n))))))
  (def-stream-write ascii-file-output-stream write-ascii)
  (def-stream-write ucs-2-file-output-stream write-ucs2)
  (def-stream-write ucs-4-file-output-stream write-ucs4)
  (def-stream-write mb-file-output-stream write-mb)
  (def-stream-write beu8-file-output-stream write-beu8)
  (def-stream-write beu16-file-output-stream write-beu16)
  (def-stream-write beu32-file-output-stream write-beu32)
  (def-stream-write leu8-file-output-stream write-leu8)
  (def-stream-write leu16-file-output-stream write-leu16)
  (def-stream-write leu32-file-output-stream write-leu32)
  (def-stream-write bes8-file-output-stream write-bes8)
  (def-stream-write bes16-file-output-stream write-bes16)
  (def-stream-write bes32-file-output-stream write-bes32)
  (def-stream-write les8-file-output-stream write-les8)
  (def-stream-write les16-file-output-stream write-les16)
  (def-stream-write les32-file-output-stream write-les32))

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
		       :interactive-p t
		       :pathname "stdin"
		       :input-fd input-fd))
  (setq *standard-output*
	(make-instance 'ascii-file-output-stream
		       :interactive-p t
		       :pathname "stdout"
		       :output-fd output-fd))
  (setq *error-output*
	(make-instance 'ascii-file-output-stream
		       :interactive-p t
		       :pathname "stderr"
		       :output-fd error-fd))
  (setq *terminal-io*
	(make-two-way-stream *standard-input* *standard-output*))
  (setq *debug-io* (make-synonym-stream '*terminal-io*)
	*query-io* (make-synonym-stream '*terminal-io*)
	*trace-output* *standard-output*
	*null-output* (make-broadcast-stream))
  *terminal-io*)
(initilize-stream-variables 0 1 2)

