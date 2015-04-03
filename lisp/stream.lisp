(defparameter *default-buffer-size* 1024)
(defparameter *default-element-type* 'character)

(defun ENSURE-OUTPUT-STREAM (stream)
  (if (output-stream-p stream) stream
      (error 'type-error :expected-type 'output-stream :datum stream)))
(defun ENSURE-INPUT-STREAM (stream)
  (if (input-stream-p stream) stream
      (error 'type-error :expected-type 'input-stream :datum stream)))
(defun ENSURE-OPEN-STREAM (stream)
  (or (open-stream-p stream)
      (error 'stream-error :stream stream :complaint "~s is not open.")))
(defun ENSURE-ELEMENT-TYPE-COMPATIBILITY (streams)
  (let ((types (delete-duplicates
		(sort (mapcar #'stream-element-type streams)
		      #'subtypep)
		:test #'subtypep)))
    (if (cdr types)
	(error
	 "The :element-types ~s are not compatible."
	 types)
      (car types))))

(defvar *original-stream* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; INPUT FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Make things more consistent and allows stream specializer to take
;;; precedence. 
(defun READ-SEQUENCE (sequence stream &key (start 0) end)
  (stream-read-sequence stream sequence start :start start :end end))

(defun input-stream (designator)
  (case designator
    ((nil) *standard-input*)
    ((t) *terminal-io*)
    (t designator)))

(defun eof-process (result stream eof-error-p eof-value recursive-p)
  (declare (ignore recursive-p))
  (if (eq result :eof)
      (if eof-error-p
	  (error 'end-of-file :stream stream) ;Pass on recursive-p?
	  eof-value)
      result))

(defun READ-CHAR (&optional input-stream (eof-error-p t) eof-value recursive-p)
  (let ((stream (input-stream input-stream)))
    (eof-process (stream-read-char stream) stream eof-error-p
		 eof-value recursive-p))) 

(defun READ-CHAR-NO-HANG (&optional input-stream (eof-error-p t) eof-value recursive-p)
  (let ((stream (input-stream input-stream)))
    (eof-process (stream-read-char-no-hang stream) stream eof-error-p
		 eof-value recursive-p)))

(defun UNREAD-CHAR (character &optional input-stream)
  (stream-unread-char (input-stream input-stream) character))

(defun PEEK-CHAR (&optional peek-type input-stream (eof-error-p t) eof-value recursive-p)
  (let ((stream (input-stream input-stream)))
    (if peek-type
	(loop for char = (stream-peek-char stream)
	      when (eq char :eof)
	      return (eof-process char stream eof-error-p eof-value recursive-p)
	      if (if (eq peek-type t)
		     (not (whitespace-char-p char))
		     (eq char peek-type))
	      return char
	      else do (stream-read-char stream))
	(eof-process (stream-peek-char stream) stream
		     eof-error-p eof-value recursive-p))))

(defun READ-LINE (&optional input-stream (eof-error-p t) eof-value
			    recursive-p)
  (let ((stream (input-stream input-stream)))
    (multiple-value-bind (string eofp) (stream-read-line stream)
      (values
       (eof-process (if (and eofp (zerop (length string)))
			:eof
			string)
		    stream eof-error-p eof-value recursive-p)
       eofp))))

(defun READ-BYTE (stream &optional (eof-error-p t) eof-value)
  (eof-process (stream-read-byte stream) stream eof-error-p eof-value nil))

	 
(defun LISTEN (&optional input-stream)
  (stream-listen (input-stream input-stream)))
(defun CLEAR-INPUT (&optional input-stream)
  (stream-clear-input (input-stream input-stream)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; OUTPUT FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Make things more consistent and allows stream specializer to take
;;; precedence. 
(defun WRITE-SEQUENCE (sequence stream &key (start 0) end)
  (stream-write-sequence stream sequence :start start :end end))


(defun output-stream (designator)
  (case designator
    ((nil) *standard-output*)
    ((t) *terminal-io*)
    (t designator)))

(defun WRITE-CHAR (character &optional output-stream)
  (stream-write-char (output-stream output-stream) character))

(defun WRITE-STRING (string &optional output-stream &key (start 0) end)
  (stream-write-string (output-stream output-stream) string start end))

(defun WRITE-LINE (string &optional output-stream &key (start 0) end)
  (let ((stream (output-stream output-stream)))
    (stream-write-string stream string start end)
    (stream-terpri stream))
  string)

(defun WRITE-BYTE (byte stream) (stream-write-byte stream byte))

(defun TERPRI (&optional output-stream)
  (stream-terpri (output-stream output-stream)))
(defun FRESH-LINE (&optional output-stream)
  (stream-fresh-line (output-stream output-stream)))
(defun FINISH-OUTPUT (&optional output-stream)
  (stream-finish-output (output-stream output-stream)))
(defun FORCE-OUTPUT (&optional output-stream)
  (stream-force-output (output-stream output-stream)))
(defun CLEAR-OUTPUT (&optional output-stream)
  (stream-clear-output (output-stream output-stream)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; RESOURCES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-buffer (element-type)
  (make-array *default-buffer-size*
	      :element-type element-type
	      :fill-pointer 0 :adjustable t))

;;; IWBNI these could make use of user-defined stream classes.
(defresource output-buffer (element-type string)
  :constructor
  (make-instance (find-type 'string-output-stream)
		 :element-type element-type
		 :output-buffer (or string (make-buffer element-type)))
  :matcher
  (and (null string)
       (eq element-type (array-element-type (output-buffer output-buffer))))
  :deinitializer
  (when string
    (setf (slot-value output-buffer 'output-buffer) (make-buffer element-type))))

(defresource input-buffer (string string-start string-end)
  :constructor
  (make-instance (find-type 'string-input-stream))
  :initializer
  (initialize-instance input-buffer
		       :element-type (array-element-type string)
		       :input-buffer string
		       :position string-start
		       :end (or string-end (length string)))
  :matcher t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NON-STREAMS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; TYPE-ERROR REQUIRED BY ANSI
(macrolet ((def-error (name &rest more-args)
	     `(defmethod ,name (stream ,@more-args) 
		(error 'type-error :datum stream :expected-type 'stream))))
  (def-error OPEN-STREAM-P)
  (def-error INPUT-STREAM-P)
  (def-error OUTPUT-STREAM-P)
  (def-error INTERACTIVE-STREAM-P)
  (def-error STREAM-ELEMENT-TYPE)
  (def-error FILE-LENGTH)
  (def-error stream-CLEAR-INPUT)
  (def-error stream-CLEAR-OUTPUT)
  (def-error stream-FINISH-OUTPUT)
  (def-error stream-FORCE-OUTPUT)
  (def-error stream-READ-BYTE)
  (def-error stream-WRITE-BYTE (integer t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; STREAM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass STREAM ()
  ((open :reader OPEN-STREAM-P :initform t)))

(macrolet ((def-pred (name class)
	     `(progn (defmethod ,name ((stream t)) nil)
		     (defmethod ,name ((stream ,class)) t))))
  (def-pred STREAMP stream))

;;; Should we change-class to closed-stream?!
(defmethod CLOSE ((stream STREAM) &key abort)
  (declare (ignore abort))
  (with-slots (open) stream
    (when open
      (setf open nil) t)))

(defclass INPUT-STREAM (stream) ())
(defclass OUTPUT-STREAM (stream) ())
(defclass BIDIRECTIONAL-STREAM (input-stream output-stream) ())
(macrolet ((def-pred (name class)
	     `(progn (defmethod ,name ((stream STREAM)) nil)
		     (defmethod ,name ((stream ,class)) t))))
  (def-pred INPUT-STREAM-P input-stream)
  (def-pred OUTPUT-STREAM-P output-stream))

;;; We assume that concrete stream classes will define appropriate
;;; specialized methods.  If the stream and sequence don't match,
;;; these will provide the correct error condition.
(macrolet ((def-error (name class)
	     `(defmethod ,name ((stream ,class) (sequence SEQUENCE)
				&key (start 0) end)
		(declare (ignore start end))
		(let ((element-type (stream-element-type stream)))
		  (error 'type-error :datum sequence
			 :expected-type `(or (cons ,element-type)
					     (vector ,element-type)))))))
  (def-error stream-READ-SEQUENCE input-stream)
  (def-error stream-WRITE-SEQUENCE output-stream))


;;; "a stream whose source or sink is a Lisp object"
(defclass constructed-stream (stream) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FUNDAMENTAL STREAMS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; An actual, non-composite, stream.
(defclass FUNDAMENTAL-STREAM (stream)
  ((element-type :reader STREAM-ELEMENT-TYPE
		 :initarg :element-type
		 :initform *default-element-type*)
   (interactive-p :initform nil :reader INTERACTIVE-STREAM-P
		  :initarg :interactive-p)))

(defclass FUNDAMENTAL-INPUT-STREAM (FUNDAMENTAL-STREAM input-stream) ())
(defclass FUNDAMENTAL-OUTPUT-STREAM (FUNDAMENTAL-STREAM output-stream) ())

(defclass FUNDAMENTAL-CHARACTER-STREAM (fundamental-stream) ())
(defclass FUNDAMENTAL-CHARACTER-INPUT-STREAM (fundamental-character-stream
					      fundamental-input-stream) ())
(defclass FUNDAMENTAL-CHARACTER-OUTPUT-STREAM (fundamental-character-stream
					       fundamental-output-stream) ())

(defclass FUNDAMENTAL-BINARY-STREAM (fundamental-stream) ())
(defclass FUNDAMENTAL-BINARY-INPUT-STREAM (fundamental-binary-stream
					   fundamental-input-stream) ())
(defclass FUNDAMENTAL-BINARY-OUTPUT-STREAM (fundamental-binary-stream
					    fundamental-output-stream) ())


(defmethod stream-CLEAR-INPUT ((stream FUNDAMENTAL-INPUT-STREAM))
  nil)
(defmethod stream-CLEAR-OUTPUT ((stream FUNDAMENTAL-OUTPUT-STREAM))
  nil)
(defmethod stream-FINISH-OUTPUT ((stream FUNDAMENTAL-OUTPUT-STREAM))
  nil)
(defmethod stream-FORCE-OUTPUT ((stream FUNDAMENTAL-OUTPUT-STREAM))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; INPUT
(defmethod stream-READ-CHAR-NO-HANG ((stream FUNDAMENTAL-CHARACTER-INPUT-STREAM))
  (stream-read-char stream))

(defmethod stream-PEEK-CHAR ((stream FUNDAMENTAL-CHARACTER-INPUT-STREAM))
  (let ((char (stream-read-char stream)))
    (unless (eq char :eof) (stream-unread-char stream char))
    char))

(defmethod stream-LISTEN ((stream FUNDAMENTAL-CHARACTER-INPUT-STREAM))
  (let ((char (stream-read-char-no-hang stream)))
    (when (and char (not (eq char :eof)))
      (stream-unread-char stream char)
      t)))

;;; Maybe this should use a per-stream buffer accessed through the
;;; stream-input-stream instance variable.  This, in turn, would be
;;; initialized from a resource based  on the stream-element-type.
(let ((buffer (make-buffer 'character)))
  (defmethod stream-READ-LINE ((stream FUNDAMENTAL-CHARACTER-INPUT-STREAM))
    (loop for char = (stream-read-char stream)
	  when (or (eq char :eof) (char= char #\newline))
	  return (values (copy-seq buffer) (eq char :eof))
	  do (vector-push-extend char buffer))))
  

(defmethod stream-READ-SEQUENCE ((stream FUNDAMENTAL-CHARACTER-INPUT-STREAM)
				 (sequence VECTOR)
				 &key (start 0) end)
  (loop for position from start below (or end (length sequence))
	and char = (stream-read-char stream)
	if (eq char :eof) do (loop-finish)
	else do (setf (elt sequence position) char)
	finally (return position)))
(defmethod stream-READ-SEQUENCE ((stream FUNDAMENTAL-CHARACTER-INPUT-STREAM)
				 (sequence LIST)
				 &key (start 0) end)
  (loop for position from start
	and char = (stream-read-char stream)
	and sub on sequence
	do (list-check-end-noinc position end position)
	if (eq char :eof) do (loop-finish)
	else do (rplaca sub char)
	finally (return position)))

(defmethod stream-READ-SEQUENCE ((stream FUNDAMENTAL-BINARY-INPUT-STREAM)
				 (sequence VECTOR)
				 &key (start 0) end)
  (loop for position from start below (or end (length sequence))
	and char = (stream-read-byte stream)
	if (eq char :eof) do (loop-finish)
	else do (setf (elt sequence position) char)
	finally (return position)))
(defmethod stream-READ-SEQUENCE ((stream FUNDAMENTAL-BINARY-INPUT-STREAM)
				 (sequence LIST)
				 &key (start 0) end)
  (loop for position from start
	and char = (stream-read-byte stream)
	and sub on sequence
	do (list-check-end-noinc position end position)
	if (eq char :eof) do (loop-finish)
	else do (rplaca sub char)
	finally (return position)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; OUTPUT
(defmethod STREAM-START-LINE-P ((stream FUNDAMENTAL-CHARACTER-OUTPUT-STREAM))
  (let ((col (stream-line-column stream)))
    (and col (zerop col))))

(defmethod (SETF STREAM-START-LINE-P) (start-line-p (stream stream))
  start-line-p)

(defmethod stream-TERPRI ((stream FUNDAMENTAL-CHARACTER-OUTPUT-STREAM))
  (stream-write-char stream #\newline)
  nil)

(defmethod stream-FRESH-LINE ((stream FUNDAMENTAL-CHARACTER-OUTPUT-STREAM))
  (unless (stream-start-line-p stream)
    (stream-terpri stream)
    t))


(defmethod stream-ADVANCE-TO-COLUMN ((stream FUNDAMENTAL-CHARACTER-OUTPUT-STREAM)
				     (column INTEGER))
  (let ((start (stream-line-column stream)))
    (when (and start (> start column))
      (stream-terpri stream)
      (setq start 0))
    (loop for i from (or start 0) below column
	  do (stream-write-char stream #\space))
    (not (null start))))


(defmethod STREAM-WRITE-STRING ((stream FUNDAMENTAL-CHARACTER-OUTPUT-STREAM)
				(string STRING) &optional
				(start 0) end)
  (loop for position from start below (or end (length string))
	do (stream-write-char stream (char string position)))
  string)

(defmethod STREAM-WRITE-SEQUENCE ((stream FUNDAMENTAL-CHARACTER-OUTPUT-STREAM)
				  (sequence string)
				  &key (start 0) end)
  (stream-write-string stream sequence start end))
(defmethod STREAM-WRITE-SEQUENCE ((stream FUNDAMENTAL-CHARACTER-OUTPUT-STREAM)
				  (sequence VECTOR)
				  &key (start 0) end)
  (loop for position from start below (or end (length sequence))
	do (stream-write-char stream (elt sequence position)))
  sequence)
(defmethod STREAM-WRITE-SEQUENCE ((stream FUNDAMENTAL-CHARACTER-OUTPUT-STREAM)
				  (sequence LIST)
				  &key (start 0) end)
  (loop for position from start
	and elt in sequence
	do (list-check-end-noinc position end sequence)
	do (stream-write-char stream elt))
  sequence)

(defmethod STREAM-WRITE-SEQUENCE ((stream FUNDAMENTAL-BINARY-OUTPUT-STREAM)
				  (sequence VECTOR)
				  &key (start 0) end)
  (loop for position from start below (or end (length sequence))
	do (stream-write-byte stream (elt sequence position)))
  sequence)
(defmethod STREAM-WRITE-SEQUENCE ((stream FUNDAMENTAL-BINARY-OUTPUT-STREAM)
				  (sequence LIST)
				  &key (start 0) end)
  (loop for position from start
	and elt in sequence
	do (list-check-end-noinc position end sequence)
	do (stream-write-byte stream elt))
  sequence)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FILLABLE INPUT STREAMS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Expects stream-fill-input to be defined.
(defclass fillable-input-stream (input-stream) ())

(defmethod stream-READ-SEQUENCE ((stream FILLABLE-INPUT-STREAM) sequence
				 &key start end)
  (declare (ignore start))
  (let ((position (call-next-method)))
    (if (and (< position (or end (length sequence)))
	     (stream-fill-input stream))
	(stream-read-sequence stream sequence
			      :start position :end end)
	position)))

(defclass fillable-character-input-stream (fillable-input-stream) ())

(defmethod stream-READ-LINE ((stream FILLABLE-CHARACTER-INPUT-STREAM))
  (multiple-value-bind (string eofp) (call-next-method)
    (if (and eofp (stream-fill-input stream))
	(multiple-value-bind (string2 eofp) (stream-read-line stream)
	  (values (concatenate
		   (if (or (eq 'character (array-element-type string))
			   (eq 'character (array-element-type string2)))
		       'string 'base-string)
		   string string2) eofp))
	(values string eofp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BUFFERED STREAMS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Where input comes from a buffer or output goes to a buffer.
;;; An input-buffer may also be filled by an OS entity.
;;; An output-buffer may also get dumped to an OS entity.

(defclass buffered-input-stream (input-stream)
  ((original-input-buffer :initarg :input-buffer :reader input-buffer)
   (offset :initform 0)
   (input-buffer)
   (position :initform 0 :initarg :position)
   (end :initarg :end)))

;;; The initargs :INPUT-BUFFER, :POSITION, and :END, and the readers
;;; INPUT-BUFFER, INPUT-BUFFER-POSITION and INPUT-BUFFER-END all refer
;;; to a (possibly complex) vector given at stream instantiation time.
;;; Internally, buffered-input-stream maintains the slots named
;;; INPUT-BUFFER, POSITION, and END as refering to the simple-vector
;;; that underlies the (possibly displaced) original vector.  These
;;; internal slots are set by and INITIALIZE-INSTANCE :AFTER method
;;; and at no other time.
(defmethod initialize-instance :after ((stream BUFFERED-INPUT-STREAM)
				       &rest ignore)
  (declare (ignore ignore))
  (with-slots (input-buffer original-input-buffer offset position end)
      stream
    (when (slot-boundp stream 'original-input-buffer)
      (multiple-value-bind (vector vector-offset)
	  (get-simple-vector original-input-buffer 0)
	(setf offset vector-offset
	      input-buffer vector)
	(incf position vector-offset)
	(incf end vector-offset)))))

(defmethod input-buffer-position ((stream buffered-input-stream))
  (with-slots (position offset) stream (- position offset)))
(defmethod input-buffer-end ((stream buffered-input-stream))
  (with-slots (end offset) stream (- end offset)))


;; Output buffer must have a fill-pointer and be adjustable.
(defclass buffered-output-stream (output-stream)
  ((output-buffer :initarg :output-buffer :reader output-buffer)))


(defmethod stream-READ-SEQUENCE ((stream BUFFERED-INPUT-STREAM)
				 (sequence SEQUENCE)
				 &key (start 0) end)
  (with-slots (input-buffer position (term end)) stream
    (let* ((end (or end (length sequence)))
	   (start2 position)
	   (end2 term)
	   (length (max 0 (min (- end start) (- end2 start2)))))
      (setf position (setq end2 (+ start2 length)))
      (replace sequence input-buffer :start1 start :end1 end
	       :start2 start2 :end2 end2)
      (+ start length))))

;;; Returns the position in the buffer in which new data should be written to.
;;; This method is responsible for making sure that the "length" of
;;; buffer is big enough (eg. by adjusting the fill-pointer).
(defmethod check-buffer-size ((stream BUFFERED-OUTPUT-STREAM) n-additional-items)
  (with-slots (output-buffer) stream
    (let* ((buf output-buffer)
	   (pos (fill-pointer buf))
	   (needed (+ pos n-additional-items))
	   (current (array-dimension buf 0)))
      (when (> needed current)
	(setf output-buffer
	      (adjust-array buf
			    (extension current (- needed current)
				       *default-buffer-size* 2048))))
      ;; Replace won't go beyond fill-pointer
      (setf (fill-pointer buf) needed)
      pos)))

(defmethod stream-WRITE-SEQUENCE ((stream BUFFERED-OUTPUT-STREAM)
				  (sequence SEQUENCE)
				  &key (start 0) end)
  (let* ((end2 (or end (length sequence)))
	 (start1 (check-buffer-size stream (- end2 start))))
    (replace (output-buffer stream) sequence :start1 start1
	     :start2 start :end2 end2))
  sequence)

(defmethod stream-CLEAR-OUTPUT ((stream BUFFERED-OUTPUT-STREAM))
  (setf (fill-pointer (output-buffer stream)) 0)
  nil)

(defmethod stream-CLEAR-INPUT ((stream BUFFERED-INPUT-STREAM))
  (with-slots (position end) stream (setf position end))
  nil)


(defclass buffered-character-input-stream (buffered-input-stream
					   fundamental-character-input-stream)
  ())
(defclass buffered-character-output-stream (buffered-output-stream
					    fundamental-character-output-stream)
  ((line-length :initform nil :initarg :line-length
		:reader stream-line-length)))

(defclass buffered-binary-input-stream (buffered-input-stream
					fundamental-binary-input-stream)
  ())
(defclass buffered-binary-output-stream (buffered-output-stream
					 fundamental-binary-output-stream)
  ())

(defmethod stream-UNREAD-CHAR ((stream BUFFERED-CHARACTER-INPUT-STREAM)
			       (character character))
  (with-slots (position) stream (decf position))
  nil)

(defmethod stream-READ-CHAR ((stream BUFFERED-CHARACTER-INPUT-STREAM))
  (with-slots (input-buffer position end) stream
    (let ((pos position))
      (cond ((>= pos end) :eof)
	    (t (setf position (1+ pos))
	       (char input-buffer pos))))))

(defmethod stream-PEEK-CHAR ((stream BUFFERED-CHARACTER-INPUT-STREAM))
  (with-slots (input-buffer position end) stream
    (let ((pos position))
      (cond ((>= pos end) :eof)
	    (t (char input-buffer pos))))))

(defmethod stream-READ-LINE ((stream BUFFERED-CHARACTER-INPUT-STREAM))
  (with-slots (input-buffer position end) stream
    (let* ((buf input-buffer)
	   (start position)
	   (end end)
	   (lfp (position #\newline buf :start start :end end)))
      (setf position (if lfp (1+ lfp) end))
      (values (subseq buf start (or lfp end))
	      (null lfp)))))


(defmethod stream-WRITE-CHAR ((stream BUFFERED-CHARACTER-OUTPUT-STREAM)
			      (character CHARACTER))
  (vector-push-extend character (output-buffer stream))
  character)

(defmethod stream-LINE-COLUMN ((stream BUFFERED-CHARACTER-OUTPUT-STREAM))
  (let* ((string (output-buffer stream))
	 (pos (fill-pointer string))
	 (start (position #\newline string :end pos :from-end t)))
    (if start
	(- pos start 1)
	pos)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; STRING STREAMS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass STRING-STREAM (fundamental-character-stream constructed-stream)
  ())
	   
(defclass string-input-stream (buffered-character-input-stream
			       string-stream) 
  ())
(defclass string-output-stream (buffered-character-output-stream
				string-stream)
  ())


(defmethod stream-LISTEN ((stream STRING-INPUT-STREAM))
  (with-slots (end position) stream
    (< position end)))

(defmethod stream-START-LINE-P ((stream STRING-OUTPUT-STREAM))
  (let* ((string (output-buffer stream))
	 (pos (fill-pointer string)))
    (or (zerop pos)
	(char= #\newline (char string (1- pos))))))


;;; IWBNI these could make use of user-defined stream classes.
(defun MAKE-STRING-INPUT-STREAM (string &optional (start 0) end)
  (make-instance (find-type 'string-input-stream)
		 :element-type (array-element-type string)
		 :input-buffer string
		 :position start
		 :end (if end
			  (min end (length string))
			  (length string))))

(defun MAKE-STRING-OUTPUT-STREAM (&key (element-type *default-element-type*)
				       line-length)
  (make-instance (find-type 'string-output-stream)
		 :element-type element-type
		 :output-buffer (make-buffer element-type)
		 :line-length line-length))

(defun GET-OUTPUT-STREAM-STRING (stream)
  (let* ((buf (output-buffer stream))
	 (result (copy-seq buf)))
    (setf (fill-pointer buf) 0)
    result))

