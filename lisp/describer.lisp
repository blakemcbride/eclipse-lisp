;;; Describe-components is in eclipse-compile2.lisp

#+not-used ; See describe-components
(defun list-description (stream argument colon atsign)
  (declare (ignore colon atsign))
  (typecase argument
    (cons (format stream "~{~s~#[~; and ~:;, ~]~}" argument))
    (null (write-string "none" stream))
    (simple-base-string (write-string argument stream))
    (t (prin1 argument stream))))

(defun describe-header (object stream type)
  (format stream #"~s ~:_is a ~s ~:_at #x~x" object type
	  (int-integer (object-word object))))

(defun describe-trailer (stream)
  (write-char #\. stream)
  (terpri stream)
  nil)

(defgeneric describe-object (object stream))

(defmethod describe-object (object stream)
  (describe-components (object stream) nil))

(defgeneric inspect-object (object stream))
(defmethod inspect-object (object stream)
  (describe-object object stream))

;;; Like WRITE, but inserts conditional newlines after spaces.
;;; Used by report-condition.
(defun wrap (object &rest keys &key stream (right-margin *print-right-margin*)
		    &allow-other-keys)
  (let ((stream (output-stream stream)))
    (remf keys :stream)
    (let ((string (apply #'write-to-string object
			 :right-margin
			 (- (or right-margin *default-right-margin*)
			    (stream-line-column stream))
			 keys)))
      (loop for i below (length string)
	    for char = (schar string i)
	    do (write-char char stream)
	    when (and (eql char #\space)		      
		      (not (member (schar string (1+ i))
				   '(#\newline #\space))))
	    do (pprint-newline :fill stream)))))
