;;; Temporary stuff

#-no-debugging
(defun cm (name &rest args)
  (compute-applicable-methods (fdefinition name) args))

#-no-debugging
(defun cmc (name &rest classes)
  (compute-applicable-methods-using-classes
   (fdefinition name)
   (loop for class in classes collect (canonicalize-class class t))))

#-no-debugging
(defun delete-method (name qualifiers &rest classes)
  (let* ((gf (fdefinition name))
	 (method (find-method gf qualifiers
			     (mapcar #'canonicalize-class classes))))
    (cl:print method)
    (remove-method gf method)))

#-machine-compile
(progn

(defun presentp (x) x)

(deftype cl:string () 'string)

(defun enclose (lambda-form &optional env name)
  (declare (ignore env name))  
  (cl:coerce lambda-form 'cl:function))
  

(defmethod stream-write-char ((stream unknown-object) character)
  (cl:write-char character stream))
(defmethod stream-write-string ((stream unknown-object) string
				&optional (start 0) (end (length string)))
  (stream-write-sequence stream string :start start :end end))
(defmethod stream-write-sequence ((stream unknown-object) sequence
				  &key (start 0) end)
  (loop for i from start below (or end (length sequence))
	for elt = (elt sequence i)
	do (cl:write-char elt stream))
  sequence)
(defmethod stream-terpri ((stream unknown-object))
  (cl:terpri stream))
(defmethod stream-fresh-line ((stream unknown-object))
  (cl:fresh-line stream))
(defmethod stream-finish-output ((stream unknown-object))
  (cl:finish-output stream))
(defmethod stream-force-output ((stream unknown-object))
  (cl:force-output stream))
(defmethod stream-clear-output ((stream unknown-object))
  (cl:clear-output stream))
(defmethod stream-start-line-p ((stream unknown-object)) nil)
(defmethod stream-line-length ((stream unknown-object)) nil)
(defmethod stream-line-column ((stream unknown-object)) nil)
(defmethod stream-advance-to-column ((stream unknown-object) (column t)) nil)

(defmethod stream-element-type ((stream unknown-object)) 'base-char)

(defmethod stream-READ-CHAR ((stream unknown-object))
  (cl:read-char stream nil :eof))
(defmethod stream-UNREAD-CHAR ((stream unknown-object) character)
  (cl:unread-char character stream))
(defmethod stream-READ-CHAR-NO-HANG ((stream unknown-object))
  (cl:read-char stream nil :eof))
(defmethod stream-PEEK-CHAR ((stream unknown-object))
  (cl:peek-char nil stream nil :eof))
(defmethod stream-READ-LINE ((stream unknown-object))
  (cl:read-line stream nil :eof))
(defmethod stream-LISTEN ((stream unknown-object))
  (cl:listen stream))
(defmethod stream-CLEAR-INPUT ((stream unknown-object))
  (cl:clear-input stream))
(defmethod maybe-force-output ((stream unknown-object))
  (cl:force-output stream))

(defparameter *forwarding-streams* (cl:make-hash-table :test 'cl:eq))
(defmethod stream-pprint ((stream unknown-object) object printer &rest args)
  (apply #'stream-pprint (or (cl:gethash stream *forwarding-streams*)
			   (cl:setf (cl:gethash stream *forwarding-streams*)
				    (make-instance 'encapsulating-stream
						   :stream stream)))
		 object printer args))

(defmethod stream-case-print ((stream unknown-object) mode printer)
  (stream-case-print (or (cl:gethash stream *forwarding-streams*)
			 (cl:setf (cl:gethash stream *forwarding-streams*)
				  (make-instance 'encapsulating-stream
						 :stream stream)))
		     mode printer))
		 
(defmethod stream-newline ((stream unknown-object) kind)
  (case kind
    (:unconditional (stream-terpri stream))
    (:fresh (stream-fresh-line stream))))
(defmethod stream-indent ((stream unknown-object) kind n)
  (declare (ignore kind n)) nil)
(defmethod stream-tab ((stream unknown-object) kind colnum colinc)
  (declare (ignore kind colnum colinc))
  nil)

(defmethod format ((destination unknown-object) (processor function) &rest args)
  (apply processor destination args)
  nil)

#+nn
(defmethod stream-READ-SEQUENCE ((stream unknown-object)
				 (sequence VECTOR)
				 &key (start 0) end)
  (loop for i form start to (or end (length sequence))
	while (setq (char sequence i) (cl:read-char stream nil nil)))
  sequence)

(setq *MACROEXPAND-HOOK* #'funcall)

(defun etpkg () (cl:in-package :et))
(defun epkg () (cl:in-package :eclipse))

(defun ccc (name &optional (pkg :eclipse)
		 (user::*eval* (not (eq pkg :eclipse))))
  (let ((*package* (or (cl:find-package pkg)
		       #.(cl:find-package :eclipse)))
	(cl:*features* user::*machine-compile-features*)
	(*features* user::*machine-compile-features*))
    (compile-file name
		  #+cmu :loader-name #+excl 'loader-name
		  (make-name "INIT-~:@(~a~)" (cl:pathname-name name))
		  #+cmu :output-format #+excl 'output-format :c
		  :output-file "c/"
		  :external-format :ascii)))

#+cmu
(defun ccq (&rest args)
  (apply #'ccc args)
  (user::quit))
)
