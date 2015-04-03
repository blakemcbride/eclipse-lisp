(defun host-stream (fd)
  (case fd
    (0 cl:*standard-input*)
    (1 cl:*standard-output*)
    (2 cl:*error-output*)
    (t fd)))

(defparameter *stream-readers* (make-hash-table :test 'eq))
(defun stream-reader (fd)
  (gethash fd *stream-readers* #'read-char))

#|

(defun eclipse::block-stream (fd)
  (setf (gethash fd *stream-readers*) #'read-char))
(defun eclipse::unblock-stream (fd)
  (setf (gethash fd *stream-readers*) #'read-char-no-hang))
(eclipse::unblock-stream 0)
|#



(defun eclipse::read-ascii (fd buffer offset n)
  (loop with vector = (base-char-vector-contents buffer)
	;; i.e. simple-base-string-charp
	and host-fd = (host-stream fd)
  	for reader = (stream-reader fd) then #'read-char-no-hang
	for i from offset below (+ offset n)
	for char = (funcall reader host-fd nil)
	unless char do (loop-finish)
	do (setf (schar vector i) char)
	finally (return (- i offset))))

(defun eclipse::write-ascii (fd buffer offset n)
  (loop with vector = (base-char-vector-contents buffer)
	;;i.e. simple-base-string-charp
	and host-fd = (host-stream fd)
	for i from offset below (+ offset n)
	do (write-char (schar vector i) host-fd)
	finally (return (- i offset))))

;;; The same, but with different argument types
(defun eclipse::read-mb (fd buffer offset n aux)
  (declare (ignore aux))
  (loop with vector = (extended-char-vector-contents buffer) 
	and host-fd = (host-stream fd)
	for reader = (stream-reader fd) then #'read-char-no-hang
	for i from offset below (+ offset n)
	for char = (funcall reader host-fd nil)
	unless char do (loop-finish)
	do (setf (schar vector i) char)
	finally (return (- i offset))))

(defun eclipse::write-mb (fd buffer offset n)
  (loop with vector = (extended-char-vector-contents buffer)
	for i from offset below (+ offset n)
	do (write-char (schar vector i) (host-stream fd))
	finally (return (- i offset))))

(defun eclipse::drain (fd) (finish-output (host-stream fd)))
(defun eclipse::flush-input (fd) (clear-input (host-stream fd)))
(defun eclipse::flush-output (fd) (clear-output (host-stream fd)))
(defun eclipse::poll (fd) (listen (host-stream fd)))
(defun eclipse::interactivep (fd) (interactive-stream-p (host-stream fd)))

#|
(defun ec::getc (file)
  (let ((char (cl:read-char file nil :eof)))
    (if (eq char :eof) -1
	(cl:char-int char))))

(defun ec::ungetc (char file)
  (cl:unread-char (cl:code-char char) file))

(defun ec::putc (char file)
  (cl:write-char (cl:code-char char) file))
|#