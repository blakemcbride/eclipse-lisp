(defun eclipse::eclipse-version ()
  (concatenate 'cl:string
	       (or (first (last (pathname-directory *default-pathname-defaults*)))
		   "1.0.3")
	       "-HOSTED"))

(defun eclipse::getenv (key)
  (let* ((val #+excl (sys:getenv key)
	      #+cmu (cdr (assoc (eclipse::make-keyword key)
				cl-user::*ENVIRONMENT-LIST*))))
    (eclipse::copy-seq val)))

(defun eclipse::time-units-per-second ()
  internal-time-units-per-second)
(defun eclipse::RUN-TIME ()
  (get-internal-run-time))
(defun eclipse::REAL-TIME (reset)
  (declare (ignore reset))
  (get-internal-real-time))

(defun eclipse::UNIX-TIME ()
  (declare (special eclipse::unix-to-universal-time))
  (- (get-universal-time) eclipse::unix-to-universal-time))
(defun eclipse::GET-TIMEZONE (unix-time)
  (declare (special eclipse::unix-to-universal-time))
  (multiple-value-bind (second minute hour date month year
			day savings-p zone)
      (decode-universal-time (+ unix-time eclipse::unix-to-universal-time))
    (declare (ignore second minute hour date month year day))
    (when savings-p (decf zone))
    (values (* 60 zone) savings-p)))
  
(defun eclipse::millisleep (milliseconds)
  (sleep (ceiling milliseconds 1000)))

(defun eclipse::UNAME (char)
  (case char
    (#.(char-code #\s) (software-type))
    (#.(char-code #\r) (let ((v (software-version)))
			 (unless (string= v "")
			   v)))
    (#.(char-code #\m)
       #+cmu
       (let ((version-line
	      (with-output-to-string (stream)
		(system::run-program "/bin/uname"
			     '("-m" )
			     :output stream
			     :pty nil
			     :error nil))))
	 (subseq version-line 0 (1- (length version-line))))
       #-cmu (machine-type))
    (#.(char-code #\n)
       #+cmu (unix:unix-gethostname)
       #+allegro-v4.3 "BOBS"
       #+(and excl (not allegro-v4.3)) (short-site-name))
    (#.(char-code #\v) nil)
    (#.(char-code #\h) (eclipse::getenv "HOME"))
    (#.(char-code #\u) (eclipse::getenv "USER"))
    ))

;;; Returns an integer.
;;;  SUN, LINUX: hostid, gethostid()
;;;  HP: uname -i, getpitimer(0)
;;;  DEC3100: getpitimer(NULL)
;;;  IBM: (parse-integer (shell "uname -m"))
(defun eclipse::MACHINE-ID ()
  #+cmu (unix:unix-gethostid)
  #+excl (values (parse-integer (machine-instance) :start 6 :radix 16)))

;;; We should really parse out the host data!
(defun eclipse::gc-data ()
  (values (cl:random most-positive-fixnum)
	  (cl:random most-positive-fixnum)))