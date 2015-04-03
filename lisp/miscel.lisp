
#-machine-compile
(defconstant INTERNAL-TIME-UNITS-PER-SECOND
  (int-integer (time-units-per-second)))
;; Interpreter can't handle required type checks yet, and besides,
;; it's better to have the actual value set by interface.c, because it
;; must vary from one platform to the next.
#+machine-compile
(progn
  (setf (global-declaration 'internal-time-units-per-second
			    'global-variable
			    'special)
	t)
  (setf (global-declaration 'internal-time-units-per-second
			    'global-variable
			    'constant)
	t))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 25.1 THE COMPILER                                            ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; See evaluation.lisp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 25.3 DEBUGGING TOOLS                                         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; STEP is in evaluation.lisp

#| ISSUES:
- What does it mean to trace a method?
- How is a method specified?
- Should the "arguments" to a method be the arguments to the generic
  function or the arguments to the method-function (which are
  different if according to the MOP)?
- Should one be able to specify method qualifiers in the trace?
|#

(defparameter *trace-level* 0)
(defparameter *trace-data* nil)
(defun list-traced () (mapcar #'car *trace-data*))
(defun trace1 (function-names)
  (dolist (function-name function-names (list-traced))
    (unless (assoc function-name *trace-data*)
      (let ((original (fdefinition function-name))
	    ;; Eclipse does not create new bindings for elements in
	    ;; dolist.  Since we want to close over each individual
	    ;; function-name, we need to create a binding.
	    (function-name function-name))
	(setq *trace-data*
	      (acons function-name original *trace-data*))
	(setf (fdefinition function-name)
	      #'(lambda (&rest args)
		  (let* ((*trace-level* (1+ *trace-level*))
			 (indent (* *trace-level* 2)))
		    (format *trace-output*
			    "~&~v@t~d Call ~s, Args:~{ ~s~}~%"
			    indent *trace-level*
			    function-name args)
		    (let ((values (multiple-value-list
				   (apply original args))))
		      (format *trace-output*
			      "~&~v@t~d  ~s  Returned:~{ ~s~}~%"
			      indent *trace-level*
			      function-name values)
		      (values-list values)))))))))


(defun untrace1 (function-names)
  (flet ((untrace0 (pair) (setf (fdefinition (car pair)) (cdr pair))))
    (if function-names
	(dolist (function-name function-names (list-traced))
	  (let ((pair (find function-name *trace-data* :key #'car)))
	    (when pair (untrace0 pair)
		  (setq *trace-data*
			(delete function-name *trace-data* :key #'car)))))
      (dolist (pair *trace-data* (setq *trace-data* nil))
	(untrace0 pair)))))
    

(defmacro TRACE (&rest function-names)
  `(trace1 ',function-names))
    
(defmacro UNTRACE (&rest function-names)
  `(untrace1 ',function-names))

;;; Should also include (room nil) data!!!
;; We "cheat" here and in GET-UNIVERSAL-TIME by passing around longs as lisp data.
(defun END-TIME (real-start run-start heap-start)
  (let ((real-end (real-time nil))
	(run-end (run-time))
	(heap-end (room nil)))
    (format *trace-output* "~%~9,2f seconds of real time."
	    (/ (- (int-integer (object-word real-end))
		  (int-integer (object-word real-start)))
	       (symbol-value 'internal-time-units-per-second)))
    (format *trace-output* "~%~10,3f seconds of run time."
	    (/ (- (int-integer (object-word run-end))
		  (int-integer (object-word run-start)))
	       (symbol-value 'internal-time-units-per-second)))
    (format *trace-output* "~%The heap grew by ~d bytes."
	    (- heap-end heap-start))
    (terpri *trace-output*)))
	      

(defmacro TIME (form)
  `(let ((heap-start (room nil))
	 (real-start (real-time nil))
	 (run-start (run-time)))
     (multiple-value-prog1 ,form (end-time real-start run-start heap-start))))



;;; DESCRIBE
(defun DESCRIBE (x &optional stream)
  (describe-object x (output-stream stream))
  (values))
  
(defun INSPECT (object)
  (with-simple-restart (abort "Quit inspector.")
    (inspect-object object *debug-io*)))

;;; Needs gc !!!
(defun ROOM (&optional (amount :default))
  (if amount
      (multiple-value-bind (bytes since mem limit) (gc-data)
	(format *standard-output* "~:d bytes allocated.~%" bytes)
	(when (eq amount t)
	  (format *standard-output*
		  "~:d bytes allocated since the last GC.~%" since)
	  (format *standard-output*
		  "Total memory usage is ~:d out of ~:d bytes available.~%"
		  mem limit))
	(values))
      (gc-data)))

;;; Should really check to see if emacs is already running!!!
;;; IWBNI we used `emacsclient' (in the emacs distribution).
(defun ED (&optional x)
  (declare (ignore x))
  (warn "No Lisp-resident editor."))

;;; Dribbble is defined in debug.lisp

(defun APROPOS (string &optional package)
  (setq string (string string))
  (if package
      (do-symbols (symbol package)
	(when (search string (string symbol)) (describe symbol)))
    (do-all-symbols (symbol)
      (when (search string (string symbol)) (describe symbol))))
  (values))

(defun APROPOS-LIST (string &optional package &aux list)
  (setq string (string string))
  (if package
      (do-symbols (symbol package)
	(when (search string (string symbol)) (pushnew symbol list)))
    (do-all-symbols (symbol)
      (when (search string (string symbol)) (pushnew symbol list))))
  list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 25.4 ENVIRONMENT INQUIRIES                                   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 25.4.1 TIME FUNCTIONS	 
(defun GET-INTERNAL-RUN-TIME () (int-integer (object-word (run-time))))
(defun GET-INTERNAL-REAL-TIME () (int-integer (object-word (real-time nil))))
  
(defconstant unix-to-universal-time 2208988800)
(defun GET-UNIVERSAL-TIME ()
  (+ (unix-time) unix-to-universal-time))

;;; From CMUCL
;;; The offset between our time base and the Perq one is 2145 weeks and
;;; five days.
(defconstant minutes-per-day (* 24 60))
(defconstant seconds-offset 432000)
(defconstant seconds-in-week (* 60 60 24 7))
(defconstant weeks-offset 2145)
(defconstant november-17-1858 678882)
(defconstant weekday-november-17-1858 2)
(defconstant quarter-days-per-year (1+ (* 365 4)))
(defconstant quarter-days-per-century 146097)
(defun decode-universal-time (universal-time &optional time-zone)
  (multiple-value-bind (weeks secs)
      (truncate (+ universal-time seconds-offset)
		seconds-in-week)
    (let* ((weeks (+ weeks weeks-offset))
	   (second NIL)
	   (minute NIL)
	   (hour NIL)
	   (date NIL)
	   (month NIL)
	   (year NIL)
	   (day NIL)
	   (daylightp NIL)
	   (zone (if time-zone
		     (* time-zone 60)
		     (multiple-value-bind (minwest dst)
			 (get-timezone (- universal-time
					  unix-to-universal-time))
		       (setf daylightp dst)
		       minwest))))
      (declare (fixnum zone))
      (multiple-value-bind (t1 seconds) (truncate secs 60)
	(setq second seconds)
	(setq t1 (- t1 zone))
	(let* ((tday (if (< t1 0)
			 (1- (truncate (1+ t1) minutes-per-day))
			 (truncate t1 minutes-per-day))))
	  (multiple-value-setq (hour minute)
	    (truncate (- t1 (* tday minutes-per-day)) 60))
	  (let* ((t2 (1- (* (+ (* weeks 7) tday november-17-1858) 4)))
		 (tcent (truncate t2 quarter-days-per-century)))
	    (setq t2 (mod t2 quarter-days-per-century))
	    (setq t2 (+ (- t2 (mod t2 4)) 3))
	    (setq year (+ (* tcent 100) (truncate t2 quarter-days-per-year)))
	    (let ((days-since-mar0 (1+ (truncate (mod t2 quarter-days-per-year)
						 4))))
	      (setq day (mod (+ tday weekday-november-17-1858) 7))
	      (let ((t3 (+ (* days-since-mar0 5) 456)))
		(cond ((>= t3 1989)
		       (setq t3 (- t3 1836))
		       (setq year (1+ year))))
		(multiple-value-setq (month t3) (truncate t3 153))
		(setq date (1+ (truncate t3 5))))))))
      (values second minute hour date month year day
	      daylightp
	      (if daylightp
		  (1+ (/ zone 60))
		  (/ zone 60))))))
		  
  
(defun GET-DECODED-TIME () (decode-universal-time (get-universal-time)))

(defconstant seconds-per-hour (* 60 60))
(defconstant seconds-per-day (* 60 60 24))

;;; Broken host file compiler can't handle our vectors as literals.
#+lisp-host
(defvar days-in-year-by-month
  (let ((sum -1)
	(v (make-vector 't 13)))
    (dotimes (i 12 v)
      (setf (svref v (1+ i)) sum)
      (incf sum (svref;;J  F  Mr Ap My Jn Jl Au S  O  N  D
		 #(31 28 31 30 31 30 31 31 30 31 30 31)
		 i)))))
#-lisp-host
(defconstant days-in-year-by-month #.days-in-year-by-month)

(defun canonical-year (year)
  (if (>= year 100) year
    (let* ((current-year (nth-value 5 (get-decoded-time)))
	   (guess (+ year (* (truncate (- current-year 50) 100) 100))))
      (if (> (- current-year guess) 50)
	  (+ guess 100)
	guess))))

(defun leap-days-since-1900 (years-since-1900 days-in-year)
  (multiple-value-bind (year-by-4 rem-by-4)
      (truncate years-since-1900 4)
    (multiple-value-bind (year-by-100 rem-by-100)
	(truncate years-since-1900 100)
      (multiple-value-bind (year-by-400 rem-by-400)
	  (truncate (+ 1900 years-since-1900) 400)
	(+ year-by-4
	   (- year-by-100)
	   year-by-400 -4 ;;4 years in (0,1900) were divisible by 400
	   (if (and (zerop rem-by-4)
		    (or (zerop rem-by-400)
			(not (zerop rem-by-100)))
		    (< days-in-year 59))
	       -1 0))))))

(defun ENCODE-UNIVERSAL-TIME (second minute hour date month year
				     &optional (time-zone 0 time-zone-p))
  (let* ((years-since-1900 (- (canonical-year year) 1900))
	 (days-in-year (+ date (svref days-in-year-by-month month)))
	 (raw (+ (* seconds-per-day
		    (+ (* 365 years-since-1900)
		       (leap-days-since-1900
			years-since-1900 (- days-in-year
					    (if (and (= month 2) (= date 29))
						1 0)))
		       days-in-year))
		 (* (+ hour time-zone) seconds-per-hour)
		 (* minute 60)
		 second)))
    (if time-zone-p
	raw
	(+ raw (* (get-timezone (- raw eclipse::unix-to-universal-time))
		  60)))))

(defun SLEEP (seconds)
  (let ((s (ceiling (* seconds 1000))))
    (etypecase s
      (fixnum (millisleep (fixnum-int s)))))
  nil)


;;; 25.4.2 OTHER ENVIRONMENT INQUIRIES
(defun LISP-IMPLEMENTATION-TYPE () "Eclipse Common Lisp")
(defun LISP-IMPLEMENTATION-VERSION () (charp-simple-base-string (eclipse-version)))

(defun USER-ID () (uname (character-int #\u)))

(defvar *machine-version* nil)
(defun MACHINE-VERSION () (or *machine-version* (uname (character-int #\z))))
(defun MACHINE-INSTANCE () (uname (character-int #\n)))
(defun MACHINE-TYPE () (uname (character-int #\m)))
(defun SOFTWARE-TYPE () (uname (character-int #\s)))
;;; IWBNI this included "uname -v", but the result is weird on some
;;; systems (LINUX).
(defun SOFTWARE-VERSION () (uname (character-int #\r)))

;;; IWBNI we called domainname or dname to initialize the site.
(defvar *short-site-name* nil)
(defvar *long-site-name* nil)
(defun SHORT-SITE-NAME () *short-site-name*)
(defun LONG-SITE-NAME () (or *long-site-name* (short-site-name)))			      

;;; ID-STRING is for making id strings such as "2.3 (release)" without
;;; using runtime format.  Major, minor and qualifier are princ'd. 
;;; - Qualifier can be nil (default), in which case it isn't printed.
;;; - Decimalp (default t) prints a decimal point between major and minor.
;;; - Spacep prints a space before the qualifier (if any), and defaults
;;;   to true when decimalp is true.

(defun id-string (major minor &optional
			qualifier (decimalp t) (spacep decimalp))
  (format nil (formatter "~a~:[~;.~]~a~:[~; ~]~@[~a~]")
	  major decimalp minor (and spacep qualifier) qualifier))


(setq *FEATURES*
      (let ((machine (string-upcase (machine-type)))
	    (software (string-upcase (software-type))))
	`(:eclipse :elwood :common-lisp :ansi-cl :x3j13
		   :ieee-floating-point
		   ,(make-keyword
		     (id-string "ECLIPSE-"
				(lisp-implementation-version) nil nil))
		   ,(make-keyword machine)
		   ,@(cond ((search "SUN" machine) '(:SPARC))
			   ((search "9000" machine) '(:PA-RISC))
			   ((or (search "I386" machine)
				(search "I486" machine)
				(search "I586" machine)
				(search "I686" machine)
				(search "X86" machine))
			    '(:PC :INTEL :X86)))
		   
		   ,(make-keyword software)
		   ,@(cond ((search "SUN" software) '(:SUN :UNIX))
			   ((search "HP" software) '(:HP :UNIX :UCS-4-IO))
			   ((search "LINUX" software) '(:UNIX))
			   ((search "WINDOWS" software)
			    `(:WINDOWS :WIN32
				       ,@(when (search "95" software) '(:WIN95))
				       ,@(when (search "NT" software) '(:NT))))))))
				  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 25.5 IDENTITY FUNCTION                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Identity is needed early in loading, so it is defined in kernel.

(defun CONSTANTLY (object)
  #'(lambda (&rest args)
      (declare (ignore args))
      object))
