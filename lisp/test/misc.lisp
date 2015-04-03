;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 25.2 DOCUMENTATION                                           ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; documentation!!!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 25.3 DEBUGGING TOOLS                                         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TRACE, UNTRACE 
(defun factorial (n) (if (= n 1) 1 (* n (factorial (- n 1)))))
(deftest untrace-all (progn (untrace) (trace)) nil)
(deftest trace
  (let ((*trace-output* (make-broadcast-stream)))
    (untrace)
    (trace factorial)
    (let ((traced (trace)))
      (factorial 4)
      (untrace factorial)
      (values traced (trace))))
  (factorial) nil)
   
;;; STEP !!!

;;; TIME
(deftest time
  (let ((*trace-output* (make-broadcast-stream)))
    (time (factorial 10))) 3628800)
  
;;; DESCRIBE !!!
;;; INSPECT !!!

;;; ROOM
(deftest room
  (let ((*standard-output* (make-broadcast-stream)))
    (room) (room t) (room nil) (room :default) nil)
  nil)

;;; ED !!!

;;; DRIBBLE  Cannot be tested except interactively at top-level.

;;; APROPOS
#-lisp-host
(progn
  (deftest apropos (apropos "FOO" :common-lisp) )
  (deftest apropos-all
    (let ((*standard-output* (make-broadcast-stream)))
      (apropos "FOO")))

;;; APROPOS-LIST
  (deftest apropos-list-not-found (apropos-list "FOO" :common-lisp) nil)
  (deftest apropos-list-found
    (set-difference
     '(READ-DELIMITED-LIST MAPLIST VALUES-LIST LIST* MULTIPLE-VALUE-LIST
			   LISTEN DOLIST COPY-ALIST SYMBOL-PLIST LIST-ALL-PACKAGES
			   PACKAGE-USE-LIST LIST-LENGTH COPY-LIST PACKAGE-USED-BY-LIST
			   APROPOS-LIST LIST LISTP MAKE-LIST LAMBDA-LIST-KEYWORDS)
     (apropos-list "LIST" :common-lisp))
    nil)
  (deftest apropos-list-all (listp (apropos-list "FOO")) t)
  )
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 25.4 ENVIRONMENT INQUIRIES                                   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 25.4.1 TIME FUNCTIONS

;;; GET-DECODED-TIME
(deftest get-decoded-time
  (multiple-value-bind (second minute hour date month year day-of-week
			       dst time-zone)
      (get-decoded-time)
    (declare (ignore dst))
    (and (typep second '(integer 0 59))
	 (typep minute '(integer 0 59))
	 (typep hour '(integer 0 23))
	 (typep date '(integer 0 31))
	 (typep month '(integer 1 12))
	 (typep year '(integer 1900))
	 (typep day-of-week '(integer 0 6))
	 (typep time-zone '(integer -24 24))))
  t)
	 
;;; GET-UNIVERSAL-TIME
(deftest get-universal-time (typep (get-universal-time) '(integer 0)) t)

;;; DECODE-UNIVERSAL-TIME
(deftest decode-time-without-zone
  (typep (nth-value 8 (decode-universal-time 0))
	 '(rational -24 24)) t)
(deftest decode-time0
  (decode-universal-time 0 0) 0 0 0 1 1 1900 0 nil 0)
(deftest decode-time-second
  (decode-universal-time 1 0) 1 0 0 1 1 1900 0 nil 0)
(deftest decode-time-minute
  (decode-universal-time 60 0) 0 1 0 1 1 1900 0 nil 0)
(deftest decode-time-hour
  (decode-universal-time 3600 0) 0 0 1 1 1 1900 0 nil 0)
(deftest decode-time-day
  (decode-universal-time 86400 0) 0 0 0 2 1 1900 1 nil 0)
(deftest decode-time-month
  (decode-universal-time 2678400 0) 0 0 0 1 2 1900 3 nil 0)
(deftest decode-time-month-1
  (decode-universal-time 2678399 0) 59 59 23 31 1 1900 2 nil 0)
(deftest decode-time-2-28-1900
  (decode-universal-time 5011200 0) 0 0 0 28 2 1900 2 nil 0)
(deftest decode-time-3-1-1900
  (decode-universal-time 5097600 0) 0 0 0 1 3 1900 3 nil 0)
(deftest decode-time-2-28-1904
  (decode-universal-time 131155200 0) 0 0 0 28 2 1904 6 nil 0)
(deftest decode-time-2-29-1904
  (decode-universal-time 131241600 0) 0 0 0 29 2 1904 0 nil 0)
(deftest decode-time-3-1-1904
  (decode-universal-time 131328000 0) 0 0 0 1 3 1904 1 nil 0)
(deftest decode-time-2-28-2000
  (decode-universal-time 3160684800 0) 0 0 0 28 2 2000 0 nil 0)
(deftest decode-time-2-29-2000
  (decode-universal-time 3160771200 0) 0 0 0 29 2 2000 1 nil 0)
(deftest decode-time-3-1-2000
  (decode-universal-time 3160857600 0) 0 0 0 1 3 2000 2 nil 0)
(deftest decode-time-2-28-2004
  (decode-universal-time 3286915200 0) 0 0 0 28 2 2004 5 nil 0)
(deftest decode-time-3-1-2004
  (decode-universal-time 3287088000 0) 0 0 0 1 3 2004 0 nil 0)
(deftest decode-time-2-28-2400
  (decode-universal-time 15783465600 0) 0 0 0 28 2 2400 0 nil 0)
(deftest decode-time-3-1-2400
  (decode-universal-time 15783638400 0) 0 0 0 1 3 2400 2 nil 0)
(deftest decode-today
  (let ((now (get-universal-time)))
    (multiple-value-bind (sec min hour date month year)
	(decode-universal-time now)
      (= now (encode-universal-time sec min hour date month year))))
  t)

;;; ENCODE-UNIVERSAL-TIME
(deftest encode-time0
  (encode-universal-time 0 0 0 1 1 1900 0) 0)
(deftest encode-time-second
  (encode-universal-time 1 0 0 1 1 1900 0) 1)
(deftest encode-time-minute
  (encode-universal-time 0 1 0 1 1 1900 0) 60)
(deftest encode-time-hour
  (encode-universal-time 0 0 1 1 1 1900 0) 3600)
(deftest encode-time-day
  (encode-universal-time 0 0 0 2 1 1900 0) 86400)
(deftest encode-time-month
  (encode-universal-time 0 0 0 1 2 1900 0) 2678400)
(deftest encode-time-month-1
  (encode-universal-time 59 59 23 31 1 1900 0) 2678399)
(deftest encode-time-2-28-1900
  (encode-universal-time 0 0 0 28 2 1900 0) 5011200)
(deftest encode-time-1-3-1900
  (encode-universal-time 0 0 0 1 3 1900 0) 5097600)
(deftest encode-time-2-28-1904
  (encode-universal-time 0 0 0 28 2 1904 0) 131155200)
(deftest encode-time-2-29-1904
  (encode-universal-time 0 0 0 29 2 1904 0) 131241600)
(deftest encode-time-1-3-1904
  (encode-universal-time 0 0 0 1 3 1904 0) 131328000)
(deftest encode-time-2-28-2000
  (encode-universal-time 0 0 0 28 2 2000 0) 3160684800)
(deftest encode-time-2-29-2000
  (encode-universal-time 0 0 0 29 2 2000 0) 3160771200)
(deftest encode-time-1-3-2000
  (encode-universal-time 0 0 0 1 3 2000 0) 3160857600)
(deftest encode-time-2-28-2004
  (encode-universal-time 0 0 0 28 2 2004 0) 3286915200)
(deftest encode-time-1-3-2004
  (encode-universal-time 0 0 0 1 3 2004 0) 3287088000)
(deftest encode-time-2-28-2400
  (encode-universal-time 0 0 0 28 2 2400 0) 15783465600)
(deftest encode-time-1-3-2400
  (encode-universal-time 0 0 0 1 3 2400 0) 15783638400)

;;; INTERNAL-TIME-UNITS-PER-SECOND
(deftest internal-time-units-per-second
  (typep internal-time-units-per-second '(integer 1)) t)

;;; GET-INTERNAL-RUN-TIME
(deftest get-internal-run-time
  (let ((time (get-internal-run-time)))
    (and (typep time '(integer 0))
	 (<= time (get-internal-run-time))))
  t)

;;; GET-INTERNAL-REAL-TIME
(deftest get-internal-real-time
  (let ((time (get-internal-real-time)))
    (and (typep time '(integer 0))
	 (<= time (get-internal-real-time))))
  t)

;;; SLEEP
(deftest sleep1
  (let ((start (get-internal-real-time)))
    (sleep 1)
    (<= internal-time-units-per-second
	(- (get-internal-real-time) start)))
  t)
(deftest sleep2
  (let ((start (get-internal-real-time)))
    (sleep 2.5)
    (<= (* 2.5 internal-time-units-per-second)
	(- (get-internal-real-time) start)))
  t)

;;; 25.4.2 OTHER ENVIRONMENT INQUIRIES
(deftest lisp-implementation-type
  (not (null (search (or #+eclipse "ECLIPSE"
			 "")
		     (lisp-implementation-type)
		     :test #'char-equal))) t)
(deftest lisp-implementation-version
  (typep (lisp-implementation-version) 'string) t)
(deftest machine-type
  (not (null (search (dolist (f *features* "")
		       (case f
			 (:sun4c (RETURN "SUN4C"))
			 (:sparc (return "SPARC"))
			 (:pa-risc (return "9000"))
			 ((:pc :x86) (return "86"))))
		     (machine-type)
		     :test #'char-equal))) t)
(deftest machine-version
  (typep (machine-version) '(or string null)) t)
(deftest machine-instance
  (typep (machine-instance) '(or string null)) t)
(deftest software-type
  (not (null (search (dolist (f *features* "")
		       (case f
			 (:sunos (return "SUNOS"))
			 (:sun (return "SUN"))
			 ((:linux :linux86) (return "LINUX"))
			 ((:hp-ux :hpux) (return "HP"))
			 (:windows (return "WINDOWS"))))
		     (software-type)
		     :test #'char-equal)))
  t)
(deftest software-version
  (typep (software-version) '(or string null)) t)
(deftest short-site-name
  (typep (short-site-name) '(or string null)) t)
(deftest long-site-name
  (typep (long-site-name) '(or string null)) t)

(deftest features
  (and (find :ansi-cl *features*)
       (find :common-lisp *features*)
       (not (find :cltl2 *features*))
       (not (find :draft-ansi-cl *features*))
       (not (find :draft-ansi-cl-2 *features*)))
  t)
       

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 25.5 IDENTITY FUNCTION                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; IDENTITY
(deftest identity (identity 3) 3)
(deftest constanantly (funcall (constantly 4) 1 2 3) 4)