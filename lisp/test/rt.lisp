#|----------------------------------------------------------------------------|
 | Copyright 1990 by the Massachusetts Institute of Technology, Cambridge MA. |
 |                                                                            |
 | Permission  to  use,  copy, modify, and distribute this software  and  its |
 | documentation for any purpose  and without fee is hereby granted, provided |
 | that this copyright  and  permission  notice  appear  in  all  copies  and |
 | supporting  documentation,  and  that  the  name  of M.I.T. not be used in |
 | advertising or  publicity  pertaining  to  distribution  of  the  software |
 | without   specific,   written   prior   permission.      M.I.T.  makes  no |
 | representations  about  the  suitability of this software for any purpose. |
 | It is provided "as is" without express or implied warranty.                |
 |                                                                            |
 |  M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,  INCLUDING  |
 |  ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL  |
 |  M.I.T. BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL  DAMAGES  OR  |
 |  ANY  DAMAGES  WHATSOEVER  RESULTING  FROM  LOSS OF USE, DATA OR PROFITS,  |
 |  WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER  TORTIOUS  ACTION,  |
 |  ARISING  OUT  OF  OR  IN  CONNECTION WITH THE USE OR PERFORMANCE OF THIS  |
 |  SOFTWARE.                                                                 |
 |----------------------------------------------------------------------------|#

;This is the December 19, 1990 version of the regression tester.


;;(provide "RT")
#|(export
  '(deftest get-test do-test rem-test
    rem-all-tests do-tests pending-tests
    continue-testing *test*
    *do-tests-when-defined* gives-error))|#

#-eclipse
(defun Make-Name (format &rest args)
  (declare (dynamic-extent args))
  (intern (apply #'format nil (string format) args)))


(defvar *test* nil "Current test name")
(defvar *do-tests-when-defined* nil)
(defvar *entries* '(nil) "Test database")
(defvar *in-test* nil "Used by TEST")
(defvar *debug* nil "For debugging")

(defstruct (entry (:type list))
  pend name form func)

(defmacro vals (entry) `(cddddr ,entry))

(defmacro defn (entry) `(cdr ,entry))

(defun pending-tests ()
  (do ((l (cdr *entries*) (cdr l))
       (r nil))
      ((null l) (nreverse r))
    (when (entry-pend (car l))
      (push (entry-name (car l)) r))))

(defun rem-all-tests ()
  (setq *entries* (list nil))
  nil)

(defun rem-test (&optional (name *test*))
  (do ((l *entries* (cdr l)))
      ((null (cdr l)) nil)
    (when (equal (entry-name (cadr l)) name)
      (setf (cdr l) (cddr l))
      (return name))))

(defun get-test (&optional (name *test*))
  (defn (get-entry name)))

(defun get-entry (name)
  (let ((entry (find name (cdr *entries*)
		     :key #'entry-name
		     :test #'equal)))
    (when (null entry)
      (report-error t
        "~%No test with name ~:@(~S~)."
	name))
    entry))

(defmacro deftest (name form &rest values)
  (declare (notinline make-name))
  `(add-entry (nconc (list t ',name ',form
			   (defun ,(funcall (symbol-function 'make-name)
					    "TST-~a" name)
			     () ,form))
		     ',values)))

(defun add-entry (entry)
  (setq entry (copy-list entry))
  (do ((l *entries* (cdr l))) (nil)
    (when (null (cdr l))
      (setf (cdr l) (list entry))
      (return nil))
    (when (equal (entry-name (cadr l)) 
		 (entry-name entry))
      (setf (cadr l) entry)
      (report-error nil
        "Redefining test ~@:(~S~)."
        (entry-name entry))
      (return nil)))
  (when *do-tests-when-defined*
    (do-entry entry))
  (setq *test* (entry-name entry)))

(defun report-error (error? &rest args)
  (cond (*debug* 
	 (apply #'format *standard-output* args)
	 (if error? (throw '*debug* nil)))
	(error? (apply #'error args))
	(t (apply #'warn args))))

(defun do-test (&optional (name *test*))
  (do-entry (get-entry name)))

(defun do-entry (entry &optional
		       (s *standard-output*))
  (catch '*in-test*
    (setq *test* (entry-name entry))
    (setf (entry-pend entry) t)
    (let* ((*in-test* t)
	   (*break-on-signals* nil)
	   (r (handler-case (multiple-value-list
			     (funcall (entry-func entry)))
		(condition (condition) (list condition)))))
      (setf (entry-pend entry)
	    (not (funcall (find-symbol "EQUAL") r (vals entry))))
      (when (entry-pend entry)
	(let ((*print-circle* t)
	      *print-length* *print-level* *print-lines*)
	  (format s (formatter "~&Test ~:@(~S~) failed~
                   ~% Form: ~S~
                   ~% Expected value~P: ~
                      ~{~S~^~%~18t~}~
                   ~% Actual value~P: ~
                      ~{~S~^~%~16t~}.~%")
		  *test* (entry-form entry)
		  (length (vals entry))
		  (vals entry)
		  (length r) r)))))
  (unless (entry-pend entry) *test*))

(defun continue-testing ()
  (if *in-test*
      (throw '*in-test* nil)
      (do-entries *standard-output*)))

(defun do-tests (&optional (out *standard-output*)
			   (copy-stream *standard-output*))
  (dolist (entry (cdr *entries*))
    (setf (entry-pend entry) t))
  (if (streamp out)
      (do-entries out)
      (let ((path (pathname out)))
	(unless (pathname-type path :case :common)
	  (setq path (merge-pathnames path
				      (make-pathname :type (software-type)))))
	(with-open-file (stream path :direction :output
				:if-exists :supersede)
	  (format stream (formatter "~a ~a: ~a~@[-~a~]/~a~@[-~a~]~2%")
		  (lisp-implementation-type)
		  (lisp-implementation-version)
		  (machine-type)
		  (machine-version)
		  (software-type)
		  (software-version))
	  (let ((start (get-internal-real-time)))
	    (do-entries (if copy-stream
			    (make-broadcast-stream stream copy-stream)
			    stream))
	    (let* ((minutes (round (- (get-internal-real-time) start)
				   (* (symbol-value 'internal-time-units-per-second)
				      60)))
		   (hours (floor minutes 60)))
	      (format stream (formatter "~&Execution took ~d:~2,'0d (i.e. ~d minutes).~%")
		      hours
		      (- minutes (* hours 60))
		      minutes)))))))

(defun do-entries (s)
  (format s (formatter "~&Doing ~A pending test~:P ~
             of ~A tests total.~%")
          (count t (cdr *entries*)
		 :key #'entry-pend)
	  (length (cdr *entries*)))
  (dolist (entry (cdr *entries*))
    (when (entry-pend entry)
      (format s (formatter "~@[~<~%~:; ~S~>~]")
	      (do-entry entry s))
      (finish-output s)))
  (let ((pending (pending-tests)))
    (if (null pending)
	(format s (formatter "~%No tests failed.~%"))
	(let ((*print-circle* t)
	      *print-length* *print-level* *print-lines*)
	  (format s (formatter "~2%~A out of ~A ~
                   total tests failed: ~
                   ~:@(~{~<~%   ~1:;~S~>~
                         ~^, ~}~).~%")
		  (length pending)
		  (length (cdr *entries*))
		  pending)))
    (null pending)))

(defmacro gives-error (form)
  `(multiple-value-bind (val cond)
       (ignore-errors ,form)
     (if (null cond) val
       (values))))

