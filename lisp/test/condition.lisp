;;; The tests could be run in a context in which unhandled signals are
;;; trapped by the test harness.  This macro can be wrapped around
;;; forms to make sure that any unhandled signals from body just cause
;;; body to end.
(defmacro ignore-unhandled-signals (&body body)
  `(handler-case (progn ,@body)
     (condition () nil)))

;;; 29.4.1 SIGNALING CONDITIONS
;;; SIGNALING
(deftest signal-each-set-once
  (let ((called-handlers nil))
    (ignore-unhandled-signals
     (handler-case 
	(handler-bind
	    ((error (lambda (c)
		      (declare (ignore c))
		      (push 'error10 called-handlers)))
	     ;; 2. handler matches, pusshes condition10, then declines.
	     (simple-condition (lambda (c)
				 (declare (ignore c))
				 (push 'condition10 called-handlers)))
	     (simple-condition (lambda (c)
				 (declare (ignore c))
				 (push 'condition11 called-handlers))))
	  (handler-bind
	      ((error (lambda (c)
			(declare (ignore c))
			(push 'error00 called-handlers)))
	       ;; 1. handler matches, pushes condition00, then declines.
	       (simple-condition (lambda (c)
				   (declare (ignore c))
				   (push 'condition00 called-handlers)))
	       (simple-condition (lambda (c)
				   (declare (ignore c))
				   (push 'condition01 called-handlers))))
	    (signal "Should not see this.")))))
    (nreverse called-handlers))
  (condition00 condition10))

;;; In an interactive tes at top level (#+interactive-test), the
;;; resignaling of the condition returns.  Within some test harnelss
;;; like rt that catches unhandled conditions, the
;;; ignore-unhandled-signals macro keeps us from pushing the last two
;;; ids.

(deftest signal-rebinds-active-handlers
  (let ((called-handlers nil))
    (#-interactive-test ignore-unhandled-signals
		  #+interactive-test progn
     (handler-bind
	 ((error (lambda (c)
		   (declare (ignore c))
		   (push 'error10 called-handlers)))
	  ;; 2. matches second signal, and declined first signal.
	  (simple-condition (lambda (c)
			      (declare (ignore c))
			      (push 'condition10 called-handlers)))
	  (simple-condition (lambda (c)
			      (declare (ignore c))
			      (push 'condition11 called-handlers))))
       (handler-bind
	   ((error (lambda (c)
		     (declare (ignore c))
		     (push 'error00 called-handlers)))
	    ;; 1. handler matches and resignals, before pushind id and declining.
	    (simple-condition (lambda (c)
				(push 'start called-handlers)
				(signal c)
				(push 'condition00 called-handlers)))
	    (simple-condition (lambda (c)
				(declare (ignore c))
				(push 'condition01 called-handlers))))
	 (signal "Should not see this."))))
    (nreverse called-handlers))
  (start CONDITION10 #+interactive-test CONDITION00 #+interactive-test CONDITION10))

;;; 39.4.4 HANDLING CONDITIONS
;;; HANDLER-BIND
(defparameter *trapped-errors* nil)
(defun trap-error-handler (condition)
  (push (format nil "~A" condition) *trapped-errors*)
  (throw 'trap-errors nil))

(defmacro trap-errors (&rest forms)
  `(catch 'trap-errors
     (handler-bind ((error #'trap-error-handler))
       ,@forms)))

(deftest handler-bind
    (let ((*trapped-errors* nil))
      (list (trap-errors (ignore-unhandled-signals (signal "Foo.")) 1)
	    (trap-errors (error  "Bar.") 2)
	    (+ 1 2)
	    *trapped-errors*))
  (1 nil 3 ("Bar.")))

;;; HANDLER-CASE
(defun assess-condition (condition)
  (handler-case (signal condition)
    (warning () "Lots of smoke, but no fire.")
    ((or arithmetic-error control-error cell-error stream-error)
     (condition)
     (format nil "~a looks especially bad." (type-of condition)))
    (serious-condition (condition)
      (format nil "~a looks serious." condition))
    (condition () "Hardly worth mentioning."))
  )

(define-condition random-condition (condition) () 
   (:report (lambda (condition stream)
              (declare (ignore condition))
              (princ "Yow" stream))))

(deftest handler-case
  (values (assess-condition (make-condition 'stream-error :stream *terminal-io*))
	    (assess-condition (make-condition 'random-condition)))
  "STREAM-ERROR looks especially bad."
  "Hardly worth mentioning.")

;;; IGNORE-ERRORS
(deftest ignore-errors-normal
    (ignore-errors (values 1 2 3))
  1 2 3)
(deftest ignore-errors-error
    (multiple-value-bind (val condition)
	(ignore-errors (values (error 'control-error) 2 3))
      (unless val (type-of condition)))
  control-error)

;;; 29.4.5 DEFINING CONDITIONS
;;; DEFINE-CONDITION
(define-condition peg/hole-mismatch (error)
  ((peg-shape :initarg :peg-shape :reader peg/hole-mismatch-peg-shape)
   (hole-shape :initarg :hole-shape :reader peg/hole-mismatch-hole-shape))
  (:report (lambda (condition stream)
	     (format stream "A ~a page cannot go in a ~a hole."
		     (peg/hole-mismatch-peg-shape condition)
		     (peg/hole-mismatch-hole-shape condition)))))

;;; 29.4.6 CREATING CONDITIONS
;;; MAKE-CONDITION
(deftest make-condition
  (let ((c (make-condition 'peg/hole-mismatch
			   :peg-shape 'square :hole-shape 'round)))
    (and (typep c 'peg/hole-mismatch)
	 (typep c 'error)
	 (format nil "~a" c)))
  "A SQUARE page cannot go in a ROUND hole.")
    

;;; 29.4.7 ESTABLISHING RESTARTS
;;; RESTART-BIND !!!

;;; RESTART-CASE
(deftest restart-case1
    (restart-case
	(handler-bind ((error #'(lambda (c)
				  (declare (ignore c))
				  (invoke-restart 'my-restart 7))))
	  (error "Foo."))
      (my-restart (&optional v) v))
  7)

(define-condition food-error (error) ())
(define-condition bad-tasting-sundae (food-error) 
  ((ice-cream :initarg :ice-cream :reader bad-tasting-sundae-ice-cream)
   (sauce :initarg :sauce :reader bad-tasting-sundae-sauce)
   (topping :initarg :topping :reader bad-tasting-sundae-topping))
  (:report (lambda (condition stream)
	     (format stream "Bad tasting sundae with ~S, ~S, and ~S"
		     (bad-tasting-sundae-ice-cream condition)
		     (bad-tasting-sundae-sauce condition)
		     (bad-tasting-sundae-topping condition)))))
(defun all-start-with-same-letter (symbol1 symbol2 symbol3)
  (let ((first-letter (char (symbol-name symbol1) 0)))
    (and (eql first-letter (char (symbol-name symbol2) 0))
	 (eql first-letter (char (symbol-name symbol3) 0)))))
(defun read-new-value ()
  (declare (notinline eval))
  (format *query-io* "Enter a new value: ")
  (multiple-value-list (funcall (symbol-function 'eval) (read *query-io*))))
(defun verify-or-fix-perfect-sundae (ice-cream sauce topping)
  (do ()
      ((all-start-with-same-letter ice-cream sauce topping))
    (restart-case
	(error 'bad-tasting-sundae
	       :ice-cream ice-cream
	       :sauce sauce
	       :topping topping)
      (use-new-ice-cream (new-ice-cream)
	  :report "Use a new ice cream."
	  :interactive read-new-value  
	(setq ice-cream new-ice-cream))
      (use-new-sauce (new-sauce)
	  :report "Use a new sauce."
	  :interactive read-new-value
	(setq sauce new-sauce))
      (use-new-topping (new-topping)
	  :report "Use a new topping."
	  :interactive read-new-value
	(setq topping new-topping))))
  (values ice-cream sauce topping))

(deftest restart-case
    (handler-bind ((bad-tasting-sundae
		    #'(lambda (c)
			(invoke-restart (find-restart 'use-new-ice-cream c)
					'chocolate))))
      (verify-or-fix-perfect-sundae 'vanilla 'caramel 'cherry))
    chocolate caramel cherry)

#+not-yet
(deftest restart-case1
    (let* ((output (make-string-output-stream))
	   (*query-io* (make-two-way-stream
			(make-string-input-stream
			 "1
                         'chocolate")
			output)))
      (let ((vals (multiple-value-list
		      (verify-or-fix-perfect-sundae 'vanilla 'caramel 'cherry)))
	    (output (get-output-stream-string output)))
	(when (and (search "Bad tasting sundae with VANILLA, CARAMEL, and CHERRY" output)
		   (search "Use a new ice cream." output)
		   (search "Use a new sauce." output)
		   (search "Use a new topping" output))
	  vals)))
  (chocolate caramel cherry))

;;; WITH-SIMPLE-RESTART
(deftest with-simple-restart
  (with-simple-restart (foo "Nothing special")
    (invoke-restart 'foo))
  nil t)
(deftest with-simple-restart-normal
  (with-simple-restart (foo "Nothing special")
    (values 1 2 3))
  1 2 3)

;;; WITH-CONDITION-RESTARTS
(deftest with-condition-restarts
  (let ((c (make-condition 'error))
	(c2 (make-condition 'error)))
    (restart-case (with-condition-restarts c (list (find-restart 'foo))
		    (values (restart-name (find-restart 'foo c))
			    (find-restart 'foo c2)))
      (foo () 9)))
  foo nil)

;;; 29.4.8 FINDING AND MINIPULATING RESTARTS

;;; COMPUTE-RESTARTS
(defun collect-restart-names (n &aux names)
  (dolist (r (subseq (compute-restarts) 0 n) (reverse names))
    (push (or (restart-name r) (princ-to-string r)) names)))

(deftest compute-restarts
  (restart-case (collect-restart-names 5)
    (one () 1)
    (two () 2)
    (nil () :report "Who knows?" 'anonymous)
    (one () 'I)
    (two () 'II))
  (one two "Who knows?" one two))

;;; RESTART-NAME, FIND-RESTART 
(deftest find-restart
  (restart-case
      (let ((r (find-restart 'my-restart)))
	(format nil (formatter "~a is named ~a") r (restart-name r)))
    (my-restart () :report "foo" nil))
  "foo is named MY-RESTART")

;;; INVOKE-RESTART
(defun add3 (x) (check-type x number) (+ x 3))
(deftest invoke-restart 
  (handler-bind ((type-error #'(lambda (c)
				 (declare (ignore c))
				 (invoke-restart 'store-value 7))))
      (add3 'seven))
  10)

;;; INVOKE-RESTART-INTERACTIVELY
(deftest invoke-restart-interactively
  (restart-case
      (handler-bind ((file-error
		      #'(lambda (c)
			  (declare (ignore c))
			  (invoke-restart-interactively 'my-restart))))
	(error 'file-error :pathname "foo"))
    (my-restart (&optional (v 42))
		v))
  42)

(deftest invoke-restart-interactively-func
    (restart-case
	(handler-bind ((file-error
			#'(lambda (c)
			    (declare (ignore c))
			    (invoke-restart-interactively 'my-restart))))
	  (error 'file-error :pathname "foo"))
      (my-restart (v)
	 :interactive (lambda () (list 42))
	 v))
  42)

(deftest declined-restarts-still-available
  (restart-bind ((foo #'(lambda (x) x)))
    (values (invoke-restart 'foo 1)
	    (invoke-restart 'foo 2)))
  1 2)
    

;;; 29.4.10 RESTART FUNCTIONS
;;; MUFFLE-WARNING
(defvar *all-quiet* nil)
(defvar *saved-warnings* '())
(defun quiet-warning-handler (c)
  (when *all-quiet*
    (let ((r (find-restart 'muffle-warning c)))
      (when r 
	(push c *saved-warnings*)
	(invoke-restart r)))))
(defmacro with-quiet-warnings (&body forms)
  `(let ((*all-quiet* t)
	 (*saved-warnings* '()))
     (handler-bind ((warning #'quiet-warning-handler))
       ,@forms
       *saved-warnings*)))
#+broken-streams
(deftest muffle-warnings-restart
  (let ((s (make-array 50 :element-type 'base-char :fill-pointer 0
		       :adjustable t)))
    (with-output-to-string (*debug-io* s)
      (let ((saved (with-quiet-warnings
		    (warn "Situation #1.")
		    (let ((*all-quiet* nil))
		      (warn "Situation #2."))
		    (warn "Situation #3."))))
	(values s (format nil "~{~a ~}" (reverse saved))))))
  "Warning: Situation #2."
  "Situation #1. Situation #3.")

;;; ABORT
(defmacro abort-on-error (&body forms)
  `(handler-bind ((error #'abort))
     ,@forms))
(deftest abort-function-normal
  (abort-on-error (+ 3 5)) 8)
(deftest abort-function
  (with-simple-restart (abort "Proceed with testing.")
    (abort-on-error (error "You lose.")))
  nil t)

;;; CONTINUE
(defun real-sqrt (n)
  (when (minusp n)
    (setq n (- n))
    (cerror "Return sqrt(~D) instead." "Tried to take sqrt(-~D)." n))
  (sqrt n))

(deftest continue-function-normal
  (real-sqrt 4) 2.0f0)
(deftest continue-function
  (handler-bind ((error #'(lambda (c) (declare (ignore c)) (continue))))
    (real-sqrt -9)) 3.0f0)

;;; MUFFLE-WARNING
(defun ignore-warning (condition)
  (declare (ignore condition))
  (muffle-warning))
(deftest muffle-warning-function
  (let ((counts nil))
    (handler-bind ((warning #'ignore-warning))
      (values 
       (do ((counter 3 (1- counter)))
	   ((= counter 0) 'done)
	 (when (= counter 1)
	   (warn "Almost done"))
	 (push counter counts))
       (reverse counts))))
  done (3 2 1))

;;; STORE-VALUE, USE-VALUE
(defun careful-symbol-value (symbol)
  (check-type symbol symbol)
  (restart-case (if (boundp symbol)
		    (return-from careful-symbol-value 
		      (symbol-value symbol))
		    (error 'unbound-variable
			   :name symbol))
    (use-value (value)
	       :report "Specify a value to use this time."
	       value)
    (store-value (value)
		 :report "Specify a value to store and use in the future."
		 (setf (symbol-value symbol) value))))

(defparameter *store-value-place* 1234)
(deftest use-store-value-function
  (let ((*store-value-place* 1234))
    (values (careful-symbol-value '*store-value-place*)
	    (makunbound '*store-value-place*)
	    (handler-bind ((unbound-variable #'(lambda (c) (use-value 12 c))))
	      (careful-symbol-value '*store-value-place*))
	    (handler-bind ((unbound-variable #'(lambda (c) (store-value 24 c))))
	      (careful-symbol-value '*store-value-place*))
	    (careful-symbol-value '*store-value-place*)))
  1234 *store-value-place* 12 24 24)

;;; USE-VALUE
(defun add-symbols-with-default (default &rest symbols)
  (handler-bind ((unbound-variable
		  #'(lambda (c)
		      (declare (ignore c)) 
		      (use-value default))))
    (apply #'+ (mapcar #'careful-symbol-value symbols))))
(deftest use-value-function
  (let ((x 1) (y 2))
    (declare (special x y))
    (makunbound 'z)
    (add-symbols-with-default 3 'x 'y 'z))
  6)

;;; 29.4.11 DEBUGGING UTILITIES
(deftest debugger-hook
  ;; Handler-bind wrapper is necessary for testing because test
  ;; harness may wrap a handler-case around forms being tested which
  ;; returns the error condition rather than invoking the debugger.
  (handler-bind ((error (lambda (condition) (invoke-debugger condition))))
      ;; This is the form that, by itself (i.e. without a test
      ;; harness), should invoke the debbuger and *debugger-hook*.
      (let ((c (make-condition 'error)))
	(eq c (block foo
		(let ((*debugger-hook* #'(lambda (c old-hook)
					   (declare (ignore old-hook))
					   (return-from foo c))))
		  (error c))))))
  t)
			     