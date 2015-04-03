#| *****************************************************************
Things that are used in basic run-time control structure.
***************************************************************** |#

(defvar *standard-input*)
(defvar *standard-output*)
(defvar *error-output*)
(defvar *terminal-io*)
(defvar *debug-io*)
(defvar *query-io*)
(defvar *trace-output*)
(defvar *null-output*)

;; Our compiler isn't yet smart enough to do this without consing, so
;; its written by hand in interface.c
#-machine-compile 
(defun FUNCALL (f &rest args)
  (declare (dynamic-extent args))
  (apply f args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                       TYPES                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Note that the MOP permits programmers to define EXTENDING methods
;;; on CLASS-NAME and CLASS-PRECEDENCE-LIST, but these must return the
;;; same value as the code here.  The MOP does not specify any
;;; circumstances under which CLASS-NAME and CLASS-PRECEDENCE-LIST
;;; must be called, so we are justified in using our own, hacked,
;;; non-method calling, code here.

(defun class-name-of (x)
  (host::static-access (object-class x) name class))
(defun class-precedence-list-of (x)
  (host::static-access (object-class x) class-precedence-list class))

(defun eclipse::find-list-eq (item list)		;We know that we have a proper list.
  (loop
   (if (consp list)
       (locally (declare (type cons list))
	 (when (eq item (car list)) (return item))
	 (setq list (cdr list)))
       (return nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 6.2.2 SPECIFIC DATA TYPE PREDICATES                          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun NULL (object) (eq object nil))
(defun ATOM (object) (not (consp object)))

;;; Potential optimization: it would be faster to compare the
;;; object-class directly against the class-instance.
(macrolet ((def-leaf (predicate class-name)
	     `(defun ,predicate (x)
		(eq (object-class x) ,(host::class-var class-name)))))
  (def-leaf CONSP cons)
  (def-leaf CHARACTERP character)
  (def-leaf INTEGERP integer)
  (def-leaf COMPLEXP complex)
  (def-leaf SIMPLE-VECTOR-P simple-vector)
  (def-leaf SIMPLE-BIT-VECTOR-P simple-bit-vector)
  (def-leaf ratiop ratio)
  (def-leaf single-float-p single-float)
  (def-leaf double-float-p double-float))
  
(macrolet ((def-null (predicate class-name)
	     `(defun ,predicate (x)
		(or (eq x nil)
		    (eq (object-class x) ,(host::class-var class-name))))))
  (def-null SYMBOLP symbol)
  (def-null LISTP cons))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 6.3 EQUALITY PREDICATES                                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun EQL (x y)
  (if (eq x y) t
      (let ((x-class (object-class x))
	    (y-class (object-class y)))
	(when (eq x-class y-class)
	  ;; Typecase hand expanded, and without reference to class slots.
	  (cond
	   ((eq INTEGER-classobj x-class)
	    (locally (declare (type integer x y))
	      (eq-integer-integer x y)))
	   ((eq DOUBLE-FLOAT-classobj x-class)
	    (locally (declare (type double-float x y))
	      (eq-double-float-double-float x y)))
	   ((eq SINGLE-FLOAT-classobj x-class)
	    (locally (declare (type single-float x y))
	      (eq-single-float-single-float x y)))
	   ((eq RATIO-classobj x-class)
	    (eq-number x y))
	   ((eq COMPLEX-classobj x-class)
	    (locally (declare (type complex x y))
	      (and (eql (complex-realpart x) (complex-realpart y))
		   (eql (complex-imagpart x) (complex-imagpart y))))))))))

(defun eql-hash (x)
  ;; Hand expansion of typecase
  (case (class-name-of x)
    (integer (if (fixnump x)
		 (eq-hash x)
		 (eclipse::digitref (eclipse::bignum-xint x) 0)))
    ((single-float double-float)
     (locally (declare (type float x))
       ;; Is there some scale-float trick that works better?
       ;; The idea is to obtain an integer (ideally a fixnum) that
       ;; keeps as many of the mantissa bits as possible.
       (eql-hash (floor (if (< (abs x) 1000)
			    (* x 1000)
			    x)))))
    (ratio (locally (declare (type ratio x))
	     (eclipse::merge-hash-codes
	      (eclipse::eql-hash (eclipse::ratio-numerator x))
	      (eclipse::eql-hash (eclipse::ratio-denominator x)))))
    (complex (locally (declare (type complex x))
	       (eclipse::merge-hash-codes
		(eclipse::eql-hash (complex-realpart x))
		(eclipse::eql-hash (complex-imagpart x)))))
    (t (eclipse::eq-hash x))))

(defun list-eq (x y)
  (do ((x x (cdr x))
       (y y (cdr y)))
      ((or (endp x) (endp y)) (not (or x y)))
    (let ((x (car x)) (y (car y)))
      (unless (or (eq x y)
		  (and (consp x) (consp y)
		       (list-eq x y)))
	(return nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 6.4 LOGICAL OPERATORS                                        ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun NOT (object) (eq object nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                 CONTROL STRUCTURE                            ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This should realy use etypecase rather than symbolp, to get a
;;; better error message.  
(defun BOUNDP (symbol)
  (if (symbolp symbol)
      (locally (declare (type symbol symbol))
	(not (unboundp (symbol-value-value symbol))))
      (error 'type-error :expected-type 'symbol :datum symbol)))

(defun fdefinition1 (name)
  (let* ((setfp (setf-function-name-p name))
	 (key (function-name-key name setfp)))
    (locally (declare (type symbol key))
      (if setfp
	  (symbol-setf-function-value key)
	  (symbol-function-value key)))))

(defun set-fdefinition1 (name value)
  (let* ((setfp (setf-function-name-p name))
	 (key (function-name-key name setfp)))
    (locally (declare (type symbol key))
      (if setfp
	  (set-symbol-setf-function-value key value)
	  (set-symbol-function-value key value)))))

(defun FBOUNDP (name)
  (not (null (fdefinition1 name))))


(defun FDEFINITION (name)
  (or (fdefinition1 name)
      (error 'undefined-function :name name)))

(defun FMAKUNBOUND (name)
  (set-fdefinition1 name nil)
  name)

(defun (setf FDEFINITION) (function name)
  (set-fdefinition1 name function))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 7.8 ITERATION                                                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 7.8.4 Mapping

;;; Do we really need to always copy the original-arglists?
(defun map1 (function original-arglists accumulate take-car)
  (let* ((arglists (copy-list original-arglists))
	 (ret-list (list nil)) 
	 (temp ret-list))
    (do ((res nil)
	 (args '() '()))
	((dolist (x arglists nil) (if (null x) (return t)))
	 (if accumulate
	     (cdr ret-list)
	     (car original-arglists)))
      (do ((l arglists (cdr l)))
	  ((null l))
	(push (if take-car (caar l) (car l)) args)
	(setf (car l) (cdar l)))
      (setq res (apply function (nreverse args)))
      (case accumulate
	(:nconc (setq temp (last (nconc temp res))))
	(:list (rplacd temp (list res))
	       (setq temp (cdr temp)))))))

(defun MAPCAR (function &rest lists)
  (declare (dynamic-extent lists))
  (map1 function lists :list t))

(defun MAPCAN (function &rest lists)
  (declare (dynamic-extent lists))
  (map1 function lists :nconc t))

(defun MAPC (function &rest lists)
  (declare (dynamic-extent lists))
  (map1 function lists nil t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                 SEQUENCES                                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SATISFIES-THE-TEST
;;; See Steele.  
;;; We use 'none for item to indicate an -if or -if-not function.

;;; Note that there are several conditionals here which always execute
;;; the same branch for each member of the sequence.  This may be a
;;; chance for some optimization!!!

(defun key-item (key item)
  (if key (funcall key item) item))

(defun satisfies-the-test (item x test test-not key)
  (let* ((f (or test-not test #'eql))
	 (item2 (key-item key x))
	 (ok (if (eq item 'none)
		 (funcall f item2)
	       (funcall f item item2))))
    (if test-not (not ok) ok)))

(defun length-list (list)
  (do ((list list (cdr list))
       (n 0 (add-integer-integer n 1)))
      ((endp list) n)
    (declare (type fixnum n))))

;;; N.B.:
;;; * This is NOT GENERIC.
;;; * This uses a hand expansion of typecase so we can bootstrap compilation.
;;;   It reflects the current state of the class hierarchy and changes
;;;   need to be added by hand.
(defun LENGTH (sequence)
  (case (class-name-of sequence)
    (null 0)
    (cons (length-list sequence))
    ((simple-base-string simple-extended-string simple-bit-vector simple-vector)
     (locally (declare (type simple-basic-vector sequence))
       (vector-size sequence)))
    ((complex-base-string complex-extended-string complex-bit-vector complex-vector
      base-string extended-string bit-vector general-vector)
     (locally (declare (type complex-basic-vector sequence))
       (complex-array-fill-pointer sequence)))
    (t (error 'type-error :datum sequence :expected-type 'sequence))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                 LISTS                                        ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ENDP (object)
  (unless (consp object)
    (or (null object)		
	(error 'type-error :datum object :expected-type 'list)))
  ;; Endp is used during C initialization before clInitClasses().
  #+faster-but-will-not-bootstrap-in-c
  (case (class-name-of object)
    (cons nil)
    (null t)
    (t (error 'type-error :datum object :expected-type 'list))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 25.4 ENVIRONMENT INQUIRIES                                   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *FEATURES*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 25.5 IDENTITY FUNCTION                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun IDENTITY (object) object)
(defun COMPLEMENT (fn)
  #'(lambda (&rest args) (not (apply fn args))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 22 INPUT/OUTPUT                                              ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ANSI defined initial values
(defparameter *PRINT-BASE* 10)
(defparameter *PRINT-CASE* :upcase)
(defparameter *PRINT-CIRCLE* nil)
(defparameter *PRINT-ESCAPE* t)
(defparameter *PRINT-GENSYM* t)
(defparameter *PRINT-LENGTH* nil)
(defparameter *PRINT-LEVEL* nil)
(defparameter *PRINT-LINES* nil)
(defparameter *PRINT-RADIX* nil)
(defparameter *PRINT-READABLY* nil)	;standard-io-syntax is t!
(defparameter *PRINT-RIGHT-MARGIN* nil)

;;; Implementation-dependent initial values

;; matches with-standard-io-syntax:
(defparameter *PRINT-ARRAY* t)
(defparameter *PRINT-PRETTY* nil)	;but set to t by clInitDefaultPP()!

;; does not match with-standard-io-syntax:
(defparameter *PRINT-MISER-WIDTH* 40)	;standard-io-syntax is is nil!

;;; "Implementation-dependent" value used when *print-right-margin* is
;;; nil, and STEAM-LINE-LENGTH returns nil.  Note that ~<...~>
;;; directive defaults to a line width of 72.
(defparameter *default-right-margin* 72)

(defparameter *current-level* 0)		; Current depth in logical blocks.
(defparameter *current-length* 0)		; Current position in logical block.
;;; Contains hash table used for locating circularities, or a stack.
(defparameter *circularity-hash-table* nil)
(defparameter *parents* nil)
