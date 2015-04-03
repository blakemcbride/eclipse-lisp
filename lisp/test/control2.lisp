;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 7.8 ITERATION                                                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 7.8.1 INDEFINITE ITERATION
;;; LOOP !!!

;;; 7.8.2 GENERAL ITERATION
;;; DO 
(deftest do-a
    (do ((temp-one 1 (1+ temp-one))
	 (temp-two 0 (1- temp-two)))
	((> (- temp-one temp-two) 5) temp-one))
  4)

(deftest do-b
    (do ((temp-one 1 (1+ temp-one))
	 (temp-two 0 (1+ temp-one)))     
	((= 3 temp-two) temp-one))
  3)

(deftest do-c
    (do ((inputs nil)
	 (outputs nil)
	 (values '(banana (57 boxes)))
	 (j 0 (+ j 1)))
	(nil)			;Do forever.
      (push j inputs)
      (let ((item (pop values)))
	(if (null item)		;Process items until NIL seen.
	    (return (values (nreverse inputs) (nreverse outputs)))
	  (progn (push j outputs)
		 (push item outputs)))))
  (0 1 2) (0 banana 1 (57 boxes)))

(deftest do-d
    (let ((a-vector (vector 1 nil 3 nil)))
       (do ((i 0 (+ i 1))	;Sets every null element of a-vector to zero.
	   (n (array-dimension a-vector 0)))
	  ((= i n))
	(when (null (aref a-vector i))
	  (setf (aref a-vector i) 0)))
       (equalp a-vector #(1 0 3 0)))
  t)


(deftest do-parallel
    (let ((foo '(a b c d))
	  (bar '(1 2 3)))
      (flet ((f (a b) (cons a b)))
	(do ((x foo (cdr x))
	     (y bar (cdr y))
	     (z '() (cons (f (car x) (car y)) z)))
	    ((or (null x) (null y))
	     (nreverse z)))))
  ((a . 1) (b . 2) (c . 3)))
      

(defun ribcage-lookup (sym ribcage)           
  (do ((r ribcage (cdr r)))
      ((null r) nil)
    (do ((s (caar r) (cdr s))
	 (v (cdar r) (cdr v))) 
	((null s))
      (when (eq (car s) sym)
	(return-from ribcage-lookup (car v))))))
(deftest do-nested
    (ribcage-lookup 'f '(((a b c) . (1 2 3))
			 ((e f) . (4 5))
			 ((g h i) . (6 7 8))))
  5)

#-(and excl (not eclipse))		;!!!
(deftest do-block-scope
    (let ((x 9))
      (do ((var (if x (return x) 1) (1+ var)))
	  ((= var 10) nil)
	(error "here")))
  9)

;;; DO*
(deftest do*
    (do* ((temp-one 1 (1+ temp-one))
	  (temp-two 0 (1+ temp-one)))
	((= 3 temp-two) temp-one))
  2)


;;; 7.8.3 SIMPLE ITERATION CONSTRUCTS
;;; DOLIST
(deftest dolist1
    (let ((temp-two nil))
      (dolist (temp-one '(1 2 3 4) temp-two)
	(push temp-one temp-two)))
  (4 3 2 1))

(deftest dolist2
    (let ((temp-two 0))
      (values (dolist (temp-one '(1 2 3 4) temp-one)
		(incf temp-two))
	      temp-two))
  nil 4)

(deftest dolist-env-value-empty
    (dolist (x nil x)) nil)
(deftest dolist-env-value
    (let ((seen nil))
      (dolist (x '(1 2 3) (values x seen))
	(push x seen)))
  nil (3 2 1))

;;; DOTIMES
(deftest dotimes1 (dotimes (temp-one 10 temp-one))  10)
(deftest dotimes2 (let ((temp-two 0))
		    (values (dotimes (temp-one 10 t) (incf temp-two))
			    temp-two))
  T 10)

(defun palindromep (string &optional
                           (start 0)
                           (end (length string)))
  (dotimes (k (floor (- end start) 2) t)
    (unless (char-equal (char string (+ start k))
                        (char string (- end k 1)))
      (return nil))))
(deftest dotimes-palindrome
    (values (palindromep "Able was I ere I saw Elba")
	    (palindromep "A man, a plan, a canal--Panama!")
	    (palindromep
	     (remove-if-not #'alpha-char-p
			    "A man, a plan, a canal--Panama!"))
            (palindromep
	     (remove-if-not
	      #'alpha-char-p
	      "Unremarkable was I ere I saw Elba Kramer, nu?"))
	    (palindromep
	     (remove-if-not
	      #'alpha-char-p
	      "A man, a plan, a cat, a ham, a yak,
                  a yam, a hat, a canal--Panama!")))
  t nil t t t)


;;; 7.8.4 MAPPING
;;; MAPCAR 
(deftest mapcar1 (mapcar #'car '((1 a) (2 b) (3 c))) (1 2 3))
(deftest mapcar2 (mapcar #'abs '(3 -4 2 -5 -6)) (3 4 2 5 6))
(deftest mpacar3 (mapcar #'cons '(a b c) '(1 2 3)) ((A . 1) (B . 2) (C . 3)))

;;; MAPLIST
(deftest maplist1 (maplist #'append '(1 2 3 4) '(1 2) '(1 2 3)) 
   ((1 2 3 4 1 2 1 2 3) (2 3 4 2 2 3))) 
(deftest maplist2 (maplist #'(lambda (x) (cons 'foo x)) '(a b c d))
  ((FOO A B C D) (FOO B C D) (FOO C D) (FOO D)))
(deftest maplist3
    (maplist #'(lambda (x) (if (member (car x) (cdr x)) 0 1)) '(a b a c d b c))
  (0 0 1 0 1 1 1))

;;; MAPC
(deftest mapc
    (let ((dummy nil))
      (values (mapc #'(lambda (&rest x) (setq dummy (append dummy x)))
		    '(1 2 3 4)
		    '(a b c d e)
		    '(x y z))
	      dummy))
  (1 2 3 4) (1 A X 2 B Y 3 C Z))

;;; MAPL 
(deftest mapl
    (let ((dummy nil))
      (values (mapl #'(lambda (x) (push x dummy)) '(1 2 3 4))
	      dummy))
  (1 2 3 4) ((4) (3 4) (2 3 4) (1 2 3 4)))

;;; MAPCAN 
(deftest mapcan1
    (mapcan #'(lambda (x y) (if (null x) nil (list x y)))
	    '(nil nil nil d e)
	    '(1 2 3 4 5 6))
  (D 4 E 5))
(deftest mapcan2
    (mapcan #'(lambda (x) (and (numberp x) (list x)))
	    '(a 1 b c 3 4 d 5))
  (1 3 4 5))

;;; MAPCON !!!

;;; 7.8.5 THE "PROGRAM FEATURE"
;;; TAGBODY
(deftest empty-tagbody (tagbody) nil)
(deftest tagbody-no-labels
     (let ((a 1) (b 2))
      (list (tagbody (setq a 2) (setq b 3) '0) a b))
  (nil 2 3))

#-allegro-v4.3				;!!!
(deftest tagbody-unused-labels
    (let ((a 1) (b 2))
      (list (tagbody (setq a 2) foo (setq b 3) 1 '0) a b))
  (nil 2 3))

;Neither Franz nor C accepts duplicate labels.
;; We could fix this by removing labels which no one goes to.
;; "If a tree falls in the forrest..."
#-(or excl cmu (and eclipse (not (or lisp-host machine-compile))))
(deftest tagbody-silly-labels
    (let ((a 1) (b 2))
      (list (tagbody ----
	      "first one assignment"
	      (setq a 2)
	     ----
	     "then another"
	      (setq b 3)
	     ----
	      "end") a b))
  (nil 2 3))
(deftest go-out-of-throw
    (list (catch 'foo
	    (let ((a 1))
	      (tagbody
		(throw 'foo (if a (go on) 99))
	       on)
	      a)))
  (1))

(deftest go-out-of-throw-in-mvc
    (multiple-value-call #'+
      (values 2 3)
      (catch 'foo
	(let ((a 1))
	  (tagbody
	    (throw 'foo (if a (go on) 99))
	   on)
	  a))
      (values 4))
  10)
(deftest go-out-of-mvc
    (list (let ((a 1))
	    (tagbody
	      (setq a (multiple-value-call #'+
			(values 1 2)
			(if a (go on) 99)))
	     on)
	    a))
  (1))

;;; PROG
(deftest prog
    (let ((a 1))
      (prog ((a 2) (b a)) (return (if (= a b) '= '/=))))
  /=)
(deftest prog-empty (prog () 'no-return-value) NIL)

;;; PROG*
(deftest prog*
    (prog* ((a 2) (b a)) (return (if (= a b) '= '/=)))
  =)
(deftest prog*-empty (prog* () 'no-return-value) NIL)

;;; GO
(deftest local-tagbody
    (let ((a 1) (b 2))
      (list (tagbody (go 1) (setq a 2) foo (setq b 3) 1) a b))
  (nil 1 2))

#-allegro-v4.3				;!!!
(deftest local-tagbody2
    (let ((a 1) (b 2))
      (list (tagbody (go foo) (setq a 2) foo (setq b 3) 1) a b))
  (nil 1 3))

(deftest nonlocal-tagbody
    (let ((a 1) (b 2))
      (list (tagbody (flet ((foo () (go 1))) (foo))
	      (setq a 2) foo (setq b 3) 1) a b))
  (nil 1 2))

#-allegro-v4.3				;!!!
(deftest nonlocal-tagbody2
    (let ((a 1) (b 2))
      (list (tagbody (flet ((foo () (go foo))) (foo))
	      (go foo) (setq a 2) foo (setq b 3) 1) a b))
  (nil 1 3))

(deftest double-nonlocal-tagbody
    (let ((a 1) (b 2))
      (list (tagbody (flet ((foo () (go 1)))
		       (flet ((bar () (foo)))
			 (bar)))
	      (setq a 2) foo (setq b 3) 1) a b))
  (nil 1 2))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 7.10 MULTIPLE VALUES                                             ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 7.10.1 CONSTRUCTS FOR HANDLING MULTIPLE VALUES
;;; VALUES 
(defun multiple-value-function (x)
  (values x (+ x 1)))
(eval-when (:compile-toplevel)
  (declaim (notinline multiple-value-function)))
(deftest values (values 1 2 3) 1 2 3)
(deftest no-values (values) )
(deftest values-order (let ((a 1) (b 2) (c 3) x)
			(values (progn (setq x 10)
				       a)
				(progn (setq x 20)
				       b)
				(progn (setq x (+ x 1))
				       c)
				x))
  1 2 3 21)

;;; MULTIPLE-VALUES-LIMIT
(deftest multiple-values-limit
    (typep call-arguments-limit '(integer 20)) t)

(defun many-values ()
  (values 1 2 3 4 5 6 7 8 9 10
	  11 12 13 14 15 16 17 18 19))
  
(deftest multiple-values-limit-works
    (multiple-value-list
	(locally (declare (notinline many-values))
	  (many-values)))
  (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19))

;;; VALUES-LIST
(deftest values-list-empty (values-list nil))
(deftest values-list1 (values-list '(1)) 1)
(deftest values-list3 (values-list '(1 2 3)) 1 2 3)

;;; MULTIPLE-VALUE-LIST
(deftest multiple-value-list
    (multiple-value-list (values 1 2)) (1 2))

;;; MULTIPLE-VALUE-CALL
(deftest multiple-value-call
    (multiple-value-call #'+ (values 1 2) (values) (values 3 4))
  10)

#-(and excl (not machine-compile))
(deftest empty-multiple-value-call
    (multiple-value-call #'+) 0)
(deftest function-multiple-value-call
    (multiple-value-call #'+
      (multiple-value-function 1)
      (multiple-value-function 3))
  10)
(deftest multiple-value-call2
    (let ((f #'+))
      (multiple-value-call f (values 1 2) (values) (values 3 4)))
  10)
#-(and excl (not machine-compile))
(deftest empty-multiple-value-call2
    (let ((f #'+)) (multiple-value-call f)) 0)
(deftest function-multiple-value-call2
    (let ((f #'+))
      (multiple-value-call f
	(multiple-value-function 1)
	(multiple-value-function 3)))
  10)
      
;;; MULTIPLE-VALUE-PROG1
(deftest empty-multiple-value-prog1 (multiple-value-prog1 2) 2)
(deftest empty-multiple-value-prog1-values
    (multiple-value-prog1 (values 1 2)) 1 2)
(deftest multiple-value-prog1
    (let (x y)
      (declare (ignorable y))
      (multiple-value-prog1 (values 1 2 x)
	(setq x 2)
	(setq y 3)))
  1 2 nil)
(deftest multiple-value-prog1-2
    (let (x y)
      (declare (ignorable x))
      (multiple-value-call #'+
	(multiple-value-prog1 (values 1 2)
	  (setq x 2)
	  (setq y 3))
	y))
  6)

;;; MULTIPLE-VALUE-BIND
(deftest multiple-value-bind
    (multiple-value-bind (f r) (values 11 9)
      (list f r))
  (11 9))

;;; MULTIPLE-VALUE-SETQ
(deftest multiple-value-setq
    (let (quotient remainder)
      (values 
       (multiple-value-setq (quotient remainder)
	 (values 1 2))
       quotient
       remainder))
  1 1 2)
(deftest multiple-value-setq-short
    (let (a b c)
      (values 
       (multiple-value-setq (a b c)
	 (values 1 2))
       a b c))
  1 1 2 nil)
(deftest multiple-value-setq-long
    (let (a b)
      (values (multiple-value-setq (a b)
		(values 4 5 6))
	      a b))
  4 4 5)
(deftest multiple-value-setq-empty
    (multiple-value-setq () (values 1 2 3))
  1)
(deftest multiple-value-setq-none
    (let (a b)
      (values (multiple-value-setq (a b) (values))
	      a b))
  nil nil nil)
;;; NTH-VALUE
(deftest nth-value-0 (nth-value 0 (values 'a 'b)) a)
(deftest nth-value-1 (nth-value 1 (values 'a 'b)) b)
(deftest nth-value-2 (nth-value 2 (values 'a 'b)) nil)

;;; 7.10.2 RULES GOVERNING THE PASSING OF MULTIPLE VALUES
;;; EVALUATION AND APPLICATION
(let ((f #'multiple-value-function))
  ;;; eval-values !!!
  (deftest apply-values (apply f '(2)) 2 3)
  (deftest funcall-values (funcall f 2) 2 3))
(deftest progn-values
    (progn (multiple-value-function 3)
	   (multiple-value-function 4))
  4 5)

(deftest load-time-value-single-value
  (load-time-value (values 1 2 3)) 1)

;;; IMPLICIT PROGN CONTEXTS
(deftest defun-values
    (progn (defun some-values (x)
	     (multiple-value-function x)
	     (multiple-value-function (+ x 1)))
	   (some-values 3))
  4 5)
;;; defmacro !!!
;;; deftype !!!
(deftest eval-when-values
    (eval-when (:execute)
      (multiple-value-function 3)
      (multiple-value-function 4))
  4 5)
(deftest progv-values
    (progv nil nil
      (multiple-value-function 3)
      (multiple-value-function 4))
  4 5)
(deftest let-values
    (let ()
      (multiple-value-function 3)
      (multiple-value-function 4))
  4 5)
(deftest let*-values
    (let* ()
      (multiple-value-function 3)
      (multiple-value-function 4))
  4 5)
(deftest when-values (when (some-identity-function 2)
		       (multiple-value-function 3)
		       (multiple-value-function 4))
  4 5)
(deftest unless-values (unless (some-identity-function nil)
			 (multiple-value-function 3)
			 (multiple-value-function 4))
  4 5)
;;; multiple-value-bind
(deftest localy-values
    (locally (multiple-value-function 3)
	     (multiple-value-function 4))
  4 5)
(deftest case-values
    (case (some-identity-function 2)
      (1 (error "here"))
      (2 (multiple-value-function 3)
	 (multiple-value-function 4))
      (t (error "here")))
  4 5)
(deftest case-values2
    (case (some-identity-function 2) 
      (1 (error "here"))
      (t (multiple-value-function 3)
	 (multiple-value-function 4)))
  4 5)
	
;;; typecase !!!
(deftest ecase-values
    (ecase (some-identity-function 2)
      (1 (error "here"))
      (2 (multiple-value-function 3)
	 (multiple-value-function 4)))
  4 5)
;;; etypecase !!!
;;; ccase !!!
;;; ctypecase !!!

;;; CONDITIONAL CONSTRUCTS
(deftest if-values (if (some-identity-function t)
		       (multiple-value-function 3)
		     (multiple-value-function 4))
  3 4)
(deftest if-values2 (if (some-identity-function nil)
		       (multiple-value-function 3)
		     (multiple-value-function 4))
  4 5)
(deftest if-simple (let ((a 1)) (if a 2 3)) 2)
(deftest if-simple-values (let ((a 1)) (declare (notinline values)) (if (values a) 2 3)) 2)
(deftest if-simple-neg (let ((a nil)) (if a 2 3)) 3)
(deftest if-simple-values-neg (locally (declare (notinline values)) (if (values) 2 3)) 3)
(deftest if-simple-statement (let ((a 1)) (if a (catch 'foo (throw 'foo 2)) 3)) 2)
(deftest if-simple-values-statement
  (let ((a 1)) (declare (notinline values)) (if (values a) (catch 'foo (throw 'foo 2)) 3)) 2)
(deftest if-simple-neg-statement (let ((a nil)) (if a 2 (catch 'foo (throw 'foo 3)))) 3)
(deftest if-simple-values-neg-statement
  (locally (declare (notinline values)) (if (values) 2 (catch 'foo (throw 'foo 3)))) 3)
(deftest if-complex (if (catch 'foo (throw 'foo 9)) 1 2) 1)
(deftest if-complex-neg (if (catch 'foo (throw 'foo nil)) 1 2) 2)
(deftest and-values (and (multiple-value-function 3)
			 (multiple-value-function 4))
  4 5)
(deftest or-values (and (multiple-value-function 3)
			(multiple-value-function 4))
  4 5)
(deftest cond-values2
    (cond ((some-identity-function nil) (multiple-value-function 1)
					(multiple-value-function 2))
	  ((some-identity-function t) (multiple-value-function 3)
				      (multiple-value-function 4))
	  (t (multiple-value-function 10) (multiple-value-function 20)))
  4 5)
(deftest cond-values3
    (cond ((some-identity-function nil) (multiple-value-function 1)
					(multiple-value-function 2))
	  (t (multiple-value-function 3) (multiple-value-function 4)))
  4 5)
(deftest cond-values-singleton
    (cond ((some-identity-function nil) (multiple-value-function 1)
					(multiple-value-function 2))
	  ((multiple-value-function 3))
	  (t (multiple-value-function 10) (multiple-value-function 20)))
  3)
(deftest cond-values-singleton2
    (cond ((some-identity-function nil) (multiple-value-function 1)
					(multiple-value-function 2))
	  ((multiple-value-function 3)))
  3)

;;; RETURNING FROM A BLOCK
(deftest block-values
    (block foo
      (multiple-value-function 3)
      (multiple-value-function 4))
  4 5)
(deftest return-from-values
    (block foo
      (when (some-identity-function t)
	(return-from foo
	  (multiple-value-function 4)))
      (multiple-value-function 3))
  4 5)
(deftest nonlocal-return-from-values
    (block foo
      (flet ((local-func (x)
	       (when (some-identity-function x)
		 (return-from foo
		   (multiple-value-function x)))))
	(declare (notinline local-func))
	(local-func 4)
	(multiple-value-function 3)))
  4 5)
(deftest nonlocal-return-from-breaking-up-catch-values
    (block foo
      (flet ((local-func (x)
	       (catch 'something
		 (when (some-identity-function x)
		   (return-from foo
		     (multiple-value-function x))))))
	(declare (notinline local-func))
	(local-func 4)
	(multiple-value-function 3)))
  4 5)
(deftest dolist-values
    (dolist (x '(1 2 3) (multiple-value-function 4)))
  4 5)

;;; THROWING OUT OF A CATCH
(deftest catch-values
    (catch 'foo
      (multiple-value-function 3)
      (multiple-value-function 4))
  4 5)
(deftest throw-values
    (catch 'foo
      (when (some-identity-function t)
	(throw 'foo
	  (multiple-value-function 4)))
      (multiple-value-function 3))
  4 5)
(deftest nonlocal-throw-values
    (catch 'foo
      (flet ((local-func (x)
	       (when (some-identity-function x)
		 (throw 'foo
		   (multiple-value-function x)))))
	(declare (notinline local-func))
	(local-func 4)
	(multiple-value-function 3)))
  4 5)

;;; MISCELLANEOUS SITUATIONS
(deftest unwind-protect-values
    (unwind-protect (multiple-value-function 4)
      (multiple-value-function 3))
  4 5)
(deftest unwind-protect-values2
    (unwind-protect 4
      (multiple-value-function 3))
  4)

(deftest the-values
    (the (values &rest fixnum) (multiple-value-function 4))
  4 5)

(deftest setq-values
    (locally (declare (special x))
      (setq x (multiple-value-function 4)))
  4)
(deftest setq-values2
    (locally (declare (special x y))
      (setq x (multiple-value-function 3)
	    y (multiple-value-function 4)))
  4)

;;; GOLDEN RULE OF VALUES
(defun no-values () (values))
(defun two-values (x) (values x 99))
(eval-when (:compile-toplevel)
  (declaim (notinline no-values two-values)))

(deftest function-argument-values (list (two-values 3)) (3))
(deftest function-argument-no-values (list (no-values)) (nil))
(deftest if-test-values (if (two-values t) 1 2) 1)
(deftest if-test-values2 (if (two-values nil) 1 2) 2)
(deftest if-test-no-values (if (no-values) 1 2) 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 7.11 DYNAMIC NON-LOCAL EXITS                                     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CATCH
(deftest empty-catch (catch 'foo) nil)
(deftest no-throw (let (a) (list (catch 'foo (setq a 1) 2) a)) (2 1))
;;; Interaction with tail position
#-(and cmu (not machine-compile))
(deftest catch-tail
  (block foo
    (return-from foo
      (flet ((bar (x) (catch x (values 1 2 3))))
	(declare (notinline bar))
	(bar 8))))
  1 2 3)
#-(and cmu (not machine-compile))
(deftest catch-tail-tagbody
  (block foo
    (return-from foo
      (flet ((bar (x) (tagbody (go catch) catch (catch x (values 1 2 3)))))
	(declare (notinline bar))
	(bar 8))))
  nil)

;;; UNWIND-PROTECT
;;; simple
(deftest empty-unwind-protect (unwind-protect 3) 3)
(deftest unwind-protect-normal-exit
    (let (a)
      (+ (unwind-protect 1
	   (setq a 2) (setq a (+ a 1)))
	 a))
  4)
;;; Interaction with CATCH
(deftest unwind-protect-catch
    (let (a)
      (+ (catch 'foo (unwind-protect 1
		       (setq a 2) (setq a (+ a 1))))
	 a))
  4)
(deftest unwind-protect-throw-exit
    (let (a)
      (+ (catch 'foo (unwind-protect (throw 'foo 1)
		       (setq a 2) (setq a (+ a 1))))
	 a))
  4)
(deftest unwind-protect-nonlocal-throw-exit
    (let (a)
      (+ (catch 'foo
	   (flet ((thrower (x) (throw 'foo x)))
	     (declare (notinline thrower))
	     (unwind-protect (thrower 1)
	       (setq a 2) (setq a (+ a 1)))))
	 a))
  4)

(deftest unwind-protect-nonlocal-throw-special-exit
    (let (a (tag (make-symbol "FOO")))
      (+ (catch tag
	   (flet ((thrower (x)
		    (declare (special x))
		    (catch (make-symbol "FOO"))
		    (throw tag x)))
	     (declare (notinline thrower))
	     (unwind-protect (thrower 1)
	       (setq a 2) (setq a (+ a 1)))))
	 a))
  4)
;;; Interaction with BLOCK
(deftest unwind-protect-return-exit
    (let (a)
      (+ (block foo (unwind-protect (return-from foo 1)
		       (setq a 2) (setq a (+ a 1))))
	 a))
  4)
(deftest unwind-protect-nonlocal-return-exit
    (let (a)
      (+ (block foo
	   (flet ((returner (x) (return-from foo x)))
	     (declare (notinline returner))
	     (unwind-protect (returner 1)
	       (setq a 2) (setq a (+ a 1)))))
	 a))
  4)
(deftest unwind-protect-nonlocal-special-return-exit
    (let (a)
      (+ (block foo
	   (flet ((returner (x)
		    (declare (special x))
		    (catch 'foo
		      (return-from foo x))))
	     (declare (notinline returner))
	     (unwind-protect (returner 1)
	       (setq a 2) (setq a (+ a 1)))))
	 a))
  4)
;;; Interaction with TAGBODY
(deftest unwind-protect-go-exit
    (let (a)
      (tagbody
	(unwind-protect (go out)
	  (setq a 2) (setq a (+ a 1)))
       out)
      a)
  3)
(deftest unwind-protect-nonlocal-go-exit
    (let (a)
      (tagbody
	(flet ((goer () (go out)))
	  (declare (notinline goer))
	  (unwind-protect (goer)
	    (setq a 2) (setq a (+ a 1))))
       out)
      a)
  3)
(deftest unwind-protect-nonlocal-special-go-exit
    (let (a)
      (tagbody
	(flet ((goer ()
		 (catch 'foo (go out))))
	  (declare (notinline goer))
	  (unwind-protect (goer)
	    (setq a 2) (setq a (+ a 1))))
       out)
      a)
  3)

;;; THROW
(deftest throw (let (a (b 0))
		    (list (catch 'foo (setq a 1) (throw 'foo 9)
				 (setq b 100) 2)
			  a b))
  (9 1 0))
(deftest throw-inner
    (catch 'foo
      (catch 'bar
	(+ 1 (catch 'foo
	       (catch 'baz
		 (throw 'foo 1))))))
  2)
(deftest throw-unwind
  (catch nil 
   (unwind-protect (throw nil 1)
     (throw nil 2)))
  2)
(deftest catch-nested
  (let ((inner nil))
    (values (catch 'foo
	      (setq inner
		    (catch 'foo
		      (unwind-protect (throw 'foo :first-throw)
			(throw 'foo :second-throw))))
	      :outer-catch)
	    inner))
  :outer-catch :second-throw)