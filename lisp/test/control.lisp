(defun some-identity-function (x) x)
(eval-when (:compile-toplevel)
  (declaim (notinline some-identity-function)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 7.1 CONSTANTS AND VARIABLES                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 7.1.1 REFERENCE
;;; QUOTE
(locally (declare (special a))
  (deftest unquoted (progn (setq a 43)
			   (list a (cons a 3)))
    (43 (43 . 3)))
  (deftest quote (list (quote a) (quote (cons a 3)))
    (a (cons a 3))))

;;; FUNCTION
(defun adder (x) (function (lambda (y) (+ x y))))
(deftest defun-closure (funcall (adder 3) 5) 8)
(deftest closure-bindings
    (let ((funs ((lambda (x)
		   (list (function (lambda () x))
			 (function (lambda (y) (setq x y)))))
		 6)))
      (list (funcall (car funs))
	    (funcall (cadr funs) 43)
	    (funcall (car funs))))
  (6 43 43))
(defun two-funs (x)
  (list (function (lambda () x))
	(function (lambda (y) (setq x y)))))
(deftest closure-bindings2
    (let ((funs (two-funs 6)))
      (list (funcall (car funs))
	    (funcall (cadr funs) 43)
	    (funcall (car funs))))
  (6 43 43))

;;; SYMBOL-VALUE
(deftest symbol-value
    (values (setf (symbol-value 'a) 1)
	    (symbol-value 'a)
	    (let ((a 2)) (declare (ignorable a)) (symbol-value 'a))
	    (let ((a 2)) (declare (ignorable a)) (setq a 3) (symbol-value 'a))
	    (let ((a 2)) (declare (special a)) (declare (ignorable a)) (symbol-value 'a))
	    (let ((a 2)) (declare (special a)) (setq a 3) (symbol-value 'a))
	    (let ((a 2)) (setf (symbol-value 'a) 3) a)
	    (symbol-value 'a)
	    (let ((a 4))
	      (declare (special a))
	      (let ((b (symbol-value 'a)))
		(setf (symbol-value 'a) 5)
		(list a b)))
	    (locally (declare (special a)) a)
	    (symbol-value :any-keyword)
	    (symbol-value 'nil))
  1 1 1 1 2 3 2 3 (5 4) 3 :ANY-KEYWORD NIL)

;;; SYMBOL-FUNCTION 
(deftest symbol-function
    (let ((f #'(lambda () 3)))
      (values (eq (setf (symbol-function 'foo) f) f)
	      (funcall (symbol-function 'foo))))
  t 3)

;;; FDEFINITION
(deftest fdefinition-simple
    (let ((f #'(lambda () 3)))
      (values (eq (setf (fdefinition 'foo) f) f)
	      (funcall (fdefinition 'foo))))
  t 3)
(deftest fdefinition-setf
    (let* ((x 0)
	   (f #'(lambda (new) (setq x new))))
      (values (eq (setf (fdefinition '(setf x)) f) f)
	      (funcall (fdefinition '(setf x)) 3)
	      x))
  t 3 3)

;;; BOUNDP
(deftest boundp
    (values (locally (declare (special x)) (setq x 1))
	    (boundp 'x)
	    (makunbound 'x)
	    (boundp 'x)
	    (let ((x 2)) (declare (ignorable x)) (boundp 'x))
	    (let ((x 2)) (declare (special x)) (boundp 'x)))
  1 t x nil nil t)

;;; FBOUNDP 
(deftest fboundp
    (values (let ((f #'(lambda () 3)))
	      (eq (setf (symbol-function 'x) f) f))
	    (not (null (fboundp 'x)))
	    (fmakunbound 'x)
	    (fboundp 'x)
	    (flet ((x () 9)) (fboundp 'x) (x)))
  t t x nil 9)

;;; SPECIAL-OPERATOR-P
(deftest special-operator-p1 (not (special-operator-p 'if)) nil)
(deftest special-operator-p2 (special-operator-p 'car) nil)
(deftest special-operator-p3 (special-operator-p 'one) nil)

;;; 7.1.2 ASSIGNMENT
;;; SETQ
(deftest empty-setq (setq) nil) 
(locally (declare (special a b c x y))
  (deftest special-simple-setq (list (setq a 1 b 2 c 3) a b c)
    (3 1 2 3))
  (deftest special-sequential-assignment
      (list (setq a (+ b 1) b (+ a 1) c (+ a b))
	    a b c)
    (7 3 4 7))
  (deftest special-setq-order (list (setq x (+ 3 2 1) y (cons x nil))
				    x y)
    ((6) 6 (6)))
  (deftest special-setq-symbol-macro
      (let ((x (list 10 20 30)))
	(symbol-macrolet ((y (car x)) (z (cadr x)))
	  (setq y (+ 1 z) z (+ y))
	  (list x y z)))
      ((21 21 30) 21 21)))
(let (a b c x y)
  (deftest local-simple-setq (list (setq a 1 b 2 c 3) a b c)
    (3 1 2 3))
  (deftest local-sequential-assignment
      (list (setq a (+ b 1) b (+ a 1) c (+ a b))
	    a b c)
    (7 3 4 7))
  (deftest local-setq-order (list (setq x (+ 3 2 1) y (cons x nil))
				  x y)
    ((6) 6 (6)))
  (deftest local-setq-symbol-macro
      (let ((x (list 10 20 30)))
	(symbol-macrolet ((y (car x))
			  (z (cadr x)))
	  (setq y (+ 1 z) z (+ 1 y))
	  (list x y z)))
    ((21 22 30) 21 22)))

;;; PSETQ
(deftest empty-psetq (psetq) nil)
(deftest psetq (let (a b c)
		 (values (psetq a 1 b 2 c 3)
			 (psetq a (1+ b) b (1+ a) c (+ a b))
			 a b c))
  nil nil 3 2 3)
(deftest psetq-symbol-macro
    (let ((x (list 10 20 30)))
      (symbol-macrolet ((Y (car x)) (z (cadr x)))
	(psetq y (1+ z) z (1+ y))
	(list x y z)))
  ((21 11 30) 21 11))
(deftest psetq-swap
    (let ((a 1) (b 2))
      (psetq a b b a)
      (values a b))
  2 1)

;;; SET
(deftest set
    (values (set 'a 1)
	    (symbol-value 'a)
	    (let ((a 2)) (set 'a 3) a)
	    (symbol-value 'a)
	    (let ((a 4))
	      (declare (special a))
	      (let ((b (symbol-value 'a)))
		(set 'a 5)
		(list a b))))
  1 1 2 3 (5 4))

;;; MAKUNBOUND (See above)
;;; FMAKUNBOUND (See above)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 7.2 GENERALIZED VARIABLES                                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; All of these need to test setf expansions involving multiple store
;;; values!!! 

;;; SETF
(deftest setf-empty (setf) nil)
(deftest setf-locals
    (let ((a 1) (b 2) (c 3))
      (values (setf a 2 b c c b) a b c))
  3 2 3 3)
;;; Standard setf'able functions are tested near the accessor function test.
(deftest setf-the
    (let ((x (list 1 2)) (y 3))
      (values (setf (the integer (cadr x)) (+ y 3))
	    x y))
  6 (1 6) 3)
;;; Setf VALUES!!!
;;; Setf APPLY!!!

;;; PSETF
(deftest psetf
    (let ((x (cons 'a 'b))
	  (y (list 1 2 3)))
      (values (psetf (car x) 'x (cadr y) (car x) (cdr x) y)
	      x y))
  nil (X 1 A 3)  (1 A 3))

;;; SHIFTF
(deftest shiftf
    (let ((x (list 1 2 3))
	  (y 'trash))
      (values (shiftf y x (cdr x) '(hi there))
	      x y))
   TRASH (2 3) (1 HI THERE))

;;; ROTATEF
(deftest rotatef
    (let ((n 0)
	  (x (list 'a 'b 'c 'd 'e 'f 'g)))
      (rotatef (nth (incf n) x)
	       (nth (incf n) x)
	       (nth (incf n) x))
      x)
  (A C D B E F G))

;;; DEFINE-MODIFIY-MACRO
(eval-when (:compile-toplevel :execute)
  (define-modify-macro appendf (&rest args)
    append "Append onto list")
  (define-modify-macro new-incf (&optional (delta 1)) +)
  (define-modify-macro unionf (other-set &rest keywords) union))
(deftest define-modify-macro
    (let* ((x '(a b c))
	   (y x)
	   (a 2))
      (values (appendf x '(d e f) '(1 2 3))
	      x y
	      (new-incf a)
	      (new-incf a 10)
	      a
	      (progn (unionf y '("B" "D") :test #'string=)
		     (set-exclusive-or y '(a b c d) :test #'string=))))
  (a b c d e f 1 2 3)
  (a b c d e f 1 2 3)
  (a b c)
  3 13 13
  nil)
	      

;;; DEFSETF
(defun middleguy (x) (nth (truncate (1- (list-length x)) 2) x))
(defun set-middleguy (x v)
  (unless (null x)
    (rplaca (nthcdr (truncate (1- (list-length x)) 2) x) v))
  v)
(defsetf middleguy set-middleguy)
(deftest defsetf-short
    (let ((a (list 'a 'b 'c 'd))
	  (b (list 'x))
	  (c (list 1 2 3 (list 4 5 6) 7 8 9)))
      (values (setf (middleguy a) 3)
	      (setf (middleguy b) 7)
	      (setf (middleguy (middleguy c)) 'middleguy-symbol)
	      a b c))
  3 7 MIDDLEGUY-SYMBOL
  (A 3 C D) (7) (1 2 3 (4 MIDDLEGUY-SYMBOL 6) 7 8 9))
(defsetf example-subseq (sequence start &optional end) (new-sequence)
  `(progn (replace ,sequence ,new-sequence
		   :start1 ,start :end1 ,end)
	  ,new-sequence))

;;; The ANSI examples use non-keyword "key-names" x and y, rather than
;;; keywords :xx and :yy.  EXCL and CMU have bugs which requires the
;;; use of keywords.  In Excl, the bug is actually only manifest in
;;; defsetf - defun is ok.  In CMU, its in both places.

(defvar *xy* (make-array '(10 10)))
(defun xy (&key ((x x) 0) ((y y) 0)) (aref *xy* x y))
#-(and cmu (not eclipse))
(progn
  (defun set-xy (new-value &key ((x x) 0) ((y y) 0))
    (setf (aref *xy* x y) new-value))
  (defsetf xy (&key ((x x) 0) ((y y) 0)) (store)
    `(set-xy ,store 'x ,x 'y ,y)))

#-(or excl cmu)
(deftest setf-long
    (values (setf (xy 'x 1) 1)
	    (xy 'x 1)
	    (let ((a 'x) (b 'y))
	      (setf (xy a 1 b 2) 3)
	      (setf (xy b 5 a 9) 14))
	    (xy 'y 0 'x 1)
	    (xy 'x 1 'y 2))
    1 1 14 1 3)

#-(and (or cmu excl) (not eclipse))
(defsetf xxy (&environment zz
	      &key ((:xx x) 0) ((:yy y) 0))
  (store)
  (setf (symbol-value 'env) zz)
  `(set-xy ,store :xx ,x :yy ,y))

#-(or cmu (and excl (not eclipse)))
(when (fboundp 'augment-environment)
  (deftest setf-long-env
    (let ((e (augment-environment nil :variable '(ww))))
      (declare (special env) (notinline get-setf-expansion))
      (funcall (symbol-function 'get-setf-expansion) '(xxy a b) e)
      (eq e env))
    t))

;;; DEFINE-SETF-EXPANDER 
(defun lastguy (x) (car (last x)))
(define-setf-expander lastguy (x
			       #-cmu &environment
			       #-cmu env)
  "Set the last element in a list to the given value."
  (multiple-value-bind (dummies vals newval setter getter)
      (funcall (symbol-function 'get-setf-expansion)
	       x #-(and cmu (not machine-compile)) env)
    (declare (ignore setter newval))
    (let ((store (gensym)))
      (values dummies
	      vals
	      `(,store)
	      `(progn (rplaca (last ,getter) ,store) ,store)
	      `(lastguy ,getter)))))
(deftest define-setf-expander
  (let ((a (list 'a 'b 'c 'd))
	(b (list 'x))
	(c (list 1 2 3 (list 4 5 6))))
    (values (setf (lastguy a) 3)
	    (setf (lastguy b) 7)
	    (setf (lastguy (lastguy c)) 'lastguy-symbol)
	    a b c))
  3 7 LASTGUY-SYMBOL (A B C 3) (7) (1 2 3 (4 5 LASTGUY-SYMBOL)))

;;; GET-SETF-EXPANSION See above.

(deftest local-across-fboundary
  (let* ((a 1)
	 (f #'(lambda () a)))
    (funcall f))
  1)

(deftest local-assignment-across-fboundary
  (let* ((a 1)
	 (f #'(lambda (x) (setq a x)))
	 (f2 #'(lambda () a)))
    (funcall f 3)
    (funcall f2))
  3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 7.3 FUNCTION INVOCATION                                          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; APPLY
(deftest apply-bound-symbol (let ((f '+)) (apply f '(1 2))) 3)
(deftest apply-symbol (apply '+ '(1 2)) 3)
(deftest apply-function (apply #'+ 3 5 '(2 7 3)) 20)
(deftest apply-symbol2 (apply 'cons '((+ 2 3) 4)) ((+ 2 3) . 4))
(deftest apply-null-args (apply #'+ '()) 0)
(deftest apply-keys (apply #'(lambda (&key a b) (list a b))
				   '(:b 3))
  (nil 3))

;;; FUNCALL
(deftest funcall-bound-symbol (let ((f '+)) (funcall f 1 2)) 3)
(deftest funcall-symbol (funcall '+ 1 2) 3)
(deftest funcall-function (funcall #'+ 3 5 2 7 3) 20)
(deftest funcall-symbol2 (funcall 'cons '(+ 2 3) 4) ((+ 2 3) . 4))
(deftest funcall-null-args (funcall #'+) 0)
(deftest funcall-keys (funcall #'(lambda (&key a b) (list a b))
				       :b 3)
  (nil 3))

;;; CALL-ARGUMENTS-LIMIT
(deftest call-arguments-limit
    (typep call-arguments-limit '(integer 50)) t)

#-broken-call-arguments-limit
(defun many-parameters
     (a1 a2  a3  a4  a5  a6  a7  a8  a9  a10
     a11 a12 a13 a14 a15 a16 a17 a18 a19 a20
     a21 a22 a23 a24 a25 a26 a27 a28 a29 a30
     a31 a32 a33 a34 a35 a36 a37 a38 a39 a40
     a41 a42 a43 a44 a45 a46 a47 a48 a49)
  (declare (ignore a2 a3 a4 a5 a6 a7 a8 a9 a10
		   a11 a12 a13 a14 a15 a16 a17 a18 a19 a20
		   a21 a22 a23 a24 a25 a26 a27 a28 a29 a30
		   a31 a32 a33 a34 a35 a36 a37 a38 a39 a40
		   a41 a42 a43 a44 a45 a46 a47 a48 ))
  (values a1 a49))

#-broken-call-arguments-limit
(deftest call-arguments-limit-works
    (locally (declare (notinline many-parameters))
      (many-parameters
       1 2 3 4 5 6 7 8 9 10
       11 12 13 14 15 16 17 18 19 20
       21 22 23 24 25 26 27 28 29 30
       31 32 33 34 35 36 37 38 39 40
       41 42 43 44 45 46 47 48 49))
  1 49)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 7.4 SIMPLE SEQUENCING                                            ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PROGN
(deftest empty-sequence (progn) nil)
(deftest sequence (progn 1 (+ 1 2) 9) 9)
(deftest sequence-order
    (let (x) (list (progn (setq x 1) (setq x 2) 3) x))
  (3 2))

;;; PROG1 
(deftest prog1
    (let ((temp 1))
      (values (prog1 temp (incf temp))
	      (prog1 temp (setq temp nil))
	      temp
	      (prog1 (values 1 2 3) 4)))
  1 2 nil 1)
(deftest prog1a
    (let ((temp (list 'a 'b 'c)))
      (values (prog1 (car temp) (setf (car temp) 'alpha))
	      temp))
  a (alpha b c))
(deftest prog1b
    (flet ((swap-symbol-values (x y)
	     (setf (symbol-value x) 
	       (prog1 (symbol-value y)
		 (setf (symbol-value y) (symbol-value x))))))
      (let ((*foo* 1) (*bar* 2))
	(declare (special *foo* *bar*))
	(swap-symbol-values '*foo* '*bar*)
	(values *foo* *bar*)))
  2 1)

;;; PROG2 
(deftest prog2
    (let ((temp 1))
      (values (prog2 (incf temp) (incf temp) (incf temp))
	      temp
	      (prog2 1 (values 2 3 4) 5)))
 3 4 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 7.5 ESTABLISHING NEW VARIABLE BINDINGS                           ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LET
(deftest empty-let (let ()) nil)
(deftest empty-bindings (let () 3) 3)
(deftest bindings (let ((a 1) (b 2))
		    (list a b))
  (1 2))
(defun list-specials ()
  (declare (special a b))
  (list a b))
(deftest special-binding (let ((a 10) (b 20))
			   (declare (special a b))
			   (list-specials))
  (10 20))
(deftest special-binding2 (let ((a 10) (b 20))
			    (declare (special a b))
			    (let ((a 1)
				  (d1 (list-specials))
				  (b 2)
				  (d2 (list-specials)))
			      (declare (special a b))
			      (list d1 d2 (list-specials))))
  ((10 20) (10 20) (1 2)))
(deftest binding-order (locally (declare (special x))
			 (let ((a (setq x 1)) (b (setq x 2)))
			   (list a b x)))
  (1 2 2))
(deftest special-binding-order (locally (declare (special x))
				 (let ((a (setq x 1)) (b (setq x 2)))
				   (declare (special a b))
				   (list a b x)))
  (1 2 2))
(deftest mixed-binding-order (locally (declare (special x))
			       (let ((a (setq x 1)) (b (setq x 2)))
				 (declare (special b))
				 (list a b x)))
  (1 2 2))
(deftest mixed-binding-order2 (locally (declare (special x))
				(let ((a (setq x 1)) (b (setq x 2)))
				  (declare (special a))
				  (list a b x)))
  (1 2 2))
(deftest nested-and-empty-lets
    (let ((a 1)
	  (b 2))
      (declare (ignorable b))
      (let ()
	(let ((a 9)
	      (b a)
	      (c 3))
	  (list a b c))))
  (9 1 3))
(deftest missing-inits (let (a b) (list a b)) (nil nil))
			 
;;; LET*
(deftest empty-let* (let* ()) nil)
(deftest empty-bindings* (let* () 3) 3)
(deftest bindings* (let* ((a 1) (b 2))
		    (list a b))
  (1 2))
(deftest special-binding* (let* ((a 10) (b 20))
			   (declare (special a b))
			   (list-specials))
  (10 20))
(deftest special-binding2* (let ((a 10) (b 20))
			     (declare (special a b))
			     (let* ((a 1)
				    (d1 (list-specials))
				    (b 2)
				    (d2 (list-specials)))
			       (declare (special a b))
			       (list d1 d2 (list-specials))))
  ((1 20) (1 2) (1 2)))
(deftest binding-order* (locally (declare (special x))
			 (let* ((a (setq x 1)) (b (setq x 2)))
			   (list a b x)))
  (1 2 2))
(deftest special-binding-order* (locally (declare (special x))
				  (let* ((a (setq x 1)) (b (setq x 2)))
				    (declare (special a b))
				    (list a b x)))
  (1 2 2))
(deftest mixed-binding-order* (locally (declare (special x))
				(let* ((a (setq x 1)) (b (setq x 2)))
				  (declare (special b))
				  (list a b x)))
  (1 2 2))
(deftest mixed-binding-order2* (locally (declare (special x))
				 (let* ((a (setq x 1)) (b (setq x 2)))
				   (declare (special a))
				   (list a b x)))
  (1 2 2))
(deftest nested-and-empty-lets*
    (let* ((a 1)
	   (b 2))
      (declare (ignorable a b))
      (let* ()
	(let* ((a 9)
	       (b a)
	       (c 3))
	  (list a b c))))
  (9 9 3))
(deftest missing-inits* (let* (a b) (list a b)) (nil nil))

;;; PROGV
(deftest empty-progv (progv nil nil) nil)
(deftest no-progv-bindings (progv nil nil 3) 3)
(defun report-progv-specials ()
  (declare (special a b c))
  (list a b (if (boundp 'c) c 99)))
    
(deftest progv (progv '(a b c) '(1 2 3) (report-progv-specials))
  (1 2 3))
(deftest missing-progv-values
  (progn (makunbound 'c) (progv '(a b c) '(1 2) (report-progv-specials)))
  (1 2 99))
(deftest extra-progv-values
    (progv '(a b c) '(1 2 3 4) (report-progv-specials))
  (1 2 3))

;;; FLET
(deftest flet (flet ((+ (a b) (+ a b 1))) (+ 1 2)) 4)

;;; LABELS
(deftest labels (labels ((foo (a b)
			   (if b (bar a b 1) a))
			 (bar (a b c)
			   (list a b (foo c nil))))
		  (foo 3 2))
  (3 2 1))

;;; MACROLET
(deftest macrolet (macrolet ((foo (a b)
			       `(+ ,a (bar ,b) 1))
			     (bar (a) a))
		    (foo 1 2))
  4)

;;; SYMBOL-MACROLET
(deftest symbol-macrolet (symbol-macrolet ((pollyanna 'goody))
			   (list pollyanna (let ((pollyanna 'two-shoes))
					     pollyanna)))
  (goody two-shoes))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 7.6 CONDITIONALS                                             ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; IF
(deftest true-if (if t 1 (error "here")) 1)
(deftest false-if (if nil (error "here") 2) 2)
(deftest unused-missing-consequent (if t 1) 1)
(deftest used-missing-consequent (if nil (error "here")) nil)

;;; WHEN
(deftest when
    (values (when t 'hello)
	    (when nil 'hello)
	    (when t)
	    (let ((x 0))
	      (list (when t (incf x) (incf x) 3)
		    x))
	    (when nil (error "here"))
	    (let ((x 3))
	      (list (when (oddp x) (incf x) (list x))
		    (when (oddp x) (incf x) (list x))
		    (unless (oddp x) (incf x) (list x))
		    (unless (oddp x) (incf x) (list x))
		    (if (oddp x) (incf x) (list x)) 
		    (if (oddp x) (incf x) (list x)) 
		    (if (not (oddp x)) (incf x) (list x)) 
		    (if (not (oddp x)) (incf x) (list x)))))
  HELLO NIL NIL (3 2) NIL
  ((4) NIL (5) NIL 6 (6) 7 (7)))

;;; UNLESS
(deftest unless
    (values (unless t 'hello)
	    (unless nil 'hello)
	    (unless nil)
	    (unless t (error "here"))
	    (let ((x 0))
	      (list (unless nil (incf x) (incf x) 3)
		    x)))
  NIL HELLO NIL NIL (3 2))



;;; COND
(deftest empty-cond (cond) nil)
(deftest empty-cond-clause (cond (3)) 3)
(deftest empty-cond-clause-values (cond ((values 1 2 3))) 1)
(deftest cond-values (let ((a 0))
		       (cond ((values 1 2 3)
				(values (incf a) (incf a))
				(incf a)
				(values (incf a) a))))
  4 4)
(deftest cond-normal (let ((a 0)
			   (b 0))
		       (cond ((and (incf a) nil) (incf b))
			       ((= (incf a) 1) (incf b))
			       ((= a 2) b)
			       (nil (incf b))))
  0)
(deftest cond-none (let ((a 0)
			   (b 0))
		       (cond ((and (incf a) nil) (incf b))
			       ((= (incf a) 1) (incf b))
			       ((= a 3) b)
			       (nil (incf b))))
  nil)

;;; CASE
(deftest empty-case (case 3) nil)
;;; The usual interpretation is that this should return nil, though a
;;; case could be made that it should return (2 3).
(deftest empty-case-clause (case 2 ((1 2 3))) nil)
(deftest case-normal (let ((a 0)
			   (b 0))
		       (case (incf a)
			 (0 (incf b))
			 ((9 1) (decf b) (values (decf b) b))
			 (t (incf b)))) -2 -2)
(deftest case-t (let ((a 0)
		      (b 0))
		  (case (incf a)
		    (0 (incf b))
		    ((9 2) (incf b))
		    (t (incf b)))) 1)
(deftest case-otherwise (let ((a 0)
			      (b 0))
			  (case (incf a)
			    (0 (incf b))
			    ((9 2) (incf b))
			    (otherwise (incf b)))) 1)

;;; TYPECASE
(defun typecase-test (x)
  (let* ((counter 0)
	 (value (typecase (progn (incf counter) x)
		  (float "a float")
		  (null "a symbol ...")
		  (list "a list")
		  (t "otherwise"))))
    (and (= counter 1) value)))
(deftest typecase-nil (typecase-test nil) "a symbol ...")
(deftest typecase-list (typecase-test '(a b)) "a list")
(deftest typecase-float (typecase-test 7.0f0) "a float")
(deftest typecase-integer (typecase-test 7) "otherwise")
(deftest typecase-symbol (typecase-test 'box) "otherwise")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 7.7 BLOCKS AND EXITS                                             ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BLOCK
(deftest empty-block (block foo) nil)
(deftest normally-exited-block
    (let (a b)
      (list (block foo (setq a 2) (setq b 3) 1) a b))
  (1 2 3))

;;; RETURN-FROM
(deftest local-block
    (let (a b)
      (list (block foo (setq a 2) (setq b 3)
		   (if a
		       (return-from foo (setq b 2))
		     1))
	    a b))
  (2 2 2))
(deftest block-as-return
    (block loser
      (if (some-identity-function 3) (return-from loser 9)))
  9)
(deftest nested-inner-local-block
    (let (a b)
      (list (block foo
	      (block foo (setq a 2) (setq b 3)
		     (if a
			 (return-from foo (setq b 2))
		       1))
	      9)
	    a b))
  (9 2 2))
(deftest nested-outer-local-block
    (let (a b)
      (list (block foo
	      (block foo2 (setq a 2) (setq b 3)
		     (if a
			 (return-from foo (setq b 2))
		       1))
	      9)
	    a b))
  (2 2 2))
(deftest nonlocal-block
    (let (a b)
      (list (block foo
	      (flet ((bar (x)
		       (if x
			   (return-from foo (setq b 2))
			 1)))
		(setq a 2) (setq b 3)
		(bar a)))
	    a b))
  (2 2 2))
(deftest nested-inner-nonlocal-block
    (let (a b)
      (list (block foo
	      (block foo
		(flet ((bar (x)
			 (if x
			     (return-from foo (setq b 2))
			   1)))
		  (setq a 2) (setq b 3)
		  (bar a)))
	      9)
	    a b))
  (9 2 2))
(deftest nested-outer-nonlocal-block
    (let (a b)
      (list (block foo
	      (block foo2
		(flet ((bar (x)
			 (if x
			     (return-from foo (setq b 2))
			   1)))
		  (setq a 2) (setq b 3)
		  (bar a)))
	      9)
	    a b))
  (2 2 2))
(deftest nested-outer-double-nonlocal-block
    (let (a b)
      (list (block foo
	      (block foo2
		(flet ((bar (x)
			 (if x
			     (return-from foo (setq b 2))
			   1)))
		  (setq a 2) (setq b 3)
		  (flet ((something (a) (bar a))) (something a))))
	      9)
	    a b))
  (2 2 2))

(defun trouble (exit) (block foo (funcall exit)))
(deftest block-labels-not-dynamic
  (let ((should-be-clear nil))
    (list (block foo
	    (trouble #'(lambda () (return-from foo 9)))
	    (setq should-be-clear t))
	  should-be-clear))
  (9 nil))

(deftest block-breaks-up-catchers
    (block loser
      (catch 'stuff
	(if (some-identity-function 3) (return-from loser 9))))
  9)

(deftest block-breaks-up-catchers2
    (catch 'stuff
      (throw 'stuff
	(block loser
	  (catch 'stuff
	    (return-from loser 3))
	  9))
      4)
  3)
(deftest return-from-throw
    (list (catch 'foo
	    (let ((a 1))
	      (block foo
		(throw 'foo (if a (return-from foo a) 99))))))
  (1))

(deftest return-from-throw-mvc
    (multiple-value-call #'+
      (values 2 3)
      (catch 'foo
	    (let ((a 1))
	      (block foo
		(throw 'foo (if a (return-from foo a) 99)))))
      4)
  10)
(deftest nonlocal-return-from-throw-mvc
    (multiple-value-call #'+
      (values 2 3)
      (catch 'foo
	    (let ((a 1))
	      (block foo
		(throw 'foo (flet ((returner (x)
				     (if a (return-from foo a) x)))
			      (declare (notinline returner))
			      (returner 99))))))
      4)
  10)

(deftest return-from-mvc
    (list (let ((a 1))
	    (block foo
	      (multiple-value-call #'+
		(values 1 2)
		(if a (return-from foo a) 99)))))
  (1))

(deftest return-from-mvc-mvc
    (multiple-value-call #'+
      (values 2 3)
      (let ((a 1))
	(block foo
	  (multiple-value-call #'+
	    (values 1 2)
	    (if a (return-from foo a) 99))))
      4)
  10)

(deftest return-from-mvc-mvp
    (multiple-value-call #'+
      (values 2 3)
      (let ((a 1))
	(block foo
	  (multiple-value-prog1
	    (values 1 2)
	    (if a (return-from foo a) 99))))
      4)
  10)

(deftest return-from-mvc-mvp-captured
    (multiple-value-call #'+
      (values 2 3)
      (let ((a 1))
	(block foo
	  (multiple-value-prog1
	    (values 1 2)
	    (flet ((silly (a)
		     (if a (return-from foo a) 99)))
	      (declare (notinline silly))
	      (silly a)))))
      4)
  10)

(deftest return-from-mvc-captured-mvp
    (multiple-value-call #'+
      (values 2 3)
      (let ((a 1))
	(block foo
	  (flet ((silly (a)
		   (multiple-value-prog1
		       (values 1 2)
		     (if a (return-from foo a) 99))))
	    (declare (notinline silly))
	    (silly a))))
      4)
  10)

(deftest return-from-mvc-exit
    (multiple-value-call #'+
      (values 2 3)
      (let ((a 1))
	(block foo
	  (flet ((silly (a)
		   (multiple-value-prog1
		       (values 1 2)
		     (if a (return-from silly a) 99))))
	    (declare (notinline silly))
	    (silly a))))
      4)
  10)


;;; RETURN
(deftest RETURN
    (let ((x 0))
      (values (block nil (return 9) (incf x)) x))
  9 0)
      
