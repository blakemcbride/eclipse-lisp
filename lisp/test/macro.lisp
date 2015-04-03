;;; This function converts the environment format used by the compiler
;;; operating on this file, to the format used by the implementation
;;; of macro-function, macroexpand, etc.  Under normal circumstances,
;;; this is should be the identity function.  When testing out one
;;; Lisp implementation while bootstrapping off the compiler of
;;; another, this needs to be redefined.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun convert-env (lisp-env)
    #-lisp-host lisp-env
    #+lisp-host
    ;; Using the native Lisp compiler on eclipse definitions...
    (when lisp-env
      #+cmu
      (labels ((convt (functions variables env)
		      (cond (functions
			     (convt (rest functions)
				    variables
				    (destructuring-bind (name . f)
					(pop functions)
				      (if (consp f)
					  (augment-environment
					   env :macro `((,name ,(cdr f))))
					(augment-environment
					 env :function `(,name))))))
			    (variables
			     (convt functions
				    (rest variables)
				    (destructuring-bind (name . v)
					(pop variables)
				      (if (consp v)
					  (augment-environment
					   env :symbol-macro `((,name ,(cdr v))))
					(augment-environment
					 env :variable `(,name))))))
			    (t env))))
	(convt (reverse (c::lexenv-functions lisp-env))
	       (reverse (c::lexenv-variables lisp-env))
	       nil))
      #+allegro-v4.1
      (labels
	  ((convt (env)
		  (when env
		    (let ((rest (convt (rest env))))
		      (cl:destructuring-bind (name type . def) (car env)
					     (cl:case type
						      (COMPILER::FUNCTION-VALUE
						       (augment-environment rest :function (list name)))
						      ;; Franz bug!!!
						      ;; lexical variables don't show up in compiler
						      ;; lexical environment, so you can't tell if a
						      ;; symbol-macro is getting shadowed.
						      (EXCL::%SYMBOL-MACROLET-MARKER%
						       (augment-environment rest
									    :symbol-macro
									    (list (list name def))))
						      (EXCL::MACRO
						       (augment-environment rest
									    :macro
									    (list (list name
											def))))
						      ))))))
	(convt (cond ((null (car lisp-env)) (cdr lisp-env))
		     ((null (cdr lisp-env)) (car lisp-env))
		     (t (error "Unrecognized env ~s." lisp-env))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 8.1 MACRO DEFINITION                                         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MACRO-FUNCTION
(defmacro macfun () '(macro-function 'macfun))
(deftest global-macro-function (not (macro-function 'macfun)) nil)
(deftest macro-function
    (macrolet ((foo (&environment env)
		 (if (macro-function 'bar (convert-env env))
		     ''yes
		   ''no)))
      (list (foo)
	    (macrolet ((bar () :beep))
	      (foo))))
  (no yes))
 
;;; DEFMACRO
(defmacro mac1 (a b)
  "Mac1 multiplies and adds" 
  `(+ ,a (* ,b 3)))
(deftest defmacro1 (mac1 4 5) 19 )
(deftest defmacro-doc (documentation 'mac1 'function) "Mac1 multiplies and adds")

(defmacro mac2 (&optional (a 2 b) (c 3 d) &rest x)
  `'(,a ,(not (null b)) ,c ,(not (null d)) ,x))
(deftest defmacro2 (mac2 6) (6 T 3 NIL NIL) )
(deftest defamcro2a (mac2 6 3 8) (6 T 3 T (8)) )

(defmacro mac3 (&whole r a &optional (b 3) &rest x &key c (d a))
    `'(,r ,a ,b ,c ,d ,x))
(deftest defmacro3
  (mac3 1 6 :d 8 :c 9
	#-(and cmu (or hm (not eclipse))) :d #-(and cmu (or hm (not eclipse))) 10)
  ((MAC3 1 6 :D 8 :C 9 :D 10) 1 6 9 8 (:D 8 :C 9 :D 10)) )

(defmacro dm1a (&whole x) `',x)
(deftest dm1a (macroexpand '(dm1a)) (QUOTE (DM1A)) t)
(defmacro dm1b (&whole x a &optional b) `'(,x ,a ,b))
(deftest dm1b1 (macroexpand '(dm1b q))  (QUOTE ((DM1B Q) Q NIL)) t)
(deftest dm1b2 (macroexpand '(dm1b q r)) (QUOTE ((DM1B Q R) Q R)) t)

(defmacro dm2a (&whole form a b) `'(form ,form a ,a b ,b))
(deftest dm2a1 (macroexpand '(dm2a x y))  (QUOTE (FORM (DM2A X Y) A X B Y)) t)
(deftest dm2a2 (dm2a x y) (FORM (DM2A X Y) A X B Y))

(defmacro dm2b (&whole form a (&whole b (c . d) &optional (e 5)) 
		&body f &environment env)
  ``(,',form ,,a ,',b ,',(macroexpand c (convert-env env)) ,',d ,',e ,',f))
(deftest dm2b
    (let ((x1 5))
      (macrolet ((segundo (x) `(cadr ,x)))
	(dm2b x1 (((segundo x2) x3 x4)) x5 x6)))
 ((DM2B X1 (((SEGUNDO X2) X3 X4)) X5 X6)
  5 (((SEGUNDO X2) X3 X4)) (CADR X2) (X3 X4) 5 (X5 X6)))

(defmacro dm-dotted (a &environment e . b)
  (declare (ignore e))
  `(frob ,a 99 ,@b))

(deftest dm-dotted (values (macroexpand-1 '(dm-dotted x y z))) (frob x 99 y z))

(defmacro dm-dot args `(frob ,@args))

(deftest dm-dot (values (macroexpand-1 '(dm-dot x y z))) (frob x y z))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 8.2 MACRO EXPANSION                                         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro alpha (x y) `(beta ,x ,y))
(defmacro beta (x y) `(gamma ,x ,y))
(defmacro delta (x y) `(gamma ,x ,y))
(defmacro test-expand (form &environment env)
  (multiple-value-bind (expansion expanded-p)
      (macroexpand form (convert-env env))
    `(values ',expansion ',expanded-p)))
(defmacro test-expand-1 (form &environment env)
   (multiple-value-bind (expansion expanded-p)
       (macroexpand-1 form (convert-env env))
     `(values ',expansion ',expanded-p)))

;;; MACROEXPAND, MACROEXPAND-1
(deftest macroexpand-global-1-alpha (macroexpand-1 '(alpha a b))  (BETA A B) t)
(deftest macroexpand-global-expand1-alpha (test-expand-1 (alpha a b)) (BETA A B) t)
(deftest macroexpand-global-alpha (macroexpand '(alpha a b)) (GAMMA A B) t)
(deftest macroexpand-global-expand-alpha (test-expand (alpha a b))  (GAMMA A B) t)
(deftest macroexpand-global-1 (macroexpand-1 'not-a-macro)  NOT-A-MACRO nil)
(deftest macroexpand-global-expand1 (test-expand-1 not-a-macro) NOT-A-MACRO nil)
(deftest macroexpand-global (macroexpand '(not-a-macro a b)) (NOT-A-MACRO A B) nil)
(deftest macroexpand-global-expand (test-expand (not-a-macro a b)) (NOT-A-MACRO A B) nil)

(deftest macroexpand-1-alpha
    (macrolet ((alpha (x y) `(delta ,x ,y)))
      (macroexpand-1 '(alpha a b)))
  (BETA A B) t)
(deftest macroexpand-expand1-alpha
    (macrolet ((alpha (x y) `(delta ,x ,y)))
      (test-expand-1 (alpha a b)))
  (DELTA A B) t)
(deftest macroexpand-alpha
    (macrolet ((alpha (x y) `(delta ,x ,y)))
      (macroexpand '(alpha a b)))
  (GAMMA A B) t)
(deftest macroexpand-expand-alpha
    (macrolet ((alpha (x y) `(delta ,x ,y)))
      (test-expand (alpha a b)))
  (GAMMA A B) t)
(deftest macroexpand-expand-beta
    (macrolet ((beta (x y) `(epsilon ,x ,y)))
      (test-expand (alpha a b)))
  (EPSILON A B) t)
(deftest macroexpand-expand-a
    (let ((x (list 1 2 3)))
      (declare (ignorable x))
      (symbol-macrolet ((a (first x)))
	(test-expand a)))
  (FIRST X) t)
(deftest macroexpand-a
    (let ((x (list 1 2 3)))
      (declare (ignorable x))
      (symbol-macrolet ((a (first x)))
	(macroexpand 'a)))
  A nil)
(deftest macroexpand-expand1-b
    (symbol-macrolet ((b (alpha x y)))
      (test-expand-1 b))
  (ALPHA X Y) t)
(deftest macroexpand-expand-b
    (symbol-macrolet ((b (alpha x y)))
      (test-expand b))
  (GAMMA X Y) t)
(deftest macroexpand-expand1-ba
    (symbol-macrolet ((b (alpha x y))
		      (a b))
      (test-expand-1 a))
  B t)
(deftest macroexpand-expand-ba
    (symbol-macrolet ((b (alpha x y))
		      (a b))
      (test-expand a))
  (GAMMA X Y) t)

(deftest macroexpand-shadow-beta
    (flet ((beta (x y) (+ x y)))
      (test-expand (alpha a b)))
  (BETA A B) t)
(deftest macroexpand-shadow-alpha
    (macrolet ((alpha (x y) `(delta ,x ,y)))
      (flet ((alpha (x y) (+ x y)))
	(test-expand (alpha a b))))
  (ALPHA A B) nil)
(deftest macroexpand-shadow-a
    (let ((x (list 1 2 3)))
      (symbol-macrolet ((a (first x)))
	(let ((a x))
	  (declare (ignorable a))
	  (test-expand a))))
  A nil)

;;; *MACROEXPAND-HOOK*
(defparameter *expansions* nil)

;;; we don't wan't these interpreted, and then
;;; have to call them during their own interpretation.
#-(and eclipse (not lisp-host) (not machine-compile))
(progn
(defun hook (expander form env)
  (push form *expansions*)
  (funcall expander form env))
(defmacro machook (x y) `(/ (+ ,x ,y) 2)))

(deftest macroexpand-hook
    (let ((*macroexpand-hook* #'hook)
	  (*expansions* nil))
      (values (macroexpand '(machook 1 2))
	      *expansions*))
  (/ (+ 1 2) 2) ((MACHOOK 1 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 8.3 DESTRUCTURING                                            ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DESTRUCTURING-BIND
;;; Should include tests for other lambda-keywords, default values, etc!!!
;;; Should include tests with declarations!!!
(deftest destructure-halibut
    (destructuring-bind ((mouth eye1 eye2)
			 ((fin1 length1) (fin2 length2))
			 tail)
	'((m (car eyes) (cdr eyes))
	  ((f1 (count-scales f1)) (f2 (count-scales f2)))
	  my-favorite-tail)
      (values mouth eye1 eye2 fin1 length1 fin2 length2 tail))
  m (car eyes) (cdr eyes) f1 (count-scales f1) f2 (count-scales f2)
  my-favorite-tail)
(deftest destructure-whole-halibut
    (destructuring-bind ((&whole head mouth eye1 eye2)
			 ((fin1 length1) (fin2 length2))
			 tail)
	'((m (car eyes) (cdr eyes))
	  ((f1 (count-scales f1)) (f2 (count-scales f2)))
	  my-favorite-tail)
      (values head mouth eye1 eye2 fin1 length1 fin2 length2 tail))
  (m (car eyes) (cdr eyes))
  m (car eyes) (cdr eyes) f1 (count-scales f1) f2 (count-scales f2)
  my-favorite-tail)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 8.4 COMPILER MACROS                                          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DEFINE-COMPILER-MACRO, COMPILER-MACRO-FUNCTION
;;; Need test to show shadowing, and proper passing of environment!!!
(defun square (x) (expt x 2))
(define-compiler-macro square (&whole form arg)
  (if (atom arg)
      `(expt ,arg 2)
    (case (car arg)
      (square (if (= (length arg) 2)
		  `(expt ,(nth 1 arg) 4)
		form))
      (expt   (if (= (length arg) 3)
		  (if (numberp (nth 2 arg))
		      `(expt ,(nth 1 arg) ,(* 2 (nth 2 arg)))
		    `(expt ,(nth 1 arg) (* 2 ,(nth 2 arg))))
		form))
      (otherwise `(expt ,arg 2)))))

(deftest compiler-macro-call (square (square 3)) 81)
(deftest compiler-macro-macroexpand
    (macroexpand '(square x))  (SQUARE X) nil)
(deftest compiler-macro-expand-1
    (funcall (compiler-macro-function 'square) '(square x) nil)
  (EXPT X 2))
(deftest compiler-macro-expand-2
    (funcall (compiler-macro-function 'square) '(square (square x)) nil)
  (EXPT X 4))
#-(and cmu (or hm (not eclipse)))
(deftest compiler-macro-funcall-expand
    (funcall (compiler-macro-function 'square)
	     '(funcall #'square x) nil)
  (EXPT X 2))
