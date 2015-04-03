;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 5.1 FORMS                                                        ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 5.1.1 SELF-EVALUATING FORMS
(deftest self-eval-number 1 1)
(deftest self-eval-symbol 'foo foo)
(deftest self-eval-keyword :keyword :keyword)
(deftest self-eval-nil nil nil)
;;; 5.1.2 VARIABLES
(deftest eval-special (locally (declare (special items))
			(setq items 3) items) 3)
(deftest eval-lexical (let (items) (setq items 4) items) 4)
;;; 5.1.5 FUNCTION CALLS
(deftest nest-function (+ 1 (+ 2 3) 4) 10)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 5.2 FUNCTIONS                                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 5.2.2 Lambda-Expressions
;;; Examples of &optional and &rest parameters:
(deftest required ((lambda (a b) (+ a (* b 3))) 4 5) 19)
(deftest required-and-optional-supplied
    ((lambda (a &optional (b 2)) (+ a (* b 3))) 4 5)
  19)
(deftest required-and-optional-missing
    ((lambda (a &optional (b 2)) (+ a (* b 3))) 4)
  10)
(deftest supplied-and-rest-none
    ((lambda (&optional (a 2 b) (c 3 d) &rest x)
       (list a (not (null b)) c (not (null d)) x)))
  (2 nil 3 nil nil))
(deftest supplied-and-rest-one
    ((lambda (&optional (a 2 b) (c 3 d) &rest x)
       (list a (not (null b)) c (not (null d)) x))
     6)
  (6 t 3 nil nil))
(deftest supplied-and-rest-two
    ((lambda (&optional (a 2 b) (c 3 d) &rest x)
       (list a (not (null b)) c (not (null d)) x))
     6 3)
  (6 t 3 t nil))
(deftest supplied-and-rest-three
    ((lambda (&optional (a 2 b) (c 3 d) &rest x)
       (list a (not (null b)) c (not (null d)) x))
     6 3 8)
  (6 t 3 t (8)))
(deftest supplied-and-rest-all
    ((lambda (&optional (a 2 b) (c 3 d) &rest x)
       (list a (not (null b)) c (not (null d)) x))
     6 3 8 9 10 11)
  (6 t 3 t (8 9 10 11)))

;;; Again...
(deftest flet-required (flet ((foo (a b) (+ a (* b 3))))
			 (declare (notinline foo))
			 (foo 4 5))
  19)
(deftest flet-required-and-optional-supplied
    (flet ((foo (a &optional (b 2)) (+ a (* b 3))))
      (declare (notinline foo))
      (foo 4 5))
  19)
(deftest flet-required-and-optional-missing
    (flet ((foo (a &optional (b 2)) (+ a (* b 3))))
      (declare (notinline foo))
      (foo 4))
  10)
(deftest flet-supplied-and-rest-none
    (flet ((foo (&optional (a 2 b) (c 3 d) &rest x)
		(list a (not (null b)) c (not (null d)) x)))
      (declare (notinline foo))
      (foo))
  (2 nil 3 nil nil))
(deftest flet-supplied-and-rest-one
    (flet ((foo (&optional (a 2 b) (c 3 d) &rest x)
		(list a (not (null b)) c (not (null d)) x)))
      (declare (notinline foo))
      (foo 6))
  (6 t 3 nil nil))
(deftest flet-supplied-and-rest-two
    (flet ((foo (&optional (a 2 b) (c 3 d) &rest x)
		(list a (not (null b)) c (not (null d)) x)))
      (declare (notinline foo))
      (foo 6 3))
  (6 t 3 t nil))
(deftest flet-supplied-and-rest-three
    (flet ((foo (&optional (a 2 b) (c 3 d) &rest x)
		(list a (not (null b)) c (not (null d)) x)))
      (declare (notinline foo))
      (foo 6 3 8))
  (6 t 3 t (8)))
(deftest flet-supplied-and-rest-all
    (flet ((foo (&optional (a 2 b) (c 3 d) &rest x)
		(list a (not (null b)) c (not (null d)) x)))
      (declare (notinline foo))
      (foo 6 3 8 9 10 11))
  (6 t 3 t (8 9 10 11)))


;;; Examples of &key parameters:
(deftest key-none
    ((lambda (a b &key c d) (list a b c d))
     1 2)
  (1 2 nil nil))
(deftest key-first
    ((lambda (a b &key c d) (list a b c d))
     1 2 :c 6)
  (1 2 6 nil))
(deftest key-second
    ((lambda (a b &key c d) (list a b c d))
     1 2 :d 8)
  (1 2 nil 8))
(deftest key-both
    ((lambda (a b &key c d) (list a b c d))
     1 2 :c 6 :d 8)
  (1 2 6 8))
(deftest key-reversed
    ((lambda (a b &key c d) (list a b c d))
     1 2 :d 8 :c 6)
  (1 2 6 8))
(deftest key-key-as-required-value
    ((lambda (a b &key c d) (list a b c d))
     :a 1 :d 8 :c 6)
  (:a 1 6 8))
(deftest key-key-as-key-value
    ((lambda (a b &key c d) (list a b c d))
     :a :b :c :d)
  (:a :b :d nil))

;;; Again...
(deftest flet-key-none
    (flet ((foo (a b &key c d) (list a b c d)))
      (declare (notinline foo))
      (foo 1 2))
  (1 2 nil nil))
(deftest flet-key-first
    (flet ((foo (a b &key c d) (list a b c d)))
      (declare (notinline foo))
      (foo 1 2 :c 6))
  (1 2 6 nil))
(deftest flet-key-second
    (flet ((foo (a b &key c d) (list a b c d)))
      (declare (notinline foo))
      (foo 1 2 :d 8))
  (1 2 nil 8))
(deftest flet-key-both
    (flet ((foo (a b &key c d) (list a b c d)))
      (declare (notinline foo))
      (foo 1 2 :c 6 :d 8))
  (1 2 6 8))
(deftest flet-key-reversed
    (flet ((foo (a b &key c d) (list a b c d)))
      (declare (notinline foo))
      (foo 1 2 :d 8 :c 6))  
  (1 2 6 8))
(deftest flet-key-key-as-required-value
    (flet ((foo (a b &key c d) (list a b c d)))
      (declare (notinline foo))
      (foo :a 1 :d 8 :c 6))
  (:a 1 6 8))
(deftest flet-key-key-as-key-value
    (flet ((foo (a b &key c d) (list a b c d)))
      (declare (notinline foo))
      (foo :a :b :c :d))
  (:a :b :d nil))

;;; Examples of mixtures:
(deftest mixed-args-one
    ((lambda (a &optional (b 3) &rest x &key c (d a)) (list a b c d x))
     1)
  (1 3 nil 1 nil))
(deftest mixed-args-two
    ((lambda (a &optional (b 3) &rest x &key c (d a)) (list a b c d x))
     1 2)
  (1 2 nil 1 nil))
(deftest mixed-args-as-req-and-opt
    ((lambda (a &optional (b 3) &rest x &key c (d a)) (list a b c d x))
     :c 7)
  (:c 7 nil :c nil))
(deftest mixed-args-key-first
    ((lambda (a &optional (b 3) &rest x &key c (d a)) (list a b c d x))
     1 6 :c 7)
  (1 6 7 1 (:c 7)))
(deftest mixed-args-key-second
    ((lambda (a &optional (b 3) &rest x &key c (d a)) (list a b c d x))
     1 6 :d 8)
  (1 6 nil 8 (:d 8)))
(deftest mixed-args-key-reversed-and-suplicated
    ((lambda (a &optional (b 3) &rest x &key c (d a)) (list a b c d x))
     1 6 :d 8 :c 9 :d 10)
  (1 6 9 8 (:d 8 :c 9 :d 10)))

;;; CMU dodn't handle non-keyword "key-names".
(defun wager (&key ((#-cmu secret
		     #+cmu :secret
		       password) nil) amount)
  (list (if (eq password 'joe-sent-me) "win" "lose")
	amount))
(deftest symbol-keywords
  (values (wager :amount 100)
	  (wager :amount 100
		 #-cmu 'secret
		 #+cmu :secret
		 'joe-sent-me))
  ("lose" 100) ("win" 100))
   

;;; Again
(deftest flet-mixed-args-one
    (flet ((foo (a &optional (b 3) &rest x &key c (d a)) (list a b c d x)))
      (declare (notinline foo))
      (foo 1))
  (1 3 nil 1 nil))
(deftest flet-mixed-args-two
    (flet ((foo (a &optional (b 3) &rest x &key c (d a)) (list a b c d x)))
      (declare (notinline foo))
      (foo 1 2))
  (1 2 nil 1 nil))
(deftest flet-mixed-args-as-req-and-opt
    (flet ((foo (a &optional (b 3) &rest x &key c (d a)) (list a b c d x)))
      (declare (notinline foo))
      (foo :c 7))
  (:c 7 nil :c nil))
(deftest flet-mixed-args-key-first
    (flet ((foo (a &optional (b 3) &rest x &key c (d a)) (list a b c d x)))
      (declare (notinline foo))
      (foo 1 6 :c 7))
  (1 6 7 1 (:c 7)))
(deftest flet-mixed-args-key-second
    (flet ((foo (a &optional (b 3) &rest x &key c (d a)) (list a b c d x)))
      (declare (notinline foo))
      (foo 1 6 :d 8))
  (1 6 nil 8 (:d 8)))
(deftest flet-mixed-args-key-reversed-and-suplicated
    (flet ((foo (a &optional (b 3) &rest x &key c (d a)) (list a b c d x)))
      (declare (notinline foo))
      (foo 1 6 :d 8 :c 9 :d 10))
  (1 6 9 8 (:d 8 :c 9 :d 10)))

;;; Examples of aux:
(deftest aux1
    ((lambda (&aux x) (list x)))
  (nil))
(deftest aux2
    ((lambda (&aux x y) (list x y)))
  (nil nil))
(deftest aux-empty-default
    ((lambda (&aux (x)) (list x)))
  (nil))
(deftest aux-default
    ((lambda (&aux (x 9)) (list x)))
  (9))
(deftest aux-default-first
    ((lambda (&aux (x 9) y) (list x y)))
  (9 nil))
(deftest aux-default-second
    ((lambda (&aux x (y x)) (list x y)))
  (nil nil))
(deftest aux-default-both
    ((lambda (&aux (x 9) (y x)) (list x y)))
  (9 9))

;;; Again
(deftest flet-aux1
    (flet ((foo (&aux x) (list x)))
      (declare (notinline foo))
      (foo))
  (nil))
(deftest flet-aux2
    (flet ((foo (&aux x y) (list x y)))
      (declare (notinline foo))
      (foo))
  (nil nil))
(deftest flet-aux-empty-default
    (flet ((foo (&aux (x)) (list x)))
      (declare (notinline foo))
      (foo))
  (nil))
(deftest flet-aux-default
    (flet ((foo (&aux (x 9)) (list x)))
      (declare (notinline foo))
      (foo))
  (9))
(deftest flet-aux-default-first
    (flet ((foo (&aux (x 9) y) (list x y)))
      (declare (notinline foo))
      (foo))
  (9 nil))
(deftest flet-aux-default-second
    (flet ((foo (&aux x (y x)) (list x y)))
      (declare (notinline foo))
      (foo))
  (nil nil))
(deftest flet-aux-default-both
    (flet ((foo (&aux (x 9) (y x)) (list x y)))
      (declare (notinline foo))
      (foo))
  (9 9))


;;; ALLOW-OTHER-KEYS
(deftest empty-key
    ((lambda (&key &aux (x 9)) x))
  9)

;; Franz can't handle this.
;#-(and excl (not eclipse))
(progn
  (deftest allow-other-keys-key
      ((lambda (&key &aux (x 9)) x)
       :allow-other-keys 3)
    9)
  (deftest allow-other-keys-key2
      ((lambda (&key (x 9)) x)
       :x 8 :allow-other-keys 3)
    8)
  (deftest allow-other-keys-key2-reversed
      ((lambda (&key (x 9)) x)
       :allow-other-keys 3
       :x 8)
    8))

;; CMU can't handle this.
#-cmu
(deftest allow-other-keys-lambda-key0
  ((lambda (&key &allow-other-keys &aux (x 9)) x)
     :y 3)
  9)
(deftest allow-other-keys-lambda-key1
    ((lambda (&key (x 9) &allow-other-keys) x)
     :y 3)
  9)
(deftest allow-other-keys-lambda-key2
    ((lambda (&key (x 9) &allow-other-keys) x)
     :y 3 :x 8)
  8)
(deftest allow-other-keys-lambda-key-and-lambda
    ((lambda (&key (x 9) &allow-other-keys) x)
     :y 3
     :allow-other-keys 2
     :x 8)
  8)

;;; Again...
(deftest flet-empty-key
    (flet ((foo (&key &aux (x 9)) x))
      (declare (notinline foo))
      (foo))
  9)
(deftest flet-allow-other-keys-key
    (flet ((foo (&key &aux (x 9)) x))
      (declare (notinline foo))
      (foo :allow-other-keys 3))
  9)
(deftest flet-allow-other-keys-key2
    (flet ((foo (&key (x 9)) x))
      (declare (notinline foo))
      (foo :x 8 :allow-other-keys 3))
  8)
(deftest flet-allow-other-keys-key2-reversed
    (flet ((foo (&key (x 9)) x))
      (declare (notinline foo))
      (foo :allow-other-keys 3 :x 8))
  8)


#-cmu
(deftest flet-allow-other-keys-lambda-key0
    (flet ((foo (&key &allow-other-keys &aux (x 9)) x))
      (declare (notinline foo))
      (foo :y 3))
  9)
(deftest flet-allow-other-keys-lambda-key1
    (flet ((foo (&key (x 9) &allow-other-keys) x))
      (declare (notinline foo))
      (foo :y 3))
  9)
(deftest flet-allow-other-keys-lambda-key2
    (flet ((foo (&key (x 9) &allow-other-keys) x))
      (declare (notinline foo))
      (foo :y 3 :x 8))
  8)
(deftest flet-allow-other-keys-lambda-key-and-lambda
    (flet ((foo (&key (x 9) &allow-other-keys) x))
      (declare (notinline foo))
      (foo :y 3
	   :allow-other-keys 2
	   :x 8))
  8)

;;; LAMBDA-LIST-KEYWORDS
(deftest lambda-list-keywords
    (set-difference '(&optional &rest &key &allow-other-keys &aux &body &whole
		      &environment) lambda-list-keywords) nil)

;;; LAMBDA-PARAMETERS-LIMIT
(deftest lambda-parameters-limit (typep lambda-parameters-limit '(integer 50)) t)

;;; LAMBDA
;; #-(and cmu (not eclipse))
(deftest lambda-macro
    (funcall (lambda (x) (+ x 3)) 4)
  7)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 5.3 TOP-LEVEL FORMS                                          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 5.3.1 Defining Named Functions
;;; DEFUN

(defun redefined () 1)
(defun redefined () 2)
(deftest defun-redefine (funcall #'redefined) 2)
(defun fun-with-block (a) (when a (return-from fun-with-block 3)) 9)
(deftest defun-implicit-block (fun-with-block t) 3)
(let ((counter 0))
  (defun lex-counter () "some doc" (incf counter))
  (defun (setf lex-counter) (new random)
    (setq counter new)
    random))
(deftest defun-fdef (equal #'lex-counter (fdefinition 'lex-counter)) t)
(deftest defun-fdef2 (equal (function (cl:setf lex-counter)) (fdefinition '(setf lex-counter))) t)
(deftest defun-nontop
    (values (lex-counter)
	    (lex-counter)
	    (setf (lex-counter 42) 9)
	    (lex-counter)
	    (lex-counter)
	    (setf (lex-counter -1) 0))
  1 2 42 10 11 -1)

;;; 5.3.1 Declaring Global Variables and Named Constants
;;; These tests do not show when the values get evaluated!
;;; DEFVAR
(defvar *an-unbound-variable*)
(defvar *a-bound-variable* 9)
(defvar *a-bound-variable* (1- *a-bound-variable*) #-boostrap "some doc")
(deftest defvar-no-init (boundp '*an-unbound-variable*) nil)
(deftest defvar
    (flet ((get-var (new)
	     (prog1 *a-bound-variable*
	       (setq *a-bound-variable* new))))
      (list (let ((*a-bound-variable* 42))
	      (list (get-var 100) *a-bound-variable*))
	    (constantp '*a-bound-variable*)
	    *a-bound-variable*))
  ((42 100) nil 9))
		
;;; DEFPARAMETER
(defparameter *a-bound-parameter* 9)
(defparameter *a-bound-parameter* (1- *a-bound-parameter*) #-bootstrap "some doc")
(deftest defparameter
    (flet ((get-var (new)
	     (prog1 *a-bound-parameter*
	       (setq *a-bound-parameter* new))))
      (list (let ((*a-bound-parameter* 42))
	      (list (get-var 100) *a-bound-parameter*))
	    (constantp '*a-bound-parameter*)
	    *a-bound-parameter*))
  ((42 100) nil 8))

;;; DEFCONSTANT
(defconstant *a-bound-constant* 9)
(defconstant *a-bound-constant* 9 #-bootstrap "some doc")
(deftest defconstant (and (constantp '*a-bound-constant*) *a-bound-constant*) 9)
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 5.3.3 CONTROL OF TIME OF EVALUATION                          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EVAL-WHEN
(deftest eval-when-empty
    (eval-when ())
  nil)
(deftest eval-when-none
    (eval-when () 3)
  nil)
(deftest eval-when-definately-not-top
    (let () (eval-when (:compile-toplevel :load-toplevel) 3))
  nil)
(deftest eval-when-not-top-value
    (let () (eval-when (:execute) 3))
  3)
