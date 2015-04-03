;;; 6.2.1 SPECIFIC TYPE PREDICATES
;;; NULL
(deftest null-nil (null nil) t)
(deftest null-3 (null 3) nil)

;;; SYMBOLP
(deftest symbolp-symb (symbolp 'foo) t)
(deftest symbolp-key (symbolp :foo) t)
(deftest symbolp-nil (symbolp nil) t)
(deftest symbolp-3 (symbolp 3) nil)

;;; ATOM
(deftest atom-nil (atom nil) t)
(deftest atom-cons (atom '(1 . 2)) nil)
(deftest atom-3 (atom 3) t)

;;; CONSP
(deftest consp-nil (consp nil) nil)
(deftest consp-cons (consp '(1 . 2)) t)
(deftest consp-3 (consp 3) nil)

;;; LISTP
(deftest listp-nil (listp nil) t)
(deftest listp-cons (listp '(1 . 2)) t)
(deftest listp-3 (listp 3) nil)

;;; NUMBERP
(deftest numberp-fix (numberp 3) t)
(deftest numberp-big (numberp #xffffffff) t)
(deftest numberp-real (numberp 1.0f0) t)
(deftest numberp-double (numberp 1.0d0) t)
(deftest numberp-short (numberp 1.0s0) t)
(deftest numberp-long (numberp 1.0l0) t)
(deftest numberp-rat (numberp 1/2) t)
(deftest numberp-comp (numberp #c(1 2)) t)
(deftest numberp-comp2 (numberp #c(1.0d0 2)) t)
(deftest numberp-nil (numberp nil) nil)

;;; INTEGERP
(deftest integerp-fix (integerp 3) t)
(deftest integerp-big (integerp #xffffffff) t)
(deftest integerp-float (integerp 1.0f0) nil)
(deftest integerp-rat (integerp 1/2) nil)
(deftest integerp-comp (integerp #c(1 2)) nil)
(deftest integerp-nil (integerp nil) nil)

;;; RATIONALP
(deftest rationalp-fix (rationalp 3) t)
(deftest rationalp-big (rationalp #xffffffff) t)
(deftest rationalp-float (rationalp 1.0f0) nil)
(deftest rationalp-rat (rationalp 1/2) t)
(deftest rationalp-comp (rationalp #c(1 2)) nil)
(deftest rationalp-nil (rationalp nil) nil)

;;; FLOATP
(deftest floatp-fix (floatp 3) nil)
(deftest floatp-big (floatp #xffffffff) nil)
(deftest floatp-float (floatp 1.0f0) t)
(deftest floatp-double (floatp 1.0d0) t)
(deftest floatp-short (floatp 1.0s0) t)
(deftest floatp-long (floatp 1.0l0) t)
(deftest floatp-rat (floatp 1/2) nil)
(deftest floatp-comp (floatp #c(1 2)) nil)
(deftest floatp-nil (floatp nil) nil)

;;; REALP
(deftest realp-fix (realp 3) t)
(deftest realp-big (realp #xffffffff) t)
(deftest realp-real (realp 1.0f0) t)
(deftest realp-double (realp 1.0d0) t)
(deftest realp-short (realp 1.0s0) t)
(deftest realp-long (realp 1.0l0) t)
(deftest realp-rat (realp 1/2) t)
(deftest realp-comp (realp #c(1 2)) nil)
(deftest realp-nil (realp nil) nil)

;;; COMPLEXP
(deftest complexp-fix (complexp 3) nil)
(deftest complexp-big (complexp #xffffffff) nil)
(deftest complexp-real (complexp 1.0f0) nil)
(deftest complexp-double (complexp 1.0d0) nil)
(deftest complexp-short (complexp 1.0s0) nil)
(deftest complexp-long (complexp 1.0l0) nil)
(deftest complexp-rat (complexp 1/2) nil)
(deftest complexp-comp (complexp #c(1 2)) t)
(deftest complexp-comp-float (complexp #c(1.0f0 2)) t)
(deftest complexp-nil (complexp nil) nil)

;;; CHARACTERP
(deftest characterp-char (characterp #\a) t)
(deftest characterp-sym (characterp 'a) nil)

;;; STRINGP
(deftest stringp-simple-base (stringp "abc") t)
(deftest stringp-simple-char (stringp (make-array 3 :element-type 'character)) t)
(deftest stringp-complex-base (stringp (make-array 3 :element-type 'base-char :fill-pointer t)) t)
(deftest stringp-complex-char (stringp (make-array 3 :element-type 'character :fill-pointer t)) t)
(deftest stringp-char-array (stringp (make-array '(2 2) :element-type 'character)) nil)
(deftest stringp-vector-of-char (stringp (make-array 3 :initial-contents '(#\a #\b #\c))) nil)
(deftest stringp-3 (stringp 3) nil)

;;; BIT-VECTOR-P
(deftest bit-vector-p-simple (bit-vector-p #*101) t)
(deftest bit-vector-p-complex (bit-vector-p (make-array 3 :element-type 'bit :fill-pointer t)) t)
(deftest bit-vector-p-bit-array (bit-vector-p (make-array '(2 2) :element-type 'bit)) nil)
(deftest bit-vector-p-vector-of-bit (bit-vector-p (make-array 3 :initial-contents '(1 0 1))) nil)
(deftest bit-vector-p-3 (bit-vector-p 3) nil)

;;; VECTORP
(deftest vectorp-simple-base (vectorp "abc") t)
(deftest vectorp-simple-char (vectorp (make-array 3 :element-type 'character)) t)
(deftest vectorp-complex-base (vectorp (make-array 3 :element-type 'base-char :fill-pointer t)) t)
(deftest vectorp-complex-char (vectorp (make-array 3 :element-type 'character :fill-pointer t)) t)
(deftest vectorp-char-array (vectorp (make-array '(2 2) :element-type 'character)) nil)
(deftest vectorp-vector-of-char (vectorp (make-array 3 :initial-contents '(#\a #\b #\c))) t)
(deftest vectorp-simple-bit (vectorp #*101) t)
(deftest vectorp-complex-bit (vectorp (make-array 3 :element-type 'bit :fill-pointer t)) t)
(deftest vectorp-bit-array (vectorp (make-array '(2 2) :element-type 'bit)) nil)
(deftest vectorp-vector-of-bit (vectorp (make-array 3 :initial-contents '(1 0 1))) t)
(deftest vectorp-complex (vectorp (make-array 3 :fill-pointer t)) t)
(deftest vectorp-3 (vectorp 3) nil)

;;; SIMPLE-VECTOR-P
(deftest simple-vector-p-simple-base (simple-vector-p "abc") nil)
(deftest simple-vector-p-simple-char (simple-vector-p (make-array 3 :element-type 'character)) nil)
(deftest simple-vector-p-complex-base (simple-vector-p (make-array 3 :element-type 'base-char :fill-pointer t)) nil)
(deftest simple-vector-p-complex-char (simple-vector-p (make-array 3 :element-type 'character :fill-pointer t)) nil)
(deftest simple-vector-p-char-array (simple-vector-p (make-array '(2 2) :element-type 'character)) nil)
(deftest simple-vector-p-vector-of-char (simple-vector-p (make-array 3 :initial-contents '(#\a #\b #\c))) t)
(deftest simple-vector-p-simple-bit (simple-vector-p #*101) nil)
(deftest simple-vector-p-complex-bit (simple-vector-p (make-array 3 :element-type 'bit :fill-pointer t)) nil)
(deftest simple-vector-p-bit-array (simple-vector-p (make-array '(2 2) :element-type 'bit)) nil)
(deftest simple-vector-p-vector-of-bit (simple-vector-p (make-array 3 :initial-contents '(1 0 1))) t)
(deftest simple-vector-p-simple (simple-vector-p (make-array 3)) t)
(deftest simple-vector-p-complex (simple-vector-p (make-array 3 :fill-pointer t)) nil)
(deftest simple-vector-p-3 (simple-vector-p 3) nil)

;;; SIMPLE-STRING-P
(deftest simple-string-p-simple-base (simple-string-p "abc") t)
(deftest simple-string-p-simple-char (simple-string-p (make-array 3 :element-type 'character)) t)
(deftest simple-string-p-complex-base (simple-string-p (make-array 3 :element-type 'base-char :fill-pointer t)) nil)
(deftest simple-string-p-complex-char (simple-string-p (make-array 3 :element-type 'character :fill-pointer t)) nil)
(deftest simple-string-p-char-array (simple-string-p (make-array '(2 2) :element-type 'character)) nil)
(deftest simple-string-p-vector-of-char (simple-string-p (make-array 3 :initial-contents '(#\a #\b #\c))) nil)
(deftest simple-string-p-3 (simple-string-p 3) nil)

;;; SIMPLE-BIT-VECTOR-P
(deftest simple-bit-vector-p-simple (simple-bit-vector-p #*101) t)
(deftest simple-bit-vector-p-complex (simple-bit-vector-p (make-array 3 :element-type 'bit :fill-pointer t)) nil)
(deftest simple-bit-vector-p-bit-array (simple-bit-vector-p (make-array '(2 2) :element-type 'bit)) nil)
(deftest simple-bit-vector-p-vector-of-bit (simple-bit-vector-p (make-array 3 :initial-contents '(1 0 1))) nil)
(deftest simple-bit-vector-p-3 (simple-bit-vector-p 3) nil)

;;; ARRAYP
(deftest arrayp-simple-base (arrayp "abc") t)
(deftest arrayp-simple-char (arrayp (make-array 3 :element-type 'character)) t)
(deftest arrayp-complex-base (arrayp (make-array 3 :element-type 'base-char :fill-pointer t)) t)
(deftest arrayp-complex-char (arrayp (make-array 3 :element-type 'character :fill-pointer t)) t)
(deftest arrayp-char-array (arrayp (make-array '(2 2) :element-type 'character)) t)
(deftest arrayp-vector-of-char (arrayp (make-array 3 :initial-contents '(#\a #\b #\c))) t)
(deftest arrayp-simple-bit (arrayp #*101) t)
(deftest arrayp-complex-bit (arrayp (make-array 3 :element-type 'bit :fill-pointer t)) t)
(deftest arrayp-bit-array (arrayp (make-array '(2 2) :element-type 'bit)) t)
(deftest arrayp-vector-of-bit (arrayp (make-array 3 :initial-contents '(1 0 1))) t)
(deftest arrayp-complex (arrayp (make-array 3 :fill-pointer t)) t)
(deftest arrayp-3 (arrayp 3) nil)

;;; FUNCTIONP
(deftest functionp1 (functionp '(lambda (x) x)) nil)
(deftest functionp2 (functionp #'(lambda (x) x)) t)
(deftest functionp3 (functionp #'list) t)
(deftest functionp4 (functionp (coerce '(lambda (x) x) 'function)) t)

;;; COMPILED-FUNCTION-P
(deftest compiled-function-p1 (compiled-function-p '(lambda (x) x)) nil)
(eval-when (:execute)
  (deftest compiled-function-p2 (compiled-function-p #'(lambda (x) x)) nil))
(eval-when (:load-toplevel)
  (deftest compiled-function-p2 (compiled-function-p #'(lambda (x) x)) t))
(deftest compiled-function-p3 (compiled-function-p #'list) t)
(deftest compiled-function-p4 (compiled-function-p (coerce '(lambda (x) x) 'function)) nil)
(when (fboundp 'compile)
  (let ((comp (symbol-function 'compile)))
    (declare (notinline compile))
    (deftest compiled-function-p5
      (compiled-function-p (funcall comp nil '(lambda (x) x)))
      t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 6.3 EQUALITY PREDICATES                                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EQ
(deftest eq1 (eq 'a 'b) nil)
(deftest eq2 (eq 'a 'a) t)
(deftest eq3 (eq 3 3.0) nil)
(deftest eq4 (eq #c(3 -4.0) #c(3 -4)) nil)
(deftest eq5 (eq (cons 'a 'b) (cons 'a 'c)) nil)
(deftest eq6 (eq (cons 'a 'b) (cons 'a 'b)) nil)
(deftest eq7 (eq "Foo" (copy-seq "Foo")) nil)
(deftest eq8 (eq "Foo" "foo") nil)
(deftest eq9 (let ((x (cons 'a 'b))) (eq x x)) t)
(deftest eq10 (let ((x '(a . b))) (eq x x)) t)

;;; EQL
(deftest eql1 (eql 'a 'b) nil)
(deftest eql2 (eql 'a 'a) t)
(deftest eql3a (eql 3 3) t)
(deftest eql3b (eql 3.0 3.0) t)
(deftest eql3c (eql #c(3 -4) #c(3 -4)) t)
(deftest eql3d (eql 3.0s0 3.0l0) nil)
(deftest eql3 (eql 3 3.0) nil)
(deftest eql4 (eql #c(3 -4.0) #c(3 -4)) nil)
(deftest eql5 (eql (cons 'a 'b) (cons 'a 'c)) nil)
(deftest eql6 (eql (cons 'a 'b) (cons 'a 'b)) nil)
(deftest eql7 (eql "Foo" (copy-seq "Foo")) nil)
(deftest eql8 (eql "Foo" "foo") nil)
(deftest eql9 (let ((x (cons 'a 'b))) (eql x x)) t)
(deftest eql10 (let ((x '(a . b))) (eql x x)) t)
(deftest eql11 (eql #\a #\a) t)

;;; EQUAL
(deftest equal1 (equal 'a 'b) nil)
(deftest equal2 (equal 'a 'a) t)
(deftest equal3a (equal 3 3) t)
(deftest equal3b (equal 3.0 3.0) t)
(deftest equal3c (equal #c(3 -4) #c(3 -4)) t)
(deftest equal3d (equal 3.0s0 3.0l0) nil)
(deftest equal3 (equal 3 3.0) nil)
(deftest equal4 (equal #c(3 -4.0) #c(3 -4)) nil)
(deftest equal5 (equal (cons 'a 'b) (cons 'a 'c)) nil)
(deftest equal6 (equal (cons 'a 'b) (cons 'a 'b)) t)
(deftest equal7 (equal "Foo" (copy-seq "Foo")) t)
(deftest equal8 (equal "Foo" "foo") nil)
(deftest equal9 (let ((x (cons 'a 'b))) (equal x x)) t)
(deftest equal10 (let ((x '(a . b))) (equal x x)) t)
(deftest equal11 (equal #\a #\a) t)
(deftest equal12 (equal "this-string" "this-string") t)
(deftest equal13 (equal "this-string" "This-string") nil)
(deftest equal14 (equal #*101 (copy-seq #*101)) t)
(deftest equal15 (equal "abc" (make-array 3 :element-type 'character
					  :displaced-to "xabcd"
					  :displaced-index-offset 1
					  :fill-pointer 3))
  t)
(deftest equal16 (equal #*101 (make-array 3 :element-type 'bit
					  :displaced-to #*01010
					  :displaced-index-offset 1
					  :fill-pointer 3))
  t)
(deftest equal17 (equal #\a #\A) nil)
(deftest equal18 (equal '("abc" "def") '("ABC" "DEF")) nil)
(deftest equal19 (equal #(#\A #\B #\C)
			(make-array 3	:element-type 'character
				    :displaced-to "xabcd"
				    :displaced-index-offset 1
				    :fill-pointer 3))
  nil)
(deftest equal20 (equal (make-array '(2 2) :initial-contents #("AB" "CD"))
			(make-array '(2 2) :element-type 'character
				    :displaced-to "xabcd"
				    :displaced-index-offset 1))
  nil)
;; need equal of pathnames, structures!!!

;;; EQUALP
(deftest equalp1 (equalp 'a 'b) nil)
(deftest equalp2 (equalp 'a 'a) t)
(deftest equalp3a (equalp 3 3) t)
(deftest equalp3b (equalp 3.0 3.0) t)
(deftest equalp3c (equalp #c(3 -4) #c(3 -4)) t)
(deftest equalp3d (equalp 3.0s0 3.0l0) t)
(deftest equalp3 (equalp 3 3.0) t)
(deftest equalp4 (equalp #c(3 -4.0) #c(3 -4)) t)
(deftest equalp5 (equalp (cons 'a 'b) (cons 'a 'c)) nil)
(deftest equalp6 (equalp (cons 'a 'b) (cons 'a 'b)) t)
(deftest equalp7 (equalp "Foo" (copy-seq "Foo")) t)
(deftest equalp8 (equalp "Foo" "foo") t)
(deftest equalp9 (let ((x (cons 'a 'b))) (equalp x x)) t)
(deftest equalp10 (let ((x '(a . b))) (equalp x x)) t)
(deftest equalp11 (equalp #\a #\a) t)
(deftest equalp12 (equalp "this-string" "this-string") t)
(deftest equalp13 (equalp "this-string" "This-string") t)
(deftest equalp14 (equalp #*101 (copy-seq #*101)) t)
(deftest equalp15 (equalp "abc" (make-array 3 :element-type 'character
					  :displaced-to "xabcd"
					  :displaced-index-offset 1
					  :fill-pointer 3))
  t)
(deftest equalp16 (equalp #*101 (make-array 3 :element-type 'bit
					  :displaced-to #*01010
					  :displaced-index-offset 1
					  :fill-pointer 3))
  t)
(deftest equalp17 (equalp #\a #\A) t)
(deftest equalp18 (equalp '("abc" "def") '("ABC" "DEF")) t)
(deftest equalp19 (equalp #(#\A #\B #\C)
			  (make-array 3 :element-type 'character
				    :displaced-to "xabcd"
				    :displaced-index-offset 1
				    :fill-pointer 3))
  t)
(deftest equalp20 (equalp (make-array '(2 2) :initial-contents #("AB" "CD"))
			  (make-array '(2 2) :element-type 'character
				    :displaced-to "xabcd"
				    :displaced-index-offset 1))
  t)
(defparameter hash1 (let ((h (make-hash-table :test 'equal)))
		      (setf (gethash 'a h) 1)
		      (setf (gethash "b" h) 2)
		      h))
(defparameter hash2 (let ((h (make-hash-table :test 'equal)))
		      (setf (gethash 'a h) 1)
		      (setf (gethash (make-array 2 :initial-element #\b
						 :element-type 'base-char
						 :fill-pointer 1) h) 2.0f0)
		      h))
(deftest equalp21 (equalp hash1 hash2) t)
;;#-(and cmu (not eclipse))
(deftest equalp22 (equalp hash1
			  (let ((h (make-hash-table :test 'equalp)))
			    (setf (gethash 'a h) 1)
			    (setf (gethash "b" h) 2)
			    h))
  nil)
(deftest equalp23 (equalp hash1
			  (let ((h (make-hash-table :test 'equal)))
			    (setf (gethash 'a h) 1)
			    (setf (gethash "b" h) 2.0f0)
			    h)) t)

;; need equalp of pathnames, structures!!!


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 6.4 LOGICAL OPERATORS                                        ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; NOT
(deftest not-nil (null nil) t)
(deftest not-3 (null 3) nil)

;;; AND
(deftest and
    (let ((temp1 1)
	  (temp2 1)
	  (temp3 1))
      (list (and (incf temp1) (incf temp2) (incf temp3))
	    (and (eql 2 temp1) (eql 2 temp2) (eql 2 temp3))
	    (decf temp3)
	    (and (decf temp1) (decf temp2) (eq temp3 'nil) (decf temp3))
	    (and (eql temp1 temp2) (eql temp2 temp3))
	    (and)))
  (2 t 1 nil t t))
(deftest and-2values (and 1 t (values 2 3)) 2 3)
(deftest and-1value (and 1 (values nil 2) (values 2 3)) nil)

;;; OR
(deftest or
    (let ((temp0 nil)
	  (temp1 10)
	  (temp2 20)
	  (temp3 30))
      (list (or)
	    (or temp0 temp1 (setq temp2 37))
	    temp2
	    (or (incf temp1) (incf temp2) (incf temp3))
	    (list temp1 temp2 temp3)
	    (or (values) temp1)
	    (multiple-value-list (or (values temp1 temp2) temp3))
	    (multiple-value-list (or temp0 (values temp1 temp2)))
	    (multiple-value-list (or (values temp0 temp1) (values temp2 temp3)))))
  (nil 10 20 11 (11 20 30) 11 (11) (11 20) (20 30)))