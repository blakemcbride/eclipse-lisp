;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 4.8 TYPE CONVERSION FUNCTION                                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; COERCE
(deftest coerce-list (coerce "abc" 'list) (#\a #\b #\c))
(deftest coerce-cons (coerce "abc" 'cons) (#\a #\b #\c))
(deftest coerce-vector (equalp (coerce "abc" '(vector t)) #(#\a #\b #\c)) t)
(deftest coerce-bit-vector (coerce '(1 0 1) 'bit-vector) #*101)
(deftest coerce-simple-bit-vector (coerce '(1 0 1) '(simple-bit-vector 3)) #*101)
(deftest coerce-string (coerce '(#\a #\b #\c) 'string) "abc")
(deftest coerce-simple-string (coerce '(#\a #\b #\c) 'simple-string) "abc")
(deftest coerce-simple-base-string (coerce '(#\a #\b #\c) 'simple-base-string) "abc")
(deftest coerce-base-string (coerce '(#\a #\b #\c) 'base-string) "abc")
(deftest coerce-string-noop (let ((s "abc")) (eq s (coerce s 'string))) t)
(deftest coerce-list-noop (let ((s '(1 2 3))) (eq s (coerce s 'list))) t)
(deftest coerce-vector-noop (let ((s #(1 2 3))) (eq s (coerce s 'vector))) t)
(deftest coerce-character-noop (let ((s #\a)) (eq s (coerce s 'character))) t)
(deftest coerce-character (coerce "a" 'character) #\a)
(deftest coerce-character2 (coerce 'a 'character) #\A)
(deftest coerce-complex-noop (let ((s #c(1 2))) (eq s (coerce s 'complex))) t)
#-(and cmu (not eclipse))
(deftest coerce-complex-rat (coerce 3 'complex) 3)
(deftest coerce-complex-float (coerce 3.0f0 'complex) #c(3.0f0 0.0f0))
(deftest coerce-float-noop (let ((s 1.0d0))
			     (eql s (coerce s 'float))) t)
(deftest coerce-double (coerce 1.0f0 'double-float) 1.0d0)
(deftest coerce-single (coerce 1.0d0 'single-float) 1.0f0)
(deftest coerce-long (coerce 1.0f0 'long-float) 1.0l0)
(deftest coerce-short (coerce 1.0d0 'short-float) 1.0s0)
(deftest coerce-float (coerce 1 'float) 1.0f0)
(deftest coerce-t-noop (let ((s *random-state*)) (eq s (coerce s 't))) t)
(deftest coerce-function-noop (let ((s #'list)) (eq s (coerce s 't))) t)
#-(and cmu (not eclipse))
(progn
  (deftest coerce-function (eq (coerce 'list 'function) #'list) t)
  (deftest coerce-lambda
    (let ((f (coerce '(lambda (x) x) 'function)))
      (and (functionp f)
	   (if (fboundp 'eval)		;run-time systems don't have eval
	       (funcall f 3)
	       3)))
    3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 4.9 DETERMINING THE TYPE OF AN OBJECT                        ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TYPE-OF 
(deftest type-of-array-s (subtypep (type-of (make-array '(2 2))) 'array) t t)
(deftest type-of-array-c (subtypep (type-of (make-array '(2 2) :adjustable t)) 'array) t t)
(deftest type-of-bit-vector-s (subtypep (type-of #*101) 'bit-vector) t t)
(deftest type-of-bit-vector-c (subtypep (type-of (make-array 3 :element-type 'bit :fill-pointer t))
					'bit-vector) t t)
(deftest type-of-character (subtypep (type-of #\a) 'character) t t)
(deftest type-of-complex (subtypep (type-of #c(1 2)) 'complex) t t)
;; condition!!!
(deftest type-of-cons (subtypep (type-of '(1 . 2)) 'cons) t t)
(deftest type-of-double-float (subtypep (type-of 2.0d0) 'double-float) t t)
(deftest type-of-function (subtypep (type-of #'list) 'function) t t)
(deftest type-of-hash (subtypep (type-of (make-hash-table)) 'hash-table) t t)
(deftest type-of-fix (subtypep (type-of 3) 'integer) t t)
(deftest type-of-big (subtypep (type-of #xffffffff) 'integer) t t)
(deftest type-of-long-float (subtypep (type-of 2.0l0) 'long-float) t t)
(deftest type-of-null (type-of nil) null)
(deftest type-of-pkg (let ((p (make-package "typed-pkg")))
			(prog1 (type-of p) (delete-package p))) package)
(deftest type-of-random-state (type-of *random-state*) random-state)
(deftest type-of-ratio (subtypep (type-of 1/2) 'ratio) t t)
;;; readable!!!
;;; restart!!!
(deftest type-of-short-float (subtypep (type-of 2.0s0) 'short-float) t t)
(deftest type-of-single-float (subtypep (type-of 2.0f0) 'single-float) t t)
;;; stream!!!
(deftest type-of-string-s (subtypep (type-of "abc") 'string) t t)
(deftest type-of-string-c (subtypep (type-of (make-array 3 :element-type 'character :fill-pointer t))
					'string) t t)
(deftest type-of-symbol (type-of 'a) symbol)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 4.10 TYPE UPGRADING                                          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; UPGRADED-ARRAY-ELEMENT-TYPE
(deftest upgraded-array-element-type-bit
    (subtypep (upgraded-array-element-type 'bit) 'bit) t t)
(deftest upgraded-array-element-type-base-char
    (subtypep (upgraded-array-element-type 'base-char) 'base-char) t t)
(deftest upgraded-array-element-type-character
    (subtypep (upgraded-array-element-type 'character) 'character) t t)
(deftest upgraded-array-element-type-symbol
    (upgraded-array-element-type 'symbol) t)


;;; UPGRADED-COMPLEX-PART-TYPE
(deftest upgraded-complex-part-type
    (subtypep (upgraded-complex-part-type 'integer)
	      (upgraded-complex-part-type 'real) )
  t t)

