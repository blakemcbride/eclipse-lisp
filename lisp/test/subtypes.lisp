
;;; SUBTYPEP

(deftest subtypep-t (subtypep nil t) t t)
;(deftest subtypep-t1 (subtypep 'some-undefined-type t) t t)
(deftest subtypep-nil-nil (subtypep nil nil) t t)
(deftest subtypep-t-nil (subtypep t nil) nil t)
;(deftest subtypep-nil1 (subtypep 'foo nil) nil t)

(defun find-non-disjoint-pairs (types &aux failures)
  (labels ((maybe-sub (a b)
	     (multiple-value-bind (subtypep? sure?) (subtypep a b)
	       (or subtypep? (not sure?))))
	   (check (a b)
	     (when (or (maybe-sub a b)
		       (maybe-sub b a))
	       (push (list a b) failures))))
    (do ((types types (cdr types)))
	((endp types) failures)
      (dolist (other-type (rest types))
	(check (first types) other-type)))))

(defun types-are-equivalent (a b)
  (and (subtypep a b) (subtypep b a) t))

(defun find-non-disjoint-subtypes (type subs)
  (or (find-non-disjoint-pairs subs)
      (dolist (sub subs)
	(unless (subtypep sub type) (return sub)))))

;;; Note Franz fails with readtable, pathname, stream.
(deftest main-disjoints
    (find-non-disjoint-pairs '(cons symbol array number character
			       hash-table random-state
			       readtable package pathname stream
			       ))
  nil)

(deftest list-cons1
  (types-are-equivalent 'cons '(and list (not null))) t)
(deftest list-cons2
  (types-are-equivalent 'cons '(and list (not symbol))) t)

;;; FUNCTION
(deftest function-disjoints
    (find-non-disjoint-pairs '(cons symbol array number character function))
  nil)
(deftest compiled-function (subtypep 'compiled-function 'function) t t)

(deftest numbers (find-non-disjoint-subtypes 'NUMBER '(REAL COMPLEX)) nil)
(deftest t-real (find-non-disjoint-subtypes 'REAL '(RATIONAL FLOAT)) nil)
(deftest t-rational (find-non-disjoint-subtypes 'rational '(integer ratio)) nil)
(deftest t-integer (find-non-disjoint-subtypes 'integer '(fixnum bignum)) nil)
(deftest exhaustive-integer (types-are-equivalent 'bignum '(and integer (not fixnum))) t)

(deftest t-float
    (dolist (sub '(long-float double-float single-float short-float) t)
      (unless (subtypep sub 'float) (return))) t)

(deftest t-null (subtypep 'null 'symbol) t t)
(deftest t-list (find-non-disjoint-subtypes 'list '(cons null)) nil)
(deftest t-standard-char (subtypep 'standard-char 'base-char) t t)
(deftest t-character (find-non-disjoint-subtypes 'character '(base-char extended-char)) nil)

(deftest t-string (subtypep 'string 'vector) t t)
(deftest t-character-string (subtypep '(vector character) 'string) t t)
(deftest t-standard-char-string (subtypep '(vector standard-char) 'string) t t)
(deftest t-base-char-string (subtypep '(vector base-char) 'string) t t)
(deftest t-extended-char-string (subtypep '(vector extended-char) 'string) t t)

(deftest t-bit-vector (subtypep 'bit-vector 'vector) t t)
(deftest t-bit-vector2 (types-are-equivalent 'bit-vector '(vector bit)) t)

(deftest vector-subs (find-non-disjoint-pairs
		      '((vector t) string bit-vector)) nil)
(deftest t-vector (subtypep 'vector 'array) t t)
(deftest t-vector2 (types-are-equivalent '(vector t) '(array t (*))) t)

(deftest t-simple-array (subtypep 'simple-array 'array) t t)
(deftest t-simple-array2 (find-non-disjoint-subtypes
			'simple-array
			'(simple-vector simple-string simple-bit-vector))
  nil)

(deftest t-simple-vector (subtypep 'simple-vector '(vector t)) t t)
(deftest t-simple-string (subtypep 'simple-string 'string) t t)
(deftest t-simple-bit-vector (subtypep 'simple-bit-vector 'bit-vector) t t)

(deftest t-sequence (find-non-disjoint-subtypes
		   'sequence '(vector list)) nil)

(deftest t-stream (find-non-disjoint-subtypes
		 'stream
		 '(two-way-stream echo-stream broadcast-stream
		   file-stream synonym-stream string-stream
		   concatenated-stream)) nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(progn
(deftest sub-satisfies1
    (subtypep '(and fixnum (satisfies evenp)) 'integer) t t)
(deftest sub-satisfies2
    (subtypep '(and (satisfies evenp) fixnum) 'integer) t t)
(deftest sub-satisfies3
    (subtypep '(eql 3) '(and (satisfies evenp) fixnum)) nil t)
(deftest sub-satisfies4
    (subtypep '(integer 2 2) '(and (satisfies evenp) fixnum)) t t)
(deftest sub-satisfies5
    (subtypep '(integer 2 (3)) '(and (satisfies evenp) fixnum)) t t)
)

(deftest sub-member0
    (subtypep '(member) 'nil) t t)
(deftest sub-member1
    (subtypep '(member a b c) 'symbol) t t)
(deftest sub-member2
    (subtypep '(member a b c) '(member a b c)) t t)
(deftest sub-member3
    (subtypep '(member a b c) '(member a b)) nil t)
(deftest sub-member4
    (subtypep '(member a 1 2.0) '(or symbol number)) t t)
(deftest sub-member5
  (subtypep '(and (or keyword real)
		  (not (member :foo 1 2.0)))
	    '(or symbol number)) t t)
(deftest sub-member6
    (subtypep '(member :bar 3 4.0)
	      '(and (or keyword real)
		(not (member :foo 1 2.0)))) t t)
(deftest sub-member7
    (subtypep '(member :bar 3 4.0 1)
	      '(and (or keyword real)
		(not (member :foo 1 2.0)))) nil t)
(deftest sub-member7a
    (subtypep '(member :bar 3 4.0 1)
	      '(and (or keyword real)
		(not (member :foo 9 2.0)))) t t)
(deftest sub-member8
    (subtypep '(integer 3 (5)) '(and fixnum (member 3 4))) t t)
(deftest sub-member9
  (subtypep 'null '(member nil)) t t)
(deftest sub-member10
  (subtypep '(member #\a) 'character) t t)
(deftest sub-member11
  (subtypep '(member #\a 3) 'character) nil t)

(deftest sub-eql1
  (subtypep '(eql 3) '(member 1 2 3)) t t)
(deftest sub-eql2
    (subtypep '(eql 3) 'fixnum) t t)
(deftest sub-eql3
    (subtypep '(eql 3) '(rational 2 3)) t t)
(deftest sub-eql4
    (subtypep '(integer 3 (4)) '(and fixnum (eql 3))) t t)

(deftest sub-and (subtypep 'fixnum '(and)) t t)
(deftest sub-or (subtypep '(or) nil) t t)

(deftest sub-array1 (subtypep '(array t) '(array *)) t t)
(deftest sub-array2 (subtypep '(array character) '(array *)) t t)
(deftest sub-array3 (subtypep '(array character) '(array t)) nil t)
(deftest sub-array4 (subtypep '(array t 3) '(array * *)) t t)
(deftest sub-array5 (subtypep '(array t 3) '(array * 4)) nil t)
(deftest sub-array6 (subtypep '(array t 3) '(array * (* * *))) t t)
(deftest sub-array7 (subtypep '(array t 3) '(array * (1 2 3))) nil t)
(deftest sub-array8 (subtypep '(array t (1 2 3)) '(array t 3)) t t)
(deftest sub-array9 (subtypep '(array (integer 0 1) 3) '(array bit *)) t t)
(deftest sub-array10 (subtypep '(array (integer 0 1) *) '(array bit 3)) nil t)
(deftest sub-array11 (subtypep '(array bit) '(array base-char)) nil t)
(deftest sub-array12 (subtypep '(array standard-char) '(array base-char)) t t)
(deftest sub-array13 (subtypep '(array standard-char 0) '(array base-char *)) t t)
(deftest sub-array14 (subtypep '(array standard-char 0) '(array base-char 1)) nil t)

(deftest sub-simple-array1 (subtypep '(simple-array t) '(array *)) t t)
(deftest sub-simple-array2 (subtypep '(simple-array character) '(array *)) t t)
(deftest sub-simple-array4 (subtypep '(simple-array t 3) '(array * *)) t t)
(deftest sub-simple-array6 (subtypep '(simple-array t 3) '(array * (* * *))) t t)
(deftest sub-simple-array7 (subtypep '(simple-array t (1 2 3)) '(array t 3)) t t)

(deftest sub-vector1 (subtypep '(vector character) '(vector t)) nil t)
(deftest sub-vector2 (subtypep '(vector character) '(vector *)) t t)
(deftest sub-vector3 (subtypep '(vector character 3) '(vector * *)) t t)
(deftest sub-vector4 (subtypep '(vector character 3) '(vector * 4)) nil t)

(deftest sub-simple-vector2 (subtypep '(simple-vector) '(vector t)) t t)
(deftest sub-simple-vector3 (subtypep '(simple-vector 3) '(vector t *)) t t)

(deftest sub-member-string
    (subtypep '(member "abc") '(string 3)) t t)
(deftest sub-member-string2
    (subtypep '(member "abc") '(simple-string 3)) t t)
(deftest sub-member-string3
    (subtypep '(member "abc") '(string 4)) nil t)
(defparameter 2d-array (make-array '(2 2)))
(deftest sub-member-array
    (subtypep `(member ,2d-array)
		'(array * (2 2))) t t)
(deftest sub-member-array2
    (subtypep `(member ,2d-array)
		'(simple-array integer (2 2))) t t)
(deftest sub-member-array3
    (subtypep `(member ,2d-array)
		'(simple-array integer (2 3))) nil t)


#-(and cmu (not eclipse))
(progn
  (deftest sub-complex1 (subtypep 'complex '(complex integer)) nil t)
  (deftest sub-complex2 (subtypep '(complex integer) 'complex) t t)
  (deftest sub-complex3
    (subtypep '(complex (integer 0 10)) '(complex (integer 0 100))) t t)
  (deftest sub-complex4
    (subtypep '(complex (integer 0 10)) '(complex (integer 0 9))) t t)
  #+eclipse
  (deftest sub-complex5
    (subtypep '(complex (or ratio (integer 0 100))) '(complex (rational 0 10)))
    t t)
  (deftest sub-complex6
    (subtypep '(complex (or ratio (integer 0 100))) '(complex (rational 0 100)))
    t t)
  (deftest sub-complex6a
    (subtypep '(complex (or (ratio 2/3 3/2) (integer 0 100))) '(complex (rational 0 100)))
    t t)
  (deftest sub-complex-7
    (subtypep '(complex (and ratio (real 0 10))) '(complex (or (integer 0 10) ratio)))
    t t)
  #+eclipse
  (deftest sub-complex-8
    (subtypep '(complex (and ratio (real 0 10))) '(complex (integer 0 10)))
    t t)
  (deftest sub-complex-9
    (subtypep '(complex (not integer)) '(complex (or ratio float)))
    t t)
  (deftest sub-complex-10
    (subtypep '(complex (not integer)) '(complex float))
    t t)
  (deftest sub-complex-10a
    (subtypep '(complex (and (not rational) (not single-float))) '(complex double-float))
    t t)

  (deftest sub-complex-member1
    (subtypep '(complex (or (eql 3) (single-float 0.0f0)))
	      '(complex (and real (not (member 3 4)))))
    t t)
  (deftest sub-complex-member2
    (subtypep '(complex (or (eql 3) (single-float 0.0f0)))
	      '(complex (and real (not (member 4)))))
    t t)

  (deftest sub-member-complex1
    (subtypep '(member #c(1 2)) '(complex integer)) t t)
  (deftest sub-member-complex2
    (subtypep '(member #c(1 2)) '(complex (integer 0 5))) t t)
  (deftest sub-member-complex3
    (subtypep '(member #c(1 9)) '(complex (integer 0 5))) t t)
  (deftest sub-member-complex4
    (subtypep '(member #c(1 9)) '(member #c(9 9))) nil t)
  (deftest sub-member-complex5
    (subtypep '(member #c(1 9)) '(member #c(1 9))) t t))

(deftest sub-cons1 (subtypep 'cons '(cons integer)) nil t)
(deftest sub-cons2 (subtypep '(cons (integer 0 5)) '(cons number)) t t)
(deftest sub-cons3 (subtypep '(cons (integer 0 5)) '(cons (integer * 0))) nil t)
(deftest sub-cons4 (subtypep '(cons * (integer 0 5)) '(cons * number)) t t)
(deftest sub-cons5 (subtypep '(cons * (integer 0 5)) '(cons * (integer * 0))) nil t)
(deftest sub-cons6
    (subtypep '(cons (integer 0 5))
		'(or (cons (integer 0 (2))) (cons (integer 2 5))))
  t t)
(deftest sub-cons7
    (subtypep '(cons (integer 0 5))
		'(or (cons (integer 0 (2))) (cons (integer (2) 5))))
  nil t)
(deftest sub-cons8
    (subtypep '(cons keyword integer) '(not (cons keyword integer)))
  nil t)
(deftest sub-cons9
    (subtypep '(cons *) '(cons (and)))
  t t)
(deftest sub-cons10
    (subtypep '(cons (and)) '(cons *))
  t t)

(deftest sub-cons11
    (subtypep '(member (1 . 2)) '(cons integer integer)) t t)
(deftest sub-cons12
    (subtypep '(member (1 . 2)) '(cons * (integer 0 5))) t t)
(deftest sub-cons13
    (subtypep '(member (1 . 9)) '(cons * (integer 0 5))) nil t)

(deftest sub-cons-eql14
    (subtypep `(eql ,(cons 1 2))
		`(member ,(cons 1 2) (3 . 4)))
  nil t)
(deftest sub-cons-eql5
    (let ((c (cons 1 2)))
      (subtypep `(eql ,c)
		  `(member ,c (3 . 4))))
  t t)

(deftest sub-i1 (subtypep '(integer 1 3) '(integer 1 4)) t t)
(deftest sub-i2 (subtypep '(integer (0) (0)) nil) t t)
(deftest sub-i3 (subtypep nil '(integer (0) (0))) t t)
    
(deftest sub-mod1
    (types-are-equivalent '(mod 34) '(integer 0 (34))) t)
(deftest sub-signed-byte
    (types-are-equivalent '(signed-byte 7) '(integer -64 (64))) t)
(deftest sub-unsigned-byte
    (types-are-equivalent '(unsigned-byte 7) '(integer 0 127)) t)
(deftest sub-rational
    (types-are-equivalent '(rational -9/8 17/8)
			 '(or (ratio -9/8 17/8) (integer -1 2))) t)
(deftest sub-rational1
    (subtypep '(eql 17/8) '(rational 1 3)) t t)
(deftest sub-rational2
    (subtypep '(eql 17/8) '(rational 1 17/8)) t t)
(deftest sub-rational3
    (subtypep '(eql 17/8) '(rational 1 (17/8))) nil t)
(deftest sub-rational4
    (subtypep '(eql 17/8) '(rational 17/8 100)) t t)
(deftest sub-rational5
    (subtypep '(eql 17/8) '(rational (17/8) 100)) nil t)
(deftest sub-rational6
    (subtypep '(rational 2/3 7) '(real 0 10)) t t)
(deftest sub-rational7
    (subtypep '(rational 3 *) '(real 2 *)) t t)
(deftest sub-rational8
    (subtypep '(rational * 3) '(real * 4)) t t)


(deftest sub-float1
    (subtypep '(eql 1.2f0) '(float 1.0 2.0)) t t)
(deftest sub-float2
    (subtypep '(eql 1.2f0) '(float 1.0 1.2)) t t)
(deftest sub-float3
    (subtypep '(eql 1.2f0) '(float 1.0 (1.2))) nil t)
(deftest sub-float4
    (subtypep '(eql 1.2f0) '(float 1.2 2.0)) t t)
(deftest sub-float5
    (subtypep '(eql 1.2f0) '(float (1.2) 2.0)) nil t)
(deftest sub-short
    (subtypep '(short-float 1.2s0 2.3s0) '(float 1.2s0 2.3s0)) t t)
(deftest sub-single
    (subtypep '(single-float 1.2f0 2.3f0) '(float 1.2f0 2.3f0)) t t)
(deftest sub-double
    (subtypep '(double-float 1.2d0 2.3d0) '(float 1.2d0 2.3d0)) t t)
(deftest sub-long
    (subtypep '(double-float 1.2l0 2.3l0) '(float 1.2l0 2.3l0)) t t)

(deftest sub-string1
    (subtypep '(vector character 3) '(string 3)) t t)
(deftest sub-simple-string1
    (subtypep '(simple-array character (3)) '(string 3)) t t)
(deftest sub-simple-string2
    (subtypep '(simple-array character (3)) '(simple-string 3)) t t)
;; In Eclipse
(deftest sub-string2 (subtypep '(string 3) '(vector character 3)) nil t)
(deftest sub-simple-string3
    (subtypep '(simple-string 3) '(simple-array character (3))) nil t)

(deftest sub-base-string
    (types-are-equivalent '(base-string 3) '(vector base-char 3)) t)
(deftest sub-simple-base-string
    (types-are-equivalent '(simple-base-string 3)
			  '(simple-array base-char (3))) t)
(deftest sub-bit-vector
    (types-are-equivalent '(bit-vector 3) '(vector bit 3)) t)
(deftest sub-simple-bit-vector
    (types-are-equivalent '(simple-bit-vector 3)
			  '(simple-array bit (3))) t)

