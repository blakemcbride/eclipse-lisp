
;;; COMPLEMENT
(deftest complement-nil (funcall (complement #'endp) nil) nil)
(deftest complement-t (funcall (complement #'endp) '(1 . 2)) t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 14.1 SIMPLE SEQUENCE FUNCTIONS                               ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter seq-list (list 1 2 3))
(defparameter seq-t (make-array 3 :initial-contents '(1 2 3)))
(defparameter seq-bit (make-array 3 :element-type 'bit
				:initial-contents '(0 0 1)))
(defparameter seq-base-char (make-array 3 :element-type 'base-char
					:initial-contents "abc"))
(defparameter seq-character (make-array 3 :element-type 'character
					:initial-contents "abc"))
(defparameter seq-complex-t (make-array 4 :adjustable t
					:fill-pointer 3
					:initial-contents '(1 2 3 4)))
(defparameter seq-complex-bit (make-array 4 :element-type 'bit
					  :adjustable t
					  :fill-pointer 3
					  :initial-contents '(0 0 1 0)))
(defparameter seq-complex-base-char
    (make-array 4 :element-type 'base-char
		:adjustable t
		:fill-pointer 3
		:initial-contents "abcd"))
(defparameter seq-complex-character
    (make-array 4 :element-type 'character
		:adjustable t
		:fill-pointer 3
		:initial-contents "abcd"))

;;; ELT
(deftest set-elt-list (list (setf (elt seq-list 2) 9)
			    (elt seq-list 2)
			    (setf (elt seq-list 2) 3))
  (9 9 3))
(deftest set-elt-t (list (setf (elt seq-t 2) 9)
			 (elt seq-t 2)
			 (setf (elt seq-t 2) 3))
  (9 9 3))
(deftest set-elt-bit (list (setf (elt seq-bit 2) 0)
			   (elt seq-bit 2)
			   (setf (elt seq-bit 2) 1))
  (0 0 1))
(deftest set-elt-base-char (list (setf (elt seq-base-char 2) #\x)
				 (elt seq-base-char 2)
				 (setf (elt seq-base-char 2) #\c))
  (#\x #\x #\c))
(deftest set-elt-character (list (setf (elt seq-character 2) #\x)
				 (elt seq-character 2)
				 (setf (elt seq-character 2) #\c))
  (#\x #\x #\c))
(deftest set-elt-complex-t (list (setf (elt seq-complex-t 2) 9)
			 (elt seq-complex-t 2)
			 (setf (elt seq-complex-t 2) 3))
  (9 9 3))
(deftest set-elt-complex-bit (list (setf (elt seq-complex-bit 2) 0)
			   (elt seq-complex-bit 2)
			   (setf (elt seq-complex-bit 2) 1))
  (0 0 1))
(deftest set-elt-complex-base-char (list (setf (elt seq-complex-base-char 2) #\x)
				 (elt seq-complex-base-char 2)
				 (setf (elt seq-complex-base-char 2) #\c))
  (#\x #\x #\c))
(deftest set-elt-complex-character (list (setf (elt seq-complex-character 2) #\x)
				 (elt seq-complex-character 2)
				 (setf (elt seq-complex-character 2) #\c))
  (#\x #\x #\c))

;;; SUBSEQ
(deftest subseq-nil (subseq nil 0) nil)
(deftest subseql (subseq seq-list 0) (1 2 3))
(deftest subseql1 (subseq seq-list 1) (2 3))
(deftest subseql03 (subseq seq-list 0 3) (1 2 3))
(deftest subseql02 (subseq seq-list 0 2) (1 2))
(deftest subseql12 (subseq seq-list 1 2) (2))
(deftest subseq-t-end (equalp (subseq seq-t 1 2) #(2)) t)
(deftest subseq-t (equalp (subseq seq-t 1) #(2 3)) t)
(deftest subseq-bit-end (equalp (subseq seq-bit 1 2) #(0)) t)
(deftest subseq-bit (equalp (subseq seq-bit 1) #(0 1)) t)
(deftest subseq-base-char-end (subseq seq-base-char 1 2) "b")
(deftest subseq-base-char (subseq seq-base-char 1) "bc")
(deftest subseq-character-end (subseq seq-character 1 2) "b")
(deftest subseq-character (subseq seq-character 1) "bc")
(deftest subseq-complex-t-end (equalp (subseq seq-complex-t 1 2) #(2)) t)
(deftest subseq-complex-t (equalp (subseq seq-complex-t 1) #(2 3)) t)
(deftest subseq-complex-bit-end (equalp (subseq seq-complex-bit 1 2) #(0)) t)
(deftest subseq-complex-bit (equalp (subseq seq-complex-bit 1) #(0 1)) t)
(deftest subseq-complex-base-char-end (subseq seq-complex-base-char 1 2) "b")
(deftest subseq-complex-base-char (subseq seq-complex-base-char 1) "bc")
(deftest subseq-complex-character-end (subseq seq-complex-character 1 2) "b")
(deftest subseq-complex-character (subseq seq-complex-character 1) "bc")
(deftest setf-subseq-bit-vector
    (let ((v (copy-seq #*0101010)))
      (setf (subseq v 2 5) #*1010) v) #*0110110)
(deftest setf-subseq-string
    (let ((v (copy-seq "xyabcyx")))
      (setf (subseq v 2 6) "ABC") v) "xyABCyx")
(deftest setf-subseq-list
    (let ((v (copy-seq '(x y a b c y x))))
      (setf (subseq v 2 5) "ABCx") v) (x y #\A #\B #\C y x))
(deftest setf-subseq-vector
    (let ((v (copy-seq '#(x y a b c y x))))
      (setf (subseq v 2 6) '(1 2 3))
      (equalp v #(x y 1 2 3 y x))) t)

;;; COPY-SEQ
(deftest copy-seq-nil (copy-seq nil) nil)
(deftest copy-seq-list (equal (copy-seq seq-list) seq-list) t)
(deftest copy-seq-t (equalp (copy-seq seq-t) seq-t) t)
(deftest copy-seq-bit (equalp (copy-seq seq-bit) seq-bit) t)
(deftest copy-seq-base-char (not (null (equal (copy-seq seq-base-char) seq-base-char))) t)
(deftest copy-seq-character (not (null (equal (copy-seq seq-character) seq-character))) t)
(deftest copy-seq-complex-t (equalp (copy-seq seq-complex-t) seq-t) t)
(deftest copy-seq-complex-bit (equalp (copy-seq seq-complex-bit) seq-bit) t)
(deftest copy-seq-complex-base-char (not (null (equal (copy-seq seq-complex-base-char) seq-base-char))) t)
(deftest copy-seq-complex-character (not (null (equal (copy-seq seq-complex-character) seq-character))) t)

;;; LENGTH
(deftest length-nil (length nil) 0)
(deftest length-nil2 (length (list)) 0)
(deftest length-list1 (length (list 1)) 1)
(deftest length-list2 (length (list 1 2)) 2)
(deftest length-list (length seq-list) 3)
(deftest length-t (length seq-t) 3)
(deftest length-bit (length seq-bit) 3)
(deftest length-base-char (length seq-base-char) 3)
(deftest length-character (length seq-character) 3)
(deftest length-complex-bit (length seq-complex-bit) 3)
(deftest length-complex-base-char (length seq-complex-base-char) 3)
(deftest length-complex-character (length seq-complex-character) 3)

;;; REVERSE
(deftest reverse-nil (reverse nil) nil)
(deftest reverse-list (reverse seq-list) (3 2 1))
(deftest reverse-t (equalp (reverse seq-t) #(3 2 1)) t)
(deftest reverse-bit (equalp (reverse seq-bit) #(1 0 0)) t)
(deftest reverse-base-char (reverse seq-base-char) "cba")
(deftest reverse-character (reverse seq-character) "cba")
(deftest reverse-complex-t (equalp (reverse seq-complex-t) #(3 2 1)) t)
(deftest reverse-complex-bit (equalp (reverse seq-complex-bit) #(1 0 0)) t)
(deftest reverse-complex-base-char (reverse seq-complex-base-char) "cba")
(deftest reverse-complex-character (reverse seq-complex-character) "cba")

;;; REVERSE
(deftest nreverse-nil (nreverse nil) nil)
(deftest nreverse-list
    (let* ((s (list 1 2 3))
	   (r (nreverse s)))
      (list (length s) r)) (1 (3 2 1)))
(deftest nreverse-t
    (let ((s (vector 1 2 3)))
      (and (eq s (nreverse s))
	   (equalp s #(3 2 1)))) t)
(deftest nreverse-bit
    (let ((s (make-array 3 :element-type 'bit
			 :initial-contents '(0 0 1))))
      (and (eq s (nreverse s))
	   (equalp s #(1 0 0))))
  t)
(deftest nreverse-base-char
    (let ((s (make-array 3 :element-type 'base-char
			 :initial-contents "abc")))
      (and (eq s (nreverse s)) s)) "cba")
(deftest nreverse-character
    (let ((s (make-array 3 :element-type 'character
			 :initial-contents "abc")))
      (and (eq s (nreverse s)) s)) "cba")
(deftest nreverse-complex-t
    (let ((s (make-array 3
			 :fill-pointer 3
			 :adjustable t
			 :displaced-to (vector 1 2 3))))
      (and (eq s (nreverse s))
	   (equalp s #(3 2 1)))) t)
(deftest nreverse-complex-bit
    (let ((s (make-array 3
			 :element-type 'bit
			 :fill-pointer 3
			 :adjustable t
			 :displaced-to
			 (make-array 3 :element-type 'bit
				     :initial-contents '(0 0 1)))))
      (and (eq s (nreverse s))
	   (equalp s #(1 0 0))))
  t)
(deftest nreverse-complex-base-char
    (let ((s (make-array 3
			 :element-type 'base-char
			 :fill-pointer 3
			 :adjustable t
			 :displaced-to
			 (make-array 3 :element-type 'base-char
				     :initial-contents "abc"))))
      (and (eq s (nreverse s))
	   (not (null (string= s "cba"))))) t)
(deftest nreverse-complex-character
    (let ((s (make-array 3
			 :element-type 'character
			 :fill-pointer 3
			 :adjustable t
			 :displaced-to
			 (make-array 3 :element-type 'character
				     :initial-contents "abc"))))
      (and (eq s (nreverse s))
	   (not (null (string= s "cba"))))) t)

;;; MAKE-SEQUENCE
(deftest make-sequence-nil-0 (make-sequence 'list 0) nil)
(deftest make-sequence-nil (make-sequence 'list 0 :initial-element 9) nil)
(deftest make-sequence-list (make-sequence 'list 3) (nil nil nil))
(deftest make-sequence-list-element
    (make-sequence 'list 3 :initial-element 9) (9 9 9))
(deftest make-sequence-t-length (length (make-sequence 'vector 3)) 3)
(deftest make-sequence-bit-length (length (make-sequence 'bit-vector 3)) 3)
(deftest make-sequence-bit-type
    (array-element-type (make-sequence 'bit-vector 3)) bit)
(deftest make-sequence-bit-elt
    (elt (make-sequence 'bit-vector 3 :initial-element 1) 0) 1)
(deftest make-sequence-base-char-length
	 (length (make-sequence 'base-string 3)) 3)
(deftest make-sequence-base-char-type
	 (array-element-type (make-sequence 'base-string 3)) base-char)
(deftest make-sequence-base-char-elt
	 (elt (make-sequence 'base-string 3 :initial-element #\a) 0) #\a)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 14.2 CONCATENATING, MAPPING, AND REDUCING SEQUENCES          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CONCATENATE
(defparameter bv (make-array 2 :element-type 'bit :initial-contents '(1 0)))
(defparameter bv-fill (make-array 3 :element-type 'bit :initial-contents '(1 0 1)
				  :fill-pointer 2))
(defparameter s-fill (make-array 4 :element-type 'base-char
				      :initial-contents "wxyz"
				      :fill-pointer 3))
(defparameter v-fill (make-array 4 :initial-contents '(a b c d)
				 :fill-pointer 3))

(deftest concatenate-empty-list (concatenate 'list) nil)
(deftest concatenate-empty-vector (equalp (concatenate 'vector) #()) t)
(deftest concatenate-list
    (concatenate 'list nil '(1 2 3) nil #() #(4 5 6) "abc" bv bv-fill
		 s-fill v-fill #())
  (1 2 3 4 5 6 #\a #\b #\c 1 0 1 0 #\w #\x #\y A B C))
(deftest concatenate-vector
    (equalp (concatenate 'vector nil '(1 2 3) nil #() #(4 5 6) "abc" bv bv-fill
		 s-fill v-fill #())
	    #(1 2 3 4 5 6 #\a #\b #\c 1 0 1 0 #\w #\x #\y A B C)) t)
(deftest concatenate-string
    (concatenate 'string nil '(#\A #\B) nil #() #(#\C #\D) "abc" s-fill #())
  "ABCDabcwxy")
(deftest concatenate-copies
    (let* ((x '(a b c))
	   (copy (concatenate 'list x)))
      (list (eq x copy) (equal x copy)))
  (nil t))
(deftest concatenate-list-subtype (concatenate 'null nil) nil)

;;; MAP
(deftest map-list (map 'list #'- '(1 2 3 4)) (-1 -2 -3 -4))
(deftest map-string
    (map 'string #'(lambda (x) (if (oddp x) #\1 #\0)) '(1 2 3 4))
  "1010")
(deftest map-nil
    (let ((v (vector 1 2 3 4))
	  (i -1))
      (and (null (map nil #'(lambda (x y)
			      (setf (svref v (incf i)) (+ x y)))
		      '(1 2 3) #(4 5 6)))
	   (equalp v #(5 7 9 4)))) t)

;;; MAP-INTO
(deftest map-into-list
    (let* ((list (list 1 2 3))
	   (result (map-into list #'+ list '(4 5 6) '(3 2 1 x))))
      (cons (eq list result) result))
  (t 8 9 10))
(deftest map-into-long-list
    (let* ((list (list 1 2 3 'x))
	   (result (map-into list #'+ '(4 5 6) list)))
      (cons (eq list result) result))
  (t 5 7 9 x))
(deftest map-into-short-list
    (let* ((list (list 1 2 3))
	   (result (map-into list #'+ '(4 5 6 x) list)))
      (cons (eq list result) result))
  (t 5 7 9))
(deftest map-into-vector
    (let* ((vector (vector 1 2 3))
	   (result (map-into vector #'+ vector '(4 5 6) '(3 2 1 x))))
      (and (eq vector result)
	   (equalp result #(8 9 10)))) t)
(deftest map-into-long-vector
    (let* ((vector (vector 1 2 3 'x))
	   (result (map-into vector #'+ '(4 5 6) vector)))
      (and (eq vector result)
	   (equalp result #(5 7 9 x)))) t)
(deftest map-into-short-vector
    (let* ((vector (vector 1 2 3))
	   (result (map-into vector #'+ '(4 5 6 x) vector)))
      (and (eq vector result)
	   (equalp result #(5 7 9)))) t)
(deftest map-into-displaced-vector
    (let* ((vector (make-array 3 :displaced-to (vector 0 1 2 3 4)
			       :displaced-index-offset 1))
	   (result (map-into vector #'+ vector '(4 5 6) '(3 2 1 x))))
      (and (eq vector result)
	   (equalp result #(8 9 10)))) t)
(deftest map-into-long-displaced-vector
    (let* ((vector (make-array 4 :displaced-to (vector 0 1 2 3 'x 4)
			       :displaced-index-offset 1
			       :fill-pointer t))
	   (result (map-into vector #'+ '(4 5 6) vector)))
      (and (eq vector result)
	   (equalp result #(5 7 9)))) t)
(deftest map-into-short-displaced-vector
    (let* ((vector (make-array 3 :displaced-to (vector 0 1 2 3 4)
			       :displaced-index-offset 1))
	   (result (map-into vector #'+ '(4 5 6 x) vector)))
      (and (eq vector result)
	   (equalp result #(5 7 9)))) t)
(deftest map-into-fill-pointer
    (let* ((vector (make-array 4 :fill-pointer 0 :initial-element 'x))
	   (result (map-into vector #'+
			     (make-array 4 :initial-contents '(1 2 3 4)
					 :fill-pointer 3)
			     '(4 5 6 7))))
      (and (eq result vector)
	   (= (fill-pointer result) 3)
	   (equalp result #(5 7 9))
	   (eq (aref vector 3) 'x))) t)
(deftest map-into-no-args
    (let ((i 0))
      (map-into (list 0 0 0) #'(lambda () (incf i))))
    (1 2 3))  

;;; SOME
(deftest some-value (some #'identity '(x y z)) x)
(deftest some-true-end (some #'oddp '(2 4 6 3)) t)
(deftest some-true-abort (some #'oddp '(3 x y z)) t)
(deftest some-false (some #'oddp '(2 4 6)) nil)
;;; EVERY
(deftest every-true (every #'oddp '(1 3 5 7)) t)
(deftest every-false-abort (every #'oddp '(2 x y z)) nil)
(deftest every-false-end (every #'oddp '(1 3 5 6)) nil)
;;; NOTANY
(deftest notany-true (notany #'oddp '(2 4 6 8)) t)
(deftest notany-false-abort (notany #'oddp '(3 x y z)) nil)
(deftest notany-false-end (notany #'oddp '(2 4 6 7)) nil)
;;; NOTEVERY
(deftest notevery-true-end (notevery #'oddp '(1 3 5 6)) t)
(deftest notevery-false-abort (notevery #'oddp '(2 x y z)) t)
(deftest notevery-false (notevery #'oddp '(1 3 5 7)) nil)

;;; REDUCE
(deftest reduce+ (reduce #'+ '(1 2 3 4)) 10)
(deftest reduce- (reduce #'- '(1 2 3 4)) -8)
(deftest reduce-end (reduce #'- '(1 2 3 4) :from-end t) -2)
(deftest reduce-empty (reduce #'+ '()) 0)
(deftest reduce-single (reduce #'+ '(3)) 3)
(deftest reduce-no-eval-first (reduce #'+ '(foo)) foo)
(deftest reduce-list (reduce #'list '(1 2 3 4)) (((1 2) 3) 4))
(deftest reduce-list-end (reduce #'list '(1 2 3 4) :from-end t) (1 (2 (3 4))))
(deftest reduce-list-init (reduce #'list '(1 2 3 4) :initial-value 'foo)
  ((((foo 1) 2) 3) 4))
(deftest reduce-list-init-end (reduce #'list '(1 2 3 4)
				      :initial-value 'foo :from-end t)
  (1 (2 (3 (4 foo)))))
(deftest reduce-key (reduce #'+ '((a . 1) (b . 2) (c . 3) (d . 4))
			    :key #'cdr) 10)
(deftest reduce-no-key-init (reduce #'+ '() :initial-value 9 :key #'cdr) 9)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 14.3 MODIFYING SEQUENCES                                     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FILL
(deftest fill-fours (fill (list 0 1 2 3 4 5) '(444))
  ((444) (444) (444) (444) (444) (444)))
(deftest fill-string (fill (copy-seq "01234") #\e :start 3) "012ee")
(deftest fill-vector
    (let ((x (vector 'a 'b 'c 'd 'e)))
      (fill x 'z :start 1 :end 3)
      (and (equalp x #(a z z d e))
	   (progn (fill x 'p)
		  (equalp x #(p p p p p))))) t)
(deftest fill-displaced-vector
    (let ((x (make-array 5 :displaced-to (vector 'x 'a 'b 'c 'd 'e 'y)
			 :displaced-index-offset 1)))
      (fill x 'z :start 1 :end 3)
      (and (equalp x #(a z z d e))
	   (progn (fill x 'p)
		  (equalp x #(p p p p p))))) t)
(deftest fill-list
    (let ((x (list 'a 'b 'c 'd 'e)))
      (fill x 'z :start 1 :end 3)
      (and (equalp x '(a z z d e))
	   (progn (fill x 'p)
		  (equalp x '(p p p p p))))) t)
(deftest fill-empty-range-vector (fill "abc" 'bad :start 1 :end 1) "abc")
(deftest fill-empty-range-list (fill '(a b c) 'bad :start 1 :end 1)
  (a b c))
(deftest fill-empty-vector (fill "" 'bad) "")
(deftest fill-null (fill nil 'bad) nil)

;;; REPLACE
(deftest replace-string
    (replace "abcdefghij" "0123456789" :start1 4 :end1 7 :start2 4)
  "abcd456hij")
(deftest replace-list
    (replace '(a b c d e f g h i j) "0123456789" :start1 4 :end1 7 :start2 4)
  (A B C D #\4 #\5 #\6 H I J))
(deftest replace-displaced
    (equalp (replace (make-array 10 :displaced-to #(x a b c d e f g h i j y)
				 :displaced-index-offset 1)
		     (make-array 10 :displaced-to "x0123456789y"
				 :displaced-index-offset 1)
		     :start1 4 :end1 7 :start2 4)
	    #(A B C D #\4 #\5 #\6 H I J)) t)
(deftest replace-eq-lists
    (let ((seq (list 0 1 2 3 4 5 6 7 8)))
      (replace seq seq :start1 2 :start2 0)
      seq)
  (0 1 0 1 2 3 4 5 6))
(deftest replace-eq-vectors
    (let ((seq (copy-seq "012345678")))
      (replace seq seq :start1 2 :start2 0)
      seq)
  "010123456")
(deftest replace-eq-end1
    (let ((seq (copy-seq "012345678")))
      (replace seq seq :start1 2 :start2 0 :end1 6)
      seq)
  "010123678")
(deftest replace-eq-end2
    (let ((seq (copy-seq "012345678")))
      (replace seq seq :start1 2 :start2 0 :end1 6 :end2 3)
      seq)
  "010125678")

;;; REMOVE, REMOVE-IF, REMOVE-IF-NOT
(deftest remove-list1 (remove 4 '(1 3 4 5 9)) (1 3 5 9))
(deftest remove-list2 (remove 4 '(1 2 4 1 3 4 5)) (1 2 1 3 5))
(deftest remove-list3 (remove 4 '(1 2 4 1 3 4 5) :count 1) (1 2 1 3 4 5))
(deftest remove-list4 (remove 4 '(1 2 4 1 3 4 5) :count 1 :from-end t)
  (1 2 4 1 3 5))
(deftest remove-list5 (remove 3 '(1 2 4 1 3 4 5) :test #'>) (4 3 4 5))
(deftest remove-list-if1 (remove-if #'oddp '(1 2 4 1 3 4 5)) (2 4 4))
(deftest remove-list-if2
    (remove-if #'evenp '(1 2 4 1 3 4 5) :count 1 :from-end t)
  (1 2 4 1 3 5))
(deftest remove-list-if-not1
    (remove-if-not #'evenp '(1 2 3 4 5 6 7 8 9) :count 2 :from-end t)
  (1 2 3 4 5 6 8))
(deftest remove-vector1 (equalp (remove 4 #(1 3 4 5 9)) #(1 3 5 9)) t)
(deftest remove-vector2 (equalp (remove 4 #(1 2 4 1 3 4 5)) #(1 2 1 3 5)) t)
(deftest remove-vector3 (equalp (remove 4 #(1 2 4 1 3 4 5) :count 1) #(1 2 1 3 4 5)) t)
(deftest remove-vector4 (equalp (remove 4 #(1 2 4 1 3 4 5) :count 1 :from-end t)
  #(1 2 4 1 3 5)) t)
(deftest remove-vector5 (equalp (remove 3 #(1 2 4 1 3 4 5) :test #'>) #(4 3 4 5)) t)
(deftest remove-vector-if1 (equalp (remove-if #'oddp #(1 2 4 1 3 4 5)) #(2 4 4)) t)
(deftest remove-vector-if2
    (equalp (remove-if #'evenp #(1 2 4 1 3 4 5) :count 1 :from-end t)
	    #(1 2 4 1 3 5)) t)
(deftest remove-vector-if-not1
    (equalp (remove-if-not #'evenp #(1 2 3 4 5 6 7 8 9) :count 2 :from-end t)
  #(1 2 3 4 5 6 8)) t)
		  
;;; DELETE, DELETE-IF, DELETE-IF-NOT
(deftest delete-list2 (delete 4 (list 1 2 4 1 3 4 5)) (1 2 1 3 5))
(deftest delete-list3 (delete 4 (list 1 2 4 1 3 4 5) :count 1) (1 2 1 3 4 5))
(deftest delete-list4 (delete 4 (list 1 2 4 1 3 4 5) :count 1 :from-end t)
  (1 2 4 1 3 5))
(deftest delete-list5 (delete 3 (list 1 2 4 1 3 4 5) :test #'>) (4 3 4 5))
(deftest delete-list-if1 (delete-if #'oddp (list 1 2 4 1 3 4 5)) (2 4 4))
(deftest delete-list-if2
    (delete-if #'evenp (list 1 2 4 1 3 4 5) :count 1 :from-end t)
  (1 2 4 1 3 5))
(deftest delete-list-if-not1
    (delete-if-not #'evenp (list 1 2 3 4 5 6 7 8 9) :count 2 :from-end t)
  (1 2 3 4 5 6 8))
(deftest delete-vector1 (equalp (delete 4 (vector 1 3 4 5 9)) #(1 3 5 9)) t)
(deftest delete-vector2 (equalp (delete 4 (vector 1 2 4 1 3 4 5)) #(1 2 1 3 5)) t)
(deftest delete-vector3 (equalp (delete 4 (vector 1 2 4 1 3 4 5) :count 1) #(1 2 1 3 4 5)) t)
(deftest delete-vector4 (equalp (delete 4 (vector 1 2 4 1 3 4 5) :count 1 :from-end t)
  #(1 2 4 1 3 5)) t)
(deftest delete-vector5 (equalp (delete 3 (vector 1 2 4 1 3 4 5) :test #'>) #(4 3 4 5)) t)
(deftest delete-vector-if1 (equalp (delete-if #'oddp (vector 1 2 4 1 3 4 5)) #(2 4 4)) t)
(deftest delete-vector-if2
    (equalp (delete-if #'evenp (vector 1 2 4 1 3 4 5) :count 1 :from-end t)
	    #(1 2 4 1 3 5)) t)
(deftest delete-vector-if-not1
    (equalp (delete-if-not #'evenp (vector 1 2 3 4 5 6 7 8 9) :count 2 :from-end t)
  #(1 2 3 4 5 6 8)) t)

;;; REMOVE-DUPLICATES
(deftest remove-duplicates1
    (not (null (string= (remove-duplicates "aBcDAbCd" :test #'char-equal :from-end t)
			"aBcD"))) t)
(deftest remove-duplicates2 
    (remove-duplicates '(a b c b d d e)) (a c b d e))
(deftest remove-duplicates3
    (remove-duplicates '(a b c b d d e) :from-end t) (a b c d e))
(deftest remove-duplicates4
    (remove-duplicates '((foo #\a) (bar #\%) (baz #\A))
		       :test #'char-equal :key #'cadr)
  ((bar #\%) (baz #\A)))
(deftest remove-duplicates5
    (remove-duplicates '((foo #\a) (bar #\%) (baz #\A))
		       :test #'char-equal :key #'cadr :from-end t)
  ((foo #\a) (bar #\%)))
(deftest remove-duplicates6
    (remove-duplicates '(0 1 2 3 4 5 6) :key #'oddp :start 1 :end 6)
  (0 4 5 6))
(deftest remove-duplicates7
    (remove-duplicates '(0 1 2 3 4 5 6) :key #'oddp :start 1 :end 6 :from-end t)
  (0 1 2 6))

;;; DELETE-DUPLICATES
(deftest delete-duplicates1
    (not (null (string= (delete-duplicates (copy-seq "aBcDAbCd") :test #'char-equal :from-end t)
			"aBcD"))) t)
(deftest delete-duplicates2 
    (delete-duplicates (copy-seq '(a b c b d d e))) (a c b d e))
(deftest delete-duplicates3
    (delete-duplicates (copy-seq '(a b c b d d e)) :from-end t) (a b c d e))
(deftest delete-duplicates4
    (delete-duplicates (copy-seq '((foo #\a) (bar #\%) (baz #\A)))
		       :test #'char-equal :key #'cadr)
  ((bar #\%) (baz #\A)))
(deftest delete-duplicates5
    (delete-duplicates (copy-seq '((foo #\a) (bar #\%) (baz #\A)))
		       :test #'char-equal :key #'cadr :from-end t)
  ((foo #\a) (bar #\%)))
(deftest delete-duplicates6
    (delete-duplicates (list 0 1 2 3 4 5 6) :key #'oddp :start 1 :end 6)
  (0 4 5 6))
(deftest delete-duplicates7
    (delete-duplicates (list 0 1 2 3 4 5 6) :key #'oddp :start 1 :end 6 :from-end t)
  (0 1 2 6))

;;; SUBSTITUTE, SUBSTITUTE-IF, SUBSTITUTE-IF-NOT
(deftest substitute1 (substitute #\. #\space "0 2 4 6") "0.2.4.6")
(deftest substitute2 (substitute  9 4 '(1 2 4 1 3 4 5)) (1 2 9 1 3 9 5))
(deftest substitute3 (substitute  9 4 '(1 2 4 1 3 4 5) :count 1)
  (1 2 9 1 3 4 5))
(deftest substitute4 (substitute  9 4 '(1 2 4 1 3 4 5) :count 1 :from-end t)
  (1 2 4 1 3 9 5))
(deftest substitute5 (substitute  9 3 '(1 2 4 1 3 4 5) :test #'>)
  (9 9 4 9 3 4 5))
(deftest substitute-if1
    (substitute-if 0 #'evenp '((1) (2) (3) (4)) :start 2 :key #'car)
  ((1) (2) (3) 0))
(deftest substitute-if2
    (substitute-if 9 #'oddp '(1 2 4 1 3 4 5)) (9 2 4 9 9 4 9))
(deftest substitute-if3
    (substitute-if 9 #'evenp '(1 2 4 1 3 4 5) :count 1 :from-end t)
  (1 2 4 1 3 9 5))
(deftest nsubstitute1
    (not (null (string= (nsubstitute #\. #\space (copy-seq "0 2 4 6")) "0.2.4.6")))
  t)
(deftest nsubstitute2 (nsubstitute  9 4 (list 1 2 4 1 3 4 5)) (1 2 9 1 3 9 5))
(deftest nsubstitute3 (nsubstitute  9 4 (list 1 2 4 1 3 4 5) :count 1)
  (1 2 9 1 3 4 5))
(deftest nsubstitute4 (nsubstitute  9 4 (list 1 2 4 1 3 4 5) :count 1 :from-end t)
  (1 2 4 1 3 9 5))
(deftest nsubstitute5 (nsubstitute  9 3 (list 1 2 4 1 3 4 5) :test #'>)
  (9 9 4 9 3 4 5))
(deftest nsubstitute-if1
    (nsubstitute-if 0 #'evenp (copy-tree '((1) (2) (3) (4))) :start 2 :key #'car)
  ((1) (2) (3) 0))
(deftest nsubstitute-if2
    (nsubstitute-if 9 #'oddp (list 1 2 4 1 3 4 5)) (9 2 4 9 9 4 9))
(deftest nsubstitute-if3
    (nsubstitute-if 9 #'evenp (list 1 2 4 1 3 4 5) :count 1 :from-end t)
  (1 2 4 1 3 9 5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 14.4 SEARCHING SEQUENCES FOR ITEMS                           ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FIND, FIND-IF, FIND-IF-NOT
(deftest find (find #\d "here are some letters abcde" :test #'char>)
  #\space)
(deftest find-if (find-if #'oddp '(1 2 3 4 5) :end 3 :from-end t) 3)
(deftest find-if-not (find-if-not #'symbolp #(1 2 x y) :start 2) nil)

;;; POSITION, POSITION-IF, POSITION-IF-NOT
(deftest position (position #\a "baobab" :from-end t) 4)
(deftest position-if
    (position-if #'oddp '((1) (2) (3) (4)) :start 1 :key #'car) 2)
(deftest position-empty (position 595 nil) nil)
(deftest position-if-not (position-if-not #'symbolp '(a b c d 5)) 4)

;;; COUNT, COUNT-IF, COUNT-IF-NOT
(deftest count (count #\a "how many A's are there in here?") 2)
(deftest count2 (count 2 '(0 1 2 3 2 4 2) :start 3 :end 5) 1)
(deftest count-if
    (count-if #'upper-case-p "The Crying of Lot 49" :start 4) 2)
(deftest count-if2
    (count-if #'upper-case-p "The Crying of Lot 49" :start 5) 1)
(deftest count-if-not (count-if-not #'oddp '((1) (2) (3) (4)) :key #'car) 2)
(deftest count-effect
    (let ((order nil))
      (cons (count-if #'oddp '(1 2 3 4 5 6 7)
		      :key #'(lambda (x) (push x order) x)
		      :from-end t)
	    order))
  (4 1 2 3 4 5 6 7))

;;; MISMATCH
(deftest mismatch-short (mismatch "abcd" "ABCDE" :test #'char-equal) 4)
(deftest mismatch-end
    (mismatch '(3 2 1 1 2 3) '(1 2 3) :from-end t) 3)
(deftest mismatch-not-key
    (mismatch '(1 2 3) '(2 3 4) :test-not #'eq :key #'oddp) nil)
(deftest mismatch-bound
    (mismatch '(1 2 3 4 5 6) '(3 4 5 6 7) :start1 2 :end2 4) nil)
(deftest mismatch-key-applied-to-both (mismatch '(0 1) '(6 1) :key #'oddp) nil)
(deftest mismatch-forward
  (mismatch "accd" "abcd" :from-end nil) 1)
(deftest mismatch-backward
  (mismatch "accd" "abcd" :from-end t) 2)

;;; SEARCH
(deftest search-identical-string (search "abc" "abc") 0)
(deftest search-identical-list (search '(1 2 3) '(1 2 3)) 0)
(deftest search-identical-vector (search #(1 2 3) #(1 2 3)) 0)
(deftest search-forward (search "ABC" "xyzABCxyzABCxyz") 3)
(deftest search-forward-cut (search "ABC" "xyzABC") 3)
(deftest search-backward (search "ABC" "xyzABCxyzABCxyz" :from-end t) 9)
(deftest search-forward-start
  (search "pdqABCmno" "xyzABCxyzABCxyz" :start1 3 :end1 6 :start2 4) 9)
(deftest search-forward-end
  (search "pdqABCmno" "xyzABCxyzABCxyz" :start1 3 :end1 6
	    :start2 4 :end2 9) nil)
(deftest search-list (search '(1 2) '(0 1 2 3 1 2 3)) 1)
(deftest search-list-from-end (search '(1 2) '(0 1 2 3 1 2 3) :from-end t) 4)
(deftest search-vector (search '(1 2) #(0 1 2 3 1 2 3)) 1)
(deftest search-vector-from-end (search '(1 2) #(0 1 2 3 1 2 3) :from-end t) 4)
(deftest search-key (search '(0 1) '(2 4 6 1 3 5) :key #'oddp) 2)
(deftest search-past (search "NORMAL" "TV-EMULATION") nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 14.5 SORTING AND MERGING                                     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MERGE
(deftest merge-list
    (merge 'list (list 1 3 4 6 7) (list 2 5 8) #'<)
  (1 2 3 4 5 6 7 8))
(deftest merge-string
    (merge 'string (copy-seq "BOY") (copy-seq "nosy") #'char-lessp)
  "BnOosYy")
(deftest merge-key
    (equalp (merge 'vector
		   (vector '(red . 1) '(blue . 4))
		   (list '(yellow . 2) '(green . 7))
		   #'< :key #'cdr)
	    #((red . 1) (yellow . 2) (blue . 4) (green . 7))) t)


;;; SORT, STABLE-SORT
(deftest sort1
    (sort (copy-seq "lkjashd") #'char-lessp)
  "adhjkls")
(deftest sort2
    (sort (list '(1 2 3) '(4 5 6) '(7 8 9)) #'> :key #'car)
  ((7 8 9) (4 5 6) (1 2 3)))
(deftest stable-sort1
    (stable-sort (list 1 2 3 4 5 6 7 8 9 0)
		 #'(lambda (x y) (and (oddp x) (evenp y))))
  (1 3 5 7 9 2 4 6 8 0))
(deftest stable-sort2
    (equalp (stable-sort (vector 1 2 3 4 5 6 7 8 9 0)
		 #'(lambda (x y) (and (oddp x) (evenp y))))
	     #(1 3 5 7 9 2 4 6 8 0)) t)
(deftest stable-sort-sort
    (let ((committee-data
	   (vector (list (list "JonL" "White") "Iteration")
		      (list (list "Dick" "Waters") "Iteration")
		      (list (list "Dick" "Gabriel") "Objects")
		      (list (list "Kent" "Pitman") "Conditions")
		      (list (list "Gregor" "Kickzales") "Objects")
		      (list (list "David" "Moon") "Objects")
		      (list (list "Kathy" "Chapman") "Editorial")
		      (list (list "Larry" "Masinter") "Cleanup")
		      (list (list "Sandra" "Loosemore") "Compiler"))))
      (sort committee-data #'string-lessp :key #'cadar)
      (equalp (stable-sort committee-data #'string-lessp :key #'cadr)
	      #((("Larry" "Masinter") "Cleanup")
		(("Sandra" "Loosemore") "Compiler")
		(("Kent" "Pitman") "Conditions") 
		(("Kathy" "Chapman") "Editorial")
		(("Dick" "Waters") "Iteration")
		(("JonL" "White") "Iteration")
		(("Dick" "Gabriel") "Objects")
		(("Gregor" "Kickzales") "Objects")
		(("David" "Moon") "Objects")))) t)
