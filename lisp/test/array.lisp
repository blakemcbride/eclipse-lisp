;;; Still need:
;;; - adjust-array tests with displaced arrays and with fill-pointers.
;;; - bit-operations on (doubly) displaced arrays with an (multiple) offset

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 17.1 ARRAY CREATION                                          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun report-array (a &aux elements)
  (dotimes (i (array-total-size a) (list (array-dimensions a) (nreverse elements)))
    (push (row-major-aref a i) elements)))

;;; MAKE-ARRAY
(deftest make-array-rank-0-nil
    (report-array (make-array '() :initial-element nil))
  (() (nil)))
(deftest make-array-rank-0-99
    (report-array (make-array '() :initial-element 99))
  (() (99)))
(deftest make-array-rank-1-nil
    (report-array (make-array '(4) :initial-element nil))
  ((4) (nil nil nil nil)))
(deftest make-array-rank-1-99
    (report-array (make-array '(4) :initial-element 99))
  ((4) (99 99 99 99)))
(deftest make-array-rank-1-integer
    (report-array (make-array 4 :initial-element 99))
  ((4) (99 99 99 99)))
(deftest make-array-rank-2-nil
    (report-array (make-array '(2 3) :initial-element nil))
  ((2 3) (nil nil nil nil nil nil)))
(deftest make-array-rank-2-99
    (report-array (make-array '(2 3) :initial-element 99))
  ((2 3) (99 99 99 99 99 99)))
(deftest make-array-rank-3-nil
    (report-array (make-array '(1 2 3) :initial-element nil))
  ((1 2 3) (nil nil nil nil nil nil)))
(deftest make-array-rank-3-99
    (report-array (make-array '(1 2 3) :initial-element 99))
  ((1 2 3) (99 99 99 99 99 99)))
(deftest make-array-rank-3-nil-a
    (report-array (make-array '(2 2 3) :initial-element nil))
  ((2 2 3) (nil nil nil nil nil nil nil nil nil nil nil nil)))
(deftest make-array-rank-3-99-a 
    (report-array (make-array '(2 2 3) :initial-element 99))
  ((2 2 3) (99 99 99 99 99 99 99 99 99 99 99 99)))
(deftest make-array-rank-3-0
    (report-array (make-array '(0 2 3) :initial-element 99))
  ((0 2 3) ()))

(deftest make-array-rank-0
    (report-array (make-array '() :initial-contents nil))
  (() (nil)))
(deftest make-array-rank-0-99-again
    (report-array (make-array '() :initial-contents 99))
  (() (99)))
(deftest make-array-rank-1
    (report-array (make-array '(4) :initial-contents '(1 2 3 4)))
  ((4) (1 2 3 4)))
(deftest make-array-rank-2
    (report-array (make-array '(2 3) :initial-contents '((1 2 3) (4 5 6))))
  ((2 3) (1 2 3 4 5 6)))
(deftest make-array-rank-3
    (report-array (make-array '(1 2 3) :initial-contents '(((1 2 3) (4 5 6)))))
  ((1 2 3) (1 2 3 4 5 6)))
(deftest make-array-rank-3-a
    (report-array (make-array '(2 2 3)
			      :initial-contents '(((1 2 3) (4 5 6))
						  ((7 8 9) (10 11 12)))))
  ((2 2 3) (1 2 3 4 5 6 7 8 9 10 11 12)))
(deftest make-array-rank-3-none
    (report-array (make-array '(0 2 3) :initial-contents '()))
  ((0 2 3) ()))

(deftest make-array-rank-1-v
    (report-array (make-array '(4) :initial-contents #(1 2 3 4)))
  ((4) (1 2 3 4)))
(deftest make-array-rank-2-v
    (report-array (make-array '(2 3) :initial-contents #(#(1 2 3) #(4 5 6))))
  ((2 3) (1 2 3 4 5 6)))
(deftest make-array-rank-3-v
    (report-array (make-array '(1 2 3) :initial-contents #(#(#(1 2 3) #(4 5 6)))))
  ((1 2 3) (1 2 3 4 5 6)))
(deftest make-array-rank-3-a-v
    (report-array (make-array '(2 2 3)
			      :initial-contents #(#(#(1 2 3) #(4 5 6))
						  #(#(7 8 9) #(10 11 12)))))
  ((2 2 3) (1 2 3 4 5 6 7 8 9 10 11 12)))
(deftest make-array-rank-3-none-v
    (report-array (make-array '(0 2 3) :initial-contents #()))
  ((0 2 3) ()))

;;; ARRAY-RANK-LIMIT, ARRAY-DIMENSION-LIMIT, ARRAY-TOTAL-SIZE-LIMIT
(deftest array-rank-limit
    (and (typep array-rank-limit 'fixnum)
	 (>= array-rank-limit 8)) t)
(deftest array-dimension-limit
    (and (typep array-dimension-limit 'fixnum)
	 (>= array-total-size-limit 1024)) t)
(deftest array-total-size-limit
    (and (typep array-total-size-limit 'fixnum)
	 (>= array-total-size-limit 1024)) t)

;;; VECTOR
(deftest vector1 (let ((v (vector)))
		   (and (vectorp v)
			(= (length v) 0))) t)
(deftest vector2 (let ((v (vector 'a 'b 'c)))
		   (and (vectorp v)
			(= (length v) 3)
			(eq (svref v 0) 'a)
			(eq (svref v 1) 'b)
			(eq (svref v 2) 'c)))
  t)
(deftest vector3 (let ((v (vector #\a #\b #\c)))
		   (and (vectorp v)
			(= (length v) 3)
			(eq (svref v 0) #\a)
			(eq (svref v 1) #\b)
			(eq (svref v 2) #\c)))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 17.2 ARRAY ACCESS                                            ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; AREF
(deftest aref-0
    (let* ((a (make-array () :initial-contents 9))
	   (init (aref a))
	   (set (setf (aref a) 'sirens)))
      (list init set (aref a)))
  (9 sirens sirens))
(deftest aref-1
    (let* ((a (make-array 4 :initial-contents '(1 2 3 4)))
	   (init (aref a 3))
	   (set (setf (aref a 3) 'sirens)))
      (list init set (aref a 3)))
  (4 sirens sirens))
(deftest aref-fill
    (let* ((a (make-array 4 :initial-contents '(1 2 3 4)
			  :fill-pointer 0))
	   (init (aref a 3))
	   (set (setf (aref a 3) 'sirens)))
      (list init set (aref a 3)))
  (4 sirens sirens))
(deftest aref-2
    (let* ((a (make-array '(2 4) :initial-contents '((1 2 3 4)
						     (5 6 7 8))))
	   (init (aref a 0 2))
	   (set (setf (aref a 0 2) 'sirens)))
      (list init set (aref a 0 2)))
  (3 sirens sirens))

(deftest aref-0-base-char
    (let* ((a (make-array () :element-type 'base-char
			  :initial-contents #\a))
	   (init (aref a))
	   (set (setf (aref a) #\b)))
      (list init set (aref a)))
  (#\a #\b #\b))
(deftest aref-1-base-char
    (let* ((a (make-array 4 :element-type 'base-char
			  :initial-contents '(#\a #\b #\c #\d)))
	   (init (aref a 3))
	   (set (setf (aref a 3) #\z)))
      (list init set (aref a 3)))
  (#\d #\z #\z))
(deftest aref-fill-base-char
    (let* ((a (make-array 4 :element-type 'base-char
			  :initial-contents '(#\a #\b #\c #\d)
			  :fill-pointer 0))
	   (init (aref a 3))
	   (set (setf (aref a 3) #\z)))
      (list init set (aref a 3)))
  (#\d #\z #\z))
(deftest aref-2-base-char
    (let* ((a (make-array '(2 4) :element-type 'base-char
			   :initial-contents '((#\a #\b #\c #\d)
					       (#\e #\f #\g #\h))))
	   (init (aref a 1 2))
	   (set (setf (aref a 0 2) #\z)))
      (list init set (aref a 0 2)))
  (#\g #\z #\z))

(deftest aref-0-character
    (let* ((a (make-array () :element-type 'character
			  :initial-contents #\a))
	   (init (aref a))
	   (set (setf (aref a) #\b)))
      (list init set (aref a)))
  (#\a #\b #\b))
(deftest aref-1-character
    (let* ((a (make-array 4 :element-type 'character
			  :initial-contents '(#\a #\b #\c #\d)))
	   (init (aref a 3))
	   (set (setf (aref a 3) #\z)))
      (list init set (aref a 3)))
  (#\d #\z #\z))
(deftest aref-fill-character
    (let* ((a (make-array 4 :element-type 'character
			  :initial-contents '(#\a #\b #\c #\d)
			  :fill-pointer 0))
	   (init (aref a 3))
	   (set (setf (aref a 3) #\z)))
      (list init set (aref a 3)))
  (#\d #\z #\z))
(deftest aref-2-character
    (let* ((a (make-array '(2 4) :element-type 'character
			   :initial-contents '((#\a #\b #\c #\d)
					       (#\e #\f #\g #\h))))
	   (init (aref a 1 2))
	   (set (setf (aref a 0 2) #\z)))
      (list init set (aref a 0 2)))
  (#\g #\z #\z))

(deftest aref-0-bit
    (let* ((a (make-array () :element-type 'bit
			  :initial-contents 1))
	   (init (aref a))
	   (set (setf (aref a) 0)))
      (list init set (aref a)))
  (1 0 0))
(deftest aref-1-bit
    (let* ((a (make-array 4 :element-type 'bit
			  :initial-contents '(1 0 1 0)))
	   (init (aref a 3))
	   (set (setf (aref a 3) 1)))
      (list init set (aref a 3)))
  (0 1 1))
(deftest aref-fill-bit
    (let* ((a (make-array 4 :element-type 'bit
			  :initial-contents '(1 0 1 0)
			  :fill-pointer 0))
	   (init (aref a 3))
	   (set (setf (aref a 3) 1)))
      (list init set (aref a 3)))
  (0 1 1))
(deftest aref-2-bit
    (let* ((a (make-array '(2 4) :element-type 'bit
			   :initial-contents '((1 0 1 0)
					       (0 1 0 1))))
	   (init (aref a 1 2))
	   (set (setf (aref a 1 2) 1)))
      (list init set (aref a 0 2)))
  (0 1 1))

;;; SVREF
(deftest svref-t
    (let* ((a (make-array 4 :initial-contents '(1 2 3 4)))
	   (init (svref a 3))
	   (set (setf (svref a 3) 'sirens)))
      (list init set (svref a 3)))
  (4 sirens sirens))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 17.3 ARRAY INFORMATION                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ARRAY-ELEMENT-TYPE
(deftest array-element-type-bit-1
    (array-element-type (make-array 3 :element-type 'bit)) bit)
(deftest array-element-type-base-char-1
    (array-element-type (make-array 3 :element-type 'base-char)) base-char)
(deftest array-element-type-character-1
    (subtypep (array-element-type (make-array 3 :element-type 'character)) 'character)
  t t)
(deftest array-element-type-t-1
    (array-element-type (make-array 3 :element-type 't)) t)

(deftest array-element-type-bit-2
    (array-element-type (make-array '(2 3) :element-type 'bit)) bit)
(deftest array-element-type-base-char-2
    (array-element-type (make-array '(2 3) :element-type 'base-char)) base-char)
(deftest array-element-type-character-2
    (subtypep (array-element-type (make-array '(2 3) :element-type 'character)) 'character)
  t t)
(deftest array-element-type-t-2
    (array-element-type (make-array '(2 3) :element-type 't)) t)

(deftest array-element-type-bit-fill-pointer
    (array-element-type (make-array 3 :fill-pointer 0
				    :element-type 'bit)) bit)
(deftest array-element-type-base-char-fill-pointer
    (array-element-type (make-array 3 :fill-pointer 0
				    :element-type 'base-char)) base-char)
(deftest array-element-type-character-fill-pointer
    (subtypep (array-element-type (make-array 3 :fill-pointer 0
					      :element-type 'character))
	      'character)
  t t)
(deftest array-element-type-t-fill-pointer
    (array-element-type (make-array 3 :fill-pointer 0
				    :element-type 't)) t)

(deftest array-element-type-t-displaced
    (array-element-type (make-array 3 
				    :displaced-to (make-array 3 :element-type 't)
				    :element-type 't)) t)
(deftest array-element-type-bit-displaced
    (array-element-type (make-array 3 
				    :displaced-to (make-array 3 :element-type 'bit)
				    :element-type 'bit)) bit)
(deftest array-element-type-base-char-displaced
    (array-element-type (make-array 3 
				    :displaced-to (make-array 3 :element-type 'base-char)
				    :element-type 'base-char)) base-char)
(deftest array-element-type-character-displaced
    (subtypep (array-element-type (make-array 3 
					      :displaced-to (make-array 3 :element-type 'character)
					      :element-type 'character))
	      'character)
  t t)


;;; ARRAY-RANK
(deftest array-rank-0 (array-rank (make-array '())) 0)
(deftest array-rank-0-adjustable
    (array-rank (make-array '() :adjustable t)) 0)
(deftest array-rank-1 (array-rank (make-array '(4))) 1)
(deftest array-rank-1-adjustable
    (array-rank (make-array '(4) :adjustable t)) 1)
(deftest array-rank-2 (array-rank (make-array '(2 3))) 2)
(deftest array-rank-2-adjustable
    (array-rank (make-array '(2 3) :adjustable t)) 2)

;;; ARRAY-DIPLACEMENT
(deftest array-displacement-nil
    (array-displacement (make-array 4)) nil 0)
(deftest array-displacement-t
	  (let* ((a (make-array 4))
		 (b (make-array 4 :displaced-to a))
		 (c (make-array 3 :displaced-to b
				:displaced-index-offset 1)))
	    (multiple-value-bind (ad ai)
		(array-displacement a)
	      (multiple-value-bind (bd bi)
		(array-displacement b)
		(multiple-value-bind (cd ci)
		    (array-displacement c)
		  (list ad ai (eq bd a) bi (eq cd b) ci)))))
  (nil 0 t 0 t 1))

;;; ARRAY-DIMENSION
(deftest array-dimension-1
    (array-dimension (make-array 4) 0) 4)
(deftest array-dimension-1-fill
    (array-dimension (make-array 4 :fill-pointer 0) 0) 4)
(deftest array-dimension-1-adjustable
    (array-dimension (make-array 4 :adjustable t) 0) 4)
(deftest array-dimension-2
    (array-dimension (make-array '(2 3)) 0) 2)
(deftest array-dimension-2-adjustable
    (array-dimension (make-array '(2 3) :adjustable t) 1) 3)
(deftest array-dimension-3
    (array-dimension (make-array '(2 3 4)) 2) 4)
(deftest array-dimension-3-adjustable
    (array-dimension (make-array '(2 3 4) :adjustable t) 1) 3)

;;; ARRAY-DIMENSIONS
;;; See make-array, above

;;; ARRAY-TOTAL-SIZE
(deftest array-total-size-0 (array-total-size (make-array '())) 1)
(deftest array-total-size-0-adjustable
    (array-total-size (make-array '() :adjustable t)) 1)
(deftest array-total-size-1 (array-total-size (make-array '(4))) 4)
(deftest array-total-size-1-adjustable
    (array-total-size (make-array '(4) :adjustable t)) 4)
(deftest array-total-size-1-fill-pointer
    (array-total-size (make-array '(4) :fill-pointer 0)) 4)
(deftest array-total-size-2 (array-total-size (make-array '(2 3))) 6)
(deftest array-total-size-2-adjustable
    (array-total-size (make-array '(2 3) :adjustable t)) 6)
(deftest array-total-size-3 (array-total-size (make-array '(2 3 4))) 24)
(deftest array-total-size-3-adjustable
    (array-total-size (make-array '(2 3 4) :adjustable t)) 24)

;;; ARRAY-IN-BOUNDS-P
(deftest array-in-bounds-p-0
    (array-in-bounds-p (make-array '())) t)
(deftest array-in-bounds-p-0-adjustable
    (array-in-bounds-p (make-array '() :adjustable t)) t)
(deftest array-in-bounds-p-1
    (let ((a (make-array 4)))
      (list (array-in-bounds-p a -1)
	      (array-in-bounds-p a 0)
	      (array-in-bounds-p a 3)
	      (array-in-bounds-p a 4)))
  (nil t t nil))
(deftest array-in-bounds-p-1-adjustable
    (let ((a (make-array 4 :adjustable t)))
      (list (array-in-bounds-p a -1)
	      (array-in-bounds-p a 0)
	      (array-in-bounds-p a 3)
	      (array-in-bounds-p a 4)))
  (nil t t nil))
(deftest array-in-bounds-p-1-fill
    (let ((a (make-array 4 :fill-pointer 0)))
      (list (array-in-bounds-p a -1)
	      (array-in-bounds-p a 0)
	      (array-in-bounds-p a 3)
	      (array-in-bounds-p a 4)))
  (nil t t nil))
(deftest array-in-bounds-p-2
    (let ((a (make-array '(2 3))))
      (list (array-in-bounds-p a 0 -1)
	      (array-in-bounds-p a 0 0)
	      (array-in-bounds-p a 0 2)
	      (array-in-bounds-p a 0 3)))
  (nil t t nil))
(deftest array-in-bounds-p-2-adjustable
    (let ((a (make-array '(2 3) :adjustable t)))
      (list (array-in-bounds-p a -1 0)
	      (array-in-bounds-p a 0 0)
	      (array-in-bounds-p a 1 0)
	      (array-in-bounds-p a 2 0)))
  (nil t t nil))
(deftest array-in-bounds-p-3
    (let ((a (make-array '(2 3 4))))
      (list (array-in-bounds-p a 0 0 -1)
	      (array-in-bounds-p a 0 0 0)
	      (array-in-bounds-p a 0 0 3)
	      (array-in-bounds-p a 0 0 4)))
  (nil t t nil))
(deftest array-in-bounds-p-3-adjustable
    (let ((a (make-array '(2 3 4) :adjustable t)))
      (list (array-in-bounds-p a -1 0 0)
	      (array-in-bounds-p a 0 0 0)
	      (array-in-bounds-p a 1 0 0)
	      (array-in-bounds-p a 2 0 0)))
  (nil t t nil))


;;; ADJUSTABLE-ARRAY-P
(deftest adjustable-array-p-0-nil
    (adjustable-array-p (make-array '())) nil)
(deftest adjustable-array-p-0-t
    (adjustable-array-p (make-array '() :adjustable t)) t)
(deftest adjustable-array-p-1-nil
    (adjustable-array-p (make-array 4)) nil)
(deftest adjustable-array-p-1-t
    (adjustable-array-p (make-array 4 :adjustable t)) t)

;;; ARRAY-ROW-MAJOR-INDEX
(deftest array-row-major-index-0
    (array-row-major-index (make-array '())) 0)
(deftest array-row-major-index-0-adjustable
    (array-row-major-index (make-array '()
				       :adjustable t)) 0)
(deftest array-row-major-index-1
    (array-row-major-index (make-array 4) 3) 3)
(deftest array-row-major-index-1-adjustable
    (array-row-major-index (make-array '(4)
				       :displaced-to (make-array '(6))
				       :displaced-index-offset 2
				       :adjustable t) 3) 3)
(deftest array-row-major-index-1-fill
    (array-row-major-index (make-array '(4) :fill-pointer 0) 3) 3)
(deftest array-row-major-index-2
    (array-row-major-index (make-array '(2 3)) 1 2) 5)
(deftest array-row-major-index-2-adjustable
    (array-row-major-index (make-array '(2 3)
				       :displaced-to (make-array 8)
				       :displaced-index-offset 2
				       :adjustable t) 0 1) 1)
(deftest array-row-major-index-3
    (array-row-major-index (make-array '(2 3 4)) 1 1 1) 17)
(deftest array-row-major-index-3-adjustable
    (array-row-major-index (make-array '(2 3 4)
				       :displaced-to (make-array 26)
				       :displaced-index-offset 2
				       :adjustable t) 1 2 3) 23)

;;; ROW-MAJOR-AREF
;;; See make-array, above.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 17.4 FUNCTIONS ON ARRAYS OF BITS                             ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BIT
(deftest bit-1
    (let ((a (make-array 3 :element-type 'bit
				:initial-contents '(0 0 1))))
      (values (bit a 2) (setf (bit a 2) 0) (bit a 2)))
  1 0 0)
(deftest bit-1-clr
    (let ((a (make-array 3 :element-type 'bit
				:initial-contents '(1 1 0))))
      (values (bit a 2) (setf (bit a 2) 1) (bit a 2)))
  0 1 1)
(deftest bit-1-fill
    (let ((a (make-array 3 :element-type 'bit
				     :fill-pointer 0
				     :initial-contents '(0 0 1))))
      (values (bit a 2) (setf (bit a 2) 0) (bit a 2)))
  1 0 0)
(deftest bit-1-disp
    (let ((a (make-array 3 :element-type 'bit
				     :displaced-to
				     (make-array 5
						 :element-type 'bit
						 :initial-contents '(0 0 0 0 1))
				     :displaced-index-offset 2)))
      (values (bit a 2) (setf (bit a 2) 0) (bit a 2)))
  1 0 0)

(deftest bit-0
  (let ((a (make-array '() :element-type 'bit
		       :initial-contents 1)))
      (values (bit a) (setf (bit a) 0) (bit a)))
  1 0 0)

(deftest bit-2
  (let ((a (make-array '(2 3) :element-type 'bit
		       :initial-contents '((0 0 0)
					   (0 0 1)))))
      (values (bit a 1 2) (setf (bit a 1 2) 0) (bit a 1 2)))
  1 0 0)

;;; SBIT
(deftest sbit-1
    (let ((a (make-array 3 :element-type 'bit
			 :initial-contents '(0 0 1))))
      (values (sbit a 2) (setf (sbit a 2) 0) (sbit a 2)))
  1 0 0)
(deftest sbit-1-clr
    (let ((a (make-array 3 :element-type 'bit
				:initial-contents '(1 1 0))))
      (values (sbit a 2) (setf (sbit a 2) 1) (sbit a 2)))
  0 1 1)
#-(and cmu (not eclipse))
(deftest sbit-0
  (let ((a (make-array '() :element-type 'bit
		       :initial-contents 1)))
    (values (sbit a) (setf (sbit a) 0) (sbit a)))
  1 0 0)
#-(and cmu (not eclipse))
(deftest sbit-2
  (let ((a (make-array '(2 3) :element-type 'bit
		       :initial-contents '((0 0 0)
					   (0 0 1)))))
    (values (sbit a 1 2) (setf (sbit a 1 2) 0) (sbit a 1 2)))
  1 0 0)

(defparameter *bit-array1*
  (make-array '(2 2) :element-type 'bit
		:initial-contents '((0 0) (1 1))))
(defparameter *bit-array2*
    (make-array '(2 2) :element-type 'bit
		:initial-contents '((0 1) (0 1))))

(defparameter *displaced-bit-array1*
    (make-array '(2 2) :element-type 'bit
		:displaced-to
		(make-array 8 :element-type 'bit
			    :initial-contents '(1 1 0 0 1 1 1 1))
		:displaced-index-offset 2))

(defparameter *displaced-bit-array2*
    (make-array '(2 2) :element-type 'bit
		:displaced-to
		(make-array 10 :element-type 'bit
			    :initial-contents '(0 1 1 0 1 0 1 1 1 0))
		:displaced-index-offset 3))

(defparameter *displaced-bit-array3*
    (make-array '(2 2) :element-type 'bit
		:displaced-to
		(make-array 12 :element-type 'bit
			    :initial-contents '(1 1 1 1 0 0 0 0 1 1 1 1))
		:displaced-index-offset 4))

;;; BIT-AND, bit-ior, bit-xor, bit-eqv, bit-nand, bit-nor, bit-andc1,
;;; bit-andc2, bit-orc1, bit-orc2, bit-not
(deftest bit-and
    (report-array (bit-and *bit-array1* *bit-array2*))
  ((2 2) (0 0 0 1)))
(deftest bit-ior
    (report-array (bit-ior *bit-array1* *bit-array2*))
  ((2 2) (0 1 1 1)))
(deftest bit-xor
    (report-array (bit-xor *bit-array1* *bit-array2*))
  ((2 2) (0 1 1 0)))
(deftest bit-eqv
    (report-array (bit-eqv *bit-array1* *bit-array2*))
  ((2 2) (1 0 0 1)))
(deftest bit-nand
    (report-array (bit-nand *bit-array1* *bit-array2*))
  ((2 2) (1 1 1 0)))
(deftest bit-nor
    (report-array (bit-nor *bit-array1* *bit-array2*))
  ((2 2) (1 0 0 0)))
(deftest bit-andc1
    (report-array (bit-andc1 *bit-array1* *bit-array2*))
  ((2 2) (0 1 0 0)))
(deftest bit-andc2
    (report-array (bit-andc2 *bit-array1* *bit-array2*))
  ((2 2) (0 0 1 0)))
(deftest bit-orc1
    (report-array (bit-orc1 *bit-array1* *bit-array2*))
  ((2 2) (1 1 0 1)))
(deftest bit-orc2
    (report-array (bit-orc2 *bit-array1* *bit-array2*))
  ((2 2) (1 0 1 1)))
(deftest bit-not (report-array (bit-not *bit-array1*))
  ((2 2) (1 1 0 0)))

(deftest displaced-bit-and
    (report-array (bit-and *displaced-bit-array1* *displaced-bit-array2* *displaced-bit-array3*))
  ((2 2) (0 0 0 1)))
(deftest displaced-bit-ior
    (report-array (bit-ior *displaced-bit-array1* *displaced-bit-array2* *displaced-bit-array3*))
  ((2 2) (0 1 1 1)))
(deftest displaced-bit-xor
    (report-array (bit-xor *displaced-bit-array1* *displaced-bit-array2* *displaced-bit-array3*))
  ((2 2) (0 1 1 0)))
(deftest displaced-bit-eqv
    (report-array (bit-eqv *displaced-bit-array1* *displaced-bit-array2* *displaced-bit-array3*))
  ((2 2) (1 0 0 1)))
(deftest displaced-bit-nand
    (report-array (bit-nand *displaced-bit-array1* *displaced-bit-array2* *displaced-bit-array3*))
  ((2 2) (1 1 1 0)))
(deftest displaced-bit-nor
    (report-array (bit-nor *displaced-bit-array1* *displaced-bit-array2* *displaced-bit-array3*))
  ((2 2) (1 0 0 0)))
(deftest displaced-bit-andc1
    (report-array (bit-andc1 *displaced-bit-array1* *displaced-bit-array2* *displaced-bit-array3*))
  ((2 2) (0 1 0 0)))
(deftest displaced-bit-andc2
    (report-array (bit-andc2 *displaced-bit-array1* *displaced-bit-array2* *displaced-bit-array3*))
  ((2 2) (0 0 1 0)))
(deftest displaced-bit-orc1
    (report-array (bit-orc1 *displaced-bit-array1* *displaced-bit-array2* *displaced-bit-array3*))
  ((2 2) (1 1 0 1)))
(deftest displaced-bit-orc2
    (report-array (bit-orc2 *displaced-bit-array1* *displaced-bit-array2* *displaced-bit-array3*))
  ((2 2) (1 0 1 1)))
(deftest displaced-bit-not (report-array (bit-not *displaced-bit-array1* *displaced-bit-array3*))
  ((2 2) (1 1 0 0)))

(deftest bit-and-t
    (let ((a (make-array '(2 2) :element-type 'bit
		:initial-contents '((0 0) (1 1)))))
      (bit-and a *bit-array2* t)
      (report-array a))
  ((2 2) (0 0 0 1)))
(deftest bit-and-other
    (let ((a (make-array '(2 2) :element-type 'bit)))
      (bit-and *bit-array1* *bit-array2* a)
      (report-array a))
  ((2 2) (0 0 0 1)))
(deftest bit-not-t
    (let ((a (make-array '(2 2) :element-type 'bit
		:initial-contents '((0 0) (1 1)))))
      (bit-not a t)
      (report-array a))
  ((2 2) (1 1 0 0)))
(deftest bit-not-other
    (let ((a (make-array '(2 2) :element-type 'bit)))
      (bit-not *bit-array1* a)
      (report-array a))
  ((2 2) (1 1 0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 17.5 FILL POINTERS                                           ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ARRAY-HAS-FILL-POINTER-P
(deftest array-has-fill-pointer-nil
    (array-has-fill-pointer-p (make-array '())) nil)
(deftest array-has-fill-pointer-nil-bit
    (array-has-fill-pointer-p (make-array '() :element-type 'bit)) nil)
(deftest array-has-fill-pointer-nil-base-char
    (array-has-fill-pointer-p (make-array '() :element-type 'base-char)) nil)
(deftest array-has-fill-pointer-nil-character
    (array-has-fill-pointer-p (make-array '() :element-type 'character)) nil)

;;; ANSI allows simple arrays to have fill-pointers.  We don't.
#+eclipse
(progn
(deftest array-has-fill-pointer-nil-x
    (array-has-fill-pointer-p (make-array 3)) nil)
(deftest array-has-fill-pointer-nil-bit-x
    (array-has-fill-pointer-p (make-array 3 :element-type 'bit)) nil)
(deftest array-has-fill-pointer-nil-base-char-x
    (array-has-fill-pointer-p (make-array 3 :element-type 'base-char)) nil)
(deftest array-has-fill-pointer-nil-character-x
    (array-has-fill-pointer-p (make-array 3 :element-type 'character)) nil))

(deftest array-has-fill-pointer-t
    (array-has-fill-pointer-p (make-array 3 :fill-pointer t)) t)
(deftest array-has-fill-pointer-t-bit
    (array-has-fill-pointer-p (make-array 3 :element-type 'bit :fill-pointer t)) t)
(deftest array-has-fill-pointer-t-base-char
    (array-has-fill-pointer-p (make-array 3 :element-type 'base-char :fill-pointer t)) t)
(deftest array-has-fill-pointer-t-character
    (array-has-fill-pointer-p (make-array 3 :element-type 'character :fill-pointer t)) t)

;;; FILL-POINTER
(deftest fill-pointer-t
    (fill-pointer (make-array 3 :fill-pointer t)) 3)
(deftest fill-pointer-t-bit
    (fill-pointer (make-array 3 :element-type 'bit :fill-pointer t)) 3)
(deftest fill-pointer-t-base-char
    (fill-pointer (make-array 3 :element-type 'base-char :fill-pointer t)) 3)
(deftest fill-pointer-t-character
    (fill-pointer (make-array 3 :element-type 'character :fill-pointer t)) 3)

(deftest fill-pointer-0
    (fill-pointer (make-array 3 :fill-pointer 0)) 0)
(deftest fill-pointer-0-bit
    (fill-pointer (make-array 3 :element-type 'bit :fill-pointer 0)) 0)
(deftest fill-pointer-0-base-char
    (fill-pointer (make-array 3 :element-type 'base-char :fill-pointer 0)) 0)
(deftest fill-pointer-0-character
    (fill-pointer (make-array 3 :element-type 'character :fill-pointer 0)) 0)

(deftest fill-pointer-n
    (fill-pointer (make-array 3 :fill-pointer 3)) 3)
(deftest fill-pointer-n-bit
    (fill-pointer (make-array 3 :element-type 'bit :fill-pointer 3)) 3)
(deftest fill-pointer-n-base-char
    (fill-pointer (make-array 3 :element-type 'base-char :fill-pointer 3)) 3)
(deftest fill-pointer-n-character
    (fill-pointer (make-array 3 :element-type 'character :fill-pointer 3)) 3)

(deftest set-fill-pointer
    (let ((a (make-array 3 :fill-pointer 0)))
      (list (fill-pointer a) (setf (fill-pointer a) 3) (fill-pointer a)))
  (0 3 3))
(deftest set-fill-pointer-bit
    (let ((a (make-array 3 :fill-pointer 0 :element-type 'bit)))
      (list (fill-pointer a) (setf (fill-pointer a) 3) (fill-pointer a)))
  (0 3 3))
(deftest set-fill-pointer-base-char
    (let ((a (make-array 3 :fill-pointer 0 :element-type 'base-char)))
      (list (fill-pointer a) (setf (fill-pointer a) 3) (fill-pointer a)))
  (0 3 3))
(deftest set-fill-pointer-character
    (let ((a (make-array 3 :fill-pointer 0 :element-type 'character)))
      (list (fill-pointer a) (setf (fill-pointer a) 3) (fill-pointer a)))
  (0 3 3))

;;; VECTOR-PUSH
(deftest vector-push
    (let ((v (make-array 2 :fill-pointer 0)))
      (list (vector-push 1 v)
	    (vector-push 2 v)
	    (vector-push 3 v)
	    (vector-push 4 v)
	    (aref v 0)
	    (aref v 1)))
  (0 1 nil nil 1 2))
(deftest vector-push-bit
    (let ((v (make-array 2 :element-type 'bit :fill-pointer 0)))
      (list (vector-push 0 v)
	    (vector-push 1 v)
	    (vector-push 0 v)
	    (vector-push 1 v)
	    (aref v 0)
	    (aref v 1)))
  (0 1 nil nil 0 1))
(deftest vector-push-base-char
    (let ((v (make-array 2 :element-type 'base-char :fill-pointer 0)))
      (list (vector-push #\a v)
	    (vector-push #\b v)
	    (vector-push #\c v)
	    (vector-push #\d v)
	    (aref v 0)
	    (aref v 1)))
  (0 1 nil nil #\a #\b))
(deftest vector-push-character
    (let ((v (make-array 2 :element-type 'character :fill-pointer 0)))
      (list (vector-push #\a v)
	    (vector-push #\b v)
	    (vector-push #\c v)
	    (vector-push #\d v)
	    (aref v 0)
	    (aref v 1)))
  (0 1 nil nil #\a #\b))

;;; VECTOR-PUSH-EXTEND
(deftest vector-push-extend
    (let ((v (make-array 2 :fill-pointer 0 :adjustable t)))
      (list (vector-push-extend 1 v)
	    (vector-push-extend 2 v)
	    (vector-push-extend 3 v)
	    (vector-push-extend 4 v)
	    (aref v 0)
	    (aref v 1)
	    (aref v 3)))
  (0 1 2 3 1 2 4))
(deftest vector-push-extend-bit
    (let ((v (make-array 2 :element-type 'bit :fill-pointer 0
			  :adjustable t)))
      (list (vector-push-extend 0 v)
	    (vector-push-extend 1 v)
	    (vector-push-extend 0 v)
	    (vector-push-extend 1 v)
	    (aref v 0)
	    (aref v 1)
	    (aref v 3)))
  (0 1 2 3 0 1 1))
(deftest vector-push-extend-base-char
    (let ((v (make-array 2 :element-type 'base-char :fill-pointer 0
			  :adjustable t)))
      (list (vector-push-extend #\a v)
	    (vector-push-extend #\b v)
	    (vector-push-extend #\c v)
	    (vector-push-extend #\d v)
	    (aref v 0)
	    (aref v 1)
	    (aref v 3)))
  (0 1 2 3 #\a #\b #\d))
(deftest vector-push-extend-character
    (let ((v (make-array 2 :element-type 'character :fill-pointer 0
			  :adjustable t)))
      (list (vector-push-extend #\a v)
	    (vector-push-extend #\b v)
	    (vector-push-extend #\c v)
	    (vector-push-extend #\d v)
	    (aref v 0)
	    (aref v 1)
	    (aref v 3)))
  (0 1 2 3 #\a #\b #\d))


;;; VECTOR-POP
(deftest vector-pop
    (let ((v (make-array 2 :fill-pointer t :initial-contents '(a b))))
      (list (vector-pop v)
	    (vector-pop v)
	    (fill-pointer v)
	    (aref v 0)
	    (aref v 1)))
  (b a 0 a b))
(deftest vector-pop-bit
    (let ((v (make-array 2 :element-type 'bit
			 :fill-pointer t :initial-contents '(0 1))))
      (list (vector-pop v)
	    (vector-pop v)
	    (fill-pointer v)
	    (aref v 0)
	    (aref v 1)))
  (1 0 0 0 1))
(deftest vector-pop-base-char
    (let ((v (make-array 2 :element-type 'base-char
			 :fill-pointer t :initial-contents '(#\a #\b))))
      (list (vector-pop v)
	    (vector-pop v)
	    (fill-pointer v)
	    (aref v 0)
	    (aref v 1)))
  (#\b #\a 0 #\a #\b))
(deftest vector-pop-character
    (let ((v (make-array 2 :element-type 'character
			 :fill-pointer t :initial-contents '(#\a #\b))))
      (list (vector-pop v)
	    (vector-pop v)
	    (fill-pointer v)
	    (aref v 0)
	    (aref v 1)))
  (#\b #\a 0 #\a #\b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 17.6 CHANGING THE DIMENSIONS OF AN ARRAY                     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ADJUST-ARRAY
(deftest adjust-array-eq-1
    (let ((a (make-array 4 :adjustable t)))
      (eq a (adjust-array a 5))) t)
(deftest adjust-array-eq-0
    (let ((a (make-array '() :adjustable t)))
      (eq a (adjust-array a '()))) t)
(deftest adjust-array-eq-2
    (let ((a (make-array '(2 3) :adjustable t)))
      (eq a (adjust-array a '(3 0)))) t)
(deftest adjust-array-eq-1-bit
    (let ((a (make-array 4 :adjustable t :element-type 'bit)))
      (eq a (adjust-array a 3))) t)
#-(and cmu (not eclipse))
(deftest adjust-array-eq-0-bit
    (let ((a (make-array '() :adjustable t :element-type 'bit)))
      (eq a (adjust-array a '()))) t)
(deftest adjust-array-eq-2-bit
  (let ((a (make-array '(2 3) :adjustable t :element-type 'bit)))
      (eq a (adjust-array a '(1 2)))) t)

;;;
(deftest adjust-array-case1-r1-grow
    (report-array (adjust-array (make-array
				 4 :initial-contents '(1 2 3 4))
				6 :initial-element 99))
  ((6) (1 2 3 4 99 99)))
(deftest adjust-array-case1-r1-shrink
    (report-array (adjust-array (make-array
				 4 :initial-contents '(1 2 3 4))
				3 :initial-element 99))
  ((3) (1 2 3)))
(deftest adjust-array-case1-r1-grow-contents
    (report-array (adjust-array (make-array
				 4 :initial-contents '(1 2 3 4))
				6 :initial-contents '(6 5 4 3 2 1)))
  ((6) (6 5 4 3 2 1)))
(deftest adjust-array-case1-r1-shrink-contents
    (report-array (adjust-array (make-array
				 4 :initial-contents '(1 2 3 4))
				3 :initial-contents '(3 2 1)))
  ((3) (3 2 1)))
(deftest adjust-array-case1-r1-grow-eq
    (report-array (adjust-array (make-array
				 4 :adjustable t
				 :initial-contents '(1 2 3 4))
				6 :initial-element 99))
  ((6) (1 2 3 4 99 99)))
(deftest adjust-array-case1-r1-shrink-eq
    (report-array (adjust-array (make-array
				 4 :adjustable t
				 :initial-contents '(1 2 3 4))
				3 :initial-element 99))
  ((3) (1 2 3)))
(deftest adjust-array-case1-r1-grow-contents-eq
    (report-array (adjust-array (make-array
				 4 :adjustable t
				 :initial-contents '(1 2 3 4))
				6 :initial-contents '(6 5 4 3 2 1)))
  ((6) (6 5 4 3 2 1)))
(deftest adjust-array-case1-r1-shrink-contents-eq
    (report-array (adjust-array (make-array
				 4 :adjustable t
				 :initial-contents '(1 2 3 4))
				3 :initial-contents '(3 2 1)))
  ((3) (3 2 1)))
;;
(deftest adjust-array-base-char-case1-r1-grow
    (report-array (adjust-array (make-array
				 4 :element-type 'base-char
				 :initial-contents '(#\a #\b #\c #\d))
				6 :initial-element #\z))
  ((6) (#\a #\b #\c #\d #\z #\z)))
(deftest adjust-array-base-char-case1-r1-shrink
    (report-array (adjust-array (make-array
				 4 :element-type 'base-char
				 :initial-contents '(#\a #\b #\c #\d))
				3 :initial-element #\z))
  ((3) (#\a #\b #\c)))
(deftest adjust-array-base-char-case1-r1-grow-contents
    (report-array (adjust-array (make-array
				 4 :element-type 'base-char
				 :initial-contents '(#\a #\b #\c #\d))
				6 :initial-contents '(#\f #\e #\d #\c #\b #\a)))
  ((6) (#\f #\e #\d #\c #\b #\a)))
(deftest adjust-array-base-char-case1-r1-shrink-contents
    (report-array (adjust-array (make-array
				 4 :element-type 'base-char
				 :initial-contents '(#\a #\b #\c #\d))
				3 :initial-contents '(#\c #\b #\a)))
  ((3) (#\c #\b #\a)))
(deftest adjust-array-base-char-case1-r1-grow-eq
    (report-array (adjust-array (make-array
				 4 :element-type 'base-char
				 :adjustable t
				 :initial-contents '(#\a #\b #\c #\d))
				6 :initial-element #\z))
  ((6) (#\a #\b #\c #\d #\z #\z)))
(deftest adjust-array-base-char-case1-r1-shrink-eq
    (report-array (adjust-array (make-array
				 4 :element-type 'base-char
				 :adjustable t
				 :initial-contents '(#\a #\b #\c #\d))
				3 :initial-element #\z))
  ((3) (#\a #\b #\c)))
(deftest adjust-array-base-char-case1-r1-grow-contents-eq
    (report-array (adjust-array (make-array
				 4 :element-type 'base-char
				 :adjustable t
				 :initial-contents '(#\a #\b #\c #\d))
				6 :initial-contents '(#\f #\e #\d #\c #\b #\a)))
  ((6) (#\f #\e #\d #\c #\b #\a)))
(deftest adjust-array-base-char-case1-r1-shrink-contents-eq
    (report-array (adjust-array (make-array
				 4 :element-type 'base-char
				 :adjustable t
				 :initial-contents '(#\a #\b #\c #\d))
				3 :initial-contents '(#\c #\b #\a)))
  ((3) (#\c #\b #\a)))
;;
(deftest adjust-array-bit-case1-r1-grow
    (report-array (adjust-array (make-array
				 4 :element-type 'bit
				 :initial-contents '(0 1 0 1))
				6 :initial-element 1))
  ((6) (0 1 0 1 1 1)))
(deftest adjust-array-bit-case1-r1-shrink
    (report-array (adjust-array (make-array
				 4 :element-type 'bit
				 :initial-contents '(0 1 0 1))
				3 :initial-element 1))
  ((3) (0 1 0)))
(deftest adjust-array-bit-case1-r1-grow-contents
    (report-array (adjust-array (make-array
				 4 :element-type 'bit
				 :initial-contents '(0 1 0 1))
				6 :initial-contents '(1 0 1 0 1 0)))
  ((6) (1 0 1 0 1 0)))
(deftest adjust-array-bit-case1-r1-shrink-contents
    (report-array (adjust-array (make-array
				 4 :element-type 'bit
				 :initial-contents '(0 1 0 1))
				3 :initial-contents '(1 0 1)))
  ((3) (1 0 1)))
(deftest adjust-array-bit-case1-r1-grow-eq
    (report-array (adjust-array (make-array
				 4 :element-type 'bit
				 :adjustable t
				 :initial-contents '(1 0 1 0))
				6 :initial-element 1))
  ((6) (1 0 1 0 1 1)))
(deftest adjust-array-bit-case1-r1-shrink-eq
    (report-array (adjust-array (make-array
				 4 :element-type 'bit
				 :adjustable t
				 :initial-contents '(1 0 1 0))
				3 :initial-element 1))
  ((3) (1 0 1)))
(deftest adjust-array-bit-case1-r1-grow-contents-eq
    (report-array (adjust-array (make-array
				 4 :element-type 'bit
				 :adjustable t
				 :initial-contents '(1 0 1 0))
				6 :initial-contents '(0 1 0 1 0 1)))
  ((6) (0 1 0 1 0 1)))
(deftest adjust-array-bit-case1-r1-shrink-contents-eq
    (report-array (adjust-array (make-array
				 4 :element-type 'bit
				 :adjustable t
				 :initial-contents '(1 0 1 0))
				3 :initial-contents '(0 1 0)))
  ((3) (0 1 0)))
;;;
;;;
(deftest adjust-array-case1-r0
    (report-array (adjust-array (make-array '() :initial-contents 2)
				'()))
  (() (2)))
(deftest adjust-array-case1-r0-eq
    (report-array (adjust-array (make-array '() :initial-contents 2
					    :adjustable t)
				'()))
  (() (2)))
#-(and cmu (not eclipse))
(progn
  (deftest adjust-array-case1-r0-contents
    (report-array (adjust-array (make-array '() :initial-contents 2)
				'() :initial-contents 3))
    (() (3)))
  (deftest adjust-array-case1-r0-contents-eq
    (report-array (adjust-array (make-array '() :initial-contents 2
					    :adjustable t)
				'() :initial-contents 3))
    (() (3)))
  ;;
  (deftest adjust-array-base-char-case1-r0
    (report-array (adjust-array (make-array '() :element-type 'base-char
					    :initial-contents #\a)
				'()))
    (() (#\a)))
  (deftest adjust-array-base-char-case1-r0-eq
    (report-array (adjust-array (make-array '() :element-type 'base-char
					    :initial-contents #\a
					    :adjustable t)
				'()))
    (() (#\a)))
  (deftest adjust-array-base-char-case1-r0-contents
    (report-array (adjust-array (make-array '() :element-type 'base-char
					    :initial-contents #\a)
				'() :element-type 'base-char
				:initial-contents #\b))
    (() (#\b)))
  (deftest adjust-array-base-char-case1-r0-contents-eq
    (report-array (adjust-array (make-array '() :element-type 'base-char
					    :initial-contents #\a
					    :adjustable t)
				'() :element-type 'base-char
				:initial-contents #\b))
    (() (#\b))))
;;
(deftest adjust-array-bit-case1-r0
    (report-array (adjust-array (make-array '() :element-type 'bit
					    :initial-contents 0)
				'()))
  (() (0)))
(deftest adjust-array-bit-case1-r0-eq
    (report-array (adjust-array (make-array '() :element-type 'bit
					    :initial-contents 0
					    :adjustable t)
				'()))
  (() (0)))
#-(and cmu (not eclipse))
(progn
  (deftest adjust-array-bit-case1-r0-contents
    (report-array (adjust-array (make-array '() :element-type 'bit
					    :initial-contents 0)
				'() :element-type 'bit
				:initial-contents 1))
    (() (1)))
  (deftest adjust-array-bit-case1-r0-contents-eq
    (report-array (adjust-array (make-array '() :element-type 'bit
					    :initial-contents 0
					    :adjustable t)
				'() :element-type 'bit
				:initial-contents 1))
    (() (1))))
;;;
;;;
(deftest adjust-array-case1-r2-grow-grow
    (report-array (adjust-array
		   (make-array '(2 3)
			       :initial-contents
			       '((a b c) (1 2 3)))
		   '(4 6)))
  ((4 6) (a b c nil nil nil
	    1 2 3 nil nil nil
	    nil nil nil nil nil nil
	    nil nil nil nil nil nil)))
(deftest adjust-array-case1-r2-grow-grow-eq
    (report-array (adjust-array
		   (make-array '(2 3)
			       :adjustable t
			       :initial-contents
			       '((a b c) (1 2 3)))
		   '(4 6)))
  ((4 6) (a b c nil nil nil
	    1 2 3 nil nil nil
	    nil nil nil nil nil nil
	    nil nil nil nil nil nil)))
(deftest adjust-array-case1-r2-grow-grow-element
    (report-array (adjust-array
		   (make-array '(2 3)
			       :initial-contents
			       '((a b c) (1 2 3)))
		   '(4 6) :initial-element 99))
  ((4 6) (a b c 99 99 99
	    1 2 3 99 99 99
	    99 99 99 99 99 99
	    99 99 99 99 99 99)))
(deftest adjust-array-case1-r2-grow-grow-eq-element
    (report-array (adjust-array
		   (make-array '(2 3)
			       :adjustable t
			       :initial-contents
			       '((a b c) (1 2 3)))
		   '(4 6) :initial-element 99))
  ((4 6) (a b c 99 99 99
	    1 2 3 99 99 99
	    99 99 99 99 99 99
	    99 99 99 99 99 99)))
(deftest adjust-array-case1-r2-grow-grow-contents
    (report-array (adjust-array
		   (make-array '(2 3)
			       :initial-contents
			       '((a b c) (1 2 3)))
		   '(4 6)
		   :initial-contents
		   '((1 2 3 4 5 6)
		     (a b c d e f)
		     (7 8 9 10 11 12)
		     (g h i j k l))))
  ((4 6) (1 2 3 4 5 6
	  a b c d e f
	  7 8 9 10 11 12
	  g h i j k l)))
(deftest adjust-array-case1-r2-grow-grow-eq-contents
    (report-array (adjust-array
		   (make-array '(2 3)
			       :adjustable t
			       :initial-contents
			       '((a b c) (1 2 3)))
		   '(4 6)
		   :initial-contents
		   '((1 2 3 4 5 6)
		     (a b c d e f)
		     (7 8 9 10 11 12)
		     (g h i j k l))))
  ((4 6) (1 2 3 4 5 6
	  a b c d e f
	  7 8 9 10 11 12
	  g h i j k l)))
;;;
(defparameter greek
    '((alpha beta gamma delta)
      (epsilon zeta eta theta)
      (iota kappa lambda mu)
      (nu xi omicron pi)))
    
(deftest adjust-array-case1-r2-shrink-grow
    (report-array (adjust-array
		   (make-array '(4 4)
			       :initial-contents greek)
		   '(3 5) :initial-element 'baz))
  ((3 5) (alpha beta gamma delta baz
		epsilon zeta eta theta baz
		iota kappa lambda mu baz)))
(deftest adjust-array-case1-r2-shrink-grow-eq
    (report-array (adjust-array
		   (make-array '(4 4)
			       :adjustable t
			       :initial-contents greek)
		   '(3 5) :initial-element 'baz))
  ((3 5) (alpha beta gamma delta baz
		epsilon zeta eta theta baz
		iota kappa lambda mu baz)))
(deftest adjust-array-case1-r2-grow-shrink
    (report-array (adjust-array
		   (make-array '(4 4)
			       :initial-contents greek)
		   '(5 3) :initial-element 'baz))
  ((5 3) (alpha beta gamma
		epsilon zeta eta
		iota kappa lambda
		nu xi omicron
		baz baz baz)))
(deftest adjust-array-case1-r2-shrink-eq-grow
    (report-array (adjust-array
		   (make-array '(4 4)
			       :adjustable t
			       :initial-contents greek)
		   '(5 3) :initial-element 'baz))
  ((5 3) (alpha beta gamma
		epsilon zeta eta
		iota kappa lambda
		nu xi omicron
		baz baz baz)))
;;;
(defparameter pattern
    '(((a1 b1 c1) (d1 e1 f1) (g1 h1 i1))
      ((a2 b2 c2) (d2 e2 f2) (g2 h2 i2))
      ((a3 b3 c3) (d3 e3 f3) (g3 h3 i3))))
    
(deftest adjust-array-case1-r3-shrink-grow
    (report-array (adjust-array
		   (make-array '(3 3 3)
			       :initial-contents pattern)
		   '(1 3 5) :initial-element 'x))
  ((1 3 5) (a1 b1 c1 x x d1 e1 f1 x x g1 h1 i1 x x)))
(deftest adjust-array-case1-r3-shrink-grow-eq
    (report-array (adjust-array
		   (make-array '(3 3 3)
			       :adjustable t
			       :initial-contents pattern)
		   '(1 3 5) :initial-element 'x))
  ((1 3 5) (a1 b1 c1 x x d1 e1 f1 x x g1 h1 i1 x x)))
(deftest adjust-array-case1-r3-grow-shrink
    (report-array (adjust-array
		   (make-array '(3 3 3)
			       :initial-contents pattern)
		   '(5 3 1) :initial-element 'x))
  ((5 3 1) (a1 d1 g1 a2 d2 g2 a3 d3 g3 x x x x x x)))
(deftest adjust-array-case1-r3-shrink-eq-grow
    (report-array (adjust-array
		   (make-array '(3 3 3)
			       :adjustable t
			       :initial-contents pattern)
		   '(5 3 1) :initial-element 'x))
  ((5 3 1) (a1 d1 g1 a2 d2 g2 a3 d3 g3 x x x x x x)))

(deftest adjust-array-displaced
    (report-array (adjust-array
		   (make-array '(2 3))
		   '(3 5)
		   :displaced-to
		   (make-array 20 :initial-contents
			       '(0 1 2 3 4 5 6 7 8 9
				 10 11 12 13 14 15 16 17 18 19))
		   :displaced-index-offset 5))
  ((3 5) (5 6 7 8 9 10 11 12 13 14 15 16 17 18 19)))
