;;; test default printing of structures!!!
;;; test :print-object/:print-function!!!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 19 STRUCTURES                                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Don't define a bunch of stuff.
(defstruct (sparse
	    (:constructor nil)
	    (:predicate nil)
	    (:copier nil)))

(deftest sparse-struct (or (fboundp 'MAKE-SPARSE)
			   (fboundp 'SPARSE-P)
			   (fboundp 'COPY-SPARSE)) nil)

;;; These three are different legal syntax for the same thing.
(defstruct person name age sex)
(defstruct (person2 (:predicate) (:copier))
  name age sex)
(defstruct (person3 :predicate :copier :named)
  name age sex)

(deftest test-default-struct1
    (let ((struct (copy-person (make-person :sex 3 :age 2 :name 1))))
      (and (person-p struct)
	   (typep struct 'person)
	   (not (person-p (make-person2)))
	   (= (person-name struct) 1)
	   (= (person-age struct) 2)
	   (= (person-sex struct) 3))) t)

(deftest test-default-struct2
    (let ((struct (copy-person2 (make-person2 :sex 3 :age 2 :name 1))))
      (and (person2-p struct)
	   (typep struct 'person2)
	   (not (person2-p (make-person)))
	   (= (person2-name struct) 1)
	   (= (person2-age struct) 2)
	   (= (person2-sex struct) 3))) t)

(deftest test-default-struct3
    (let ((struct (copy-person3 (make-person3 :sex 3 :age 2 :name 1))))
      (and (person3-p struct)
	   (typep struct 'person3)
	   (not (person3-p (make-person)))
	   (= (person3-name struct) 1)
	   (= (person3-age struct) 2)
	   (= (person3-sex struct) 3))) t)

;;; Non-default names.
(defstruct (personx
	    (:predicate xx-person-p)
	    (:constructor build-xx)
	    (:copier xx-copy)
	    (:conc-name zap-))
  name age sex)

(deftest test-nondefault-struct-names
    (let ((struct (xx-copy (build-xx :sex 3 :age 2 :name 1))))
      (and (xx-person-p struct)
	   (typep struct 'personx)
	   (= (zap-name struct) 1)
	   (= (zap-age struct) 2)
	   (= (zap-sex struct) 3))) t)

;;; Multiple-constructors.
(defstruct (multi-construct
	    (:predicate nil)
	    (:copier nil)
	    :constructor
	    (:constructor keyword-constructor
			  ;; CMU doesn't default this properly 
			  #+(and cmu (not eclipse))
			  (&key b d q r y u e g h i j k))
	    (:constructor boa-constructor
			  (a &optional b (c 'sea)
			     &rest d
			     &key p (q 'cue) r ((:why y)) ((:you u) 'ewe)
			     &aux e (f 'eff) (g a) (h c) (i p) (j f))))
  (b 1 :read-only t) (d 2 :read-only t) (q 3 :read-only t) (r 33 :read-only t) (y 4 :read-only t)
  (u 44 :read-only t) (e 5 :read-only t) (g 6 :read-only t) (h 7 :read-only t)
  (i 8 :read-only t) (j 9 :read-only t) (k 10 :read-only t))

(deftest multi-constructor-boa1
    (let ((struct (boa-constructor 99)))
      (list (multi-construct-b struct)
	    (multi-construct-d struct)
	    (multi-construct-q struct)
	    (multi-construct-r struct)
	    (multi-construct-y struct)
	    (multi-construct-u struct)
	    (multi-construct-g struct)
	    (multi-construct-h struct)
	    (multi-construct-i struct)
	    (multi-construct-j struct)
	    (multi-construct-k struct)))
  (1 nil cue 33 4 ewe 99 sea nil eff 10))
(deftest multi-constructor-boa2
    (let ((struct (boa-constructor 99 11 88 :why 'y :you 'u)))
      (list (multi-construct-b struct)
	    (multi-construct-d struct)
	    (multi-construct-q struct)
	    (multi-construct-r struct)
	    (multi-construct-y struct)
	    (multi-construct-u struct)
	    (multi-construct-g struct)
	    (multi-construct-h struct)
	    (multi-construct-i struct)
	    (multi-construct-j struct)
	    (multi-construct-k struct)))
  (11 (:why y :you u) cue 33 y u 99 88 nil eff 10))
(deftest multi-constructor-default
    (let ((struct (make-multi-construct :d 22 :h 77)))
      (list (multi-construct-b struct)
	    (multi-construct-d struct)
	    (multi-construct-q struct)
	    (multi-construct-r struct)
	    (multi-construct-y struct)
	    (multi-construct-u struct)
	    (multi-construct-e struct)
	    (multi-construct-g struct)
	    (multi-construct-h struct)
	    (multi-construct-i struct)
	    (multi-construct-j struct)
	    (multi-construct-k struct)))
  (1 22 3 33 4 44 5 6 77 8 9 10))
(deftest multi-constructor-key
    (let ((struct (keyword-constructor :d 22 :h 77)))
      (list (multi-construct-b struct)
	    (multi-construct-d struct)
	    (multi-construct-q struct)
	    (multi-construct-r struct)
	    (multi-construct-y struct)
	    (multi-construct-u struct)
	    (multi-construct-e struct)
	    (multi-construct-g struct)
	    (multi-construct-h struct)
	    (multi-construct-i struct)
	    (multi-construct-j struct)
	    (multi-construct-k struct)))
  (1 22 3 33 4 44 5 6 77 8 9 10))
	    
	    
;;; :include, :type, and :initial-offset
(defstruct (super (:copier nil))
  (a 1 :read-only t) (b 2 :read-only t) (c 3 :read-only t))
(defstruct (super-list (:copier nil) (:type list) (:initial-offset 2) :named)
  (a 1 :read-only t) (b 2 :read-only t) (c 3 :read-only t))
(defstruct (super-vector (:copier nil) (:type vector) (:initial-offset 2) :named)
  (a 1 :read-only t) (b 2 :read-only t) (c 3 :read-only t))
(defstruct (sub-struct (:copier nil) (:include super))
  (d 4 :read-only t) (e 5 :read-only t) (f 6 :read-only t))
(defstruct (sub-list (:copier nil) (:include super-list) (:type list) (:initial-offset 3) :named)
  (d 4 :read-only t) (e 5 :read-only t) (f 6 :read-only t))
(defstruct (sub-vector (:copier nil) (:include super-vector) (:type vector) (:initial-offset 3) :named)
  (d 4 :read-only t) (e 5 :read-only t) (f 6 :read-only t))

(defparameter sub-struct (make-sub-struct :b 22 :e 55))
(defparameter sub-list (make-sub-list :b 22 :e 55))
(defparameter sub-vector (make-sub-vector :b 22 :e 55))

(deftest sub-p-sub (sub-struct-p sub-struct) t)
(deftest super-p-sub (not (null (super-p sub-struct))) t)
(deftest type-of-sub (type-of sub-struct) sub-struct)
(deftest typep-sub-sub (typep sub-struct 'sub-struct) t)
(deftest typep-sub-super (typep sub-struct 'super) t)
(deftest typep-sub-structure-object (typep sub-struct 'structure-object) t)
(deftest subtypep-sub-super (subtypep 'sub-struct 'super) t t)
(deftest subtypep-super-sub (subtypep 'super 'sub-struct) nil t)
(deftest subtypep-sub-structure-object (subtypep 'sub-struct 'structure-object) t t)
(deftest subtypep-structure-object-sub (subtypep 'structure-object 'sub-struct) nil t)

(deftest sub-p-sub-list (sub-struct-p sub-list) nil)
(deftest super-p-sub-list (super-p sub-list) nil)
(deftest type-of-sub-list (type-of sub-list) cons)
(deftest typep-sub-list-sub (typep sub-list 'sub-struct) nil)
(deftest typep-sub-list-super (typep sub-list 'super) nil)

(deftest sub-p-sub-vector (sub-struct-p sub-vector) nil)
(deftest super-p-sub-vector (super-p sub-vector) nil)
(deftest type-of-sub-vector (subtypep (type-of sub-vector) 'vector) t t)
(deftest typep-sub-vector-sub (typep sub-vector 'sub-struct) nil)
(deftest typep-sub-vector-super (typep sub-vector 'super) nil)

(deftest sub-list-p-sub-list (sub-list-p sub-list) t)
(deftest sub-list-p-sub-vector (sub-list-p sub-vector) nil)
(deftest sub-list-p-sub (sub-list-p sub-struct) nil)
(deftest sub-vector-p-sub-list (sub-vector-p sub-list) nil)
(deftest sub-vector-p-sub-vector (sub-vector-p sub-vector) t)
(deftest sub-vector-p-sub (sub-vector-p sub-struct) nil)

(deftest sub-contents
    (list (sub-struct-a sub-struct) (sub-struct-b sub-struct) (sub-struct-c sub-struct)
	  (sub-struct-d sub-struct) (sub-struct-e sub-struct) (sub-struct-f sub-struct))
  (1 22 3 4 55 6))
(deftest sub-list-contents
    (list (sub-list-a sub-list) (sub-list-b sub-list) (sub-list-c sub-list)
	  (sub-list-d sub-list) (sub-list-e sub-list) (sub-list-f sub-list))
  (1 22 3 4 55 6))
(deftest sub-vector-contents
    (list (sub-vector-a sub-vector) (sub-vector-b sub-vector) (sub-vector-c sub-vector)
	  (sub-vector-d sub-vector) (sub-vector-e sub-vector) (sub-vector-f sub-vector))
  (1 22 3 4 55 6))

(deftest sub-list sub-list
  (nil nil super-list 1 22 3 nil nil nil sub-list 4 55 6))
(deftest sub-vector
    (equalp sub-vector #(nil nil super-vector 1 22 3 nil nil nil sub-vector 4 55 6)) t)

