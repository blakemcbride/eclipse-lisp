;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 12.8 BYTE MANIPULATION FUNCTIONS                             ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BYTE, BYTE-SIZE, BYTE-POSITION
(deftest byte-0-1
    (let ((byte (byte 0 1)))
      (values (byte-size byte) (byte-position byte))) 0 1)
(deftest byte-1-0
    (let ((byte (byte 1 0)))
      (values (byte-size byte) (byte-position byte))) 1 0)
(deftest byte-small
    (let ((byte (byte 2 3)))
      (values (byte-size byte) (byte-position byte))) 2 3)
(deftest byte-big
    (let ((byte (byte #xffff #xfff9)))
      (values (byte-size byte) (byte-position byte))) #xffff #xfff9)

;;; LDB
(deftest ldb-fix (ldb (byte 2 1) 10) 1)
(deftest ldb-big-fix (ldb (byte 20 25) #xafbe5763ffff) 515883)
(deftest ldb-big-big (ldb (byte 32 25) #x-afbe5763ffff) 4289208532)
(deftest setf-ldb-fix
     (let ((a (list 8)))
      (values (setf (ldb (byte 2 1) (car a)) 1)
	      a))
  1 (10))

;;; LDB-TEST
(deftest ldb-test-fix1 (not (ldb-test (byte 4 1) 16)) nil)
(deftest ldb-test-fix2 (ldb-test (byte 3 1) 16) nil)
(deftest ldb-test-fix3 (not (ldb-test (byte 3 2) 16)) nil)
(deftest ldb-test-big1 (not (ldb-test (byte 17 31) #x80000000)) nil)
(deftest ldb-test-big2 (ldb-test (byte 17 32) #x80000000) nil)

;;; MASK-FIELD
(deftest mask-field-fix (mask-field (byte 1 5) -1) 32)
(deftest mask-field-big (mask-field (byte 17 18) #xfabcffff) #xFABC0000)
(deftest setf-mask-field-fix
    (let ((a 15))
      (values (mask-field (byte 2 0) a)
	      (setf (mask-field (byte 2 0) a) 1)
	      a))
  3 1 13)

;;; DPB
(deftest dpb-fix1 (dpb 1 (byte 1 10) 0) 1024)
(deftest dpb-fix2 (dpb -2 (byte 2 10) 0) 2048)
(deftest dpb-fix3 (dpb 1 (byte 2 10) 2048) 1024)
(deftest dpb-big (dpb #xfabc78ab (byte 19 18) #xfff5555aaaa) #xff1e2adaaaa)

;;; DEPOSIT-FIELD
(deftest deposit-field-fix1 (deposit-field 7 (byte 2 1) 0) 6)
(deftest deposit-field-fix2 (deposit-field -1 (byte 4 0) 0) 15)
(deftest deposit-field-fix3 (deposit-field 0 (byte 2 1) -3) -7)
(deftest deposit-field-big
    (deposit-field #xfff5555aaaa (byte 18 19) #x-ff1e2adaaaa)
  #x-FE0AAADAAAA)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 12.9 RANDOM NUMBERS                                          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; RANDOM-STATE-P
(deftest random-state-p-var (random-state-p *random-state*) t)
(deftest random-state-p-make-nil (random-state-p (make-random-state)) t)
(deftest random-state-p-make-t (random-state-p (make-random-state t)) t)
(deftest random-state-p-bignum (random-state-p (1+ most-positive-fixnum)) nil)

;;; MAKE-RANDOM-STATE
(deftest make-random-state
    (let* ((rs1 (make-random-state nil))
	   (rs2 (make-random-state t))
	   (rs3 (make-random-state rs2))
	   (rs4 nil))
      (let (s0 s1 s2 s3 s4)
	(dotimes (i 10)
	  (push (random 100) s0)
	  (when (= i 4) (setq rs4 (make-random-state))))
	(dotimes (i 10) (push (random 100 rs1) s1))
	(dotimes (i 10) (push (random 100 rs2) s2))
	(dotimes (i 10) (push (random 100 rs3) s3))
	(dotimes (i 10) (push (random 100 rs4) s4))
	(and (equal s0 s1)
	     (equal s2 s3)
	     (not (equal s0 s2))
	     (not (equal s0 s4))
	     (equal (subseq s0 0 5) (subseq s4 5)))))
  t)

;;; RANDOM
(deftest random-fix (typep (random 1000) '(integer 0 (1000))) t)
(deftest random-big (typep (random #xffffffff) '(integer 0 (#xffffffff))) t)
(deftest random-single (typep (random 1000.0f0) '(single-float 0.0f0 (1000.0f0))) t)
(deftest random-double (typep (random 1000.0d0) '(double-float 0.0d0 (1000.0d0))) t)
(deftest random-reproducible
    (let ((state1 (make-random-state))
	  (state2 (make-random-state)))
      (= (random 1000 state1) (random 1000 state2))) t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 12.10 IMPLEMENTATION PARAMETERS                              ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Some of these tests are likely to fail unless the definitions of
;;; the implementation constants are compiled in the same lisp in
;;; which the tests are run.  For example, if the system defines some
;;; implementation constants using functions from a different Lisp
;;; system, the constants generated might be different than the ones
;;; expected by these tests.

(deftest most-positive-fixnum
     (and (typep most-positive-fixnum 'fixnum)
	 (>= most-positive-fixnum (expt 2 16))) t)
(deftest most-negative-fixnum
    (and (typep most-negative-fixnum 'fixnum)
	 (< most-negative-fixnum (- (expt 2 16)))) t)

(deftest most-positive-short-float
    (typep most-positive-short-float
	   `(short-float ,(scale-float (float (1- (ash 1 13)) 1.0s0) (- 16 13)))) t)
(deftest most-positive-single-float
    (typep most-positive-single-float
	   `(single-float ,(scale-float (float (1- (ash 1 24)) 1.0f0) (- 128 24)))) t)
(deftest most-positive-double-float
    (typep most-positive-double-float
	   `(double-float ,(scale-float (float (1- (ash 1 50)) 1.0d0) (- 128 50)))) t)
(deftest most-positive-long-float
    (typep most-positive-long-float
	   `(long-float ,(scale-float (float (1- (ash 1 50)) 1.0l0) (- 128 50)))) t)

;;; Typep is not required to treat some-package:* like cl:*.
(deftest most-negative-short-float
    (typep most-negative-short-float
	   `(short-float * ,(- (scale-float (float (1- (ash 1 13)) 1.0s0) (- 16 13))))) t)
(deftest most-negative-single-float
    (typep most-negative-single-float
	   `(single-float * ,(- (scale-float (float (1- (ash 1 24)) 1.0f0) (- 128 24))))) t)
(deftest most-negative-double-float
    (typep most-negative-double-float
	   `(double-float * ,(- (scale-float (float (1- (ash 1 50)) 1.0d0) (- 128 50))))) t)
(deftest most-negative-long-float
    (typep most-negative-long-float
	   `(long-float * ,(- (scale-float (float (1- (ash 1 50)) 1.0l0) (- 128 50))))) t)

(deftest least-positive-normalized-short-float
    (typep least-positive-normalized-short-float
	   `(short-float 0.0s0 ,(scale-float 2.0s0 (- 1 16)))) t)
(deftest least-positive-normalized-single-float
    (typep least-positive-normalized-single-float
	   `(single-float 0.0f0 ,(scale-float 2.0f0 (- 1 128)))) t)
(deftest least-positive-normalized-double-float
    (typep least-positive-normalized-double-float
	   `(double-float 0.0d0 ,(scale-float 2.0d0 (- 1 128)))) t)
(deftest least-positive-normalized-long-float
    (typep least-positive-normalized-double-float
	   `(long-float 0.0l0 ,(scale-float 2.0l0 (- 1 128)))) t)

(deftest least-negative-normalized-short-float
    (typep least-negative-normalized-short-float
	   `(short-float ,(- (scale-float 2.0s0 (- 1 16))) 0.0s0)) t)
(deftest least-negative-normalized-single-float
    (typep least-negative-normalized-single-float
	   `(single-float ,(- (scale-float 2.0f0 (- 1 128))) 0.0f0)) t)
(deftest least-negative-normalized-double-float
    (typep least-negative-normalized-double-float
	   `(double-float ,(- (scale-float 2.0d0 (- 1 128))) 0.0d0)) t)
(deftest least-negative-normalized-long-float
    (typep least-negative-normalized-double-float
	   `(long-float ,(- (scale-float 2.0l0 (- 1 128))) 0.0l0)) t)

(deftest least-positive-short-float
    (typep least-positive-short-float
	   `(short-float 0.0s0 ,least-positive-normalized-short-float)) t)
(deftest least-positive-single-float
    (typep least-positive-single-float
	   `(single-float 0.0f0 ,least-positive-normalized-single-float)) t)
(deftest least-positive-double-float
    (typep least-positive-double-float
	   `(double-float 0.0d0 ,least-positive-normalized-double-float)) t)
(deftest least-positive-long-float
    (typep least-positive-long-float
	   `(long-float 0.0l0 ,least-positive-normalized-long-float)) t)

(deftest least-negative-short-float
    (typep least-negative-short-float
	   `(short-float ,least-negative-normalized-short-float 0.0s0)) t)
(deftest least-negative-single-float
    (typep least-negative-single-float
	   `(single-float ,least-negative-normalized-single-float 0.0f0)) t)
(deftest least-negative-double-float
    (typep least-negative-double-float
	   `(double-float ,least-negative-normalized-double-float 0.0d0)) t)
(deftest least-negative-long-float
    (typep least-negative-long-float
	   `(long-float ,least-negative-normalized-long-float 0.0l0)) t)

(deftest float-constant-order
    (<= most-negative-long-float
	most-negative-double-float
	most-negative-single-float
	most-negative-short-float
	
	least-negative-normalized-short-float
	least-negative-normalized-single-float
	least-negative-normalized-double-float
	least-negative-normalized-long-float
	
	least-positive-normalized-long-float
	least-positive-normalized-double-float
	least-positive-normalized-single-float
	least-positive-normalized-short-float

	most-positive-short-float
	most-positive-single-float
	most-positive-double-float
	most-positive-long-float) t)
	
(deftest short-float-epsilon
    (typep short-float-epsilon
	   `(short-float 0.0s0 ,(scale-float 2.0s0 (- 13)))) t)
(deftest single-float-epsilon
    (typep single-float-epsilon
	   `(single-float 0.0s0 ,(scale-float 2.0f0 (- 24)))) t)
(deftest double-float-epsilon
    (typep double-float-epsilon
	   `(double-float 0.0d0 ,(scale-float 2.0d0 (- 50)))) t)
(deftest long-float-epsilon
    (typep long-float-epsilon
	   `(long-float 0.0l0 ,(scale-float 2.0l0 (- 50)))) t)
(deftest short-float-negative-epsilon
    (typep short-float-negative-epsilon
	   `(short-float 0.0s0 ,(scale-float 2.0s0 (- 13)))) t)
(deftest single-float-negative-epsilon
    (typep single-float-negative-epsilon
	   `(single-float 0.0s0 ,(scale-float 2.0f0 (- 24)))) t)
(deftest double-float-negative-epsilon
    (typep double-float-negative-epsilon
	   `(double-float 0.0d0 ,(scale-float 2.0d0 (- 50)))) t)
(deftest long-float-negative-epsilon
    (typep long-float-negative-epsilon
	   `(long-float 0.0l0 ,(scale-float 2.0l0 (- 50)))) t)

(deftest short-float-epsilon2
    (= (float 1 short-float-epsilon)
       (+ (float 1 short-float-epsilon)
	  short-float-epsilon)) nil)
(deftest single-float-epsilon2
    (= (float 1 single-float-epsilon)
       (+ (float 1 single-float-epsilon)
	  single-float-epsilon)) nil)
(deftest double-float-epsilon2
    (= (float 1 double-float-epsilon)
       (+ (float 1 double-float-epsilon)
	  double-float-epsilon)) nil)
(deftest long-float-epsilon2
    (= (float 1 long-float-epsilon)
       (+ (float 1 long-float-epsilon)
	  long-float-epsilon)) nil)

(deftest short-float-negative-epsilon2
    (= (float 1 short-float-negative-epsilon)
       (- (float 1 short-float-negative-epsilon)
	  short-float-negative-epsilon)) nil)
(deftest single-float-negative-epsilon2
    (= (float 1 single-float-negative-epsilon)
       (- (float 1 single-float-negative-epsilon)
	  single-float-negative-epsilon)) nil)
(deftest double-float-negative-epsilon2
    (= (float 1 double-float-negative-epsilon)
       (- (float 1 double-float-negative-epsilon)
	  double-float-negative-epsilon)) nil)
(deftest long-float-negative-epsilon2
    (= (float 1 long-float-negative-epsilon)
       (- (float 1 long-float-negative-epsilon)
	  long-float-negative-epsilon)) nil)
