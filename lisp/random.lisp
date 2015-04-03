;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 12.9 RANDOM NUMBERS                                          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Based on CMUCL

;;; Inclusive upper bound on the size of fixnum kept in the state (and
;;; returned by random-chunk.)  Must be even.
(defconstant RANDOM-UPPER-BOUND 	(- most-positive-fixnum 3))
(defconstant RANDOM-CHUNK-LENGTH 	(integer-length random-upper-bound))
;; Extra bits of randomness that we generate before taking the value
;; MOD the limit, to avoid loss of randomness near the limit.
(defconstant RANDOM-INTEGER-EXTRA-BITS 10)
(defconstant RANDOM-MAX 54)

;;; Amount we overlap chunks by when building a large integer to make
;;; up for the loss of randomness in the low bits.
(defconstant RANDOM-INTEGER-OVERLAP 3)

;;; Largest fixnum we can compute from one chunk of bits.
(defconstant RANDOM-FIXNUM-MAX
  (1- (ash 1 (- random-chunk-length random-integer-extra-bits))))

(defvar *RANDOM-STATE*)

(defstruct (RANDOM-STATE
	    (:copier nil)
	    (:constructor
	     MAKE-RANDOM-STATE
	     (&optional state &aux
			(source (or state *random-state*))
			(j (if (eq source t) 24
			       (random-state-j source)))
			(k (if (eq source t) 0
			       (random-state-k source)))
			(seed (if (eq source t)
				  (let* ((n (1+ random-max))
					 (seed (make-array n))
					 (rand-seed (get-universal-time))
					 (const-a 8373)
					 (const-c 101010101)
					 (mask (1+ random-upper-bound)))
				    (dotimes (i n seed)
				      (setf (svref seed i)
					    (setq rand-seed
						  ;; Should be rem rather than logand
						  ;; but rem is too slow right now!!!
						  (logand (+ (* rand-seed const-a) const-c)
							  mask)))))
				  (copy-seq (random-state-seed source)))))))
  ;;#+excl (state) ;Excl has broken boa-constuctors
  (j 0 :type index)
  (k 0 :type index)
  (seed))

;;; Generates fixnums between 0 and random-upper-bound, inclusive.
(defun random-chunk (state)
  (let* ((seed (random-state-seed state))
	 (j (random-state-j state))
	 (k (random-state-k state))
	 (ja (svref seed
		    (setf (random-state-j state)
			  (if (= j 0) random-max (1- j)))))
	 (ka (svref seed
		    (setf (random-state-k state)
			  (if (= k 0) random-max (1- k)))))
	 (a (- random-upper-bound ja ka)))
    (setf (svref seed k)
	  (if (minusp a) (- a) (- random-upper-bound a)))))

(defun RANDOM (arg &optional (state *random-state*))
  (etypecase arg
    ((integer 0)
     (if (<= arg random-fixnum-max)
	 (rem (random-chunk state) arg)
       ;; IWBNI we allocated the xint first and then filled it, rather 
       ;; than consing up new ones as we ASH things over.
       (let ((shift (- random-chunk-length random-integer-overlap)))
	 (do ((bits (random-chunk state)
		    (logxor (ash bits shift) (random-chunk state)))
	      (count (+ (integer-length arg)
			(- random-integer-extra-bits shift))
		     (- count shift)))
	     ((minusp count) (rem bits arg))))))
    ((float 0.0f0)
     ;; There are "more efficient" ways of doing the division, but who cares?
     (* arg (/ (random-chunk state) (float random-upper-bound arg))))))

(defmethod MAKE-LOAD-FORM ((object RANDOM-STATE) &optional environment)
  (make-load-form-saving-slots object :environment environment))

(setq *random-state* (make-random-state t))
