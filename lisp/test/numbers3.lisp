(defmacro within-tolerance2 (form &rest vals)
  `(loop for actual in (multiple-value-list ,form)
	 and expected in ',vals
	 for diff = (within-tolerance actual expected)
	 unless (eq diff 't)
	 return (list actual expected diff)
	 finally (return (values))))

;;; FLOOR
(deftest floor-fix-pos (floor 2) 2 0)
(deftest floor-fix-neg (floor -2) -2 0)
(deftest floor-big-pos (floor #xffffffff) #xffffffff 0)
(deftest floor-big-neg (floor #x-ffffffff) #x-ffffffff 0)
(deftest floor-rat-down-pos (floor 4/3) 1 1/3)
(deftest floor-rat-up-pos (floor 5/3) 1 2/3)
(deftest floor-rat-down-neg (floor -4/3) -2 2/3)
(deftest floor-rat-up-neg (floor -5/3) -2 1/3)
(deftest floor-rat-big-down-pos
    (let ((n #x155555555/ffffffff))
      (multiple-value-bind (q r) (floor n)
	(and (= q 1)
	     (<= (abs (- n q r)) single-float-epsilon)))) t)
(deftest floor-rat-big-up-pos
    (let ((n #x1aaaaaaaa/ffffffff))
      (multiple-value-bind (q r) (floor n)
	(and (= q 1)
	     (<= (abs (- n q r)) single-float-epsilon)))) t)
(deftest floor-rat-big-down-neg
    (let ((n #x-155555555/ffffffff))
      (multiple-value-bind (q r) (floor n)
	(and (= q -2)
	     (<= (abs (- n q r)) single-float-epsilon)))) t)
(deftest floor-rat-big-up-neg
    (let ((n #x-1aaaaaaaa/ffffffff))
      (multiple-value-bind (q r) (floor n)
	(and (= q -2)
	     (<= (abs (- n q r)) single-float-epsilon)))) t)
(deftest floor-single-down-pos (within-tolerance2 (floor 1.3f0)
  1 0.3f0)) ;;0.29999995f0
(deftest floor-single-up-pos (within-tolerance2 (floor 1.7f0)
  1 0.7f0)) ;;0.70000005f0
(deftest floor-single-down-neg (within-tolerance2 (floor -1.3f0)
  -2 0.7f0)) ;;0.70000005f0
(deftest floor-single-up-neg (within-tolerance2 (floor -1.7f0)
  -2 0.3f0)) ;;0.29999995f0
(deftest floor-double-down-pos (within-tolerance2 (floor 1.3d0)
  1 0.3d0)) ;;0.30000000000000004d0
(deftest floor-double-up-pos (floor 1.7d0)
  1 0.7d0)
(deftest floor-double-down-neg (floor -1.3d0)
  -2 0.7d0)
(deftest floor-double-up-neg (within-tolerance2 (floor -1.7d0)
  -2 0.3d0)) ;;0.30000000000000004d0

(deftest floor-fix-2 (floor 4 3) 1 1)
(deftest floor-single-2 (floor 4 3.0f0) 1 1.0f0)
(deftest floor-double-2 (floor 4 3.0d0) 1 1.0d0)

;;; CEILING
(deftest ceiling-fix-pos (ceiling 2) 2 0)
(deftest ceiling-fix-neg (ceiling -2) -2 0)
(deftest ceiling-big-pos (ceiling #xffffffff)  #xffffffff 0)
(deftest ceiling-big-neg (ceiling #x-ffffffff) #x-ffffffff 0)
(deftest ceiling-rat-down-pos (ceiling 4/3) 2 -2/3)
(deftest ceiling-rat-up-pos (ceiling 5/3) 2 -1/3)
(deftest ceiling-rat-down-neg (ceiling -4/3) -1 -1/3)
(deftest ceiling-rat-up-neg (ceiling -5/3) -1 -2/3)
(deftest ceiling-rat-big-down-pos
    (let ((n #x155555555/ffffffff))
      (multiple-value-bind (q r) (ceiling n)
	(and (= q 2)
	     (<= (abs (- n q r)) single-float-epsilon)))) t)
(deftest ceiling-rat-big-up-pos
    (let ((n #x1aaaaaaaa/ffffffff))
      (multiple-value-bind (q r) (ceiling n)
	(and (= q 2)
	     (<= (abs (- n q r)) single-float-epsilon)))) t)
(deftest ceiling-rat-big-down-neg
    (let ((n #x-155555555/ffffffff))
      (multiple-value-bind (q r) (ceiling n)
	(and (= q -1)
	     (<= (abs (- n q r)) single-float-epsilon)))) t)
(deftest ceiling-rat-big-up-neg
    (let ((n #x-1aaaaaaaa/ffffffff))
      (multiple-value-bind (q r) (ceiling n)
	(and (= q -1)
	     (<= (abs (- n q r)) single-float-epsilon)))) t)
(deftest ceiling-single-down-pos (within-tolerance2 (ceiling 1.3f0)
  2 -0.7f0)) ;;-0.70000005f0
(deftest ceiling-single-up-pos (within-tolerance2 (ceiling 1.7f0)
  2 -0.3f0)) ;;-0.29999995f0
(deftest ceiling-single-down-neg (within-tolerance2 (ceiling -1.3f0)
  -1 -0.3f0)) ;;-0.29999995f0
(deftest ceiling-single-up-neg (within-tolerance2 (ceiling -1.7f0)
  -1 -0.7f0)) ;;-0.70000005f0
(deftest ceiling-double-down-pos (ceiling 1.3d0)
  2 -0.7d0)
(deftest ceiling-double-up-pos (within-tolerance2 (ceiling 1.7d0)
  2 -0.3d0)) ;; -0.30000000000000004d0
(deftest ceiling-double-down-neg (within-tolerance2 (ceiling -1.3d0)
  -1 -0.3d0)) ;;-0.30000000000000004d0
(deftest ceiling-double-up-neg (ceiling -1.7d0)
  -1 -0.7d0)

(deftest ceiling-fix-2 (ceiling 4 3) 2 -2)
(deftest ceiling-single-2 (ceiling 4 3.0f0) 2 -2.0f0)
(deftest ceiling-double-2 (ceiling 4 3.0d0) 2 -2.0d0)

;;; TRUNCATE
(deftest truncate-fix-pos (truncate 2) 2 0)
(deftest truncate-big-pos (truncate #xffffffff) #xffffffff 0)
(deftest truncate-rat-down-pos (truncate 4/3) 1 1/3)
(deftest truncate-rat-up-pos (truncate 5/3) 1 2/3)
(deftest truncate-rat-big-down-pos
    (let ((n #x155555555/ffffffff))
      (multiple-value-bind (q r) (truncate n)
	(and (= q 1)
	     (<= (abs (- n q r)) single-float-epsilon)))) t)
(deftest truncate-rat-big-up-pos
    (let ((n #x1aaaaaaaa/ffffffff))
      (multiple-value-bind (q r) (truncate n)
	(and (= q 1)
	     (<= (abs (- n q r)) single-float-epsilon)))) t)
(deftest truncate-single-down-pos (within-tolerance2 (truncate 1.3f0)
  1 0.3f0)) ;;0.29999995f0
(deftest truncate-single-up-pos (within-tolerance2 (truncate 1.7f0)
  1 0.7f0)) ;;0.70000005f0
(deftest truncate-double-down-pos (within-tolerance2 (truncate 1.3d0)
  1 0.3d0)) ;;0.30000000000000004d0
(deftest truncate-double-up-pos (truncate 1.7d0)
  1 0.7d0)
(deftest truncate-fix-neg (truncate -2) -2 0)
(deftest truncate-big-neg (truncate #x-ffffffff) #x-ffffffff 0)
(deftest truncate-rat-down-neg (truncate -4/3) -1 -1/3)
(deftest truncate-rat-up-neg (truncate -5/3) -1 -2/3)
(deftest truncate-rat-big-down-neg
    (let ((n #x-155555555/ffffffff))
      (multiple-value-bind (q r) (truncate n)
	(and (= q -1)
	     (<= (abs (- n q r)) single-float-epsilon)))) t)
(deftest truncate-rat-big-up-neg
    (let ((n #x-1aaaaaaaa/ffffffff))
      (multiple-value-bind (q r) (truncate n)
	(and (= q -1)
	     (<= (abs (- n q r)) single-float-epsilon)))) t)
(deftest truncate-single-down-neg (within-tolerance2 (truncate -1.3f0)
  -1 -0.3f0)) ;;-0.29999995f0
(deftest truncate-single-up-neg (within-tolerance2 (truncate -1.7f0)
  -1 -0.7f0)) ;;-0.70000005f0
(deftest truncate-double-down-neg (within-tolerance2 (truncate -1.3d0)
  -1 -0.3d0)) ;;-0.30000000000000004d0
(deftest truncate-double-up-neg (truncate -1.7d0)
  -1 -0.7d0)

(deftest truncate-fix-2 (truncate 4 3) 1 1)
(deftest truncate-single-2 (truncate 4 3.0f0) 1 1.0f0)
(deftest truncate-double-2 (truncate 4 3.0d0) 1 1.0d0)

(deftest truncate2big1 (truncate #xffffffff #xffffffff) 1 0)
(deftest truncate2big2 (truncate #xffffffffffff #xffffffff) 65536 65535)
(deftest truncate2big3 (truncate #x7fffffffffff #xffffffff) 32768 32767)
(deftest truncate2big4 (truncate #x-7fffffffffff #xffffffff) -32768 -32767)
(deftest truncate2big5 (truncate #x7fffffffffff #x-ffffffff) -32768 32767)
(deftest truncate2big6 (truncate #x-7fffffffffff #x-ffffffff) 32768 -32767)
(deftest truncate2big7 (truncate #x80000000 #x7fffffff) 1 1)
(deftest truncate2big8 (truncate #x-80000000 #x7fffffff) -1 -1)
(deftest truncate2big9 (truncate #x80000000 #x-7fffffff) -1 1)
(deftest truncate2big10 (truncate #x-80000000 #x-7fffffff) 1 -1)
(deftest truncate2big11 (truncate #xffffffff #xffff) 65537 0)
(deftest truncate2big12 (truncate #xffffffff #x7fff) 131076 3)
(deftest truncate2big13 (truncate #xffffffff #x8000) 131071 32767)
(deftest truncate2big14 (truncate #xffffffff #x10000) 65535 65535)

;;; ROUND
(deftest round-fix-pos (round 2) 2 0)
(deftest round-big-pos (round #xffffffff) #xffffffff 0)
(deftest round-rat-down-pos (round 4/3) 1 1/3)
(deftest round-rat-up-pos (round 5/3) 2 -1/3)
(deftest round-rat-big-down-pos
    (let ((n #x155555555/ffffffff))
      (multiple-value-bind (q r) (round n)
	(and (= q 1)
	     (<= (abs (- n q r)) single-float-epsilon)))) t)
(deftest round-rat-big-up-pos
    (let ((n #x1aaaaaaaa/ffffffff))
      (multiple-value-bind (q r) (round n)
	(and (= q 2)
	     (<= (abs (- n q r)) single-float-epsilon)))) t)
(deftest round-single-down-pos (within-tolerance2 (round 1.3f0)
  1 0.3f0)) ;;0.29999995f0
(deftest round-single-up-pos (within-tolerance2 (round 1.7f0)
  2 -0.3f0)) ;;-0.29999995f0
(deftest round-double-down-pos (within-tolerance2 (round 1.3d0)
  1 0.3d0)) ;;0.30000000000000004d0
(deftest round-double-up-pos (within-tolerance2 (round 1.7d0)
  2 -0.3d0)) ;;-0.30000000000000004d0
(deftest round-fix-neg (round -2) -2 0)
(deftest round-big-neg (round #x-ffffffff) #x-ffffffff 0)
(deftest round-rat-down-neg (round -4/3) -1 -1/3)
(deftest round-rat-up-neg (round -5/3) -2 1/3)
(deftest round-rat-big-down-neg
    (let ((n #x-155555555/ffffffff))
      (multiple-value-bind (q r) (round n)
	(and (= q -1)
	     (<= (abs (- n q r)) single-float-epsilon)))) t)
(deftest round-rat-big-up-neg
    (let ((n #x-1aaaaaaaa/ffffffff))
      (multiple-value-bind (q r) (round n)
	(and (= q -2)
	     (<= (abs (- n q r)) single-float-epsilon)))) t)
(deftest round-single-down-neg (within-tolerance2 (round -1.3f0)
  -1 -0.3f0)) ;;-0.29999995f0
(deftest round-single-up-neg (within-tolerance2 (round -1.7f0)
  -2 0.3f0)) ;;0.29999995f0
(deftest round-double-down-neg (within-tolerance2 (round -1.3d0)
  -1 -0.3d0)) ;;-0.30000000000000004d0
(deftest round-double-up-neg (within-tolerance2 (round -1.7d0)
  -2 0.3d0)) ;; 0.30000000000000004d0

(deftest round-rat-even-pos (round 1/2) 0 1/2)
(deftest round-rat-even-neg (round -1/2) 0 -1/2)
(deftest round-rat-odd-pos (round 3/2) 2 -1/2)
(deftest round-rat-odd-neg (round -3/2) -2 1/2)
(deftest round-single-even-pos (round 0.5f0) 0 0.5f0)
(deftest round-single-even-neg (round -0.5f0) 0 -0.5f0)
(deftest round-single-odd-pos (round 1.5f0) 2 -0.5f0)
(deftest round-single-odd-neg (round -1.5f0) -2 0.5f0)
(deftest round-double-even-pos (round 0.5d0) 0 0.5d0)
(deftest round-double-even-neg (round -0.5d0) 0 -0.5d0)
(deftest round-double-odd-pos (round 1.5d0) 2 -0.5d0)
(deftest round-double-odd-neg (round -1.5d0) -2 0.5d0)

(deftest round-fix-2 (round 4 3) 1 1)
(deftest round-single-2 (round 4 3.0f0) 1 1.0f0)
(deftest round-double-2 (round 4 3.0d0) 1 1.0d0)


;;; REM
(deftest rem-fix-1 (rem -1 5) -1)
(deftest rem-fix-2 (rem 13 4) 1)
(deftest rem-fix-3 (rem -13 4) -1)
(deftest rem-fix-4 (rem 13 -4) 1)
(deftest rem-fix-5 (rem -13 -4) -1)
(deftest rem-single-1 (< (abs (- (rem 13.4f0 1) 0.39999962f0)) single-float-epsilon) t)
(deftest rem-single-2 (< (abs (- (rem -13.4f0 1) -0.39999962f0)) single-float-epsilon) t)
(deftest rem-double-1 (< (abs (- (rem 13.4d0 1) 0.40000000000000036d0)) double-float-epsilon) t)
(deftest rem-double-2 (< (abs (- (rem -13.4d0 1) -0.40000000000000036d0)) double-float-epsilon) t)

(deftest truncate2-fix-pos (rem 2 1) 0)
(deftest truncate2-big-pos (rem #xffffffff 1) 0)
(deftest truncate2-rat-down-pos (rem 4/3 1) 1/3)
(deftest truncate2-rat-up-pos (rem 5/3 1) 2/3)
(deftest truncate2-single-down-pos (within-tolerance (rem 1.3f0 1)
  0.3f0) t) ;;0.29999995f0
(deftest truncate2-single-up-pos (within-tolerance (rem 1.7f0 1)
  0.7f0) t) ;;0.70000005f0
(deftest truncate2-double-down-pos (within-tolerance (rem 1.3d0 1)
  0.3d0) t) ;;0.30000000000000004d0
(deftest truncate2-double-up-pos (rem 1.7d0 1)
   0.7d0)
(deftest truncate2-fix-neg (rem -2 1) 0)
(deftest truncate2-big-neg (rem #x-ffffffff 1) 0)
(deftest truncate2-rat-down-neg (rem -4/3 1) -1/3)
(deftest truncate2-rat-up-neg (rem -5/3 1)  -2/3)
(deftest truncate2-single-down-neg (within-tolerance (rem -1.3f0 1)
  -0.3f0) t) ;;-0.29999995f0
(deftest truncate2-single-up-neg (within-tolerance (rem -1.7f0 1)
  -0.7f0) t) ;;-0.70000005f0
(deftest truncate2-double-down-neg (within-tolerance (rem -1.3d0 1)
  -0.3d0) t) ;;-0.30000000000000004d0
(deftest truncate2-double-up-neg (rem -1.7d0 1)
  -0.7d0)

(deftest truncate2-fix-2 (rem 4 3)  1)
(deftest truncate2-single-2 (rem 4 3.0f0) 1.0f0)
(deftest truncate2-double-2 (rem 4 3.0d0) 1.0d0)

;;; MOD
(deftest mod-fix-1 (mod -1 5) 4)
(deftest mod-fix-2 (mod 13 4) 1)
(deftest mod-fix-3 (mod -13 4) 3)
(deftest mod-fix-4 (mod 13 -4) -3)
(deftest mod-fix-5 (mod -13 -4) -1)
(deftest mod-single-1 (< (abs (- (mod 13.4f0 1) 0.39999962f0)) single-float-epsilon) t)
(deftest mod-single-2 (< (abs (- (mod -13.4f0 1) 0.6000004f0)) single-float-epsilon) t)
(deftest mod-double-1 (< (abs (- (mod 13.4d0 1) 0.40000000000000036d0)) double-float-epsilon) t)
(deftest mod-double-2 (< (abs (- (mod -13.4d0 1) 0.5999999999999996d0)) double-float-epsilon) t)

(deftest mod2-fix-pos (mod 2 1) 0)
(deftest mod2-fix-neg (mod -2 1) 0)
(deftest mod2-big-pos (mod #xffffffff 1) 0)
(deftest mod2-big-neg (mod #x-ffffffff 1) 0)
(deftest mod2-rat-down-pos (mod 4/3 1) 1/3)
(deftest mod2-rat-up-pos (mod 5/3 1) 2/3)
(deftest mod2-rat-down-neg (mod -4/3 1) 2/3)
(deftest mod2-rat-up-neg (mod -5/3 1) 1/3)
(deftest mod2-single-down-pos (within-tolerance (mod 1.3f0 1)
  0.3f0) t) ;;0.29999995f0
(deftest mod2-single-up-pos (within-tolerance (mod 1.7f0 1)
  0.7f0) t) ;;0.70000005f0
(deftest mod2-single-down-neg (within-tolerance (mod -1.3f0 1)
  0.7f0) t) ;;0.70000005f0
(deftest mod2-single-up-neg (within-tolerance (mod -1.7f0 1)
  0.3f0) t) ;;0.29999995f0
(deftest mod2-double-down-pos (within-tolerance (mod 1.3d0 1)
  0.3d0) t) ;;0.30000000000000004d0
(deftest mod2-double-up-pos (mod 1.7d0 1)
  0.7d0)
(deftest mod2-double-down-neg (mod -1.3d0 1)
  0.7d0)
(deftest mod2-double-up-neg (within-tolerance (mod -1.7d0 1)
  0.3d0) t) ;;0.30000000000000004d0

(deftest mod2-fix-2 (mod 4 3) 1)
(deftest mod2-single-2 (mod 4 3.0f0) 1.0f0)
(deftest mod2-double-2 (mod 4 3.0d0) 1.0d0)

;;; FLOAT-RADIX, FLOAT-DIGITS, FLOAT-PRECISION
#+ieee-floating-point
(progn
  (deftest float-radix-single-pos (float-radix 1.0f0) 2)
  (deftest float-radix-single-neg (float-radix -1.0f0) 2)
  (deftest float-radix-single-zero (float-radix 0.0f0) 2)
  (deftest float-radix-double-pos (float-radix 1.0d0) 2)
  (deftest float-radix-double-neg (float-radix -1.0d0) 2)
  (deftest float-radix-double-zero (float-radix 0.0d0) 2)

  (deftest float-precision-single-pos (float-precision 1.0f0) 24)
  (deftest float-precision-single-neg (float-precision -1.0f0) 24)
  (deftest float-precision-single-zero (float-precision 0.0f0) 0)
  (deftest float-precision-double-pos (float-precision 1.0d0) 53)
  (deftest float-precision-double-neg (float-precision -1.0d0) 53)
  (deftest float-precision-double-zero (float-precision 0.0d0) 0)

  (deftest float-digits-single-pos (float-digits 1.0f0) 24)
  (deftest float-digits-single-neg (float-digits -1.0f0) 24)
  (deftest float-digits-single-zero (float-digits 0.0f0) 24)
  (deftest float-digits-double-pos (float-digits 1.0d0) 53)
  (deftest float-digits-double-neg (float-digits -1.0d0) 53)
  (deftest float-digits-double-zero (float-digits 0.0d0) 53))

;;; SCALE-FLOAT
(deftest scale-float-single-1 (scale-float 1.0f0 1) 2.0f0)
(deftest scale-float-single-2 (scale-float 10.01f0 -2) 2.5025f0)
(deftest scale-float-single-3 (scale-float 23.0f0 0) 23.0f0)
(deftest scale-float-double-1 (scale-float 1.0d0 1) 2.0d0)
(deftest scale-float-double-2 (scale-float 10.01d0 -2) 2.5025d0)
(deftest scale-float-double-3 (scale-float 23.0d0 0) 23.0d0)

;;; DECODE-FLOAT
(deftest decode-float-single-0
  (multiple-value-bind (s e n) (decode-float 0.0f0)
    (values s (typep e 'fixnum) n))
  0.0f0 t 1.0f0)
(deftest decode-float-single-0-pos (decode-float 0.5f0)
  0.5f0 0 1.0f0)
(deftest decode-float-single-1-pos (decode-float 1.0f0)
  0.5f0 1 1.0f0)
(deftest decode-float-single-0-neg (decode-float -0.5f0)
  0.5f0 0 -1.0f0)
(deftest decode-float-single-1-neg (decode-float -1.0f0)
  0.5f0 1 -1.0f0)
(deftest decode-float-double-0
  (multiple-value-bind (s e n) (decode-float 0.0d0)
    (values s (typep e 'fixnum) n))
  0.0d0 t 1.0d0)
(deftest decode-float-double-0-pos (decode-float 0.5d0)
  0.5d0 0 1.0d0)
(deftest decode-float-double-1-pos (decode-float 1.0d0)
  0.5d0 1 1.0d0)
(deftest decode-float-double-0-neg (decode-float -0.5d0)
  0.5d0 0 -1.0d0)
(deftest decode-float-double-1-neg (decode-float -1.0d0)
  0.5d0 1 -1.0d0)

;;; INTEGER-DECODE-FLOAT
(deftest integer-decode-float-single-0
    (multiple-value-bind (s e n) (integer-decode-float 0.0f0)
      (values s (typep e 'fixnum) n))
    0 t 1)
(deftest integer-decode-float-single-0-pos (integer-decode-float 0.5f0)
   8388608 -24 1)
(deftest integer-decode-float-single-1-pos (integer-decode-float 1.0f0)
   8388608 -23 1)
(deftest integer-decode-float-single-0-neg (integer-decode-float -0.5f0)
   8388608 -24 -1)
(deftest integer-decode-float-single-1-neg (integer-decode-float -1.0f0)
   8388608 -23 -1)
(deftest integer-decode-float-double-0
  (multiple-value-bind (s e n) (integer-decode-float 0.0d0)
    (values s (typep e 'fixnum) n))
  0 t 1)
(deftest integer-decode-float-double-0-pos (integer-decode-float 0.5d0)
   4503599627370496 -53 1)
(deftest integer-decode-float-double-1-pos (integer-decode-float 1.0d0)
   4503599627370496 -52 1)
(deftest integer-decode-float-double-0-neg (integer-decode-float -0.5d0)
   4503599627370496 -53 -1)
(deftest integer-decode-float-double-1-neg (integer-decode-float -1.0d0)
   4503599627370496 -52 -1)

;;; FLOAT-SIGN
(deftest float-sign-single-1 (float-sign 5.0f0) 1.0f0)
(deftest float-sign-single-2 (float-sign -5.0f0) -1.0f0)
(deftest float-sign-single-3 (float-sign 0.0f0) 1.0f0)
(deftest float-sign-single-4 (float-sign 1.0f0 0.0f0) 0.0f0)
(deftest float-sign-single-5 (float-sign 1.0f0 -10.0f0) 10.0f0)
(deftest float-sign-single-6 (float-sign -1.0f0 10.0f0) -10.0f0)
(deftest float-sign-single-7 (float-sign -1.0f0 -10.0f0) -10.0f0)
(deftest float-sign-double-1 (float-sign 5.0d0) 1.0d0)
(deftest float-sign-double-2 (float-sign -5.0d0) -1.0d0)
(deftest float-sign-double-3 (float-sign 0.0d0) 1.0d0)
(deftest float-sign-double-4 (float-sign 1.0d0 0.0d0) 0.0d0)
(deftest float-sign-double-5 (float-sign 1.0d0 -10.0d0) 10.0d0)
(deftest float-sign-double-6 (float-sign -1.0d0 10.0d0) -10.0d0)
(deftest float-sign-double-7 (float-sign -1.0d0 -10.0d0) -10.0d0)
;; It is not clear if contagion applies.  
;; Should (float-sign double single) => double?
(deftest float-sign-cross-4 (float-sign 1.0f0 0.0d0) 0.0d0)
(deftest float-sign-cross-5 (float-sign 1.0f0 -10.0d0) 10.0d0)
(deftest float-sign-cross-6 (float-sign -1.0f0 10.0d0) -10.0d0)
(deftest float-sign-cross-7 (float-sign -1.0f0 -10.0d0) -10.0d0)

;;; COMPLEX
(deftest complex1 (complex 0) 0)
(deftest complex2 (complex 0.0f0) #c(0.0f0 0.0f0))
(deftest complex3 (complex 1 1/2) #c(1 1/2))
(deftest complex4 (complex 1 0.99d0) #c(1.0d0 0.99d0))
(deftest complex5 (complex 3/2 0.0f0) #c(1.5f0 0.0f0))

;;; REALPART
(deftest realpart1 (realpart #c(23 31)) 23)
(deftest realpart2 (realpart #c(23 31.0f0)) 23.0f0)
(deftest realpart3 (realpart 3) 3)

;;; IMAGPART
(deftest imagpart1 (imagpart #c(23 31)) 31)
(deftest imagpart2 (imagpart #c(23.0d0 31.0f0)) 31.0d0)
(deftest imagpart3 (imagpart 3) 0)
(deftest imagpart4 (imagpart 3.0f0) 0.0f0)

