;;; Fix these two!!!
(defmethod push-char-mode ((stream t) (kind t)))
(defmethod pop-char-mode ((stream t)))

(defgeneric FORMAT (destination string-or-fn &rest args))

(defmethod format ((destination NULL) string-or-fn &rest args)
  (with-output-to-string (stream)
    (apply #'format stream string-or-fn args)))

(defmethod format ((destination STRING) string-or-fn &rest args)
  (with-output-to-string (stream destination)
    (apply #'format stream string-or-fn args)))

(defmethod format ((destination SYMBOL) string-or-fn &rest args)
  (if (eq destination 't)
      (apply #'format *standard-output* string-or-fn args)
      (error 'type-error :datum destination
	     :expected-type '(member t nil))))

(defmethod format ((destination STREAM) (processor FUNCTION) &rest args)
  (if (and *print-circle* (null *circularity-map*))
      (with-two-circularity-passes (destination)
	  (apply processor destination args))
      (apply processor destination args))
  nil)
  
;;; Default, used for strings by non-development applications.
(defmethod format ((destination STREAM) processor &rest args)
  (write processor :stream destination)
  (dolist (arg args)
    (write-char #\space destination)
    (write arg :stream destination)))

;;; We can speed up formatted output when *print-circle* is true by
;;; testing for null-stream-p in many of the functions below. 
(defun null-stream-p (stream)
  (and (typep stream 'broadcast-stream)
       (null (broadcast-stream-streams stream))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Used by some directives at runtime

(defun digits-1 (x radix)
  (if (zerop x) 0
    ;; Do something else for large bignums???
    (floor (log x radix))))

(defun format-grouped-integer (commachar comma-interval obj
					 base signed commap stream)
  (if (integerp obj)
      (let* ((minusp (minusp obj))
	     (signp (or signed minusp))
	     (x (abs obj))
	     (n-digits-1 (digits-1 x base))
	     (spacing (1+ comma-interval)))
	(when signp (write-char (if minusp #\- #\+) stream))
	(do ((divisor (expt base n-digits-1))
	     (i (- comma-interval (rem n-digits-1 comma-interval)) (1+ i)))
	    ((= divisor 1) (write-char (digit-char x base) stream))
	  (if (and commap (zerop (rem i spacing)))
	      (write-char commachar stream)
	    (multiple-value-bind (digit next)
		(truncate x divisor)
	      (write-char (digit-char digit base) stream)
	      (setq x next divisor (/ divisor base))))))
    (write obj :stream stream :radix base :pretty nil :readably nil)))

(defun pad-chars (xp width mincol colinc padchar)
  (when (zerop colinc) (error "Zero column increment.")) 
  (do ((chars width (+ chars colinc)))
      ((>= chars mincol))
    (multiple-chars1 xp colinc padchar)))

(defconstant cardinal-ones
  #(nil "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"))

(defconstant cardinal-tens
  #(nil nil "twenty" "thirty" "forty"
	"fifty" "sixty" "seventy" "eighty" "ninety"))

(defconstant cardinal-teens
  #("ten" "eleven" "twelve" "thirteen" "fourteen"  ;;; RAD
    "fifteen" "sixteen" "seventeen" "eighteen" "nineteen"))

(defconstant cardinal-periods
  #("" " thousand" " million" " billion" " trillion" " quadrillion"
    " quintillion" " sextillion" " septillion" " octillion" " nonillion"
    " decillion"))

(defconstant ordinal-ones
  #(nil "first" "second" "third" "fourth"
	"fifth" "sixth" "seventh" "eighth" "ninth"))

(defconstant ordinal-tens 
  #(nil "tenth" "twentieth" "thirtieth" "fortieth"
	"fiftieth" "sixtieth" "seventieth" "eightieth" "ninetieth"))

(defun format-print-small-cardinal (stream n)
  (multiple-value-bind 
      (hundreds rem) (truncate n 100)
    (when (plusp hundreds)
      (write-string (svref cardinal-ones hundreds) stream)
      (write-string " hundred" stream)
      (when (plusp rem)
	(write-char #\space stream)))
    (when (plusp rem)
      (multiple-value-bind (tens ones)
			   (truncate rem 10)
       (cond ((< 1 tens)
	      (write-string (svref cardinal-tens tens) stream)
	      (when (plusp ones)
		(write-char #\- stream)
		(write-string (svref cardinal-ones ones) stream)))
	     ((= tens 1)
	      (write-string (svref cardinal-teens ones) stream))
	     ((plusp ones)
	      (write-string (svref cardinal-ones ones) stream)))))))

(defun format-print-cardinal (stream n)
  (cond ((minusp n)
	 (write-string "negative " stream)
	 (format-print-cardinal-aux stream (- n) 0 n))
	((zerop n)
	 (write-string "zero" stream))
	(t
	 (format-print-cardinal-aux stream n 0 n))))

(defun format-print-cardinal-aux (stream n period err)
  (multiple-value-bind (beyond here) (truncate n 1000)
    (unless (<= period 10)
      (error "Number too large to print in English: ~:D" err))
    (unless (zerop beyond)
      (format-print-cardinal-aux stream beyond (1+ period) err))
    (unless (zerop here)
      (unless (zerop beyond)
	(write-char #\space stream))
      (format-print-small-cardinal stream here)
      (write-string (svref cardinal-periods period) stream))))

(defun format-print-ordinal (stream n)
  (when (minusp n)
    (write-string "negative " stream))
  (let ((number (abs n)))
    (multiple-value-bind
	(top bot) (truncate number 100)
      (unless (zerop top)
	(format-print-cardinal stream (- number bot)))
      (when (and (plusp top) (plusp bot))
	(write-char #\space stream))
      (multiple-value-bind
	  (tens ones) (truncate bot 10)
	(cond ((= bot 12) (write-string "twelfth" stream))
	      ((= tens 1)
	       (write-string (svref cardinal-teens ones) stream);;;RAD
	       (write-string "th" stream))
	      ((and (zerop tens) (plusp ones))
	       (write-string (svref ordinal-ones ones) stream))
	      ((and (zerop ones)(plusp tens))
	       (write-string (svref ordinal-tens tens) stream))
	      ((plusp bot)
	       (write-string (svref cardinal-tens tens) stream)
	       (write-char #\- stream)
	       (write-string (svref ordinal-ones ones) stream))
	      ((plusp number)
	       (write-string "th" stream))
	      (t
	       (write-string "zeroeth" stream)))))))

;;; Print Roman numerals

(defun format-print-old-roman (stream n)
  (unless (< 0 n 5000)
    (error "Number too large to print in old Roman numerals: ~:D" n))
  (do ((char-list '(#\D #\C #\L #\X #\V #\I) (cdr char-list))
       (val-list '(500 100 50 10 5 1) (cdr val-list))
       (cur-char #\M (car char-list))
       (cur-val 1000 (car val-list))
       (start n (do ((i start (progn
				(write-char cur-char stream)
				(- i cur-val))))
		    ((< i cur-val) i))))
      ((zerop start))))

(defun format-print-roman (stream n)
  (unless (< 0 n 4000)
    (error "Number too large to print in Roman numerals: ~:D" n))
  (do ((char-list '(#\D #\C #\L #\X #\V #\I) (cdr char-list))
       (val-list '(500 100 50 10 5 1) (cdr val-list))
       (sub-chars '(#\C #\X #\X #\I #\I) (cdr sub-chars))
       (sub-val '(100 10 10 1 1 0) (cdr sub-val))
       (cur-char #\M (car char-list))
       (cur-val 1000 (car val-list))
       (cur-sub-char #\C (car sub-chars))
       (cur-sub-val 100 (car sub-val))
       (start n (do ((i start (progn
				(write-char cur-char stream)
				(- i cur-val))))
		    ((< i cur-val)
		     (cond ((<= (- cur-val cur-sub-val) i)
			    (write-char cur-sub-char stream)
			    (write-char cur-char stream)
			    (- i (- cur-val cur-sub-val)))
			   (t i))))))
	  ((zerop start))))


(defun format-fixed (stream number w d k ovf pad atsign)
  (if (realp number)
      (format-fixed-aux stream (float number) w d k ovf pad atsign)
    (format-write-field stream (decimal-string number)
			w 1 0 #\space t)))

(defun decimal-string (n)
  (write-to-string n :base 10 :radix nil :escape nil))

(defun format-write-field (stream string mincol colinc minpad padchar padleft)
  (unless padleft
    (write-string string stream))
  (dotimes (i minpad)
    (write-char padchar stream))
  (do ((chars (+ (length string) minpad) (+ chars colinc)))
      ((>= chars mincol))
    (dotimes (i colinc)
      (write-char padchar stream)))
  (when padleft
    (write-string string stream)))
    
(defun format-fixed-aux (stream number w d k ovf pad atsign)
  (cond
   ((not (or w d))
    (prin1 number stream)
    nil)
   (t
    (let ((spaceleft w))
      (when (and w (or atsign (minusp number))) (decf spaceleft))
      (multiple-value-bind 
	  (str len lpoint tpoint)
	  (flonum-to-string (abs number) spaceleft d k)
	;;if caller specifically requested no fraction digits, suppress the
	;;optional trailing zero
	(when (and d (zerop d)) (setq tpoint nil))
	(when w 
	  (decf spaceleft len)
	  ;;optional leading zero
	  (when lpoint
	    (if (or (> spaceleft 0) tpoint) ;force at least one digit
		(decf spaceleft)
		(setq lpoint nil)))
	  ;;optional trailing zero
	  (when tpoint
	    (if (> spaceleft 0)
		(decf spaceleft)
		(setq tpoint nil))))
	(cond ((and w (< spaceleft 0) ovf)
	       ;;field width overflow
	       (dotimes (i w) (write-char ovf stream))
	       t)
	      (t
	       (when w (dotimes (i spaceleft) (write-char pad stream)))
	       (if (minusp number)
		   (write-char #\- stream)
		   (if atsign (write-char #\+ stream)))
	       (when lpoint (write-char #\0 stream))
	       (write-string str stream)
	       (when tpoint (write-char #\0 stream))
	       nil)))))))    
  

(defun format-exponential (stream number w d e k ovf pad marker atsign)
  (if (realp number)
      (format-exp-aux stream (float number) w d e k ovf pad marker atsign)
    (format-write-field stream (decimal-string number) w 1 0 #\space
			t)))

(defun format-exponent-marker (number)
  (if (typep number *read-default-float-format*)
      #\e
      (typecase number
	(single-float #\f)
	(double-float #\d)
	(short-float #\s)
	(long-float #\l))))

;;;Here we prevent the scale factor from shifting all significance out of
;;;a number to the right.  We allow insignificant zeroes to be shifted in
;;;to the left right, athough it is an error to specify k and d such that this
;;;occurs.  Perhaps we should detect both these condtions and flag them as
;;;errors.  As for now, we let the user get away with it, and merely guarantee
;;;that at least one significant digit will appear.

(defun format-exp-aux (stream number w d e k ovf pad marker atsign)
  (if (not (or w d))
      (prin1 number stream)
      (multiple-value-bind (num expt)
			   (scale-exponent (abs number))
	(let* ((expt (- expt k))
	       (estr (decimal-string (abs expt)))
	       (elen (if e (max (length estr) e) (length estr)))
	       (fdig (if d (if (plusp k) (1+ (- d k)) d) nil))
	       (fmin (if (minusp k) (- 1 k) nil))
	       (spaceleft (if w
			      (- w 2 elen
				 (if (or atsign (minusp number))
				     1 0))
			      nil)))
	  (if (and w ovf e (> elen e)) ;exponent overflow
	      (dotimes (i w) (write-char ovf stream))
	      (multiple-value-bind
		  (fstr flen lpoint)
		  (flonum-to-string num spaceleft fdig k fmin)
		(when w 
		  (decf spaceleft flen)
		  (when lpoint
		    (if (> spaceleft 0)
			(decf spaceleft)
			(setq lpoint nil))))
		(cond ((and w (< spaceleft 0) ovf)
		       ;;significand overflow
		       (dotimes (i w) (write-char ovf stream)))
		      (t (when w
			   (dotimes (i spaceleft) (write-char pad stream)))
			 (if (minusp number)
			     (write-char #\- stream)
			     (if atsign (write-char #\+ stream)))
			 (when lpoint (write-char #\0 stream))
			 (write-string fstr stream)
			 (write-char (if marker
					 marker
					 (format-exponent-marker number))
				     stream)
			 (write-char (if (minusp expt) #\- #\+) stream)
			 (when e 
			   ;;zero-fill before exponent if necessary
			   (dotimes (i (- e (length estr)))
			     (write-char #\0 stream)))
			 (write-string estr stream)))))))))

(defun format-general (stream number w d e k ovf pad marker atsign)
  (if (realp number)
      (format-general-aux stream (float number) w d e k ovf pad marker atsign)
    (format-write-field stream (decimal-string number) w 1 0 #\space t)))

(defun format-general-aux (stream number w d e k ovf pad marker atsign)
  (multiple-value-bind (ignore n) 
		       (scale-exponent (abs number))
    (declare (ignore ignore))
    ;;Default d if omitted.  The procedure is taken directly
    ;;from the definition given in the manual, and is not
    ;;very efficient, since we generate the digits twice.
    ;;Future maintainers are encouraged to improve on this.
    (unless d
      (multiple-value-bind (str len) 
			   (flonum-to-string (abs number))
	(declare (ignore str))
	(let ((q (if (= len 1) 1 (1- len))))
	  (setq d (max q (min n 7))))))
    (let* ((ee (if e (+ e 2) 4))
	   (ww (if w (- w ee) nil))
	   (dd (- d n)))
      (cond ((<= 0 dd d)
	     (let ((char (if (format-fixed-aux stream number ww dd nil
					       ovf pad atsign)
			     ovf
			     #\space)))
	       (dotimes (i ee) (write-char char stream))))
	    (t
	     (format-exp-aux stream number w d e (or k 1)
			     ovf pad marker atsign))))))

(defun multiple-newlines1 (xp kind num)
  (do ((n num (1- n))) ((not (plusp n)))
    (stream-newline xp kind)
    (setq kind :unconditional)))

(defun multiple-chars1 (xp num char)
  (do ((n num (1- n))) ((not (plusp n)))
    (write-char char xp)))

;fancy stuff here, so will not get spurious indications of circularity.

(defun backup-in-list (num list some-tail)
  (backup-to (- (tail-pos list some-tail) num) list some-tail))

(defun backup-to (num list some-tail)
  (if *circularity-map*
      (multiple-value-bind (pos share) (tail-pos list some-tail)
	  (declare (ignore pos))
	(if (not (< num share)) (nthcdr num list)
	    (do ((L (nthcdr num list) (cdr L))
		 (n (- share num) (1- n))
		 (R nil (cons (car L) R)))
		((zerop n) (nreconc R L)))))
      (nthcdr num list)))

;because of backup-to, a prefix of some-tail may have been copied (in which
;case it cannot share anything with list), but there is a cons in some-tail
;that is in list.  This can be used to determine the position of some-tail
;relative to list.  However, we have to be careful, because they both could
;be cdr recursive.

(defun tail-pos (list some-tail)
  (block outer
    (do ((n 0 (1+ n))
	 (L list (cdr L)))
	(nil)
      (do ((m n (1- m))
	   (ST some-tail (cdr ST)))
	  (nil)
	(if (minusp m) (return nil))
	(if (eq ST L) (return-from outer (values m n)))))))


(defun format-justify (stream newline-prefix extra-space line-length strings
			      pad-left pad-right
			      mincol colinc minpad padchar)
  (unless (null-stream-p stream)
    (let* ((strings (or strings '("")))
	   (nstrings (length strings))
	   (singletonp (= 1 nstrings))
	   (pad-left (or pad-left (and singletonp (not pad-right))))
	   (npads (+ nstrings
		     (if pad-left 1 0)
		     (if pad-right 1 0)
		     -1))
	   (text (reduce #'+ strings :key #'length))
	   (min (+ text (* npads minpad)))
	   (total-width (if (> min mincol)
			    (+ mincol
			       (* colinc
				  (floor (+ min (1- colinc))
					 colinc)))
			    mincol))
	   (width total-width)
	   (extra (when pad-right
		    (let ((pad (floor (- width text) npads)))
		      (decf width pad)
		      (decf npads)
		      pad))))
      (when (and newline-prefix
		 (> (+ (or (stream-line-column stream) 0) total-width
		       extra-space)
		    line-length))
	(write-string newline-prefix stream))
      (unless pad-left
	(let* ((string (pop strings))
	       (length (length string)))
	  (decf width length)
	  (decf text length)
	  (decf nstrings) 
	  (write-string string stream)))
      (unless (zerop nstrings)
	(multiple-value-bind (minpad minpad-index)
	    (floor (- width text) npads)
	  (let ((maxpad (1+ minpad))
		(index 0))
	    (dolist (string strings)
	      (multiple-chars1 stream (if (< index minpad-index)
					  maxpad minpad)  padchar)
	      (incf index)
	      (write-string string stream)))))
      (when extra (multiple-chars1 stream extra padchar)))))

