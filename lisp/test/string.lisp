(defparameter s-simple "abcdxxx")
(defparameter s-more "xxxabdd")
(defparameter s-shorter "abc")
(defparameter s-fancy (make-array 9
				  :element-type 'base-char
				  :displaced-to "yyyxxxabddxxx"
				  :displaced-index-offset 3
				  :adjustable t
				  :fill-pointer 7))
(defparameter s-symbol '|xxxabdd|)
(defparameter s-capital "ABCDxxx")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 18.1 STRING ACCESS                                           ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SCHAR
(deftest schar (schar s-simple 0) #\a)
(deftest schar2 (schar s-simple 6) #\x)
;;; CHAR 
(deftest char (char s-simple 0) #\a)
(deftest char2 (char s-simple 6) #\x)
(deftest char3 (char s-fancy 0) #\x)
(deftest char4 (char s-fancy 3) #\a)
(deftest char5 (char s-fancy 6) #\d)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 18.2 STRING COMPARISON                                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun test-string-comparisons (s f)
  (let ((s-more1 (funcall f s s-more :start2 3 :end1 4))
	(s-more2 (funcall f s-more s :start1 3 :end2 4)))
    (and (eql s-more1
	      (funcall f s s-fancy :start2 3 :end1 4))
	 (eql s-more1
	      (funcall f s s-symbol :start2 3 :end1 4))
	 (eql s-more2
	      (funcall f s-fancy s :start1 3 :end2 4))
	 (eql s-more2
	      (funcall f s-symbol s :start1 3 :end2 4))
	 (list (funcall f s s :end1 4 :end2 4)
	       s-more1 s-more2
	       (funcall f s s-shorter :end1 4)
	       (funcall f s-shorter s :end2 4)))))

;;; STRING=, STRING<, STRING>, STRING<=, STRING>=, STRING/=
(deftest string< (test-string-comparisons s-simple #'string<)
  (NIL 2 NIL NIL 3))
(deftest string<= (test-string-comparisons s-simple #'string<=)
  (4 2 NIL NIL 3))
(deftest string> (test-string-comparisons s-simple #'string>)
  (NIL NIL 5 3 NIL))
(deftest string>= (test-string-comparisons s-simple #'string>=)
  (4 NIL 5 3 NIL))
(deftest string/= (test-string-comparisons s-simple #'string/=)
  (NIL 2 5 3 3))
(deftest string= (mapcar #'(lambda (x) (not (null x)))
			 (test-string-comparisons s-simple #'string=))
  (t nil nil nil nil))

;;; STRING-EQUAL, STRING-LESSP, STRING-GREATERP, 
;;; STRING-NOT-GREATERP,STRING-NOT-LESSP, STRING-NOT-EQUAL 
(deftest string-lessp (test-string-comparisons s-capital #'string-lessp)
  (NIL 2 NIL NIL 3))
(deftest string-not-greaterp (test-string-comparisons s-capital #'string-not-greaterp)
  (4 2 NIL NIL 3))
(deftest string-greaterp (test-string-comparisons s-capital #'string-greaterp)
  (NIL NIL 5 3 NIL))
(deftest string-not-lessp (test-string-comparisons s-capital #'string-not-lessp)
  (4 NIL 5 3 NIL))
(deftest string-not-equal (test-string-comparisons s-capital #'string-not-equal)
  (NIL 2 5 3 3))
(deftest string-equal (mapcar #'(lambda (x) (not (null x)))
			 (test-string-comparisons s-capital #'string-equal))
  (t nil nil nil nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 18.3 STRING CONSTRUCTION AND MANIPULATION                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MAKE-STRING
(deftest make-string-element
  (make-string 3 :initial-element #\a
	       #-(and cmu (not eclipse)) :element-type
	       #-(and cmu (not eclipse)) 'base-char)
  "aaa")
(deftest make-string (length (make-string 0)) 0)

;;; STRING-TRIM
(defun make-displaced-string ()
  (make-array 12
		:element-type 'base-char
		:displaced-to (copy-seq "ooozyxabcxyzooo")
		:displaced-index-offset 3
		:fill-pointer 9
		:adjustable t))
(defparameter displaced-string (make-displaced-string))
(deftest string-trim-none (string-trim nil "") "")
(deftest string-trim-no-change (string-trim "xyz" "abc") "abc")
(deftest string-trim (string-trim '(#\x #\y #\z) "zyxabcxyz") "abc")
(deftest string-trim-fancy
    (string-trim '(#\x #\y #\z) displaced-string) "abc")
(deftest string-trim-symbol (string-trim "XYZ" 'zyxabcxyz) "ABC")
(deftest string-trim-char (string-trim "xyz" #\x) "")

;;; STRING-LEFT-TRIM
(deftest string-left-trim-none (string-left-trim nil "") "")
(deftest string-left-trim-no-change (string-left-trim "xyz" "abc") "abc")
(deftest string-left-trim (string-left-trim '(#\x #\y #\z) "zyxabcxyz") "abcxyz")
(deftest string-left-trim-fancy
    (string-left-trim '(#\x #\y #\z) displaced-string) "abcxyz")
(deftest string-left-trim-symbol (string-left-trim "XYZ" 'zyxabcxyz) "ABCXYZ")
(deftest string-left-trim-char (string-left-trim "xyz" #\x) "")
;;; STRING-RIGHT-TRIM
(deftest string-right-trim-none (string-right-trim nil "") "")
(deftest string-right-trim-no-change (string-right-trim "xyz" "abc") "abc")
(deftest string-right-trim (string-right-trim '(#\x #\y #\z) "zyxabcxyz") "zyxabc")
(deftest string-right-trim-fancy
    (string-right-trim '(#\x #\y #\z) displaced-string) "zyxabc")
(deftest string-right-trim-symbol (string-right-trim "XYZ" 'zyxabcxyz) "ZYXABC")
(deftest string-right-trim-char (string-right-trim "xyz" #\x) "")

(deftest string-trim2 
    (string-trim '(#\space #\tab #\newline) " garbanzo beans
") "garbanzo beans")
(deftest string-trim3
    (string-trim " (*)" " ( *three (silly) words* ) ")
  "three (silly) words")
(deftest string-left-trim3
    (string-left-trim " (*)" " ( *three (silly) words* ) ")
  "three (silly) words* ) ")
(deftest string-right-trim3
    (string-right-trim " (*)" " ( *three (silly) words* ) ")
  " ( *three (silly) words")

;;; STRING-UPCASE
(deftest string-upcase1
    (string-upcase "Dr. Livingstone, I presume?")
  "DR. LIVINGSTONE, I PRESUME?")
(deftest string-upcase2
    (string-upcase "Dr. Livingstone, I presume?" :start 6 :end 10)
  "Dr. LiVINGstone, I presume?")
(deftest string-upcase3
    (string-upcase '|foo-bar| :start 2 :end 5)
  "foO-Bar")
(deftest string-upcase4 (string-upcase #\x) "X")
(deftest string-upcase5
    (string-upcase displaced-string :start 3 :end 6)
  "zyxABCxyz")

;;; STRING-DOWNCASE
(deftest string-downcase1
    (string-downcase "Dr. Livingstone, I presume?")
  "dr. livingstone, i presume?")
(deftest string-downcase2
    (string-downcase "Dr. Livingstone, I presume?" :start 4 :end 10)
  "Dr. livingstone, I presume?")
(deftest string-downcase3
    (string-downcase 'foo-bar :start 2 :end 5)
  "FOo-bAR")
(deftest string-downcase4 (string-downcase #\X) "x")
(deftest string-downcase5
    (string-downcase (string-upcase displaced-string) :start 3 :end 6)
  "ZYXabcXYZ")

;;; STRING-CAPITALIZE
(deftest string-capitalize1
    (string-capitalize " hello" ) " Hello")
(deftest string-capitalize2
    (string-capitalize "occlUDeD cASEmenTs FOreSTALL iNADVertent DEFenestraTION")
  "Occluded Casements Forestall Inadvertent Defenestration")
(deftest string-capitalize3
    (string-capitalize 'kludgy-hash-search)
  "Kludgy-Hash-Search")
(deftest string-capitalize4
    (string-capitalize "DON'T!") "Don'T!")
(deftest string-capitalize5
    (string-capitalize "pipe 13a, foo16c") "Pipe 13a, Foo16c")
(deftest string-capitalize6
    (string-capitalize
     (make-array 10
		 :element-type 'base-char
		 :displaced-to "XXaBcdEFGhXX"
		 :displaced-index-offset 2
		 :fill-pointer 8
		 :adjustable t)
     :start 3 :end 6)
  "aBcDefGh")

;;; NSTRING-UPCASE
(deftest nstring-upcase1
    (nstring-upcase (copy-seq "Dr. Livingstone, I presume?"))
  "DR. LIVINGSTONE, I PRESUME?")
(deftest nstring-upcase2
    (nstring-upcase (copy-seq "Dr. Livingstone, I presume?") :start 6 :end 10)
  "Dr. LiVINGstone, I presume?")
(deftest nstring-upcase5
    (nstring-upcase (make-displaced-string) :start 3 :end 6)
  "zyxABCxyz")
(deftest nstring-upcase-eq (let ((s (copy-seq "abc")))
			    (eq s (nstring-upcase s))) t)

;;; NSTRING-DOWNCASE
(deftest nstring-downcase1
    (nstring-downcase (copy-seq "Dr. Livingstone, I presume?"))
  "dr. livingstone, i presume?")
(deftest nstring-downcase2
    (nstring-downcase (copy-seq "Dr. Livingstone, I presume?") :start 4 :end 10)
  "Dr. livingstone, I presume?")
(deftest nstring-downcase5
    (nstring-downcase (nstring-upcase (make-displaced-string)) :start 3 :end 6)
  "ZYXabcXYZ")
(deftest nstring-downcase-eq (let ((s (copy-seq "ABC")))
			      (eq s (nstring-downcase s))) t)

;;; NSTRING-CAPITALIZE
(deftest nstring-capitalize1
    (nstring-capitalize (copy-seq " hello")) " Hello")
(deftest nstring-capitalize2
    (nstring-capitalize (copy-seq "occlUDeD cASEmenTs FOreSTALL iNADVertent DEFenestraTION"))
  "Occluded Casements Forestall Inadvertent Defenestration")
(deftest nstring-capitalize4
    (nstring-capitalize (copy-seq "DON'T!")) "Don'T!")
(deftest nstring-capitalize5
    (nstring-capitalize (copy-seq "pipe 13a, foo16c")) "Pipe 13a, Foo16c")
(deftest nstring-capitalize6
    (nstring-capitalize (make-array 10
				     :element-type 'base-char
				     :displaced-to "XXaBcdEFGhXX"
				     :displaced-index-offset 2
				     :fill-pointer 8
				     :adjustable t) :start 3 :end 6)
  "aBcDefGh")
(deftest nstring-capitalize-eq (let ((s (copy-seq "aBC")))
				(eq s (nstring-capitalize s))) t)

;;; STRING
(deftest string-string (let ((s "abc")) (eq s (string s))) t)
(deftest string-symbol (equal (symbol-name 'abc) (string 'abc)) t)
(deftest string-char (string #\x) "x")
