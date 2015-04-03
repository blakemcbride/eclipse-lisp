(eval-when (:compile-toplevel :execute)
  (unless (fboundp 'preserving-package)
    (defmacro preserving-package (&body body)
      `(let ((*package* (cl:find-package ,(cl:package-name *package*))))
	 ,@body))))

(defparameter *case-table* (copy-readtable nil))

(eval-when (:compile-toplevel :execute)
  (defmacro defprint-case (readcase printcase symbolcase result)
    `(deftest ,(make-name "PRINTCASE-~a-~a-~a" readcase printcase symbolcase)
       (preserving-package
	(let ((*print-case* ,printcase)
	      (*readtable* *case-table*))
	  (setf (readtable-case *readtable*) ,readcase)
	  (prin1-to-string ',(case symbolcase
			       (:upcase '|ZEBRA|)
			       (:capitalize '|Zebra|)
			       (:downcase '|zebra|)))))
       ,result)))

(defprint-case	:UPCASE		:UPCASE		:UPCASE		"ZEBRA")
(defprint-case	:UPCASE		:UPCASE		:CAPITALIZE	"|Zebra|")
(defprint-case	:UPCASE		:UPCASE		:DOWNCASE	"|zebra|")
(defprint-case	:UPCASE		:DOWNCASE	:UPCASE		"zebra")
(defprint-case	:UPCASE		:DOWNCASE	:CAPITALIZE	"|Zebra|")
(defprint-case	:UPCASE		:DOWNCASE	:DOWNCASE	"|zebra|")
(defprint-case	:UPCASE		:CAPITALIZE	:UPCASE		"Zebra")
(defprint-case	:UPCASE		:CAPITALIZE	:CAPITALIZE	"|Zebra|")
(defprint-case	:UPCASE		:CAPITALIZE	:DOWNCASE	"|zebra|")
(defprint-case	:DOWNCASE	:UPCASE		:UPCASE		"|ZEBRA|")
(defprint-case	:DOWNCASE	:UPCASE		:CAPITALIZE	"|Zebra|")
(defprint-case	:DOWNCASE	:UPCASE		:DOWNCASE	"ZEBRA")
(defprint-case	:DOWNCASE	:DOWNCASE	:UPCASE		"|ZEBRA|")
(defprint-case	:DOWNCASE	:DOWNCASE	:CAPITALIZE	"|Zebra|")
(defprint-case	:DOWNCASE	:DOWNCASE	:DOWNCASE	"zebra")
(defprint-case	:DOWNCASE	:CAPITALIZE	:UPCASE		"|ZEBRA|")
(defprint-case	:DOWNCASE	:CAPITALIZE	:CAPITALIZE	"|Zebra|")
(defprint-case	:DOWNCASE	:CAPITALIZE	:DOWNCASE	"Zebra")
(defprint-case	:PRESERVE	:UPCASE		:UPCASE		"ZEBRA")
(defprint-case	:PRESERVE	:UPCASE		:CAPITALIZE	"Zebra")
(defprint-case	:PRESERVE	:UPCASE		:DOWNCASE	"zebra")
(defprint-case	:PRESERVE	:DOWNCASE	:UPCASE		"ZEBRA")
(defprint-case	:PRESERVE	:DOWNCASE	:CAPITALIZE	"Zebra")
(defprint-case	:PRESERVE	:DOWNCASE	:DOWNCASE	"zebra")
(defprint-case	:PRESERVE	:CAPITALIZE	:UPCASE		"ZEBRA")
(defprint-case	:PRESERVE	:CAPITALIZE	:CAPITALIZE	"Zebra")
(defprint-case	:PRESERVE	:CAPITALIZE	:DOWNCASE	"zebra")
(defprint-case	:INVERT		:UPCASE		:UPCASE		"zebra")
(defprint-case	:INVERT		:UPCASE		:CAPITALIZE	"Zebra")
(defprint-case	:INVERT		:UPCASE		:DOWNCASE	"ZEBRA")
(defprint-case	:INVERT		:DOWNCASE	:UPCASE		"zebra")
(defprint-case	:INVERT		:DOWNCASE	:CAPITALIZE	"Zebra")
(defprint-case	:INVERT		:DOWNCASE	:DOWNCASE	"ZEBRA")
(defprint-case	:INVERT		:CAPITALIZE	:UPCASE		"zebra")
(defprint-case	:INVERT		:CAPITALIZE	:CAPITALIZE	"Zebra")
(defprint-case	:INVERT		:CAPITALIZE	:DOWNCASE	"ZEBRA")
  

(defstruct circular-struct a b)

#-cmu
(deftest print-circular
  (preserving-package
   (let* ((sym (make-symbol "SYM"))
	  (low (cons sym sym))
	  (list '(1 2 3))
	  (struct (make-circular-struct))
	  (clist (list nil nil struct list low))
	  (vector (vector nil 'a sym low struct clist)))
     (setf (circular-struct-a struct) clist
	   (circular-struct-b struct) struct
	   (car clist) clist
	   (cadr clist) vector
	   (svref vector 0) vector)
     (let ((printed (write-to-string clist :circle t)))
       (not (string= printed (write-to-string (read-from-string printed) :circle t))))))
    nil)

(deftest print-circular-string
  (let ((s "abc")) (write-to-string `(,s ,s) :circle t :escape t))
  "(#1=\"abc\" #1#)")

(deftest print-circular-string2
  (let ((s "abc")) (write-to-string `((,s) ,s) :circle t :escape t))
  "((#1=\"abc\") #1#)")

(deftest print-length-list
  (with-output-to-string (s)
    (dotimes (i 7) (write '(1 2 3 4 5 6) :stream s :length i) (terpri s)))
  "(...)
(1 ...)
(1 2 ...)
(1 2 3 ...)
(1 2 3 4 ...)
(1 2 3 4 5 ...)
(1 2 3 4 5 6)
")

(deftest print-length-dotted-list-good
  (write-to-string '(1 2 . 3) :length 2)
  "(1 2 . 3)")

(deftest print-length-dotted-list-bad
  (write-to-string '(1 2 . 3) :length 1)
  "(1 ...)")

(deftest print-depth-list
  (with-output-to-string (s)
    (dotimes (i 8) (write '(1 (2 (3 (4 (5 (6)))))) :stream s :level i) (terpri s)))
  "#
(1 #)
(1 (2 #))
(1 (2 (3 #)))
(1 (2 (3 (4 #))))
(1 (2 (3 (4 (5 #)))))
(1 (2 (3 (4 (5 (6))))))
(1 (2 (3 (4 (5 (6))))))
")



(deftest print-escaped-10
    (with-output-to-string (s)
      (let (#+cross-package(*package* (cl:find-package :eclipse)))
	(dolist (sym '("1B50" "27^19" "3.121_592_653" "7777Q" "3^4/5" "1.7J"
		       "6//7" "-3/4+6.7J" "3.1.2.6" "12/25/83" "^-43^"))
	  (write (cl:intern sym) :stream s :base 10) (write-char #\space s))))
  "|1B50| |27^19| |3.121_592_653| |7777Q| |3^4/5| |1.7J| |6//7| |-3/4+6.7J| |3.1.2.6| |12/25/83| |^-43^| ")

(deftest print-escaped-16
    (with-output-to-string (s)
      (let (#+cross-package(*package* (cl:find-package :eclipse)))
	(dolist (sym '("BAD-FACE" "25-DEC-83" "A/B" "FAD_CAFE" "F^"))
	  (write (cl:intern sym) :stream s :base 16) (write-char #\space s))))
  "|BAD-FACE| |25-DEC-83| |A/B| |FAD_CAFE| |F^| ")

(deftest print-unescaped-10
    (with-output-to-string (s)
      (let (#+cross-package(*package* (cl:find-package :eclipse)))
	(dolist (sym '("BAD-FACE" "25-DEC-83" "A/B" "FAD_CAFE" "F^"))
	  (write (cl:intern sym) :stream s :base 10) (write-char #\space s))))
  "BAD-FACE 25-DEC-83 A/B FAD_CAFE F^ ")

(deftest print-unescaped-16
    (with-output-to-string (s)
      (let (#+cross-package(*package* (cl:find-package :eclipse)))
	(dolist (sym '("/" "/5" "FOO+" "AB.CD" "+" "-" "1+" "^" "1-" "^/-"))
	  (write (cl:intern sym) :stream s :base 16) (write-char #\space s))))
  "/ /5 FOO+ AB.CD + - 1+ ^ 1- ^/- ")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 22.4 QUERYING THE USER                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Y-OR-N-P !!!
;;; YES-OR-NO-P !!!
