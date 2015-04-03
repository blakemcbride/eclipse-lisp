(defun delete-packages (pkgs) (mapc #'delete-package pkgs) t)

;;; MAKE-PACKAGE
(deftest make-package
    (values
     (packagep (make-package 'mp1 :nicknames '("mp1")))
     (package-name (make-package 'mp2 :use '("mp1")))
     (mapcar #'package-name (package-used-by-list 'mp1))
     (mapcar #'package-name (package-use-list 'mp2))
     (delete-packages '(mp2 mp1)))
  t "MP2" ("MP2") ("MP1") t)

;;; PACKAGEP  See delete package.

#+error
(unwind-protect (progn (make-package 'mp1)
		       (export (intern "FOO" 'mp1) 'mp1)
		       (make-package 'mp2)
		       (export (intern "FOO" 'mp2) 'mp2)
		       (make-package 'mp3 :use '(mp1 mp2)))
  (delete-packages '(mp1 mp2)))
		       

;;; LIST-ALL-PACKAGES
(deftest list-all-packages
    (flet ((all-names (p)
	     (cons (package-name p) (package-nicknames p)))
	   (string-member (name names)
	     (member name names :test #'string=)))
      (flet ((present (name)
	       (not (null (find name (list-all-packages)
				:key #'all-names :test #'string-member)))))
	(values (present "LAP1") (present "lap1") (present "LAP2")
		(package-name (make-package 'lap1 :nicknames '("lap1" lap2)))
		(present "LAP1") (present "lap1") (present "LAP2")
		(delete-package 'lap2)
		(present "LAP1") (present "lap1") (present "LAP2"))))
  nil nil nil "LAP1" t t t t nil nil nil)
  
;;; DELETE-PACKAGE
(deftest delete-package
    (flet ((fs (sym pkg)
	     (multiple-value-bind (s w)
		 (find-symbol sym pkg)
	       (when w (list (package-name (symbol-package s)) (symbol-name s) w)))))
      (let* ((foo-package (make-package "FOO" :use nil))
	     (foo-symbol (intern "FOO" foo-package))
	     (bar-package (make-package "BAR" :use '("FOO")))
	     (bar-symbol (intern "BAR" bar-package))
	     (baz-package (make-package "BAZ" :use '("BAR"))))
	(export foo-symbol foo-package)
	(export foo-symbol bar-package)
	(export bar-symbol bar-package)
	(list
	 (list
	  (package-name (symbol-package foo-symbol))
	  (package-name (symbol-package bar-symbol))
	  (fs "FOO" bar-package)
	  (fs "FOO" baz-package)
	  (fs "BAR" baz-package)
	  (packagep foo-package) (packagep bar-package) (packagep baz-package)
	  (package-name foo-package)
	  (package-name bar-package)
	  (package-name baz-package)
	  (package-use-list foo-package)
	  (mapcar #'package-name (package-use-list bar-package))
	  (mapcar #'package-name (package-use-list baz-package))
	  (mapcar #'package-name (package-used-by-list foo-package))
	  (mapcar #'package-name (package-used-by-list bar-package))
	  (package-used-by-list baz-package))
	 (handler-bind ((package-error #'continue))
	   (delete-package bar-package))
	 (list
	  (package-name (symbol-package foo-symbol))
	  (fs "FOO" baz-package)
	  (fs "BAR" baz-package)
	  (packagep foo-package) (packagep bar-package) (packagep baz-package)
	  (package-name foo-package)
	  (package-name bar-package)
	  (package-name baz-package)
	  (package-use-list foo-package)
	  (package-use-list baz-package)
	  (package-used-by-list foo-package)
	  (package-used-by-list baz-package))
	 (delete-packages '(foo baz)))))
  (("FOO" "BAR"
	  ("FOO" "FOO" :external)
	  ("FOO" "FOO" :inherited)
	  ("BAR" "BAR" :inherited)
	  t t t "FOO" "BAR" "BAZ"
	  () ("FOO") ("BAR")
	  ("BAR") ("BAZ") ())
   t
   ("FOO"
    nil
    nil
    t t t "FOO" nil "BAZ"
    () ()
    () ())
   t))
	
;;; FIND-PACKAGE
(deftest find-package
    (values (find-package 'fp1)
	    (package-name (make-package 'fp1))
	    (package-name (find-package 'fp1))
	    (delete-package 'fp1)
	    (find-package 'fp1))
  nil "FP1" "FP1" t nil)

;;; PACKAGE-NAME
(deftest package-name
    (let ((p (make-package 'pn1 :nicknames '("pn2"))))
      (values (package-name p)
	      (package-name 'pn1)
	      (package-name "PN1")
	      (package-name "pn2")
	      (delete-package p)
	      (package-name p)))
  "PN1" "PN1" "PN1" "PN1" t nil)

;;; PACKAGE-NICKNAMES
(deftest package-nicknames
    (let* ((list '("pn2" "PN3"))
	   (p (make-package 'pn1 :nicknames list))
	   (nicks (package-nicknames p)))
      (and (equal nicks
		  (package-nicknames 'pn1))
	   (equal nicks
		  (package-nicknames "PN1"))
	   (equal nicks
		  (package-nicknames "pn2"))
	   (equal nicks
		  (package-nicknames "PN3"))
	   (equal nicks
		  (package-nicknames 'pn3))
	   (null (set-exclusive-or nicks list :test #'equal))
	   (delete-package p)))
  t)

;;; RENAME
(deftest rename
    (prog2 (rename-package (make-package "RN1" :nicknames '("rn2"))
			   'rn3)
	   (or (package-nicknames "RN3")
	       (find-package 'rn1)
	       (find-package "rn2")
	       (not (packagep (rename-package 'rn3 'rn1 '(rn2 rn4))))
	       (not (eq (find-package "RN1")
			(find-package 'rn2)))
	       (set-exclusive-or (package-nicknames 'rn4)
				 '("RN2" "RN4") :test #'string=))
      (delete-package 'rn2))
  nil)


;;; PACKAGE-SHADOWING-SYMBOLS
(deftest package-shadowing-symbols
    (values (package-shadowing-symbols (make-package 'pss1 :nicknames '(pss3)))
	    (shadow 'cdr 'pss1)
	    (let ((cdr (find-symbol "CDR" 'pss1))
		  (pill (intern "PILL" (make-package 'pss2))))
	      (or (eq cdr 'cdr)
		  (set-exclusive-or (package-shadowing-symbols 'pss1)
				    (list cdr))
		  (nth-value 1 (intern "PILL" 'pss1))
		  (null (shadowing-import pill 'pss1))
		  (set-exclusive-or (package-shadowing-symbols "PSS3")
				    (list pill cdr))))
	    (delete-packages '(pss1 pss2)))
  nil t nil t)

;;; PACKAGE-USE-LIST
(deftest package-use-list
    (values (package-use-list (make-package 'pul1 :use nil
					    :nicknames '(pul3)))
	    (use-package (make-package 'pul2) 'pul1)
	    (mapcar #'package-name (package-use-list "PUL3"))
	    (delete-packages '(pul1 pul2)))
  nil t ("PUL2") t)

;;; PACKAGE-USED-BY-LIST
(deftest package-used-by-list
    (values
     (package-used-by-list (make-package 'publ1 :nicknames '(publ3)))
     (packagep (make-package 'publ2 :use '(publ1)))
     (mapcar #'package-name (package-used-by-list 'publ3))
     (delete-packages '(publ2 publ1)))
  () t ("PUBL2") t)

;;; USE-PACKAGE, UNUSE-PACKAGE
(deftest use-package
    (prog2
	(and (export (intern "LAND-FILL" (make-package 'trash1)) 'trash1)
	     (export (intern "OTHER" (make-package 'trash2)) 'trash2))
	(multiple-value-bind (s w)
	    (find-symbol "LAND-FILL" (make-package 'temp1))
	  (use-package 'trash1 'temp1)
	  (multiple-value-bind (s2 w2)
	      (find-symbol "LAND-FILL" 'temp1)
	    (unuse-package 'trash1 'temp1)
	    (use-package (list 'trash2 "TRASH1") 'temp1)
	    (multiple-value-bind (s4 w4)
		(find-symbol "OTHER" 'temp1)
	      (unuse-package (list 'trash2 "TRASH1") 'temp1)
	      (multiple-value-bind (s3 w3)
		  (find-symbol "LAND-FILL" 'temp1)
		(multiple-value-bind (s5 w5)
		    (find-symbol "OTHER" 'temp1)
		  (list s w (package-name (symbol-package s2)) w2
			  (package-name (symbol-package s4)) w4
			  s3 w3 s5 w5))))))
      (delete-packages '(temp1 trash1 trash2)))
  (nil nil "TRASH1" :inherited "TRASH2" :inherited nil nil nil nil))

;;; IN-PACKAGE !!!     

;;; FIND-SYMBOL
(deftest find-symbol
    (multiple-value-call #'values
      (find-symbol "NEVER-BEFORE-USED" (make-package 'temp1 :use nil))
      (find-symbol "NEVER-BEFORE-USED" 'temp1)
      (multiple-value-bind (s w)
	  (intern "NEVER-BEFORE-USED" "TEMP1")
	(multiple-value-bind (s2 w2)
	  (intern "NEVER-BEFORE-USED" :temp1)
	  (multiple-value-bind (s3 w3)
	      (find-symbol "NEVER-BEFORE-USED" 'temp1)
	    (values (and (eq s s2) (eq s s3) (symbol-name s))
		  w w2 w3))))
      (find-symbol "never-before-used" 'temp1)
      (find-symbol "NIL" 'temp1)
      (export (intern "NIL" 'temp1) 'temp1)
      (multiple-value-bind (s4 w4)
	  (find-symbol "NIL" 'temp1)
	(multiple-value-bind (s5 w5)
	    (find-symbol "NIL" (make-package 'temp2 :use '(temp1)))
	  (values (and (not (eq s4 'nil)) (eq s4 s5) (symbol-name s4))
		  w4 w5)))
      (delete-packages '(temp2 temp1)))
  nil nil nil nil "NEVER-BEFORE-USED" nil :internal :internal
  nil nil nil nil t "NIL" :external :inherited t)

;;; INTERN
(deftest intern
    (multiple-value-bind (s1 w1)
	(intern "NEVER-BEFORE" (make-package 'temp1))
      (multiple-value-bind (s2 w2)
	  (intern "NEVER-BEFORE" 'temp1)
	(export s1 'temp1)
	(multiple-value-bind (s3 w3)
	    (intern "NEVER-BEFORE" "TEMP1")
	  (intern "NEVER-BEFORE" :keyword)
	  (multiple-value-bind (s4 w4)
	      (intern "NEVER-BEFORE" (make-package 'temp2 :use '(temp1)))
	    (multiple-value-bind (s5 w5)
		(intern "NEVER-BEFORE" :keyword)
	      (delete-packages '(temp2 temp1))
	      (values (and (eq s2 s3)
			   (eq s2 s4)
			   (not (eq s1 s5))
			   (string= s1 s5)
			   (symbol-name s1))
		      w1 w2 w3 w4 w5))))))
  "NEVER-BEFORE" nil :internal :external :inherited :external)

;;; SHADOW
(deftest shadow
    (let ((car (intern "CAR" (make-package 'temp0 :use nil))))
      (export car 'temp0)
      (values
       (package-shadowing-symbols (make-package 'temp1 :use '(temp0)))
       (multiple-value-bind (s w)
	   (find-symbol "CAR" 'temp1)
	 (and (eq s car) w))
       (shadow 'car 'temp1)
       (multiple-value-bind (s w)
	   (find-symbol "CAR" 'temp1)
	 (and (not (eq s car))
	      (string= s car)
	      (equal (list s) (package-shadowing-symbols 'temp1))
	      w))
       (delete-packages '(temp1 temp0))))
  nil :inherited t :internal t)

(deftest shadow2
    (let ((test (intern "TEST" (make-package 'test1))))
      (shadow test 'test1)
      (shadow (intern "TEST" (make-package 'test0)) 'test1)
      (and (not (null (member test (package-shadowing-symbols 'test1))))
	   (export (intern "TEST" (make-package 'test2)) 'test2)
	   (use-package 'test2 'test1)
	   (delete-packages '(test1 test2 test0))))
  t)
      
;;; shadowing-import
(deftest shadowing-import
    (let ((sym0 (intern "CONFLICT" (make-package 'temp0)))
	  (sym1  (intern "CONFLICT" (make-package 'temp1))))
      (values (package-shadowing-symbols 'temp1)
	      (shadowing-import sym0 'temp1)
	      (let ((shadows (package-shadowing-symbols 'temp1)))
		(and (equal shadows (list sym0))
		     (not (eq (car shadows) sym1))))
	      (delete-packages '(temp1 temp0))))
  nil t t t)

;;; UNEXPORT
(deftest unexport
    (let ((sym (intern "CONTRABAND" (make-package 'temp1))))
      (values (export sym 'temp1)
	      (nth-value 1 (find-symbol "CONTRABAND" (make-package 'temp2)))
	      (use-package 'temp1 'temp2)
	      (nth-value 1 (find-symbol "CONTRABAND" 'temp2))
	      (unexport sym 'temp1)
	      (nth-value 1 (find-symbol "CONTRABAND" 'temp2))
	      (delete-packages '(temp2 temp1))))
  t nil t :inherited t nil t)

;;; UNINTERN
(deftest unintern
    (let ((temps-unpack (intern "UNPACK" (make-package 'temp1))))
      (values (unintern temps-unpack "TEMP1")
	      (nth-value 1 (find-symbol "UNPACK" 'temp1))
	      (symbol-package temps-unpack)
	      (delete-package 'temp1)))
  t nil nil t)

;;; IMPORT
(deftest import
    (let ((sym (intern "FOO" (make-package 'temp0))))
      (values (import sym (make-package 'temp1))
	      (multiple-value-bind (s w)
		  (find-symbol "FOO" 'temp1)
		(and (eq sym s) w))
	      (delete-packages '(temp1 temp0))))
  t :internal t)

;;; EXPORT 
;;; See unexport, shadow, intern, find-symbol

(deftest package-iterators
    (let ((m (make-package :m1 :use nil))
	  (n (make-package :n1 :use nil))
	  (o (make-package :o1 :use '(:m1 :n1)))
	  (me (intern "M-PUBLIC" :m1))
	  (ne (intern "N-PUBLIC" :n1))
	  (oe (intern "O-PUBLIC" :o1))
	  pkgs o-symbols1 o-symbols2
	  oe-symbols1 oe-symbols2 all-symbols1 all-symbols2)
      (intern "M-LOCAL" :m1) (intern "N-LOCAL" :n1) (intern "O-LOCAL" :o1)
      (export me :m1) (export ne :n1) (export oe :o1)
      (setq pkgs (list m n o))
      (values
       (with-package-iterator (next :o1 :internal :external :inherited)
	 (loop
	   (multiple-value-bind (flag sym where pkg) (next)
	     (unless flag (return))
	     (and (eq pkg o)
		  (eq where (if (string= "O-" sym :end2 2)
				(if (string= "-PUBLIC" sym :start2 1)
				    :external :internal)
			      :inherited))
		  (pushnew sym o-symbols1)))) 'a)
       (with-package-iterator (next :o1 :external)
	 (loop
	   (multiple-value-bind (flag sym) (next)
	     (if flag (pushnew sym oe-symbols1) (return)))) 'b)
       (with-package-iterator (next '(o1 m1 n1) :external :internal)
	 (loop
	   (multiple-value-bind (flag sym) (next)
	     (if flag
		 (pushnew sym all-symbols1) (return)))) 'c)
       (do-symbols (sym :o1 1) (pushnew sym o-symbols2))
       (do-external-symbols (sym :o1 2) (pushnew sym oe-symbols2))
       (do-all-symbols (sym 3)
	 (when (member (symbol-package sym) pkgs)
	   (pushnew sym all-symbols2)))
       (set-exclusive-or o-symbols1 o-symbols2)
       (set-exclusive-or oe-symbols1 oe-symbols2)
       (set-exclusive-or all-symbols1 all-symbols2)
       (length o-symbols1) (length oe-symbols1) (length all-symbols1)
       ;;; Entire do-symbols is enclosed in a block named nil.
       (let ((s nil) (count 0))
	 (do-symbols (sym :o1 (length s))
	   (cond ((zerop count) (push sym s) (incf count))
		 (t (return (list (length s) 99))))))
       (delete-packages '(o1 m1 n1))))
      a b c 1 2 3 nil nil nil 4 1 6 (1 99) t)

;;; DEFPACKAGE !!!
;;; In particular, make sure we can export nil, shadow nil, etc.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 11.8 MODULES                                                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PROVIDE !!!
;;; REQUIRE !!!