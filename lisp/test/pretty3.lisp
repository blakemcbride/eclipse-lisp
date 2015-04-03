(eval-when (:compile-toplevel :execute)
  (unless (fboundp 'preserving-package)
    (defmacro preserving-package (&body body)
      `(let ((*package* (cl:find-package ,(cl:package-name *package*))))
	 ,@body))))

(eval-when (:compile-toplevel :execute)
  (unless (fboundp 'plet)
    (defmacro plet (width miser &body body)
      `(preserving-package
	(let ((*PRINT-RIGHT-MARGIN* ,width)
	      (*PRINT-MISER-WIDTH* ,miser)
	      (*PRINT-PRETTY* T)
	      (*PRINT-ARRAY* T)
	      (*PRINT-ESCAPE* T)
	      (*PRINT-CASE* :UPCASE)
	      (*PRINT-CIRCLE* T)
	      (*PRINT-GENSYM* T)
	      (*PRINT-LEVEL* 100)
	      (*PRINT-LENGTH* NIL)
	      (*PRINT-LINES* NIL))
	  .,body)))))

(eval-when (:compile-toplevel :execute)
  (unless (fboundp 'formmterx)
    (defmacro formatterxx (string)
      (let ((eclipse::*default-format-function-package* :ECLIPSE))
	(macroexpand-1
	 `(formatter #+waters-xp-behavior ,string
		     #-waters-xp-behavior ,(format nil "~~@<~a~~:>"
						   string)))))

    (defmacro undo-level (ff)
      `#'(lambda (s &rest args)
	   (let ((*print-level* (when *print-level* (1+ *print-level*))))
	     (apply ,ff s args))))

    (defmacro formatterx (string)
      `(undo-level (formatterxx ,string)))
    ))




(defmacro prints (thing &rest bindings)
  `(plet 50 0
	 (let* (,@ bindings
		   (xp-result (write-to-string ,thing))
		   (normal-result (let ((*print-pretty* nil)) (write-to-string ,thing))))
	   (if (string= xp-result normal-result)
	       (values)
	     `("pretty print-output" ,xp-result and "normal print-output"
	       ,normal-result disagree)))))


(DEFTEST XP282
 (PLET 10 0
  (LET ((*PRINT-PRETTY* NIL))
    (PRINTS '#(12 3456 789 22 456 78)))))
(DEFTEST XP283
 (PLET 15 0
  (LET ((*PRINT-LENGTH* 5))
    (FORMAT NIL (FORMATTER "~W") '#(12 3456 789 22 456 78))))
 "#(12 3456 789
  22 456 ...)")
(DEFTEST XP284
 (PLET 15 0
  (LET ((*PRINT-LENGTH* 0))
    (FORMAT NIL (FORMATTER "~W") '#(12 3456 789 22 456 78))))
 "#(...)")
(DEFTEST XP285
 (PLET 15 0
  (LET ((*PRINT-LEVEL* 0))
    (FORMAT NIL (FORMATTER "~W") '#(12 3456 789 22 456 78))))
 "#")
(DEFTEST XP286
 (PLET 10 0
  (LET ((*PRINT-PRETTY* NIL))
    (PRINTS '#()))))
(defparameter *xp-array2* (make-array '(2 3)
				     :initial-contents '((12 3456 789)
							 (22 456
							     78))))
(defparameter *xp-array3*
  (make-array '(2 3 2)
	      :initial-contents '(((1 12) (1 3456) (1 789)) ((1 22) (1 456) (1 78)))))
(DEFTEST XP287
 (PLET 10 0
  (LET ((*PRINT-PRETTY* NIL))
    (STRING-UPCASE
     (FORMAT NIL (FORMATTER "~W") *xp-array2*))))
 "#2A((12 3456 789) (22 456 78))")
(DEFTEST XP288
 (PLET 20 0
  (LET ((*PRINT-PRETTY* T))
    (STRING-UPCASE
     (FORMAT NIL (FORMATTER "~W") *xp-array2*))))
 "#2A((12 3456 789)
    (22 456 78))")
(DEFTEST XP289
 (PLET 17 0
  (LET ((*PRINT-PRETTY* T))
    (STRING-UPCASE
     (FORMAT NIL (FORMATTER "~W") *xp-array2*))))
 "#2A((12 3456
     789)
    (22 456 78))")
(DEFTEST XP290
 (PLET 10 0
  (LET ((*PRINT-PRETTY* T))
    (FORMAT NIL (FORMATTER "~W") (make-array nil :initial-contents 'foo))))
 "#0A FOO")
(DEFTEST XP291
 (PLET 30 0
  (LET ((*PRINT-PRETTY* T))
    (STRING-UPCASE
     (FORMAT NIL
             (FORMATTER "~W")
             *xp-array3*))))
 "#3A(((1 12) (1 3456) (1 789))
    ((1 22) (1 456) (1 78)))")
(DEFTEST XP292
 (PLET 30 0
  (LET ((*PRINT-PRETTY* T))
    (STRING-UPCASE (FORMAT NIL (FORMATTER "~W")
			   (make-array '(2 2 0)
				       :initial-contents 
				       '((() ()) (() ())))))))
 "#3A((() ()) (() ()))")
(DEFTEST XP293
 (PLET 30 0
  (LET ((*PRINT-PRETTY* T) (*PRINT-LEVEL* 2))
    (STRING-UPCASE
     (FORMAT NIL
             (FORMATTER "~W")
             *xp-array3*))))
 "#3A((# # #) (# # #))")
(DEFTEST XP294
 (PLET 30 0
  (LET ((*PRINT-PRETTY* T) (*PRINT-LEVEL* 0))
    (STRING-UPCASE
     (FORMAT NIL
             (FORMATTER "~W")
             *xp-array3*))))
 "#")
(defparameter *xp-array3a* (make-array '(2 2 6)
				       :initial-contents
				       `(((1 12 1 1 1 1) (1 3456 1 1 1 1))
					 ((1 22 1 1 1 1) (1 456 1 1 1 1)))))
(DEFTEST XP295
 (PLET 30 0
  (LET ((*PRINT-PRETTY* T) (*PRINT-LENGTH* 4))
    (STRING-UPCASE
     (FORMAT NIL
             (FORMATTER "~W") *xp-array3a*))))
 "#3A(((1 12 1 1 ...)
     (1 3456 1 1 ...))
    ((1 22 1 1 ...)
     (1 456 1 1 ...)))")
(DEFTEST XP296
 (PLET 30 0
  (LET ((*PRINT-PRETTY* T) (*PRINT-LENGTH* 0))
    (STRING-UPCASE
     (FORMAT NIL
             (FORMATTER "~W") *xp-array3a*))))
 "#3A(...)")
(DEFUN format-FOO (XP OBJ COLON ATSIGN &OPTIONAL (N 3))
  (IF COLON (PUSH ":" OBJ))
  (IF ATSIGN (PUSH "@" OBJ))
  (FORMAT XP (FORMATTER "~V{~A~}") N OBJ))
(DEFTEST XP297 
  (FLET ((FOO (XP OBJ COLON ATSIGN &OPTIONAL (N 3))
	      (DECLARE (IGNORE COLON ATSIGN N))
	      (FORMAT XP (FORMATTER "~A") OBJ)))
	(PLET 20 0
	      (LIST (WITH-OUTPUT-TO-STRING (S) (FOO S '(1 2 3 4) NIL NIL))
		    (FORMAT NIL (FORMATTER "-~/eclipse-test::format-foo/-") '(1 2 3 4)))))
  ("(1 2 3 4)" "-123-"))
(DEFUN user::format-FOO1 (XP OBJ COLON ATSIGN &OPTIONAL (N 3))
    (IF COLON (PUSH ":" OBJ))
    (IF ATSIGN (PUSH "@" OBJ))
    (FORMAT XP (FORMATTER "~V{~A~}") N OBJ))
(DEFTEST XP298
  (PLET 20 0 (FORMAT NIL (FORMATTER "-~4/format-foo1/-") '(1 2 3 4)))
  "-1234-")
(DEFUN user::format-FOO2 (XP OBJ COLON ATSIGN &OPTIONAL (N 3))
    (IF COLON (PUSH ":" OBJ))
    (IF ATSIGN (PUSH "@" OBJ))
    (FORMAT XP (FORMATTER "~V{~A~}") N OBJ))
(DEFTEST XP299
  (PLET 20 0 (FORMAT NIL (FORMATTER "-~#/format-foo2/-") '(1 2 3 4)))
 "-1-")
(DEFUN user::format-FOO3 (XP OBJ COLON ATSIGN &OPTIONAL (N 3))
    (IF COLON (PUSH ":" OBJ))
    (IF ATSIGN (PUSH "@" OBJ))
    (FORMAT XP (FORMATTER "~V{~A~}") N OBJ))
(DEFTEST XP300
  (PLET 20 0 (FORMAT NIL (FORMATTER "-~V/format-foo3/-") 2 '(1 2 3 4)))
 "-12-")
(DEFUN user::format-FOO4 (XP OBJ COLON ATSIGN &OPTIONAL (N 3))
    (IF COLON (PUSH ":" OBJ))
    (IF ATSIGN (PUSH "@" OBJ))
    (FORMAT XP (FORMATTER "~V{~A~}") N OBJ))
(DEFTEST XP301 
  (PLET 20 0 (FORMAT NIL (FORMATTER "-~:@/format-foo4/-") '(1 2 3 4)))
 "-@:1-")
(DEFUN format-FOO5 (XP OBJ COLON ATSIGN &OPTIONAL (N 3))
    (IF COLON (PUSH ":" OBJ))
    (IF ATSIGN (PUSH "@" OBJ))
    (FORMAT XP (FORMATTER "~V{~A~}") N OBJ))
(DEFTEST XP302 
  (PLET 20 0
   (LET ((*PACKAGE* (cl:FIND-PACKAGE "USER")))
     (FORMAT NIL (FORMATTER "-~/eclipse-test::format-foo5/-") '(1 2 3 4))))
 "-123-")
(DEFUN user::format-FOO6 (XP OBJ COLON ATSIGN &OPTIONAL (N 3))
    (IF COLON (PUSH ":" OBJ))
    (IF ATSIGN (PUSH "@" OBJ))
    (FORMAT XP (FORMATTER "~V{~A~}") N OBJ))
(DEFTEST XP303 
  (PLET 20 0
   (LET ((*PACKAGE* (FIND-PACKAGE "LISP")))
     (FORMAT NIL (FORMATTER "-~/user::format-foo6/-") '(1 2 3 4))))
 "-123-")
(DEFUN format-BAR (XP &REST OBJECTS) (FORMAT XP (FORMATTER "~{~A~}") OBJECTS))
(DEFTEST XP304 
  (PLET 20 0 (FORMAT NIL (FORMATTER "-~?-") #'format-BAR '(1 2 3 4)))
 "-1234-")
(DEFTEST XP305
 (LET ((S (MAKE-ARRAY 20 :ELEMENT-TYPE 'CHARACTER :FILL-POINTER 0)))
   (LIST (FORMAT S (FORMATTER "test~A ") "ing")
         (FORMAT S (FORMATTER "test~A ") "ing")
         S))
 (NIL NIL "testing testing "))
(DEFTEST XP306
 (PLET 10 0
  (LET ((*PRINT-PRETTY* NIL) X)
    (LIST
     (WITH-OUTPUT-TO-STRING (S)
       (SETQ X (WRITE '(SETQ A 8 C "2") :STREAM S :BASE 8 :LINES 4)))
     X)))
 ("(SETQ A 10 C \"2\")" (SETQ A 8 C "2")))
(DEFTEST XP307
 (PLET 10 0
  (LET ((*PRINT-PRETTY* NIL))
    (WITH-OUTPUT-TO-STRING (S)
      (WRITE '(SETQ A 8 C "2") :STREAM S :ESCAPE NIL :PRETTY T))))
 "(SETQ A 8
      C 2)")
(DEFUN user::format-BAR1 (XP LIST &REST STUFF)
  (DECLARE (IGNORE STUFF))
  (WRITE LIST :STREAM XP :LENGTH 4 :PRETTY NIL))
(DEFTEST XP308 
  (PLET 10 0 (FORMAT NIL (FORMATTER "-~/format-bar1/-") '(SETQ A 8 C "2")))
 "-(SETQ A 8 C ...)-")
(DEFUN FORMAT-BAR2 (XP LIST &REST STUFF)
  (DECLARE (IGNORE STUFF))
    (WRITE LIST :STREAM XP :LENGTH 4))
(DEFTEST XP309
  (PLET 14 0 (FORMAT NIL (FORMATTER "-~/eclipse-test::format-bar2/-") '(SETQ A 8 C "2")))
 "-(SETQ A 8
       C ...)-")
(DEFTEST XP310
 (PLET 10 0
  (LET ((*PRINT-PRETTY* NIL) X)
    (CONS
     (WITH-OUTPUT-TO-STRING (S)
       (SETQ X
             (LIST (PRIN1 "2" S)
                   (FRESH-LINE S)
                   (WRITE-LINE "This is a test" S :START 2)
                   (TERPRI S)
                   (WRITE-STRING "This is a test" S :END 7)
                   (MULTIPLE-VALUE-LIST (PPRINT '(SETQ A B C D) S))
                   (WRITE-STRING "more
tests"
                                 S)
                   (PRINT "2" S)
                   (WRITE-CHAR #\a S)
                   (WRITE-CHAR #\NEWLINE S)
                   (FRESH-LINE S)
                   (PRINC "2" S))))
     X)))
 ("\"2\"
is is a test

This is
(SETQ A B
      C D)more
tests
\"2\" a
2"
  "2" T "This is a test" NIL "This is a test" NIL "more
tests"
  "2" #\a #\NEWLINE NIL "2"))
(DEFTEST XP311
 (PLET 10 0
  (LET ((*PRINT-PRETTY* T) X)
    (CONS
     (WITH-OUTPUT-TO-STRING (S)
       (SETQ X
             (LIST (PRIN1 "2" S)
                   (FRESH-LINE S)
                   (WRITE-LINE "This is a test" S :START 2)
                   (TERPRI S)
                   (WRITE-STRING "This is a test" S :END 7)
                   (MULTIPLE-VALUE-LIST (PPRINT '(SETQ A B C D) S))
                   (WRITE-STRING "more
tests"
                                 S)
                   (PRINT "2" S)
                   (WRITE-CHAR #\a S)
                   (WRITE-CHAR #\NEWLINE S)
                   (FRESH-LINE S)
                   (PRINC "2" S))))
     X)))
 ("\"2\"
is is a test

This is
(SETQ A B
      C D)more
tests
\"2\" a
2"
  "2" T "This is a test" NIL "This is a test" NIL "more
tests"
  "2" #\a #\NEWLINE NIL "2"))
(defun user::format-bar3 (s item &rest stuff)
	       (declare (ignore stuff))
	       (prin1 (copy-seq item) s)
	       (fresh-line s)
	       (write-line "This is a test" s :start 2)
	       (terpri s)
	       (write-string "This is a test" s :end 7)
	       (pprint '(setq a b c d) s)
	       (write-string "more
tests" s) 
	       (print (copy-seq item) s)
	       (write-char #\a s)
	       (write-char #\newline s)
	       (fresh-line s)
	       (princ (copy-seq item) s))
(DEFTEST XP312
  (plet 14 0 (format nil (formatterx "-~/user:format-bar3/-") "2"))
 "-\"2\"
is is a test

This is
(SETQ A B
      C D)more
tests
\"2\" a
2-")
(DEFTEST XP313 (NULL NIL) T)
(DEFTEST XP314 (PLET 14 0 (PROGN (FORMAT NIL "~A" 4))) "4")
(DEFTEST XP315 (PLET 14 0 (FORMAT NIL "~10<foo~>" 4)) "       foo")
(DEFTEST XP316 (PLET 14 0 (FORMAT NIL "~@<foo~:>" 4)) "foo")
(DEFTEST XP317 (PLET 14 0 (FORMAT NIL "~@<foo~:@>" 4)) "foo")
(DEFTEST XP318 (PLET 14 0 (FORMAT NIL "~w" 4)) "4")
(DEFTEST XP319
 (PLET 14 0
  (LET ()
    (FORMAT NIL "~w" 4)))
 "4")
(DEFTEST XP320
 (PLET 14 0
  (LET ((STRING "~W"))
    (LIST (FORMAT NIL STRING 4) (FORMAT NIL STRING 5))))
 ("4" "5"))
(DEFTEST XP321
 (PLET 20 0
  (FLET ((BAR (XP &REST OBJECTS)
           (FORMAT XP "~{~A~}" OBJECTS)))
    (FORMAT NIL (FORMATTER "-~?-") #'BAR '(1 2 3 4))))
 "-1234-")
(DEFTEST XP322 (WITH-OUTPUT-TO-STRING (*STANDARD-OUTPUT*) (PRIN1 44)) "44")
(DEFTEST XP323 (WITH-OUTPUT-TO-STRING (*STANDARD-OUTPUT*) (PRIN1 44 NIL)) "44")
(DEFTEST XP324 (WITH-OUTPUT-TO-STRING (*TERMINAL-IO*) (PRIN1 44 T)) "44")
(DEFTEST XP325 (PLET 100 0 (PRINC-TO-STRING '(SETQ A "2" B 8)))
 "(SETQ A 2 B 8)")
(DEFTEST XP326
 (PLET 100 0
  (LET ((*PRINT-PRETTY* NIL))
    (PRIN1-TO-STRING '(SETQ A "2" B 8))))
 "(SETQ A \"2\" B 8)")
(DEFTEST XP327
 (PLET 100 0 (WRITE-TO-STRING '(SETQ A "2" B 8) :BASE 8 :RIGHT-MARGIN 13))
 "(SETQ A \"2\"
      B 10)")
(DEFSTRUCT POO (A 1) (B 2))
(DEFTEST XP328
  (PLET 10 0 (PRIN1-TO-STRING (MAKE-POO)))
 "#S(POO :A 1
       :B 2)")
(DEFSTRUCT (POO00 (:CONC-NAME NIL)) (A 1) (B 2))
(DEFTEST XP329
  (PLET 10 0 (PRIN1-TO-STRING (MAKE-POO00)))
 "#S(POO00 :A 1
         :B 2)")
(DEFSTRUCT (FOO0 (:CONSTRUCTOR printer-MF)) (A 1) (B 2))
(DEFTEST XP330
  (PLET 11 0 (PRIN1-TO-STRING (printer-MF)))
 "#S(FOO0 :A 1
        :B 2)")
(DEFSTRUCT (POO1 (:CONC-NAME TUZ)) A (B 2))
(DEFTEST XP331
  (PLET 16 0 (PRIN1-TO-STRING (MAKE-POO1 :A '(1 2 3))))
 "#S(POO1 :A (1 2
            3)
        :B 2)")
(DEFUN format-FOO2P (OB S D)
    (IF (AND *PRINT-LEVEL* (NOT (< D *PRINT-LEVEL*)))
        (PRINC "#" S)
        (FORMAT S (FORMATTER "#<foo2 ~_is ~A>") (AA OB))))
(DEFSTRUCT (POO2 (:CONC-NAME NIL) (:PRINT-FUNCTION format-FOO2P)) (AA 3))
(DEFTEST XP332
  (PLET 13 0
   (LIST
    (LET ((*PRINT-LEVEL* 1))
      (FORMAT NIL (FORMATTERx "~W---") (MAKE-POO2)))
    (LET ((*PRINT-LEVEL* 0))
      (FORMAT NIL (FORMATTERx "~W---") (MAKE-POO2)))
    (WITH-OUTPUT-TO-STRING (S) (PRIN1 (MAKE-POO2) S) (PRINC "---" S))))
 ("#<foo2
is 3>---"
  "#---" "#<foo2 is 3>---"))
(DEFSTRUCT (POO3 (:TYPE LIST)) (A 1) (B 2))
(DEFTEST XP333
  (PRINTS (MAKE-POO3)))
(DEFSTRUCT (POO4 (:INCLUDE POO2)) (E 1))
(DEFTEST XP334
  (PRINTS (MAKE-POO4)))
(defparameter *DT* (COPY-PPRINT-DISPATCH))
(DEFTEST XP336
 (LET ((*PRINT-PPRINT-DISPATCH* *DT*))
   (SET-PPRINT-DISPATCH '(CONS (MEMBER ZOTZ))
                        #'(LAMBDA (XP LIST)
                            (FORMAT XP
                                    (FORMATTER "~@{~@<ZOTZ-~W~:>~}")
                                    (CADR LIST)))
                        0
                        *DT*)
   (PLET 30 0
    (LET ((*print-circle* T))
      (FORMAT NIL (FORMATTER "~W") '((ZOTZ 1) (ZOTZ 2) (ZOTZ 3))))))
 "(ZOTZ-1 ZOTZ-2 ZOTZ-3)")
(DEFTEST XP337
 (LET ((*PRINT-PPRINT-DISPATCH* *DT*))
   (SET-PPRINT-DISPATCH '(CONS (MEMBER ZOTZ))
                        #'(LAMBDA (XP LIST)
                           (FORMAT XP (FORMATTER "~@<ZOTZ-~W~:>") (CADR LIST)))
                        0
                        *DT*)
   (PLET 60 0
    (LET ((*print-circle* T))
      (FORMAT NIL
              (FORMATTER "~W")
              (READ-FROM-STRING "(#1=(zotz 1) #1# (zotz (1 #2=(2 #2#))))")))))
 "(#1=ZOTZ-1 #1# ZOTZ-(1 #2=(2 #2#)))")
(DEFTEST XP338
 (LET ((*PRINT-PPRINT-DISPATCH* *DT*))
   (SET-PPRINT-DISPATCH '(CONS (MEMBER ZOTZ))
                        #'(LAMBDA (XP LIST)
                            (FORMAT XP (FORMATTER "~<~*ZOTZ-~W~:>") LIST))
                        0
                        *DT*)
   (PLET 30 0
    (LET ((*print-circle* T))
      (FORMAT NIL (FORMATTER "~W") '((ZOTZ 1) (ZOTZ 2) (ZOTZ 3))))))
 "(ZOTZ-1 ZOTZ-2 ZOTZ-3)")
(DEFTEST XP339
 (LET ((*PRINT-PPRINT-DISPATCH* *DT*))
   (SET-PPRINT-DISPATCH '(CONS (MEMBER ZOTZ))
                        #'(LAMBDA (XP LIST)
                            (FORMAT XP (FORMATTER "~<~*ZOTZ-~W~:>") LIST))
                        0
                        *DT*)
   (PLET 30 0
    (LET ((*print-circle* T))
      (FORMAT NIL
              (FORMATTER "~W")
              (READ-FROM-STRING "((zotz 1) #1=(zotz 2) #1#)")))))
 "(ZOTZ-1 #1=ZOTZ-2 #1#)")
(DEFTEST XP340
 (LET ((*PRINT-PPRINT-DISPATCH* *DT*))
   (SET-PPRINT-DISPATCH 'NULL (FORMATTER "()"))
   (PLET 20 0 (FORMAT NIL (FORMATTER "~W") NIL)))
 "()")
(DEFTEST XP341
 (LET ((*PRINT-PPRINT-DISPATCH* *DT*))
   (SET-PPRINT-DISPATCH '(CONS (NOT (SATISFIES NUMBERP)) (CONS (MEMBER A B C)))
                        (FORMATTER "~{~a+~a~}"))
   (PLET 20 0 (FORMAT NIL (FORMATTER "~W") '(A A))))
 "A+A")
(DEFTEST XP342
 (LET ((*PRINT-PPRINT-DISPATCH* *DT*))
   (SET-PPRINT-DISPATCH '(CONS (CONS) (CONS (MEMBER A)))
                        (FORMATTER "~{~a-~a~}")
                        1)
   (PLET 20 0 (FORMAT NIL (FORMATTER "~W") '((A) A))))
 "(A)-A")
(DEFTEST XP343
 (LET ((*PRINT-PPRINT-DISPATCH* *DT*))
   (SET-PPRINT-DISPATCH 'HASH-TABLE (FORMATTER "foof"))
   (PLET 20 0 (FORMAT NIL (FORMATTER "~W") (MAKE-HASH-TABLE))))
 "foof")
(DEFTEST XP344
 (LET ((*PRINT-PPRINT-DISPATCH* *DT*))
   (SET-PPRINT-DISPATCH '(AND INTEGER (SATISFIES EVENP))
                        (FORMATTER "+~D")
                        3
                        *DT*)
   (PLET 20 0 (FORMAT NIL (FORMATTER "~W") '(1 2 3 4))))
 "(1 +2 3 +4)")
(DEFTEST XP345
 (LET ((*PRINT-PPRINT-DISPATCH* *DT*))
   (SET-PPRINT-DISPATCH '(AND (MEMBER 10 11 12)) (FORMATTER "**~D") 20 *DT*)
   (PLET 20 0 (FORMAT NIL (FORMATTER "~W") '(1 12 3 11))))
 "(1 **12 3 **11)")
(DEFTEST XP346
 (LET ((*PRINT-PPRINT-DISPATCH* *DT*))
   (SET-PPRINT-DISPATCH '(AND (MEMBER 10 11 12)) NIL 0 *DT*)
   (PLET 20 0 (FORMAT NIL (FORMATTER "~W") '(1 12 3 11))))
 "(1 +12 3 11)")
(DEFTEST XP347
 (LET ((*PRINT-PPRINT-DISPATCH* *DT*))
   (SET-PPRINT-DISPATCH '(VECTOR * 4)
                        #'(LAMBDA (XP OBJ)
                            (FORMAT XP (FORMATTER "--~S") (COERCE OBJ 'LIST))))
   (PLET 20 0 (FORMAT NIL (FORMATTER "~W") '#(K L D A))))
 "--(K L D A)")
#+eclipse
(DEFTEST XP348
 (LET ((*PRINT-PPRINT-DISPATCH* *DT*))
   (SET-PPRINT-DISPATCH '(CONS (MEMBER UNWIND-PROTECT))
                        #'(LAMBDA (XP LIST)
                            (eclipse::PRINT-FANCY-FN-CALL XP LIST '(0 3 1 0))))
   (PLET 20 0
    (FORMAT NIL (FORMATTER "~W") '(UNWIND-PROTECT (OPEN F) (PRINT ERRORMSG)))))
 "(UNWIND-PROTECT
    (OPEN F)
 (PRINT ERRORMSG))")
(DEFTEST XP349
 (LET ((*PRINT-PPRINT-DISPATCH* *DT*))
   (SET-PPRINT-DISPATCH '(CONS (MEMBER UNWIND-PROTECT))
                        (PPRINT-DISPATCH '(UNWIND-PROTECT) NIL))
   (PLET 20 0
    (FORMAT NIL (FORMATTER "~W") '(UNWIND-PROTECT (OPEN F) (PRINT ERRORMSG)))))
 "(UNWIND-PROTECT
    (OPEN F)
  (PRINT ERRORMSG))")
#+symbolics
(DEFTEST XP350
 (LET ((*PRINT-PPRINT-DISPATCH* *DT*))
   (SET-PPRINT-DISPATCH '(CONS (MEMBER UNWIND-PROTECT)) NIL)
   (PLET 30 1
    (FORMAT NIL (FORMATTER "~W") '(UNWIND-PROTECT (OPEN F) (PRINT ERRORMSG)))))
"(UNWIND-PROTECT (OPEN F)
                (PRINT ERRORMSG))")
(DEFTEST XP351
 (LET ((*PRINT-PPRINT-DISPATCH* *DT*))
   (SET-PPRINT-DISPATCH 'CONS (FORMATTER "zot ~{~A ~}") 1)
   (PLET 20 0 (FORMAT NIL (FORMATTER "~W") '(SETQ A B))))
 "zot SETQ A B ")
(DEFTEST XP352
 (LET ((*PRINT-PPRINT-DISPATCH* *DT*))
   (SET-PPRINT-DISPATCH 'CONS (PPRINT-DISPATCH '(22 . 33) NIL) 1)
   (PLET 30 0
    (FORMAT NIL (FORMATTER "~W") '(UNWIND-PROTECT (OPEN F) (PRINT ERRORMSG)))))
 "(UNWIND-PROTECT (OPEN F)
 (PRINT ERRORMSG))")
(DEFTEST XP353
 (LET ((*PRINT-PPRINT-DISPATCH* *DT*))
   (SET-PPRINT-DISPATCH '(CONS) (FORMATTER "zoz ~{~A ~}") 2)
   (PLET 100 0 (FORMAT NIL (FORMATTER "~W") '(FOO BAR))))
 "zoz FOO BAR ")
(DEFTEST XP354
 (LET ((*PRINT-PPRINT-DISPATCH* *DT*))
   (SET-PPRINT-DISPATCH '(CONS INTEGER) (FORMATTER "int ~{~A ~}") 3)
   (PLET 100 0 (FORMAT NIL (FORMATTER "~W") '(3 BAR))))
 "int 3 BAR ")
(DEFTEST XP355
 (LET ((*PRINT-PPRINT-DISPATCH* *DT*))
   (SET-PPRINT-DISPATCH '(CONS (MEMBER A B)) (FORMATTER "pip ~{~A ~}") 3)
   (PLET 100 0 (FORMAT NIL (FORMATTER "~W") '(A BAR))))
 "pip A BAR ")
(DEFTEST XP356
 (LET ((*PRINT-PPRINT-DISPATCH* *DT*))
   (SET-PPRINT-DISPATCH '(CONS (MEMBER C)) (FORMATTER "pop ~{~A~}") 4)
   (PLET 100 0 (FORMAT NIL (FORMATTER "~W") '(A BAR))))
 "pip A BAR ")
(DEFTEST XP357
 (LET ((*PRINT-PPRINT-DISPATCH* *DT*))
   (LET ((DATA (MAKE-POO :A 10 :B 20)))
     (PLET 22 0 (FORMAT NIL (FORMATTER "~W") DATA))))
 "#S(POO :A +10 :B +20)")
(DEFTEST XP358
 (LET ((*PRINT-PPRINT-DISPATCH* *DT*))
   (SET-PPRINT-DISPATCH 'POO
                        #'(LAMBDA (XP OBJ)
                            (FORMAT XP
                                    (FORMATTER "foo-~A-~A")
                                    (POO-A OBJ)
                                    (POO-B OBJ))))
   (LET ((DATA (MAKE-POO :A 11 :B 21)))
     (PLET 20 0 (FORMAT NIL (FORMATTER "~W") DATA))))
 "foo-11-21")
(DEFTEST XP359 (PROGN (SETQ *DT* (COPY-PPRINT-DISPATCH NIL)) NIL) nil)
(DEFTEST XP360
 (LET ((*PRINT-PPRINT-DISPATCH* *DT*))
   (SET-PPRINT-DISPATCH '(CONS (MEMBER SETZ))
                        #'(LAMBDA (*STANDARD-OUTPUT* LIST)
                            (PPRINT-LOGICAL-BLOCK
                                (NIL LIST :PREFIX "[" :SUFFIX "]")
                              (PPRINT-POP)
                              (WRITE-STRING "SETZ")
                              (PPRINT-EXIT-IF-LIST-EXHAUSTED)
                              (WRITE-CHAR #\SPACE)
                              (PPRINT-INDENT :CURRENT 0)
                              (PPRINT-NEWLINE :MISER)
                              (LOOP (WRITE (PPRINT-POP))
                                    (PPRINT-EXIT-IF-LIST-EXHAUSTED)
                                    (WRITE-CHAR #\SPACE)
                                    (PPRINT-NEWLINE :FILL)
                                    (WRITE (PPRINT-POP))
                                    (PPRINT-EXIT-IF-LIST-EXHAUSTED)
                                    (WRITE-CHAR #\SPACE)
                                    (PPRINT-NEWLINE :LINEAR)))))
   NIL) nil)

(DEFTEST XP539 (PRINTS 'PRINT-FIXNUM))
(DEFTEST XP540
 (PRINTS '|fOo-BAR| (*PRINT-ESCAPE* NIL) (*PRINT-CASE* :CAPITALIZE)))
(DEFTEST XP541
 (PRINTS '|fOo-BAR| (*PRINT-ESCAPE* NIL) (*PRINT-CASE* :DOWNCASE)))
(DEFTEST XP542 (PRINTS '|fOo-BAR| (*PRINT-ESCAPE* NIL) (*PRINT-CASE* :UPCASE)))
(DEFTEST XP543 (PRINTS '||))
(DEFTEST XP544 (PRINTS '|abcdef|))
(DEFTEST XP545 (PRINTS '|4a|))
(DEFTEST XP546 (PRINTS 'A%))
(DEFTEST XP547 (PRINTS '%))
(DEFTEST XP548 (PRINTS '#*10101))
(DEFTEST XP549 (PRINTS *xp-array2* (*PRINT-ARRAY* NIL)))


(DEFTEST XP591 (PLET 15 0 (FORMAT NIL (FORMATTERx "aaa~@<bbb~_ccc~:>ddd~_eee")))
 "aaabbbcccdddeee")
(DEFTEST XP592 (PLET 14 0 (FORMAT NIL (FORMATTERx "aaa~@<bbb~_ccc~:>ddd~_eee")))
 "aaabbbcccddd
eee")
(DEFTEST XP593 (PLET 12 0 (FORMAT NIL (FORMATTERx "aaa~@<bbb~_ccc~:>ddd~_eee")))
 "aaabbbcccddd
eee")
(DEFTEST XP594 (PLET 11 0 (FORMAT NIL (FORMATTERx "aaa~@<bbb~_ccc~:>ddd~_eee")))
 "aaabbb
   cccddd
eee")
(DEFTEST XP595 (PLET 5 0 (FORMAT NIL (FORMATTERx "aaa~@<bbb~_ccc~:>ddd~_eee")))
 "aaabbb
   cccddd
eee")
(DEFTEST XP596
 (PLET 15 0 (FORMAT NIL (FORMATTERx "a~@<aa~@<bbb~_ccc~:>ddd~_ee~:>e")))
 "aaabbbcccdddeee")
(DEFTEST XP597
 (PLET 14 0 (FORMAT NIL (FORMATTERx "a~@<aa~@<bbb~_ccc~:>ddd~_ee~:>e")))
 "aaabbbcccddd
 eee")
(DEFTEST XP598
 (PLET 12 0 (FORMAT NIL (FORMATTERx "a~@<aa~@<bbb~_ccc~:>ddd~_ee~:>e")))
 "aaabbbcccddd
 eee")
(DEFTEST XP599
 (PLET 11 0 (FORMAT NIL (FORMATTERx "a~@<aa~@<bbb~_ccc~:>ddd~_ee~:>e")))
 "aaabbb
   cccddd
 eee")
(DEFTEST XP600
 (PLET 5 0 (FORMAT NIL (FORMATTERx "a~@<aa~@<bbb~_ccc~:>ddd~_ee~:>e")))
 "aaabbb
   cccddd
 eee")