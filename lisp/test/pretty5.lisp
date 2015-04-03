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

(defmacro print*s (thing &rest bindings)
  `(plet 50 0
     (let* ,bindings
       (write-to-string ,thing))))

(defmacro print*c (thing &rest bindings)
  (push '(*print-circle* T) bindings)
  `(plet 150 0
     (let* ,bindings (write-to-string (read-from-string ,thing)))))

(defmacro format*c (string thing &rest bindings)
  (push '(*print-circle* T) bindings)
  `(plet 150 0
     (let* ,bindings
       (format nil ,string (read-from-string ,thing)))))

(DEFTEST XP433 (PRINT*C "#1=(define-modify-macro foo #1#)")
 "#1=(DEFINE-MODIFY-MACRO FOO #1#)")

(DEFTEST XP516 (PRINT*S "foo") "\"foo\"")
(DEFTEST XP517
 (PRINT*S "fo
o")
 "\"fo
o\"")
(DEFTEST XP518 (PRINT*S "foo" (*PRINT-ESCAPE* NIL)) "foo")
(DEFTEST XP519 (PRINT*S "fo\"o") "\"fo\\\"o\"")
(DEFTEST XP520 (PRINT*S "fo\"o" (*PRINT-ESCAPE* NIL)) "fo\"o")
(DEFTEST XP521 (PRINT*S "fo\\o") "\"fo\\\\o\"")
(DEFTEST XP522 (PRINT*S "fo\\o" (*PRINT-ESCAPE* NIL)) "fo\\o")
(DEFTEST XP523 (PRINT*S 20) "20")
(DEFTEST XP524 (PRINT*S 20 (*PRINT-BASE* 8)) "24")
(DEFTEST XP525 (PRINT*S 20 (*PRINT-RADIX* T)) "20.")
(DEFTEST XP526 (PRINT*S 1/2) "1/2")
(DEFTEST XP527 (PRINT*S 12345678901234567890) "12345678901234567890")
(DEFTEST XP528 (PRINT*S -20) "-20")
(DEFTEST XP529 (PRINT*S 1234567890) "1234567890")
(DEFTEST XP530 (PRINT*S 'FOO2) "FOO2")
(DEFTEST XP531 (PRINT*S 'FOO-BAR (*PRINT-CASE* :DOWNCASE)) "foo-bar")
(DEFTEST XP532 (PRINT*S 'FOO-BAR (*PRINT-CASE* :UPCASE)) "FOO-BAR")
(DEFTEST XP533 (PRINT*S 'FOO-BAR (*PRINT-CASE* :CAPITALIZE)) "Foo-Bar")
(DEFTEST XP534 (PRINT*S 'FOO (*PRINT-ESCAPE* NIL)) "FOO")
(DEFTEST XP535 (PRINT*S ':FOO) ":FOO")
(DEFTEST XP536 (PRINT*S ':FOO (*PRINT-ESCAPE* NIL)) "FOO")
(DEFTEST XP537 (PRINT*S '*<-+->*) "*<-+->*")
(DEFTEST XP538 (PRINT*S '*<-+->*) "*<-+->*")
(DEFTEST XP550 (PRINT*C "(A A NIL NIL B B)" (*print-circle* T))
 "(A A NIL NIL B B)")
(DEFTEST XP551 (PRINT*C "(Ac$ A NIL NIL B B)" (*PRINT-RIGHT-MARGIN* 15))
 "(AC$ A NIL NIL
 B B)")
(DEFTEST XP552 (PRINT*C "(1 #1=#:FOO #1# #1# . #1#)" (*print-circle* T))
 "(1 #1=#:FOO #1# #1# . #1#)")
(DEFTEST XP553 (PRINT*C "(1 #1=#:FOO #1# #1# . #1#)" (*print-circle* NIL))
 "(1 #:FOO #:FOO #:FOO . #:FOO)")
(DEFTEST XP554 (PRINT*C "(1 #1=#:FOO #1# . #1#)" (*PRINT-GENSYM* NIL))
 "(1 FOO FOO . FOO)")
(DEFTEST XP555
 (PRINT*C "(1 #1=#:FOO #1# . #1#)" (*PRINT-LENGTH* 2) (*print-circle* T))
 "(1 #:FOO ...)")
(DEFTEST XP556 (PRINT*C "(0 (#1=(1 2) B) #1# . #1#)" (*print-circle* T))
 "(0 (#1=(1 2) B) #1# . #1#)")
(DEFTEST XP557 (PRINT*C "(0 (#1=(1 2) B) #1# . #1#)" (*print-circle* NIL))
 "(0 ((1 2) B) (1 2) 1 2)")
(DEFTEST XP558 (PRINT*C "#1=#(0 (1 #2=(A B) #1#) #2#)" (*print-circle* T))
 "#1=#(0 (1 #2=(A B) #1#) #2#)")
(DEFTEST XP560
 (PRINT*C "(COND #1=((PLUSP X) Y) (X Y) . #1#)" (*print-circle* T))
 "(COND #1=((PLUSP X) Y) (X Y) . #1#)")
(DEFTEST XP561
 (PRINT*C "(COND #1=((PLUSP X) Y) (X Y) . #1#)" (*print-circle* NIL))
 "(COND ((PLUSP X) Y) (X Y) (PLUSP X) Y)")
(DEFTEST XP562 (PRINT*C "(A (B . #1=(C D)) #1# . #1#)")
 "(A (B . #1=(C D)) #1# . #1#)")
(DEFTEST XP563 (PRINT*C "(A (B . #1=(C D)) #1# . #1#)" (*print-circle* NIL))
 "(A (B C D) (C D) C D)")
(DEFTEST XP564 (PRINT*C "(A (B . #1=(C #1#)) #1#)" )
 "(A (B . #1=(C #1#)) #1#)")
(DEFTEST XP566
 (PRINT*C "(A (B . #1=(C . #2=(D E))) #2# #1# F)" )
 "(A (B . #2=(C . #1=(D E))) #1# #2# F)")
(DEFTEST XP567
 (PRINT*C "(A (B . #1=(C . #2=(D E))) #2# #1# F)" (*print-circle* NIL))
 "(A (B C D E) (D E) (C D E) F)")
(DEFTEST XP568
 (PRINT*C "(A (B . #1=(C . #2=(D E))) #2# #1# F)" (*PRINT-LEVEL* 2)
  )
 "(A (B . #2=(C . #1=(D E))) #1# #2# F)")
(DEFTEST XP569
 (PRINT*C "(A ((B . #1=(C . #2=(D E)))) #2# #1# F)" (*PRINT-LEVEL* 2)
  )
 "(A (#) #1=(D E) (C . #1#) F)")
(DEFTEST XP570
 (PRINT*C "(setq A #1=(car f) B C D #1#)" (*PRINT-LINES* 2)
  (*PRINT-RIGHT-MARGIN* 20) )
 "(SETQ A (CAR F)
      B C ..)")
(DEFTEST XP571
 (PRINT*C "(setq A #1=(car f) B C D #1#)" (*PRINT-LINES* 1)
  (*PRINT-RIGHT-MARGIN* 20) )
 "(SETQ A (CAR F) ..)")
(DEFTEST XP572
 (FORMAT*C (FORMATTER "~:<~:<~W ~W ~W ~W~:>~:>") "(#1=(1 a #1# 2 3 4))"
  )
 "(#1=(1 A #1# 2))")

(DEFTEST XP574
 (FORMAT*C (FORMATTER "~:<~:<~W ~W ~W ~W~:>~:>") "((1 #1=(a) #1# 2 3 4))"
  )
 "((1 #1=(A) #1# 2))")
(DEFTEST XP575
 (FORMAT*C (FORMATTER "~:<~:<~W ~W ~W ~W~:>~:>") "((1 #1=(a) #1# 2 3 4))"
  (*print-circle* NIL))
 "((1 (A) (A) 2))")
(DEFTEST XP576
 (FORMAT*C (FORMATTER "~:<~W ~W ~W ~W~:>") "#1=(1 a #1# 2 3 4)"
  )
 "#1=(1 A #1# 2)")
(DEFTEST XP578
 (FORMAT*C (FORMATTER "~:<~a ~a ~W ~a~:>") "#1=(1 a #1# 2 3 4)"
  )
 "#1=(1 A #1# 2)")
(DEFTEST XP580
 (FORMAT*C (FORMATTER "~:<~a ~a ~<~:> ~a~:>") "#1=(1 a #1# 2 3 4)"
  )
 "#1=(1 A #1# 2)")
(DEFTEST XP582
 (FORMAT*C (FORMATTER "~:<~W ~W ~W ~W~:>") "#1=(1 2 . #1#)" )
 "#1=(1 2 . #1#)")
(DEFTEST XP583
 (FORMAT*C (FORMATTER "~:<~W ~W ~W ~W~:>") "#1=(1 2 . #1#)"
  (*print-circle* NIL))
 "(1 2 1 2)")
(DEFTEST XP584
 (FORMAT*C (FORMATTER "~:<~a ~a ~a ~a~:>") "#1=(1 2 . #1#)" )
 "#1=(1 2 . #1#)")
(DEFTEST XP585
 (FORMAT*C (FORMATTER "~:<~a ~a ~a ~a~:>") "#1=(1 2 . #1#)"
  (*print-circle* NIL))
 "(1 2 1 2)")
(DEFTEST XP586
 (FORMAT*C (FORMATTER "~:<~a ~a ~a ~2@*~a ~a~:>") "#1=(1 2 3 4 5)"
  )
 "(1 2 3 3 4)")
(DEFTEST XP587
 (FORMAT*C (FORMATTER "~:<~a ~a ~a ~2:*~a ~a~:>") "#1=(1 2 3 4 5)"
  )
 "(1 2 3 2 3)")
(DEFTEST XP588
 (FORMAT*C (FORMATTER "~:<~W ~W ~:@<~W ~W~:>~:>") "#1=(1 a b . #1#)"
  )
 "#1=(1 A (B . #1#))")
(DEFTEST XP589
 (FORMAT*C (FORMATTER "~@{~:<~W ~W ~:@<~W ~W~:>~:>~}") "#1=(1 a . #1#)"
  )
 "#1=(1 A #1#)")
(DEFTEST XP590
 (FORMAT*C (FORMATTER "~:<~W ~W ~:@<~W ~W~:>~:>") "#1=(1 . #1#)"
  )
 "#1=(1 . #1#)")
(DEFTEST XP601
 (PLET 15 0
  (LET ((*PRINT-LINES* 1))
    (FORMAT NIL (FORMATTER "~W") '(101 BAR B ZOTO))))
 "(101 BAR B ..)")
(DEFTEST XP602
 (PLET 15 0
  (LET ((*PRINT-LINES* 1))
    (FORMAT NIL (FORMATTER "~W") '(101 BAR BA ZOTO))))
 "(101 BAR BA ..)")
(DEFTEST XP603
 (PLET 15 0
  (LET ((*PRINT-LINES* 1))
    (FORMAT NIL (FORMATTER "~W") '(101 BAR BAZ ZOTO))))
 "(101 BAR ..)")
(DEFTEST XP604
 (PLET 15 0
  (LET ((*PRINT-LINES* 1))
    (FORMAT NIL (FORMATTER "~W") '(101 (20 2) ZOTO))))
 "(101 (20 2) ..)")
(DEFTEST XP605
 (PLET 15 0
  (LET ((*PRINT-LINES* 1))
    (FORMAT NIL (FORMATTER "~W") '(101 (20 20) ZOTO))))
 "(101 ..)")
(DEFTEST XP606
 (PLET 15 0
  (LET ((*PRINT-LINES* 2))
    (FORMAT NIL
            (FORMATTER "~:<---~<~;~a ~_~a ~_~a~;>+~:>--~:>")
            '((12 3456 789)))))
 "(---12
    3456 ..>+)")
(DEFTEST XP607
 (PLET 20 0
  (WITH-OUTPUT-TO-STRING (S)
    (PRINC "abcde" S)
    (FORMAT S (FORMATTER "~%~@<1234~:@_5678~:>"))))
 "abcde
1234
5678")
(DEFTEST XP608 (PLET 20 0 (FORMAT NIL (FORMATTER "~@<foo ~4:ia~:@_b~:>")))
 "foo a
        b")
(DEFTEST XP609 (PLET 20 0 (FORMAT NIL (FORMATTER "~@<foo ~4:ia~:@_~:@_b~:>")))
 "foo a

        b")

(DEFTEST XP610
 (PROGN
  (PLET 85 0
   (FORMAT NIL
           (FORMATTER "~W")
           '((((((((((((((((((((((((((((((((((((((SETQ A
                                                         B
                                                       C
                                                         D)))))))))))))))))))))))))))))))))))))))))
 "((((((((((((((((((((((((((((((((((((((SETQ A B
                                           C D))))))))))))))))))))))))))))))))))))))")
(DEFTEST XP611
 (PROGN
  (PLET 200 0
   (FORMAT NIL
           (FORMATTER "~W")
           '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
             1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
             1 1 1 1 1 1 1 1 1 1 1 1 1 1))))
 "(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)")
(DEFTEST XP612
 (PROGN
  (PLET 50 0
   (FORMAT NIL
           (FORMATTER "~W")
           '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
             1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
             1 1 1 1 1 1 1 1 1 1 1 1 1 1))))
 "(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
 1 1 1 1 1 1 1 1)")

#+not-yet				;tests of formatter error checking
(progn		
(DEFTEST XP613 ETEST (FORMAT NIL (FORMATTER "ab~1,2")) (1 2))
(DEFTEST XP614 ETEST (FORMAT NIL (FORMATTER "ab~1,'")) (2 5))
(DEFTEST XP615 ETEST (FORMAT NIL (FORMATTER "ab~1/foo")) (3 4))
(DEFTEST XP616 ETEST (FORMAT NIL (FORMATTER "ab~1{foo~(..~{..~} ~}")) (4 9))
(DEFTEST XP617 ETEST (FORMAT NIL (FORMATTER "ab~!foo")) (5 3))
(DEFTEST XP618 ETEST (FORMAT NIL (FORMATTER "ab~1,2:@Ifoo")) (6 6))
(DEFTEST XP619 ETEST (FORMAT NIL (FORMATTER "ab~2:@:Ifoo")) (7 6))
(DEFTEST XP620 ETEST (FORMAT NIL (FORMATTER "ab~2:@@Ifoo")) (8 6))
(DEFTEST XP621 ETEST (FORMAT NIL (FORMATTER "ab~2:%foo")) (9 5))
(DEFTEST XP622 ETEST (FORMAT NIL (FORMATTER "ab~2@%foo")) (10 5))
(DEFTEST XP623 ETEST (FORMAT NIL (FORMATTER "ab~2@:[foo~]")) (11 6))
(DEFTEST XP624 ETEST (FORMAT NIL (FORMATTER "ab~:[foo~]")) (13 4))
(DEFTEST XP625 ETEST (FORMAT NIL (FORMATTER "ab~@[foo~;bar~]")) (14 4))
(DEFTEST XP626 ETEST (FORMAT NIL (FORMATTER "ab foo~;bar~]")) (15 7))
(DEFTEST XP627 ETEST (FORMAT NIL (FORMATTER "ab ~(foo~]bar~)")) (16 9))
(DEFTEST XP628 ETEST (FORMAT NIL (FORMATTER "ab ~[foo~)bar~]")) (17 9))
(DEFTEST XP629 ETEST (FORMAT NIL (FORMATTER "ab ~[foo~>bar~]")) (18 9))
(DEFTEST XP630 ETEST (FORMAT NIL (FORMATTER "ab ~[foo~}bar~]")) (19 9))
(DEFTEST XP631 ETEST (FORMAT NIL (FORMATTER "ab ~#<ff~>foo")) (21 4))
(DEFTEST XP632 ETEST (FORMAT NIL (FORMATTER "ab ~<f~#%f~>foo")) (21 7))
(DEFTEST XP633 ETEST (FORMAT NIL (FORMATTER "ab ~<f~#af~>foo")) (21 7))
(DEFTEST XP634 ETEST (FORMAT NIL (FORMATTER "ab ~22<f~:;f~>foo")) (22 10))
(DEFTEST XP635 ETEST (FORMAT NIL (FORMATTER "ab ~<f~Wf~>foo")) (23 7))
(DEFTEST XP636 ETEST (FORMAT NIL (FORMATTER "ab ~<f~;g~;h~;f~:>foo")) (24 4))
(DEFTEST XP637 ETEST (FORMAT NIL (FORMATTER "ab ~<f~Af~;g~;h~:>foo")) (25 5))
(DEFTEST XP638 ETEST (FORMAT NIL (FORMATTER "ab ~<f~;g~;h~Ag~:>foo")) (26 11))
)

(DEFTEST XP639 (FORMAT NIL "A ~:<1 ~_2~:>~%B" NIL) "A (1 2)
B")

