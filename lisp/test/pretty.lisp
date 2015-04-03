
(eval-when (:compile-toplevel :execute)
  (unless (fboundp 'preserving-package)
    (defmacro preserving-package (&body body)
      `(let ((*package* (cl:find-package ,(cl:package-name *package*))))
	 ,@body))))

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
      .,body)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest print-lines
  (with-output-to-string (s)
    (plet 18 nil (let ((*print-lines* 3))
		   (pprint '(setq a 1 b 2 c 3 d 4) s))))
  "
(SETQ A 1
      B 2
      C 3 ..)")

(defun pprint-defun (list)
  (pprint-logical-block (nil list :prefix "(" :suffix ")")
    (write (first list))
    (write-char #\space)
    (pprint-newline :miser)
    (pprint-indent :current 0)
    (write (second list))
    (write-char #\space)
    (pprint-newline :fill)
    (write (third list))
    (pprint-indent :block 1)
    (write-char #\space)
    (pprint-newline :linear)
    (write (fourth list))))

(deftest pprint-defun1
  (with-output-to-string (*standard-output*)
    (plet 26 nil (pprint-defun '(defun prod (y x) (* x y)))))
  "(DEFUN PROD (Y X) (* X Y))")

(deftest pprint-defun2
  (with-output-to-string (*standard-output*)
    (plet 25 nil (pprint-defun '(defun prod (y x) (* x y)))))
  "(DEFUN PROD (Y X)
  (* X Y))")

(deftest pprint-defun3
  (with-output-to-string (*standard-output*)
    (plet 15 nil (pprint-defun '(defun prod (y x) (* x y)))))
  "(DEFUN PROD
       (Y X)
  (* X Y))")

(deftest pprint-defun-miser
  (with-output-to-string (*standard-output*)
    (plet 15 14 (pprint-defun '(defun prod (y x) (* x y)))))
  "(DEFUN
 PROD
 (Y X)
 (* X Y))")

(deftest pprint-defun3-prefix
  (with-output-to-string (*standard-output*)
    (plet 20 nil
	  (pprint-logical-block (nil nil :per-line-prefix ";;; ")
	    (pprint-defun '(defun prod (y x) (* x y))))))
  ";;; (DEFUN PROD
;;;        (Y X)
;;;   (* X Y))")

(deftest pprint-defun-miserprefix
  (with-output-to-string (*standard-output*)
    (plet 20 15
	  (pprint-logical-block (nil nil :per-line-prefix ";;; ")
	    (pprint-defun '(defun prod (y x) (* x y))))))
  ";;; (DEFUN
;;;  PROD
;;;  (Y X)
;;;  (* X Y))")

(deftest pprint-roads
  (with-output-to-string (*standard-output*)
    (plet 25 nil
	  (princ "Roads ")
	  (pprint-tabular nil '(elm main maple center) nil nil 8)))
  "Roads ELM     MAIN
      MAPLE   CENTER")

(deftest pprint-fill
  (with-output-to-string (*standard-output*)
    (plet 15 nil
	  (pprint-fill nil '(12 34 567 8 9012 34 567 89 0 1 23))))
  "(12 34 567 8
 9012 34 567
 89 0 1 23)")
  

(DEFTEST XP8
 (FORMAT NIL
         "test
  test")
 "test
  test")
(DEFTEST XP9
 (FORMAT NIL
         "test

  test")
 "test

  test")
(DEFTEST XP10 (FORMAT NIL "test~&  test") "test
  test")
(DEFTEST XP11 (FORMAT NIL "test~&~&  test") "test
  test")
(DEFTEST XP12 (FORMAT NIL "test~2&  test") "test

  test")
(DEFTEST XP13 (FORMAT NIL "test ~~ test") "test ~ test")
(DEFTEST XP14 (FORMAT NIL "test ~#~ test") "test  test")
(DEFTEST XP15 (FORMAT NIL "test ~v~ test" 3) "test ~~~ test")
(DEFTEST XP16 (FORMAT NIL "test ~v~ test" NIL) "test ~ test")
(DEFTEST XP17
 (FORMAT NIL
         "test~
 	test")
 "testtest")
(DEFTEST XP18
 (FORMAT NIL
         "test~
  ")
 "test")
(DEFTEST XP19
 (FORMAT NIL
         "test~
 	test")
 "testtest")
(DEFTEST XP20
 (FORMAT NIL
         "test~:
  test")
 "test  test")
(DEFTEST XP21
 (FORMAT NIL
         "test~@
  test")
 "test
test")
(DEFTEST XP22 (FORMAT NIL "test~ta") "test a")
(DEFTEST XP23 (FORMAT NIL "test~,1ta") "test a")
(DEFTEST XP24 (FORMAT NIL "test2~ta") "test2 a")
(DEFTEST XP25 (FORMAT NIL "-te~5,2ta") "-te  a")
(DEFTEST XP26 (FORMAT NIL "-tes~5,2ta") "-tes a")
(DEFTEST XP27 (FORMAT NIL "-test~5,2ta") "-test  a")
(DEFTEST XP28 (FORMAT NIL "-teste~5,2ta") "-teste a")
(DEFTEST XP29 (FORMAT NIL "-tester~5,2ta") "-tester  a")
(DEFTEST XP30 (FORMAT NIL "-te~5,0ta") "-te  a")
(DEFTEST XP31 (FORMAT NIL "-tes~5,0ta") "-tes a")
(DEFTEST XP32 (FORMAT NIL "-test~5,0ta") "-testa")
(DEFTEST XP33 (FORMAT NIL "-teste~5,0ta") "-testea")
(DEFTEST XP34 (FORMAT NIL "-tester~5,0ta") "-testera")
(DEFTEST XP35 (FORMAT NIL "-te~0,2ta") "-te a")
(DEFTEST XP36 (FORMAT NIL "-tes~0,2ta") "-tes  a")
(DEFTEST XP37 (FORMAT NIL "-test~0,2ta") "-test a")
(DEFTEST XP38 (FORMAT NIL "-teste~0,2ta") "-teste  a")
(DEFTEST XP39 (FORMAT NIL "-tester~0,2ta") "-tester a")
(DEFTEST XP40 (FORMAT NIL "test~8,3ta") "test    a")
(DEFTEST XP41 (FORMAT NIL "test~V,Vta" 3 3) "test  a")
(DEFTEST XP42 (FORMAT NIL "+++~%-te~5,2ta") "+++
-te  a")
(DEFTEST XP43 (FORMAT NIL "+++~%-tes~5,2ta") "+++
-tes a")
(DEFTEST XP44 (FORMAT NIL "+++~%-test~5,2ta") "+++
-test  a")
(DEFTEST XP45 (FORMAT NIL "+++~%-teste~5,2ta") "+++
-teste a")
(DEFTEST XP46 (FORMAT NIL "+++~%-tester~5,2ta") "+++
-tester  a")
(DEFTEST XP47 (FORMAT NIL "test~@ta") "test a")
(DEFTEST XP48 (FORMAT NIL "-te~1,2@ta") "-te a")
(DEFTEST XP49 (FORMAT NIL "-tes~1,2@ta") "-tes  a")
(DEFTEST XP50 (FORMAT NIL "-test~1,2@ta") "-test a")
(DEFTEST XP51 (FORMAT NIL "-teste~1,2@ta") "-teste  a")
(DEFTEST XP52 (FORMAT NIL "-tester~1,2@ta") "-tester a")
(DEFTEST XP53 (FORMAT NIL "-te~0,2@ta") "-te a")
(DEFTEST XP54 (FORMAT NIL "-tes~0,2@ta") "-tesa")
(DEFTEST XP55 (FORMAT NIL "-test~0,2@ta") "-test a")
(DEFTEST XP56 (FORMAT NIL "-teste~0,2@ta") "-testea")
(DEFTEST XP57 (FORMAT NIL "-tester~0,2@ta") "-tester a")
(DEFTEST XP58 (FORMAT NIL "-te~3,0@ta") "-te   a")
(DEFTEST XP59 (FORMAT NIL "-tes~3,0@ta") "-tes   a")
(DEFTEST XP60 (FORMAT NIL "-test~3,0@ta") "-test   a")
(DEFTEST XP61 (FORMAT NIL "-te~3@ta") "-te   a")
(DEFTEST XP62 (FORMAT NIL "-tes~3@ta") "-tes   a")
(DEFTEST XP63 (FORMAT NIL "-test~3@ta") "-test   a")
(DEFTEST XP64 (FORMAT NIL "-te~0,0@ta") "-tea")
(DEFTEST XP65 (FORMAT NIL "-tes~0,0@ta") "-tesa")
(DEFTEST XP66 (FORMAT NIL "-test~0,0@ta") "-testa")
(DEFTEST XP67 (FORMAT NIL "test~8@ta") "test        a")
(DEFTEST XP68 (FORMAT NIL "test~8,3@ta") "test        a")
(DEFTEST XP69 (FORMAT NIL "test~V,V@ta" 8 5) "test           a")
(DEFTEST XP70 (FORMAT NIL "~a~a~*~a" 1 2 3 4 5 6 7) "124")
(DEFTEST XP71 (FORMAT NIL "~a~a~2*~a" 1 2 3 4 5 6 7) "125")
(DEFTEST XP72 (FORMAT NIL "~a~a~V*~a" 1 2 3 4 5 6 7) "127")
(DEFTEST XP73 (FORMAT NIL "~a~a~:*~a" 1 2 3 4 5 6 7) "122")
(DEFTEST XP74 (FORMAT NIL "~a~a~2:*~a" 1 2 3 4 5 6 7) "121")
(DEFTEST XP75 (FORMAT NIL "~a~a~V:*~a" 1 2 3 4 5 6 7) "121")
(DEFTEST XP76 (FORMAT NIL "~a~a~1@*~a" 1 2 3 4 5 6 7) "122")
(DEFTEST XP77 (FORMAT NIL "~a~a~3@*~a" 1 2 3 4 5 6 7) "124")
(DEFTEST XP78 (FORMAT NIL "~a~a~V@*~a" 1 2 3 4 5 6 7) "124")
(DEFTEST XP79 (FORMAT NIL "test~d ~? test" 2 "(item ~a)" '(4))
 "test2 (item 4) test")
(DEFTEST XP80 (FORMAT NIL (FORMATTER "test~d ~? test") 2 "(item ~a)" '(4))
 "test2 (item 4) test")
(DEFTEST XP81
 (LET ((STRING "(item ~a)"))
   (LIST (FORMAT NIL STRING 4)
         (FORMAT NIL (FORMATTER "test~d ~@? test") 2 STRING 4)
         (FORMAT NIL (FORMATTER "test~d ~@? test") 2 STRING 4)))
 ("(item 4)" "test2 (item 4) test" "test2 (item 4) test"))
(DEFTEST XP82
 (PLET 20 0
  (FORMAT NIL (FORMATTER "test~d ~? test") 2 (FORMATTER "(item ~a~^)") '(4)))
 "test2 (item 4 test")
(DEFTEST XP83
 (PLET 20 0
  (FORMAT NIL
          (FORMATTER "test~d ~? ~D test")
          2
          (FORMATTER "(item ~a~0^)")
          '(4 5)
          6))
 "test2 (item 4 6 test")
(DEFTEST XP84 (FORMAT NIL "test~d ~@? test ~A" 2 "(item ~a)" 4 5)
 "test2 (item 4) test 5")
(DEFTEST XP85 (FORMAT NIL (FORMATTER "test~d ~@? test ~A") 2 "(item ~a)" 4 5)
 "test2 (item 4) test 5")
(DEFTEST XP86
 (FORMAT NIL (FORMATTER "test~d ~@? test ~A") 2 "(item ~a~1:*)" 4 5)
 "test2 (item 4) test 4")
(DEFTEST XP87
 (PLET 20 0
  (FORMAT NIL
          (FORMATTER "test~d ~@? test ~A")
          2
          (FORMATTER "(item ~a~0^)")
          4
          5))
 "test2 (item 4 test 5")
(DEFTEST XP88 (FORMAT NIL "tEst~(tesT ~a~) test" 'ONE) "tEsttest one test")
(DEFTEST XP89 (FORMAT NIL "tEst~:(tesT ~a~) test" 'ONE) "tEstTest One test")
(DEFTEST XP90 (FORMAT NIL "tEst~:(tesT~a~) test" 'ONE) "tEstTestone test")
(DEFTEST XP91 (FORMAT NIL (FORMATTER "tEst~:( tesT~T~a~) test") 'ONE)
 "tEst Test One test")
(DEFTEST XP92 (FORMAT NIL "tEst~@( tesT ~a~) test" 'ONE) "tEst Test one test")
(DEFTEST XP93 (FORMAT NIL "tEst~:@( tesT ~a~) test" 'ONE) "tEst TEST ONE test")
(DEFTEST XP94
 (PLET 35 0
  (FORMAT NIL
          (FORMATTER "~:(~W~)")
          '(LET ((A (FOO 3)) (B (FOO 4)) (C 1))
             (TUZ A B))))
 "(Let ((A (Foo 3))
      (B (Foo 4))
      (C 1))
  (Tuz A B))")
(DEFTEST XP95
 (PLET 50 0
  (FORMAT NIL
          (FORMATTER
           "foo ~@<aa~@;p~:@_ ~:@(hi ~@<bb ~@;q~(~:@_R~:@_S~)~:>~:@_t~)~:>u")))
 "foo aap
    aa HI BB Q
    aa    BB R
    aa    BB S
    aaTu") 
(DEFTEST XP96 (FORMAT NIL "~[a~;b~;c~]" 1) "b")
(DEFTEST XP97 (FORMAT NIL "~2[a~;b~;c~]") "c")
(DEFTEST XP98 (FORMAT NIL "~[foo~]" 1) "")
(DEFTEST XP99 (FORMAT NIL "~[a~;b~:;c~]" 10) "c")
(DEFTEST XP100 (FORMAT NIL "~:[a~;b~]" NIL) "a") 
(DEFTEST XP101 (FORMAT NIL "~:[a~;b~]" 3) "b")
(DEFTEST XP102 (FORMAT NIL "~@[~A~A~] ~A" 1 2 3) "12 3")
(DEFTEST XP103 (FORMAT NIL "~@[~A~A~] ~A" NIL 2 3) " 2")
(DEFTEST XP104 (FORMAT NIL "~{~a~^,~}." '(1 2 3 4)) "1,2,3,4.")
(DEFTEST XP105 (FORMAT NIL "~V{~a~^,~}." 2 '(1 2 3 4)) "1,2,.")
(DEFTEST XP106 (FORMAT NIL "~2{~a~^,~}." '(1)) "1.")
(DEFTEST XP107 (FORMAT NIL "~{foo~:}." 'NIL) "foo.")
(DEFTEST XP108 (FORMAT NIL "~{~a~#,1^,~}." '(1 2 3 4)) "1,2,3.")
#-(and (or cmu excl) (not eclipse))
(DEFTEST XP109 (FORMAT NIL "~{~a~3@*~^,~a~^,~}." '(1 2 3 4 5 6 7 8 9))
 "1,4,5,8,9.")
(DEFTEST XP110 (FORMAT NIL "~:{~a~^,~}." '((1 2) (3 4))) "1,3,.")
(DEFTEST XP111 (FORMAT NIL "~V:{~a~^,~}." 1 '((1 2) (3 4))) "1,.")
(DEFTEST XP112 (FORMAT NIL "~1:{~a~^,~}." 'NIL) ".")
(DEFTEST XP113 (FORMAT NIL "~:{foo~:}." '(NIL)) "foo.")
(DEFTEST XP114 (FORMAT NIL "~:{~a~:^,~}." '((1 2) (3 4))) "1,3.")
(DEFTEST XP115 (FORMAT NIL "~:{~a~1,1:^,~}." '((1 2) (3 4))) "1.")
(DEFTEST XP116 (FORMAT NIL "~:{~a~#,1:^,~}." '((1 2) (3 4))) "1.")
(DEFTEST XP117 (FORMAT NIL "~:{~a~#:^,~}." '((1) (3 4))) "1.")
(DEFTEST XP118 (FORMAT NIL "~:{~a~3@*~^,~a~^,~}." '((1 2 3 4 5) (6 7 8 9)))
 "1,4,6,9.")
(DEFTEST XP119 (FORMAT NIL "~@{~a~^,~}." 1 2 3 4) "1,2,3,4.")
(DEFTEST XP120 (FORMAT NIL "~@{~a~1,1^,~}." 1 2 3 4) "1.")
(DEFTEST XP121 (FORMAT NIL "~@{~a~1,1,1^,~}." 1 2 3 4) "1.")
(DEFTEST XP122 (FORMAT NIL "~@{~a~2,1,1^,~}." 1 2 3 4) "1,2,3,4,.")
(DEFTEST XP123 (FORMAT NIL "~V@{~a~^,~}." 2 1 2 3 4) "1,2,.")
(DEFTEST XP124 (FORMAT NIL "~2@{~a~^,~}." 1) "1.")
(DEFTEST XP125 (FORMAT NIL "~@{foo~:}.") "foo.")
(DEFTEST XP126 (PLET 20 0 (FORMAT NIL (FORMATTER "~@{~a~#,1^,~} ~A.") 1 2 3 4))
 "1,2,3 4.")
#-(and (or cmu excl) (not eclipse))
(DEFTEST XP127 (FORMAT NIL "~@{~a~3@*~^,~a~^,~}." 1 2 3 4 5 6 7 8 9)
 "1,4,5,8,9.")
(DEFTEST XP128 (FORMAT NIL "~:@{~a~^,~}." '(1 2) '(3 4)) "1,3,.")
(DEFTEST XP129 (FORMAT NIL "~V:@{~a~^,~}." 1 '(1 2) '(3 4)) "1,.")
(DEFTEST XP130 (FORMAT NIL "~1:@{~a~^,~}.") ".")
(DEFTEST XP131 (FORMAT NIL "~:@{foo~:}." NIL) "foo.")
(DEFTEST XP132 (FORMAT NIL "~:@{foo~}.") ".")
(DEFTEST XP133 (FORMAT NIL "~:@{~a~:^,~}." '(1 2) '(3 4)) "1,3.")
(DEFTEST XP134 (FORMAT NIL "~:@{~a~1,1:^,~}." '(1 2) '(3 4)) "1.")
(DEFTEST XP135 (FORMAT NIL "~:@{~a~#,1:^,~}." '(1 2) '(3 4)) "1.")
(DEFTEST XP136 (FORMAT NIL "~:@{~a~#:^,~}." '(1) '(3 4)) "1.")
(DEFTEST XP137 (FORMAT NIL "~:@{~a~3@*~^,~a~^,~}." '(1 2 3 4 5) '(6 7 8 9))
 "1,4,6,9.")
(DEFTEST XP138 (FORMAT NIL "~{~}." "~A+~A," '(1 2 3 4)) "1+2,3+4,.")
(DEFTEST XP139 (FORMAT NIL "~@{~}." "~A+~A," 1 2 3 4) "1+2,3+4,.")
(DEFTEST XP140 (FORMAT NIL "~:{~}." "~A+~A," '((1 2) (3 4))) "1+2,3+4,.")
(DEFTEST XP141 (FORMAT NIL "~:@{~}." "~A+~A," '(1 2) '(3 4)) "1+2,3+4,.")
(DEFTEST XP142 (FORMAT NIL (FORMATTER "~{~}.") (FORMATTER "~A~^+~A,") '(1 2 3))
 "1+2,3.")
(DEFTEST XP143
 (FORMAT NIL (FORMATTER "~:{~}.") (FORMATTER "~A~^+~A,") '((1) (2 3))) "12+3,.")

(DEFTEST XP144 (FORMAT NIL (FORMATTER "test~:ta")) "testa")
(DEFTEST XP145 (FORMAT NIL (FORMATTER "test~8:ta")) "testa")
(DEFTEST XP146 (FORMAT NIL (FORMATTER "test~V,V:ta") 8 3) "testa")
(DEFTEST XP147 (FORMAT NIL (FORMATTER "test~0,3:ta")) "testa")
(DEFTEST XP148 (FORMAT NIL (FORMATTER "test~0,4:ta")) "testa")
(DEFTEST XP149 (FORMAT NIL (FORMATTER "test~0,5:ta")) "testa")
(DEFTEST XP150 (FORMAT NIL (FORMATTER "test~@:ta")) "testa")
(DEFTEST XP151 (FORMAT NIL (FORMATTER "test~8@:ta")) "testa")
(DEFTEST XP152 (FORMAT NIL (FORMATTER "test~V,V@:ta") 8 3) "testa")
(DEFTEST XP153 (FORMAT NIL (FORMATTER "test~8,5@:ta")) "testa")
(DEFTEST XP154 (FORMAT NIL (FORMATTER "test~0,3@:ta")) "testa")
(DEFTEST XP155 (FORMAT NIL (FORMATTER "test~0,4@:ta")) "testa")

(DEFTEST XP156 (FORMAT NIL (FORMATTER "fo-~<test~:ta~:>") NIL) "fo-test a")
(DEFTEST XP157 (FORMAT NIL (FORMATTER "fo-~<test~8:ta~:>") NIL) "fo-test    a")
(DEFTEST XP158 (FORMAT NIL (FORMATTER "fo-~<test~8,3:ta~:>") NIL) "fo-test    a")
(DEFTEST XP158a (FORMAT NIL (FORMATTER "fo-~<test~v,v:ta~:>") '(8 3)) "fo-test    a")
(DEFTEST XP159 (FORMAT NIL (FORMATTER "fo-~<test~0,3:ta~:>") NIL) "fo-test  a")
(DEFTEST XP160 (FORMAT NIL (FORMATTER "fo-~<test~0,4:ta~:>") NIL) "fo-test    a")
(DEFTEST XP161 (FORMAT NIL (FORMATTER "fo-~<test~0,5:ta~:>") NIL) "fo-test a")
(DEFTEST XP162 (FORMAT NIL (FORMATTER "fo-~:@_~<test~0,3ta~:>") NIL)
  #+waters-behavior "fo-
test  a"
  #-waters-behavior "fo-test  a")
(DEFTEST XP163 (FORMAT NIL (FORMATTER "fo-~:@_~<test~0,4ta~:>") NIL)
  #+waters-behavior "fo-
test    a"
  #-waters-behavior "fo-test a")
(DEFTEST XP164 (FORMAT NIL (FORMATTER "fo-~:@_~<test~0,5ta~:>") NIL)
  #+waters-behavior "fo-
test a"
  #-waters-behavior "fo-test   a")
(DEFTEST XP165 (FORMAT NIL (FORMATTER "fo-~<test~:@ta~:>") NIL) "fo-test a")
(DEFTEST XP166 (FORMAT NIL (FORMATTER "fo-~<test~8:@ta~:>") NIL) "fo-test        a")
(DEFTEST XP166a (FORMAT NIL (FORMATTER "fo-~<test~v,v:@ta~:>") '(8 3)) "fo-test        a")
(DEFTEST XP167 (FORMAT NIL (FORMATTER "fo-~<test~8,3:@ta~:>") NIL) "fo-test        a")
(DEFTEST XP168 (FORMAT NIL (FORMATTER "fo-~<test~8,5:@ta~:>") NIL) "fo-test           a")
(DEFTEST XP169 (FORMAT NIL (FORMATTER "fo-~<test~0,3:@ta~:>") NIL) "fo-test  a")
(DEFTEST XP170 (FORMAT NIL (FORMATTER "fo-~<test~0,4:@ta~:>") NIL) "fo-testa")
(DEFTEST XP171 (FORMAT NIL (FORMATTER "fo-~<test~6,4ta~:>") NIL) "fo-test   a")
(DEFTEST XP172 (FORMAT NIL (FORMATTER "fo-~<test~6,3ta~:>") NIL) "fo-test  a")

(DEFTEST XP175
 (PLET 20 0 (FORMAT NIL (FORMATTER "~<~@{~A~^ ~}~:>") '(1 2 . 3))) "1 2 . 3")
(DEFTEST XP176 (PLET 20 0 (FORMAT NIL (FORMATTER "tes~<t~ta~:>") NIL)) "test a")
(DEFTEST XP177 (PLET 20 0 (FORMAT NIL (FORMATTER "tes~<t~8ta~:>") NIL))
 "test    a")
(DEFTEST XP178 (PLET 20 0 (FORMAT NIL (FORMATTER "tes~<t~8,3ta~:>") NIL))
 "test    a")
(DEFTEST XP179 (PLET 20 0 (FORMAT NIL (FORMATTER "tes~<t~0,3ta~:>") NIL))
 "test  a")
(DEFTEST XP180 (PLET 20 0 (FORMAT NIL (FORMATTER "tes~<t~0,4ta~:>") NIL))
 "test    a")
(DEFTEST XP181 (PLET 20 0 (FORMAT NIL (FORMATTER "tes~<t~0,5ta~:>") NIL))
 "test a")
(DEFTEST XP182 (PLET 20 0 (FORMAT NIL (FORMATTER "tes~<t~@ta~:>") NIL))
 "test a")
(DEFTEST XP183 (PLET 20 0 (FORMAT NIL (FORMATTER "tes~<t~8@ta~:>") NIL))
 "test        a")
(DEFTEST XP184 (PLET 20 0 (FORMAT NIL (FORMATTER "tes~<t~8,3@ta~:>") NIL))
 "test        a")
(DEFTEST XP185 (PLET 20 0 (FORMAT NIL (FORMATTER "tes~<t~8,5@ta~:>") NIL))
 "test           a")
(DEFTEST XP186 (PLET 20 0 (FORMAT NIL (FORMATTER "tes~<t~0,3@ta~:>") NIL))
 "test  a")
(DEFTEST XP187 (PLET 20 0 (FORMAT NIL (FORMATTER "tes~<t~0,4@ta~:>") NIL))
 "testa")
(DEFTEST XP188
 (PLET 20 0 (FORMAT NIL (FORMATTER "~a~<~a~*~a~:>~a") 1 '(2 3 4 5 6 7) 0))
 "1240")
(DEFTEST XP189
 (PLET 20 0 (FORMAT NIL (FORMATTER "~a~<~a~0@*~a~:>~a") 1 '(2 3 4 5 6 7) 0))
 "1220")
(DEFTEST XP190
 (PLET 20 0 (FORMAT NIL (FORMATTER "~a~@<~a~*~a~:>") 1 2 3 4 5 6 7)) "124")
(DEFTEST XP191
 (PLET 20 0 (FORMAT NIL (FORMATTER "~a~@<~a~0@*~a~:>") 1 2 3 4 5 6 7)) "122")
(DEFTEST XP192 (PLET 20 0 (FORMAT NIL (FORMATTER "~a~<~a~^~a~:>~a") 1 '(2) 0))
 "120")
(DEFTEST XP193 (PLET 20 0 (FORMAT NIL (FORMATTER "~a~<~a~^~a~:>~a") 1 '(2) 0))
 "120")
(DEFTEST XP194
 (PLET 20 0 (FORMAT NIL (FORMATTER "~a~@<~a~#,4^~a~:>") 1 2 3 4 5 6)) "12")

