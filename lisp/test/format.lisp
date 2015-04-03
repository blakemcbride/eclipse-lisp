(eval-when (:compile-toplevel :execute)
  (unless (fboundp 'preserving-package)
    (defmacro preserving-package (&body body)
      `(let ((*package* (cl:find-package ,(cl:package-name *package*))))
	 ,@body))))

(defun format-test (&rest args)
  (preserving-package 
   (apply #'format nil args)))

(defmacro defformat-test (name (&rest args) result)
  `(deftest ,name (format-test ,@args) ,result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FORMAT A

(defformat-test format-a1 ("foo")
"foo")

(defformat-test format-a2 ("format-a:--~a--end" (QUOTE AB\c))
"format-a:--ABc--end")

(deftest format-a3 (let ((y "elephant"))
		     (format-test "Look at the ~A!" Y))
"Look at the elephant!")

(defformat-test format-a4 ("format-%:--~%--1-newline-*")
"format-%:--
--1-newline-*")

(defformat-test format-a5 ("format-%:--~3%--3-newline-*")
"format-%:--


--3-newline-*")

(defformat-test format-a6 ("format-a:--~5a--end-*" (QUOTE AB\c))
"format-a:--ABc  --end-*")

(defformat-test format-a7 ("format-a:--~5,2a--end-*" (QUOTE AB\c))
"format-a:--ABc  --end-*")

(defformat-test format-a8 ("format-a:--~5,2,3a--end-*" (QUOTE AB\c))
"format-a:--ABc   --end-*")

(defformat-test format-a9 ("format-a:--~5,2,3,'*a--end-*" (QUOTE AB\c))
"format-a:--ABc***--end-*")

(defformat-test format-a10 ("format-a:--~@a--end-*" (QUOTE AB\c))
"format-a:--ABc--end-*")

(defformat-test format-a11 ("format-a:--~5@a--end-*" (QUOTE AB\c))
"format-a:--  ABc--end-*")

(defformat-test format-a12 ("format-a:--~5,2@a--end-*" (QUOTE AB\c))
"format-a:--  ABc--end-*")

(defformat-test format-a13 ("format-a:--~5,2,3@a--end-*" (QUOTE AB\c))
"format-a:--   ABc--end-*")

(defformat-test format-a14 ("format-a:--~5,2,3,'*@a--end-*" (QUOTE AB\c))
"format-a:--***ABc--end-*")

(defformat-test format-a15 ("format-a:--~:a--end-*" (QUOTE (AB\c NIL XYZ)))
"format-a:--(ABc NIL XYZ)--end-*")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FORMAT S
(defformat-test format-s1 ("format-s:--~s--end-*" (QUOTE AB\c))
"format-s:--|ABc|--end-*")

(defformat-test format-s2 ("format-s:--~5s--end-*" (QUOTE AB\c))
"format-s:--|ABc|--end-*")

(defformat-test format-s3 ("format-s:--~5,2s--end-*" (QUOTE AB\c))
 "format-s:--|ABc|--end-*")

(defformat-test format-s4 ("format-s:--~5,2,3s--end-*" (QUOTE AB\c))
 "format-s:--|ABc|   --end-*")

(defformat-test format-s5 ("format-s:--~5,2,3,'*s--end-*" (QUOTE AB\c))
 "format-s:--|ABc|***--end-*")

(defformat-test format-s6 ("format-s:--~@s--end-*" (QUOTE AB\c))
 "format-s:--|ABc|--end-*")

(defformat-test format-s7 ("format-s:--~5@s--end-*" (QUOTE AB\c))
 "format-s:--|ABc|--end-*")

(defformat-test format-s8 ("format-s:--~5,2@s--end-*" (QUOTE AB\c))
 "format-s:--|ABc|--end-*")

(defformat-test format-s9 ("format-s:--~5,2,3@s--end-*" (QUOTE AB\c))
 "format-s:--   |ABc|--end-*")

(defformat-test format-s10 ("format-s:--~5,2,3,'*@s--end-*" (QUOTE AB\c))
 "format-s:--***|ABc|--end-*")

(defformat-test format-s11 ("format-s:--~:s--end-*" (QUOTE (AB\c NIL XYZ)))
 "format-s:--(|ABc| NIL XYZ)--end-*")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FORMAT D
(defvar *integer-five* 5)

(defformat-test format-d1 ("The answer is ~D." *integer-Five*)
"The answer is 5.")

(defformat-test format-d2 ("The answer is ~3D." *integer-Five*)
"The answer is   5.")

(defformat-test format-d3 ("The answer is ~3,'0D." *integer-Five*)
"The answer is 005.")

(defformat-test format-d4 ("The answer is ~:D." (EXPT 47 *integer-Five*))
"The answer is 229,345,007.")

(defformat-test format-d5 ("decimal:~d, width=5:~5d-*" 10 10)
"decimal:10, width=5:   10-*")

(defformat-test format-d6 ("format-d:--~d--end-*" 123)
"format-d:--123--end-*")

(defformat-test format-d7 ("format-d:--~10d--end-*" 123)
"format-d:--       123--end-*")

(defformat-test format-d8 ("format-d:--~10,'?d--end-*" 123)
"format-d:--???????123--end-*")

(defformat-test format-d9 ("format-d:--~@d--end-*" 123)
"format-d:--+123--end-*")

(defformat-test format-d10 ("format-d:--~10@d--end-*" 123)
"format-d:--      +123--end-*")

(defformat-test format-d11 ("format-d:--~10,'?@d--end-*" 123)
"format-d:--??????+123--end-*")

(defformat-test format-d12 
    ("decimal pad with period:~10,vd-*" #\. 12)
  "decimal pad with period:........12-*")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FORMAT B
(defformat-test format-b1 ("format-b:--~b--end-*" 123)
"format-b:--1111011--end-*")

(defformat-test format-b2 ("format-b:--~10b--end-*" 123)
"format-b:--   1111011--end-*")

(defformat-test format-b3 ("format-b:--~10,'?b--end-*" 123)
"format-b:--???1111011--end-*")

(defformat-test format-b4 ("format-b:--~:b--end-*" 123)
"format-b:--1,111,011--end-*")

(defformat-test format-b5 ("format-b:--~10:b--end-*" 123)
"format-b:-- 1,111,011--end-*")

(defformat-test format-b6 ("format-b:--~10,'?:b--end-*" 123)
"format-b:--?1,111,011--end-*")

;; #- cmu
(defformat-test format-b7 ("format-b:--~10,'?,'.:b--end-*" 123)
"format-b:--?1.111.011--end-*")

(defformat-test format-b8 ("format-b:--~@b--end-*" 123)
"format-b:--+1111011--end-*")

(defformat-test format-b9 ("format-b:--~10@b--end-*" 123)
"format-b:--  +1111011--end-*")

(defformat-test format-b10 ("format-b:--~10,'?@b--end-*" 123)
"format-b:--??+1111011--end-*")

(defformat-test format-b11 ("format-b:--~:@b--end-*" 123)
"format-b:--+1,111,011--end-*")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FORMAT O
(defformat-test format-o1 ("format-o:--~o--end-*" 123)
"format-o:--173--end-*")

(defformat-test format-o2 ("format-o:--~10o--end-*" 123)
"format-o:--       173--end-*")

(defformat-test format-o3 ("format-o:--~10,'?o--end-*" 123)
"format-o:--???????173--end-*")

(defformat-test format-o4 ("format-o:--~@o--end-*" 123)
"format-o:--+173--end-*")

(defformat-test format-o5 ("format-o:--~10@o--end-*" 123)
"format-o:--      +173--end-*")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FORMAT X
(defformat-test format-x1 ("format-x:--~x--end-*" 123)
"format-x:--7B--end-*")

(defformat-test format-x2 ("format-x:--~10x--end-*" 123)
"format-x:--        7B--end-*")

(defformat-test format-x3 ("format-x:--~10,'?x--end-*" 123)
"format-x:--????????7B--end-*")

(defformat-test format-x4 ("format-x:--~10:x--end-*" 123)
"format-x:--        7B--end-*")

(defformat-test format-x5 ("format-x:--~@x--end-*" 123)
"format-x:--+7B--end-*")

(defformat-test format-x6 ("format-x:--~10@x--end-*" 123)
"format-x:--       +7B--end-*")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FORMAT R
(defformat-test format-r1 ("format-r:--~20r--end-*" 123)
"format-r:--63--end-*")

(defformat-test format-r2 ("format-r:--~20,10r--end-*" 123)
"format-r:--        63--end-*")

(defformat-test format-r3 ("format-r:--~20@r--end-*" 123)
"format-r:--+63--end-*")

(defformat-test format-r4 ("format-r:--~r--end-*" 9)
"format-r:--nine--end-*")

(defformat-test format-r5 ("format-r:--~:r--end-*" 9)
"format-r:--ninth--end-*")

(defformat-test format-r6 ("format-r:--~@r--end-*" 9)
"format-r:--IX--end-*")

(defformat-test format-r7 ("format-r:--~:@r--end-*" 9)
"format-r:--VIIII--end-*")

(defformat-test format-r8 ("cardinal:~r, roman new:~@r, roman-old:~:@r~
                <same line I hope>~@
                new line but at beginning~:
   same line, but spaced out~@
        new line and over two tabs-*" 4 4 4)
"cardinal:four, roman new:IV, roman-old:IIII<same line I hope>
new line but at beginning   same line, but spaced out
new line and over two tabs-*")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FORMAT P
(defformat-test format-p1 ("format-p:--~d  object~p-*" 1 1)
"format-p:--1  object-*")

(defformat-test format-p2 ("format-p:--~d  object~p-*" 2 2)
"format-p:--2  objects-*")

(defformat-test format-p3 ("format-p:--~d  bab~@p-*" 1 1)
"format-p:--1  baby-*")

(defformat-test format-p4 ("format-p:--~d  bab~@p-*" 2 2)
"format-p:--2  babies-*")

(defformat-test format-p5 ("format-p:--~d  object~:p-*" 1)
"format-p:--1  object-*")

(defformat-test format-p6 ("format-p:--~d  object~:p-*" 2)
"format-p:--2  objects-*")

(defformat-test format-p7 ("format-p:--~d  bab~:@p-*" 1)
"format-p:--1  baby-*")

(deftest format-p8
    (let ((n 3))
      (with-output-to-string (s)
	(FORMAT s "~D item~:P found.~%" N)
	(FORMAT s "~R dog~:[s are~; is~] here.~%" N (= N 1))
	(FORMAT s "~R dog~:*~[s are~; is~:;s are~] here.~%" N)
	(FORMAT s "Here ~[are~;is~:;are~] ~:*~R pupp~:@p." N)))
  "3 items found.
three dogs are here.
three dogs are here.
Here are three puppies.")

(deftest format-p9
    (let ((n 1))
      (with-output-to-string (s)
	(FORMAT s "~D item~:P found.~%" N)
	(FORMAT s "~R dog~:[s are~; is~] here.~%" N (= N 1))
	(FORMAT s "~R dog~:*~[s are~; is~:;s are~] here.~%" N)
	(FORMAT s "Here ~[are~;is~:;are~] ~:*~R pupp~:@p." N)))
  "1 item found.
one dog is here.
one dog is here.
Here is one puppy.")


(deftest format-p10
    (let ((n 0))
      (with-output-to-string (s)
	(FORMAT s "~D item~:P found.~%" N)
	(FORMAT s "~R dog~:[s are~; is~] here.~%" N (= N 1))
	(FORMAT s "~R dog~:*~[s are~; is~:;s are~] here.~%" N)
	(FORMAT s "Here ~[are~;is~:;are~] ~:*~R pupp~:@p." N)))
  "0 items found.
zero dogs are here.
zero dogs are here.
Here are zero puppies.")

(defformat-test format-p11
("~D tr~:@p/~D win~:P" 7 1)
"7 tries/1 win")

(defformat-test format-p12
("~D tr~:@p/~D win~:P" 1 0)
"1 try/0 wins")

(defformat-test format-p13 ("~D tr~:@p/~D win~:P" 1 3)
"1 try/3 wins")

(defformat-test format-p14 ("~d tr~@p/~d win~p" 1 1 0 0)
  "1 try/0 wins")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FORMAT C
(defformat-test format-c1
    ("char normal:~c, as # would read:~@c, human read:~:c, as prompted: ~:@c-*"
	    #\SPACE #\SPACE #\SPACE #\SPACE)
  "char normal: , as # would read:#\\Space, human read:Space, as prompted: Space-*")

(defformat-test format-c2
    ("char normal:~c, as # would read:~@c, human read:~:c, as prompted: ~:@c-*"
	    #\a #\a #\a #\a)
  "char normal:a, as # would read:#\\a, human read:a, as prompted: a-*")

(defformat-test format-c3
    ("char normal:~c, as # would read:~@c, human read:~:c, as prompted: ~:@c-*"
	    #\a #\a #\a #\a)
  "char normal:a, as # would read:#\\a, human read:a, as prompted: a-*")

(defformat-test format-c4
    ("char normal:~c, as # would read:~@c, human read:~:c, as prompted: ~:@c-*"
	    #\backspace #\backspace #\backspace #\backspace)
  "char normal:, as # would read:#\\Backspace, human read:Backspace, as prompted: Backspace (Control-H)-*")

#| ;;; These are just way too slow for now!!!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FORMAT F

(DEFUN f-format-test (X)
       (format-test "~6,2F|~6,2,1,'*F|~6,2,,'?F|~6F|~,2F|~F" X X X X
X X))


(deftest format-f1 (f-format-test 3.14159f0)
"  3.14| 31.42|  3.14|3.1416|3.14|3.14159")

(deftest format-f2 (f-format-test -3.14159f0)
" -3.14|-31.42| -3.14|-3.142|-3.14|-3.14159")

(deftest format-f3 (f-format-test 100.0f0)
"100.00|******|100.00| 100.0|100.00|100.0")

(deftest format-f4 (f-format-test 1234.0f0)
"1234.00|******|??????|1234.0|1234.00|1234.0")

(deftest format-f5 (f-format-test 0.006f0)
"  0.01|  0.06|  0.01| 0.006|0.01|0.006")

(defformat-test format-f6 ("~5,2,-13f" 1.1f13)
" 1.10")

(defformat-test format-f7 ("~9,0,6f" 3.14159f0)
" 3141590.")

(defformat-test format-f8 ("~5,3F" (QUOTE A))
"A")

(defformat-test format-f9 ("~5,3F" #C(1.2f0 0.3f0))
"#C(1.2 0.3)")

(defformat-test format-f10 ("~5,3F" 2/3)
"0.667")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FORMAT E

(defun e-format-test (x)
  (format-test
          "~9,2,1,,'*E|~10,3,2,2,'?,,'$E|~9,3,2,-2,'%@e|~9,2E"
          x x x x))
(deftest format-e1 (e-format-test 3.14159f0)
"  3.14E+0| 31.42$-01|+.003E+03|  3.14E+0")

(deftest format-e2 (e-format-test -3.14159f0)
" -3.14E+0|-31.42$-01|-.003E+03| -3.14E+0")

(deftest format-e3 (e-format-test 1100.0f0)
"  1.10E+3| 11.00$+02|+.001E+06|  1.10E+3")

(deftest format-e4 (e-format-test 1100.0D0)
"  1.10d+3| 11.00$+02|+.001d+06|  1.10d+3")

(deftest format-e5 (e-format-test 1.1f13)
"*********| 11.00$+12|+.001E+16| 1.10E+13")

(deftest format-e6 ("_~10,4E_" 1.2f0)
"_ 1.2000E+0_")

(deftest format-e7 ("~9,2,1E" 0.0314159f0)
"  3.14E-2")

(deftest format-e8
    (with-output-to-string (s)
      (dotimes (k 13)
	(format-test "Scale factor ~2D: |~13,6,2,VE|~%"
		(- k 5) (- k 5) 3.14159f0)))

"Scale factor  7: | 3141590.E-06|
Scale factor  6: | 314159.0E-05|
Scale factor  5: | 31415.90E-04|
Scale factor  4: | 3141.590E-03|
Scale factor  3: | 314.1590E-02|
Scale factor  2: | 31.41590E-01|
Scale factor  1: | 3.141590E+00|
Scale factor  0: | 0.314159E+01|
Scale factor -1: | 0.031416E+02|
Scale factor -2: | 0.003142E+03|
Scale factor -3: | 0.000314E+04|
Scale factor -4: | 0.000031E+05|
Scale factor -5: | 0.000003E+06|")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FORMAT G
(defun g-format-test (x)
  (format-test "~9,2,1,,'*G|~9,3,2,3,'?,,'$G|~9,3,2,0,'%G|~9,2G"
          x x x x))


(deftest format-g1 (g-format-test 0.0314159f0)
"  3.14E-2|314.2$-04|0.314E-01|  3.14E-2")

(deftest format-g2 (g-format-test 0.314159f0)
"  0.31   |0.314    |0.314    | 0.31    ")

(deftest format-g3 (g-format-test 3.14159f0)
"   3.1   | 3.14    | 3.14    |  3.1    ")

(deftest format-g4 (g-format-test 31.4159f0)
"   31.   | 31.4    | 31.4    |  31.    ")

(deftest format-g5 (g-format-test 314.159f0)
"  3.14E+2| 314.    | 314.    |  3.14E+2")

(deftest format-g6 (g-format-test 3141.59f0)
"  3.14E+3|314.2$+01|0.314E+04|  3.14E+3")

(deftest format-g7 (g-format-test 3141.59L0)
"  3.14L+3|314.2$+01|0.314L+04|  3.14L+3")

(deftest format-g8 (g-format-test 3.14f12)
"*********|314.0$+10|0.314E+13| 3.14E+12")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FORMAT $ !!!

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; format %
(defformat-test format-%1 ("format-%:--~%-*")
  "format-%:--
-*")
(defformat-test format-%2 ("format-%:--~3%-*")
  "format-%:--


-*")
(defformat-test format-%3 ("format-%:--~0%-*")
  "format-%:---*")
(defformat-test format-%4 ("format-%:--~v%-*" 2)
  "format-%:--

-*")
(defformat-test format-%5 ("test~%~%  test") "test

  test")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FORMAT &
(defformat-test format-&1 ("format-&:--~%~&--1-newline-*")
"format-&:--
--1-newline-*")

(defformat-test format-&2 ("format-&:--~%~3&--3-newline-*")
"format-&:--


--3-newline-*")

(defformat-test format-&3 ("Start test, newline:~%freshline:~&")
"Start test, newline:
freshline:
")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FORMAT |
(deftest format-page1
    (equal (format-test "~|")
	   (make-array 1 :element-type 'character :initial-element #\page))
  t)
(deftest format-page2
    (equal (format-test "~3|")
	   (make-array 3 :element-type 'character :initial-element #\page))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; format ~
(defformat-test format-tilde1 ("format-tilde:--~~--1-tilde-*")
"format-tilde:--~--1-tilde-*")

(defformat-test format-tilde2 ("format-tilde:--~3~--3-tilden-*")
"format-tilde:--~~~--3-tilden-*")

(defformat-test format-tilde3 ("format-|:--~|--1-ff-*")
"format-|:----1-ff-*")

(defformat-test format-tilde4 ("format-|:--~2|--2-ff-*")
"format-|:----2-ff-*")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FORMAT NEWLINE
(defformat-test format-nl1 ("format-<nl>:~
                         -*")
"format-<nl>:-*")

(defformat-test format-nl2 ("format-<nl>:~@
                         -*")
"format-<nl>:
-*")

(defformat-test format-nl3 ("format-<nl>:~:
	-*")
"format-<nl>:	-*")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FORMAT T
(defformat-test format-tab1 ("1234567890123456789012345678901234567890
~10,4txx~10,4ty~10,4tzzz~10,4twwww~10,4tvvvvv~10,4tend")
"1234567890123456789012345678901234567890
          xx  y   zzz wwww    vvvvv   end")

(defformat-test format-tab2 ("123456789012345678901234567890
~3,4@txx~3,4@ty~3,4@tzzz~3,4@tend")
"123456789012345678901234567890
    xx      y   zzz     end")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FORMAT *
(defformat-test format-ignore1 ("-~a-~a-~a-~a-" 1 2 3 4 5 6 7 8 9)
"-1-2-3-4-")

(defformat-test format-ignore2 ("-~a-~a-~*~a-~a-" 1 2 3 4 5 6 7 8 9)
"-1-2-4-5-")

(defformat-test format-ignore3 ("-~a-~a-~3*~a-~a-" 1 2 3 4 5 6 7 8 9)
"-1-2-6-7-")

(defformat-test format-ignore4 ("-~a-~a-~:*~a-~a-" 1 2 3 4 5 6 7 8 9)
"-1-2-2-3-")

(defformat-test format-ignore5 ("-~a-~a-~2:*~a-~a-" 1 2 3 4 5 6 7 8 9)
"-1-2-1-2-")

(defformat-test format-ignore6 ("-~a-~a-~@*~a-~a-" 1 2 3 4 5 6 7 8 9)
"-1-2-1-2-")

(defformat-test format-ignore7 ("-~a-~a-~6@*~a-~a-" 1 2 3 4 5 6 7 8 9)
"-1-2-7-8-")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FORMAT ?
(defformat-test format-indirect1 ("format-?:***~a***~?***~a***-*" 1 "+++~s+++~s+++" (QUOTE
(A B)) 2)
"format-?:***1***+++A+++B+++***2***-*")

(defformat-test format-indirect2 ("format-?:***~a***~?***~a***-*" 1 "+++++++++++++" NIL 2)
"format-?:***1***+++++++++++++***2***-*")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FORMAT _ !!!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FORMAT W !!!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FORMAT I !!!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FORMAT ()
(defformat-test format-case1 ("~(AAAAAAAA BBBBBB ccccccc dddddddd~)")
"aaaaaaaa bbbbbb ccccccc dddddddd")

(defformat-test format-case2 ("~:(AAAAAAAA BBBBBB ccccccc dddddddd~)")
"Aaaaaaaa Bbbbbb Ccccccc Dddddddd")

(defformat-test format-case3 ("~@(AAAAAAAA BBBBBB ccccccc dddddddd~)")
"Aaaaaaaa bbbbbb ccccccc dddddddd")

(defformat-test format-case4 ("~:@(AAAAAAAA BBBBBB ccccccc dddddddd~)")
  "AAAAAAAA BBBBBB CCCCCCC DDDDDDDD")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FORMAT []
(defformat-test format-conditional1 ("~[aa~;bb~;cc~]" 1)
"bb")

(defformat-test format-conditional2 ("~[aa~;bb~;cc~]" 10)
"")

(defformat-test format-conditional3 ("~2[aa~;bb~;cc~]" 10)
"cc")

(defformat-test format-conditional4 ("~@[aaa~]" NIL 10)
"")

(defformat-test format-conditional5 ("~@[aaa~]" 20 10)
"aaa")

(defformat-test format-conditional6 ("~@[aaa~d~]" NIL 10)
"")

(defformat-test format-conditional7 ("~@[aaa~d~]" 20 10)
"aaa20")

(defformat-test format-conditional8 ("~@[aaa~d~]bbb~d" NIL 10 30)
"bbb10")

(defformat-test format-conditional9 ("~@[aaa~d~]bbb~d" 20 10 30)
"aaa20bbb10")

(defformat-test format-conditional10 ("~:[-nil-~;-true-~d~]-end~d" NIL 10 20)
"-nil--end10")

(defformat-test format-conditional11 ("~:[-nil-~;-true-~d~]-end~d" T 10 20)
"-true-10-end20")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FORMAT { }
(defformat-test format-iterate1 ("++~{-=~s=-~}++" (QUOTE (1 2 3)))
"++-=1=--=2=--=3=-++")

(defformat-test format-iterate2 ("++~2{-=~s=-~}++" (QUOTE (1 2 3)))
"++-=1=--=2=-++")

(defformat-test format-iterate3 ("++~@{-=~s=-~}++" 1 2 3)
"++-=1=--=2=--=3=-++")

(defformat-test format-iterate4 ("++~:{-=~s=~s=-~}++" (QUOTE ((1 2) (3 4 5) (6 7))))
"++-=1=2=--=3=4=--=6=7=-++")

(defformat-test format-iterate5 ("++~:@{-=~s=~s=-~}++" (QUOTE (1 2)) (QUOTE (3 4 5)) (QUOTE
(6 7)))
"++-=1=2=--=3=4=--=6=7=-++")

(defformat-test format-iterate8 ("~1{~:}" "-~s-" (QUOTE (1 2)) 3)
"-1-")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FORMAT JUSTIFY
(defformat-test format-justify1 ("~10<foo~;bar~>")
"foo    bar")

(defformat-test format-justify2 ("~10:<foo~;bar~>")
"  foo  bar")

(defformat-test format-justify3 ("~10@<foo~;bar~>")
"foo  bar  ")

(defformat-test format-justify4 ("~10:@<foo~;bar~>")
  "  foo bar ")

(defformat-test format-justify5 ("~10<foobar~>")
"    foobar")

(defformat-test format-justify6 ("~10:<foobar~>")
"    foobar")

(defformat-test format-justify7 ("~10@<foobar~>")
"foobar    ")

(defformat-test format-justify8 ("~10:@<foobar~>")
"  foobar  ")

(defformat-test format-justify9 ("~15<~S~>" 'foo)
"            FOO")

(defformat-test format-justify10 ("~15<~S~;~^~S~>" 'foo)
"            FOO")

(defformat-test format-justify11 ("~15<~S~;~^~S~;~^~S~>" 'foo)
"            FOO")

(defformat-test format-justify12 ("~15<~S~;~^~S~>" 'foo 'bar)
"FOO         BAR")

(defformat-test format-justify13 ("~15<~S~;~^~S~;~^~S~>" 'foo 'bar)
"FOO         BAR")

(defformat-test format-justify14 ("~15<~S~;~^~S~;~^~S~>" 'foo 'bar 'baz)
"FOO   BAR   BAZ")

(defformat-test format-justify15
("~%;; ~<~%;; ~1:; ~s~; ~s~; ~s~; ~s~; ~s~; ~s~; ~s~; ~s~; ~s~; ~s~; ~
 ~s~; ~s~; ~s~; ~s~; ~s~; ~s~; ~s~; ~s~; ~s~; ~s~; ~
 ~s~; ~s~; ~s~; ~s~;~>~%"                          
'aaaaaaa 'bbbbbb 'cccccccccccc 'dddddddddddddd 'eeee 'fffffffff 'gggggggg
'hhhhh 'iiii 'j 'kk 'lll 'mmmm 'nnnnnn 'oooooooooo 'ppppppppppppppp
'qqqqqqq
'rrrrrrrrrrrr 's 'ttt 'uuuuuuuuu 'vvvvvvv 'wwwwwwwwww 'xxxxx 'yyyyyy
'zzzzzzzz)
"
;; 
;;  AAAAAAA BBBBBB CCCCCCCCCCCC DDDDDDDDDDDDDD EEEE FFFFFFFFF GGGGGGGG HHHHH IIII J KK LLL MMMM NNNNNN OOOOOOOOOO PPPPPPPPPPPPPPP QQQQQQQ RRRRRRRRRRRR S TTT UUUUUUUUU VVVVVVV WWWWWWWWWW XXXXX
"
)
