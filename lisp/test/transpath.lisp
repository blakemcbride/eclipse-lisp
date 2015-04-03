;;; Tests of internal function used by translate-pathname.  Includes
;;; tests with complex wildcards. 

;; Non-wild TO: 
(deftest atom-atom-atom   (eclipse::translate-item :newest :newest 9) 	9)
(deftest atom-atom-string (eclipse::translate-item :unspecific :unspecific "abc") "abc")
(deftest string-string-atom (eclipse::translate-item "abc" "abc"   9) 	9)

;; Replacing wild:
(deftest atom-atom-wild (eclipse::translate-item 9 9     :wild) 	9)
(deftest atom-atom-nil  (eclipse::translate-item 9 9     nil)	9)
(deftest atom-wild-wild (eclipse::translate-item 9 :wild :wild)	9)
(deftest atom-wild-nil  (eclipse::translate-item 9 :wild nil) 	9)
(deftest atom-nil-wild  (eclipse::translate-item 9 nil   :wild)	9)
(deftest atom-nil-nil   (eclipse::translate-item 9 nil   nil)	9)

(deftest string-atom-wild (eclipse::translate-item "abc" "abc" :wild) 	"abc")
(deftest string-atom-nil  (eclipse::translate-item "abc" "abc" nil)	"abc")
(deftest string-wild-wild (eclipse::translate-item "abc" :wild :wild)	"abc")
(deftest string-wild-nil  (eclipse::translate-item "abc" :wild nil) 	"abc")
(deftest string-nil-wild  (eclipse::translate-item "abc" nil   :wild)	"abc")
(deftest string-nil-nil   (eclipse::translate-item "abc" nil   nil)	"abc")

(deftest wild-wild-nil  (eclipse::translate-item :wild :wild nil)	:wild)
(deftest wild-wild-wild (eclipse::translate-item :wild :wild :wild)	:wild)
(deftest wild-nil-nil   (eclipse::translate-item :wild nil   nil)	:wild)
(deftest wild-nil-wild  (eclipse::translate-item :wild nil   :wild)	:wild)

;; Replacing complex strings:
(deftest single-replace (eclipse::translate-item "ABCDE" "A?C?E" "xx?yy?zz") "xxByyDzz")
(deftest single-replace2 (eclipse::translate-item "A?C*E" "A?C?E" "xx?yy?zz") "xx?yy*zz")
(deftest multiple-replace (eclipse::translate-item "ABCDEFGHI" "A?C*D*F*I*" "zz*yy*xx*ww*vv*uu*tt")
  "zzByyxxEwwGHvvuutt")
(deftest from-gets-remainder (eclipse::translate-item "ABCDE" "A?C*" "xx*yy*zz") "xxByyDEzz")
(deftest to-gets-remainder (eclipse::translate-item "ABCDE" "A?CDE" "xx*yy*zz") "xxByyCDEzz")

(deftest complex-wild-from (eclipse::translate-item "AB"  "*"   "xx*yy") "xxAByy")
(deftest wild-from-complex (eclipse::translate-item "AB"  :wild "xx*yy") "xxAByy")
(deftest nil-from-complex (eclipse::translate-item "AB"  nil   "xx*yy") "xxAByy")
(deftest wild-source-complex (eclipse::translate-item :wild :wild "xx*yy") "xx*yy")
(deftest nil-source-complex (eclipse::translate-item nil   nil   "xx*yy") "xx*yy")

;; Replacing simple lists:
(deftest single-list-replace
    (eclipse::translate-item '(a b c d e) '(a :wild c :wild e) '(x 1 :wild y 2 :wild z 3))
  (x 1 B y 2 D z 3))
(deftest multiple-list-replace
    (eclipse::translate-item '(a b c d e f g h i)
		    '(a :wild c :wild-inferiors d :wild-inferiors f :wild-inferiors i :wild-inferiors)
		    '(z 1 :wild-inferiors y 2 :wild-inferiors x 3 :wild-inferiors w 4 :wild-inferiors
		      v 5 :wild-inferiors u 6 :wild-inferiors t 7))
  (z 1 B y 2 x 3 E w 4 G H v 5 u 6 t 7))

(deftest wild-source-list
    (eclipse::translate-item '(a b :wild) '(a :wild :wild) '(x 1 :wild :wild))
  (x 1 B :wild))

;; Replacing complex lists:
(deftest single-complex-list
    (eclipse::translate-item '("a" "b1" "c" "d2" "e") '("a" "b?" "c" "d?" "e") '(x 1 :wild y 2 :wild z 3))
  (x 1 "1" y 2 "2" z 3))
(deftest multi-complex-list
    (eclipse::translate-item '("a" "b12c" "c" "d23e" "e") '("a" "b*c" "c" "d*e" "e") '(x 1 :wild y 2 :wild z 3))
  (x 1 "12" y 2 "23" z 3))
(deftest single-complex-complex
    (eclipse::translate-item '("a" "b1" "c" "d2" "e") '("a" "b?" "c" "d?" "e") '(x 1 "q?" y 2 "r?" z 3))
  (x 1 "q1" y 2 "r2" z 3))
(deftest multi-complex-complex
    (eclipse::translate-item '("a" "b12c" "c" "d23e" "e") '("a" "b*c" "c" "d*e" "e") '(x 1 :wild y 2 :wild z 3))
  (x 1 "12" y 2 "23" z 3))

(deftest not-inferiors
    (eclipse::translate-item '("abc" "def" "ghi" "jkl" "mno" "pqr")
		    '("a?c" :wild-inferiors "def" :wild-inferiors "m*" :wild-inferiors)
		    '("xxx" :wild "yyy" :wild-inferiors :wild-inferiors :wild :wild-inferiors))
  ("xxx" "b" "yyy" "ghi" "jkl" "no" "pqr"))

(deftest inferiors
    (eclipse::translate-item '("abc" "def" "ghi" "jkl" "mno" "pqr")
		    '("a?c" :wild-inferiors "def" :wild-inferiors "m*" :wild-inferiors)
		    '("xxx" :wild-inferiors "yyy" :wild-inferiors :wild-inferiors
		      :wild-inferiors :wild-inferiors))
  ("xxx" "b" "yyy" "ghi" "jkl" "no" "pqr"))


(deftest inferiors2
    (eclipse::translate-item '("jkl" "mno" "pqr" "m")
		    '(:wild-inferiors "m*")
		    '("xxx" :wild-inferiors "yyy" :wild-inferiors))
  ("xxx" "jkl" "mno" "pqr" "yyy" ""))


;; another look at cross types:
(deftest cross1 (eclipse::translate-item '("x" "abc") '("x" :wild) '(:wild)) ("abc"))
(deftest cross2 (eclipse::translate-item "abc" "a?c" "xx*") "xxb")
(deftest cross3 (eclipse::translate-item "abc" "a*c" "xx*") "xxb")
(deftest cross4 (eclipse::translate-item "ac"  "a*c" "xx*") "xx")
(deftest cross5 (eclipse::translate-item "abcc" "a*c" "xx*") "xxbc")

(deftest cross6 (eclipse::translate-item '("x" "abc") '("x" "a?c") '(:wild)) ("b"))
(deftest cross7 (eclipse::translate-item '("x" "abc") '("x" "a*c") '(:wild)) ("b"))
(deftest cross8 (eclipse::translate-item '("x" "ac") '("x" "a*c") '(:wild)) (""))
(deftest cross9 (eclipse::translate-item '("x" "abcc") '("x" "a*c") '(:wild)) ("bc"))
(deftest cross10 (eclipse::translate-item '("x" "abc") '("x" "a?c") '(:wild-inferiors)) ("b"))
(deftest cross11 (eclipse::translate-item '("x" "abc") '("x" "a*c") '(:wild-inferiors)) ("b"))
(deftest cross12 (eclipse::translate-item '("x" "ac") '("x" "a*c") '(:wild-inferiors)) (""))
(deftest cross13 (eclipse::translate-item '("x" "abcc") '("x" "a*c") '(:wild-inferiors)) ("bc"))

(deftest cross14 (eclipse::translate-item '("x" "abc") '("x" :wild) '(:wild-inferiors)) ("abc"))
(deftest cross15 (eclipse::translate-item '("x" "abc") '("x" :wild-inferiors) '(:wild-inferiors)) ("abc"))
(deftest cross16 (eclipse::translate-item '("x") '("x" :wild-inferiors) '(:wild-inferiors)) ())
(deftest cross17 (eclipse::translate-item '("x" "abc" "def") '("x" :wild-inferiors) '(:wild-inferiors))
  ("abc" "def"))

;;; absolute/relative issues:
(deftest abs-rel1 (eclipse::translate-item '(:absolute "f")
				  '(:absolute :wild)
				  '(:relative :wild)) (:relative "f"))
(deftest abs-rel2 (eclipse::translate-item '(:absolute "f")
				  '(:absolute :wild-inferiors)
				  '(:relative :wild-inferiors)) (:relative "f"))
#+never-occurs
(deftest abs-rel3 (eclipse::translate-item '(:absolute "f")
				  nil
				  '(:relative :wild-inferiors)) (:relative "f"))
;; ... because translate-component changes nil into '(:absolute :wild-inferiors).


;; CL examples
(deftest translate-dir1
    (eclipse::translate-item '(:absolute "USR" "ME")
		    '(:absolute "USR" "ME")
		    '(:absolute "DEV" "HER")) (:absolute "DEV" "HER"))
(deftest translate-file1 (eclipse::translate-item "INIT" :wild :wild) "INIT")
(deftest translate-file1a (eclipse::translate-item "INIT" "*" "*") "INIT")
(deftest translate-type (eclipse::translate-item "LISP" "LISP" "L") "L")

(deftest translate-dir2
    (eclipse::translate-item '(:absolute "USR" "ME" "PCL-5-MAY")
		    '(:absolute "USR" "ME" "PCL*")
		    '(:absolute "SYS" "PCL" :wild))
  (:absolute "SYS" "PCL" "-5-MAY")) ; or ... "PCL-5-MAY"
(deftest translate-dir2a
    (eclipse::translate-item '(:absolute "USR" "ME" "PCL-5-MAY")
		    '(:absolute "USR" "ME" "PCL*")
		    '(:absolute "SYS" "PCL" "*"))
  (:absolute "SYS" "PCL" "-5-MAY"))
(deftest translate-file2 (eclipse::translate-item "LOW" :wild nil) "LOW")
(deftest translate-file2a (eclipse::translate-item "LOW" "*" nil) "LOW")

(deftest translate-dir3
    (eclipse::translate-item '(:absolute "USR" "ME")
		    '(:absolute "USR" "ME")
		    '(:absolute "USR" "ME2")) (:absolute "USR" "ME2"))
(deftest translate-file3 (eclipse::translate-item "FOO" "FOO" nil) "FOO")    
(deftest translate-type3 (eclipse::translate-item "BAR" "BAR" nil) "BAR")    

(deftest translate-file4
    (eclipse::translate-item "LAMB-RECIPES" "*-RECIPES" "JOE'S-*-REC") "JOE'S-LAMB-REC")

(deftest translate-dir5
    (eclipse::translate-item '(:absolute "USR" "DMR" "HACKS")
		    '(:absolute "USR" "D*" "HACKS")
		    '(:absolute "USR" "D*" "BACKUP" "HACKS"))
  (:absolute "USR" "DMR" "BACKUP" "HACKS"))
(deftest translate-file5 (eclipse::translate-item "FROB" :wild "BACKUP-*") "BACKUP-FROB")
(deftest translate-file5a (eclipse::translate-item "FROB" "*" "BACKUP-*") "BACKUP-FROB")
(deftest translate-type5a (eclipse::translate-item "L" "L" :wild) "L")
(deftest translate-type5 (eclipse::translate-item "L" "L" "*") "L")

(deftest translate-file6 (eclipse::translate-item "FROB" "FR*" "BACKUP-*") "BACKUP-OB")

(deftest translate-dir7
    (eclipse::translate-item '(:absolute "BAR" "BAZ")
		    '(:absolute :wild-inferiors)
		    '(:absolute "LIBRARY" "FOO" :wild-inferiors))
  (:absolute "LIBRARY" "FOO" "BAR" "BAZ"))

(deftest translate-dir8
    (eclipse::translate-item '(:absolute "MAIL" "SAVE")
		    '(:absolute "MAIL" :wild-inferiors)
		    '(:absolute "JOE" "MAIL" "PROG" :wild-inferiors))
  (:absolute "JOE" "MAIL" "PROG" "SAVE"))
(deftest translate-dir9
    (eclipse::translate-item '(:absolute "EXPERIMENTAL")
		    '(:absolute "EXPERIMENTAL")
		    '(:absolute "USR" "Joe" "DEVELOPMENT" "PROG"))
  (:absolute "USR" "Joe" "DEVELOPMENT" "PROG"))

(deftest translate-file10 (eclipse::translate-item "FOOBAR" "FOO*" "*BAZ") "BARBAZ")
(deftest translate-file11 (eclipse::translate-item "FOOBAR" "FOO*" :wild) "BAR");or "FOOBAR"
(deftest translate-file11a (eclipse::translate-item "FOOBAR" "FOO*" "*") "BAR")

(deftest translate-file12 (eclipse::translate-item "FOOBAR" :wild "FOO*") "FOOFOOBAR")
(deftest translate-file12a (eclipse::translate-item "FOOBAR" "*" "FOO*") "FOOFOOBAR")
(deftest translate-file13 (eclipse::translate-item "BAR" :wild "FOO*") "FOOBAR")
(deftest translate-file13a (eclipse::translate-item "BAR" "*" "FOO*") "FOOBAR")
(deftest translate-file14 (eclipse::translate-item "FOOBAR" "FOO*" "BAZ*") "BAZBAR")

#| ;We should confirm that the following fail!
(eclipse::translate-item 9 :newest 9)	;unmatched source
(eclipse::translate-item 9 :newest nil)	;unmatched source
(eclipse::translate-item "abc" "abc?" "xyz") ;unmatched source
(eclipse::translate-item "ABC" "a?C" "x?y") ;unmatched source
(eclipse::translate-item "ABC" "A?C" "x?y?z") ;missing wilcard
(eclipse::translate-item "ABC" "A*C" "x?y") ;improper wildcard
(eclipse::translate-item "ABCD" "A*C?" "x*y") ; extra wildcard
(eclipse::translate-item '("AB" "CD") '(:wild-inferiors) "xx*yy") ;improper wildcard
(eclipse::translate-item '(a b) '(:wild-inferiors) '(:wild)) ;improper wildcard
|#