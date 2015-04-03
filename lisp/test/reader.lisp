(defmacro preserving-package (&body body)
  `(let ((*package* (cl:find-package ,(cl:package-name *package*))))
     ,@body))

(defun test-reader (string
		    &optional (base 10)
			      (format *read-default-float-format*))
  (values (preserving-package
	   (let ((*read-base* base)
		 (*read-default-float-format* format)
		 #+eclipse (eclipse::*remove-bq-tokens* t))
	     (read-from-string string)))))

(defmacro defread-test (name string value)
  `(deftest ,name (test-reader ,string) ,value))

(defmacro defweak-read-test (name string value)
  `(deftest ,name (equalp (test-reader ,string) ,value) t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 22.1 PRINTED REPRESENTATION OF LISP OBJECTS                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 22.1.2 PARSING OF NUMBERS AND SYMBOLS

(defread-test reader-symbol "foo" foo)
(defread-test reader-user-symbol "user::foo" user::foo)
(defread-test reader-keyword ":foo" :foo)
(defread-test reader-div "/" /)
(defread-test reader-add "+" +)
(defread-test reader-subt "-" -)
(defread-test reader-subt1 "1-" 1-)
(defread-test reader-add1 "1+" 1+)
(defread-test reader-slash5 "/5" /5)
(defread-test reader-foo+ "foo+" foo+)
(defread-test reader-caret "^" ^)
(defread-test reader-underscore "_" |_|)
(defread-test reader-caret/dash "^/-" |^/-|)
(defread-test reader-symbol1 "\\256" |\256|)
(defread-test reader-symbol2 "25\\64" |2564|)
(defread-test reader-symbol3 "1.0\\E6" |1.0\E6|)
(defread-test reader-symbol4 "|100|" |100|)
(defread-test reader-symbol5 "3\\.14159" |3\.14159|)
(defread-test reader-symbol6 "|3/4|" |3/4|)
(defread-test reader-symbol7 "3\\/4" |3\/4|)
(defread-test reader-symbol8 "5||" |5|)

(defread-test reader-integer "123" 123)
(deftest reader-integer-base-16
    (test-reader "face" 16) 64206)
(deftest reader-integer-base-16-16
    (test-reader "a" 16) 10)
(deftest reader-integer-base-16-16-2
    (test-reader "10" 16) 16)
(deftest reader-integer-base-16-10
    (test-reader "10." 16) 10)
(defread-test reader-pos-integer "+123" 123)
(deftest reader-pos-integer-base-16
    (test-reader "+face" 16) 64206)
(deftest reader-pos-integer-base-16-16
    (test-reader "+a" 16) 10)
(deftest reader-pos-integer-base-16-16-2
    (test-reader "+10" 16) 16)
(deftest reader-pos-integer-base-16-10
    (test-reader "+10." 16) 10)
(defread-test reader-neg-integer "-123" -123)
(deftest reader-neg-integer-base-16
    (test-reader "-face" 16) -64206)
(deftest reader-neg-integer-base-16-16
    (test-reader "-a" 16) -10)
(deftest reader-neg-integer-base-16-16-2
    (test-reader "-10" 16) -16)
(deftest reader-neg-integer-base-16-10
    (test-reader "-10." 16) -10)
(deftest reader-integer-base-16-symbol
    (test-reader "a." 16) |A.|)
(deftest reader-integer-base-16-fake-float
    (test-reader "1f0" 16) 496)
(deftest reader-integer-base-8-8-dot
    (test-reader "8." 8) 8)
(deftest reader-integer-base-8-8
    (test-reader "8" 8) |8|)

(defread-test reader-ratio "123/456" 41/152)
(deftest reader-ratio-base (test-reader "a/b" 16) 10/11)
(defread-test reader-pos-ratio "+123/456" 41/152)
(deftest reader-pos-ratio-base (test-reader "+a/b" 16) 10/11)
(defread-test reader-neg-ratio "-123/456" -41/152)
(deftest reader-neg-ratio-base (test-reader "-a/b" 16) -10/11)

(defread-test reader-leading-dec ".12" 0.12f0)
(defread-test reader-pos-leading-dec "+.12" 0.12f0)
(defread-test reader-neg-leading-dec "-.12" -0.12f0)

(defread-test reader-float-default "1.23" 1.23f0)
(defread-test reader-float-single "1.23f0" 1.23f0)
(defread-test reader-float-double "1.23d0" 1.23d0)
(deftest reader-float-default-double
    (test-reader "1.23e0" 10 'double-float) 1.23d0)
(deftest reader-float-default-single
    (test-reader "1.23e0" 10 'single-float) 1.23f0)
(defread-test reader-float-single-no-dec "123f0" 1.23f2)
(defread-test reader-float-double-no-dec "123d0" 1.23d2)
(deftest reader-float-default-double-no-dec
    (test-reader "123e0" 10 'double-float) 1.23d2)
(deftest reader-float-default-single-no-dec
    (test-reader "123e0" 10 'single-float) 1.23f2)
(defread-test reader-float-single-empty-dec "123.f0" 1.23f2)
(defread-test reader-float-double-empty-dec "123.d0" 1.23d2)
(deftest reader-float-default-double-empty-dec
    (test-reader "123.e0" 10 'double-float) 1.23d2)
(deftest reader-float-default-single-empty-dec
    (test-reader "123.e0" 10 'single-float) 1.23f2)

(defread-test reader-exp-single "1.23f3" 1.23f3)
(defread-test reader-exp-double "1.23d3" 1.23d3)
(deftest reader-exp-default-double
    (test-reader "1.23e3" 10 'double-float) 1.23d3)
(deftest reader-exp-default-single
    (test-reader "1.23e3" 10 'single-float) 1.23f3)
(defread-test reader-exp-single-no-dec "123f3" 1.23f5)
(defread-test reader-exp-double-no-dec "123d3" 1.23d5)
(deftest reader-exp-default-double-no-dec
    (test-reader "123e3" 10 'double-float) 1.23d5)
(deftest reader-exp-default-single-no-dec
    (test-reader "123e3" 10 'single-float) 1.23f5)

(defread-test reader-exp-pos-single "1.23f+3" 1.23f3)
(defread-test reader-exp-pos-double "1.23d+3" 1.23d3)
(deftest reader-exp-pos-default-double
    (test-reader "1.23e+3" 10 'double-float) 1.23d3)
(deftest reader-exp-pos-default-single
    (test-reader "1.23e+3" 10 'single-float) 1.23f3)
(defread-test reader-exp-pos-single-no-dec "123f+3" 1.23f5)
(defread-test reader-exp-pos-double-no-dec "123d+3" 1.23d5)
(deftest reader-exp-pos-default-double-no-dec
    (test-reader "123e+3" 10 'double-float) 1.23d5)
(deftest reader-exp-pos-default-single-no-dec
    (test-reader "123e+3" 10 'single-float) 1.23f5)

(defread-test reader-exp-neg-single "1.23f-3" 1.23f-3)
(defread-test reader-exp-neg-double "1.23d-3" 1.23d-3)
(deftest reader-exp-neg-default-double
    (test-reader "1.23e-3" 10 'double-float) 1.23d-3)
(deftest reader-exp-neg-default-single
    (test-reader "1.23e-3" 10 'single-float) 1.23f-3)
(defread-test reader-exp-neg-single-no-dec "123f-3" 1.23f-1)
(defread-test reader-exp-neg-double-no-dec "123d-3" 1.23d-1)
(deftest reader-exp-neg-default-double-no-dec
    (test-reader "123e-3" 10 'double-float) 1.23d-1)
(deftest reader-exp-neg-default-single-no-dec
    (test-reader "123e-3" 10 'single-float) 1.23f-1)


(defread-test reader-pos-float-default "+1.23" 1.23f0)
(defread-test reader-pos-float-single "+1.23f0" 1.23f0)
(defread-test reader-pos-float-double "+1.23d0" 1.23d0)
(deftest reader-pos-float-default-double
    (test-reader "+1.23e0" 10 'double-float) 1.23d0)
(deftest reader-pos-float-default-single
    (test-reader "+1.23e0" 10 'single-float) 1.23f0)
(defread-test reader-pos-float-single-no-dec "+123f0" 1.23f2)
(defread-test reader-pos-float-double-no-dec "+123d0" 1.23d2)
(deftest reader-pos-float-default-double-no-dec
    (test-reader "+123e0" 10 'double-float) 1.23d2)
(deftest reader-pos-float-default-single-no-dec
    (test-reader "+123e0" 10 'single-float) 1.23f2)

(defread-test reader-pos-exp-single "+1.23f3" 1.23f3)
(defread-test reader-pos-exp-double "+1.23d3" 1.23d3)
(deftest reader-pos-exp-default-double
    (test-reader "+1.23e3" 10 'double-float) 1.23d3)
(deftest reader-pos-exp-default-single
    (test-reader "+1.23e3" 10 'single-float) 1.23f3)
(defread-test reader-pos-exp-single-no-dec "+123f3" 1.23f5)
(defread-test reader-pos-exp-double-no-dec "+123d3" 1.23d5)
(deftest reader-pos-exp-default-double-no-dec
    (test-reader "+123e3" 10 'double-float) 1.23d5)
(deftest reader-pos-exp-default-single-no-dec
    (test-reader "+123e3" 10 'single-float) 1.23f5)

(defread-test reader-pos-exp-pos-single "+1.23f+3" 1.23f3)
(defread-test reader-pos-exp-pos-double "+1.23d+3" 1.23d3)
(deftest reader-pos-exp-pos-default-double
    (test-reader "+1.23e+3" 10 'double-float) 1.23d3)
(deftest reader-pos-exp-pos-default-single
    (test-reader "+1.23e+3" 10 'single-float) 1.23f3)
(defread-test reader-pos-exp-pos-single-no-dec "+123f+3" 1.23f5)
(defread-test reader-pos-exp-pos-double-no-dec "+123d+3" 1.23d5)
(deftest reader-pos-exp-pos-default-double-no-dec
    (test-reader "+123e+3" 10 'double-float) 1.23d5)
(deftest reader-pos-exp-pos-default-single-no-dec
    (test-reader "+123e+3" 10 'single-float) 1.23f5)

(defread-test reader-pos-exp-neg-single "+1.23f-3" 1.23f-3)
(defread-test reader-pos-exp-neg-double "+1.23d-3" 1.23d-3)
(deftest reader-pos-exp-neg-default-double
    (test-reader "+1.23e-3" 10 'double-float) 1.23d-3)
(deftest reader-pos-exp-neg-default-single
    (test-reader "+1.23e-3" 10 'single-float) 1.23f-3)
(defread-test reader-pos-exp-neg-single-no-dec "+123f-3" 1.23f-1)
(defread-test reader-pos-exp-neg-double-no-dec "+123d-3" 1.23d-1)
(deftest reader-pos-exp-neg-default-double-no-dec
    (test-reader "+123e-3" 10 'double-float) 1.23d-1)
(deftest reader-pos-exp-neg-default-single-no-dec
    (test-reader "+123e-3" 10 'single-float) 1.23f-1)

(defread-test reader-neg-float-default "-1.23" -1.23f0)
(defread-test reader-neg-float-single "-1.23f0" -1.23f0)
(defread-test reader-neg-float-double "-1.23d0" -1.23d0)
(deftest reader-neg-float-default-double
    (test-reader "-1.23e0" 10 'double-float) -1.23d0)
(deftest reader-neg-float-default-single
    (test-reader "-1.23e0" 10 'single-float) -1.23f0)
(defread-test reader-neg-float-single-no-dec "-123f0" -1.23f2)
(defread-test reader-neg-float-double-no-dec "-123d0" -1.23d2)
(deftest reader-neg-float-default-double-no-dec
    (test-reader "-123e0" 10 'double-float) -1.23d2)
(deftest reader-neg-float-default-single-no-dec
    (test-reader "-123e0" 10 'single-float) -1.23f2)

(defread-test reader-neg-exp-single "-1.23f3" -1.23f3)
(defread-test reader-neg-exp-double "-1.23d3" -1.23d3)
(deftest reader-neg-exp-default-double
    (test-reader "-1.23e3" 10 'double-float) -1.23d3)
(deftest reader-neg-exp-default-single
    (test-reader "-1.23e3" 10 'single-float) -1.23f3)
(defread-test reader-neg-exp-single-no-dec "-123f3" -1.23f5)
(defread-test reader-neg-exp-double-no-dec "-123d3" -1.23d5)
(deftest reader-neg-exp-default-double-no-dec
    (test-reader "-123e3" 10 'double-float) -1.23d5)
(deftest reader-neg-exp-default-single-no-dec
    (test-reader "-123e3" 10 'single-float) -1.23f5)

(defread-test reader-neg-exp-pos-single "-1.23f+3" -1.23f3)
(defread-test reader-neg-exp-pos-double "-1.23d+3" -1.23d3)
(deftest reader-neg-exp-pos-default-double
    (test-reader "-1.23e+3" 10 'double-float) -1.23d3)
(deftest reader-neg-exp-pos-default-single
    (test-reader "-1.23e+3" 10 'single-float) -1.23f3)
(defread-test reader-neg-exp-pos-single-no-dec "-123f+3" -1.23f5)
(defread-test reader-neg-exp-pos-double-no-dec "-123d+3" -1.23d5)
(deftest reader-neg-exp-pos-default-double-no-dec
    (test-reader "-123e+3" 10 'double-float) -1.23d5)
(deftest reader-neg-exp-pos-default-single-no-dec
    (test-reader "-123e+3" 10 'single-float) -1.23f5)

(defread-test reader-neg-exp-neg-single "-1.23f-3" -1.23f-3)
(defread-test reader-neg-exp-neg-double "-1.23d-3" -1.23d-3)
(deftest reader-neg-exp-neg-default-double
    (test-reader "-1.23e-3" 10 'double-float) -1.23d-3)
(deftest reader-neg-exp-neg-default-single
    (test-reader "-1.23e-3" 10 'single-float) -1.23f-3)
(defread-test reader-neg-exp-neg-single-no-dec "-123f-3" -1.23f-1)
(defread-test reader-neg-exp-neg-double-no-dec "-123d-3" -1.23d-1)
(deftest reader-neg-exp-neg-default-double-no-dec
    (test-reader "-123e-3" 10 'double-float) -1.23d-1)
(deftest reader-neg-exp-neg-default-single-no-dec
    (test-reader "-123e-3" 10 'single-float) -1.23f-1)

;;; *READ-BASE* See above.
;;; *READ-SUPPRESS*
(deftest read-suppress
    (let ((*read-suppress* t))
      (mapcar #'read-from-string
	      '("#(foo bar baz)"
		"#P(:type :lisp)"
		"#c1.2"
		"#.(PRINT 'FOO)"
		"#3AHELLO"
		"#S(INTEGER)"
		"#*ABC"
		"#\\GARBAGE"
		"#RALPHA"
		"#3R444")))
  (nil nil nil nil nil nil nil nil nil nil))
	       
	       
;;; *READ-EVAL*
(deftest read-eval-suppress
    (let ((*read-eval* nil)
	  (*read-suppress* t))
      (read-from-string "#.(error 'FOO)"))
  nil 14)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 22.1.3 MACRO CHARACTERS

(defread-test reader-empty-list "()" nil)
(defread-test reader-empty-list2 "( )" nil)
(defread-test reader-list "(1 2 3)" (1 2 3))
(defread-test reader-list2 "( 1 2 3 )" (1 2 3))
(defread-test reader-nested-list "(1 (2 3 (4 (5 (6 ())))))"
  (1 (2 3 (4 (5 (6 nil))))))
(defread-test dot "( 1 2 . 3 )" (1 2 . 3))
(defread-test dot-with-float "(.1 .2 . .3)" (0.1f0 0.2f0 . 0.3f0))
(deftest close-paren (gives-error (test-reader ")")))

(defread-test quote-symbol "'foo" (quote foo))
(defread-test quote-list "'(foo bar baz)" (quote (foo bar baz)))

(defread-test line-comment ";foo
1" 1)

(defread-test reader-string "\"a string\"" "a string")
(defread-test escaped-string "\"a st\\ring\"" "a string")
(defread-test multi-escaped-string "\"a |st\\ring|\"" "a |string|")

(when (fboundp 'eval)
  (let ((xeval (symbol-function 'eval)))
    (declare (notinline eval))
    (deftest backquote
      (let ((b 3))
	(declare (special b))
	(funcall xeval (test-reader "`(a b ,b ,(+ b 1) b)")))
      (a b 3 4 b))
    (deftest spliced-backquote
      (let ((x '(a b c)))
	(declare (special x))
	(funcall xeval (test-reader
		       "`(x ,x ,@x foo ,(cadr x) bar ,(cdr x) baz ,@(cdr x))")))
      (x (a b c) a b c foo b bar (b c) baz b c))
    (deftest spliced-backquote2
      (let ((x '(a b c)))
	(declare (special x))
	(funcall xeval (test-reader
		       "`(x ,x ,@x foo ,(cadr x) bar ,(cdr x) baz ,.(cdr x))")))
      (x (a b c) a b c foo b bar (b c) baz b c))))
;; The result here are the results Steele produces.  Other Lisps might
;; produce any other results which when evaluted twice give the same
;; results. 
(deftest sbackquote (test-reader "``(,,q)") (list 'list q))
(deftest sbackquote2 (test-reader "``(,@,q)") q)
(deftest sbackquote3 (test-reader "``(,,@q)") (cons 'list q))

(deftest sbackquote4 (test-reader "``(,@,@q)") (cons 'append q))
(deftest sbackquote2a (test-reader "``(,.,q)") q)
(deftest sbackquote3a (test-reader "``(,,.q)") (cons 'list q))

(progn
  (deftest sbackquote4a (test-reader "``(,@,.q)") (cons 'append q))
  (deftest sbackquote4b (test-reader "``(,.,@q)") (cons 'nconc q ))
  (deftest sbackquote4c (test-reader "``(,.,.q)") (cons 'nconc q )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 22.1.4 STANDARD DISPATCHING MACRO CHARACTER SYNTAX

(defread-test reader-lower-char "#\\a" #\a)
(defread-test reader-upper-char "#\\A" #\A)
(defread-test reader-lower-char2 "#\\a " #\a)
(defread-test reader-upper-char2 "#\\A " #\A)
(defread-test reader-named-char "#\\space" #\space)
(defread-test reader-named-char2 "#\\space " #\space)
(defread-test reader-named-char3 "#\\SPACE " #\space)
;;; This should work because reading a name like "SP\\ACE" should
;;; produce the symbol SPACE.
(defread-test reader-named-char4 "#\\SP\\ACE " #\space)
(deftest reader-newline (char-int (read-from-string "#\\newline")) 10)
(deftest reader-meta-null (char-int (read-from-string "#\\meta-null")) 128)

;;; Expected names for ASCII users

(progn
  (deftest reader-control-char-nul (read-from-string "#\\control-@") #\null 11)
  (deftest reader-control-char-esc (read-from-string "#\\control-[") #\escape 11)
  (deftest reader-control-char-bel (read-from-string "#\\control-g") #\bell 11)
  (deftest reader-control-char-bs (read-from-string "#\\control-H") #\backspace 11)
  (deftest reader-control-char-nl (read-from-string "#\\control-j") #\linefeed 11)
  (deftest reader-control-char-cr (read-from-string "#\\control-M") #\return 11)
;;; Assuming ASCII
  (deftest reader-control-char-a
    (char-int (read-from-string "#\\CONTROL-A")) 1)
  (deftest reader-control-char-a-case
    (char-int (read-from-string "#\\control-a")) 1)
;;; Assuming meta turns on the high order bit:
  (deftest reader-meta-char-a
    (char-int (read-from-string "#\\META-A")) 193)
  (deftest reader-meta-char-a-case
    (char-int (read-from-string "#\\meta-a")) 225)
  (deftest reader-meta-char-rubout
    (char-int (read-from-string "#\\meta-rubout")) 255)
  (deftest reader-control-meta-char-a
    (char-int (read-from-string "#\\CONTROL-META-A")) 129)
  (deftest reader-control-meta-char-a-case
    (char-int (read-from-string "#\\control-meta-a")) 129)
  (deftest reader-control-meta-char-rubout
    (char-int (read-from-string "#\\control-meta-_")) 159)

;;; Alternate bit qualifiers
  (deftest reader-control-char-a-alternate
    (char-int (read-from-string "#\\^A")) 1)
  (deftest reader-control-char-alternate-case
    (char-int (read-from-string "#\\^a")) 1)
  (deftest reader-control-char-underscore
    (char-int (read-from-string "#\\^_")) 31)
  (deftest reader-meta-char-a-alternate
    (char-int (read-from-string "#\\%A")) 193)
  (deftest reader-meta-char-a-case-alternate
    (char-int (read-from-string "#\\%a")) 225)
  (deftest reader-meta-char-rbout-alternate
    (char-int (read-from-string "#\\%rubout")) 255)
  (deftest reader-control-meta-char-a-alternate
    (char-int (read-from-string "#\\%^A")) 129)
  (deftest reader-control-meta-char-a-case-alternate
    (char-int (read-from-string "#\\%^a")) 129)
  (deftest reader-control-meta-char-rubout-alternate
    (char-int (read-from-string "#\\%^_")) 159))

(defread-test reader-function "#'list" (function list))

(defweak-read-test reader-vector "#(1 2 3)" #(1 2 3))
(defweak-read-test reader-vector2 "#3(1 2 3)" #(1 2 3))
(defweak-read-test reader-vector3 "#5(1 2 3)" #(1 2 3 3 3))
(defweak-read-test reader-vector4 "#()" #())
(defweak-read-test reader-vector5 "#(a b c)" #(a b c))

(defread-test bit-vector1 "#*101111" #*101111)
(defread-test bit-vector2 "#6*101111" #*101111)
(defread-test bit-vector3 "#6*101" #*101111)
(defread-test bit-vector4 "#6*1011" #*101111)
(defread-test bit-vector5 "#6*1010" #*101000)
(defread-test bit-vector6 "#*" #*)
(defread-test bit-vector7 "#0*" #*)
(defread-test bit-vector6a "(#*)" (#*))
(defread-test bit-vector7a "(#0*)" (#*))

(deftest reader-uninterned
    (not (string= (symbol-name (test-reader "#:foo"))
		  (symbol-name '#:foo))) nil)

(defread-test reader-binary "#B1101" 13)
(defread-test reader-binary2 "#b101/11" 5/3)
(defread-test reader-binary-neg "#B-1101" -13)
(defread-test reader-binary2-neg "#b-101/11" -5/3)
(defread-test reader-binary-pos "#B+1101" 13)
(defread-test reader-binary2-pos "#b+101/11" 5/3)

(defread-test reader-octal "#o37/15" 31/13)
(defread-test reader-octal2 "#O777" 511)
(defread-test reader-octal-neg "#o-37/15" -31/13)
(defread-test reader-octal2-neg "#O-777" -511)
(defread-test reader-octal-pos "#o+37/15" 31/13)
(defread-test reader-octal2-pos "#O+777" 511)

(defread-test reader-hex "#XF00" 3840)
(defread-test reader-hex2 "#xabcd" 43981)
(defread-test reader-hex3 "#xa/b" 10/11)
(defread-test reader-hex-neg "#X-F00" -3840)
(defread-test reader-hex2-neg "#x-ABCD" -43981)
(defread-test reader-hex3-neg "#x-a/b" -10/11)
(defread-test reader-hex-pos "#X+f00" 3840)
(defread-test reader-hex2-pos "#x+abcd" 43981)
(defread-test reader-hex3-pos "#x+a/B" 10/11)

(defread-test reader-radix "#3r102" 11)
(defread-test reader-radix2 "#11R32" 35)
(defread-test reader-radix3 "#12R1a/b" 2)
(defread-test reader-radix-neg "#3r-102" -11)
(defread-test reader-radix2-neg "#11R-32" -35)
(defread-test reader-radix3-neg "#12R-1A/b" -2)
(defread-test reader-radix-pos "#3r+102" 11)
(defread-test reader-radix2-pos "#11R+32" 35)
(defread-test reader-radix3-pos "#12R+1a/B" 2)

(defread-test reader-complex1 "#c(3.0f1 2.0f-1)" #c(3.0f1 2.0f-1))
(defread-test reader-complex2 "#C(5 -3)" #c(5 -3))
(defread-test reader-complex3 "#c(1 7.0)" #c(1.0f0 7.0f0))
(defread-test reader-complex4 "#c(1 0)" 1)
(defread-test reader-complex5 "#C(1/2 2/3)" #c(1/2 2/3))

(defweak-read-test read-array "#2a((0 1 5) (foo 2 (hot dog)))"
  (make-array '(2 3) :initial-contents '((0 1 5) (foo 2 (hot dog)))))
(defweak-read-test read-array2
  "#1a((0 1 5) (foo 2 (hot dog)))"
  #((0 1 5) (FOO 2 (HOT DOG))))
(defweak-read-test read-array3
  "#0a((0 1 5) (foo 2 (hot dog)))"
  (make-array '() :initial-element '((0 1 5) (foo 2 (hot dog)))))

(defstruct sample-struct (a 9) b c)
(deftest read-struct
  (preserving-package
   (let ((s (read-from-string "#S(sample-struct :a 1 :b 2 :c 3)")))
     (and (sample-struct-p s)
	  (= (sample-struct-a s) 1)
	  (= (sample-struct-b s) 2)
	  (= (sample-struct-c s) 3))))
  t)
;;; Currently, ANSI says that non-keyword slot names should be coerced
;;; to the keyword names, but that this will not be supported in the
;;; future. 
(deftest read-struct2
  (preserving-package
    (let ((s (read-from-string
	       #-(and cmu (not eclipse)) "#S(sample-struct c 3 b 2)"
	       #+(and cmu (not eclipse)) "#S(sample-struct :c 3 :b 2)")))
      (and (sample-struct-p s)
	   (= (sample-struct-a s) 9)
	   (= (sample-struct-b s) 2)
	   (= (sample-struct-c s) 3))))
  t)

(deftest read-pathname
  (let ((p (read-from-string "#P\"FOO.LISP\"")))
    (and (string= (pathname-name p) "FOO")
	 (string= (pathname-type p) "LISP")
	 t))
  t)


(defread-test circular "(#9=42 #9#)" (42 42))
(defread-test circular2 "(#9=(a b) #0=32 (#9# #0#))"
  (#9=(a b) #0=32 (#9# #0#)))

(defread-test features1 "#+zzz 1 2" 2)
(defread-test features2 "#-zzz 1 2" 1)
(defread-test features3 "#+(and xxx zzz) 1 2" 2)
(defread-test features4 "#+(not (or xxx zzz)) 1 2" 1)
(defread-test features5 "#-(and xxx zzz) 1 2" 1)
(defread-test features6 "#-(not (or xxx zzz)) 1 2" 2)

(defread-test block-comment "#|<evil stuff>|# ok" ok)
(defread-test nested-block-comment "#|<evil #|stuff#|>|#|#|# ok" ok)
(defread-test nested-block-comment2 "#|<evil #|stuff#|>||#||#||# ok" ok)
(defread-test nested-block-comment3 "#|| <evil stuff ||# ok" ok)

(deftest unprintable (gives-error (test-reader "#<foo>")))
(deftest dispatch-space (gives-error (test-reader "# ")))
(deftest dispatch-close (gives-error (test-reader "#)")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 22.1.5 THE READTABLE
;;; *READTABLE*, COPY-READTABLE 
(defparameter *table2* (copy-readtable))
(deftest copy-readtable
    (values (set-syntax-from-char #\z #\' *table2*)
	    (test-reader "zvar")
	    (eq *table2* (copy-readtable *table2* *readtable*))
	    (test-reader "zvar")
	    (eq *table2* (setq *readtable* (copy-readtable)))
	    (test-reader "zvar")
	    (eq *table2* (setq *readtable* (copy-readtable nil)))
	    (test-reader "zvar")
	    (set-syntax-from-char #\z #\z *table2*))
  t zvar nil (quote var) nil (quote var) nil zvar t)

;;; READTABLEP
(deftest readtablep1 (readtablep *readtable*) t)
(deftest readtablep2 (readtablep (copy-readtable)) t)
(deftest readtablep3 (readtablep '*readtable*) nil)
    
;;; SET-SYNTAX-FROM-CHAR
(deftest set-synax-from-char
    (values (set-syntax-from-char #\7 #\; *table2*)
	    (let ((*readtable* *table2*)) (test-reader "123579"))
	    (set-syntax-from-char #\7 #\7 *table2*)
	    (let ((*readtable* *table2*)) (test-reader "123579")))
  t 1235 t 123579)

;;; SET-MACRO-CHARACTER !!!
;;; GET-MACRO-CHARACTER 
(deftest get-macro-character-empty
    (get-macro-character #\{) nil nil)
(deftest get-macro-character
    (not (get-macro-character #\;)) nil)

;;; MAKE-DISPATCH-MACRO-CHARACTER
(deftest make-dispatch-macro-character
    (multiple-value-call #'values
      (get-macro-character #\{ *table2*)
      (make-dispatch-macro-character #\{ t *table2*)
      (nth-value 1 (get-macro-character #\{ *table2*))
      (set-syntax-from-char #\{ #\{ *table2*))
  nil nil t t t)

;;; SET-DISPATCH-MACRO-CHARACTER, GET-DISPATCH-MACRO-CHARACTER
(defun brace-reader (s c n)
  (declare (ignore c))
  (let ((list (read s nil (values) t)))
    (when (consp list)
      (unless (and n (< 0 n (length list))) (setq n 0))
      (setq list (nth n list)))
    list))
(deftest dispatch-macro-character
    (values (get-dispatch-macro-character #\# #\{ *table2*)
	    (set-dispatch-macro-character
	     #\# #\{ (function brace-reader) *table2*)
	    (let ((*readtable* *table2*))
	      (with-input-from-string (s "#{(1 2 3 4)
                                          #3{(0 1 2 3)
                                          #{123")
		(list (read s)
		      (read s)
		      (read s))))
	    (eq (function brace-reader)
		(get-dispatch-macro-character #\# #\{ *table2*))
	    (set-syntax-from-char #\{ #\{ *table2*))
  nil t (1 3 123) t t)
    

;;; READTABLE-CASE
#+not-yet
(deftest test-readtable-case-reading ()
  (with-output-to-string (s)
    (let ((*readtable* (copy-readtable nil)))
      (format t "READTABLE-CASE  Input   Symbol-name~
              ~%-----------------------------------~
              ~%")
      (dolist (readtable-case '(:upcase :downcase :preserve :invert))
	(setf (readtable-case *readtable*) readtable-case)
	(dolist (input '("ZEBRA" "Zebra" "zebra"))
	  (format t "~&:~A~16T~A~24T~A"
		  (string-upcase readtable-case)
		  input
		  (symbol-name (read-from-string input)))))))
"READTABLE-CASE  Input   Symbol-name
-----------------------------------
:UPCASE         ZEBRA   ZEBRA
:UPCASE         Zebra   ZEBRA
:UPCASE         zebra   ZEBRA
:DOWNCASE       ZEBRA   zebra
:DOWNCASE       Zebra   zebra
:DOWNCASE       zebra   zebra
:PRESERVE       ZEBRA   ZEBRA
:PRESERVE       Zebra   Zebra
:PRESERVE       zebra   zebra
:INVERT         ZEBRA   zebra
:INVERT         Zebra   Zebra
:INVERT         zebra   ZEBRA
")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 22.2 INPUT FUNCTIONS                                         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 22.2.1 INPUT FROM CHARACTER STREAMS
;;; READ See above.
;;; *READ-DEFAULT-FLOAT-FORMAT* See above.
;;; READ-PRESERVING-WHITESPACE
(deftest read-preserving-whitespace-normal-read
  (preserving-package
    (with-input-from-string (s "   'a  ")
      (values (read s) (read s nil 'the-end))))
  (quote a) the-end)
(defun skip-then-read-char (s c n)
  (declare (ignore n))
  (if (char= c #\{) (read s t nil t) (read-preserving-whitespace s))
  (read-char-no-hang s))
(deftest read-reserving-whitespace
    (let ((*readtable* *table2*))
      (values
       (set-dispatch-macro-character #\# #\{ #'skip-then-read-char)
       (set-dispatch-macro-character #\# #\} #'skip-then-read-char)
       (with-input-from-string (is "#{123 x #}123 y")
	 (list (read is) (read is)))
       (set-syntax-from-char #\# #\#)))
  t t (#\x #\space) t)

;;; READ-DELIMITED-LIST
(deftest read-delimited-list
    (with-input-from-string (s " 1 2 3 4 5 6 ]")
      (read-delimited-list #\] s))
  (1 2 3 4 5 6))
(defun pairs-reader (stream char arg)
  (declare (ignore char arg))
  (mapcon #'(lambda (x)
			(mapcar #'(lambda (y)
				    (list (car x) y))
				(cdr x)))
	  (read-delimited-list #\} stream t)))
(deftest read-delimited-list-pairs
  (preserving-package
   (let ((*readtable* *table2*))
     (set-dispatch-macro-character
      #\# #\{ (function pairs-reader))
     (set-macro-character
      #\} (get-macro-character #\) nil))
     (prog1 (read-from-string "#{p q z a}")
       (set-syntax-from-char #\# #\#))))
  ((p q) (p z) (p a) (q z) (q a) (z a)))

;;; READ-LINE, READ-CHAR, UNREAD-CHAR, PEEK-CHAR, LISTEN,
;;; READ-CHAR-NO-HANG, CLEAR-INPUT and READ-BYTE are all tested with
;;; streams. 

;;; READ-FROM-STRING
(deftest read-from-string1 (read-from-string " 1 3 5" t nil :start 2) 3 5)
(deftest read-from-string2
  (preserving-package
    (read-from-string "(a b c)"))
  (a b c) 7)

;;; PARSE-INTEGER
(deftest parse-integer1 (parse-integer "123") 123 3)
(deftest parse-integer2 (parse-integer "123" :start 1 :radix 5) 13 3)
(deftest parse-integer3 (parse-integer "no-integer" :junk-allowed t)
  nil 0)
(deftest parse-integer4 (parse-integer "   123") 123 6)
(deftest parse-integer5 (parse-integer "#xab" :junk-allowed t) nil 0)
(deftest parse-integer6
    (parse-integer "#xab" :radix 16 :junk-allowed t) nil 0)
(deftest parse-integer7
    (parse-integer "#xab" :start 2 :radix 16 :junk-allowed t) 171 4)
(deftest parse-integer8 (parse-integer "+123") 123 4)
(deftest parse-integer9 (parse-integer "-123") -123 4)
(deftest parse-integer10 (parse-integer " -123" :radix 5) -38 5)
(deftest parse-integer11 (parse-integer "123+" :junk-allowed t) 123 3)
(deftest parse-integer12
    (parse-integer "123." :junk-allowed t :radix 5) 38 3)
(deftest parse-integer13 (parse-integer "123" :end nil) 123 3)

