#-eclipse (defun no-abbreviation-recall () "no abbreviation recall")
#-eclipse (defvar *last-abbreviated-printing* #'no-abbreviation-recall)
#+eclipse (eval-when (:compile-toplevel :load-toplevel :execute)
	    (cl:import 'eclipse::*last-abbreviated-printing*))


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



;;; It is not clear if text outside the outermost logical block is to
;;; be considered as part of the "section immediately containing a
;;; conditional newline."  Water's test cases seems to suggest that it
;;; should be.  In addition, Water's test cases show conditional
;;; newlines causing line breaks even when not contained in ANY
;;; logical block, and this is clearly wrong.
;;;
;;; This macro becomes just formatter if you want Water's XP behavior,
;;; otherwise, it wraps the control string in a logical block.
;;; 
;;; The other thing this does is allow us to specify the default
;;; "user" package for reading  ~/.../ functions. This is useful when
;;; running in some host system where USER is not really the user
;;; package of the system we are testing.
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


(DEFTEST XP162a (FORMAT NIL (FORMATTERx "fo-~:@_~<test~0,3ta~:>") NIL)
 "fo-
test  a")
(DEFTEST XP163x (FORMAT NIL (FORMATTERx "fo-~:@_~<test~0,4ta~:>") NIL)
 "fo-
test    a")
(DEFTEST XP164a (FORMAT NIL (FORMATTERx "fo-~:@_~<test~0,5ta~:>") NIL)
 "fo-
test a")


(DEFTEST XP195
 (PLET 16 0
  (LET ((*PRINT-LENGTH* 2))
    (FORMAT NIL (FORMATTER "---~<~a~^ ~a~^ ~a~:>--") '(12 3456 789))))
 "---12 3456 ...--")
(DEFTEST XP196
 (PLET 16 0
  (LET ((*PRINT-LENGTH* 1))
    (FORMAT NIL (FORMATTER "---~<~a~^ ~a~^ ~a~:>--") '(12 3456 789))))
 "---12 ...--")
(DEFTEST XP197
 (PLET 16 0
  (WITH-OUTPUT-TO-STRING (S)
    (LET ((*PRINT-LENGTH* 1))
      (FORMAT S (FORMATTER "---~<~a~^ ~a~^ ~a~:>--") '(12 3456 789)))
    (PRINC " " S)
    #+abbreviations (FUNCALL *LAST-ABBREVIATED-PRINTING*)))
#+abbreviations "---12 ...-- ---12 3456 789--"
#-abbreviations  "---12 ...-- ")
(DEFTEST XP198
 (PLET 16 0
  (LET ((*PRINT-LENGTH* 2))
    (FORMAT NIL (FORMATTER "---~@<~a~^ ~a~^ ~a~:>--") 12 3456 789)))
 "---12 3456 ...--")
(DEFTEST XP199
 (PLET 16 0
  (LET ((*PRINT-LENGTH* 2))
    (FORMAT NIL (FORMATTER "---~:@<~a~^ ~a~^ ~a~:>--") 12 3456 789)))
 "---(12 3456 ...)--")
(DEFTEST XP200
 (PLET 16 0
  (LET ((*PRINT-LENGTH* 2))
    (FORMAT NIL (FORMATTER "---~:@<~a~^ ~a~^ ~a~:>~A--") 12 3456 789)))
 "---(12 3456 ...)NIL--") 
(DEFTEST XP201 (PLET 20 0 (FORMAT NIL (FORMATTER "test~<a~2%  test~:>b") NIL))
 "testa

  testb")
(DEFTEST XP202
 (PLET 20 0
  (FORMAT NIL
          (FORMATTER "test~<a
  test~:>b")
          NIL))
 "testa
  testb")
(DEFTEST XP203 (PLET 20 0 (FORMAT NIL (FORMATTER "test~<a~&  test~:>b") NIL))
 "testa
  testb")
(DEFTEST XP204 (PLET 20 0 (FORMAT NIL (FORMATTER "~a~:@<~:>") 1 2 4 5)) "1()")
(DEFTEST XP205 (PLET 20 0 (FORMAT NIL (FORMATTER "~a~:@<~a~a~:>") 1 2 4 5))
 "1(24)")
(DEFTEST XP206 (PLET 20 0 (FORMAT NIL (FORMATTER "~a~@<+~;~a~a~:>") 1 2 4 5))
 "1+24")
(DEFTEST XP207 (PLET 20 0 (FORMAT NIL (FORMATTER "~a~:@<+~;~a~a~:>") 1 2 4 5))
 "1+24)")
(DEFTEST XP208
 (PLET 20 0 (FORMAT NIL (FORMATTER "~a~@<+(~;~a~a~;)*~:>") 1 2 4 5)) "1+(24)*")
(DEFTEST XP209
 (PLET 20 0 (FORMAT NIL (FORMATTER "~a~:@<+~;~a~a~;*~:>") 1 2 4 5)) "1+24*")
(DEFTEST XP210
 (PLET 50 0 (FORMAT NIL (FORMATTER "foo ~@<++~@;1~:@_2~:@_3~:>4"))) "foo ++1
    ++2
    ++34")
(DEFTEST XP211
 (PLET 50 0
  (FORMAT NIL
          (FORMATTER "foo ~@<++~@;1~:@_ hi ~@<##~@;1~:@_2~:@_3~:>~:@_3~:>4")))
 "foo ++1
    ++ hi ##1
    ++    ##2
    ++    ##3
    ++34")
(DEFTEST XP212
 (PLET 50 0
  (FORMAT NIL
          (FORMATTER "foo ~@<++~@;1~:@_2 ~S~:@_3~:>4")
          "testing
linebreaks"))
 "foo ++1
    ++2 \"testing
    ++linebreaks\"
    ++34")
(DEFTEST XP213
 (PLET 18 0 (FORMAT NIL (FORMATTERx "---~:<~a ~_~a ~_~a~:>--") '(12 3456 789)))
 "---(12 3456 789)--")
(DEFTEST XP214
 (PLET 17 0 (FORMAT NIL (FORMATTERx "---~:<~a ~_~a ~_~a~:>--") '(12 3456 789)))
 "---(12
    3456
    789)--")
(DEFTEST XP215
 (PLET 13 0 (FORMAT NIL (FORMATTERx "---~{~a ~_~a~2I ~_~a~}--") '(12 3456 789)))
 "---12
3456
  789--")
(DEFTEST XP216
 (PLET 17 0
  (FORMAT NIL (FORMATTERx "---~<<~;~a ~_~a ~_~a~;>~:>--") '(12 3456 789)))
 "---<12
    3456
    789>--")
(DEFTEST XP217
 (PLET 17 0
  (FORMAT NIL (FORMATTERx "---~<<~@;~a ~_~a ~_~a~;>~:>--") '(12 3456 789)))
 "---<12
   <3456
   <789>--")
(DEFTEST XP218
 (PLET 16 0
  (FORMAT NIL (FORMATTERx "---~<<~@;~a ~_~a ~_~a~:>--") '(12 3456 789)))
 "---<12
   <3456
   <789--")
(DEFTEST XP219
 (PLET 15 0 (FORMAT NIL (FORMATTERx "---~<12	3456 789~:@>--") NIL))
 "---12	3456
   789--")
(DEFTEST XP220
 (PLET 15 0 (FORMAT NIL (FORMATTERx "---~<~a ~a ~a~:@>--") '(12 3456 789)))
 "---12 3456
   789--")

;; This depends on your intepretation of what blanks are "immediately
;; contained" in a string.  Waters felt it excluded multi-key directives.

(DEFTEST XP221
 (PLET 15 0 (FORMAT NIL (FORMATTERx "---~<~a ~@{~a ~a~}~:@>--") '(12 3456 789)))
 #+waters-xp-behavior
 "---12
   3456 789--"
 #-waters-xp-behavior
"---12 3456
   789--"
)
(DEFTEST XP222
 (PLET 25 0 (FORMAT NIL (FORMATTERx "---~<~a	~a-~a~@:>--") '(12 3456 789)))
 "---12	3456-789--")
(DEFTEST XP223
 (PLET 15 0
  (LET ((*PRINT-LEVEL* 3))
    (FORMAT NIL (FORMATTER "0~:@<1~:@<2~:@<3~:>~:>~:>"))))
 "0(1(2(3)))")
(DEFTEST XP224
 (PLET 15 0
  (LET ((*PRINT-LEVEL* 2))
    (FORMAT NIL (FORMATTER "0~:@<1~:@<2~:@<3~:>~:>~:>"))))
 "0(1(2#))")
(DEFTEST XP225
 (PLET 15 0
  (LET ((*PRINT-LEVEL* 1))
    (FORMAT NIL (FORMATTER "0~:@<1~:@<2~:@<3~:>~:>~:>"))))
 "0(1#)")
(DEFTEST XP226
 (PLET 15 0
  (LET ((*PRINT-LEVEL* 0))
    (FORMAT NIL (FORMATTER "0~:@<1~:@<2~:@<3~:>~:>~:>"))))
 "0#")
(DEFTEST XP227
 (PLET 15 0
  (WITH-OUTPUT-TO-STRING (S)
    (LET ((*PRINT-LEVEL* 1))
      (FORMAT S (FORMATTER "0~:@<1~:@<2~:@<3~:>~:>~:>")))
    (FORMAT S " ")
    (FORMAT S (FORMATTER "0~:@<1~:@<2~:@<3~:>~:>~:>"))))
 "0(1#) 0(1(2(3)))")
(DEFTEST XP228
 (PLET 50 0
  (LET ((*PRINT-LEVEL* 1))
    (FORMAT NIL (FORMATTER "~:<~W~:@<~W~:>~:>") '(0 1 2 3 4))))
 "(0#)")
(DEFTEST XP229
 (PLET 16 0 (FORMAT NIL (FORMATTERx "---~<~a ~_~a ~_~a~:>--") '(12 3456 789)))
 "---12 3456 789--")
(DEFTEST XP230
 (PLET 15 0 (FORMAT NIL (FORMATTERx "---~<~a ~_~a ~_~a~:>--") '(12 3456 789)))
 "---12
   3456
   789--")
(DEFTEST XP231
 (PLET 16 0 (FORMAT NIL (FORMATTERx "---~<~a ~:_~a ~:_~a~:>--") '(12 3456 789)))
 "---12 3456 789--")
(DEFTEST XP232
 (PLET 11 0 (FORMAT NIL (FORMATTERx "---~<~a ~:_~a ~:_~a~:>--") '(12 3456 789)))
 "---12 3456
   789--")
(DEFTEST XP233
 (PLET 10 0 (FORMAT NIL (FORMATTERx "---~<~a ~:_~a ~:_~a~:>--") '(12 3456 789)))
 "---12
   3456
   789--")
(DEFTEST XP234
 (PLET 50 0
  (FORMAT NIL (FORMATTERx "---~<~a ~<<<~:@_>>~:>~:_~a~:>--") '(12 NIL 789)))
 "---12 <<
      >>
   789--")
(DEFTEST XP235
 (PLET 50 0
  (FORMAT NIL (FORMATTERx "---~<~a ~:_~<<<~:@_>>~:>~:_~a~:>--") '(12 NIL 789)))
 "---12
   <<
   >>
   789--")
(DEFTEST XP236
 (PLET 16 0 (FORMAT NIL (FORMATTERx "---~<~a ~@_~a ~:_~a~:>--") '(12 3456 789)))
 "---12 3456 789--")
(DEFTEST XP237
 (PLET 11 0 (FORMAT NIL (FORMATTERx "---~<~a ~@_~a ~:_~a~:>--") '(12 3456 789)))
 "---12 3456
   789--")
(DEFTEST XP238
 (PLET 11 NIL
  (FORMAT NIL (FORMATTERx "---~<~a ~@_~a ~:_~a~:>--") '(12 3456 789)))
 "---12 3456
   789--")
(DEFTEST XP239
 (PLET 11 20
  (FORMAT NIL (FORMATTERx "---~<~a ~@_~a ~:_~a~:>--") '(12 3456 789)))
 "---12
   3456
   789--")
(DEFTEST XP240
 (PLET 25 0 (FORMAT NIL (FORMATTER "---~<~a ~:@_~a ~_~a~:>--") '(12 3456 789)))
 "---12
   3456
   789--")
(DEFTEST XP241
 (PLET 13 0
  (FORMAT NIL (FORMATTERx "---~<~a ~:@_~a ~:_~a~:>--") '(12 3456 789)))
 "---12
   3456 789--")
(DEFTEST XP242
 (PLET 12 0
  (FORMAT NIL (FORMATTERx "---~<~a ~:@_~a ~:_~a~:>--") '(12 3456 789)))
 "---12
   3456
   789--")
(DEFTEST XP243
 (PLET 15 0
  (FORMAT NIL (FORMATTERx "---~<~a~1I ~_~a ~_~a~:>--") '(12 3456 789)))
 "---12
    3456
    789--")
(DEFTEST XP244
 (PLET 15 0
  (FORMAT NIL (FORMATTERx "---~<~a~-2I ~_~a ~_~a~:>--") '(12 3456 789)))
 "---12
 3456
 789--")
(DEFTEST XP245
 (PLET 15 0
  (FORMAT NIL (FORMATTERX "---~<~a~VI ~_~a ~_~a~:>--") '(12 -2 3456 789)))
 "---12
 3456
 789--")
(DEFTEST XP246
 (PLET 15 0
  (FORMAT NIL (FORMATTERX "---~<~a~:I ~_~a ~_~a~:>--") '(12 3456 789)))
 "---12
     3456
     789--")
(DEFTEST XP247
 (PLET 15 20
  (FORMAT NIL (FORMATTERX "---~<~a~:I ~_~a ~_~a~:>--") '(12 3456 789)))
 "---12
   3456
   789--")
(DEFTEST XP248
 (PLET 15 0
  (FORMAT NIL (FORMATTERX "---~<~a ~_~a~-1:I ~_~a~:>--") '(12 3456 789)))
 "---12
   3456
      789--")
(DEFTEST XP249
 (PLET 15 0
  (FORMAT NIL (FORMATTERX "---~<~a ~_~a~V:I ~_~a~:>--") '(12 3456 -1 789)))
 "---12
   3456
      789--")
(DEFTEST XP250
 (PLET 16 0
  (LET ((*PRINT-LENGTH* 3))
    (FORMAT NIL (FORMATTERX "---~<~a ~_~a ~_~a~:>--") '(12 3456 789))))
 "---12 3456 789--")
(DEFTEST XP251
 (PLET 15 0
  (LET ((*PRINT-LINES* 3))
    (FORMAT NIL (FORMATTERX "---~<~a ~_~a ~_~a~:>--") '(12 3456 789))))
 "---12
   3456
   789--")
(DEFTEST XP252
 (PLET 15 0
  (LET ((*PRINT-LINES* 2))
    (FORMAT NIL (FORMATTERX "---~<~a ~_~a ~_~a~:>--") '(12 3456 789))))
 "---12
   3456 ..")
(DEFTEST XP253
 (PLET 15 0
  (LET ((*PRINT-LINES* 1))
    (FORMAT NIL (FORMATTERX "---~<~a ~_~a ~_~a~:>--") '(12 3456 789))))
 "---12 ..")
(DEFTEST XP254
 (PLET 15 0
  (WITH-OUTPUT-TO-STRING (S)
    (LET ((*PRINT-LINES* 1))
      (FORMAT S (FORMATTERX "---~:<~a ~_~a ~_~a~:>--") '(12 3456 789)))
    (TERPRI S)
    #+abbreviations (FUNCALL *LAST-ABBREVIATED-PRINTING*)))
#+abbreviations 
 "---(12 ..)
---(12
    3456
    789)--"
#-abbreviations
"---(12 ..)
")


(DEFTEST XP255
 (PLET 15 0 (FORMAT NIL (FORMATTERX "---~/pprint-fill/--") '(12 3456 789)))
 "---12 3456
   789--")
(DEFTEST XP256
 (PLET 15 0 (FORMAT NIL (FORMATTERX "---~:/pprint-fill/--") '(12 3456 789)))
 "---(12 3456
    789)--")
(DEFTEST XP257 (PLET 15 0 (FORMAT NIL (FORMATTERX "---~:/pprint-fill/--") '12))
 "---12--")
(DEFTEST XP258
 (PLET 15 0
  (LET ((*PRINT-LEVEL* 4) (*PRINT-LENGTH* 2))
    (FORMAT NIL (FORMATTERX "---~:/pprint-fill/--") '(12 3456 789))))
 "---(12 3456
    ...)--")
(DEFTEST XP259
 (PLET 25 0
  (LET ((*PRINT-LEVEL* 4) (*PRINT-LENGTH* 2))
    (FORMAT NIL (FORMATTERX "---~/pprint-fill/--") '(12 3456 789))))
 "---12 3456 ...--")
(DEFTEST XP260
 (PLET 15 0
  (LET ((*PRINT-LEVEL* 0))
    (FORMAT NIL (FORMATTERX "---~:/pprint-fill/--") '(12 3456 789))))
 "---#--")
(DEFTEST XP261
 (PLET 15 0
  (LET ((*PRINT-LEVEL* 0))
    (FORMAT NIL (FORMATTERX "---~/pprint-fill/--") '(12 3456 789))))
 "---#--")
(DEFTEST XP262
 (PLET 15 0 (FORMAT NIL (FORMATTERX "---~/pprint-linear/--") '(12 3456 789)))
 "---12
   3456
   789--")
(DEFTEST XP263
 (PLET 15 0 (FORMAT NIL (FORMATTERX "---~:/pprint-linear/--") '(12 3456 789)))
 "---(12
    3456
    789)--")
(DEFTEST XP264
 (PLET 15 0 (FORMAT NIL (FORMATTERX "---~:/pprint-linear/--") '12)) "---12--")
(DEFTEST XP265
 (PLET 15 0
  (LET ((*PRINT-LEVEL* 4) (*PRINT-LENGTH* 2))
    (FORMAT NIL (FORMATTERX "---~:/pprint-linear/--") '(12 3456 789))))
 "---(12
    3456
    ...)--")
(DEFTEST XP266
 (PLET 25 0
  (LET ((*PRINT-LEVEL* 4) (*PRINT-LENGTH* 2))
    (FORMAT NIL (FORMATTERX "---~/pprint-linear/--") '(12 3456 789))))
 "---12 3456 ...--")
(DEFTEST XP267
 (PLET 15 0
  (LET ((*PRINT-LEVEL* 0))
    (FORMAT NIL (FORMATTERX "---~:/pprint-linear/--") '(12 3456 789))))
 "---#--")
(DEFTEST XP268
 (PLET 15 0
  (LET ((*PRINT-LEVEL* 0))
    (FORMAT NIL (FORMATTERX "---~/pprint-linear/--") '(12 3456 789))))
 "---#--")
(DEFTEST XP269
 (PLET 100 0
  (FORMAT NIL
          (FORMATTERX "---~5/pprint-tabular/--")
          '(12 3456 789 22 45656 78)))
 "---12   3456 789  22   45656     78--")
(DEFTEST XP270
 (PLET 100 0
  (FORMAT NIL
          (FORMATTERX "---~5/pprint-tabular/--")
          '(12+++ 3456 789 22 45656 78)))
 "---12+++     3456 789  22   45656     78--")
(DEFTEST XP271
 (PLET 100 0
  (LET ((*PRINT-LENGTH* 3))
    (FORMAT NIL
            (FORMATTERX "---~5/pprint-tabular/--")
            '(12 3456 789 22 456 78))))
 "---12   3456 789  ...--")
(DEFTEST XP272
 (PLET 21 0
  (FORMAT NIL (FORMATTERX "---~5/pprint-tabular/--") '(12 3456 789 22 456 78)))
 "---12   3456 789
   22   456  78--")
(DEFTEST XP273
 (PLET 21 0
  (FORMAT NIL (FORMATTERX "---~5:/pprint-tabular/--") '(12 3456 789 22 456 78)))
 "---(12   3456 789
    22   456  78)--")
(DEFTEST XP274
 (PLET 100 0
  (LET ((*PRINT-LENGTH* 3))
    (FORMAT NIL
            (FORMATTERX "---~5:/pprint-tabular/--")
            '(12 3456 789 22 456 78))))
 "---(12   3456 789  ...)--")
(DEFTEST XP275
 (PLET 41 0
  (FORMAT NIL
          (FORMATTERX "---~V/pprint-tabular/--")
          NIL
          '(12 3456 789 22 456 78)))
 "---12              3456
   789             22
   456             78--")
(DEFTEST XP276
 (PLET 21 0 (FORMAT NIL (FORMATTERx "---~5:/pprint-tabular/--") NIL)) "---()--")
(DEFTEST XP277
 (PLET 21 0 (FORMAT NIL (FORMATTERx "---~5:/pprint-tabular/--") 12)) "---12--")
(DEFTEST XP278
 (PLET 21 0
  (LET ((*PRINT-LEVEL* 0))
    (FORMAT NIL (FORMATTERx "---~5/pprint-tabular/--") NIL)))
 "---#--")
(DEFTEST XP279
 (PLET 21 0
  (LET ((*PRINT-LEVEL* 0))
    (FORMAT NIL (FORMATTERx "---~5:/pprint-tabular/--") NIL)))
 "---#--")

(DEFTEST XP280
 (PLET 90 0
  (LET ((*PRINT-ESCAPE* NIL))
    (FORMAT NIL (FORMATTER "~W") "foo")))
 "foo")
(DEFTEST XP281
 (PLET 90 0
  (LET ((*PRINT-ESCAPE* T))
    (FORMAT NIL (FORMATTER "~W") "foo")))
 "\"foo\"")
