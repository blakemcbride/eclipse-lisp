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

(defmacro ftest (width miser form &rest bindings)
  `(plet ,width ,miser (let ,bindings (write-to-string ,form))))

(declaim (special *dt*))

(DEFTEST XP361
 (FTEST 100 0 '(SETZ A (CAR V) B (CDR Z)) (*PRINT-PPRINT-DISPATCH* *DT*))
 "[SETZ A (CAR V) B (CDR Z)]")
(DEFTEST XP362
 (FTEST 20 0 '(SETZ A (CAR V) B (CDR Z)) (*PRINT-PPRINT-DISPATCH* *DT*))
 "[SETZ A (CAR V)
      B (CDR Z)]")
(DEFTEST XP363
 (FTEST 20 20 '(SETZ A (CAR V) B (CDR Z)) (*PRINT-PPRINT-DISPATCH* *DT*)) "[SETZ
 A
 (CAR V)
 B
 (CDR Z)]")
(DEFTEST XP364
 (FTEST 17 0 '(SETZ A (CAR V) . B) (*PRINT-PPRINT-DISPATCH* *DT*))
 "[SETZ A (CAR V)
      . B]")
(DEFTEST XP365 (FTEST 100 0 '(SETZ . A) (*PRINT-PPRINT-DISPATCH* *DT*))
 "[SETZ . A]")
(DEFTEST XP366
 (FTEST 100 0 '(SETZ A (CAR V) B (CDR V)) (*PRINT-PPRINT-DISPATCH* *DT*)
  (*PRINT-LENGTH* 2))
 "[SETZ A ...]")
(DEFTEST XP367
 (FTEST 100 0 '(SETZ A (CAR V) B (CDR V)) (*PRINT-PPRINT-DISPATCH* *DT*)
  (*PRINT-LEVEL* 1))
 "[SETZ A # B #]")
(DEFTEST XP368
 (FTEST 100 0 '((SETZ A (CAR V) B (CDR V))) (*PRINT-PPRINT-DISPATCH* *DT*)
  (*PRINT-LEVEL* 1))
 "(#)")
(DEFTEST XP369
 (FTEST 100 0 '(SETZ A (CAR V) B (CDR Z)) (*PRINT-PPRINT-DISPATCH* *DT*)
  (*PRINT-CIRCLE* T))
 "[SETZ A (CAR V) B (CDR Z)]")
(DEFTEST XP370
 (FTEST 100 0 (READ-FROM-STRING "(setz a #1=(car v) b #1#)")
  (*PRINT-PPRINT-DISPATCH* *DT*) (*print-circle* t))
 "[SETZ A #1=(CAR V) B #1#]")
#+print-shared
(DEFTEST XP371
 (FTEST 100 0 (READ-FROM-STRING "(setz a #1=(car v) b #1#)")
  (*PRINT-PPRINT-DISPATCH* *DT*) (*print-circle* t))
 "[SETZ A (CAR V) B (CAR V)]")
(DEFTEST XP372
 (FTEST 100 0 (READ-FROM-STRING "#1=(setz a (car v) b #1#)")
  (*PRINT-PPRINT-DISPATCH* *DT*) (*print-circle* t))
 "#1=[SETZ A (CAR V) B #1#]")
(DEFTEST XP373
 (FTEST 100 0 (READ-FROM-STRING "#1=(setz a (car v) b #1#)")
  (*PRINT-PPRINT-DISPATCH* *DT*) (*print-circle* t))
 "#1=[SETZ A (CAR V) B #1#]")
(DEFTEST XP374
 (PLET 16 0
  (WITH-OUTPUT-TO-STRING (*TERMINAL-IO*)
    (LET ((LIST '(SETZ A (CAR V) B (CDR Z))))
      (PPRINT-LOGICAL-BLOCK (*TERMINAL-IO* LIST :PER-LINE-PREFIX "[")
        (PPRINT-POP)
        (WRITE-STRING "SETZ" T)
        (PPRINT-EXIT-IF-LIST-EXHAUSTED)
        (WRITE-CHAR #\SPACE T)
        (PPRINT-INDENT :CURRENT 0 T)
        (PPRINT-NEWLINE :MISER T)
        (LOOP (WRITE (PPRINT-POP) :STREAM T)
              (PPRINT-EXIT-IF-LIST-EXHAUSTED)
              (WRITE-CHAR #\SPACE T)
              (PPRINT-NEWLINE :FILL T)
              (WRITE (PPRINT-POP) :STREAM T)
              (PPRINT-EXIT-IF-LIST-EXHAUSTED)
              (WRITE-CHAR #\SPACE T)
              (PPRINT-NEWLINE :LINEAR T))))))
 "[SETZ A (CAR V)
[     B (CDR Z)")
(DEFTEST XP375
 (PLET 100 0
  (WITH-OUTPUT-TO-STRING (*STANDARD-OUTPUT*)
    (LET ((LIST '(SETZ A (CAR V) B (CDR V))))
      (POP LIST)
      (WRITE-STRING "SETZ")
      (WHEN LIST
        (WRITE-CHAR #\SPACE)
        (PPRINT-INDENT :CURRENT 0)
        (PPRINT-TAB :LINE 0 0)
        (PPRINT-NEWLINE :MISER)
        (LOOP (WRITE (POP LIST))
              (IF (NULL LIST) (RETURN NIL))
              (WRITE-CHAR #\SPACE)
              (PPRINT-NEWLINE :FILL)
              (WRITE (POP LIST))
              (IF (NULL LIST) (RETURN NIL))
              (WRITE-CHAR #\SPACE)
              (PPRINT-NEWLINE :LINEAR))))))
 "SETZ A (CAR V) B (CDR V)")
(DEFUN user::MY-PPRINT-TABULAR (S LIST &OPTIONAL (COLON? T) ATSIGN? (TABSIZE NIL))
  (DECLARE (IGNORE ATSIGN?))
  (IF (NULL TABSIZE) (SETQ TABSIZE 16))
  (PPRINT-LOGICAL-BLOCK
   (S LIST :PREFIX (IF COLON? "(" "") :SUFFIX (IF COLON? ")" ""))
   (PPRINT-EXIT-IF-LIST-EXHAUSTED)
   (LOOP (WRITE (PPRINT-POP) :STREAM S)
	 (PPRINT-EXIT-IF-LIST-EXHAUSTED)
	 (WRITE-CHAR #\SPACE S)
	 (PPRINT-TAB :SECTION-RELATIVE 0 TABSIZE S)
	 (PPRINT-NEWLINE :FILL S))))
(DEFTEST XP377
 (PLET 100 0
  (FORMAT NIL
          (FORMATTER "---~5/my-pprint-tabular/--")
          '(12 3456 789 22 45656 78)))
 "---12   3456 789  22   45656     78--")
(DEFTEST XP378
 (PLET 100 0
  (LET ((*PRINT-LENGTH* 3))
    (FORMAT NIL
            (FORMATTER "---~5/my-pprint-tabular/--")
            '(12 3456 789 22 456 78))))
 "---12   3456 789  ...--")
(DEFTEST XP379
 (PLET 21 0
  (FORMAT NIL
          (FORMATTER "---~5/my-pprint-tabular/--")
          '(12 3456 789 22 456 78)))
 "---12   3456 789
   22   456  78--")
(DEFTEST XP380
 (PLET 21 0
  (FORMAT NIL
          (FORMATTER "---~5:/my-pprint-tabular/--")
          '(12 3456 789 22 456 78)))
 "---(12   3456 789
    22   456  78)--")
(DEFTEST XP381
 (PLET 21 0
  (FORMAT NIL
          (FORMATTER "---~5:/my-pprint-tabular/--")
          '(12 3456 789 22 456 78)))
 "---(12   3456 789
    22   456  78)--")
(DEFTEST XP382
 (PLET 100 0
  (LET ((*PRINT-LENGTH* 3))
    (FORMAT NIL
            (FORMATTER "---~5:/my-pprint-tabular/--")
            '(12 3456 789 22 456 78))))
 "---(12   3456 789  ...)--")
(DEFTEST XP383
 (PLET 41 0
  (FORMAT NIL
          (FORMATTER "---~V/my-pprint-tabular/--")
          NIL
          '(12 3456 789 22 456 78)))
 "---12              3456
   789             22
   456             78--")
(DEFTEST XP384
 (PLET 21 0 (FORMAT NIL (FORMATTER "---~5:/my-pprint-tabular/--") NIL))
 "---()--")
(DEFTEST XP385
 (PLET 21 0 (FORMAT NIL (FORMATTER "---~5:/my-pprint-tabular/--") 12))
 "---12--")
(DEFTEST XP386
 (PLET 21 0
  (LET ((*PRINT-LEVEL* 0))
    (FORMAT NIL (FORMATTER "---~5/my-pprint-tabular/--") NIL)))
 "---#--")
(DEFTEST XP387
 (PLET 21 0
  (LET ((*PRINT-LEVEL* 0))
    (FORMAT NIL (FORMATTER "---~5:/my-pprint-tabular/--") NIL)))
 "---#--")
(DEFTEST XP388
 (PROGN (SETQ *PRINT-PPRINT-DISPATCH* (COPY-PPRINT-DISPATCH)) NIL)
 nil)
(DEFTEST XP389 (FTEST 15 0 '(CONS AAAAAA BBBBB)) "(CONS AAAAAA
      BBBBB)")
(DEFTEST XP390 (FTEST 15 0 '(CONS AAAAAA (CONS A B)) (*PRINT-LEVEL* 1))
 "(CONS AAAAAA #)")
(DEFTEST XP391 (FTEST 100 0 '(BLOCK FOO (IF (NULL Y) (RETURN T)) X))
 "(BLOCK FOO (IF (NULL Y) (RETURN T)) X)")
(DEFTEST XP392 (FTEST 30 0 '(BLOCK FOO (IF (NULL Y) (RETURN T)) X)) "(BLOCK FOO
  (IF (NULL Y) (RETURN T))
  X)")
(DEFTEST XP393 (FTEST 30 40 '(BLOCK FOO (IF (NULL Y) (RETURN T)) X)) "(BLOCK
 FOO
 (IF (NULL Y) (RETURN T))
 X)")
(DEFTEST XP394 (FTEST 100 0 '(BLOCK FOO . T)) "(BLOCK FOO . T)")
(DEFTEST XP395 (FTEST 100 0 '(BLOCK . T)) "(BLOCK . T)")
(DEFTEST XP396 (FTEST 100 0 '((BLOCK . T)) (*PRINT-LEVEL* 1)) "(#)")
(DEFTEST XP397 (FTEST 20 0 '(CASE TYPE (:FOO (PRINT 3)))) "(CASE TYPE
  (:FOO (PRINT 3)))")
(DEFTEST XP398 (FTEST 10 0 '(CATCH 'BAR (FOO X))) "(CATCH 'BAR
  (FOO X))")
(DEFTEST XP399 (FTEST 20 0 '(CCASE TYPE (:FOO (PRINT 3)))) "(CCASE TYPE
  (:FOO (PRINT 3)))")
(set-pprint-dispatch '(cons (eql compiler-let))
		     (pprint-dispatch '(let )))
(DEFTEST XP400
 (FTEST 100 0
  '(COMPILER-LET ((A (FOO 3)) (B (FOO 4)) (C 1))
     (TUZ A B)))
 "(COMPILER-LET ((A (FOO 3)) (B (FOO 4)) (C 1)) (TUZ A B))")
(DEFTEST XP401
 (FTEST 50 0
  '(COMPILER-LET ((A (FOO 3)) (B (FOO 4)) (C 1))
     (TUZ A B)))
"(COMPILER-LET ((A (FOO 3)) (B (FOO 4)) (C 1))
  (TUZ A B))")
(DEFTEST XP402
 (FTEST 44 0
  '(COMPILER-LET ((A (FOO 3)) (B (FOO 4)) (C 1))
     (TUZ A B)))
 "(COMPILER-LET ((A (FOO 3))
               (B (FOO 4))
               (C 1))
  (TUZ A B))")
(DEFTEST XP403
 (FTEST 44 50
  '(COMPILER-LET ((A (FOO 3)) (B (FOO 4)) (C 1))
     (TUZ A B)))
 "(COMPILER-LET
 ((A (FOO 3)) (B (FOO 4)) (C 1))
 (TUZ A B))")
(DEFTEST XP404
 (FTEST 30 0
  '(COMPILER-LET (BAR BAZ DEF GACK GORTCH)
     (TUZ A B)))
 "(COMPILER-LET (BAR BAZ DEF
               GACK GORTCH)
  (TUZ A B))")
(DEFTEST XP405
 (FTEST 40 0
  '(COMPILER-LET ((BAR BAZ DEF GACK . GORTCH))
     ))
 "(COMPILER-LET ((BAR BAZ DEF GACK
                . GORTCH)))")
(DEFTEST XP406
 (FTEST 100 0
  '(COMPILER-LET (BAR BAZ DEF GACK . GORTCH)
     ))
 "(COMPILER-LET (BAR BAZ DEF GACK . GORTCH))")
(DEFTEST XP407
 (FTEST 40 0
  '(COMPILER-LET FOO
     ))
 "(COMPILER-LET FOO)")
(DEFTEST XP408
 (FTEST 40 0
  '(COMPILER-LET ()
     ))
 "(COMPILER-LET ())")
(DEFTEST XP409 (FTEST 40 0 '(COMPILER-LET . FOO)) "(COMPILER-LET . FOO)")
(DEFTEST XP410
 (FTEST 100 0 '(COND ((PLUSP X) (PRINT Y) . 4) (A B) (T (CAR Z))))
 "(COND ((PLUSP X) (PRINT Y) . 4) (A B) (T (CAR Z)))")
(DEFTEST XP411
 (FTEST 55 0 '(COND ((PLUSP X) (PRINT Y) (MINUS Z)) (A B) (T (CAR W))))
 "(COND ((PLUSP X) (PRINT Y) (MINUS Z))
      (A B)
      (T (CAR W)))")
(DEFTEST XP412
 (FTEST 36 0 '(COND ((PLUSP X) (PRINT Y) (MINUS Z)) (A B) (T (CAR W))))
 "(COND ((PLUSP X)
       (PRINT Y)
       (MINUS Z))
      (A B)
      (T (CAR W)))")
(DEFTEST XP413
 (FTEST 30 40 '(COND ((PLUSP X) (PRINT Y) (MINUS Z)) (A B) (T (CAR W)))) "(COND
 ((PLUSP X)
  (PRINT Y)
  (MINUS Z))
 (A B)
 (T (CAR W)))")
(DEFTEST XP414 (FTEST 10 40 '(COND (A B) . T)) "(COND
 (A B)
 . T)")
(DEFTEST XP415 (FTEST 10 40 '(COND)) "(COND)")
(DEFTEST XP416 (FTEST 20 0 '(CTYPECASE TYPE (:FOO (PRINT 3)))) "(CTYPECASE TYPE
  (:FOO (PRINT 3)))")
(DEFTEST XP417 (FTEST 20 0 '(DEFCONSTANT FOO 2 "test")) "(DEFCONSTANT FOO 2
  \"test\")")
(DEFTEST XP418
 (FTEST 30 0 '(DEFCONSTANT FOO 2 (DEFCONSTANT)) (*PRINT-LEVEL* 1))
 "(DEFCONSTANT FOO 2 #)")
(set-pprint-dispatch '(cons (eql define-setf-method))
		     (pprint-dispatch '(define-setf-expander )))
(DEFTEST XP419
 (FTEST 40 0 '(DEFINE-SETF-METHOD LDB (A B) (MAKE-RIGHT BODY B A)))
"(DEFINE-SETF-METHOD LDB (A B)
  (MAKE-RIGHT BODY B A))")
(DEFTEST XP420
 (FTEST 100 0 '(DEFMACRO FOO (A (B C) &BODY D) (CAR A) (LIST C B)))
 "(DEFMACRO FOO (A (B C) &BODY D) (CAR A) (LIST C B))")
(DEFTEST XP421
 (FTEST 40 0 '(DEFMACRO FOO (A (B C) &BODY D) (CAR A) (LIST C B)))
 "(DEFMACRO FOO (A (B C) &BODY D)
  (CAR A)
  (LIST C B))")
(DEFTEST XP422
 (FTEST 25 0 '(DEFMACRO FOO (A (B C) &BODY D) (CAR A) (LIST C B)))
 "(DEFMACRO FOO (A (B C)
               &BODY D)
  (CAR A)
  (LIST C B))")
(DEFTEST XP423
 (FTEST 15 50 '(DEFMACRO FOO (A (B C) &BODY D) (CAR A) (LIST C B))) "(DEFMACRO
 FOO
 (A
  (B C)
  &BODY
  D)
 (CAR A)
 (LIST C B))")
(DEFTEST XP424 (FTEST 100 0 '(DEFMACRO FOO () . T)) "(DEFMACRO FOO () . T)")
(DEFTEST XP425 (FTEST 100 0 '(DEFMACRO . FOO)) "(DEFMACRO . FOO)")
(DEFTEST XP426
 (FTEST 100 0 '(DEFINE-MODIFY-MACRO BAR (A B) UNION "fancy union"))
 "(DEFINE-MODIFY-MACRO BAR (A B) UNION \"fancy union\")")
(DEFTEST XP427
 (FTEST 40 0 '(DEFINE-MODIFY-MACRO BAR (A B) UNION "fancy union"))
 "(DEFINE-MODIFY-MACRO BAR (A B) UNION
  \"fancy union\")")
(DEFTEST XP428
 (FTEST 30 0 '(DEFINE-MODIFY-MACRO BAR (A B) UNION "fancy union"))
 "(DEFINE-MODIFY-MACRO BAR
                     (A B)
                     UNION
  \"fancy union\")")
(DEFTEST XP429 (FTEST 100 0 '(DEFINE-MODIFY-MACRO BAR (A B) UNION . T))
 "(DEFINE-MODIFY-MACRO BAR (A B) UNION . T)")
(DEFTEST XP430 (FTEST 100 0 '(DEFINE-MODIFY-MACRO BAR ARGS . T))
 "(DEFINE-MODIFY-MACRO BAR ARGS . T)")
(DEFTEST XP431 (FTEST 100 0 '(DEFINE-MODIFY-MACRO BAR . T))
 "(DEFINE-MODIFY-MACRO BAR . T)")
(DEFTEST XP432 (FTEST 100 0 '(DEFINE-MODIFY-MACRO . T))
 "(DEFINE-MODIFY-MACRO . T)")
(DEFTEST XP434 (FTEST 20 0 '(DEFPARAMETER FOO 2 "test")) "(DEFPARAMETER FOO 2
  \"test\")")
(DEFTEST XP435
 (FTEST 40 0 '(DEFSETF BAR (A B) (STORE) (CAR X) (MAKE-BODY B A)))
 "(DEFSETF BAR (A B) (STORE)
  (CAR X)
  (MAKE-BODY B A))")
(DEFTEST XP436
 (FTEST 20 0 '(DEFSETF BAR (A B) (STORE) (CAR X) (MAKE-BODY B A)))
 "(DEFSETF BAR (A B)
         (STORE)
  (CAR X)
  (MAKE-BODY B A))")
(DEFTEST XP437
 (FTEST 40 0 '(DEFINE-SETF-METHOD BAR (A B) (CAR X) (MAKE-BODY B A)))
 "(DEFINE-SETF-METHOD BAR (A B)
  (CAR X)
  (MAKE-BODY B A))")
(DEFTEST XP438 (FTEST 40 0 '(DEFSTRUCT (FOO (:PRINT-FN BAR)) ACAC BABAB))
 "(DEFSTRUCT (FOO (:PRINT-FN BAR))
  ACAC
  BABAB)")
(DEFTEST XP439 (FTEST 30 0 '(DEFTYPE BAR (A) (SATISFIES BAR-P)))
 "(DEFTYPE BAR (A)
  (SATISFIES BAR-P))")
(DEFTEST XP440 (FTEST 30 0 '(DEFUN BAR (A) (SATISFIES BAR-P))) "(DEFUN BAR (A)
  (SATISFIES BAR-P))")
(DEFTEST XP441 (FTEST 20 0 '(DEFVAR FOO 2 "test")) "(DEFVAR FOO 2
  \"test\")")
(DEFTEST XP442
 (FTEST 100 0
  '(DO ((A X (CDR A))
        (I 1 (1+ I)))
       ((PLUSP B) T)
     (PRINT C)))
 "(DO ((A X (CDR A)) (I 1 (1+ I))) ((PLUSP B) T)  (PRINT C))")
(DEFTEST XP443
 (FTEST 55 0
  '(DO ((A X (CDR A))
        (I 1 (1+ I)))
       ((PLUSP C) T)
     (PRINT B)))
 "(DO ((A X (CDR A)) (I 1 (1+ I)))
    ((PLUSP C) T)
  (PRINT B))")
(DEFTEST XP444
 (FTEST 30 0
  '(DO ((A X (CDR A))
        (I 1 (1+ I)))
       ((PLUSP B) T)
     (PRINT C)))
 "(DO ((A X (CDR A))
     (I 1 (1+ I)))
    ((PLUSP B) T)
  (PRINT C))")
(DEFTEST XP445
 (FTEST 15 0
  '(DO ((A X (CDR A))
        (I 1 (1+ I)))
       ((PLUSP B) T)
     (PRINT C)))
 "(DO ((A X
      (CDR A))
     (I 1
      (1+ I)))
    ((PLUSP B)
     T)
  (PRINT C))")
(DEFTEST XP446
 (FTEST 15 20
  '(DO ((A X (CDR A))
        (I 1 (1+ I)))
       ((PLUSP B) T)
     (PRINT C)))
 "(DO
 ((A
   X
   (CDR A))
  (I
   1
   (1+ I)))
 ((PLUSP B)
  T)
 (PRINT C))")
(DEFTEST XP447 (FTEST 100 0 '(DO () () . T)) "(DO () ()  . T)")
(DEFTEST XP448 (FTEST 100 0 '(DO () . T)) "(DO () . T)")
(DEFTEST XP449 (FTEST 100 0 '(DO . T)) "(DO . T)")
(DEFTEST XP450
 (FTEST 55 0
  '(DO* ((A X (CDR A))
         (I 1 (1+ I)))
        ((PLUSP B) T)
     (PRINT C)))
 "(DO* ((A X (CDR A)) (I 1 (1+ I)))
     ((PLUSP B) T)
  (PRINT C))")
(DEFTEST XP451 (FTEST 35 0 '(DO-ALL-SYMBOLS (S *PACKAGE*) (PRINT S)))
 "(DO-ALL-SYMBOLS (S *PACKAGE*)
  (PRINT S))")
(DEFTEST XP452 (FTEST 35 0 '(DO-EXTERNAL-SYMBOLS (S *PACKAGE*) (PRINT S)))
 "(DO-EXTERNAL-SYMBOLS (S *PACKAGE*)
  (PRINT S))")
(DEFTEST XP453 (FTEST 35 0 '(DO-SYMBOLS (S *PACKAGE*) (PRINT S)))
 "(DO-SYMBOLS (S *PACKAGE*)
  (PRINT S))")
(DEFTEST XP454 (FTEST 25 0 '(DOLIST (S LIST) (PRINT S))) "(DOLIST (S LIST)
  (PRINT S))")
(DEFTEST XP455 (FTEST 25 0 '(DOTIMES (S LIST) (PRINT S))) "(DOTIMES (S LIST)
  (PRINT S))")
(DEFTEST XP456 (FTEST 20 0 '(ECASE TYPE (:FOO (PRINT 3)))) "(ECASE TYPE
  (:FOO (PRINT 3)))")
(DEFTEST XP457 (FTEST 20 0 '(ETYPECASE TYPE (:FOO (PRINT 3)))) "(ETYPECASE TYPE
  (:FOO (PRINT 3)))")
(DEFTEST XP458 (FTEST 20 0 '(EVAL-WHEN (COMPILE LOAD) (DEFUN FOO () (CAR X))))
 "(EVAL-WHEN (COMPILE LOAD)
  (DEFUN FOO ()
    (CAR X)))")
(DEFTEST XP459
 (FTEST 100 0
  '(FLET ((A (A B)
            (CAR A)
            (CAR D))
          (B ()
            T))
     (A (B 3))))
 "(FLET ((A (A B) (CAR A) (CAR D)) (B NIL T)) (A (B 3)))")
(DEFTEST XP460
 (FTEST 50 0
  '(FLET ((A (A B)
            (CAR A)
            (CAR D))
          (B ()
            T))
     (A (B 3))))
 "(FLET ((A (A B) (CAR A) (CAR D)) (B NIL T))
  (A (B 3)))")
(DEFTEST XP461
 (FTEST 42 0
  '(FLET ((A (A B)
            (CAR A)
            (CAR D))
          (B ()
            T))
     (A (B 3))))
 "(FLET ((A (A B) (CAR A) (CAR D))
       (B NIL T))
  (A (B 3)))")
(DEFTEST XP462
 (FTEST 30 0
  '(FLET ((A (A B)
            (CAR A)
            (CAR D))
          (B ()
            T))
     (A (B 3))))
 "(FLET ((A (A B)
         (CAR A)
         (CAR D))
       (B NIL T))
  (A (B 3)))")
(DEFTEST XP463
 (FTEST 35 50
  '(FLET ((A (A B)
            (CAR A)
            (CAR D))
          (B ()
            T))
     (A (B 3))))
 "(FLET
 ((A (A B) (CAR A) (CAR D))
  (B NIL T))
 (A (B 3)))")
(DEFTEST XP464
 (FTEST 100 0
  '(FLET (() T . T)
     . T))
 "(FLET (() T . T) . T)")
(DEFTEST XP465
 (FTEST 100 0
  '(FLET T
     . T))
 "(FLET T . T)")
(DEFTEST XP466 (FTEST 100 0 '(FLET . T)) "(FLET . T)")
(DEFTEST XP467 (FTEST 100 0 '(function (LAMBDA (A) (CAR B)))) "#'(LAMBDA (A) (CAR B))")
(DEFTEST XP468 (FTEST 100 0 '(function (LAMBDA (A) (CAR B))) (*PRINT-PRETTY* NIL))
 "(FUNCTION (LAMBDA (A) (CAR B)))")
(DEFTEST XP469 (FTEST 5 20 '(function CAR)) "#'CAR")
(DEFTEST XP470 (FTEST 100 0 '(FUNCTION . A)) "(FUNCTION . A)")
(DEFTEST XP471 (FTEST 100 0 '(FUNCTION)) "(FUNCTION)")
(DEFTEST XP472 (FTEST 100 0 '(FUNCTION (LAMBDA (A) (CAR B)) C))
 "(FUNCTION (LAMBDA (A) (CAR B)) C)")
(DEFTEST XP473
 (FTEST 42 0
  '(LABELS ((A (A B)
              (CAR A)
              (CAR D))
            (B ()
              T))
     (A (B 3))))
 "(LABELS ((A (A B) (CAR A) (CAR D))
         (B NIL T))
  (A (B 3)))")
(DEFTEST XP474 (FTEST 20 0 '(LAMBDA (A B) (CAR A) (CAR D))) "(LAMBDA (A B)
  (CAR A)
  (CAR D))")
(DEFTEST XP475
 (FTEST 34 0
  '(LET ((A (FOO 3)) (B (FOO 4)) (C 1))
     (TUZ A B)))
 "(LET ((A (FOO 3))
      (B (FOO 4))
      (C 1))
  (TUZ A B))")
(DEFTEST XP476
 (FTEST 34 0
  '(LET* ((A (FOO 3)) (B (FOO 4)) (C 1))
     (TUZ A B)))
 "(LET* ((A (FOO 3))
       (B (FOO 4))
       (C 1))
  (TUZ A B))")
(DEFTEST XP477 (FTEST 34 0 '(LOCALLY (DECLAR (SPECIAL X)) (PRINT Y)))
 "(LOCALLY (DECLAR (SPECIAL X))
  (PRINT Y))")
(DEFTEST XP478
 (FTEST 42 0
  '(MACROLET ((A (A B)
                (CAR A)
                (CAR D))
              (B ()
                T))
     (A (B 3))))
 "(MACROLET ((A (A B) (CAR A) (CAR D))
           (B NIL T))
  (A (B 3)))")
(DEFTEST XP479
 (FTEST 42 0 '(MULTIPLE-VALUE-BIND (A B) (COMPUTE-IT X) (CAR A) (CAR D)))
 "(MULTIPLE-VALUE-BIND (A B)
    (COMPUTE-IT X)
  (CAR A)
  (CAR D))")
(DEFTEST XP480 (FTEST 32 0 '(MULTIPLE-VALUE-SETQ (A B) (COMPUTE-IT X)))
 "(MULTIPLE-VALUE-SETQ (A B)
  (COMPUTE-IT X))")
(DEFTEST XP481
 (FTEST 100 0
  '(PROG (A B C) (PRINT A) L SS (IF (NULL B) G) LONG0 (CAR D) LONG))
 "(PROG (A B C)
      (PRINT A)
 L SS (IF (NULL B) G)
 LONG0 (CAR D)
 LONG)")
(DEFTEST XP482
 (FTEST 100 100
  '(PROG (A B C) (PRINT A) L SS (IF (NULL B) G) LONG0 (CAR D) LONG))
 "(PROG (A B C)
      (PRINT A)
 L SS (IF (NULL B) G)
 LONG0 (CAR D)
 LONG)")
(DEFTEST XP483 (FTEST 100 0 '(PROG () (PRINT A) NIL L . SS)) "(PROG ()
      (PRINT A)
      NIL
 L . SS)")
(DEFTEST XP484 (FTEST 100 0 '(PROG T . T)) "(PROG T . T)")
(DEFTEST XP485 (FTEST 100 0 '(PROG . T)) "(PROG . T)")
(DEFTEST XP486
 (FTEST 100 0 '(PROG* ((A 3) B C) L SS (IF (NULL D) S) LONG0 (CAR E)))
 "(PROG* ((A 3) B C)
 L SS  (IF (NULL D) S)
 LONG0 (CAR E))")
(DEFTEST XP487 (FTEST 25 0 '(PROGV (A B) (1 2) (CAR A))) "(PROGV (A B) (1 2)
  (CAR A))")
(DEFTEST XP488 (FTEST 20 0 '(SETQ A (CAR Z) B (CDR V))) "(SETQ A (CAR Z)
      B (CDR V))")
(DEFTEST XP489 (FTEST 20 20 '(SETQ A (CAR Z) B (CDR V))) "(SETQ
 A
 (CAR Z)
 B
 (CDR V))")
(DEFTEST XP490 (FTEST 17 0 '(SETQ A (CAR V) B)) "(SETQ A (CAR V)
      B)")
#-cmu
(DEFTEST XP491 (ftest 17 0 '(setq a (car v) . b))
  "(SETQ A (CAR V)
      . B)")
#-cmu
(DEFTEST XP492 (ftest 100 0 '(setq . a))
  "(SETQ . A)")
(DEFTEST XP493 (FTEST 100 0 ''(LAMBDA (A) (CAR B))) "'(LAMBDA (A) (CAR B))")
(DEFTEST XP494 (FTEST 5 20 ''CAR) "'CAR")
#-cmu
(DEFTEST XP495 (FTEST 100 0 '(QUOTE . A)) "(QUOTE . A)")
(DEFTEST XP496 (FTEST 100 0 '(QUOTE)) "(QUOTE)")
(DEFTEST XP497 (FTEST 100 0 '(QUOTE (LAMBDA (A) (CAR C)) B))
 "(QUOTE (LAMBDA (A) (CAR C)) B)")
(DEFTEST XP498 (FTEST 20 0 '(RETURN-FROM FOO (COMPUTATION BAR)))
 "(RETURN-FROM FOO
  (COMPUTATION BAR))")
(DEFTEST XP499 (FTEST 20 0 '(SETF A (CAR V) B (CDR Z))) "(SETF A (CAR V)
      B (CDR Z))")
(DEFTEST XP500 (FTEST 1000 0 '(SETF A (CAR Z) B (CDR V)))
 "(SETF A (CAR Z) B (CDR V))")
(DEFTEST XP501 (FTEST 20 0 '(PSETF A (CAR Z) B (CDR V))) "(PSETF A (CAR Z)
       B (CDR V))")
(DEFTEST XP502 (FTEST 20 0 '(PSETQ A (CAR Z) B (CDR V))) "(PSETQ A (CAR Z)
       B (CDR V))")
(DEFTEST XP503
 (FTEST 100 0 '(TAGBODY (PRINT A) L SS (IF (NULL D) C) VERYLONG (CAR B) LONG))
 "(TAGBODY (PRINT A)
 L SS    (IF (NULL D) C)
 VERYLONG (CAR B)
 LONG)")
#-cmu
(DEFTEST XP504 (FTEST 100 0 '(TAGBODY L SS (IF (NULL B) C) . T)) "(TAGBODY
 L SS    (IF (NULL B) C) . T)")
#-cmu
(DEFTEST XP505 (FTEST 100 0 '(TAGBODY L . SS)) "(TAGBODY
 L . SS)")
#-cmu
(DEFTEST XP506 (FTEST 100 0 '(TAGBODY . SS)) "(TAGBODY . SS)")
(DEFTEST XP507 (FTEST 10 0 '(THROW 'BAR (FOO X))) "(THROW 'BAR
  (FOO X))")
(DEFTEST XP508 (FTEST 20 0 '(TYPECASE TYPE (:FOO (PRINT 3)))) "(TYPECASE TYPE
  (:FOO (PRINT 3)))")
(DEFTEST XP509 (FTEST 20 0 '(UNLESS (PLUSP X) (PRINT Y))) "(UNLESS (PLUSP X)
  (PRINT Y))")
(DEFTEST XP510 (FTEST 20 0 '(UNWIND-PROTECT (OPEN F) (PRINT ERRORMSG)))
 "(UNWIND-PROTECT
    (OPEN F)
  (PRINT ERRORMSG))")
(DEFTEST XP511 (FTEST 20 0 '(WHEN (PLUSP X) (PRINT Y))) "(WHEN (PLUSP X)
  (PRINT Y))")
(DEFTEST XP512 (FTEST 35 0 '(WITH-INPUT-FROM-STRING (F STRING) (READ F)))
 "(WITH-INPUT-FROM-STRING (F STRING)
  (READ F))")
(DEFTEST XP513
 (FTEST 45 0 '(WITH-OPEN-FILE (F NAME :DIRECTION :INPUT) (READ F)))
 "(WITH-OPEN-FILE (F NAME :DIRECTION :INPUT)
  (READ F))")
(DEFTEST XP514
 (FTEST 45 0 '(WITH-OPEN-STREAM (STREAM (MAKE-STREAM)) (READ STREAM)))
 "(WITH-OPEN-STREAM (STREAM (MAKE-STREAM))
  (READ STREAM))")
(DEFTEST XP515 (FTEST 35 0 '(WITH-OUTPUT-TO-STRING (F STRING) (PRINT F)))
 "(WITH-OUTPUT-TO-STRING (F STRING)
  (PRINT F))")



(DEFTEST XP640
 (FTEST 40 0
  '(LIST (LOOP FOR X IN L AND Z IN Y DO (PRINT X) (PRINT Z) COLLECT Z INTO W)
         (PRINT 111)))
 "(LIST (LOOP FOR X IN L
            AND Z IN Y
            DO (PRINT X)
               (PRINT Z)
            COLLECT Z INTO W)
      (PRINT 111))")
(DEFTEST XP641
 (FTEST 40 0 '(LOOP (DO THIS) AND THAT AND THAT (PRINT (LIST LOTS OF STUFF))))
 "(LOOP (DO THIS)
      AND
      THAT
      AND
      THAT
      (PRINT (LIST LOTS OF STUFF)))")
(DEFTEST XP642
 (FTEST 40 0
  '(LOOP FOR
         I
         IN
         NUMBERS-LIST
         WHEN
         (ODDP I)
         DO
         (PRINT J)
         AND
         COLLECT
         I
         INTO
         ODD-NUMBERS
         AND
         DO
         (TERPRI)
         ELSE
         COLLECT
         I
         INTO
         EVEN-NUMBERS
         FINALLY
         (RETURN (VALUES ODD-NUMBERS EVEN-NUMBERS))))
 "(LOOP FOR I IN NUMBERS-LIST
      WHEN (ODDP I)
        DO (PRINT J)
        AND COLLECT I INTO ODD-NUMBERS
        AND DO (TERPRI)
      ELSE
        COLLECT I INTO EVEN-NUMBERS
      FINALLY (RETURN (VALUES ODD-NUMBERS
                              EVEN-NUMBERS)))")
(DEFTEST XP643
 (FTEST 60 0
  '(LOOP FOR
         X
         IN
         L
         AND
         Z
         IN
         Y
         DO
         (PRINT X)
         (PRINT Z)
         WHEN
         (PLUSP X)
         DO
         (PRINT X)
         END
         WHEN
         (PLUSP X)
         UNLESS
         (PLUSP Y)
         DO
         (PRINT X)
         AND
         DO
         (PRINT Y)
         ELSE
         DO
         (PRINT Z)
         WHEN
         (PLUSP X)
         UNLESS
         (PLUSP Y)
         DO
         (PRINT X)
         AND
         DO
         (PRINT Y)
         END
         ELSE
         DO
         (PRINT Z)
         WHEN
         (PLUSP X)
         DO
         (PRINT X)
         ELSE
         DO
         (PRINT Y)
         IF
         (ZEROP Y)
         DO
         (PRINT Z)
         AND
         DO
         (PRINT W))
  (*print-circle* nil))
 "(LOOP FOR X IN L
      AND Z IN Y
      DO (PRINT X)
         (PRINT Z)
      WHEN (PLUSP X) DO (PRINT X) END
      WHEN (PLUSP X)
        UNLESS (PLUSP Y)
          DO (PRINT X)
          AND DO (PRINT Y)
        ELSE
          DO (PRINT Z)
      WHEN (PLUSP X)
        UNLESS (PLUSP Y)
          DO (PRINT X)
          AND DO (PRINT Y)
        END
      ELSE
        DO (PRINT Z)
      WHEN (PLUSP X) DO (PRINT X) ELSE DO (PRINT Y)
      IF (ZEROP Y)
        DO (PRINT Z)
        AND DO (PRINT W))")
(DEFTEST XP644 (FTEST 55 0 (read-from-string "`(,A ,B ,C)"))
 "`(,A ,B ,C)")
(DEFTEST XP645 (FTEST 40 0 '(LIST (LOOP (CAR X)))) "(LIST (LOOP (CAR X)))")


