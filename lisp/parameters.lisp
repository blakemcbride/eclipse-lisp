;;; This is the exclusive bound on values produced by the function 
;;; char-code for base-chars.
(defconstant eclipse::base-char-code-limit #x100)

;;; This is the exclusive bound on values produced by the function 
;;; char-code for ALL characters.  See base-char-code-limit.
(defconstant eclipse:CHAR-CODE-LIMIT #x110000)


(defconstant eclipse::digit-size 16)
(defconstant eclipse::digit-base (expt 2 eclipse::digit-size))
(defconstant eclipse::digit-mask (1- eclipse::digit-base))
(defconstant eclipse::fixnum-size
  #-machine-compile 29 ; #.(integer-length cl:most-positive-fixnum)
  #+machine-compile (- (* 2 eclipse::digit-size) 3))

(defconstant eclipse:MOST-POSITIVE-FIXNUM
  #.(1- (expt 2 29			;really eclipse::fixnum-size
	      )))
(defconstant eclipse:MOST-NEGATIVE-FIXNUM
  #.(- (expt 2 29			;really eclipse::fixnum-size
	     )))

;;; These are really FLT_MANT_DIG, DBL_MANT_DIG, FLT_MAX_EXP and
;;; DBL_MAX_EXP, respectively.  See top of arithmetic.lisp
(defconstant eclipse::float-signif 24)
(defconstant eclipse::double-signif 53)
(defconstant eclipse::float-exponent 128)
(defconstant eclipse::double-exponent 1024)

(defconstant eclipse:boole-1   #x1)	;include first arg
(defconstant eclipse:boole-2   #x2)	;include second arg
(defconstant eclipse:boole-c1  #x4)	;complement first arg
(defconstant eclipse:boole-c2  #x8)	;complement second arg
(defconstant eclipse::b-and #x10)	;and args
(defconstant eclipse::b-ior #x20)	;inclusive or args
(defconstant eclipse::b-xor #x40)	;exclusive or args
(defconstant eclipse::b-not #x80)	;complement result

(defconstant eclipse::boole-both 	(logior eclipse:boole-1 eclipse:boole-2))
(defconstant eclipse:boole-clr 		0)
(defconstant eclipse:boole-set 		eclipse::b-not)
(defconstant eclipse:boole-and 		(logior eclipse::boole-both eclipse::b-and))
(defconstant eclipse:boole-ior 		(logior eclipse::boole-both eclipse::b-ior))
(defconstant eclipse:boole-xor 		(logior eclipse::boole-both eclipse::b-xor))
(defconstant eclipse:boole-nand 	(logior eclipse:boole-and eclipse::b-not))
(defconstant eclipse:boole-nor 		(logior eclipse:boole-ior eclipse::b-not))
(defconstant eclipse:boole-eqv 		(logior eclipse:boole-xor eclipse::b-not))
(defconstant eclipse:boole-andc1 	(logior eclipse:boole-c1 eclipse:boole-2 eclipse::b-and))
(defconstant eclipse:boole-andc2 	(logior eclipse:boole-1 eclipse:boole-c2 eclipse::b-and))
(defconstant eclipse:boole-orc1 	(logior eclipse:boole-c1 eclipse:boole-2 eclipse::b-ior))
(defconstant eclipse:boole-orc2 	(logior eclipse:boole-1 eclipse:boole-c2 eclipse::b-ior))
(defconstant eclipse::boole-not 	eclipse:boole-c1)
