;;; In principle, there are three kinds of characters, all of which
;;; are implemented as immediate data:
;;; - ATTRIBUTED-CHAR: Can represent (and is a supertype of) all
;;;   supported character repertoires, including UNICODE (ISO10646-1),
;;;   and may have additional implementation defined attributes.  No
;;;   additional attributes are defined at this time. 
;;; - WIDE-CHAR: Can represent all supported character repertoires,
;;;   but no additional implementation defined attributes.  
;;; - BASE-CHAR: Can only represent the LATIN (aka ISO8859-1),
;;;   ASCII, and STANDARD-CHAR  repertoires, and not additional
;;;   implemtation defined attributes.  (The three types mentioned are
;;;   in increasing order of specificity.)
;;;
;;; The types WIDE-CHAR and ATTRIBUTED-CHAR form an exhaustive
;;; partition of the type EXTENDED-CHAR.  ATTRIBUTED-CHAR is not
;;; currently used.  
;;; 
;;; Note that character repertoires are type specifiers useable for
;;; discrimination in the same sense as, say, (INTEGER 2 32) -- they
;;; do not necessarilly have a distinct representation type.  In fact
;;; UPGRADED-ARRAY-ELEMENT-TYPE of any of these will always return
;;; either EXTENDED-CHAR, or BASE-CHAR.
;;;
;;; LATIN (and its subtypes,) defines a control-bit attribute, which
;;; is set for non-graphic characters.  
;;;
;;; ASCII (and its subtypes,) defines a meta-bit attribute, which
;;; indicates a character outside the usual ASCII range.  Note that
;;; this bit is not available for LATIN characters. 
;;;
;;; Both the control-bit and meta-bit are preserved and respected by
;;; all character and string functions, including char-int/code-char.
;;; They are NOT ignored by char-equal.  Setting these bits are
;;; usefull for creating characters not easily entered from the
;;; keyboard.  
;;;
;;; Whether LATIN, ASCII and other non-STANDARD-CHAR character
;;; repertoires are supported depends on the platform, and perhaps
;;; on such operating system parameters as LOCALE.  When they are
;;; supported, the characters print properly when output to streams.
;;; The types can still be used when not "supported", but external
;;; interactions (i.e. with files and other streams) are undefined.
;;; In addition, such functions as upper-case-p, alphanumeric-p, etc.
;;; may return incorrect values for unsupported character repertoires.

;;; NON-ASCII ISSUES:
;;; - Stream functions may need to translate wide-char to some
;;;   multi-byte c:char format.  It may be necessary to define UTF-8,
;;;   and UTF-7 as legal :element-type arguments.
;;; - Character ordering may not be sensibly defined for non-ASCII
;;;   characters, particularly LATIN.  Note, though, that it is not
;;;   meaningul to use ANSI string ordering functions for non-English
;;;   languages anyway.  (Either new functions must be written or we
;;;   need to define a :language argument for these.)

;;; A consequence of the meta-bit behavior is that the character with
;;; code 248 might be the non-graphic character #\Meta-x on some
;;; LOCALEs, and the Scandinavian letter that looks like a lower-case
;;; null-set symbol on others.  An EMACS-like text editor written in
;;; Lisp wouldn't work in all LOCALEs if it simply tested for the
;;; meta-bit.  Therefore, it would be wise to write such systems to
;;; both test for the meta-bit and also look for a two character
;;; sequence begining with #\Escape.  Note that the interpretation of
;;; whether the control bit is "set" depends on the LOCALE.

;;; ~c format should print chars with control-bit and not meta-bit set
;;; using ^X notation instead of control-X.  The #\ reader should
;;; recognize ^ as a shorthand for control-.  Chars with meta bit set
;;; should print with ~c using \ooo octal notation, as in C.  The #\
;;; reader should recognize \ as introducing an octal number.  [~c is
;;; defined by ANSI only as an implementation defined, abbreviated
;;; format.  Fortunately, the C printer will do exactly the right
;;; thing for us.]
;;;
;;; Note, however, that for ~:c format, a mnemonic name, if defined,
;;; should be used in preference to control- or meta- notation.  
;;;
;;; For ~:@c format, the control- notation should follow the character
;;; in parentheses for named characters.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 13.2 PREDICATES ON CHARACTERS                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun STANDARD-CHAR-P (char)
  (or (char<= #\space char #\~) (char= char #\newline)))

(defun GRAPHIC-CHAR-P (char)
  (test (ec:iswprint (character-int char))))
	
(defun ALPHA-CHAR-P (char)
  (test (ec:iswalpha (character-int char))))

(macrolet ((defpredicate (name predicate)
	     `(defun ,name (char)
		(test (,predicate (character-int char))))))
  (defpredicate UPPER-CASE-P ec:iswupper)
  (defpredicate LOWER-CASE-P ec:iswlower)
  ;; Might not be true for non-ASCII!!!
  (defpredicate BOTH-CASE-P ec:iswalpha))

(defun DIGIT-CHAR-P (char &optional (radix 10)
		     &aux (code (char-int char)))
  (decf code
	(cond ((upper-case-p char) #.(cl:- (cl:char-code #\A) 10))
	      ((lower-case-p char) #.(cl:- (cl:char-code #\a) 10))
	      (t #.(cl:char-code #\0))))
  (when (< -1 code radix) code))
  
  
(defun ALPHANUMERICP (char)
  (test (ec:iswalnum (character-int char))))

;; Should declare types!!!
(defun monotonic (char rest test key
		  &aux (item (funcall key char)))
  (dolist (next rest t)
    ;; This local variable should not be necessary, but our compiler
    ;; has a bug in it!!!
    (let ((old item))
      (unless (funcall test old
		       (setf item (funcall key next)))
	(return nil)))))

(defun char-key (char) (char-int (char-downcase char)))

(macrolet ((defpredicate (name test key)
	     `(defun ,name (character &rest more-characters)
		(monotonic character more-characters
			   (function ,test) (function ,key))))
	   (defstrict (name test)
	     `(defpredicate ,name ,test char-int))
	   (defkeyed (name test)
	     `(defpredicate ,name ,test char-key)))
  (defstrict CHAR= =)
  (defstrict CHAR< <)
  (defstrict CHAR> >)
  (defstrict CHAR<= <=)
  (defstrict CHAR>= >=)
  (defkeyed CHAR-EQUAL =)
  (defkeyed CHAR-LESSP <)
  (defkeyed CHAR-GREATERP >)
  (defkeyed CHAR-NOT-GREATERP <=)
  (defkeyed CHAR-NOT-LESSP >=))

(defun different (args key)
  (do ((remaining args (rest remaining))
       current)
      ((null remaining) t)
    (setf current (funcall key (car remaining))
	  (car remaining)  current)
    (do ((previous args (cdr previous)))
	((eq previous remaining))
      (when (= (car previous) current)
	(return-from different nil)))))

(defun CHAR/= (&rest chars) (different chars #'char-int))
(defun CHAR-NOT-EQUAL (&rest chars) (different chars #'char-key))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 13.3 CHARACTERS CONSTRUCTION AND SELECTION                   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Note that char-int/code-char are inverses, 
;;; NOT char-code/code-char.

(defun CHAR-INT (char) (int-integer (character-int char)))
(defun CODE-CHAR (code)
  (when (< code char-code-limit)
    (int-character (fixnum-int code))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 13.4 CHARACTERS CONVERSIONS                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun CHARACTER (object)
  (etypecase object
    (character object)
    ((or string symbol)
     (let ((string (string object)))
       (if (= (length string) 1)
	   (char string 0)
	 (error 'type-error
		:datum object
		:expected-type 'character-designator))))))

(macrolet ((defcase (name predicate converter)
	     `(defun ,name (char &aux (code (character-wint char)))
		(declare (type ec:wint_t code))
		(if (test (,predicate code))
		    (wint-character (,converter code))
		  char))))
  (defcase CHAR-UPCASE ec:iswlower ec:towupper)
  (defcase CHAR-DOWNCASE ec:iswupper ec:towlower))
	   
(defun DIGIT-CHAR (weight &optional (radix 10))
  (when (< -1 weight radix 37)
    (code-char (+ weight
		  (if (< weight 10)
		      #.(cl:char-code #\0)
		      #.(cl:- (cl:char-code #\A) 10))))))

;; Order is important in that characters which are the same will print
;; using the name which appears earlier in the listing.
(defparameter *character-names*
    '((#\Newline . "Newline")
      (#\Space . "Space")
      (#\Tab . "Tab")
      (#\Page . "Page")
      (#\Rubout . "Rubout")
      (#\Return . "Return")
      (#\Linefeed . "Linefeed")
      (#\Backspace . "Backspace")
      (#\Escape . "Escape")
      (#\Null . "Null")
      (#\Bell . "Bell")
      (#\Rubout . "Delete")))

(defun CHAR-NAME (char)
  (cdr (assoc char *character-names* :test #'char=)))

(defun NAME-CHAR (name)
  (car (rassoc (string name) *character-names* :test #'string-equal)))

;;; CHARACTER ATTRIBUTES
;;; Meta characters are those above 127 (i.e. bit 7 is on)
;;; Control character are those below 32 (i.e. bits 5 and 6 are OFF).

(defun CHAR-BIT (char &key control meta &aux (code (char-int char)))
  (when (< code BASE-CHAR-CODE-LIMIT)
    (or (and control  (< (logand #x7f code) #.(cl:char-int #\space)))
	(and meta (> code #.(cl:char-int #\Rubout))))))

;;; The meta bit (bit 7) can be turned on and off in straightforward
;;; way.  Control characters are made into printing character (i.e.
;;; turning "off the control bit") by turning ON bit 6. Control
;;; characters are created by turning off bits 6 and 5.

;;; It is an error to try to unset the control bit of a printing
;;; character, or to set the control bit of a character
;;; outside the range [#x40 #x5F] (i.e. the capital ASCII letters and
;;; or one of "@[\]^_").

(defun SET-CHAR-BIT (char &key (control :default) (meta :default))
  (let ((code (char-int char)))
    (when (< code BASE-CHAR-CODE-LIMIT)
      (case control
	((nil) (when (< (logand #x7f code) #.(cl:char-int #\space))
		 (setq code (logior #x40 code))))
	(:default nil)
	(t (setq code (logand #x9f code))))
      (case meta
	((nil) (setq code (logand #x7f code)))
	(:default nil)
	(t (setq code (logior #x80 code)))))
    (code-char code)))

(defun CHAR-CODE (char)
  (let ((code (char-int char)))
    (when (< code base-char-code-limit)
      (setq code (logand #x7f code))
      ;; Should we really strip off the control bit?
      (when (< code #.(cl:char-int #\space))
	(setq code (logior #x40 code))))
    code))
