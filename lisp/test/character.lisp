;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 13.1 CHARACTER ATTRIBUTES                                        ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CHAR-CODE-LIMIT
(deftest char-code-limit
    (> char-code-limit
       (max 96
	    (char-code #\z)
	    (char-code #\Z)
	    (char-code #\9)
	    #+not-yet(char-code (or (name-char rubout) #\newline))))
  t)

(deftest char-code-limit2
    (characterp (code-char (1- char-code-limit)))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 13.2 PREDICATES ON CHARACTERS                                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; STANDARD-CHAR-P
(deftest standard (standard-char-p #\a) t)
(deftest standard-space (standard-char-p #\space) t)
(deftest standard-tilde (standard-char-p #\~) t)
(deftest standard-bell (standard-char-p #\bell) nil)
(deftest standard-newline (standard-char-p #\newline) t)
(deftest standard-linefeed (unless (eq #\newline #\linefeed)
			     (standard-char-p #\linefeed)) nil)
(deftest standard-return (standard-char-p #\return) nil)
(deftest standard-tab (standard-char-p #\tab) nil)

;;; GRAPHIC-CHAR-P
(deftest graphic (graphic-char-p #\G) t)
(deftest graphic-hash (graphic-char-p #\#) t)
(deftest graphic-space (graphic-char-p #\space) t)
(deftest graphic-newline (graphic-char-p #\newline) nil)
(deftest graphic-linefeed (graphic-char-p #\linefeed) nil)
(deftest graphic-tab (graphic-char-p #\tab) nil)

;;; ALPHA-CHAR-P
(deftest alpha (alpha-char-p #\a) t)
(deftest alpha-not (alpha-char-p #\5) nil)
(deftest alpha-space (alpha-char-p #\space) nil)

;;; UPPER-CASE-P
(deftest upper-big (upper-case-p #\A) t)
(deftest upper-small (upper-case-p #\a) nil)
(deftest upper-number (upper-case-p #\5) nil)
(deftest upper-space (upper-case-p #\space) nil)
(deftest upper-tab (upper-case-p #\tab) nil)
;;; LOWER-CASE-P
(deftest lower-big (lower-case-p #\A) nil)
(deftest lower-small (lower-case-p #\a) t)
(deftest lower-number (lower-case-p #\5) nil)
(deftest lower-space (lower-case-p #\space) nil)
(deftest lower-tab (lower-case-p #\tab) nil)
;;; BOTH-CASE-P
(deftest both-big (both-case-p #\A) t)
(deftest both-small (both-case-p #\a) t)
(deftest both-number (both-case-p #\5) nil)
(deftest both-space (both-case-p #\space) nil)
(deftest both-tab (both-case-p #\tab) nil)

;;; DIGIT-CHAR-P
(deftest digit-dig (digit-char-p #\0) 0)
(deftest digit-bad-dig (digit-char-p #\8 8) nil)
(deftest digit-bad-big-alpha (digit-char-p #\A) nil)
(deftest digit-bad-alpha (digit-char-p #\a) nil)
(deftest digit-alpha (digit-char-p #\a 16) 10)
(deftest digit-big-alpha (digit-char-p #\A 16) 10)
;;; ALPHANUMERIC-P
(deftest alphanum-alpha (alphanumericp #\Z) t)
(deftest alphanum-num (alphanumericp #\0) t)
(deftest alphanum-newline (alphanumericp #\newline) nil)
(deftest alphanum-hash (alphanumericp #\#) nil)
(deftest alphanum-space (alphanumericp #\space) nil)

;;; CHAR= CHAR/= CHAR< CHAR> CHAR<= CHAR>=
(deftest char= (char= #\d #\d) t)
(deftest char/= (char/= #\d #\d) nil)
(deftest char=diff (char= #\d #\x) nil)
(deftest char/=diff (char/= #\d #\x) t)
(deftest char=case (char= #\d #\D) nil)
(deftest char/=case (char/= #\d #\D) t)
(deftest char=many (char= #\d #\d #\d #\d) t)
(deftest char/=many (char/= #\d #\d #\d #\d) nil)
(deftest char=many-diff (char= #\d #\d #\x #\d) nil)
(deftest char/=many-diff (char/= #\d #\d #\x #\d) nil)
(deftest char=many-diff2 (char= #\d #\y #\x #\c) nil)
(deftest char/=many-diff2 (char/= #\d #\y #\x #\c) t)
(deftest char=3 (char= #\d #\c #\d) nil)
(deftest char/=3 (char/= #\d #\c #\d) nil)
(deftest char< (char< #\d #\x) t)
(deftest char<= (char<= #\d #\x) t)
(deftest char<same (char< #\d #\d) nil)
(deftest char<=same (char<= #\d #\d) t)
(deftest char<many (char< #\a #\e #\y #\z) t)
(deftest char<=many (char<= #\a #\e #\y #\z) t)
(deftest char<many-dup (char< #\a #\e #\e #\y) nil)
(deftest char<=many-dup (char<= #\a #\e #\e #\y) t)
(deftest char> (char> #\e #\d) t)
(deftest char>= (char>= #\e #\d) t)
(deftest char>many (char> #\d #\c #\b #\a) t)
(deftest char>=many (char>= #\d #\c #\b #\a) t)
(deftest char>many-dup (char> #\d #\d #\c #\a) nil)
(deftest char>=many-dup (char>= #\d #\d #\c #\a) t)
(deftest char>many2 (char> #\e #\d #\b #\c #\a) nil)
(deftest char>=many2 (char>= #\e #\d #\b #\c #\a) nil)

;;; CHAR-EQUAL CHAR-NOT-EQUAL CHAR-LESSP CHAR-GREATERP
;;; CHAR-NOT-GREATERP CHAR-NOT-LESSP 
(deftest char-equal (char-equal #\d #\d) t)
(deftest char-not-equal (char-not-equal #\d #\d) nil)
(deftest char-equal-diff (char-equal #\d #\x) nil)
(deftest char-not-equal-diff (char-not-equal #\d #\x) t)
(deftest char-equal-case (char-equal #\d #\D) t)
(deftest char-not-equal-case (char-not-equal #\d #\D) nil)
(deftest char-equal-many (char-equal #\d #\d #\D #\d) t)
(deftest char-not-equal-many (char-not-equal #\d #\d #\d #\d) nil)
(deftest char-equal-many-diff (char-equal #\d #\d #\x #\d) nil)
(deftest char-not-equal-many-diff (char-not-equal #\d #\d #\x #\d) nil)
(deftest char-equal-many-diff2 (char-equal #\d #\y #\x #\c) nil)
(deftest char-not-equal-many-diff2 (char-not-equal #\d #\y #\x #\c) t)
(deftest char-equal3 (char-equal #\d #\c #\d) nil)
(deftest char-not-equal3 (char-not-equal #\d #\c #\d) nil)
(deftest char-lessp (char-lessp #\d #\x) t)
(deftest char-not-greaterp (char-not-greaterp #\d #\X) t)
(deftest char-lessp-same (char-lessp #\d #\d) nil)
(deftest char-not-greaterp-same (char-not-greaterp #\d #\D) t)
(deftest char-lessp-many (char-lessp #\a #\e #\y #\z) t)
(deftest char-not-greaterp-many (char-not-greaterp #\a #\e #\y #\z) t)
(deftest char-lessp-many-dup (char-lessp #\a #\E #\e #\y) nil)
(deftest char-not-greaterp-many-dup (char-not-greaterp #\a #\E #\e #\y) t)
(deftest char-greaterp (char-greaterp #\e #\D) t)
(deftest char-not-lessp (char-not-lessp #\e #\D) t)
(deftest char-greaterp-many (char-greaterp #\d #\C #\b #\a) t)
(deftest char-not-lessp-many (char-not-lessp #\d #\c #\B #\a) t)
(deftest char-greaterp-many-dup (char-greaterp #\d #\d #\c #\a) nil)
(deftest char-not-lessp-many-dup (char-not-lessp #\d #\D #\c #\a) t)
(deftest char-greaterp-many2 (char-greaterp #\e #\d #\b #\c #\a) nil)
(deftest char-not-lessp-many2 (char-not-lessp #\e #\d #\b #\c #\a) nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 13.3 CHARACTER CONSRUCTION AND SELECTION                         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CHAR-CODE
(deftest char-code1 (char-code #\$) 36)   ;Assumes ASCII
(deftest char-code2 (char-code #\a) 97) ;Assumes ASCII
;;; CODE-CHAR
(deftest code-char1 (code-char 65.) #\A) ;Assumes ASCII  
(deftest code-char2 (code-char (char-code #\space)) #\space)
;; Arguable...
#-(and cmu (not eclipse))
(deftest code-char-bad (code-char char-code-limit) nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 13.4 CHARACTER CONVERSIONS                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CHARACTER
(deftest character-char (character #\a) #\a)
(deftest character-string (character "a") #\a)
(deftest character-symbol (character 'a) #\A)

;;; CHAR-UPCASE
(deftest upcase-small (char-upcase #\a) #\A)
(deftest upcase-big (char-upcase #\A) #\A)
(deftest upcase-num (char-upcase #\9) #\9)
(deftest upcase-other (char-upcase #\@) #\@)
;;; CHAR-DOWNCASE
(deftest downcase-small (char-downcase #\a) #\a)
(deftest downcase-big (char-downcase #\A) #\a)
(deftest downcase-num (char-downcase #\9) #\9)
(deftest downcase-other (char-downcase #\@) #\@)

;;; DIGIT-CHAR
(deftest digit0 (digit-char 0) #\0)
(deftest digit0-16 (digit-char 0 16) #\0)
(deftest digit10-11 (digit-char 10 11) #\A)
(deftest digit10 (digit-char 10) nil)
(deftest digit7 (digit-char 7) #\7)
(deftest digit13 (digit-char 13) nil)
(deftest digit13-16 (digit-char 13 16) #\D)
(deftest digit3-2 (digit-char 3 2) nil)
(deftest digit1-2 (digit-char 1 2) #\1)
(deftest digit35-36 (digit-char 35 36) #\Z)

;;; CHAR-INT
(deftest char-int
    (typep (char-int #\b) `(integer ,(char-int #\a) ,(char-int #\c))) t) 

;;; CHAR-NAME
(deftest name-space (char-name #\ ) "Space")
(deftest name-space2 (char-name #\space) "Space")
(deftest name-page (char-name #\page) "Page")
(deftest name-nil (char-name #\a) nil)
;;; NAME-CHAR
(deftest name-char-symbol (name-char 'space) #\space)
(deftest name-char-string (name-char "space") #\space)
(deftest name-char-string2 (name-char "SPACE") #\space)
(deftest name-char-nil (name-char "blue") nil)
