(defun read-error (stream format &rest args)
  (error 'reader-error :stream stream
	 :format-control format
	 :format-arguments args))

(defun illegal-character (stream c &optional (qualifier "Illegal"))
  (read-error stream "~s character ~c." qualifier c))
    
(defvar *readtable*)
(defvar *PRESERVE-WHITESPACE* nil)

(defun whitespace-char-p (char)
  (eq (get-character-syntax char *readtable*) :whitespace))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                       READER                                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; These initial values are defined by ANSI.
;;; The values used by with-standard-io-syntax are the same.
(defvar *READ-BASE* 10)
(defvar *READ-DEFAULT-FLOAT-FORMAT* 'single-float)
(defvar *READ-EVAL* t)
(defvar *READ-SUPPRESS* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 22.1.1 WHAT THE READ FUNCTION ACCEPTS

(defvar *attributes* (make-array 64 :adjustable t :fill-pointer 0))
(defvar *token* (make-array 64 :element-type 'character
			    :adjustable t
			    :fill-pointer 0))
(defvar *base-token* (make-array 64 :element-type 'base-char
			    :adjustable t
			    :fill-pointer 0))
(defun get-token-buffer (stream)
  (if (eq (stream-element-type stream) 'base-char)
      *base-token*
      *token*))

;;; How a token consisting of a single dot should be read.  Useful to
;;; avoid interning they symbol "." while reading dotted lists.  Nil
;;; indicates that the sysmbol should just be interned in the usual way.
(defparameter *dot-token* nil)

;;; See algorithm in Steele, p.513.
;;; We convert case as we go along, but we can't do :invert case until
;;; we get the whole token, and then we need to know which characters
;;; were escaped, so we have to keep track of these.
;;; N.B. The :case rules apply to any package prefix as well as
;;; to the symbol.  This allows package prefixing of symbols in
;;; packages which have lowercase names.
(defun READ-TOKEN (s &key (eof-error-p t) eof-value
			  (case (readtable-case *readtable*))
			  (token (get-token-buffer s))
			  (reset? t)
			  (unescaped *attributes*)
			  (parse (readtable-parse *readtable*))
			  (dot-token *dot-token*)
		   &aux (some-upper-case-p nil)
			(some-lower-case-p nil)
			x y (index 0) 
			package-end (symbol-start 0) exponent
			ratio sign decimal digit-p non-digit-p
			non-decimal-digit-p
			(sometimes-escape (not (eq case :preserve)))
			(never-escape (and (eq case :unescaped-invert)
					   (setq case :invert))))
  (flet ((add-char (char replaceable-p)
	   (when (or never-escape (and sometimes-escape replaceable-p))
	     (cond ((lower-case-p char)
		    (case case
		      (:upcase (setf char (char-upcase char)))
		      (:invert (setf some-lower-case-p t))))
		   ((upper-case-p char)
		    (case case
		      (:downcase (setf char (char-downcase char)))
		      (:invert (setf some-upper-case-p t))))))
	   (when (and replaceable-p parse)
	     (case char
	       (#\: (unless package-end (setq package-end index))
		    (setq symbol-start (1+ index)))
	       (#\/ (if (or ratio (null digit-p))
			(setq replaceable-p nil)
		      (setq ratio index
			    digit-p nil)))
	       (#\. (if decimal (setq replaceable-p nil)
		      (setq decimal index)))
	       ((#\+ #\-) (if sign
			      (setq replaceable-p nil)
			    (setq sign index)))
	       (t (let* ((marker? (and (not exponent)
				       (find char "ESFDL")))
			 (digit? (unless non-digit-p
				   (digit-char-p char *read-base*))))
		    (when marker? (setf exponent index
					sign nil))
		    (unless digit-p (setq digit-p digit?))
		    (unless (or non-digit-p digit?)
		      (setq non-digit-p t))
		    (unless (or marker? non-decimal-digit-p
				(if (or non-digit-p
					(/= *read-base* 10))
				    (digit-char-p char)
				  digit?))
		      (setq non-decimal-digit-p t))))))
	   (unless replaceable-p (setq non-digit-p t
				       non-decimal-digit-p t))
	   (incf index)
	   (vector-push-extend char token)
	   (vector-push-extend replaceable-p unescaped)))
    (tagbody
     STEP-1
       (when reset?
	 (setf (fill-pointer token) 0)
	 (setf (fill-pointer unescaped) 0))  
     STEP-1a
      (when (eq (setf x (read-char s nil 'eof)) 'eof)
	(if eof-error-p (error 'end-of-file :stream s)
	  (return-from read-token eof-value)))
      (ecase (get-character-syntax x)
	(:constituent (add-char x t)
		      (go step-8))
	(:whitespace (go step-1a))
	((:terminating-macro :non-terminating-macro) 
	 (let ((v (multiple-value-list
		      (funcall (get-macro-character x)
			       s x))))
	   (when v (return-from read-token (car v)))
	   (go step-1)))
	(:single-escape (add-char (read-char s) nil)
			(go step-8))
	(:illegal (illegal-character s x))
	(:multiple-escape (go step-9)))
     STEP-8 
      (when (eq (setf y (read-char s nil 'eof)) 'eof)
	(go step-10))
      (ecase (get-character-syntax y)
	((:constituent :non-terminating-macro)
	 (add-char y t)
	 (go step-8))
	(:whitespace (when *preserve-whitespace*
		       (unread-char y s))
		     (go step-10))
	(:single-escape (let ((p (read-char s)))
			  (add-char p nil))
			(go step-8))
	(:multiple-escape (go step-9))
	(:illegal (illegal-character s y))
	(:terminating-macro (unread-char y s)
			    (go step-10)))
     STEP-9 
      (setq non-digit-p t non-decimal-digit-p t)
      (ecase (get-character-syntax (setf y (read-char s)))
	((:constituent :terminating-macro :non-terminating-macro
	  :whitespace)
	 (add-char y nil)
	 (go step-9))
	(:single-escape (add-char (read-char s) nil)
			(go step-9))
	(:multiple-escape (go step-8))
	(:illegal (illegal-character s y)))
     STEP-10 
      ;; :invert case
      (flet ((change-case (func)
	       (dotimes (p (length token))
		 (when (or never-escape (elt unescaped p))
		   (setf (elt token p)
		     (funcall func (elt token p)))))))
	(when (and some-upper-case-p (not some-lower-case-p))
	  (change-case #'char-downcase))
	(when (and some-lower-case-p (not some-upper-case-p))
	  (change-case #'char-upcase)))
      (return-from read-token
	(let* ((index (1- index))
	       (potnum? (unless (or *read-suppress* package-end
				    (eql index sign))
			  (and parse digit-p 
			       (every #'identity unescaped)))))
	  (cond ((not parse) (copy-seq token))
		(*read-suppress* nil)
		((= symbol-start 1) ;KEYWORD
		 (make-keyword (subseq token 1)))
		(package-end	;PACKAGE QUALIFIED
		 (let ((symbol (subseq token symbol-start))
		       (package (subseq token 0 package-end)))
		   (case (- symbol-start package-end)
		     (1 (or (find-symbol symbol package)
			    (when symbol
			      (error "~s is not exported from package ~s."
				     symbol package))))
		     (2 (intern symbol package))
		     (t (read-error s "Too many package markers in ~s."
			       token)))))
		;; RATIO
		((and potnum? ratio (not (or decimal non-digit-p)))
		 (/ (parse-integer token :end ratio :radix *read-base*)
		    (parse-integer token :start (1+ ratio) :radix
				   *read-base*)))
		;; INTEGER
		((and potnum? (null non-digit-p)
		      (or (null decimal)
			  (and (= decimal index)
			       (not non-decimal-digit-p))))
		 (parse-integer token :end decimal
				:radix (if decimal 10 *read-base*)))
		;; FLOAT
		((and potnum? (not non-decimal-digit-p)
		      (or decimal exponent))
		 (parse-float token decimal exponent))
		;; DOT IN DOTTED LIST: Don't intern.
		((and decimal dot-token (zerop index))
		 dot-token)
		;; LOCAL SYMBOL
		(t (intern (subseq token symbol-start)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 22.1.2 PARSING NUMBERS AND SYMBOLS

;;; IWBNI this used nongeneric arithmetic.
(defun parse-float (string decimal exponent)
  (let* ((base (case (and exponent (char string exponent))
		 ((#\D #\L) 10.0d0)
		 ((#\S #\F) 10.0f0)
		 (t (coerce 10 *read-default-float-format*))))
	 (start 0)
	 (end (or decimal exponent))
	 (fraction-start (and decimal (1+ decimal)))
	 (sign (case (char string 0)
		 (#\+ (incf start) 1)
		 (#\- (incf start) -1)
		 (t 1))))
    (* (+ (if (< start end)
	      (parse-integer string :start start :end end)
	    0)
	  (if (and decimal
		   (or (null exponent) (> exponent fraction-start)))
	      (/ (parse-integer string :start fraction-start
				:end exponent)
		 (expt base (- (or exponent (length string))
			       fraction-start)))
	    0))
       (if exponent
	   (expt base (parse-integer string :start (1+ exponent)))
	 1)
       sign)))
		     
	  
(defun PARSE-INTEGER (string &key (start 0) end (radix 10) junk-allowed)
  (unless end (setq end (length string)))
  (let ((index (do ((i start (1+ i)))
		   ((= i end)
		    (if junk-allowed
			(return-from parse-integer (values nil end))
		      (read-error string "No non-whitespace characters in number.")))
		 (declare (fixnum i))
		 (unless (whitespace-char-p (char string i)) (return i))))
	(minusp nil)
	(found-digit nil)
	(result 0))
    (declare (fixnum index))
    (let ((char (char string index)))
      (cond ((char= char #\-)
	     (setq minusp t)
	     (incf index))
	    ((char= char #\+)
	     (incf index))))
    (loop
      (when (= index end) (return nil))
      (let* ((char (char string index))
	     (weight (digit-char-p char radix)))
	(cond (weight
	       (setq result (+ weight (* result radix))
		     found-digit t))
	      (junk-allowed (return nil))
	      ((whitespace-char-p char)
	       (do ((jndex (1+ index) (1+ jndex)))
		   ((= jndex end))
		 (declare (fixnum jndex))
		 (unless (whitespace-char-p (char string jndex))
		   (illegal-character string (char string jndex)
				      "Non-numeric")))
	       (return nil))
	      (t (illegal-character string char "Non-numeric"))))
      (incf index))
    (values
     (if found-digit
	 (if minusp (- result) result)
       (if junk-allowed
	   nil
	 (read-error string "No non-whitespace characters in number.")))
     index)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 22.1.3 MACRO CHARACTERS

(defun READ-LIST (terminator stream dot-ok-p
		  &aux (char (peek-char t stream t nil t)))
  (cond ((eql terminator char) (read-char stream) nil)
	((member (get-character-syntax char)
		 `(:terminating-macro :non-terminating-macro))
	 (read-char stream)
	 (let ((v (multiple-value-list
		      (funcall (get-macro-character char)
				    stream char))))
	   (if v (cons (car v)
		       (read-list terminator stream dot-ok-p))
	     (read-list terminator stream dot-ok-p))))
	(t (let ((next (if dot-ok-p
			   (let ((*dot-token* '.dot.))
			     (read stream t nil t))
			   (read stream t nil t))))
	     (if (eq next '.dot.)
		 (prog1 (read stream t nil t)
		   (when (read-list terminator stream nil)
		     (read-error stream "Too many items following dot in list.")))
		 (cons next
		       (read-list terminator stream dot-ok-p)))))))

(defun LIST-READER (s c) (declare (ignore c)) (read-list #\) s t))

(defun UNMATCHED-READER (s c)
  (read-error s "Unmatched ~s" c))

(defun QUOTE-READER (s c)
  (declare (ignore c))
  (list 'quote (read s t nil t)))

(defun LINE-COMMENT-READER (s c)
  (declare (ignore c))
  (peek-char #\newline s nil nil t)
  (values))

(defparameter *STRING-ACCUMULATOR*
    (make-array 120 :element-type 'character :adjustable t :fill-pointer 0))
(defparameter *BASE-STRING-ACCUMULATOR*
    (make-array 120 :element-type 'base-char :adjustable t :fill-pointer 0))
(defun get-string-accumulator (stream)
  (if (eq (stream-element-type stream) 'base-char)
      *base-string-accumulator*
      *string-accumulator*))

(defun STRING-READER (s quote-char)
  (let ((accumulator (get-string-accumulator s)))
    (setf (fill-pointer accumulator) 0)
    (do (c)
	((eql (setf c (read-char s t nil t)) quote-char)
	 (copy-seq accumulator))
      (vector-push-extend    
       (if (eq (get-character-syntax c) :single-escape)
	   (read-char s t nil t)
	   c)
       accumulator))))

;;; It is not clear if an unrecognized dispatch character should
;;; signal an error when *read-suppress* is true.  If it doesn't
;;; signal an error, what SHOULD it do?

(defun DISPATCHER (s char)
  (do* ((c (read-char s) (read-char s))
	(sum nil (+ n (* (or sum 0) 10)))
	(n (digit-char-p c) (digit-char-p c)))
      ((null n) (funcall (or (get-dispatch-macro-character
			      char c *readtable*)
			     (read-error s
 "No dispatch function associated with dispatch character ~s, subchar ~s."
			      char c))
			 s c sum))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 22.1.4 STANDARD DIPATCHING MACRO CHARACTER SYNTAX

;;; control-, meta- and control-meta- prefixes are respected, as are
;;; ^, %, and %^, which are synonyms.  (Note that different ordering of
;;; control-meta- vs %^.)  Note that case is not preserved for control
;;; characters. 

;;; Note that control- (and ^) prefixes do not respect case.
(defun CHARACTER-READER (s char font)
  (declare (ignore char))
  (let ((c (read-char s t nil t))
        (c2 (peek-char nil s nil nil t)))
    (when (and c2 (eq :constituent (get-character-syntax c2)))
      ;; Named character.
      (setf (fill-pointer *string-accumulator*) 0)
      (vector-push-extend c *string-accumulator*)
      (setf c (read-token s :token *string-accumulator*
			  :reset? nil :parse nil
			  :case :preserve)))
    (unless *read-suppress*
      (when font (warn "Ignoring font ~s designation for ~s." font c))
      (when (stringp c)
	(let* ((name c)
	       (end (length name))
	       (meta-p1 (leading-string= "%" name :end end))
	       (cstart (or meta-p1 0))
	       (control-p (or (leading-string= "^" name :start cstart
					       :end end)
			      (leading-string-equal "CONTROL-" name
						    :start cstart
						    :end end)))
	       (meta-p (or meta-p1
			   (leading-string-equal "META-" name
					     :start (or control-p 0)
					     :end end)))
	       (stripped-name (if (or meta-p control-p)
				  (subseq name (max (or meta-p 0)
						    (or control-p 0))
					  end)
				name)))
	  (setf c
	    (set-char-bit
	     (or (name-char stripped-name)
		 (when (= (length stripped-name) 1)
		   (char stripped-name 0))
		 ;; Should we try to parse integer here?
		 (read-error s "Unrecognized character name ~s."
			     stripped-name))
	     :control (or control-p :default) :meta (or meta-p :default)))))
      c)))

(defun BAD-READER-PARAMETER (s bad c)
  (unless (or *read-suppress* (not bad))
    (read-error s "Unrecognized parameter ~d for ~s dispatch macro." bad c)))

(defun FUNCTION-READER (s c bad)
  (bad-reader-parameter s bad c)
  (list 'function (read s t nil t)))

(defun VECTOR-READER (s c len)
  (declare (ignore c))
  (let* ((items (read-delimited-list #\) s t))
	 (item-length (length items))
	 (length (or len item-length)))
    (unless *read-suppress*
      (do ((n item-length (1+ n)) last)
	  ((>= n length) (make-array length
				     :initial-contents items))
	;; Only get last when needed, so #0() works.
	(unless last (setf last (car (last items))))
	(setf items (nconc items (list last)))))))

;;; Errors should be reader-errors!!!  We can do this by trapping
;;; catching errors and resignalling.
(defun BIT-VECTOR-READER (s c n)
  (let ((string
	 (case (peek-char nil s nil nil t)
	   ((#\0 #\1) (read-token s :eof-error-p nil :eof-value ""
				  :parse nil))
	   (t ""))))
    (unless *read-suppress*
      (let* ((nbits (length string))
	     (length (or n nbits))
	     (v (if (or (and (zerop nbits) (plusp length))
			(< length nbits))
		    (read-error s "Incorrect number of bits in \"#~d~c~a\"."
				n c string)
		    (make-array length :element-type 'bit))))
	(dotimes (i nbits
		    (if (< i length)
			(do ((bit (sbit v (1- i)))
			     (i i (1+ i)))
			    ((= i length) v)
			  (setf (sbit v i) bit))
			v))
	  (let ((weight (digit-char-p (char string i) 2)))
	    (if weight
		(setf (sbit v i) weight)
		(read-error s "Non bit character in \"#~c~a\"." c string))))))))

(defun UNINTERNED-READER (s c bad)
  (bad-reader-parameter s bad c)
  (let ((token (read-token s :token (get-string-accumulator s) :parse nil)))
    (unless *read-suppress*
      (make-symbol (copy-seq token)))))

(defun BINARY-READER (s c bad)
  (bad-reader-parameter s bad c)
  (radix-reader s c 2))

(defun OCTAL-READER (s c bad)
  (bad-reader-parameter s bad c)
  (radix-reader s c 8))

(defun HEX-READER (s c bad)
  (bad-reader-parameter s bad c)
  (radix-reader s c 16))

(defun RADIX-READER (s c radix)
  (declare (ignore c))
  (let* ((*read-base* (if *read-suppress* *read-base* radix))
	 (value (read s t nil t)))
    (unless *read-suppress*
      (the rational value))))

(defun COMPLEX-READER (s c bad)
  (bad-reader-parameter s bad c)
  (let ((args (read s t nil t)))
    (unless *read-suppress* (apply #'complex args))))

(defun ARRAY-READER (s c rank)
  (let ((contents (read s t nil t)))
    (unless *read-suppress*
      (make-array
       (do ((d 0 (1+ d))
	    (row contents (car row))
	    (dimensions nil (cons (length row) dimensions)))
	   ((= d (or rank
		     (read-error s "No rank supplied to ~s." c)))
	    (nreverse dimensions)))
       :initial-contents contents))))

(defun STRUCTURE-READER (s c bad)
  (bad-reader-parameter s bad c)
  (let ((data (read s t nil t)))
    (unless *read-suppress*
      (destructuring-bind (name . args) data
	(apply (make-standard-structure-constructor-name name)
	       (do ((pairs args (cddr pairs)))
		   ((endp pairs) args)
		 (setf (car pairs)
		   (make-keyword (car pairs)))))))))

(defun PATHNAME-READER (s c bad)
  (bad-reader-parameter s bad c)
  (let ((token (read s t nil t)))
    (unless *read-suppress*
      (parse-namestring token))))


(defun EVAL-FEATURE (feature)
  (declare (special *features*))
  (if (atom feature)
      (member feature *features*)
    (destructuring-bind (key . rest) feature
      (ecase key
	(:not (not (apply #'eval-feature rest)))
	(:and (every #'eval-feature rest))
	(:or (some #'eval-feature rest))))))

(defun FEATURE-READER (s c bad &optional reverse-p)
  (bad-reader-parameter s bad c)
  (if (xor (eval-feature (let ((*package* (find-package :keyword)))
			   (read s t nil t)))
	   reverse-p)
      (read s t nil t)
      (let ((*read-suppress* t)) (read s t nil t) (values))))

(defun NOT-FEATURE-READER (s c bad)
  (feature-reader s c bad t))

(defun BLOCK-COMMENT-READER (s c bad)
  (bad-reader-parameter s bad c)
  (do ((level 1))
      ((zerop level) (values))
    (case (read-char s t nil t)
      (#\| (when (eql (peek-char nil s t nil t) #\#)
	     (read-char s t nil t)
	     (decf level)))
      (#\# (when (eql (peek-char nil s t nil t) #\|)
	     (read-char s t nil t)
	     (incf level))))))

(defun EVAL-READER (s c bad)
  (declare (notinline eval))
  (bad-reader-parameter s bad c)
  (let ((form (read s t nil t)))
    (unless *read-suppress*
      (if *read-eval*
	  (funcall (symbol-function 'eval) form)
	  (read-error
	   s "Attempt to use ~s reader with null *read-eval*." c)))))


;;; CIRCULAR REFERENCES.  Based on CMUCL algorithm, but ours works.
;;; Alist for #=. Used to keep track of objects with labels assigned that have
;;; been completly read.  Entry is (integer-tag gensym-tag value).
(defparameter *reader-labels* nil)

;;; Holds objects already seen by CIRCLE-SUBST.
(defparameter *circular-reference-table* "No current references.")

;; This function is kind of like to NSUBLIS, but checks for circularities and
;; substitutes in arrays and structures as well as lists.  The first arg is an
;; alist of the things to be replaced assoc'd with the things to replace them.
(defun circle-subst (old-new-alist tree)
  (cond ((not (typep tree '(or cons (array t) structure-object)))
	 (let ((entry (find tree old-new-alist :key #'second)))
	   (if entry (third entry) tree)))
	((null (gethash tree *circular-reference-table*))
	 (setf (gethash tree *circular-reference-table*) t)
	 (typecase tree
	   (structure-object
	    (let ((class (class-of tree)))
	      (unless (class-finalized-p class)
		(finalize-inheritance class))
	      (dotimes (i (length (class-slots class)))
		(let* ((old (structref tree i))
		       (new (circle-subst old-new-alist old)))
		  (unless (eq old new)
		    (setf (structref tree i) new))))))
	   (array
	    (let ((i 0))
	      (map nil #'(lambda (old &aux
				      (new (circle-subst old-new-alist old)))
			   (unless (eq old new)
			     (setf (row-major-aref tree i) new))
			   (incf i))
		   tree)))
	   (t (let ((a (circle-subst old-new-alist (car tree)))
		    (d (circle-subst old-new-alist (cdr tree))))
		(unless (eq a (car tree)) (rplaca tree a))
		(unless (eq d (cdr tree)) (rplacd tree d)))))
	 tree)
	(t tree)))

;;; Sharp-equal works as follows.  When a label is assigned (ie when #= is
;;; called) we GENSYM a symbol is which is used as an unforgeable tag.
;;; *READER-REFERENCES* maps the integer tag to this gensym.
;;;
;;; When SHARP-SHARP encounters a reference to a label, it returns the symbol
;;; assoc'd with the label.  Resolution of the reference is deferred until the
;;; read done by #= finishes.  Any already resolved tags (in
;;; *READER-LABELS*) are simply returned.
;;;
;;; After reading of the #= form is completed, we add an entry to
;;; *READER-LABELS* that maps the gensym tag to the resolved object.  Then
;;; for each entry in the *READER-REFERENCES*, the current object is searched
;;; and any uses of the gensysm token are replaced with the actual value.
(defparameter *reader-references* nil)

(defun LABEL-READER (stream c label)
  (when *read-suppress* (return-from label-reader (read stream t nil t)))
  (unless label (read-error stream "Missing label for #~c." c))
  (when (or (assoc label *reader-references*)
	    (assoc label *reader-labels*))
    (read-error stream "Multiply defined label: #~D" label))
  (let* ((tag (gensym))
	 (*reader-references* (acons label tag *reader-references*))
	 (obj (read stream t nil t)))
    (when (eq obj tag)
      (read-error stream "Unable to resolve #~D~c." label c))
    (push (list label tag obj) *reader-labels*)
    (let ((*circular-reference-table* (make-hash-table :test #'eq :size 20)))
      (circle-subst *reader-labels* obj))))


(defun REFERENCE-READER (stream c label)
  (cond (*read-suppress* nil)
	(label
	 (let ((entry (assoc label *reader-labels*)))
	   (if entry (third entry)
	     (let ((pair (assoc label *reader-references*)))
	       (if pair (cdr pair)
		 (read-error
		  stream
		  "The label #~d~c has not been defined." label c))))))
	(t (read-error stream "Missing label for #~c." c))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 22.1.5 THE READTABLE
(defun sethash (key table value)
  (if value
      (setf (gethash key table) value)
    (remhash key table)))

(defstruct (READTABLE
	    (:copier nil)
	    (:predicate READTABLEP)
	    (:print-object
	     (lambda (r s)
	       (print-unreadable-object (r s :identity t :type t)))))
  (syntax (make-hash-table))	;A keyword for each char
  (macros (make-hash-table))	;A function for each char
  ;; A hash-table for each dispatch-char.
  ;; Within the inner hash-table, there is a function for each char.
  (dispatch-tables (make-hash-table))
  (case :upcase)
  (parse t))

(defvar *TEXT-READTABLE* nil)
(defvar *STANDARD-READTABLE* (make-readtable))

;;; SYNTAX ACCESSORS
(defun get-character-syntax (char &optional (readtable *readtable*))
  (gethash char (readtable-syntax
		 (or readtable *standard-readtable*))
	   :constituent))
(defun set-character-syntax (char syntax
			     &optional (readtable *readtable*))
  (sethash char (readtable-syntax
		 (or readtable *standard-readtable*)) syntax))

;;; MACRO ACCESSORS
(defun GET-MACRO-CHARACTER (char
			    &optional (readtable *readtable*))
  (values
   (gethash char (readtable-macros
		  (or readtable
		      (setf readtable *standard-readtable*))))
   (eq (get-character-syntax char readtable) :non-terminating-macro)))

(defun set-macro-character1 (char function readtable)
  (sethash char (readtable-macros readtable) function))

;;; DISPATCH-TABLE ACCESSORS
(defun get-dispatch-table (disp-char readtable errorp)
  (or (gethash disp-char (readtable-dispatch-tables readtable))
      (and errorp
	   (error "~s is not a dispatching macro character in ~s."
		  disp-char readtable))))
(defun set-dispatch-table (disp-char dispatch-table readtable)
  (sethash disp-char (readtable-dispatch-tables readtable) dispatch-table))


;;; PUBLIC MANIPULATIONS
(defun COPY-READTABLE (&optional (from-readtable *readtable*)
				 (to-readtable (make-readtable)))
  (unless from-readtable (setf from-readtable *standard-readtable*))
  (setf (readtable-CASE to-readtable) (readtable-case from-readtable)
	(readtable-PARSE to-readtable) (readtable-parse from-readtable))
  (setf (readtable-SYNTAX to-readtable)
    (copy-hash-table (readtable-syntax from-readtable)))
  (setf (readtable-MACROS to-readtable)
    (copy-hash-table (readtable-macros from-readtable)))
  (let ((dispatch (copy-hash-table (readtable-dispatch-tables
				    from-readtable))))
    (setf (readtable-DISPATCH-TABLES to-readtable) dispatch)
    (maphash #'(lambda (key value)
		 (setf (gethash key dispatch) (copy-hash-table value)))
	     dispatch))
  to-readtable)


(defun SET-SYNTAX-FROM-CHAR (to-char from-char
			     &optional (to-readtable *readtable*)
				       from-readtable)
  (SET-CHARACTER-SYNTAX     
   to-char
   (get-character-syntax
    from-char (or from-readtable
		  (setf from-readtable *standard-readtable*)))
   to-readtable)
  (SET-MACRO-CHARACTER1
   to-char (get-macro-character from-char from-readtable)
   to-readtable)
  (let ((table (get-dispatch-table from-char from-readtable nil)))
    (set-dispatch-table to-char (and table (copy-hash-table table))
			to-readtable))
  t)


(defun SET-MACRO-CHARACTER (char function
			    &optional (non-terminating-p nil)
				      (readtable *readtable*))
  (set-character-syntax char (if non-terminating-p
				 :non-terminating-macro
			       :terminating-macro)
			readtable)
  (set-macro-character1 char function readtable)
  t)



(defun MAKE-DISPATCH-MACRO-CHARACTER
    (char &optional (non-terminating-p nil) (readtable *readtable*))
  (set-dispatch-table char (make-hash-table) readtable)
  (set-macro-character char #'dispatcher non-terminating-p readtable))

(defun GET-DISPATCH-MACRO-CHARACTER
    (disp-char sub-char &optional (readtable *readtable*))
  (values
   (gethash (char-upcase sub-char)
	    (get-dispatch-table disp-char
				(or readtable *standard-readtable*)
				t))))

(defun SET-DISPATCH-MACRO-CHARACTER
    (disp-char sub-char function &optional (readtable *readtable*))
  (setf (gethash (char-upcase sub-char)
		 (get-dispatch-table disp-char readtable t))
    function)
  t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                       INPUT FUNCTIONS                        ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 22.2.1 INPUT FROM CHARACTER STREAMS
(defun READER (input-stream eof-error-p eof-value recursive-p)
  (values
   (if recursive-p
       (read-token input-stream
		   :eof-error-p eof-error-p
		   :eof-value eof-value)
       (let ((*reader-labels* nil))
	 (read-token input-stream
		     :eof-error-p eof-error-p
		     :eof-value eof-value)))))

(defun READ (&optional (input-stream *standard-input*)
			 (eof-error-p t) eof-value recursive-p)
  (let ((*preserve-whitespace* (if recursive-p
				   *preserve-whitespace*
				 nil)))
    (reader input-stream eof-error-p eof-value recursive-p)))

(defun READ-PRESERVING-WHITESPACE
    (&optional (input-stream *standard-input*)
	       (eof-error-p t) eof-value recursive-p)
  (let ((*preserve-whitespace* (if recursive-p
				   *preserve-whitespace*
				 t)))
    (reader input-stream eof-error-p eof-value recursive-p)))

(defun READ-DELIMITED-LIST (char
			    &optional (stream *standard-input*)
				      recursive-p)
  (if recursive-p
      (read-list char stream nil)
    (let ((*reader-labels* nil)
	  (*preserve-whitespace* nil))
      (read-list char stream nil))))

(defun READ-FROM-STRING (string
                         &optional (eof-error-p t) eof-value
                         &key (start 0) end
                              preserve-whitespace)
  (let* (index
	 (result (with-input-from-string (s string :index index
					  :start start :end end)
		   (if preserve-whitespace
		       (read-preserving-whitespace
			s eof-error-p eof-value)
		     (read s eof-error-p eof-value)))))
    (values result index)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                    BACKQUOTE                                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; See appendix C in Steele.
(defvar *bq-level*     0)
(defvar *bq-list* 'bq-list)
(defvar *bq-list** 'bq-list*)
(defvar *bq-cons* 'bq-cons)		;???
(defvar *bq-append* 'bq-append)
(defvar *bq-nconc* 'bq-nconc)
(defvar *bq-quote* 'bq-quote)
(defvar *remove-bq-tokens* nil)

(macrolet ((def-bq (name op)
	     `(defmacro ,name (&rest args)
		(cons ',op args))))
  (def-bq bq-list list)
  (def-bq bq-list* list*)
  (def-bq bq-append append)
  (def-bq bq-nconc nconc)
  (def-bq bq-quote quote))

(defun backquote-reader (stream char)
  (declare (ignore char))
  (incf *bq-level*)
  (prog1
      (bq-completely-process (read stream))
    (decf *bq-level*)))

(defun comma-reader (stream char)
  (declare (ignore char))
  (when (<= *bq-level* 0)
    (read-error stream "A comma appeared outside of a backquote"))
  (decf *bq-level*)
  (prog1
      (cons (case (peek-char nil stream t nil t)
              (#\@ (read-char stream t nil t) 'comma-atsign)
              (#\. (read-char stream t nil t) 'comma-dot)
              (otherwise 'comma))
            (read stream))
    (incf *bq-level*)))

(defun bq-completely-process (x)
  (let ((simple (bq-simplify (bq-process x))))
    (if *remove-bq-tokens*
	(bq-remove-tokens simple)
	simple)))

(defun bq-process (x)
  (cond
    ((atom x) (list *bq-quote* x))
    ((eq (car x) 'comma) (cdr x))
    ((eq (car x) 'comma-atsign) (error ",@~S after `" (cdr x)))
    ((eq (car x) 'comma-dot) (error ",.~S after `" (cdr x)))
    (t (do ((p x (cdr p))
            (q '() (cons (bracket (car p)) q)))
           ((atom p)
            (cons *bq-append*
                  (nreconc q (list (list *bq-quote* p)))))
         (when (eq (car p) 'comma)
           (return (cons *bq-append* (nreconc q (list (cdr p))))))
         (when (eq (car p) 'comma-atsign) (error "Dotted ,@~s" (cdr p)))
         (when (eq (car p) 'comma-dot) (error "Dotted ,@~s" (cdr p)))))))

(defun bracket (x)
  (cond
    ((atom x) (list *bq-list* (bq-process x)))
    ((eq (car x) 'comma) (list *bq-list* (cdr x)))
    ((eq (car x) 'comma-atsign) (cdr x))
    ((eq (car x) 'comma-dot) (list 'bq-clobberable (cdr x)))
    (t (list *bq-list* (bq-process x)))))

(defun maptree (fn x)
  (if (atom x)
    (funcall fn x)
    (let ((a (funcall fn (car x)))
          (d (maptree fn (cdr x))))
      (if (and (eql a (car x)) (eql d (cdr x)))
        x
        (cons a d)))))

(defun bq-splicing-frob (x)
  (and (consp x)
       (or (eq (car x) 'comma-atsign)
           (eq (car x) 'comma-dot))))

(defun bq-frob (x)
  (and (consp x)
       (or (eq (car x) 'comma)
           (eq (car x) 'comma-atsign)
           (eq (car x) 'comma-dot))))

(defun bq-simplify (x)
  (if (atom x)
    x
    (let ((x (if (eq (car x) *bq-quote*)
               x
               (maptree #'bq-simplify x))))
      (if (not (eq (car x) *bq-append*))
        x
        (bq-simplify-args x)))))

(defun bq-simplify-args (x)
  (do ((args (reverse (cdr x)) (cdr args))
       (result
        nil
        (cond ((atom (car args))
               (bq-attach-append *bq-append* (car args) result))
              ((and (eq (caar args) *bq-list*)
                    (notany #'bq-splicing-frob (cdar args)))
               (bq-attach-conses (cdar args) result))
              ((and (eq (caar args) *bq-list**)
                    (notany #'bq-splicing-frob (cdar args)))
               (bq-attach-conses
                (reverse (cdr (reverse (cdar args))))
                (bq-attach-append *bq-append*
                                  (car (last (car args)))
                                  result)))
              ((and (eq (caar args) *bq-quote*)
                    (consp (cadar args))
                    (not (bq-frob (cadar args)))
                    (null (cddar args)))
               (bq-attach-conses (list (list *bq-quote*
                                             (caadar args)))
                                 result))
              ((eq (caar args) 'bq-clobberable)
               (bq-attach-append *bq-nconc* (cadar args) result))
              (t (bq-attach-append *bq-append*
                                   (car args)
                                   result)))))
      ((null args) result)))

(defun null-or-quoted (x)
  (or (null x) (and (consp x) (eq (car x) *bq-quote*))))

(defun bq-attach-append (op item result)
  (cond ((or (null result) (equal result '(bq-quote nil)))
         (if (bq-splicing-frob item) (list op item) item))
        ((and (null-or-quoted item) (null-or-quoted result))
         (list *bq-quote* (append (cadr item) (cadr result))))
        ((and (consp result) (eq (car result) op))
         (list* (car result) item (cdr result)))
        (t (list op item result))))

(defun bq-attach-conses (items result)
  (cond
    ((and (every #'null-or-quoted items)
          (null-or-quoted result))
     (list *bq-quote*
           (append (mapcar #'cadr items) (cadr result))))
    ((or (null result) (equal result '(bq-quote nil)))
     (cons *bq-list* items))
    ((and (consp result)
         (or (eq (car result) *bq-list*)
             (eq (car result) *bq-list**)))
     (cons (car result) (append items (cdr result))))
    (t (cons *bq-list** (append items (list result))))))

(defun bq-remove-tokens (x)
  (cond
    ((atom x) (cond
                ((eq x *bq-list*) 'list)
                ((eq x *bq-append*) 'append)
                ((eq x *bq-nconc*) 'nconc)
                ((eq x *bq-list**) 'list*)
                ((eq x *bq-quote*) 'quote)
                (T x)))
    ((eq (car x) 'bq-clobberable) (bq-remove-tokens (cadr x)))
    ((and (eq (car x) *bq-list**) (consp (cddr x)) (null (cdddr x)))
     (cons 'cons (maptree #'bq-remove-tokens (cdr x))))
    (T (maptree #'bq-remove-tokens x))))
	       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                    STANDARD READTABLE                        ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-when (:load-toplevel :execute)

(set-character-syntax #\space :whitespace *standard-readtable*)
(set-syntax-from-char #\tab #\space *standard-readtable*)
(set-syntax-from-char #\page #\space *standard-readtable*)
(set-syntax-from-char #\return #\space *standard-readtable*)
(set-syntax-from-char #\newline #\space *standard-readtable*)
(set-syntax-from-char #\rubout #\space *standard-readtable*)
(set-syntax-from-char #\linefeed #\space *standard-readtable*)

(setf *text-readtable* (copy-readtable *standard-readtable*))

(set-character-syntax #\\ :single-escape *standard-readtable*)
(set-character-syntax #\| :multiple-escape *standard-readtable*)

(set-macro-character #\( #'list-reader nil *standard-readtable*)
(set-macro-character #\) #'unmatched-reader nil *standard-readtable*)
(set-macro-character #\' #'quote-reader nil *standard-readtable*)
(set-macro-character #\; #'line-comment-reader nil *standard-readtable*)
(set-macro-character #\" #'string-reader nil *standard-readtable*)
(set-macro-character #\` #'backquote-reader nil *standard-readtable*)
(set-macro-character #\, #'comma-reader nil *standard-readtable*)

(make-dispatch-macro-character #\# t *standard-readtable*)
(set-dispatch-macro-character #\# #\\ #'character-reader *standard-readtable*)
(set-dispatch-macro-character #\# #\' #'function-reader *standard-readtable*)
(set-dispatch-macro-character #\# #\( #'vector-reader *standard-readtable*)
(set-dispatch-macro-character #\# #\* #'bit-vector-reader *standard-readtable*)
(set-dispatch-macro-character #\# #\: #'uninterned-reader *standard-readtable*)
(set-dispatch-macro-character #\# #\B #'binary-reader *standard-readtable*)
(set-dispatch-macro-character #\# #\O #'octal-reader *standard-readtable*)
(set-dispatch-macro-character #\# #\X #'hex-reader *standard-readtable*)
(set-dispatch-macro-character #\# #\R #'radix-reader *standard-readtable*)
(set-dispatch-macro-character #\# #\C #'complex-reader *standard-readtable*)
(set-dispatch-macro-character #\# #\A #'array-reader *standard-readtable*)
(set-dispatch-macro-character #\# #\S #'structure-reader *standard-readtable*)
(set-dispatch-macro-character #\# #\P #'pathname-reader *standard-readtable*)
(set-dispatch-macro-character #\# #\= #'label-reader *standard-readtable*)
(set-dispatch-macro-character #\# #\# #'reference-reader *standard-readtable*)
(set-dispatch-macro-character #\# #\+ #'feature-reader *standard-readtable*)
(set-dispatch-macro-character #\# #\- #'not-feature-reader *standard-readtable*)
(set-dispatch-macro-character #\# #\| #'block-comment-reader *standard-readtable*)
(set-dispatch-macro-character #\# #\. #'eval-reader *standard-readtable*)

)

;; Useless for machine-compile because of binding of *readtable* during init.
#-machine-compile
(setf *readtable* (copy-readtable *standard-readtable*))
  

#-machine-compile
(when (fboundp 'format-string-reader)
  (set-dispatch-macro-character #\# #\" #'format-string-reader *readtable*)
  (cl:set-dispatch-macro-character #\# #\" #'format-string-reader))
  
