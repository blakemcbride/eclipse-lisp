#| BUGS:
 Functions beigining with '-' miss the minus.  For example -dderiv
 becomes clDderiv instead of clMINUSDderiv!!!
 |#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                      IDENTIFIERS                             ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PRINTING IDENTIFIERS 
(defparameter character-map
    '((#\! . "BANG")       
      (#\" . "QUOTATION")  
      (#\# . "HASH")       
      (#\$ . "DOLLAR")     
      (#\% . "PERCENT")    
      (#\& . "AMPERSAND")  
      (#\' . "QUOTE")      
      (#\( . "LPAREN")     
      (#\) . "RPAREN")     
      (#\* . "STAR")       
      (#\+ . "PLUS")       
      (#\, . "COMMA")      
      (#\- . "_")          
      (#\. . "DOT")        
      (#\/ . "SLASH")      
      (#\: . "COLON")      
      (#\; . "SEMICOLON")  
      (#\< . "LESS")       
      (#\= . "EQUAL")      
      (#\> . "GREATER")    
      (#\? . "QUESTION")   
      (#\space . "SPACE")  
      (#\@ . "AT")         
      (#\[ . "LBRACKET")   
      (#\\ . "BACKSLASH")  
      (#\] . "RBRACKET")   
      (#\^ . "CARET")      
      (#\_ . "__")         
      (#\` . "BACKQUOTE")  
      (#\{ . "LBRACE")     
      (#\| . "PIPE")       
      (#\} . "RBRACE")     
      (#\~ . "TILDE")
      (#\newline . "NEWLINE")
      (#\return . "RETURN")
      (#\backspace . "BACK")
      (#\page . "PAGE")
      (#\tab . "TAB")))

(defparameter function-character-map
    (append '((#\_ . "UNDER")
	      (#\/ . "DIV")
	      (#\- . "MINUS"))
	    character-map))

(defparameter c-character-map nil)
(defparameter constant-id-numeric-character-map
    (append '((#\. . "_"))
	    function-character-map))
(defparameter constant-id-character-map
    (append '((#\space . "_"))
	    character-map))

;;; Escape only the truly escaped characters.
;;; Substitute names from map in appropriate case.
;;; Add distinguishing string at end if there weren't any uppercase
;;; characters in a distinguishing position.
;;; Strip single dashes when asked.
(defun print-identifier (stream string
			 &key distinguisher
			      (case *print-case*)
			      strip
			      (map character-map)
			 &aux char subst escaped stripped
			      (new-word t) capitalize
			      alpha maybe-alpha
			      (max (1- (length string)))
			      (down-escape (eql case :upcase))
			      (mult (cdr (assoc #\| map)))
			      (single (cdr (assoc #\\ map))))
  (when down-escape
    (setf mult (string-downcase mult)
	  single (string-downcase single)))
  (dotimes (i (length string))
    (setf char (schar string i))
    (when strip
      (if (char= char #\-)
	  (if (or stripped (= i max))
	      (setf stripped nil)
	    (setf char (schar string (incf i))
		  new-word t stripped t))
	(setf stripped nil)))
    (when (eql case :capitalize)
      (if (alphanumericp char)
	  (setf capitalize new-word new-word nil)
	  (setf capitalize nil new-word t)))
    (cond ((lower-case-p char)
	   (unless escaped
	     (write-string
	      (if (and (< i max)
		       (lower-case-p
			(schar string (1+ i))))
		  (setf escaped mult)
		single) stream)))
	  (escaped (setf escaped nil)
		   (write-string mult stream)))
    (cond ((upper-case-p char)
	   (write-char 
	    (ecase case
	      (:upcase (setf alpha t) (char-upcase char))
	      (:downcase (setf alpha t) (char-downcase char))
	      (:capitalize
	       (cond (capitalize (setf capitalize nil)
				 (setf maybe-alpha t)
				 (char-upcase char))
		     (t (when maybe-alpha (setf alpha t))
			(char-downcase char)))))
	    stream))
	  ((setf subst (or (cdr (assoc char map))
			   (let ((code (char-int char)))
			     (when (>= code base-char-code-limit)
			       (format nil #"_X~x_" code)))))
	   (write-string (if down-escape
			     (string-downcase subst)
			   subst) stream))
	  (t (write-char char stream)))
    (when (and stripped (not (char= char #\-)));???
      (setf stripped nil)))
  (when escaped (write-string mult stream))
  (unless (or alpha (null distinguisher))
    (write-string distinguisher stream)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PACKAGE QUALIFIERS
(defun c-qualify-p (symbol &optional strip-p
		    &aux (name (symbol-name symbol)))
  (or (not (eq symbol (find-symbol name)))
      (digit-char-p (schar name
			   (if strip-p
			       (position-if-not
				#'(lambda (c)
				    (or (char= c #\_) (char= c #\-)))
				name)
			     0)))))

;;; NOTE: The symbol xstar:foo* is indistinguishable from the symbol
;;; x:*foo*, so it is inadvisable to have package prefixes which end in
;;; STAR.  
(defparameter *package-prefixes* (make-hash-table :test 'equal))

(defun package-prefix (pkg)
  (or (gethash pkg *package-prefixes*)
      (setf (gethash pkg *package-prefixes*)
	    (loop with best = (package-name pkg)
		  with best-length = (length best)
		  for nick in (package-nicknames pkg)
		  for length = (length nick)
		  when (< length best-length)
		  do (setq best nick best-length length)
		  finally (return best)))))
(defun (setf package-prefix) (name pkg)
  (setf (gethash pkg *package-prefixes*) name))

(setf (package-prefix (find-package :eclipse)) "CL")
(setf (package-prefix (find-package :cl)) "CL")
#-machine-compile (setf (package-prefix (find-package :host)) "CL")
(setf (package-prefix (find-package :ec)) "")
(setf (package-prefix (find-package :user)) "USR")
(setf (package-prefix (find-package :keyword)) "KEY")
(setf (package-prefix nil) "L")


(defun c-qualify (s symbol &optional (case :downcase))
  (print-identifier s (package-prefix (symbol-package symbol))
		    :case case))

(defun common-c-token (s symbol &aux string invert char subst)
  (when (c-symbol-p symbol)
    (setf string (symbol-name symbol)
	  invert (not (and (some #'lower-case-p string)
			   (some #'upper-case-p string))))
    (dotimes (i (length string) symbol)
      (setf char (schar string i))
      (if (setf subst (cdr (assoc char c-character-map)))
	  (write-string subst s)
	(write-char
	 (cond ((and invert (upper-case-p char)) (char-downcase char))
	       ((and invert (lower-case-p char)) (char-upcase char))
	       (t char))
	 s)))))

(defun common-function (s symbol)
  (or (common-c-token s symbol)
      (print-identifier s (symbol-name symbol)
			:case :capitalize :distinguisher "_FUNC"
			:strip t :map function-character-map)))

(defun common-symbol (s symbol)
  (or (common-c-token s symbol)
      (print-identifier s (symbol-name symbol)
			:case :upcase
			:distinguisher "symbol")))

(defun common-variable (s symbol)
  (or (common-c-token s symbol)
      (print-identifier s (symbol-name symbol)
			:case :downcase)))

 
(defun c-index (s index &optional c? at?)
  (when index (format s #"~:[__~;~]~c~d" at? (if c? #\R #\r) index)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; VARIOUS CATEGORIES OF C IDENTIFIERS
#| The rules are:
- Follow general conventions for C:

  - Local variables are in lower case.  

    Most variables will look just like they do in Lisp source code
    (except '-' becomes '_'). 

  - Functions and type identifiers have words formed by capitalizing
    the first letter of each word (and removing dashes).  

    Nested functions have underscores between the name of the parent
    and the local name.  Functions and type specifiers in the C
    package appear "directly" so c:int => int, c:toupper => toupper.

  - Everything else such as labels and "`const' variables" holding
    interned symbols are in upper case. 

- Global identifiers are always package qualified while local identifiers
  are qualifed only when needed.  For example, to distinguish between
  variables with the same name in different packages, we package
  qualify only the one which is not accessible in the current package.

- For symbols in the C package, all bets are off -- they are just case
  inverted. Its up to the user to make sure that these don't clash.
  (We do distinguish between functions/typedefs vs
  local-variables/labels by prepending an '_' to the latter.) 

- Lowercase characters in the Lisp names of symbols are escaped, just as
  they would be printed in Lisp.  However, unlike the Lisp printer, the
  maximum number of unescaped character are used, so as to give the
  best chance of distinguishishing between different types of identifiers
  with the same name.  If an insufficient number of unescaped characters
  appear, an additional distinguishing string may appear at the end of the
  identifier. 

- Non alphanumeric characters are replaced with their names (and '-'
  with '_').

|#  

;;; This should be a hash table!!!
(defparameter *reserved-c-tokens*
    '("INT" "CHAR" "FLOAT" "DOUBLE" "STRUCT" "UNION" "LONG" "SHORT"
      "SIGNED" "UNSIGNED" "AUTO" "EXTERN" "REGISTER" "TYPEDEF" "STATIC" "GOTO"
      "RETURN" "SIZEOF" "BREAK" "CONTINUE" "IF" "ELSE" "FOR" "DO"
      "WHILE" "SWITCH" "CASE" "DEFAULT" "ENTRY" "FORTRAN" "ASM" "VOID"
      "VA_LIST" "BOOL" "CATCH" "CLASS" "CONST-CAST" "DELETE" "DYNAMIC-CAST"
      "FALSE" "FRIEND" "INLINE" "MUTABLE" "NAMESPACE" "NEW" "OPERATOR"
      "PRIVATE" "PROTECTED" "PUBLIC" "REINTERPRET_CAST" "STATIC-CAST"
      "TEMPLATE" "THIS" "THROW" "TRUE" "TRY" "TYPEID" "USING"
      "VIRTUAL"))


(defun c-variable-binding (s symbol &rest ignore)
  (declare (ignore ignore))
  (when (or (c-qualify-p symbol)
	    (member (symbol-name symbol) *reserved-c-tokens* :test #'equal))
    (c-qualify s symbol :upcase) (write-char #\_ s))
  (common-variable s symbol))

(defun c-modifier (s modifier)
  (when modifier
    (write-char #\_ s)
    (print-identifier s (symbol-name modifier)
		      :case :upcase)
    (write-char #\_ s)))

(defun c-function-binding (s symbol &rest ignore &aux modifier)
  (declare (ignore ignore))
  (when (consp symbol)
    (setq modifier (first symbol)
	  symbol (second symbol)))
  (cond ((numberp symbol) (c-index s symbol nil t))
	(t (when (c-qualify-p symbol)
	     (c-qualify s symbol) (write-char #\_ s))
	   (c-modifier s modifier)
	   (common-function s symbol))))

(defun c-other-binding (s symbol &rest ignore)
  (declare (ignore ignore))
  (cond ((integerp symbol)
	 (format s #"~a~d" (package-prefix nil) symbol))
	(t (when (c-qualify-p symbol)
	     (c-qualify s symbol) (write-char #\_ s))
	   (common-symbol s symbol))))


(defun c-global-symbol (s symbol &rest ignore)
  (declare (ignore ignore))
  (when (and (c-symbol-p symbol)
	     (member (symbol-name symbol) *reserved-c-tokens* :test #'equal))
    (write-char #\_ s))
  (c-qualify s symbol)
  (common-symbol s symbol))

(defun c-global-function (s symbol &rest ignore &aux modifier args)
  (declare (ignore ignore))
  (when (consp symbol)
    (if (eq (car symbol) 'macro)
	;; On some hosts, the fname of macros is (macro xxx).  Leaving
	;; this alone will work -- until someone tries to call the macro
	;; directly from C using the name xxx.
	(setq symbol (second symbol))
	(unless (setf-function-name-p symbol)
	  (setq args (cdr symbol)
		symbol (car symbol)))))
  (when (setf-function-name-p symbol)
    (setq modifier (first symbol)
	  symbol (second symbol)))
  (c-qualify s symbol)
  (c-modifier s modifier)
  (common-function s symbol)
  (dolist (arg args)
    (write-char #\_ s)
    (typecase arg
      (symbol (c-global-symbol s arg))
      (t (when (and (car-eq arg 'eql)
		    (cdr arg)
		    (null (cddr arg)))
	   (write-string "eql" s)
	   (setq arg (cadr arg)))
	 (print-identifier s (prin1-to-string arg)
			   :case :upcase)))))
      
    

(defun c-function (s nesting &rest ignore)
  (declare (ignore ignore))
  (format s #"~{~/eclipse:c-global-function/~@{_~/eclipse:c-function-binding/~}~}"
	  nesting))

(defun c-typedef (s symbol &rest ignore)
  (declare (ignore ignore))
  (c-global-function s symbol))

(defun c-constant-id (s id &rest ignore)
  (declare (ignore ignore))
  (write-string (constant-id-identifier id) s)
  (write-char #\_ s)
  (let ((map (if (typep id 'numeric-constant-id)
		 constant-id-numeric-character-map
		 constant-id-character-map)))
    (map nil
	 #'(lambda (char &aux (subst (cdr (assoc char map))))
	     (if subst (write-string (string-downcase subst) s)
		 (write-char char s)))
	 (constant-id-string id))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                 THE DISPATCH TABLE                           ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; To get the debugger to print in Lisp again, execute:
;;; (setq *print-pprint-dispatch* (copy-pprint-dispatch nil))
;;; To temporarilly bind dispatch:
;;; (let ((*print-pprint-dispatch* (copy-pprint-dispatch nil))) (vars ...))
;;; or
;;; (let ((*print-pprint-dispatch* *IPD*)) (vars ...))

(defparameter *cprint-dispatch* (copy-pprint-dispatch nil))
;;; Effectively disables the standard dispatch entries.
(set-pprint-dispatch 't #'print-object-reversed-args -10 *cprint-dispatch*)

(defmacro with-C-syntax (&body body)
  `(handler-bind ((error #'(lambda (c)
			     (let ((*print-pprint-dispatch* (copy-pprint-dispatch nil)))
			       (error c)))))
     (let ((*print-pretty* t)
	   (*print-circle* nil)
	   (*print-length* nil)
	   (*print-lines* nil)
	   (*print-level* nil)
	   (*print-base* 10)
	   (*print-pprint-dispatch* *cprint-dispatch*)
	   (*print-miser-width* 50)
	   (*print-right-margin* 72))
       ,@body)))

(defun make-constant-string (format args)
  (with-c-syntax
      (if (or (stringp format) (functionp format))
	  (apply #'format nil format args)
	  (princ-to-string format))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                   C LITERALS                                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Uses \n, etc, when necessary, otherwise \000, for non-graphic
;;; ascii-chars, or 0xXXXX for extended-chars.
;;; : surrounds char with ' ' (if it's really a char)
;;; @ escapes '"'
(defun c-char (s char &optional colonp atp &aux name)
  (when colonp (write-char #\' s))
  (cond ((setq name (getf '(#\newline "\\n"
			    #\tab "\\t"
			    #\backspace "\\b"
			    #\return "\\r"
			    #\page "\\f"
			    #\null "\\0"
			    #\' "\\'"
			    #\\ "\\\\")
			  char))
	 (write-string name s))
	((and atp (char= char #\")) (write-string "\\\"" s))
	((standard-char-p char) (princ char s))
	(t (let ((code (char-int char)))
	     (cond ((< code base-char-code-limit)
		    (format s #"\\~3,'0o" code))
		   (atp
		    (error "Attempt to output extended-char ~@:c in a char[])."
			   char))
		   (t (format s #"0x~x" code))))))
  (when colonp (write-char #\' s)))

(macrolet ((def-prim (name (stream obj &optional (priority 1)) &body body)
	     `(set-pprint-dispatch
	       ',name #'(lambda (,stream ,obj &rest ignore)
			  (declare (ignore ignore))
			  ,@body)
	       ,priority *cprint-dispatch*)))
  (def-prim ec:char (s char 2)
    (c-char s (char-value char) t))
  (def-prim ec:int (s int) (format s #"~d" (int-value int)))
  (def-prim ec:float (s float)
    (let ((*read-default-float-format* 'single-float))
      (write-toplevel (float-value float) s)))
  (def-prim ec:double (s float)
    (let ((*read-default-float-format* 'double-float))
      (write-toplevel (double-value float) s)))
  (def-prim charp (s string)
    (write-char #\" s)
    (loop for c across (the simple-base-string (charp-value string))
	  do (c-char s c nil t))
    (write-char #\" s)))

  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                   PRINT FUNCTIONS                            ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun timezone (s zone &optional c? daylight-savings-p)
  (cond ((and (not c?) (integerp zone) (<= 5 zone 8))
	 (format s #"~a~:[S~;D~]T"
		 (schar "ECMP" (- zone 5)) daylight-savings-p))
	((zerop zone) (write-string (if daylight-savings-p
					"GDT"
					"GMT")
				    s))
	(t (format s #"GMT~@d"
		   (if daylight-savings-p
		       (+ zone 1)
		       zone)))))

;;; default: Friday the fifteenth of February, 1991, 4:29:06 pm CDT
;;; : qualified: 2/15/91
;;; @ qualified: 16:29:06
;;; :@ qualifed: 2/15/91; 16:29:06
(defun date (&optional s (universal-time (get-universal-time)) c? at?)
  (multiple-value-bind (second minute hour date month year
			day savings-p zone pm-p)
      (decode-universal-time universal-time)
    (setq s (output-stream s))
    (cond ((or c? at?)
	   (when c?
	     (format s #"~d/~d/~d~:[~; ~]" month date (mod year 100) at?))
	   (when at?
	     (format s #"~2,'0d:~2,'0d:~2,'0d" hour minute second)))
	  (t (when (> hour 12) (decf hour 12) (setf pm-p t))
	     (format
	      s
	      #"~a the ~:r of ~a, ~d, ~d:~2,'0d:~2,'0d ~:[am~;pm~] "
	      (svref '#("Monday" "Tuesday" "Wednesday" "Thursday"
			"Friday" "Saturday" "Sunday") day) 
	      date
	      (svref '#(nil "January" "February" "March" "April" "May"
			"June" "July" "August" "September" "October"
			"November" "December")  month)
	      year hour minute second pm-p)
	     (timezone s  zone nil savings-p)))))
      

;;; Comma separated series.
;;; colon qualifer indicates parens.
;;; at qualifier indicates parens should be used if more than one item
;;; and that eoa should be printed if no items.
(defun series (s items &optional c? at?)
  (when at? (cond ((rest items) (setq c? t))
		  (items )
		  (t (setq items `(,eoa-flag)))))
  ;;; IWBNI pprint-fill accepted an inter-object string.
  (cond (items
	 (pprint-logical-block (s items :prefix (when c? "(")
				  :suffix (when c? ")"))
	   (loop (write-toplevel (pprint-pop) s)
		 (pprint-exit-if-list-exhausted)
		 (write-string++ ", " s 0 2)
		 (pprint-newline :fill s))))
	(c? (write-string "()" s))))

(defun c-call (s args &rest ignore)
  (declare (ignore ignore))
  (write-string "clFuncallFunction" s)
  (pprint-logical-block (s nil :prefix "(" :suffix ")")
    (dolist (arg args (write-string++ "clEOA" s 0 5))
      (write-toplevel arg s)
      (write-string++ ", " s 0 2)
      (pprint-newline :fill s))))

;;; Some C statements do NOT end in ';'!
(defun statement (s statement &rest ignore)
  (declare (ignore ignore))
  (when (atom statement) (write-string "(void) " s))
  (write-toplevel statement s)
  (unless (and (consp statement)
	       (member (car statement)
		       '(if compound lisp-function)))
    (write-char #\; s)))

(defun statements (s statements &rest ignore)
  (declare (ignore ignore))
  (loop (statement s (pop statements))
	(unless statements (return t))
	(write-char #\space s)
	(pprint-newline :linear s)))

;;; Prints a sequence:
;;; - surrounds with { } when more than one statement.
;;; - always prints at least ';', even for null sequence.
(defun sblock (s statements &optional c? at?)
  (declare (ignore at?))
  (cond ((or (rest statements) c?)
	 (pprint-logical-block (s statements :prefix "{ " :suffix " }")
				(statements s statements)))
	(statements (statement s (first statements)))
	(t (write-char #\; s))))

(defun parsing-block (s statements &rest ignore)
  (declare (ignore ignore))
  (format s #"~@<{ ~;clBeginParse(T_ap); ~/eclipse:statements/ clEndParse(T_ap);~; }~:@>"
	  statements))

;;; BINDING DECLARATIONS
(defun make-initialization (s b &rest ignore
		      &aux (decs (binding-declarations b))
			   (type (getf decs 'type)))
  (declare (ignore ignore))
  (when (indirectp decs)
    (if (c-symbol-p type)
	(format s #" = (~/eclipse:c-typedef/ *) Malloc(sizeof(~/eclipse:c-typedef/))"
		type type)
      (format s #" = ~w" '(make-binding)))))

(defun array-declaration (s b &rest ignore
			  &aux (type (getf (binding-declarations b)
					   'type)))
  (declare (ignore ignore))
  (when (car-eq type 'ec::array)
    (format s #"~{[~d]~}" (third type))))

(defun binding-declaration (s b &rest ignore)
  (declare (ignore ignore))
  (format s
	  #"~@<~w~/eclipse:array-declaration/~/eclipse:make-initialization/~:@>"
	  b b b))

;;; IWBNI we noticed that non-dynamic-extent varibles which are
;;; enclosed only by dynamic-extent functions need not be indirect.
(defun indirectp (decs)
  (and (getf decs 'enclosed) (not (getf decs 'dynamic-extent))))

(macrolet ((def-type-disp (type func)
	       `(set-pprint-dispatch ',type #',func 0 *cprint-dispatch*)))
  (def-type-disp constant-id c-constant-id)
  (def-type-disp numeric-constant-id c-constant-id)
  (def-type-disp symbol c-global-symbol)) ;nil, eoa, etc.

(macrolet ((def-type-disp (type (stream form priority) &body body)
	       `(set-pprint-dispatch
		 ',type #'(lambda (,stream ,form) ,@body)
		 ,priority *cprint-dispatch*)))
  ;; NORMAL FUNCTION APPLICATIONS
  (def-type-disp CONS (s form -1)
    (destructuring-bind (function . args) form
      (format s #"~:[~/eclipse:c-global-function/~;~w~]~:/eclipse:series/"
	      (or (consp function) (binding-p function)) function args)))
  (def-type-disp BINDING (s b 0)
    (let ((decs (binding-declarations b))
	  (name (binding-name b))
	  (status (binding-status b)))
     (when (and (not (eq status 'env)) (indirectp decs))
       (write-char #\* s))
     (case status
       (function 
	(let ((nesting (getf decs 'nesting)))
	   (format s #"~/eclipse:c-function/_~/eclipse:c-function-binding/_"
		   nesting name))
	(c-index s (getf decs 'index) nil))
       (reference (write-toplevel name s))		      
       (t (typecase status 
	    (cons (write-toplevel (first status) s))
	    (t ;; ordinary variable
	     (c-variable-binding s name)
	     (c-index s (getf decs 'index) t)))))))
  (def-type-disp TAG (s b 1)
    (c-other-binding s (binding-name b))
    (c-index s (getf (binding-declarations b) 'index)))
  ;; These are really all the same...
  (def-type-disp CAPTURED-VAR (s b 1)
    (format s #"clEnv(~d, ~w)" (captured-var-index b) (captured-var-binding b)))
  (def-type-disp CAPTURED-FUNCTION (s b 1)
    (format s #"clEnv(~d, ~w)" (captured-function-index b) (captured-function-binding b)))
  (def-type-disp CAPTURED-BLOCK (s b 2)
    (format s #"clEnv(~d, ~w)" (captured-block-index b) (captured-block-binding b)))
  (def-type-disp CAPTURED-TAG (s b 2)
    (format s #"clEnv(~d, ~w)" (captured-tag-index b) (captured-tag-binding b)))
  #+not-yet
  (def-type-disp CAPTURED-BINDING (s b 1)
    (let ((type (captured-binding-type b))
	  (index (captured-binding-index b)))
      (if (eq 't type)
	  (format s #"clEnv(~d)" index)
	(format s #"clCEnv(~/eclipse:c-typedef/, ~d)" type index))))
  #+not-yet  
  (def-type-disp GO-TAG (s tag 0) (format s #"~w: " (go-tag-id tag))))

(defun assignment (s pair &rest ignore)
  (declare (ignore ignore))
  (destructuring-bind (var val) pair
    (if (and (binding-p var)
	     (c-symbol-p (getf (binding-declarations var) 'type)))
	(format s #"~w = ~w" var val)
	(format s #"clSetq~:/eclipse:series/" pair))))
  
(defun ptrue (s form &optional reversed at?)
  (declare (ignore at?))
  (when reversed (write-char #\! s))
  (when (consp form)
    (case (car form)
      (eq (return-from ptrue
	    (format s #"_clEq(~w, ~w)" (second form) (third form))))
      (vp (return-from ptrue
	    (format s #"_clVp(~w)" (second form))))
      (presentp (return-from ptrue
		  (format s #"_clPresentp(~w)" (second form))))))
  (format s #"clTrue(~w)" form))

(macrolet ((def-key-disp (key format-string)
	       `(set-pprint-dispatch
		 '(cons (member ,key))
		 #'(lambda (s list)
		     (format s ,format-string (rest list)))
		 0 *cprint-dispatch*)))
  (def-key-disp INTERNAL 	#"_~/eclipse:c-function/")
  (def-key-disp FUNCTION 	#"~/eclipse:c-function/")
  (def-key-disp HOOK 		#"~{(*~/eclipse:c-global-function/_)~}")
  (def-key-disp ID #"~{~/eclipse:c-other-binding/~/eclipse:c-index/~}")

  ;; These could be done using cpp macros, but this is "prettier".  
  (def-key-disp ADDRESS	#"~<&~w~:>")
  (def-key-disp ASSIGN	#"~<~w~2i = ~:_~w~:>")
  (def-key-disp INITIALIZE #"~/eclipse:assignment/")
  (def-key-disp SETQ #"~/eclipse:assignment/")
  (def-key-disp GOTO 	#"~{goto ~w~}")
  (def-key-disp DOUBLE-INT		#"~:<(int) ~w~:>")
  (def-key-disp INT-DOUBLE 	#"~:<(double) ~w~:>")
  (def-key-disp FLOAT-DOUBLE 	#"~:<(double) ~w~:>")
  (def-key-disp INT-FLOAT 	#"~:<(float) ~w~:>")
  (def-key-disp DOUBLE-cFLOAT 	#"~:<(float) ~w~:>")
  (def-key-disp WITH-OLD-VALUES #"~:<clRestoreValues(~w), ~w~:>"))

;;; These are not defined with a formatter because they might be given
;;; a long of instructions which would break format due to
;;; call-arguments-limit restrictions.
(macrolet ((def-key-disp (key (stream statements) &body body)
	     `(set-pprint-dispatch
	       '(cons (member ,key))
	       #'(lambda (,stream full-instruction)
		   (let ((,statements (cdr full-instruction)))
		     ,@body))
	       0 *cprint-dispatch*)))

  (def-key-disp CALL (s instr) (c-call s instr)) ;not used???
  (def-key-disp COMPOUND (s instr) (sblock s instr t))
  (def-key-disp SEQUENCE (s instr) (series s instr nil t))

  (def-key-disp DECLARE (s instr)
    (pprint-logical-block (s instr)
      (when (pprint-pop) (write-string "volatile " s))
      (c-typedef s (pprint-pop)) (write-char #\space s)
      (loop (binding-declaration s (pprint-pop))
	    (pprint-exit-if-list-exhausted)
	    (write-string ", " s) (pprint-newline :fill s))))

  (def-key-disp COND (s instr)
    (destructuring-bind (pred cons alt) instr
      (pprint-logical-block (s instr :prefix "(" :suffix ")")
	(ptrue s pred)
	(write-string " ? " s) (pprint-newline :fill s) 
	(series s cons nil t)
	(write-string " : " s) (pprint-newline :fill s) 
	(series s alt nil t))))

  (def-key-disp IF (s instr)
    (pprint-logical-block (s instr :prefix "" :suffix "")
      (c-if-body s instr))))
  		  
;;; An IF statement without an else which appears as the consequent
;;; clause of an enclosing if which DOES have an else, must have the
;;; inner if statement enclosed in braces. I hate C.

;;; We also handle else-if within the same block.

(defun c-if-body (s instr)
  (flet ((if-part (clause)
		  (and (null (rest clause))
		       (car-eq (first clause) 'if))))
    (destructuring-bind (pred conseq alt) instr
      (let ((reversed (and alt (null conseq))))
	(when reversed (rotatef conseq alt))
	(write-string "if (" s)
	(ptrue s pred reversed)
	(write-string ") " s)
	(pprint-indent :block 2 s)
	(pprint-newline :linear s)

	(sblock s conseq (and alt (if-part conseq)
			      (null (fourth (car conseq)))))

	(when alt
	  (pprint-indent :block 0 s)
	  (write-char #\space s) (pprint-newline :linear s)
	  (write-string "else " s)

	  (cond ((if-part alt) (c-if-body s (cdar alt)))
		(t (pprint-indent :block 2 s)
		   (pprint-newline :fill s)
		   (sblock s alt))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;             FILE OPERATIONS                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get-constants-names (table &aux names)
  (maphash #'(lambda (key value)
	       (declare (ignore key))
	       (push value names))
	   table)
  (sort names #'name-sorter))

(defun name-sorter (id1 id2)
  (flet ((ids (id)
	   (if (symbolp id)
	       (values (package-name (symbol-package id))
		       (symbol-name id))
	     (values (constant-id-identifier id)
		     (constant-id-string id)))))
    (multiple-value-bind (id1a id1b) (ids id1)
      (multiple-value-bind (id2a id2b) (ids id2)
	(or (string< id1a id2a)
	    (and (string= id1a id2a)
		 (string< id1b id2b)))))))

