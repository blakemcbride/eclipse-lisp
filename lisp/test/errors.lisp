(defmacro check-error-class (form condition-name)
  (with-unique-names (result condition)
    `(multiple-value-bind (,result ,condition)
	 (ignore-errors ,form)
       (if (typep ,condition ',condition-name)
	   ',condition-name
	   (values ,result ,condition)))))

(defmacro deferror-test (name form condition-name)
  `(deftest ,name
     (check-error-class ,form ,condition-name)
     ',condition-name))

#+debatable
(deferror-test error-abort (abort)
control-error)

  
(deferror-test error-acos (acos 'x)
type-error)

(deferror-test error-acosh (acosh 'x)
type-error)

(deferror-test error-remove-method (progn
  (defgeneric foo01 (x))
  (defmethod foo01 ((x number)) t)
  (let ((m (find-method #'foo01 nil (list (find-class 'number)))))
    (remove-method #'foo01 m)
    (defgeneric foo01 (x y))
    (add-method #'foo01 m)
) )
error)

(deferror-test error-remove-method2 (progn
  (defgeneric foo02 (x))
  (defmethod foo02 ((x number)) t)
  (let ((m (find-method #'foo02 nil (list (find-class 'number)))))
    (remove-method #'foo02 m)
    (defgeneric foo03 (x))
    (add-method #'foo03 m)
) )
error)

(deferror-test error-adjust (let ((a (make-array 5 :adjustable t)))
  (adjust-array a 4 :fill-pointer 1)
)
error)

(deferror-test error-adjustable-array-p (adjustable-array-p '(x))
type-error)

(deferror-test error-alpha-char-p (alpha-char-p 33)
type-error)

(deferror-test error-alphanumericp (alphanumericp 33)
type-error)

(deferror-test error-array-dimensions (array-dimensions '(x))
type-error)

(deferror-test error-array-displacement (array-displacement '(x))
type-error)

(deferror-test error-array-element-type (array-element-type '(x))
type-error)

(deferror-test error-array-has-fill-pointer-p (array-has-fill-pointer-p '(x))
type-error)

(deferror-test error-array-rank (array-rank '(x))
type-error)

(deferror-test error-array-total-size (array-total-size '(x))
type-error)

(deferror-test error-ash-ratio (ash 3/4 2)
type-error)

(deferror-test error-ash-float (ash 3 4.0)
type-error)

(deferror-test error-asin (asin 'x)
type-error)

(deferror-test error-asinh (asinh 'x)
type-error)

(deferror-test error-atan (atan 'x)
type-error)

(deferror-test error-atan (atan #c(0 0.4) 3.4)
type-error)

(deferror-test error-atan (atan -4 #c(3 4))
type-error)

(deferror-test error-atanh (atanh 'x)
type-error)

(deferror-test error-boole (boole 'x 3 4)
type-error)

(deferror-test error-boole (boole boole-and 3/4 -7)
type-error)

(deferror-test error-boole (boole boole-set 5 #c(-3 4))
type-error)

(deferror-test error-both-case-p (both-case-p 33)
type-error)

(deferror-test error-boundp (boundp 47)
type-error)

(deferror-test error-butlast (butlast '(a b c) -1)
type-error)

(deferror-test error-butlast (butlast '#(a b c))
type-error)

(deferror-test error-car (car 'x)
type-error)

(deferror-test error-cdr (cdr '#(a b c))
type-error)

(deferror-test error-cdadar (cdadar '((x y)))
type-error)

(deferror-test error-call-next (progn
  (defgeneric foo04 (x))
  (defmethod foo04 ((x real)) 'ok)
  (defmethod foo04 ((x integer)) (call-next-method (sqrt x)))
  (foo04 -1))
error)

(deferror-test error-call-next2 (progn
  (defgeneric foo041 (x))
  (defmethod foo041 ((x real)) 'ok)
  (defmethod foo041 ((x integer)) (call-next-method (sqrt x)))
  (foo04 2))
error)

(deferror-test error-ccase (ccase 'x)
type-error)

(deferror-test error-char-code (char-code 33)
type-error)

(deferror-test error-char-downcase (char-downcase 33)
type-error)

(deferror-test error-char-equal (char-equal)
program-error)

(deferror-test error-char-greaterp (char-greaterp)
program-error)

(deferror-test error-char-lessp (char-lessp)
program-error)

(deferror-test error-char-name (char-name 33)
type-error)

(deferror-test error-char-not-equal (char-not-equal)
program-error)

(deferror-test error-char-not-greaterp (char-not-greaterp)
program-error)

(deferror-test error-char-not-lessp (char-not-lessp)
program-error)

(deferror-test error-char-upcase (char-upcase 33)
type-error)

(deferror-test error-char/= (char/=)
program-error)

(deferror-test error-char< (char<)
program-error)

(deferror-test error-char<= (char<=)
program-error)

(deferror-test error-char= (char=)
program-error)

(deferror-test error-char> (char>)
program-error)

(deferror-test error-char>= (char>=)
program-error)

(deferror-test error-character-string (character "abc")
type-error)

(deferror-test error-character-empty (character "")
type-error)

(deferror-test error-character-integer (character 33)
type-error)

(deferror-test error-clear-input (clear-input '*terminal-io*)
type-error)

(deferror-test error-clear-output (clear-output '*terminal-io*)
type-error)

(deferror-test error-coerce-list-vector-size (coerce '(a b c) '(vector * 4))
type-error)

(deferror-test error-coerce-vector-size (coerce '#(a b c) '(vector * 4))
type-error)

(deferror-test error-coerce-list-vector-size2 (coerce '(a b c) '(vector * 2))
type-error)

(deferror-test error-coerce-vector-size2 (coerce '#(a b c) '(vector * 2))
type-error)

(deferror-test error-coerce-string-size (coerce "foo" '(string 2))
type-error)

(deferror-test error-coerce-string-size2 (coerce '#(#\a #\b #\c) '(string 2))
type-error)

(deferror-test error-coerce-bit-vector-size (coerce '(0 1) '(simple-bit-vector 3))
type-error)

(deferror-test error-coerce-nil (coerce nil 'nil)
type-error)

(deferror-test error-coerce-function (coerce '#:nonexistent 'function)
error)

(deferror-test error-coerce-macro-function (coerce 'and 'function)
error)

(deferror-test error-compile-file (compile-file "/tmp/12836123.lsp")
file-error)

(deferror-test error-concatenate-symbol (concatenate 'symbol)
error)

(deferror-test error-concatenate-size (concatenate '(string 3) "ab" "cd")
type-error)

(deferror-test error-copy-pprint-dispatch (copy-pprint-dispatch 'x)
type-error)

(deferror-test error-copy-seq (copy-seq 'x)
type-error)

(deferror-test error-copy-symbol (copy-symbol #\x)
type-error)

(deferror-test error-cos (cos 'x)
type-error)

(deferror-test error-cosh (cosh 'x)
type-error)

(deferror-test error-count (count #\x 'x)
type-error)

(deferror-test error-ctypecase (let ((x nil)) (ctypecase x))
type-error)

(deferror-test error-decode-float (decode-float 2/3)
type-error)

(deferror-test error-defclass1 (defclass foo05 () (a b a))
program-error)

(deferror-test error-defclass2 (eval '(defclass foo06 () (a b) (:default-initargs x a x b)))
program-error)

(deferror-test error-defclass3 (defclass foo07 () ((a :allocation :class :allocation :class)))
program-error)

(deferror-test error-defclass4 (eval '(defclass foo08 () ((a :initform 42 :initform 42))))
program-error)

(deferror-test error-defclass5 (defclass foo09 () ((a :type real :type real)))
program-error)

(deferror-test error-defclass6 (defclass foo10 () ((a :documentation "bla" :documentation "blabla")))
program-error)

(deferror-test error-defgeneric7 (eval '(defgeneric if (x)))
program-error)

(deferror-test error-macro-defgeneric (progn 
  (defmacro foo11 (x) x)
  (defgeneric foo11 (x)))
program-error)

(deferror-test error-function-defgeneric (progn
  (defun foo12 (x) x)
  (defgeneric foo12 (x)))
program-error)

(deferror-test error-defgeneric (defgeneric foo13 (x y &rest l)
  (:method (x y))
)
error)

(deferror-test error-defgeneric2 (eval '(defgeneric foo14 (x)
  (:documentation "bla")
  (:documentation "blabla")
))
program-error)

(deferror-test error-defgeneric3 (defgeneric foo15 (x)
  (:my-option t))
program-error)

; define-method-combination is too complicated

(deferror-test error-var-symbol-macro (progn
  (defvar foo16)
  (define-symbol-macro foo16 t))
program-error)

(deferror-test error-defmethod (eval '(defmethod if (x) nil))
error)

(deferror-test error-macro-method (progn 
  (defmacro foo17 (x) x)
  (defmethod foo17 (x) nil))
error)

(deferror-test error-function-method (progn
  (defun foo18 (x) x)
  (defmethod foo18 (x) nil))
error)

(deferror-test error-method-bad-args (progn
  (defgeneric foo19 (x))
  (defmethod foo19 (x y) nil))
error)

(deferror-test error-package (progn
  (defpackage "FOO20")
  (defpackage "FOO21" (:nicknames "FOO20")))
package-error)

(deferror-test error-defpackage (eval '(defpackage "FOO22" (:size 20) (:size 20)))
program-error)

(deferror-test error-defpackage2 (eval '(defpackage "FOO23" (:documentation "bla") (:documentation "blabla")))
program-error)

(deferror-test error-defpackage3 (eval '(defpackage "FOO24" (:my-option t)))
program-error)

(deferror-test error-defpackage4 (defpackage "FOO25" (:shadow "IF") (:intern "IF"))
program-error)

(deferror-test error-defpackage5 (defpackage "FOO26" (:shadow "IF") (:import-from "USER" "IF"))
program-error)

(deferror-test error-defpackage6 (defpackage "FOO27" (:shadow "IF") (:shadowing-import-from "USER" "IF"))
program-error)

(deferror-test error-defpackage7 (defpackage "FOO28" (:intern "IF") (:import-from "USER" "IF"))
program-error)

(deferror-test error-defpackage8 (defpackage "FOO29" (:intern "IF") (:shadowing-import-from "USER" "IF"))
program-error)

(deferror-test error-defpackage9 (defpackage "FOO30" (:import-from "USER" "IF") (:shadowing-import-from "USER" "IF"))
program-error)

(deferror-test error-defpackage10 (defpackage "FOO31" (:export "IF") (:intern "IF"))
program-error)

(deferror-test error-defstruct (defstruct foo32 a system::a)
program-error)

(deferror-test error-defstruct2 (progn
  (defstruct foo33 a)
  (defstruct (foo34 (:include foo33)) system::a))
program-error)

(deferror-test error-delete (delete #\x 'x)
type-error)

(deferror-test error-delete-duplicates (delete-duplicates 'abba)
type-error)

(deferror-test error-delete-file
  (let ((path (make-pathname :name "xjunkx" :defaults (user-homedir-pathname))))
    (when (probe-file path) (delete-file path))
    (delete-file path))
file-error)

(deferror-test error-destructuring-bind (destructuring-bind (a) '(1 2) a)
error)

; directory - no way to make a directory search fail

(deferror-test error-disassemble (disassemble #x123456)
type-error)

; dribble - no way to force a file-error

(deferror-test error-ecase (ecase 'x)
type-error)

(deferror-test error-elt-symbol (elt 'x 0)
type-error)

(deferror-test error-elt-past-end (elt "abc" 4)
type-error)

(deferror-test error-elt-past-end-list (elt '(a b c) 4)
type-error)

(deferror-test error-elt-past-end-vector (elt '#(a b c) 4)
type-error)

(deferror-test error-elt-past-end-adjustable (elt (make-array 3 :fill-pointer 3 :adjustable t) 4)
type-error)

(deferror-test error-endp (endp 'x)
type-error)

(deferror-test error-ensure-directories-exist (ensure-directories-exist "/*/")
file-error)

(deferror-test error-error (error 42)
type-error)

(deferror-test error-etypecase (let ((x nil)) (etypecase x))
type-error)

(deferror-test error-every (every '(lambda (x) x) nil)
type-error)

(deferror-test error-every (every #'identity 'x)
type-error)

(deferror-test error-fboundp (fboundp '(psetf aref))
type-error)

(deferror-test error-fdefinition (fdefinition '(psetf aref))
type-error)

(deferror-test error-fdefinition-none (fdefinition '#:nonexistent)
undefined-function)

(deferror-test error-file-author (file-author "*")
file-error)

(deferror-test error-file-length (file-length *terminal-io*)
type-error)

(deferror-test error-with-open-file (with-open-file (s "/tmp/foo35.tmp" :direction :output)
  (file-position s 0.0))
error)

(deferror-test error-with-open-file (with-open-file (s "/tmp/foo35.tmp" :direction :output)
  (file-position s -1))
error)

(deferror-test error-with-open-file2 (with-open-file (s "/tmp/foo35.tmp" :direction :input)
  (file-position s (+ (file-length s) 1000)))
error)

(deferror-test error-file-write-date (file-write-date "*")
file-error)

(deferror-test error-fill (fill 'x #\x)
type-error)

(deferror-test error-fill (fill (make-list 3) 'x :start nil)
type-error)

(deferror-test error-fill (fill (make-list 3) 'x :start -1)
type-error)

(deferror-test error-fill (fill (make-list 3) 'x :start 1 :end -1)
type-error)

(deferror-test error-fill-pointer (fill-pointer "abc")
type-error)

(deferror-test error-find (find #\x 'x)
type-error)

(deferror-test error-find-class (find-class '#:nonexistent t)
error)

(deferror-test error-find-method (progn
  (defgeneric foo36 (x y))
  (find-method #'foo36 nil (list (find-class 'number))))
error)

(deferror-test error-find-method2 (progn
  (defgeneric foo37 (x))
  (find-method #'foo37 nil (list (find-class 'number))))
error)

(deferror-test error-finish-output (finish-output '*terminal-io*)
type-error)

(deferror-test error-float-digits (float-digits 2/3)
type-error)

(deferror-test error-float-precision (float-precision 2/3)
type-error)

(deferror-test error-float-radix (float-radix 2/3)
type-error)

(deferror-test error-float-sign (float-sign 2/3)
type-error)

(deferror-test error-float-sign (float-sign -4.5 2/3)
type-error)

(deferror-test error-fmakunbound (fmakunbound '(psetf aref))
type-error)

(deferror-test error-force-output (force-output '*terminal-io*)
type-error)

(deferror-test error-funcall (funcall 'foo38)
undefined-function)

(deferror-test error-funcall (funcall 'and)
undefined-function)

(deferror-test error-gcd (gcd 4 3/4)
type-error)

(deferror-test error-gensym (gensym #\x)
type-error)

(deferror-test error-gentemp (gentemp 't)
type-error)

(deferror-test error-gentemp (gentemp "X" 24)
type-error)

(deferror-test error-get (get "a" 'x)
type-error)

(deferror-test error-get-dispatch-macro-character (get-dispatch-macro-character #\0 #\#)
error)

(deferror-test error-graphic-char-p (graphic-char-p 33)
type-error)

(deferror-test error-hash-table-rehash-size (hash-table-rehash-size *readtable*)
type-error)

(deferror-test error-hash-table-rehash-threshold (hash-table-rehash-threshold *package*)
type-error)

(deferror-test error-hash-table-size (hash-table-size *random-state*)
type-error)

(deferror-test error-hash-table-test (hash-table-test '#(a b c))
type-error)

(deferror-test error-imagpart (imagpart #\c)
type-error)

(deferror-test error-in-package (in-package "FOO39")
package-error)

(deferror-test error-input-stream-p (input-stream-p (pathname "abc"))
type-error)

(deferror-test error-integer-decode-float (integer-decode-float 2/3)
type-error)

(deferror-test error-integer-length (integer-length 0.0)
type-error)

(deferror-test error-interactive-stream-p (interactive-stream-p (pathname "abc"))
type-error)

(deferror-test error-invoke-restart (invoke-restart 'foo40)
control-error)

(deferror-test error-invoke-restart-interactively (invoke-restart-interactively 'foo41)
control-error)

(deferror-test error-isqrt (isqrt -1)
type-error)

(deferror-test error-isqrt (isqrt #c(3 4))
type-error)

(deferror-test error-last (last '(a b c) -1)
type-error)

(deferror-test error-lcm (lcm 4/7 8)
type-error)

(deferror-test error-length (length 'x)
type-error)

(deferror-test error-list-length (list-length 'x)
type-error)

(deferror-test error-list-length (list-length '(x . y))
type-error)

(deferror-test error-load (load "/tmp/128347234.lsp")
file-error)

(deferror-test error-load (load "*.lsp")
file-error)

(deferror-test error-load-logical-pathname-translations (load-logical-pathname-translations "FOO41")
error)

(deferror-test error-logand (logand -3 2.3)
type-error)

(deferror-test error-logbitp (logbitp -1 5)
type-error)

(deferror-test error-logbitp (logbitp 2 3/7)
type-error)

(deferror-test error-logcount (logcount #*01010011)
type-error)

(deferror-test error-logical-pathname (logical-pathname '#(#\A #\B))
type-error)

(deferror-test error-logical-pathname-translations (logical-pathname-translations '#(#\A #\B))
type-error)

(deferror-test error-lower-case-p (lower-case-p 33)
type-error)

(deferror-test error-make-broadcast-stream (make-broadcast-stream (make-string-input-stream "abc"))
type-error)

(deferror-test error-make-concatenated-stream (make-concatenated-stream (make-string-output-stream))
type-error)

(deferror-test error-make-instance (progn
  (defclass foo42 () ())
  (make-instance 'foo42 :x 1))
error)

(deferror-test error-make-list (make-list -1)
type-error)

(deferror-test error-make-load-form (progn
  (defstruct foo43)
  (make-load-form (make-foo43)))
error)

(deferror-test error-make-random-state (make-random-state 'x)
type-error)

(deferror-test error-make-sequence (make-sequence 'x 5)
type-error)

(deferror-test error-make-sequence2 (make-sequence 'sequence 5)
type-error)

(deferror-test error-make-sequence3 (make-sequence '(string 3) 4)
type-error)

(deferror-test error-make-symbol (make-symbol 'x)
type-error)

(deferror-test error-make-synonym-stream (make-synonym-stream *terminal-io*)
type-error)

(deferror-test error-make-two-way-stream (make-two-way-stream (make-string-input-stream "abc") (make-string-input-stream "def"))
type-error)

(deferror-test error-make-two-way-stream (make-two-way-stream (make-string-output-stream) (make-string-output-stream))
type-error)

(deferror-test error-makunbound (makunbound "xx")
type-error)

(deferror-test error-map (map 'x #'identity "abc")
type-error)

(deferror-test error-map (map '(string 3) #'identity "ab")
type-error)

(deferror-test error-max (max 3 #c(4 0.0))
type-error)

(deferror-test error-merge (merge '(vector * 5) '(3 1) '(2 4) #'<)
type-error)

(deferror-test error-min (min 3 #c(4 0.0))
type-error)

(deferror-test error-minusp (minusp #c(4 -3/4))
type-error)

(deferror-test error-muffle-warning (muffle-warning)
control-error)

(deferror-test error-name-char (name-char '#(#\N #\u #\l))
type-error)

(deferror-test error-nbutlast (nbutlast '(a b c) -1)
type-error)

(deferror-test error-nbutlast (nbutlast '#(a b c))
type-error)

(deferror-test error-no-applicable-method (no-applicable-method #'cons)
error)

(deferror-test error-no-next-method (no-next-method #'print-object (find-method #'print-object nil (list (find-class 'standard-object) (find-class 't))))
error)

(deferror-test error-notany (notany '(lambda (x) x) nil)
type-error)

(deferror-test error-notany (notany #'identity 'x)
type-error)

(deferror-test error-notevery (notevery '(lambda (x) x) nil)
type-error)

(deferror-test error-notevery (notevery #'identity 'x)
type-error)

(deferror-test error-nthcdr (nthcdr 2 '(a . b))
type-error)

(deferror-test error-oddp (oddp 3.5)
type-error)

(deferror-test error-open (open "/etc/mtab" :direction :input :if-exists :error)
nil)

(deferror-test error-open (open "/tmp/foo44nonexistent" :direction :input :if-does-not-exist :error)
file-error)

(deferror-test error-open (open "/tmp/*" :direction :input)
file-error)

(deferror-test error-open (open "/etc/mtab" :direction :input :external-format 'mtab-entries)
error)

(deferror-test error-open-stream-p (open-stream-p (pathname "foo45"))
type-error)

(deferror-test error-output-stream-p (output-stream-p (pathname "foo46"))
type-error)

(deferror-test error-package-name (package-name 47)
type-error)

(deferror-test error-package-nicknames (package-nicknames (pathname "foo47"))
type-error)

(deferror-test error-package-shadowing-symbols (package-shadowing-symbols (vector 'a 'b 'c))
type-error)

(deferror-test error-package-use-list (package-use-list (list 'a 'b 'c))
type-error)

(deferror-test error-package-used-by-list (package-used-by-list (list 'a 'b 'c))
type-error)

(deferror-test error-parse-integer (parse-integer "x-y")
error)

(deferror-test error-parse-namestring (parse-namestring (coerce (list #\f #\o #\o (code-char 0) #\4 #\8) 'string))
parse-error)

(deferror-test error-parse-namestring (parse-namestring "foo48:a" (logical-pathname "foo49:"))
error)

(deferror-test error-pathname-match-p (pathname-match-p 34 "*")
type-error)

(deferror-test error-pathname-match-p (pathname-match-p "x" 34)
type-error)

(deferror-test error-peek-char (peek-char nil (make-string-input-stream "") t)
end-of-file)

(deferror-test error-peek-char (peek-char #\space (make-string-input-stream "") t)
end-of-file)

(deferror-test error-peek-char (peek-char nil (make-string-input-stream "") nil nil t)
end-of-file)

(deferror-test error-phase (phase 'x)
type-error)

(deferror-test error-plusp (plusp #c(0 4.2))
type-error)

(deferror-test error-pprint-dispatch (pprint-dispatch nil t)
type-error)

(deferror-test error-pprint-exit-if-list-exhausted (pprint-exit-if-list-exhausted)
error)

(deferror-test error-pprint-indent (pprint-indent nil 2)
error)

(deferror-test error-let (let ((x (make-string-output-stream)))
  (pprint-logical-block (x nil :prefix 24)))
type-error)

(deferror-test error-let (let ((x (make-string-output-stream)))
  (pprint-logical-block (x nil :prefix "a" :per-line-prefix "b")))
error)

(deferror-test error-pprint-newline (pprint-newline :fresh)
type-error)

(deferror-test error-pprint-pop (pprint-pop)
error)

(deferror-test error-pprint-tab (pprint-tab :paragraph 0 1)
error)

(deferror-test error-let (let ((*print-readably* t)) (print-unreadable-object (nil *standard-output*)))
print-not-readable)

(deferror-test error-probe-file (probe-file "*")
file-error)

(deferror-test error-provide (provide 25)
type-error)

(deferror-test error-random (random -2.3)
type-error)

(deferror-test error-rational (rational #c(2.4 -0.3))
type-error)

(deferror-test error-rationalize (rationalize #c(2.4 -0.3))
type-error)

(deferror-test error-read (read (make-string-input-stream "((a b)") nil)
end-of-file)

(deferror-test error-read (read (make-string-input-stream " ") t)
end-of-file)

(deferror-test error-read-byte (read-byte (pathname "foo50"))
type-error)

(deferror-test error-read-byte (read-byte (make-string-input-stream "abc"))
error)

(deferror-test error-let (let ((filename "/tmp/foo51.bin"))
  (with-open-file (s filename :direction :output
                              :if-exists :overwrite
                              :if-does-not-exist :create))
  (with-open-file (s filename :direction :input
                              :element-type '(unsigned-byte 8))
    (read-byte s t)))
end-of-file)

(deferror-test error-let (let ((filename "/tmp/foo52.txt"))
  (with-open-file (s filename :direction :output
                              :if-exists :overwrite
                              :if-does-not-exist :create))
  (with-open-file (s filename :direction :input)
    (read-char s t)))
end-of-file)

(deferror-test error-let (let ((filename "/tmp/foo53.txt"))
  (with-open-file (s filename :direction :output
                              :if-exists :overwrite
                              :if-does-not-exist :create))
  (with-open-file (s filename :direction :input)
    (read-char-no-hang s t)))
end-of-file)

(deferror-test error-read-from-string (read-from-string "((a b))" nil nil :end 6)
end-of-file)

(deferror-test error-read-from-string (read-from-string " () () " t nil :start 3 :end 4)
end-of-file)

(deferror-test error-read-line (read-line (make-string-input-stream "") t)
end-of-file)

(deferror-test error-read-sequence (read-sequence (list 1 2 3) (make-string-input-stream "") :start nil)
type-error)

(deferror-test error-read-sequence (read-sequence (list 1 2 3) (make-string-input-stream "") :end -1)
type-error)

(deferror-test error-readtable-case (readtable-case nil)
type-error)

(deferror-test error-setf (setf (readtable-case *readtable*) ':unknown)
type-error)

(deferror-test error-realpart (realpart #\c)
type-error)

(deferror-test error-reinitialize (progn
  (defclass foo54 () ())
  (reinitialize-instance (make-instance 'foo54) :dummy 0))
error)

(deferror-test error-remove (remove #\x 'x)
type-error)

(deferror-test error-remove-duplicates (remove-duplicates 'abba)
type-error)

(deferror-test error-remprop (remprop 55 'abc)
type-error)

(deferror-test error-rplaca (rplaca nil 5)
type-error)

(deferror-test error-rplacd (rplacd nil 5)
type-error)

(deferror-test error-scale-float (scale-float 2/3 -1)
type-error)

(deferror-test error-scale-float (scale-float 3.4 1.0)
type-error)

(deferror-test error-set-dispatch-macro-character (set-dispatch-macro-character #\0 #\# #'(lambda (s c n) (loop)))
error)

(deferror-test error-set-pprint-dispatch (set-pprint-dispatch '(vector * 2) nil #c(3 4))
error)

(deferror-test error-sin (sin 'x)
type-error)

(deferror-test error-sinh (sinh 'x)
type-error)

(deferror-test error-sleep (sleep -1)
type-error)

(deferror-test error-slot-boundp (progn
  (defclass foo55 () (a))
  (slot-boundp (make-instance 'foo55) ':a))
error)

(deferror-test error-slot-makunbound (progn
  (defclass foo56 () (a))
  (slot-makunbound (make-instance 'foo56) ':a))
error)

(deferror-test error-slot-missing (slot-missing (find-class 't) nil ':a 'setf)
error)

(deferror-test error-slot-unbound (slot-unbound (find-class 't) nil ':a)
unbound-slot)

(deferror-test error-slot-value (progn
  (defclass foo57 () (a))
  (slot-value (make-instance 'foo57) ':a))
error)

(deferror-test error-some (some '(lambda (x) x) nil)
type-error)

(deferror-test error-some (some #'identity 'x)
type-error)

(deferror-test error-special-operator-p (special-operator-p '(and x y))
type-error)

(deferror-test error-special-operator-p (special-operator-p '(setf aref))
type-error)

(deferror-test error-sqrt (sqrt 'x)
type-error)

(deferror-test error-standard-char-p (standard-char-p 33)
type-error)

(deferror-test error-stream-element-type (stream-element-type '*terminal-io)
type-error)

(deferror-test error-string (string 33)
type-error)

(deferror-test error-symbol-function (symbol-function 33)
type-error)

(deferror-test error-symbol-function (symbol-function ':compile)
undefined-function)

(deferror-test error-symbol-macrolet (symbol-macrolet ((t true)))
program-error)

(deferror-test error-symbol-macrolet (symbol-macrolet ((*print-pretty* (stream-print-pretty *standard-output*))))
program-error)

(deferror-test error-symbol-macrolet (symbol-macrolet ((foo58 t)) (declare (special foo58)))
program-error)

(deferror-test error-symbol-name (symbol-name '(setf foo59))
type-error)

(deferror-test error-symbol-package (symbol-package '(setf foo59))
type-error)

(deferror-test error-symbol-plist (symbol-plist '(setf foo59))
type-error)

(deferror-test error-symbol-value (symbol-value '(setf foo59))
type-error)

(deferror-test error-symbol-value (symbol-value '#:nonexistent)
unbound-variable)

(deferror-test error-tan (tan 'x)
type-error)

(deferror-test error-tanh (tanh 'x)
type-error)

(deferror-test error-throw (throw '#:nonexistent nil)
control-error)

(deferror-test error-translate-logical-pathname (translate-logical-pathname (make-broadcast-stream))
type-error)

(deferror-test error-translate-logical-pathname (translate-logical-pathname (logical-pathname "foo61:"))
file-error)

(deferror-test error-translate-pathname (translate-pathname 'x "x" "y")
type-error)

(deferror-test error-translate-pathname (translate-pathname "a" '* '*)
type-error)

(deferror-test error-translate-pathname (translate-pathname "x" "y" "z")
error)

(deferror-test error-truename (truename "/tmp/foo62nonexistent")
file-error)

(deferror-test error-truename (truename "/tmp/*/x")
file-error)

(deferror-test error-typep-values (typep nil 'values)
error)

(deferror-test error-typep-values-list-form (typep #'cons '(values t))
error)

(deferror-test error-typep-function-list-form (typep #'cons '(function (t t) list))
error)

(deferror-test error-unexport (unexport ':foo63)
package-error)

(deferror-test error-unintern (progn
  (defpackage "FOO64" (:export "XYZ"))
  (defpackage "FOO65" (:export "XYZ"))
  (defpackage "FOO66" (:use "FOO64" "FOO65") (:shadow "XYZ"))
  (unintern (find-symbol "XYZ" (find-package "FOO66")) (find-package "FOO66")))
error)

; update-instance-for-different-class too complicated

; update-instance-for-redefined-class too complicated

(deferror-test error-upper-case-p (upper-case-p 33)
type-error)

(deferror-test error-values-list (values-list '(a b . c))
type-error)

(deferror-test error-vector-pop (vector-pop "foo67")
type-error)

(deferror-test error-vector-pop (vector-pop (make-array 10 :fill-pointer 0))
error)

(deferror-test error-vector-push (vector-push 'x (make-array 10))
error)

(deferror-test error-let (let ((a (make-array 5 :fill-pointer 0)))
  (dotimes (i 100) (vector-push-extend 'x a)))
error)

(deferror-test error-warn (warn (make-condition 'error))
type-error)

(deferror-test error-warn (warn (make-condition 'warning) "x")
type-error)

(deferror-test error-warn (warn 'error)
type-error)

(deferror-test error-wild-pathname-p (wild-pathname-p #\x)
type-error)

(deferror-test error-write-byte (write-byte 1 (pathname "foo67"))
type-error)

(deferror-test error-write-byte (write-byte 1 (make-string-output-stream))
error)

(deferror-test error-write-sequence (write-sequence '(#\1 #\2 #\3) (make-string-output-stream) :start nil)
type-error)

(deferror-test error-write-sequence (write-sequence '(#\1 #\2 #\3) (make-string-output-stream) :end -1)
type-error)

(deferror-test error-zerop (zerop 'x)
type-error)
