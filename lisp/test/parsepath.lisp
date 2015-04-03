#| 
Bugs:
- Namestrings never print with quotes, though sometimes they are needed.
|#

(eval-when (:compile-toplevel :execute)
  (declaim (special sample-logical)))

(let ((trans 
      `(("SITE;"  ,(make-pathname :name nil :type nil :version nil
				  :defaults *default-pathname-defaults*)))))
     (setf (logical-pathname-translations "HOST") trans)
     (setf (logical-pathname-translations "1") trans)
     (setf (logical-pathname-translations "-") trans))


(setf (eclipse::host-pathname-class "2") 'eclipse::unix-pathname)
(setf (eclipse::host-pathname-class "-x") 'eclipse::unix-pathname)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tests parsing and printing of namestrings.  This is
;;; implementation-specific for non-logical pathnames and for printing
;;; pathnames  which have no parseable namestring or which produce
;;; namestrings which might be parsed ambiguously.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Creates the named test which parses string into a pathname of the
;;; specified type.  The components are the expected host, device,
;;; etc. component values.
(defmacro test-pathname-parse (name type string &body components)
  `(deftest ,name
     (list-components
      (parse-namestring ,string nil (class-prototype (find-class ',type))))
     ,components))

;;; Creates the named test in which we make sure that parsing string
;;; as the specified type of pathname ceases before the end of the
;;; string. 
(defmacro test-pathname-failure (name type string)
  `(deftest ,name
       (let ((string ,string))
	 (multiple-value-bind (val term)
	     (parse-namestring ,string nil (class-prototype (find-class ',type))
			       :junk-allowed t)
	   (declare (ignore val))
	   (< term (length string))))
     t))

;;; Creates the named test in which the pathname is printed to a
;;; string with the specified values of readably and escape.  The
;;; string is compared to result.
(defmacro test-pathname-print (name pathname result
			       &key readably escape)
  `(deftest ,name (write-to-string ,pathname :readably ,readably
				   :escape ,escape)
     ,result))

(test-pathname-print print-non-default-device
		     (make-pathname :host nil :device :weird)
		     "#S(PATHNAME :HOST NIL :DEVICE :WEIRD)"
		     :readably t)
(test-pathname-print print-default-device
		     (make-pathname :host nil :device :unspecific)
		     "#S(PATHNAME :HOST NIL :DEVICE :UNSPECIFIC)"
		     :readably t)
(test-pathname-print print-non-default-version
		     (make-pathname :host nil :version 9)
		     "#S(PATHNAME :HOST NIL :DEVICE :UNSPECIFIC :VERSION 9)"
		     :readably t)
(test-pathname-print print-default-version
		     (make-pathname :host nil :version :newest)
		     "#S(PATHNAME :HOST NIL :DEVICE :UNSPECIFIC)"
		     :readably t)
(deftest print-logical-readably
  (let ((p (read-from-string (write-to-string sample-logical :readably t :escape nil))))
    (equal p sample-logical))
  t)
(test-pathname-print print-partial-logical-readably
		     (pathname "sys:foo")
		     "#S(PATHNAME :HOST \"SYS\" :DEVICE :UNSPECIFIC :NAME \"FOO\" :VERSION NIL)"
		     :readably t)
(test-pathname-print print-logical-escaped
		     sample-logical
		     "#P\"MYSYS:SITE;JUNK.LISP.99\""
		     :escape t)
(test-pathname-print print-partial-logical-escaped
		     (pathname "sys:foo")
		     "#P\"SYS:FOO\""
		     :escape t)
(test-pathname-print print-logical
		     sample-logical
		     "\"MYSYS:SITE;JUNK.LISP.99\"")
(test-pathname-print print-partial-logical
		     (pathname "sys:foo")
		     "\"SYS:FOO\"")
(test-pathname-print print-illegal-escape
		     (make-pathname :host "sys" :device :weird)
		     "#S(PATHNAME :HOST \"SYS\" :DEVICE :WEIRD)"
		     :escape t)
(test-pathname-print print-always-host
		     (make-pathname :host nil)
		     "#S(PATHNAME :HOST NIL :DEVICE :UNSPECIFIC)"
		     :readably t)
(test-pathname-print print-unix
		     (parse-namestring "*:foo/**/bar/baz/../blue/*/*.*.*"
				     "unix-host")
		     "\"*:foo/**/bar/baz/../blue/*/*.*.*\"")
(test-pathname-print print-unix-uppercase
		     (parse-namestring "/FOO/FILE.LISP.EXT"
				     "unix-host")
		     "\"unix-host:/FOO/FILE.LISP.EXT\"")
(test-pathname-print print-unix-lowercase
		     (parse-namestring "/foo/file.lisp.ext"
				     "unix-host")
		     "\"unix-host:/foo/file.lisp.ext\"")
(test-pathname-print print-unix-mixedcase
		     (parse-namestring "/fOO/fILE.lISP.eXT"
				     "unix-host")
		     "\"unix-host:/fOO/fILE.lISP.eXT\"")

;;; LOGICAL PATHNAMES
(test-pathname-parse logical-host-empty
		     logical-pathname
		     ""
		     nil :unspecific nil nil nil nil)

(test-pathname-parse logical-host
		     logical-pathname
		     "foo:"
		     "FOO" :unspecific nil nil nil nil)

(test-pathname-parse logical-wild
		     logical-pathname
		     "*:"
		     :wild :unspecific nil nil nil nil)

(test-pathname-parse logical-host-relative
		     logical-pathname
		     ";"
		     nil :unspecific (:relative) nil nil nil)

(test-pathname-parse logical-host-absolute
		     logical-pathname
		     "foo;"
		     nil :unspecific (:absolute "FOO") nil nil nil)

(test-pathname-parse logical-host-relative-multi
		     logical-pathname
		     ";foo;*;bar;**;"
		     nil :unspecific (:relative "FOO" :wild "BAR" :wild-inferiors) nil nil nil)

(test-pathname-parse logical-host-absolute-multi
		     logical-pathname
		     "foo;*;bar;**;"
		     nil :unspecific (:absolute "FOO" :wild "BAR" :wild-inferiors) nil nil nil)

(test-pathname-parse logical-host-relative-wild
		     logical-pathname
		     ";*;"
		     nil :unspecific (:relative :wild) nil nil nil)

(test-pathname-parse logical-host-absolute-wild
		     logical-pathname
		     "*;"
		     nil :unspecific (:absolute :wild) nil nil nil)

(test-pathname-parse logical-host-relative-wild-inferiors
		     logical-pathname
		     ";**;"
		     nil :unspecific (:relative :wild-inferiors) nil nil nil)

(test-pathname-parse logical-host-absolute-wild-inferiors
		     logical-pathname
		     "**;"
		     nil :unspecific (:absolute :wild-inferiors) nil nil nil)

(test-pathname-parse logical-host-relative-wild-wild-inferiors
		     logical-pathname
		     ";*;**;"
		     nil :unspecific (:relative :wild :wild-inferiors) nil nil nil)

(test-pathname-parse logical-host-absolute-wild-wild-inferiors
		     logical-pathname
		     "*;**;"
		     nil :unspecific (:absolute :wild :wild-inferiors) nil nil nil)

(test-pathname-parse logical-host-relative-not-completely-wild
		     logical-pathname
		     ";*foo;bar*;"
		     nil :unspecific (:relative "*FOO" "BAR*") nil nil nil)

(test-pathname-parse logical-host-absolute-not-completely-wild
		     logical-pathname
		     "*foo;bar*;"
		     nil :unspecific (:absolute "*FOO" "BAR*") nil nil nil)

(test-pathname-parse logical-host-file
		     logical-pathname
		     "file"
		     nil :unspecific nil "FILE" nil nil)

(test-pathname-parse logical-host-wild-file
		     logical-pathname
		     "*"
		     nil :unspecific nil :wild nil nil)

(test-pathname-parse logical-host-not-comletely-wild-file1
		     logical-pathname
		     "*file"
		     nil :unspecific nil "*FILE" nil nil)

(test-pathname-parse logical-host-not-comletely-wild-file2
		     logical-pathname
		     "file*"
		     nil :unspecific nil "FILE*" nil nil)

(test-pathname-parse logical-host-type
		     logical-pathname
		     ".type"
		     nil :unspecific nil nil "TYPE" nil)

(test-pathname-parse logical-host-wild-type
		     logical-pathname
		     ".*"
		     nil :unspecific nil nil :wild nil)

(test-pathname-parse logical-host-not-comletely-wild-type1
		     logical-pathname
		     ".*type"
		     nil :unspecific nil nil "*TYPE" nil)

(test-pathname-parse logical-host-not-comletely-wild-type2
		     logical-pathname
		     ".type*"
		     nil :unspecific nil nil "TYPE*" nil)

(test-pathname-parse logical-host-version
		     logical-pathname
		     ".type.9"
		     nil :unspecific nil nil "TYPE" 9)

(test-pathname-parse logical-host-wild-version
		     logical-pathname
		     ".*.*"
		     nil :unspecific nil nil :wild :wild)

(test-pathname-parse logical-host-newest-version
		     logical-pathname
		     ".type.newest"
		     nil :unspecific nil nil "TYPE" :newest)

(test-pathname-parse logical-host-name-type-version
		     logical-pathname
		     "foo.lisp.9"
		     nil :unspecific NIL "FOO" "LISP" 9)

(test-pathname-parse logical-host-name-type-version-wild
		     logical-pathname
		     "*.*.*"
		     nil :unspecific nil :wild :wild :wild)

(test-pathname-parse logical-host-absolute-name-type-version
		     logical-pathname
		     "dir;foo.lisp.9"
		     nil :unspecific (:absolute "DIR") "FOO" "LISP" 9)

(test-pathname-parse logical-host-relative-name-type-version
		     logical-pathname
		     ";dir;foo.lisp.9"
		     nil :unspecific (:relative "DIR") "FOO" "LISP" 9)

(test-pathname-parse logical-host-host-absolute-name-type-version
		     logical-pathname
		     "host:dir;foo.lisp.9"
		     "HOST" :unspecific (:absolute "DIR") "FOO" "LISP" 9)

(test-pathname-parse logical-host-host-relative-name-type-version
		     logical-pathname
		     "host:;dir;foo.lisp.9"
		     "HOST" :unspecific (:relative "DIR") "FOO" "LISP" 9)

(test-pathname-parse logical-host-host-absolute
		     logical-pathname
		     "host:dir;"
		     "HOST" :unspecific (:absolute "DIR") nil nil nil)

(test-pathname-parse logical-host-host-relative
		     logical-pathname
		     "host:;dir;"
		     "HOST" :unspecific (:relative "DIR") nil nil nil)

(test-pathname-parse logical-host-host-relative-single
		     logical-pathname
		     "host:;"
		     "HOST" :unspecific (:relative) nil nil nil)

(test-pathname-parse logical-host-host-name-type-version
		     logical-pathname
		     "host:foo.lisp.9"
		     "HOST" :unspecific nil "FOO" "LISP" 9)

(test-pathname-parse logical-host-host-name-type-version-numbers
		     logical-pathname
		     "1:2;3.4.5"
		     "1" :unspecific (:absolute "2") "3" "4" 5)

(test-pathname-parse logical-host-host-name-type-version-dashes
		     logical-pathname
		     "-:-;-.-.5"
		     "-" :unspecific (:absolute "-") "-" "-" 5)

(test-pathname-parse logical-host-uppercase
		     logical-pathname
		     "HOST:DIR;FOO.LISP.9"
		     "HOST" :unspecific (:absolute "DIR") "FOO" "LISP" 9)

(test-pathname-parse logical-host-mixed-case
		     logical-pathname
		     "Host:Dir;Foo.Lisp.9"
		     "HOST" :unspecific (:absolute "DIR") "FOO" "LISP" 9)
;;; Errors
(test-pathname-failure logical-host-slash-host logical-pathname
		       "foo/bar:")
(test-pathname-failure logical-host-slash-dir logical-pathname
		       "foo/bar;")
(test-pathname-failure logical-host-slash-name logical-pathname
		       "foo/bar")
(test-pathname-failure logical-host-slash-type logical-pathname
		       ".foo/bar")
(test-pathname-failure logical-host-slash-version logical-pathname
		       ".lisp.foo/bar")
(test-pathname-failure logical-host-escape-host logical-pathname
		       "foo\\bar:")
(test-pathname-failure logical-host-escape-dir logical-pathname
		       "foo\\bar;")
(test-pathname-failure logical-host-escape-name logical-pathname
		       "foo\\bar")
(test-pathname-failure logical-host-escape-type logical-pathname
		       ".foo\\bar")
(test-pathname-failure logical-host-escape-version logical-pathname
		       ".lisp.foo\\bar")
(test-pathname-failure logical-host-host-dot logical-pathname
		       "foo.bar:")
(test-pathname-failure logical-host-dir-dot logical-pathname
		       "foo.bar;")
(test-pathname-failure logical-host-name-dot logical-pathname
		       "foo.")
(test-pathname-failure logical-host-name-double-dot1 logical-pathname
		       "..")
(test-pathname-failure logical-host-name-double-dot2 logical-pathname
		       ".")
(test-pathname-failure logical-host-too-many-parts logical-pathname
		       "foo.bar.lisp.9")
;; It might not be the end of the world if we happened to permit these.
(test-pathname-failure logical-host-missing-version logical-pathname
		       "..9")
(test-pathname-failure logical-host-name-missing-version logical-pathname
		       "file..9")

		
#|
;;; These are accidental "extensions" to the allowable syntax which we
;;; don't bother to exclude:
(test-pathname-parse logical-host-keyword-version
		     logical-pathname
		     ".type.foo"
		     nil :unspecific nil nil "TYPE" :foo)

(test-pathname-parse logical-host-keyword-version1
		     logical-pathname
		     ".type.*9"
		     nil :unspecific nil nil "TYPE" :*9

(test-pathname-parse logical-host-keyword-version2
		     logical-pathname
		     ".type.9*"
		     nil :unspecific nil nil "TYPE" :9*)
|#


(test-pathname-parse unix-host-empty
		     eclipse::unix-pathname
		     ""
		     nil :unspecific nil nil nil nil)

(test-pathname-parse unix-host
		     eclipse::unix-pathname
		     "foo:"
		     "FOO" :unspecific nil nil nil nil)

(test-pathname-parse unix-wild
		     eclipse::unix-pathname
		     "*:"
		     :wild :unspecific nil nil nil nil)

(test-pathname-parse unix-host-relative
		     eclipse::unix-pathname
		     "./"
		     nil :unspecific (:relative) nil nil nil)

(test-pathname-parse unix-host-absolute
		     eclipse::unix-pathname
		     "/foo/"
		     nil :unspecific (:absolute "FOO") nil nil nil)

(test-pathname-parse unix-host-relative-multi
		     eclipse::unix-pathname
		     "foo/*/bar/**/"
		     nil :unspecific (:relative "FOO" :wild "BAR" :wild-inferiors) nil nil nil)

(test-pathname-parse unix-host-absolute-multi
		     eclipse::unix-pathname
		     "/foo/*/bar/**/"
		     nil :unspecific (:absolute "FOO" :wild "BAR" :wild-inferiors) nil nil nil)

(test-pathname-parse unix-host-absolute-multi-up
		     eclipse::unix-pathname
		     "/foo/././bar/../../*/baz/**/"
		     nil :unspecific
		     (:absolute "FOO" "BAR" :up :up :wild "BAZ" :wild-inferiors) nil nil nil)

(test-pathname-parse unix-host-relative-wild
		     eclipse::unix-pathname
		     "*/"
		     nil :unspecific (:relative :wild) nil nil nil)

(test-pathname-parse unix-host-absolute-wild
		     eclipse::unix-pathname
		     "/*/"
		     nil :unspecific (:absolute :wild) nil nil nil)

(test-pathname-parse unix-host-relative-wild-inferiors
		     eclipse::unix-pathname
		     "**/"
		     nil :unspecific (:relative :wild-inferiors) nil nil nil)

(test-pathname-parse unix-host-absolute-wild-inferiors
		     eclipse::unix-pathname
		     "/**/"
		     nil :unspecific (:absolute :wild-inferiors) nil nil nil)

(test-pathname-parse unix-host-relative-wild-wild-inferiors
		     eclipse::unix-pathname
		     "*/**/"
		     nil :unspecific (:relative :wild :wild-inferiors) nil nil nil)

(test-pathname-parse unix-host-absolute-wild-wild-inferiors
		     eclipse::unix-pathname
		     "/*/**/"
		     nil :unspecific (:absolute :wild :wild-inferiors) nil nil nil)

(test-pathname-parse unix-host-relative-not-completely-wild
		     eclipse::unix-pathname
		     "*foo/bar*/"
		     nil :unspecific (:relative "*FOO" "BAR*") nil nil nil)

(test-pathname-parse unix-host-absolute-not-completely-wild
		     eclipse::unix-pathname
		     "/*foo/bar*/"
		     nil :unspecific (:absolute "*FOO" "BAR*") nil nil nil)

(test-pathname-parse unix-host-just-dot
		     eclipse::unix-pathname
		     "."
		     nil :unspecific nil nil nil nil)

(test-pathname-parse unix-host-absolute-dot
		     eclipse::unix-pathname
		     "/."
		     nil :unspecific (:absolute) nil nil nil)

(test-pathname-parse unix-host-just-double-dot
		     eclipse::unix-pathname
		     ".."
		     nil :unspecific (:relative :up) nil nil nil)


(test-pathname-parse unix-host-file
		     eclipse::unix-pathname
		     "file"
		     nil :unspecific nil "FILE" nil nil)

(test-pathname-parse unix-host-wild-file
		     eclipse::unix-pathname
		     "*"
		     nil :unspecific nil :wild nil nil)

(test-pathname-parse unix-host-not-comletely-wild-file1
		     eclipse::unix-pathname
		     "*file"
		     nil :unspecific nil "*FILE" nil nil)

(test-pathname-parse unix-host-not-comletely-wild-file2
		     eclipse::unix-pathname
		     "file*"
		     nil :unspecific nil "FILE*" nil nil)

(test-pathname-parse unix-host-type
		     eclipse::unix-pathname
		     "file.type"
		     nil :unspecific nil "FILE" "TYPE" nil)

(test-pathname-parse unix-host-wild-type
		     eclipse::unix-pathname
		     "*.*"
		     nil :unspecific nil :wild :wild nil)

(test-pathname-parse unix-host-not-comletely-wild-type1
		     eclipse::unix-pathname
		     "file.*type"
		     nil :unspecific nil "FILE" "*TYPE" nil)

(test-pathname-parse unix-host-not-comletely-wild-type2
		     eclipse::unix-pathname
		     "file.type*"
		     nil :unspecific nil "FILE" "TYPE*" nil)

(test-pathname-parse unix-host-dot-file
		     eclipse::unix-pathname
		     ".type"
		     nil :unspecific nil ".TYPE" nil nil)

(test-pathname-parse unix-host-dot-file2
		     eclipse::unix-pathname
		     ".type.9"
		     nil :unspecific nil ".TYPE" nil 9)

(test-pathname-parse unix-host-wild-dot-file
		     eclipse::unix-pathname
		     ".*.*"
		     nil :unspecific nil ".*" nil :wild)

(test-pathname-parse unix-host-double-dot-file
		     eclipse::unix-pathname
		     "file..9"
		     nil :unspecific nil "FILE" nil 9)

(test-pathname-parse unix-host-double-dot-file2
		     eclipse::unix-pathname
		     "..9"
		     nil :unspecific nil nil nil 9)

(test-pathname-parse unix-host-name-type-version
		     eclipse::unix-pathname
		     "foo.lisp.9"
		     nil :unspecific NIL "FOO" "LISP" 9)

(test-pathname-parse unix-host-name-type-version-wild
		     eclipse::unix-pathname
		     "*.*.*"
		     nil :unspecific nil :wild :wild :wild)

(test-pathname-parse unix-host-not-name-type-version
		     eclipse::unix-pathname
		     "bar.foo.lisp.9"
		     nil :unspecific NIL "BAR.FOO" "LISP" 9)

(test-pathname-parse unix-host-absolute-name-type-version
		     eclipse::unix-pathname
		     "/dir/foo.lisp.9"
		     nil :unspecific (:absolute "DIR") "FOO" "LISP" 9)

(test-pathname-parse unix-host-relative-name-type-version
		     eclipse::unix-pathname
		     "dir/foo.lisp.9"
		     nil :unspecific (:relative "DIR") "FOO" "LISP" 9)

(test-pathname-parse unix-host-host-absolute-name-type-version
		     eclipse::unix-pathname
		     "unix-host:/dir/foo.lisp.9"
		     "UNIX-HOST" :unspecific (:absolute "DIR") "FOO" "LISP" 9)

(test-pathname-parse unix-host-host-relative-name-type-version
		     eclipse::unix-pathname
		     "unix-host:dir/foo.lisp.9"
		     "UNIX-HOST" :unspecific (:relative "DIR") "FOO" "LISP" 9)

(test-pathname-parse unix-host-host-absolute
		     eclipse::unix-pathname
		     "unix-host:/dir/"
		     "UNIX-HOST" :unspecific (:absolute "DIR") nil nil nil)

(test-pathname-parse unix-host-host-relative
		     eclipse::unix-pathname
		     "unix-host:dir/"
		     "UNIX-HOST" :unspecific (:relative "DIR") nil nil nil)

(test-pathname-parse unix-host-host-absolute-single
		     eclipse::unix-pathname
		     "unix-host:/"
		     "UNIX-HOST" :unspecific (:absolute) nil nil nil)

(test-pathname-parse unix-host-host-name-type-version
		     eclipse::unix-pathname
		     "unix-host:foo.lisp.9"
		     "UNIX-HOST" :unspecific nil "FOO" "LISP" 9)

(test-pathname-parse unix-host-uppercase
		     eclipse::unix-pathname
		     "UNIX-HOST:/DIR/FOO.LISP.9"
		     "unix-host" :unspecific (:absolute "dir") "foo" "lisp" 9)

(test-pathname-parse unix-host-mixed-case
		     eclipse::unix-pathname
		     "Unix-Host:/Dir/Foo.Lisp.9"
		     "Unix-Host" :unspecific (:absolute "Dir") "Foo" "Lisp" 9)

(test-pathname-parse unix-host-host-name-type-version-numbers
		     eclipse::unix-pathname
		     "2:/2/3.4.5"
		     "2" :unspecific (:absolute "2") "3" "4" 5)

(test-pathname-parse unix-host-host-name-type-version-dashes
		     eclipse::unix-pathname
		     "-x:/-/-.-.5"
		     "-X" :unspecific (:absolute "-") "-" "-" 5)

#|
(test-pathname-parse unix-single-quoted
		     eclipse::unix-pathname
		     "a\\:b\\/c:\\/foo\\/b\\ar:baz/x\\:y\\.z.p\\:d\\.q.9"
		     "A:B/C" :unspecific (:relative "/FOO/BaR:BAZ")
		     "X:Y.Z" "P:D.Q" 9)

(test-pathname-parse unix-double-quoted
		     eclipse::unix-pathname
		     "'a:b/c':'/foo/bar:baz'/'x:y.z'.'p:d.q'.9"
		     "a:b/c" :unspecific (:relative "/foo/bar:baz")
		     "x:y.z" "p:d.q" 9)

(test-pathname-parse unix-not-name-type-version
		     eclipse::unix-pathname
		     "foo\\.lisp\\.9"
		     nil :unspecific nil "FOO.LISP.9" nil nil)
|#

#+wrong
(test-pathname-parse unix-logical
		     eclipse::unix-pathname
		     "host:dir;file"
		     "HOST" :unspecific nil "DIR;FILE" nil nil)

(test-pathname-parse unix-colon-dir
		     eclipse::unix-pathname
		     "host/x:y"
		     nil :unspecific (:relative "HOST") "X:Y" nil nil)



