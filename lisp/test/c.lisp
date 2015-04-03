(in-package :eclipse)
(defmacro defbtest (name form value)
  `(deftest ,name (with-output-to-string (s)
		    (with-c-syntax
		     (pprint-logical-block (s nil)
		       ,form)))
     ,value))
(defmacro defctest (name form value)
  `(defbtest ,name (write ,form :stream s) ,value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                      IDENTIFIERS                             ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LOCAL VARIABLES
(defbtest var-binding (c-variable-binding s 'foo-bar) "foo_bar")
(defbtest var-binding-escaped (c-variable-binding s '|foo|) "PIPEfooPIPE")
(defbtest var-binding-undistinguished (c-variable-binding s '\1) "CL_1")
(defbtest var-binding-undistinguished2 (c-variable-binding s '1+) "CL_1PLUS")
(defbtest var-binding-other-package (c-variable-binding s 'user::foo-bar) "USR_foo_bar")
(defbtest var-binding-c (c-variable-binding s 'ec::foo_bar) "_foo_bar")
(defbtest var-binding-reserved (c-variable-binding s 'ec:int) "_int")

;;; GLOBAL VARIABLES
(defbtest global (c-global-symbol s 'foo-bar) "clFOO_BAR")
(defbtest global-escaped (c-global-symbol s '|foo|) "clpipefoopipesymbol") ;Clashes w/|foo|symbol !
(defbtest global-undistinguished (c-global-symbol s '\1) "cl1symbol")
(defbtest global-undistinguished2 (c-global-symbol s '1+) "cl1plussymbol")
(defbtest global-c (c-global-symbol s 'ec::foo_bar) "foo_bar")
(defbtest global-reserved (c-global-symbol s 'ec:int) "_int") ;Clashes w/local!

;;; TAGS/LABELS
(defbtest tag (c-other-binding s 'foo-bar) "FOO_BAR")
(defbtest tag-escaped (c-other-binding s '|foo|) "pipefoopipesymbol")
(defbtest tag-undistinguished (c-other-binding s '\1) "cl_1symbol")
(defbtest tag-undistinguished2 (c-other-binding s '1+) "cl_1plussymbol")
(defbtest tag-c (c-other-binding s 'ec::foo_bar) "_foo_bar") ;Clashes w/local var!
(defbtest tag-reserved (c-other-binding s 'ec:int) "_int") ;Clashes w/local var!

;;; LOCAL FUNCTION BINDINGS
(defbtest func-binding (c-function-binding s 'foo-bar) "FooBar")
(defbtest func-binding-escaped (c-function-binding s '|foo|) "PIPEfooPIPE_FUNC")
(defbtest func-binding-undistinguished (c-function-binding s '\1) "cl_1_FUNC")
(defbtest func-binding-undistinguished2 (c-function-binding s '1+) "cl_1PLUS_FUNC")
(defbtest func-binding-other-package (c-function-binding s 'user::foo-bar) "usr_FooBar")
(defbtest func-binding-c (c-function-binding s 'ec::foo_bar) "_foo_bar") ;Clashes w/local var!
(defbtest func-binding-reserved (c-function-binding s 'ec:int) "_int") ;Clashes w/local var!

;;; GLOBAL FUNCTIONS
(defbtest func-name (c-global-function s 'foo-bar) "clFooBar")
(defbtest func-name-escaped (c-global-function s '|foo|) "clPIPEfooPIPE_FUNC")
(defbtest func-name-undistinguished (c-global-function s '\1) "cl1_FUNC")
(defbtest func-name-undistinguished2 (c-global-function s '1+) "cl1PLUS_FUNC")
(defbtest func-name-other-package (c-global-function s 'user::foo-bar) "usrFooBar")
(defbtest func-name-c (c-global-function s 'ec::foo_bar) "foo_bar") 
(defbtest func-name-reserved (c-global-function s 'ec:int) "int") ;Clashes w/typedef!

;;; C TYPE SPECIFIERS 
;;; These are usually c reserved words, but could be arbitrary typedefs.
;;; Note that the C typedef namespace is distinct from the C global function
;;; namespace, so we can use the same translations.
(defbtest type-name (c-typedef s 'foo-bar) "clFooBar")
(defbtest type-name-escaped (c-typedef s '|foo|) "clPIPEfooPIPE_FUNC")
(defbtest type-name-undistinguished (c-typedef s '\1) "cl1_FUNC")
(defbtest type-name-undistinguished2 (c-typedef s '1+) "cl1PLUS_FUNC")
(defbtest type-name-other-package (c-typedef s 'user::foo-bar) "usrFooBar")
(defbtest type-name-c (c-typedef s 'ec::foo_bar) "foo_bar")
(defbtest type-name-reserved (c-typedef s 'ec:int) "int")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                      C LITERALS                              ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter c-int (make-int 42))
(defparameter c-float (make-float (float pi 1.0f0)))
(defparameter c-double (make-double (float pi 1.0d0)))
(defctest char-literal 		(make-char #\a) 	"'a'")
(defctest char-literal-linefeed (make-char #\linefeed) "'\\n'")
(defctest char-literal-rubout	(make-char #\rubout)	"'\\177'")
(defctest int-literal 		c-int		 	"42")
(defctest int-literal-minus 	(make-int -42)	"-42")
(defctest float-literal 	(make-float -1.5f-10)	"-1.5e-10")
(defctest float-literal-pi 	c-float			"3.1415927")
(defctest double-literal 	(make-double -1.5d-10) "-1.5e-10")
(defctest double-literal-pi 	c-double 	"3.141592653589793")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                 COMPILER DATA STRUCTURES                     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter simple-binding (make-env-binding :name 'foo-bar))
(defparameter renamed-binding (make-env-binding :name 'foo-bar
						    :declarations '(index 2)))
#|(defparameter captured-binding
    (make-env-binding :name 'foo-bar :declarations '(enclosed variable-env))) |#
(defparameter simple-function '(foo-bar))
(defparameter lex-function '(foo-bar baz quux))
(defparameter simple-id '(id foo-bar nil))
(defparameter renamed-id '(id foo-bar 2))

(defctest p-binding simple-binding		"foo_bar")
(defctest p-binding-renamed renamed-binding	"foo_bar__R2")
(defctest p-literal-id (make-constant-id "C" #\a) "C_a")
(defctest p-literal-id-renamed
    (make-numeric-constant-id "R" "~d/~d" 1 2) "R_1div2")

#|(defctest p-captured-binding captured-binding 	"*foo_bar")
(defctest p-captured-binding-ref
    (make-captured-binding :name 'foo :index 2
			   :value (make-env-binding :declarations '(type t)))
  "clEnv(2)")
(defctest p-captured-binding-ref-ctype
    (make-captured-binding :name 'foo :index 2
			   :value (make-env-binding :declarations '(type ec:int)))
  "clCEnv(int, 2)") |#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                    FORMS                                     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defctest p-call `(foo-bar ,simple-binding ,renamed-binding nil eoa)
  "clFooBar(foo_bar, foo_bar__R2, clNIL, clEOA)")
(defctest p-empty-call `(foo-bar eoa)	"clFooBar(clEOA)")
(defctest p-empty-ccall `(foo-bar)	"clFooBar()")

(defctest p-hook '(hook foo-bar)			"(*clFooBar_)")
#|(defctest p-function `(function ,simple-function) 	"clFooBar")
(defctest p-internal `(internal ,simple-function)	"_clFooBar")
(defctest p-function-lex `(function ,lex-function) 	"clFooBar_Baz_clQuux")
(defctest p-internal-lex `(internal ,lex-function) 	"_clFooBar_Baz_clQuux")|#
(defctest p-id simple-id 		"FOO_BAR")
(defctest p-id-renamed renamed-id	"FOO_BAR__r2")
(defctest p-address `(address ,simple-binding) 		"&foo_bar")
#|(defctest p-address-capt `(address ,captured-binding) 	"&*foo_bar")|#
(defctest p-assign `(assign ,simple-binding ,c-int)	"foo_bar = 42")
(defctest p-if-exp `(cond ,simple-binding (nil) (t))
  "(clTrue(foo_bar) ? clNIL : clT)")
(defctest p-goto `(goto ,simple-id)			"goto FOO_BAR")
(defctest p-cast-int `(double-int ,c-double)	"((int) 3.141592653589793)")
(defctest p-cast-float `(int-float ,c-int)	"((float) 42)")
(defctest p-cast-double `(int-double ,c-int)	"((double) 42)")

;;; declarations, compound statements, if statments, function definitions
;;; file operations !!!