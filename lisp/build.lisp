(in-package :user)

(defparameter *print-lots* nil
  "When generating Eclipse system C code from Lisp source:
   print forms.")

;;; *FEATURES* MEANINGS.  Note that this refers to host:*features*,
;;; not eclipse:*features*.  
;;; CMU, EXCL, ECLIPSE, etc. - we are processing with ALL of the
;;;   features and limitations of the specified system.
;;;   Note that it IS meaninguful to be in both, say, CMU AND ECLIPSE if
;;;   the code is processed by an embedded Eclipse reader and
;;;   compiler/loader.
;;; LISP-HOST - we are using Eclipse code within some other system
;;;   that may (or may not) have different primitive respresentations
;;;   for things like functions, strings, etc.  The "host" Lisp might
;;;   even be Eclipse itself.
;;; MACHINE-COMPILE - we are compiling Eclipse source into C.

;;; When generating C files:
;;;   cl:*features* and eclipse:*features* are *machine-compile-features*.
;;; When compiling or loading tests:
;;;   cl:*features* is *test-features*
;;; When eclipse:loading tests for evaluation,
;;;   cl:*features* has :eclipse, :lisp-host, platform-specific features
;;;   and eclipse:*features* has its usual values.
;;; When compiling or loading copied-declarations:
;;;   cl:*features* has :eclipse, :lisp-host, platform-specific features

#+(and cmu hppa) ;More consistent with Eclipse.
(progn (push :hp-ux *features*) (push :pa-risc *features*))
;; Allegro linux has broken *features* list.
#+linux86 (setq *features* (remove :sun (remove :sunos4 *features*)))

(defparameter *machine-compile-features*
  `(:machine-compile :eclipse
	     ,@(remove #+cmu :cmu
		       #+excl :excl *features*))
  "When generating Eclipse system C code from Lisp source:
   *features* gets bound to this.")
(push :LISP-HOST *features*)
(defparameter *test-features* (cons :eclipse *features*))


#+excl
(setq EXCL:*RECORD-SOURCE-FILE-INFO* nil
      EXCL:*RECORD-XREF-INFO* nil)

;;; The file class-bootstrap.lisp contains code that can process the
;;; classes defined in classes.lisp to bootstrap all the standard
;;; class objects in memory.

(defparameter *eval* t)
(defparameter *src-dir*
  ;; *load-pathname* is broken in excl.  Must merge against defaults
  (merge-pathnames			
   (make-pathname :name nil :type nil :version nil
		  :defaults *load-pathname*)))
(format t "~%Default pathname is ~s.~%" (truename *src-dir*))
(defparameter *output-dir* nil)

(setq *print-circle* t
      *print-length* 15
      *print-level* 15)

(defparameter *run0*
  '("common" "parameters" "mop-init"))

(defparameter *dev0*
  '("common-comp"))

(defparameter *dev1*
  '("prog-comp" "cont-comp" "cont-comp2"
    "macro" "macro-comp" "opts"
    "number-comp" "list-comp" "clos-comp"
    "pretty-comp"))

(defparameter *run1*
  '("kernel" "types-run" "control-run" "symbol"
	     "classes" "clos-run" "clos-define" "list"
	     "clos-seq"
	     "mop" "class-meth" "methods"))

(defparameter *dev1a* '("dev-meth"))

(defparameter *run1a*
  '("gfunc" "method-init"
	     "predicates"
	     "arithmetic" "conv" "hash" "type-ops"
	     "type-seq" "method-comb" "type-mops"
	     "sequence" "seq-mod" "search" "sort"
	     "control" "numbers" 
	     "trig" "num-conv" "array" "string" "struct-run"
	     "character" "tree"))

(defparameter *dev2*
  '("struct-comp" "cond-comp" "more-compile"))

(defparameter *run1b* '("env"))

(defparameter *dev2a* '("env-comp"))

(defparameter *run2*
  '("resource" "condition"
    "set" "alist" "bit-array" "bignum" "bits" 
    "equalp" "package" 
    "stream" "file-stream" "comp-stream" "circle" "init"))

(defparameter *dev3*
  '("loop" "directives"))

(defparameter *run3*
  '("reader" "dispatch" "printer" "pretty" "format" "print-object"
    "doc" "describer" "miscel" "random" "pathname" "file"
    "enclose"))

(defparameter *dev4*
  '("describe" "debug" "default-pp"
    "walk-util" "walk-top" "walk-special" "literal" "evaluation"
    "c" "file-walk" "c-walk" 
    ;;"walker"
    ;;"new-walk" "interp-walk" "min-compiler-walk" "new-file-walk" "new-c-walk"
    ))

(defparameter *host-files1*
  `(,@*run0* ,@*dev0*))

(defparameter *source-files*
  `(,@*run1* ,@*dev1a* ,@*run1a* ,@*dev2* ,@*run1b* ,@*dev2a* "temp"
    ,@*run2* ,@*dev3*
    ,@*run3* ,@*dev4*))
(defparameter *min-source*
  `(,@*run1* ,@*run1a* ,@*run1b* "temp" ,@*run2* ,@*run3*))

(defparameter *run-test*
  '("types" "subtypes" "typespec" "predicates" "program" "control" "control2"
    "symbol" "character" "sequence" "list" "hash" "array"
    "string" "structure" "clos"
    "condition" "error"
    "numbers" "numbers2" "numbers3" "numbers4" "numbers5" "numbers6"
    "stream" "reader" "printer"
    "loop" "misc" "package" "file"

    ;; Should work in any common lisp, but it doesn't in raw excl
    "pathname"

    ;; These are eclipse-specific
    "parsepath" "transpath" 

    ;;"clos-time"
    ))

(defparameter *dev-test*
  '("macro" "format" "pretty" "pretty2" "pretty3" "pretty4" "pretty5"
    ;; These are eclipse-specific
    "mop-defs" "mop" "env" "c"))

(defparameter *test-files*
  `(,@*run-test* ,@*dev-test*))

#| NOTES:

After a complete :c compile, the following macros will not be defined
correctly.  Any functions that that use them cannot be HOST recompiled
until the macros are HOST recompiled:

RESTART-CASE: condition-compile.lisp
TYPECASE: control-compile.lisp
IN-PACKAGE: control-compile.lisp
SAFE-SUBSEQ: eclipse-compile.lisp
DEFPACKAGE: package.lisp

The above is true for these as well, but HOST REcompiles should still
be ok (but not host compiles of NEW code):
 
DEFPARAMETER, DEFVAR, DEFCONSTANT: control-compile.lisp
DECLAIM: control.lisp

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *ignore-eclipse-file-write-date-p* nil)

(defun process-file (operator file target-type)
  (let* ((source (merge-pathnames
		  file
		  (make-pathname :type "lisp"
				 :defaults *src-dir*)))
	 (target (make-pathname :type target-type
				:defaults
				(if *output-dir*
				    (merge-pathnames *output-dir* source)
				    source)))
	 (target-date (file-write-date target))
	 (outdatedp (or *ignore-eclipse-file-write-date-p*
			(null target-date)
			(< target-date (file-write-date source)))))
    (unless outdatedp
      (format t "~&~s is already up to date.~%" target))
    (funcall operator source target outdatedp)))

(defun loader (source target outdatedp)
  (when outdatedp
    (compile-file source :output-file target :print *print-lots*))
  (load target :print *print-lots*))

(defparameter *loader-prefix* "INIT-~a")
(defun generator (source target outdatedp)
  (when outdatedp
    (funcall (find-symbol "COMPILE-FILE" :eclipse)
	     (namestring source)
	     (find-symbol "LOADER-NAME"
			  #+cmu :keyword #+excl :eclipse)
	     (intern (format nil *loader-prefix* (string-upcase (pathname-name source))))
	     :output-file (namestring target)
	     :external-format :ascii)))

(defun process-files (process target-type &rest files)
  (format t "~2&*** In package ~s, type ~s ***~%" (package-name *package*) target-type)
  (let ((start (get-internal-real-time)))
    (dolist (file files)
      (if (listp file)
	  (apply #'process-files process target-type file)
	  (process-file process file target-type)))
    (format t "~&*** ~a took ~,2f hours.~%"
	    process
	    (/ (- (get-internal-real-time) start)
	       internal-time-units-per-second
	       60.0 60.0))))

(defun host-load-list (package files)
  (let ((*package* (find-package package)))
    (apply #'process-files #'loader 
	   #+(and cmu sparc) "sparcf"
	   #+(and cmu hppa) "hpf"
	   #+(and excl sparc) "sparc"
	   #+(and excl linux86) "fasl"
	   files)))

(defun host-load (package &rest files)
  (host-load-list package files))

(defun generate-files (files &optional src (pkg :eclipse))
  (let ((*src-dir* (if src (merge-pathnames src *src-dir*)
		       *src-dir*)))
    (let ((*package* (find-package pkg))
	  (*features* *machine-compile-features*)
	  (*output-dir* (merge-pathnames "c/" *src-dir*)))
      (progv (list (find-symbol "*FEATURES*" :eclipse))
	  (list *machine-compile-features*)
	(apply #'process-files #'generator "c" files)))))

(defun eval-list (files)
  (let* ((*package* (find-package :eclipse))
	 (*features* (cons :eclipse *features*))
	 (f (find-symbol "LOAD" :eclipse))
	 (finish (find-symbol "FINISH-OUTPUT" :eclipse)))
    (mapc #'(lambda (file)
	      (funcall f
		       (namestring (merge-pathnames
				    (make-pathname :type "lisp" :defaults file)
				    *src-dir*))
		       :external-format :ascii)
	      (funcall finish)) files)))


(defun show-hosted-symbols (package)
  (let ((package (find-package package)))
    (do-external-symbols (s package)
			 (unless (eq package (symbol-package s))
			   (when (or (boundp s)
				     (fboundp s))
			     (print s))))))


(defun do-tests () (funcall (find-symbol "DO-TESTS" :eclipse)))
(defun rem-all-tests () (funcall (find-symbol "REM-ALL-TESTS" :eclipse)))
(defun eval-tests (files)
  (rem-all-tests)
  (eval-list files)
  (do-tests))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       
(host-load :user "host-pkg" "c-pkg")
#+allegro-v4.1 (load "/usr2/lisp/franz/lib/code/loop.fas")

(defun build-system (&optional min)
  (with-compilation-unit ()

    #-(or hppa linux86) (host-load :user "/usr2/lisp/ai-repository/tools/metering")

    ;; For some reason, this seems to make Allegro/linux go off into
    ;; never-never land when coercing a double-float to a single-float.
    #-allegro-v4.3
    ;; #-excl
    (declaim (optimize (speed 3)))

    (host-load-list :host
		    `("host-primitive-types" "prim-decs"
		      ,@*host-files1*
		      "host-class" "host-lisp" "host-function" "eclipse-compile"
		      "host-math" "host-hash" "host-tables" "host-array"
		      "host-stream" "host-pathname" "host-misc"))
    (unless min
      (host-load-list :host  *dev1*)
      (host-load :eclipse "eclipse-compile2"))
    (host-load-list :eclipse (if min *min-source* *source-files*))
    (host-load :host "statics")
    (unless min
      (let ((user::*declare-c-macros-p* nil)
	    (*features* (cons :eclipse *features*)))
	(declare (special user::*declare-c-macros-p*))
	(host-load :eclipse "copied-declarations")))
    (declaim (optimize (speed 1)))
    ))

(defun load-tests (&optional limited minimal)
  (if limited
      (host-load :eclipse-test
		 "pathname" "file"
		 ;; "pathname" "parsepath" "transpath" "file" "misc"
		 ;; "printer" "format" "pretty" "pretty2" "pretty3" "pretty4" "pretty5"
		 #| "predicates" "program" "control" "control2"
		 "symbol" "list" "hash" "string" "structure" "clos"
		 "reader" "printer" ;"format" "pretty"
		 "mop-defs" "mop" "condition" "macro" "package" |#) 
      (host-load-list :eclipse-test (if minimal *run-test* *test-files*))))

(defun do-c (&optional (*ignore-eclipse-file-write-date-p*
			*ignore-eclipse-file-write-date-p*)
		       (c-files *ignore-eclipse-file-write-date-p*)
		       (sourcep c-files)
		       (testp nil))
  (when c-files    
    (progn (cl:funcall (cl:find-symbol "DATE" :eclipse) t)
	   (cl:funcall (cl:find-symbol "TERPRI" :eclipse)))
    (when sourcep
      (let ((*eval* nil))
	(generate-files
	 `("c-pkg" ,@*host-files1* ,@*dev1*
	   "prim-decs" ,@*source-files*
	   "pkg"))))
    (when testp
      (let ((*eval* nil))
	(generate-files `("pkg" "rt") "test/" :eclipse-test))
      (generate-files *test-files* "test/" :eclipse-test))))

(defun do-all-tests (&optional limited (eval (not limited)) no-tests minimal)
  (let ((*src-dir* (merge-pathnames "test/" *src-dir*))
	(*features* *test-features*))
    (host-load :user "pkg")
    (in-package :eclipse)
    (host-load :eclipse-test "rt")
    (unless no-tests
      (load-tests limited minimal)
      (do-tests)
      (when (and eval (not minimal))
	(eval-tests #+nn *test-files*
		    '("program" "control" "control2" "predicates";; "macro"
		      "symbol" "structure"))))))

(defun do-all (&rest args &key minimal limited (eval (not limited))
		    regenerate (c-files regenerate)
		    no-tests recompile (retest recompile)
		    (sourcep c-files) (testp nil))
  (build-system minimal)
  (do-all-tests limited eval no-tests minimal)
  (when (or recompile retest)
    (let ((*ignore-eclipse-file-write-date-p* t))
      (rem-all-tests)
      (host-load-list :eclipse
		      `(,@*host-files1* ,@*dev1*))
      (do-all-tests limited eval)
      (when recompile
	(rem-all-tests)
	(host-load-list :eclipse *source-files*)
	(do-all-tests limited eval))))
  (do-c regenerate c-files sourcep testp)
  (let ((date (cl:find-symbol "DATE" :eclipse)))
    (when (fboundp date)
      (cl:funcall date t)
      (cl:funcall (cl:find-symbol "TERPRI" :eclipse)))
    (cl:format t "Task ~s complete." args))
  (values))		     
  
#+example ;to just run tests in a new bare host
(progn
  (setq *default-pathname-defaults* (merge-pathnames "test/"))
  (load "pkg.lisp")
  (in-package :et)
  (defparameter *test-files*
  '("rt" "types" "subtypes" "typespec" "predicates" "program" "control" "control2"
    "symbol" "character" "sequence" "list" "hash" "array"
    "string" "structure" "clos"
    "condition" "error"
    "numbers" "numbers2" "numbers3" "numbers4" "numbers5" "numbers6"
    "stream" "reader" "format" "printer" "pretty" "pretty2" "pretty3"
    "loop" "misc" 
    "macro" "package" "file" ))
  (mapc #'(lambda (f) (compile-file (merge-pathnames f)) (load f)) *test-files*))

#+example ;to generate one file
(let ((user::*eval* nil)
      (user::*features* user::*machine-compile-features*)
      (eclipse::*features* user::*machine-compile-features*))
  (compile-file "walk.lisp" :output-file "c/walk.c"
		:external-format :ascii 'loader-name 'init-walk))

#+example
(progn
  (setq *default-pathname-defaults*
	(merge-pathnames
	 (make-pathname :version :unspecific
			:directory '(:relative :back :back :back :back "test"))))
  (push :machine-compile *features*)
  (setq *load-print* t)
  (load "pkg.lisp")
  (in-package :et)
  (load "rt.lisp")
  ;;(funcall (find-symbol "DO-TESTS" :et))
  (setq files '("numbers4" "numbers5" "numbers6"
		))
  (defun cc (s)
    (compile-file s :output-file "c/"
		  :external-format :ascii
		  'output-format :c
		  'loader-name (make-name "INIT-~a"
					  (string-upcase s))))
  (mapc #'cc files))

#+(and nn cmu hppa) (do-all :no-tests t :c-files t)
#+(and nn excl sparc) (let ((*eval* nil))
		     (do-all :no-tests t)
		     (let ((*ignore-eclipse-file-write-date-p* t))
		       (generate-files `("stream" "stream2")))
		     (cl:format t "Code generation complete."))

#+example (progn (load "build") (do-all :retest t))
#+example (progn (load "build") (do-all :c-files nil :no-tests t))
#+example (progn (load "build") (do-all :limited t :c-files nil :retest t))
#+example (progn (load "build") (do-all :limited t :regenerate t))
#+example (progn (load "build") (compile-to-c '("prim-decs.lisp" "try")))

#+example (progn (load "build") (do-all :limited t))
#+example (progn (load "build") (do-all :limited t :eval t))
#+example (progn (load "build") (do-all :no-tests t :c-files t))
#+example (progn (load "build") (do-all :no-tests t :c-files t :sourcep nil :testp t))
#+example (progn (load "build") (do-all :no-tests t))
#+example (progn (load "build") (do-all :minimal t))
#+example (progn (load "build") (do-all))

(do-all :no-tests t)