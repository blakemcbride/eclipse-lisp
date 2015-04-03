(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Features not required by the spec.
  #+eclipse
  (pushnew :arbitrary-pathname-devices cl:*features*)
  )

(eval-when (;; Some systems needed this at compile time for
	    ;; defparameter values later in file.  
	    :compile-toplevel		
	    :load-toplevel :execute)
  #+eclipse
  (setf (eclipse::host-pathname-class "UNIX-HOST") 'eclipse::unix-pathname)
  #+eclipse
  (setq *default-pathname-defaults*
	(make-pathname :device :unspecific :defaults *default-pathname-defaults*))
  (setf (logical-pathname-translations "MYSYS")
    `(("SITE;"  ,(make-pathname :name nil :type nil :version nil
				:defaults *default-pathname-defaults*)))))

(defun list-components (pathname &optional (devicep t))
  ;; Implementations are allowed to use a different representation for
  ;; hosts.
  (when pathname
    (list (let ((h (pathname-host pathname :case :common)))
	    (typecase h
	      (string h)
	      (symbol h)
	      (t (host-namestring pathname))))
	  (case devicep
	    (:unspecific :unspecific)
	    ((nil) nil)
	    (t (pathname-device pathname :case :common)))
	  (pathname-directory pathname :case :common)
	  (pathname-name pathname :case :common)
	  (pathname-type pathname :case :common)
	  (pathname-version pathname))))

(defparameter sample-logical (pathname "mysys:site;junk.lisp.99"))
(defparameter logical-components (list-components sample-logical))
(defparameter sample-physical
    (translate-logical-pathname "mysys:site;tmpjnk.text.newest"))
(defparameter physical-components (list-components sample-physical))

(defmacro test-pathname (name pathname &body components)
  `(deftest ,name (list-components ,pathname) ,components))

(defmacro test-pathname1 (name pathname &body components)
  `(deftest ,name (list-components ,pathname :unspecific) ,components))

(defmacro test-namestring (name namestring &rest components)
  `(deftest ,name (list-components (parse-namestring ,namestring)
				   :unspecific) ,components))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 23.1.6 PATHNAME FUNCTIONS                                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; MAKE-PATHNAME
(test-pathname make-logical
	       (make-pathname :host "MYSYS"
			      :device :unspecific
			      :directory '(:absolute "DIR")
			      :name "FILE"
			      :type "LISP"
			      :version 9)
	       "MYSYS" :UNSPECIFIC (:ABSOLUTE "DIR") "FILE" "LISP" 9)
(test-pathname make-merged
	       (make-pathname :defaults sample-logical)
	       "MYSYS" :UNSPECIFIC (:ABSOLUTE "SITE") "JUNK" "LISP" 99)
(test-pathname make-merged-host
	       (let ((*default-pathname-defaults* sample-logical))
		 (make-pathname))
	       "MYSYS" :UNSPECIFIC nil nil nil :newest)
(test-pathname make-unmerged
	       (make-pathname
		:host nil
		:device nil
		:directory '(:relative "FOO")
		:name nil
		:type nil
		:version nil
		:case :common
		:defaults sample-logical)
	       nil nil (:relative "FOO") nil nil nil)

#+arbitrary-pathname-devices
(progn
  (test-pathname make-explicit-device
		 (make-pathname :host nil :device :weird :version nil)
		 nil :weird nil nil nil nil)
  (test-pathname make-default-device-for-host
		 (make-pathname :host nil :version nil
				:defaults (make-pathname
					   :host :unspecific
					   :device :weird))
		 nil :unspecific nil nil nil nil)
  (test-pathname make-default-device-for-host-unsupplied
		 (make-pathname :version nil
				:defaults (make-pathname
					   :host :unspecific
					   :device :weird))
		 :unspecific :weird nil nil nil nil) ;Debatable!!!
  (test-pathname make-default-device-and-version
		 (make-pathname :host :unspecific
				:defaults (make-pathname
					   :host :unspecific
					   :device :weird
					   :version 9))
		 :unspecific :weird nil nil nil 9)
  )
(test-pathname make-null-name-newest-version
	       (make-pathname :name nil
			      :defaults (make-pathname
					 :version 9))
	       :unspecific :unspecific nil nil nil :newest)
(test-pathname make-no-default-newest-version
	       (make-pathname :defaults (make-pathname
					 :version nil))
	       :unspecific :unspecific nil nil nil :newest)

(test-pathname make-unix-local-case
	       (make-pathname :case :local
			      :host "unix-host"
			      :device
			      #+arbitrary-pathname-devices "weird"
			      #-arbitrary-pathname-devices :unspecific
			      :directory "dir"
			      :name "foo"
			      :type "lisp")
	       "UNIX-HOST"
	       #+arbitrary-pathname-devices "WEIRD"
	       #-arbitrary-pathname-devices :unspecific
	       (:absolute "DIR") "FOO" "LISP"
	       :newest)
(test-pathname make-unix-common-case
	       (make-pathname :case :common
			      :host "unix-host"
			      :device 
			      #+arbitrary-pathname-devices "weird"
			      #-arbitrary-pathname-devices :unspecific
			      :directory "dir"
			      :name "foo"
			      :type "lisp")
	       "unix-host"
	       #+arbitrary-pathname-devices "weird"
	       #-arbitrary-pathname-devices :unspecific
	       (:absolute "dir") "foo" "lisp"
	       :newest)
(test-pathname make-defaulted-unix-local-case
	       (make-pathname :case :local
			      :defaults (make-pathname
					 :host "unix-host"
					 :device
					 #+arbitrary-pathname-devices "weird"
					 #-arbitrary-pathname-devices :unspecific
					 :directory "dir"
					 :name "foo"
					 :type "lisp"))
	       "UNIX-HOST"
	       #+arbitrary-pathname-devices "WEIRD"
	       #-arbitrary-pathname-devices :unspecific
	       (:absolute "DIR") "FOO" "LISP"
	       :newest)
(test-pathname make-defaulted-unix-common-case
	       (make-pathname :case :common
			      :defaults (make-pathname
					 :host "unix-host"
					 :device
					 #+arbitrary-pathname-devices "weird"
					 #-arbitrary-pathname-devices :unspecific
					 :directory "dir"
					 :name "foo"
					 :type "lisp"))
	       "UNIX-HOST"
	       #+arbitrary-pathname-devices "WEIRD"
	       #-arbitrary-pathname-devices :unspecific
	       (:absolute "DIR") "FOO" "LISP"
	       :newest)

(deftest make-creates-logical
    (typep (make-pathname :host "mysys"
			  :defaults sample-physical)
	   'logical-pathname)
  t)
(deftest make-creates-logical-defaulted
    (typep (make-pathname :defaults sample-logical)
	   'logical-pathname)
  t)
(deftest make-does-not-create-logical
    (typep (make-pathname :host nil
			  :defaults sample-logical)
	   'logical-pathname)
  nil)

(test-pathname make-handles-string-default
	       (make-pathname :defaults "mysys:site;file.lisp.9")
	       "MYSYS" :unspecific (:absolute "SITE") "FILE" "LISP" 9)

(deftest canonicalize-directory-wild
    (pathname-directory (make-pathname :directory :wild))
  (:absolute :wild-inferiors))
(deftest canonicalize-directory-string
    (pathname-directory (make-pathname :directory "FOO" :case :common)
			:case :common)
  (:absolute "FOO"))


;;; PARSE-NAMESTRING
(deftest parse-pathname 
    (multiple-value-bind (p term)
	(parse-namestring sample-logical nil
			  *default-pathname-defaults* :start 99)
      (and (eq p sample-logical)
	   (= term 99))) t)
#+not-yet
(test-pathname parse-pathname-host-arg
	       (parse-namestring (make-pathname :host nil) "sys")
	       "msys" :unspecific nil nil nil :newest)
#+not-yet(deftest parse-stream
    (multiple-value-bind (p term)
	(with-open-file (s sample-physical :if-does-not-exist :create)
	  (parse-namestring s nil
			    *default-pathname-defaults* :start 99))
      (delete-file sample-physical)
      (and (equal (list-components p) physical-components)
	   (= term 99))) t)

;; Explicit, known, host arguments.
(test-pathname parse-logical-host-arg
	       (parse-namestring "foo" "mysys")
	       "MYSYS" :unspecific nil "FOO" nil nil)
(test-pathname parse-logical-host-arg-match
	       (parse-namestring "mysys:foo" "mysys")
	       "MYSYS" :unspecific nil "FOO" nil nil)
(test-pathname parse-unix-host-arg
	       (parse-namestring "foo" "unix-host")
	       "UNIX-HOST" :unspecific nil "FOO" nil nil)
(test-pathname parse-unix-host-arg-match
	       (parse-namestring "unix-host:foo" "unix-host")
	       "UNIX-HOST" :unspecific nil "FOO" nil nil)
(deftest parse-logical-host-arg-junk
    (multiple-value-bind (p term)
	(parse-namestring "foo/bar" "mysys" *default-pathname-defaults*
			  :junk-allowed t)
      (and (equal (list-components p)
		  '("MYSYS" :unspecific nil "FOO" nil nil))
	   term))
  3)
(deftest parse-logical-host-arg-start-end
    (multiple-value-bind (p term)
	(parse-namestring "/foo/" "mysys" *default-pathname-defaults*
			  :start 1 :end 4)
      (and (equal (list-components p)
		  '("MYSYS" :unspecific nil "FOO" nil nil))
	   term))
  4)

;; host qualified namestrings
(test-pathname parse-logical-host-string
	       (parse-namestring "mysys:")
	       "MYSYS" :unspecific nil nil nil nil)
(test-pathname parse-logical-host-string-type
	       (parse-namestring "mysys:.lisp")
	       "MYSYS" :unspecific nil nil "LISP" nil)
(test-pathname parse-unix-host-string
	       (parse-namestring "unix-host:")
	       "UNIX-HOST" :unspecific nil nil nil nil)
(test-pathname parse-unix-host-string-name
	       (parse-namestring "unix-host:.lisp")
	       "UNIX-HOST" :unspecific nil ".LISP" nil nil)
(test-pathname parse-unix-host-string-dir
	       (parse-namestring "unix-host:foo/")
	       "UNIX-HOST" :unspecific (:relative "FOO") nil nil nil)
(deftest parse-logical-host-string-junk
    (multiple-value-bind (p term)
	(parse-namestring "mysys:foo/" nil *default-pathname-defaults*
			  :junk-allowed t)
      (and (equal (list-components p)
		  '("MYSYS" :unspecific nil "FOO" nil nil))
	   term))
  9)
(deftest parse-logical-host-string-start-end
    (multiple-value-bind (p term)
	(parse-namestring "/mysys:foo/" nil *default-pathname-defaults*
			  :start 1 :end 10)
      (and (equal (list-components p)
		  '("MYSYS" :unspecific nil "FOO" nil nil))
	   term))
  10)

;; parsing relative to defaults
(test-pathname parse-logical-defaults
	       (parse-namestring ".lisp" nil sample-logical)
	       nil :unspecific nil nil "LISP" nil)
(test-pathname1 parse-physical-defaults
   (let ((*default-pathname-defaults* sample-logical))
     (parse-namestring ".lisp" nil sample-physical))
  nil :unspecific nil ".LISP" nil nil)
(deftest parse-logical-defaults-junk
    (multiple-value-bind (p term)
	(parse-namestring ".lisp/" nil sample-logical
			  :junk-allowed t)
      (and (equal (list-components p)
		  '(nil :unspecific nil nil "LISP" nil))
	   term))
  5)
(deftest parse-logical-defaults-start-end
    (multiple-value-bind (p term)
	(parse-namestring "/.lisp/" nil sample-logical
			  :start 1 :end 6)
      (and (equal (list-components p)
		  '(nil :unspecific nil nil "LISP" nil))
	   term))
  6)

;; parse empty string
(test-pathname1 empty-logical
	       (parse-namestring "" nil sample-logical)
	       nil :unspecific nil nil nil nil)

(test-pathname1 parse-empty (parse-namestring "")
	       nil :unspecific nil nil nil nil)
(test-pathname1 parse-name (parse-namestring "foo")
	       nil :unspecific nil "FOO" nil nil)
(test-pathname1 parse-dot-name (parse-namestring ".foo")
	       nil :unspecific nil ".FOO" nil nil)
(test-pathname1 parse-type (parse-namestring "*.lisp")
	       nil :unspecific nil :wild "LISP" nil)
(test-pathname1 parse-version (parse-namestring "*.*.1")
	       nil :unspecific nil :wild :wild 1)
(test-pathname1 parse-newest-version (parse-namestring "*.*.newest")
	       nil :unspecific nil :wild :wild :newest)
(test-pathname1 parse-wild-version (parse-namestring "*.*.*")
	       nil :unspecific nil :wild :wild :wild)
(test-pathname parse-wild-host
	       (let ((*default-pathname-defaults* sample-logical))
		 (parse-namestring "*:foo"))
	       :wild :unspecific nil "FOO" nil nil)
(test-pathname parse-wild-host-empty-name
	       (let ((*default-pathname-defaults* sample-logical))
		 (parse-namestring "*:"))
	       :wild :unspecific nil nil nil nil)
(test-pathname1 parse-absolute-dir (parse-namestring "/foo/**/bar/./*/baz/../")
	       nil :unspecific (:absolute "FOO" :wild-inferiors "BAR" :wild "BAZ" :up)
	       nil nil nil)
(test-pathname1 parse-relative-dir (parse-namestring "foo/**/bar/./*/baz/../")
	       nil :unspecific (:relative "FOO" :wild-inferiors "BAR" :wild "BAZ" :up)
	       nil nil nil)
(test-pathname1 parse-relative-dot-dir (parse-namestring "./foo/")
	       nil :unspecific (:relative "FOO") nil nil nil)

(test-pathname lparse-empty (parse-namestring "" "MYSYS")
  "MYSYS" :unspecific nil nil nil nil)
(test-pathname lparse-name (parse-namestring "foo" "MYSYS")
  "MYSYS" :unspecific nil "FOO" nil nil)
(test-pathname lparse-type (parse-namestring "*.lisp" "MYSYS")
  "MYSYS" :unspecific nil :wild "LISP" nil)
(test-pathname lparse-version (parse-namestring "*.*.1" "MYSYS")
  "MYSYS" :unspecific nil :wild :wild 1)
(test-pathname lparse-newest-version (parse-namestring "*.*.newest" "MYSYS")
  "MYSYS" :unspecific nil :wild :wild :newest)
(test-pathname lparse-wild-version (parse-namestring "*.*.*" "MYSYS")
  "MYSYS" :unspecific nil :wild :wild :wild)
(test-pathname lparse-wild-host (parse-namestring "*:foo" nil sample-logical)
  :wild :unspecific nil "FOO" nil nil)
(test-pathname lparse-wild-host-empty-name (parse-namestring "*:" nil sample-logical)
  :wild :unspecific nil nil nil nil)
(test-pathname lparse-absolute-dir (parse-namestring "foo;**;bar;*;baz;" "MYSYS")
  "MYSYS" :unspecific (:absolute "FOO" :wild-inferiors "BAR" :wild "BAZ")
       nil nil nil)
(test-pathname lparse-relative-dir (parse-namestring ";foo;**;bar;*;baz;" "MYSYS")
  "MYSYS" :unspecific (:relative "FOO" :wild-inferiors "BAR" :wild "BAZ")
  nil nil nil)

(test-pathname parse-handles-string-default
	       (parse-namestring "foo" nil "mysys:site;file.lisp.9")
	       nil :unspecific nil "FOO" nil nil)

(deftest parse-makes-logical-host-arg
    (typep (parse-namestring "foo" "mysys") 'logical-pathname)
  t)
(deftest parse-makes-logical-host-arg2
    (typep (parse-namestring "mysys:foo" "mysys") 'logical-pathname)
  t)
(deftest parse-makes-logical-explicit-host
    (typep (parse-namestring "mysys:" nil sample-physical) 'logical-pathname)
  t)
(deftest parse-makes-logical-default-host
    (typep (parse-namestring "foo" nil sample-logical) 'logical-pathname)
  t)
(deftest parse-makes-physical-explicit-host
    (typep (parse-namestring (format nil (formatter "~a:") (machine-instance))
			     nil sample-logical :junk-allowed t)
	   'logical-pathname)
  nil)
(deftest parse-makes-physical-default-host
    (typep (parse-namestring "foo" nil sample-physical) 'logical-pathname)
  nil)

;;; MERGE-PATHNAMES
(deftest merge-host
    (pathname-host (merge-pathnames (make-pathname :host nil)
				    (make-pathname :host "unix-host")))
  "unix-host")
(deftest merge-host-not
    (pathname-host (merge-pathnames (make-pathname :host :unspecific)
				    (make-pathname :host "foo")))
  :unspecific)
(deftest merge-device
    (pathname-device (merge-pathnames (make-pathname :host "unix-host"
						     :device nil)
				      (make-pathname :host "unix-host"
						     :device "foo")))
  "foo")
(deftest merge-device-not
    (pathname-device (merge-pathnames (make-pathname :device "BAR" :case :common)
				      (make-pathname :device "foo"))
		     :case :common)
  "BAR")
(deftest merge-device-default
    (pathname-device (merge-pathnames (make-pathname :host "unix-host"
						     :device nil)
				      (make-pathname :host :unspecific
						     :device "foo")))
  :unspecific)
(deftest merge-device-no-host
    (pathname-device (merge-pathnames (make-pathname :host nil
						     :device nil)
				      (make-pathname :host "unix-host"
						     :device "foo")))
  "foo")
(deftest merge-device-ultimate-default ;maybe default-device ;instead?!!!
    (pathname-device (merge-pathnames (make-pathname :host "unix-host"
						     :device nil)
				      (make-pathname :host "unix-host"
						     :device nil)))
  nil)
(deftest merge-directory
    (pathname-directory
     (merge-pathnames (make-pathname :directory nil)
		      (make-pathname :directory '(:relative "FOO")
				     :case :common))
     :case :common)
  (:relative "FOO"))
(deftest merge-directory-not
    (pathname-directory
     (merge-pathnames (make-pathname :directory '(:absolute "FOO") :case :common)
		      (make-pathname :directory '(:absolute "bar")))
     :case :common)
  (:absolute "FOO"))
(deftest merge-directory-relative
    (pathname-directory
     (merge-pathnames (make-pathname :directory '(:relative "FOO") :case :common)
		      (make-pathname :directory '(:relative "BAR") :case :common))
     :case :common)
  (:relative "BAR" "FOO"))
(deftest merge-directory-relative-nil
    (pathname-directory
     (merge-pathnames (make-pathname :directory '(:relative "FOO") :case :common)
		      (make-pathname :directory nil))
     :case :common)
  (:relative "FOO"))
(deftest merge-name
    (pathname-name (merge-pathnames (make-pathname :name nil)
				    (make-pathname :name "FOO" :case :common))
		   :case :common)
  "FOO")
(deftest merge-name-not
    (pathname-name (merge-pathnames (make-pathname :name :unspecific)
				    (make-pathname :name "foo")))
  :unspecific)
(deftest merge-type
    (pathname-type (merge-pathnames (make-pathname :type nil)
				    (make-pathname :type "FOO" :case :common))
		   :case :common)
  "FOO")
(deftest merge-type-not
    (pathname-type (merge-pathnames (make-pathname :type :unspecific)
				    (make-pathname :type "foo")))
  :unspecific)
(deftest merge-version
    (pathname-version (merge-pathnames (make-pathname :version nil)
				       (make-pathname :version 9)))
  9)
(deftest merge-version-not
    (pathname-version (merge-pathnames (make-pathname :version :unspecific)
				       (make-pathname :version 9)))
  :unspecific)
(deftest merge-version-not2
    (pathname-version (merge-pathnames (make-pathname :name "foo"
						      :version nil)
				       (make-pathname :name "foo"
						      :version 9)))
  :newest)
(deftest merge-version-not3
    (pathname-version (merge-pathnames (make-pathname :name "foo"
						      :version nil)
				       (make-pathname :name "foo"
						      :version 9)
				       3))
  3)
(deftest merge-version-defaulted
    (pathname-version (merge-pathnames (make-pathname :version nil)
				       (make-pathname :version nil)))
  :newest)
(deftest merge-version-defaulted2
    (pathname-version (merge-pathnames (make-pathname :version nil)
				       (make-pathname :version nil)
				       3))
  3)


(deftest canonicalize-directory-remove-back
    (pathname-directory (merge-pathnames
			 (make-pathname
			  :directory '(:relative "b" :back :back "C") :case :common)
			 (make-pathname
			  :directory '(:relative "a" "bad" :back)))
			:case :common)
  (:relative "C"))
(deftest canonicalize-directory-remove-back1
    (pathname-directory (merge-pathnames
			 (make-pathname
			  :directory '(:relative :back :back))
			 (make-pathname
			  :directory '(:relative "a"))))
  (:relative :back))
(deftest canonicalize-directory-remove-back2
    (pathname-directory (merge-pathnames
			 (make-pathname
			  :directory '(:relative :back :back :back))
			 (make-pathname
			  :directory '(:absolute "a" "bad" :back))))
  (:absolute :back :back))

(test-pathname merge-defaults
	       (let ((*default-pathname-defaults*
		      (make-pathname :host "unix-host"
				     :directory :wild
				     :name "foo"
				     :type "lisp"
				     :version 9)))
		 (merge-pathnames (make-pathname :host nil
						 :device nil
						 :directory nil
						 :name nil
						 :type nil
						 :version nil)))
	       "UNIX-HOST" :unspecific (:absolute :wild-inferiors)
	       "FOO" "LISP" 9)
(test-pathname merge-namestrings
	       (merge-pathnames "foo" "proj/sub/bar.lisp.9")
	       nil :unspecific (:relative "PROJ" "SUB")
	       "FOO" "LISP" :newest)
(deftest merge-logical (typep (merge-pathnames sample-logical
					       sample-physical)
			      'logical-pathname) t)
(deftest merge-logical-namestring
    (typep (merge-pathnames "mysys:" sample-physical)
	   'logical-pathname) t)
(deftest merge-empty-host-logical
    (typep (merge-pathnames (make-pathname :host nil)
			    sample-logical)
	   'logical-pathname) t)
(deftest merge-physical
    (typep (merge-pathnames sample-physical sample-logical)
	   'logical-pathname) nil)
(deftest merge-phsyical-namestring
    (typep (merge-pathnames "unix-host:" sample-logical)
	   'logical-pathname) nil)
(deftest merge-empty-host-physical
    (typep (merge-pathnames (make-pathname :host nil)
			    sample-physical)
	   'logical-pathname) nil)

(deftest merge-parses-using-defaults
    (typep (merge-pathnames "foo" sample-logical)
	   'logical-pathname) t)
(test-pathname merge-parses-using-defaults2
	       (merge-pathnames ".bin" sample-logical)
	       "MYSYS" :unspecific (:absolute "SITE") "JUNK" "BIN" 99)

;;; PATHNAME
(deftest pathname-pathname
    (let ((pathname (make-pathname :name "foo")))
      (eq pathname (pathname pathname))) t)
(deftest pathname-namestring (pathname-name (pathname "foo") :case :common) "FOO")
#+not-yet
(deftest pathname-stream-physical
    (prog1
	(with-open-file (s sample-physical :if-does-not-exist :create)
	  (and (equal (list-components (pathname s)) physical-components)
	       (progn (close s)
		      (equal (list-components (pathname s))
			     physical-compnents))))
      (delete-file sample-physical))
  t)
#+not-yet
(deftest pathname-stream-logical
      (prog1
	  (with-open-file (s sample-logical :if-does-not-exist :create)
	    (and (equal (list-components (pathname s)) logical-components)
		 (progn (close s)
			(equal (list-components (pathname s))
			       logical-components))))
	(delete-file sample-logical))
  t)

;;; PATHNAMEP
(deftest pathnamep-string (pathnamep "foo") nil)
(deftest pathnamep-path (pathnamep (pathname "foo")) t)
(deftest pathnamep-logical (pathnamep (logical-pathname "mysys:foo")) t)

;;; PATHNAME-HOST
(deftest pathname-logical-host-string-common
    (pathname-host "mysys:" :case :common)
  "MYSYS")
(deftest pathname-logical-host-string-local
    (pathname-host "mysys:" :case :local)
  "MYSYS")
(deftest pathname-host-string-wild
    (let ((*default-pathname-defaults* sample-logical))
      (pathname-host "*:"))
  :wild)
(deftest pathname-unix-host-string-common
    (pathname-host "unix-host:" :case :common)
  "UNIX-HOST")
(deftest pathname-unix-host-string-local
    (pathname-host "unix-host:" :case :local)
  "unix-host")
(deftest pathname-unix-host-string-common2
    (pathname-host "UNIX-HOST:" :case :common)
  "unix-host")
(deftest pathname-unix-host-string-local2
    (pathname-host "UNIX-HOST:" :case :local)
  "UNIX-HOST")
(deftest pathname-host-nil
    (pathname-host (make-pathname :host nil)) nil)

;;; PATHNAME-DEVICE
#+arbitrary-pathname-devices
(progn
  (deftest pathname-device-common
      (pathname-device (make-pathname :host "unix-host"
				      :device "weird")
		       :case :common)
    "WEIRD")
  (deftest pathname-device-common2
      (pathname-device (make-pathname :host "unix-host"
				      :device "WEIRD")
		       :case :common)
    "weird")
  (deftest pathname-device-common3
      (pathname-device (make-pathname :host "unix-host"
				      :device "WEIRD")
		       :case :local)
    "WEIRD"))
(deftest pathname-device-nil
    (pathname-device (make-pathname :device nil)) nil)

;;; PATHNAME-DIRECTORY
(deftest pathname-logical-directory-string-common
    (pathname-directory "mysys:foo;" :case :common)
  (:absolute "FOO"))
(deftest pathname-logical-directory-string-local
    (pathname-directory "mysys:foo;" :case :local)
  (:absolute "FOO"))
(deftest pathname-logical-directory-string-wild
    (pathname-directory "mysys:*;")
  (:absolute :wild))
(deftest pathname-directory-common
    (pathname-directory (make-pathname :host "unix-host"
				    :directory "weird")
		     :case :common)
  (:absolute "WEIRD"))
(deftest pathname-directory-common2
    (pathname-directory (make-pathname :host "unix-host"
				    :directory "WEIRD")
		     :case :common)
  (:absolute "weird"))
(deftest pathname-directory-common3
    (pathname-directory (make-pathname :host "unix-host"
				    :directory "WEIRD")
		     :case :local)
  (:absolute "WEIRD"))
(deftest pathname-directory-nil
    (pathname-directory (make-pathname :directory nil)) nil)


;;; PATHNAME-NAME
(deftest pathname-logical-name-string-common
    (pathname-name "mysys:foo" :case :common)
  "FOO")
(deftest pathname-logical-name-string-local
    (pathname-name "mysys:foo" :case :local)
  "FOO")
(deftest pathname-logical-name-string-wild
    (pathname-name "mysys:*")
  :wild)
(deftest pathname-name-common
    (pathname-name (make-pathname :host "unix-host"
				    :name "weird")
		     :case :common)
  "WEIRD")
(deftest pathname-name-common2
    (pathname-name (make-pathname :host "unix-host"
				    :name "WEIRD")
		     :case :common)
 "weird")
(deftest pathname-name-common3
    (pathname-name (make-pathname :host "unix-host"
				    :name "WEIRD")
		     :case :local)
  "WEIRD")
(deftest pathname-name-nil
    (pathname-name (make-pathname :name nil)) nil)

;;; PATHNAME-TYPE
(deftest pathname-logical-type-string-common
    (pathname-type "mysys:.foo" :case :common)
  "FOO")
(deftest pathname-logical-type-string-local
    (pathname-type "mysys:.foo" :case :local)
  "FOO")
(deftest pathname-logical-type-string-wild
    (pathname-type "mysys:.*" :case :local)
  :wild)
(deftest pathname-type-common
    (pathname-type (make-pathname :host nil
				  :type "weird")
		     :case :common)
  "WEIRD")
(deftest pathname-type-common2
    (pathname-type (make-pathname :host "unix-host"
				    :type "WEIRD")
		     :case :common)
 "weird")
(deftest pathname-type-common3
    (pathname-type (make-pathname :host "unix-host"
				    :type "WEIRD")
		     :case :local)
  "WEIRD")
(deftest pathname-type-nil
    (pathname-type (make-pathname :type nil)) nil)

;;; PATHNAME-VERSION
(deftest pathname-logical-version-string-common
    (pathname-version "mysys:.foo.9")
  9)
(deftest pathname-logical-version-string-common2
    (pathname-version "mysys:.foo.newest")
  :newest)
(deftest pathname-logical-version-string-common3
    (pathname-version "mysys:.foo.*")
  :wild)
(deftest pathname-logical-version-string-common4
    (pathname-version "mysys:.foo")
  nil)
(deftest pathname-version-nil
    (pathname-version (make-pathname :version nil)) nil)

;;; LOAD-LOGICAL-PATHNAME-TRANSLATIONS
(deftest load-logical-pathname-translations-nil
    (load-logical-pathname-translations "mysys")
  nil)

#+not-yet
(deftest load-logical-pathname-translations-nil
    (prog2
      (with-open-file (s "mysys:site;junk.host" :direction :output)
	(write '(setf (logical-pathname-translations "junk")
		 '(("foo;*.*.*"    "mysys:site;")))
	       :stream s))
      (null (load-logical-pathname-translations "junk"))
      (delete-file "mysys:site;junk.host"))
  nil)

;;; LOGICAL-PATHNAME-TRANSLATIONS
(deftest mysys-translations
    (let ((mysys (logical-pathname-translations "mysys")))
      (and mysys
	   (every #'(lambda (rule)
		      (destructuring-bind (log phys) rule
			(and (typep log 'logical-pathname)
			     (pathnamep phys))))
		  mysys)
	   (pathnamep (translate-logical-pathname
			"mysys:site;mysys.host"))))
  t)

(test-namestring set-translations
    (let (p)
      (and (setf (logical-pathname-translations "foo")
	     '(("**;*.*.*" "/library/foo/**/")))
	   (setq p (namestring (translate-logical-pathname
				"foo:bar;baz;mum.quux.3"))) 
	   (null (setf (logical-pathname-translations "foo") nil))
	   p))
    nil :unspecific (:absolute "LIBRARY" "FOO" "BAR" "BAZ")
    "MUM" "QUUX" 3)
	   
;;; LOGICAL-PATHNAME
(deftest logical-pathname-string
    (let ((s "mysys:foo"))
      (not (null (string-equal (namestring (logical-pathname s)) s))))
  t)
(deftest logical-pathname
    (let ((p (parse-namestring "foo" "mysys")))
      (eq p (logical-pathname p)))
  t)

;;; NAMESTRING
(deftest namestring-logical (namestring sample-logical)
  "MYSYS:SITE;JUNK.LISP.99")
(deftest namestring-string (namestring "unix-host:/foo/../bar/*.*.*")
  "unix-host:/foo/../bar/*.*.*")

;;; FILE-NAMESTRING
(deftest file-namestring-logical (file-namestring sample-logical)
  "JUNK.LISP.99")
(deftest file-namestring-string (file-namestring "unix-host:/foo/../bar/*.*.*")
  "*.*.*")

;;; DIRECTORY-NAMESTRING
(deftest directory-namestring-logical (directory-namestring sample-logical)
  "SITE;")
(deftest directory-namestring-string (directory-namestring "unix-host:/foo/../bar/*.*.*")
  "/foo/../bar/")

;;; host-namestring
(deftest host-namestring-logical (host-namestring sample-logical)
  "MYSYS")
(deftest host-namestring-string (host-namestring "unix-host:/foo/../bar/*.*.*")
  "unix-host")

;;; ENOUGH-NAMESTRING
(deftest enough-namestring-same
    (enough-namestring sample-logical sample-logical)
  "")
(deftest enough-namestring-empty
    (enough-namestring "" sample-logical)
  "")
(deftest enough-namestring-host
    (enough-namestring (make-pathname :host "unix-host") sample-logical)
  "unix-host:")
(deftest enough-namestring-full-directory
    (enough-namestring "mysys:a;b;" "mysys:x;")
  "A;B;")
(deftest enough-namestring-full-directory2
    (enough-namestring "mysys:a;b;" "mysys:")
  "A;B;")
(deftest enough-namestring-directory-extend
    (enough-namestring "mysys:a;b;" "mysys:a;")
  ";B;")
(test-namestring enough-namestring-directory-back
    (enough-namestring "/a/b/" "/a/b/c/")
    nil :unspecific (:relative :up) nil nil nil)
(test-namestring enough-namestring-directory-back-extend
    (enough-namestring "/a/b/x/" "/a/b/c/")
    nil :unspecific (:relative :up "X") nil nil nil)
(test-namestring enough-namestring-directory-back-extend-more
    (enough-namestring "/a/b/c/x/y/" "/a/b/m/n/")
    nil :unspecific (:relative :up :up "C" "X" "Y") nil nil nil)
(deftest enough-namestring-swallow-back
    (enough-namestring (make-pathname :directory '(:relative :back "x"))
		       "/a/x/")
  "")
(deftest enough-namestring-name
  (let ((*default-pathname-defaults* sample-logical))
    (enough-namestring "file" "different"))
  "FILE")
(deftest enough-namestring-type
  (let ((*default-pathname-defaults* sample-logical))
    (enough-namestring "file.lisp" "file.bin"))
  ".LISP")
(deftest enough-namestring-newest-version
    (enough-namestring "file.lisp.newest" "file.lisp.9")
  "")
(deftest enough-namestring-matching-version-but-different-name
  (let ((*default-pathname-defaults* sample-logical))
    (enough-namestring "file.lisp.9" "diff.bin.9"))
  "FILE.LISP.9")
    
;;; WILD-PATHNAME-P
(deftest wild-pathname-p-nil (wild-pathname-p sample-logical) nil)
(deftest wild-pathname-p-nil2 (wild-pathname-p sample-logical nil) nil)
(deftest wild-pathname-p-host-checked-normal
    (wild-pathname-p (make-pathname :host "mysys") :host)
  nil)
;; N.B. No implementation is required to accept a :host :wild argument
;; to make-pathname.
(deftest wild-pathname-p-host-checked-wild
    (wild-pathname-p (make-pathname :host :wild) :host)
  t)
(deftest wild-pathname-p-host-wild
    (wild-pathname-p (make-pathname :host :wild :defaults sample-logical))
  t)

(deftest wild-pathname-p-device-checked-normal
    (wild-pathname-p (make-pathname :device :unspecific) :device)
  nil)
;; N.B. No implementation is required to accept a :device :wild argument
;; to make-pathname.
(deftest wild-pathname-p-device-checked-wild
    (wild-pathname-p (make-pathname :device :wild) :device)
  t)
(deftest wild-pathname-p-device-wild
    (wild-pathname-p (make-pathname :device :wild :defaults sample-logical))
  t)

(deftest wild-pathname-p-directory-checked-normal
    (wild-pathname-p (make-pathname :directory "foo") :directory)
  nil)
(deftest wild-pathname-p-directory-checked-wild
    (wild-pathname-p (make-pathname :directory :wild) :directory)
  t)
(deftest wild-pathname-p-directory-wild
    (wild-pathname-p (make-pathname :directory :wild :defaults sample-logical))
  t)

(deftest wild-pathname-p-name-checked-normal
    (wild-pathname-p (make-pathname :name "foo") :name)
  nil)
(deftest wild-pathname-p-name-checked-wild
    (wild-pathname-p (make-pathname :name :wild) :name)
  t)
(deftest wild-pathname-p-name-wild
    (wild-pathname-p (make-pathname :name :wild :defaults sample-logical))
  t)

(deftest wild-pathname-p-type-checked-normal
    (wild-pathname-p (make-pathname :type "foo") :type)
  nil)
(deftest wild-pathname-p-type-checked-wild
    (wild-pathname-p (make-pathname :type :wild) :type)
  t)
(deftest wild-pathname-p-type-wild
    (wild-pathname-p (make-pathname :type :wild :defaults sample-logical))
  t)

(deftest wild-pathname-p-version-checked-normal
    (wild-pathname-p (make-pathname :version 9) :version)
  nil)
(deftest wild-pathname-p-version-checked-wild
    (wild-pathname-p (make-pathname :version :wild) :version)
  t)
(deftest wild-pathname-p-version-wild
    (wild-pathname-p (make-pathname :version :wild :defaults sample-logical))
  t)

;; Our interpretation...
(deftest wild-pathname-p-host-checked-nil
    (wild-pathname-p (make-pathname :host nil) :host)
  t)
(deftest wild-pathname-p-device-checked-nil
    (wild-pathname-p (make-pathname :device nil) :device)
  t)
(deftest wild-pathname-p-directory-checked-nil
    (wild-pathname-p (make-pathname :directory nil) :directory)
  t)
(deftest wild-pathname-p-name-checked-nil
    (wild-pathname-p (make-pathname :name nil) :name)
  t)
(deftest wild-pathname-p-type-checked-nil
    (wild-pathname-p (make-pathname :type nil) :type)
  t)
(deftest wild-pathname-p-version-checked-nil
    (wild-pathname-p (make-pathname :version nil) :version)
  t)

;; Complex wildcards
(deftest wild-pathname-p-host-checked-partial-wild
    (wild-pathname-p (make-pathname :host "f*o") :host)
  t)
(deftest wild-pathname-p-device-checked-partial-wild
    (wild-pathname-p (make-pathname :device "f*o") :device)
  t)
(deftest wild-pathname-p-directory-checked-partial-wild
    (wild-pathname-p (make-pathname :directory "f*o") :directory)
  t)
(deftest wild-pathname-p-name-checked-partial-wild
    (wild-pathname-p (make-pathname :name "f*o") :name)
  t)
(deftest wild-pathname-p-type-checked-partial-wild
    (wild-pathname-p (make-pathname :type "f*o") :type)
  t)

;;; PATHNAME-MATCH-P
(deftest match (pathname-match-p "foo/bar.lisp" "foo/bar.lisp") t)
(deftest no-match (pathname-match-p "foo/baz.lisp" "foo/bar.lisp") nil)
(deftest match-wild-host (let ((*default-pathname-defaults* sample-logical))
			   (pathname-match-p "foo:" "*:"))
  t)
(deftest match-null-host (pathname-match-p "foo:" (make-pathname :host nil :device nil
								 :version nil)) t)
(deftest match-wild-same-host (pathname-match-p "foo:" "foo:") t)
(deftest match-diff-host (let ((*default-pathname-defaults* sample-logical))
			   (pathname-match-p "foo:" "mysys:"))
  nil)
#+arbitrary-pathname-devices
(progn
  (deftest match-wild-device
      (pathname-match-p (make-pathname :device :x) (make-pathname :device :wild)) t)
  (deftest match-null-device
      (pathname-match-p (make-pathname :device :x)
			(make-pathname :host nil :device nil :version nil)) t)
  (deftest match-same-device
      (pathname-match-p (make-pathname :device :x) (make-pathname :device :x)) t)
  (deftest match-dif-device
      (pathname-match-p (make-pathname :device :x) (make-pathname :device :y)) nil))
(deftest match-wild-dir (pathname-match-p "/foo/" "/*/") t)
(deftest match-null-dir (pathname-match-p "/foo/"
					  (make-pathname :host nil :device nil
							 :directory nil :version nil)) t)
(deftest match-same-dir (pathname-match-p "/foo/" "/foo/") t)
(deftest match-diff-dir (pathname-match-p "/foo/" "/bar/") nil)
(deftest match-diff-dir2 (pathname-match-p "foo/" "/*/") nil)
(deftest match-diff-dir3 (pathname-match-p "/foo/" "*/") nil)
(deftest match-wild-name (pathname-match-p "foo" "*") t)
(deftest match-null-name (pathname-match-p "foo" (make-pathname :host nil
								:device nil
								:name nil :version nil)) t)
(deftest match-same-name (pathname-match-p "foo" "foo") t)
(deftest match-dif-name (pathname-match-p "foo" "foo1") nil)
(deftest match-wild-type (pathname-match-p "mysys:.lisp" "mysys:.*") t)
(deftest match-null-type (pathname-match-p "mysys:.lisp"
					   (make-pathname :host "mysys" :type nil :version nil)) t)
(deftest match-same-type (pathname-match-p "mysys:.lisp" "mysys:.lisp") t)
(deftest match-diff-type (pathname-match-p "mysys:.lisp" "mysys:.bin") nil)
(deftest match-wild-version (pathname-match-p "mysys:.lisp.9" "mysys:.lisp.*") t)
(deftest match-null-version (pathname-match-p "mysys:.lisp.9"
					      (make-pathname :host "mysys" :type "lisp" :version nil)) t)
(deftest match-same-version (pathname-match-p "mysys:.lisp.9" "mysys:.lisp.9") t)
(deftest match-diff-version (pathname-match-p "mysys:.lisp.9" "mysys:.lisp.8") nil)
(deftest match-wild-subdir (pathname-match-p "foo/bar/" "foo/*/") t)
(deftest no-match-wild-subdir (pathname-match-p "foo/bar/" "foo/*/baz/") nil)
(deftest match-wild-inferiors (pathname-match-p "foo/baz/" "foo/**/baz/") t)
(deftest match-wild-inferiors2 (pathname-match-p "foo/bar/baz/" "foo/**/baz/") t)
(deftest match-wild-inferiors3 (pathname-match-p "foo/bar/fly/baz/" "foo/**/baz/") t)
(deftest no-match-wild-inferiors (pathname-match-p "foo/bar/fly/" "foo/**/baz/") nil)

;; Complex wildcards
(deftest match-complex1 (pathname-match-p "foo" "f*") t)
(deftest match-complex2 (pathname-match-p "foo" "*") t)
(deftest match-complex3 (pathname-match-p "foo" "f?o") t)
(deftest match-complex4 (pathname-match-p "foo" "f*x") nil)
(deftest match-complex5 (pathname-match-p "fo" "f?o") nil)


;;; TRANSLATE-PATHNAME
(deftest translate-host-nil-nil
    (pathname-host (translate-pathname (make-pathname :host "FOO")
					 (make-pathname :host nil)
					 (make-pathname :host nil)))
  "FOO")
(deftest translate-host-nil-text
    (pathname-host (translate-pathname (make-pathname :host "FOO")
					 (make-pathname :host "FOO")
					 (make-pathname :host nil)))
  "FOO")
(deftest translate-host-wild-text
    (pathname-host (translate-pathname (make-pathname :host "FOO")
					 (make-pathname :host "FOO")
					 (make-pathname :host :wild)))
  "FOO")
(deftest translate-host-wild-nil
    (pathname-host (translate-pathname (make-pathname :host "FOO")
					 (make-pathname :host nil)
					 (make-pathname :host :wild)))
  "FOO")
(deftest translate-host-wild-wild
    (pathname-host (translate-pathname (make-pathname :host "FOO")
					 (make-pathname :host :wild)
					 (make-pathname :host :wild)))
  "FOO")
(deftest translate-host-not-wild
    (pathname-host (translate-pathname (make-pathname :host "FOO")
					 (make-pathname :host "FOO")
					 (make-pathname :host "BAR")))
  "BAR")

(deftest translate-device-nil-nil
    (pathname-device (translate-pathname (make-pathname :device "FOO")
					 (make-pathname :device nil)
					 (make-pathname :device nil)))
  "FOO")
(deftest translate-device-nil-text
    (pathname-device (translate-pathname (make-pathname :device "FOO")
					 (make-pathname :device "FOO")
					 (make-pathname :device nil)))
  "FOO")
(deftest translate-device-wild-text
    (pathname-device (translate-pathname (make-pathname :device "FOO")
					 (make-pathname :device "FOO")
					 (make-pathname :device :wild)))
  "FOO")
(deftest translate-device-wild-nil
    (pathname-device (translate-pathname (make-pathname :device "FOO")
					 (make-pathname :device nil)
					 (make-pathname :device :wild)))
  "FOO")
(deftest translate-device-wild-wild
    (pathname-device (translate-pathname (make-pathname :device "FOO")
					 (make-pathname :device :wild)
					 (make-pathname :device :wild)))
  "FOO")
(deftest translate-device-not-wild
    (pathname-device (translate-pathname (make-pathname :device "FOO")
					 (make-pathname :device "FOO")
					 (make-pathname :device "BAR")))
  "BAR")

(deftest translate-directory-nil-nil
    (pathname-directory (translate-pathname (make-pathname :directory "FOO")
					 (make-pathname :directory nil)
					 (make-pathname :directory nil)))
  (:absolute "FOO"))
(deftest translate-directory-nil-text
    (pathname-directory (translate-pathname (make-pathname :directory "FOO")
					 (make-pathname :directory "FOO")
					 (make-pathname :directory nil)))
  (:absolute "FOO"))
(deftest translate-directory-wild-text
    (pathname-directory (translate-pathname (make-pathname :directory "FOO")
					 (make-pathname :directory "FOO")
					 (make-pathname :directory :wild)))
  (:absolute "FOO"))
(deftest translate-directory-wild-nil
    (pathname-directory (translate-pathname (make-pathname :directory "FOO")
					 (make-pathname :directory nil)
					 (make-pathname :directory :wild)))
  (:absolute "FOO"))
(deftest translate-directory-wild-wild
    (pathname-directory (translate-pathname (make-pathname :directory "FOO")
					 (make-pathname :directory :wild)
					 (make-pathname :directory :wild)))
  (:absolute "FOO"))
(deftest translate-directory-not-wild
    (pathname-directory (translate-pathname (make-pathname :directory "FOO")
					 (make-pathname :directory "FOO")
					 (make-pathname :directory "BAR")))
  (:absolute "BAR"))

(deftest translate-name-nil-nil
    (pathname-name (translate-pathname (make-pathname :name "FOO")
					 (make-pathname :name nil)
					 (make-pathname :name nil)))
  "FOO")
(deftest translate-name-nil-text
    (pathname-name (translate-pathname (make-pathname :name "FOO")
					 (make-pathname :name "FOO")
					 (make-pathname :name nil)))
  "FOO")
(deftest translate-name-wild-text
    (pathname-name (translate-pathname (make-pathname :name "FOO")
					 (make-pathname :name "FOO")
					 (make-pathname :name :wild)))
  "FOO")
(deftest translate-name-wild-nil
    (pathname-name (translate-pathname (make-pathname :name "FOO")
					 (make-pathname :name nil)
					 (make-pathname :name :wild)))
  "FOO")
(deftest translate-name-wild-wild
    (pathname-name (translate-pathname (make-pathname :name "FOO")
					 (make-pathname :name :wild)
					 (make-pathname :name :wild)))
  "FOO")
(deftest translate-name-not-wild
    (pathname-name (translate-pathname (make-pathname :name "FOO")
					 (make-pathname :name "FOO")
					 (make-pathname :name "BAR")))
  "BAR")

(deftest translate-type-nil-nil
    (pathname-type (translate-pathname (make-pathname :type "FOO")
					 (make-pathname :type nil)
					 (make-pathname :type nil)))
  "FOO")
(deftest translate-type-nil-text
    (pathname-type (translate-pathname (make-pathname :type "FOO")
					 (make-pathname :type "FOO")
					 (make-pathname :type nil)))
  "FOO")
(deftest translate-type-wild-text
    (pathname-type (translate-pathname (make-pathname :type "FOO")
					 (make-pathname :type "FOO")
					 (make-pathname :type :wild)))
  "FOO")
(deftest translate-type-wild-nil
    (pathname-type (translate-pathname (make-pathname :type "FOO")
					 (make-pathname :type nil)
					 (make-pathname :type :wild)))
  "FOO")
(deftest translate-type-wild-wild
    (pathname-type (translate-pathname (make-pathname :type "FOO")
					 (make-pathname :type :wild)
					 (make-pathname :type :wild)))
  "FOO")
(deftest translate-type-not-wild
    (pathname-type (translate-pathname (make-pathname :type "FOO")
					 (make-pathname :type "FOO")
					 (make-pathname :type "BAR")))
  "BAR")

(deftest translate-version-nil-nil
    (pathname-version (translate-pathname (make-pathname :version 9)
					 (make-pathname :version nil)
					 (make-pathname :version nil)))
  9)
(deftest translate-version-nil-text
    (pathname-version (translate-pathname (make-pathname :version 9)
					 (make-pathname :version 9)
					 (make-pathname :version nil)))
  9)
(deftest translate-version-wild-text
    (pathname-version (translate-pathname (make-pathname :version 9)
					 (make-pathname :version 9)
					 (make-pathname :version :wild)))
  9)
(deftest translate-version-wild-nil
    (pathname-version (translate-pathname (make-pathname :version 9)
					 (make-pathname :version nil)
					 (make-pathname :version :wild)))
  9)
(deftest translate-version-wild-wild
    (pathname-version (translate-pathname (make-pathname :version 9)
					 (make-pathname :version :wild)
					 (make-pathname :version :wild)))
  9)
(deftest translate-version-not-wild
    (pathname-version (translate-pathname (make-pathname :version 9)
					 (make-pathname :version 9)
					 (make-pathname :version 10)))
  10)

(test-pathname1 translate-directory-match-by-wild-not-depth
    (translate-pathname "/foo/bar/baz/blue/red/yellow/"
				    "/foo/bar/*/*/red/*/"
				    "a/*/b/*/*/")
    nil :unspecific (:relative "A" "BAZ" "B" "BLUE" "YELLOW") nil nil nil)

(test-pathname1 translate-directory-wild-inferiors
 (translate-pathname "/x/y/z/"
				    "/x/**/"
				    "a/b/**/")
 nil :unspecific (:relative "A" "B" "Y" "Z") nil nil nil)
(test-pathname1 translate-directory-wild-inferiors1
 (translate-pathname "/f/o/o/b/a/r/"
				    "/f/o/o/**/"
				    "**/b/a/z/")
 nil :unspecific (:relative "B" "A" "R" "B" "A" "Z") nil nil nil)
(test-pathname1 translate-directory-wild-inferiors2
    (translate-pathname "/f/o/o/b/a/r/"
				    "/f/o/o/**/"
				    "**/")
    nil :unspecific (:relative "B" "A" "R") nil nil nil)
(test-pathname1 translate-directory-wild-inferiors3
    (translate-pathname "/f/o/o/b/a/r/"
				    "/**/"
				    "f/o/o/**/")
    nil :unspecific (:relative "F" "O" "O" "F" "O" "O" "B" "A" "R") nil nil nil)
(test-pathname1 translate-directory-wild-inferiors4
 (translate-pathname "/b/a/r/"
				    "/**/"
				    "f/o/o/**/")
  nil :unspecific (:relative "F" "O" "O" "B" "A" "R") nil nil nil)
(test-pathname1 translate-directory-wild-inferiors5
    (translate-pathname "/l/a/m/b/-/r/e/c/i/p/e/s/"
				    "/**/-/r/e/c/i/p/e/s/"
				    "/j/o/e/s/-/**/-/r/e/c/")
  nil :unspecific (:absolute "J" "O" "E" "S" "-" "L" "A" "M" "B" "-" "R" "E" "C")
  nil nil nil)


(test-pathname1 translate 
		    (translate-pathname "/usr/me/init.lisp"
					"/usr/me/*.lisp"
					"/dev/her/*.l")
  nil :unspecific (:absolute "DEV" "HER") "INIT" "L" nil)
(test-pathname1 translate2 (namestring
		     (translate-pathname "/usr/me/foo.bar"
					 "/usr/me/foo.bar"
					 "/usr/me2/"))
  nil :unspecific (:absolute "USR" "ME2") "FOO" "BAR" nil)

(deftest translate3 (namestring
		     (translate-pathname "MYSYS:CODE;BASIC.LISP"
					 "MYSYS:CODE;"
					 "unix-host:/lib/foo/"))
  "unix-host:/lib/foo/basic.lisp")

(deftest translate4 (namestring
		     (translate-pathname "MYSYS:MAIL;SAVE;MORE;IDEAS.MAIL.3"
					 "MYSYS:MAIL;**;*.MAIL"
					 "unix-host:/joe/mail/**/*.mbx"))
  "unix-host:/joe/mail/save/more/ideas.mbx.3")

;; Complex wildcards
(test-pathname1 translate5 
		     (translate-pathname "/usr/me/pcl-5-may/low.lisp"
					 "/usr/me/pcl*/*"
					 "/sys/pcl/*/")
  nil :unspecific (:absolute "SYS" "PCL" "-5-MAY") "LOW" "LISP" nil)
(test-pathname1 translate6 
		     (translate-pathname "/usr/joe/lamb-recipes.text"
					 "/usr/joe/*-recipes.text"
					 "/usr/jim/personal/cookbook/joe's-*-rec.text")
  nil :unspecific (:absolute "USR" "JIM" "PERSONAL" "COOKBOOK")
  "JOE'S-LAMB-REC" "TEXT" nil)
(test-pathname1 translate7 
		     (translate-pathname "/usr/dmr/hacks/frob.l"
					 "/usr/d*/hacks/*.l"
					 "/usr/d*/backup/hacks/backup-*.*")
  nil :unspecific (:absolute "USR" "DMR" "BACKUP" "HACKS") "BACKUP-FROB" "L" nil)
(test-pathname1 translate8 
		     (translate-pathname "/usr/dmr/hacks/frob.l"
					 "/usr/d*/hacks/fr*.l"
					 "/usr/d*/backup/hacks/backup-*.*")
  nil :unspecific (:absolute "USR" "DMR" "BACKUP" "HACKS") "BACKUP-OB" "L" nil)
