;;; Assume a directory structure:
;;; a
;;; b
;;; c	1
;;;    	2
;;;    	3	x
;;;      	y
;;;    	4  	x
;;;      	y
;;;             z -> ../3/x
;;;             zz -> ../../d/3
;;; d  	1
;;;    	2
;;;    	3	x
;;;       	y
;;;    	4	x
;;;       	y
;;; abcd 123
;;;      file.lisp.1
;;;      file.lisp.2
;;;      file.lisp.3
;;;      file.bin.2
;;;      thing.text
;;;      thing.text.1
;;;      link.dir.1 => ../d/3
;;;      link.file.1 => ../d/3/x

(defparameter dir-test-root-namestring
  (dolist (f *features*)
    (case f
      ((:hp-ux :hpux) (return "/hp7xx/cad0/magnetic/5/howard/"))
      (:sparc (return "/usr2/howard/"))
      ((:linux86 :linux) (return "/home/howard/cad0/howard/"))
      (:windows (return "c:\\howard\\")))))


(eval-when (:execute)
  (unless (fboundp 'list-components)
    ;; Usually defined in pathname.lisp
    (defun list-components (pathname)
      ;; Implementations are allowed to use a different representation for
      ;; hosts.
      (when pathname
	(list (let ((h (pathname-host pathname :case :common)))
		(typecase h
		  (string h)
		  (symbol h)
		  (t (host-namestring pathname))))
	      (pathname-device pathname :case :common)
	      (pathname-directory pathname :case :common)
	      (pathname-name pathname :case :common)
	      (pathname-type pathname :case :common)
	      (pathname-version pathname))))))


(defparameter junk-file (merge-pathnames "junk.text.unspecific"))

(deftest probe-non
  (progn (when (probe-file junk-file) (delete-file junk-file))
	 (probe-file junk-file))
  nil)

;;; How close is get-universal-time to file-write-date?
(defparameter time-fudge
  (dolist (feature *features* 0)
    (case feature
      (:windows (return 2))
      (:sun (return 2)))))
    

(deftest write-file
  (let (path true probe author size)
    (values
     (with-open-file (s junk-file :direction :output
			:element-type 'base-char
			:if-does-not-exist :create
			:if-exists :supersede)
       (setq path (pathname s)
	     true (truename s)
	     probe (probe-file s)
	     author (file-author s))
       (let ((p0 (file-position s))
	     (p00 (file-length s)))
	 (write 42 :stream s) (write-char #\space s)
	 (write 'foo :stream s)
	 (finish-output s)
	 (setq size (file-position s))
	 (list (and p0 p00 (= p0 p00 0))
	       (<= (file-write-date s) (+ time-fudge (get-universal-time)))
	       (let ((length (file-length s)))
		 (and length (> size 0) (>= size length))))))
     (list (equal path (pathname junk-file))
	   (equal path junk-file)
	   (equal true (truename junk-file))
	   (equal true path)
	   (equal probe (probe-file junk-file))
	   (equal probe path)
	   (equal author (file-author junk-file))
	   (<= (file-write-date junk-file) (+ time-fudge (get-universal-time))))))
  (T T T) (T T T T T T T T))

(deftest read-file
  (with-open-file (s junk-file :element-type 'base-char)
    (values (read s) (read s) (read s nil 99)))
  42 foo 99)
   

(defparameter dir-test-root-dir
  (pathname-directory dir-test-root-namestring :case :common))

(eval-when (:load-toplevel :execute)
  (setf (logical-pathname-translations "DIR")
	`(("**;*.*.*"  ,(concatenate 'string dir-test-root-namestring
				     "**/*.*.*")))))
(defparameter dir-test-root (pathname (concatenate 'string
						   dir-test-root-namestring
						   "test/")))

(defun test-dir1 (relative-path results keys)
  (let ((*default-pathname-defaults* dir-test-root))
    (set-exclusive-or
     (mapcar #'enough-namestring
	     (apply #'directory
		    (merge-pathnames relative-path dir-test-root nil)
		    keys))
     results
     :key #'namestring
     :test #'equal)))
(defmacro test-dir (name relative-path results &rest keys)
  `(deftest ,name (test-dir1 ,relative-path ',results ',keys) nil))

(defun frob-file-results (pathname)
  (let* ((result (list-components pathname))
	 (dirs (nth 2 result)))
    (setf (nth 1 result) :unspecific)
    (setf (nth 2 result)
	  (dolist (dir dir-test-root-dir dirs)
	    (let ((got (first dirs)))
	      (if (equal dir got)
		  (pop dirs)
		  (return dirs)))))
    result))

(defmacro test-pathnamen (name pathname &body components)
  `(deftest ,name
     (frob-file-results ,pathname)
     ,components))

;;; PROBE-FILE
(test-pathnamen probe-file (probe-file "dir:test;abcd;file.lisp.1")
	       :unspecific :unspecific ( "TEST" "ABCD") "FILE" "LISP" 1)
(test-pathnamen probe-directory
	       (probe-file (make-pathname :host "dir"
					  :directory '(:absolute "test" "abcd")
					  :name :unspecific
					  :type :unspecific
					  :version :unspecific))
	       :unspecific :unspecific ( "TEST" "ABCD") :unspecific :unspecific :unspecific)
(test-pathnamen probe-directory-as-file
	       (probe-file (make-pathname :host "dir"
					  :directory '(:absolute "test")
					  :name "abcd"
					  :type :unspecific
					  :version :unspecific))
	       :unspecific :unspecific ( "TEST" "ABCD") :unspecific :unspecific :unspecific)
(deftest probe-non-existent (probe-file "dir:test;abcd;file.lisp.99") nil)
(test-pathnamen probe-file-merged
	       (let ((*default-pathname-defaults* (pathname "dir:test;file.lisp.1")))
		 (probe-file ";abcd;"))
	       :unspecific :unspecific ( "TEST" "ABCD") "FILE" "LISP" 1)
(test-pathnamen probe-newest1
	       (probe-file (make-pathname :host "dir"
					  :directory '(:absolute "test" "abcd")
					  :name "file"
					  :type "lisp"
					  :version :newest))
	       :unspecific :unspecific ( "TEST" "ABCD") "FILE" "LISP" 3)
(test-pathnamen probe-newest2
	       (probe-file (make-pathname :host "dir"
					  :directory '(:absolute "test" "abcd")
					  :name "thing"
					  :type "text"
					  :version :newest))
	       :unspecific :unspecific ( "TEST" "ABCD") "THING" "TEXT" :unspecific)

;;; TRUENAME
(test-pathnamen truename (truename "dir:test;abcd;file.lisp.1")
	       :unspecific :unspecific ( "TEST" "ABCD") "FILE" "LISP" 1)
(test-pathnamen truename-directory
	       (truename (make-pathname :host "dir"
					  :directory '(:absolute "test" "abcd")
					  :name :unspecific
					  :type :unspecific
					  :version :unspecific))
	       :unspecific :unspecific ( "TEST" "ABCD") :unspecific :unspecific :unspecific)
(test-pathnamen truename-directory-as-file
	       (truename (make-pathname :host "dir"
					  :directory '(:absolute "test")
					  :name "abcd"
					  :type :unspecific
					  :version :unspecific))
	       :unspecific :unspecific ( "TEST" "ABCD") :unspecific :unspecific :unspecific)
(test-pathnamen truename-merged
	       (let ((*default-pathname-defaults* (pathname "dir:test;file.lisp.1")))
		 (truename ";abcd;"))
	       :unspecific :unspecific ( "TEST" "ABCD") "FILE" "LISP" 1)
(test-pathnamen truename-newest1
	       (truename (make-pathname :host "dir"
					  :directory '(:absolute "test" "abcd")
					  :name "file"
					  :type "lisp"
					  :version :newest))
	       :unspecific :unspecific ( "TEST" "ABCD") "FILE" "LISP" 3)
(test-pathnamen truename-newest2
	       (truename (make-pathname :host "dir"
					  :directory '(:absolute "test" "abcd")
					  :name "thing"
					  :type "text"
					  :version :newest))
	       :unspecific :unspecific ( "TEST" "ABCD") "THING" "TEXT" :unspecific)


;;; Show that pathname-match-p uses the file system to resolve :newest!!!
;;; Show that a link with a type/version can be resolved to a file without a type/version

;;; DIRECTORY
(test-dir dir-file "a" ("a"))
(test-dir dir-dir "c/" ("c/1" "c/2" "c/3/" "c/4/"))
(test-dir dir-dir-as-file "c" ("c/"))
(test-dir dir-missing "zzz" ())
(test-dir dir-wild-file "*" ("a" "b" "c/" "d/" "abcd/"))
(test-dir dir-complex-file "abcd/1*" ("abcd/123"))
(test-dir dir-complex-file2 "abcd/1?3" ("abcd/123"))

(test-dir dir-wild-fail "*/a" ())
(test-dir dir-wild-end "*/1" ("c/1" "d/1"))
(test-dir dir-wild-dir "*/3/x" ("c/3/x" "d/3/x"))
(test-dir dir-wild-wild "*/*/x" ("c/3/x" "d/3/x" "c/4/x" "d/4/x"))

(test-dir dir-wild-inf-none "**/a" ("a"))
(test-dir dir-wild-inf-one2 "**/3/x" ("c/3/x" "d/3/x"))
(test-dir dir-wild-inf-one "**/1" ("c/1" "d/1"))
(test-dir dir-wild-inf-two "**/x" ("c/3/x" "d/3/x" "c/4/x" "d/4/x"))
	  
(test-dir dir-complex "*d/1*" ("d/1" "abcd/123"))
(test-dir dir-complex2 "*d/*3" ("d/3/" "abcd/123"))
(test-dir dir-complex3 "*d/*3/" ("d/3/x" "d/3/y"))

(test-dir dir-link "c/4/z*" ("c/3/x" "d/3/"))
(test-dir dir-link2 "c/4/z*" ("c/4/z" "c/4/zz") :links nil)

(test-dir dir-type "abcd/file.*.2" ("abcd/file.lisp.2" "abcd/file.bin.2"))
(test-dir dir-version "abcd/file.lisp.*"
	  ("abcd/file.lisp.1" "abcd/file.lisp.2" "abcd/file.lisp.3"))
(deftest dir-unspecfic-version
    (destructuring-bind (a b)
	(directory (merge-pathnames "abcd/thing.text.*"
		    dir-test-root))
      (or (and (eql (pathname-version a) 1)
	       (eql (pathname-version b) :unspecific))
	  (and (eql (pathname-version b) 1)
	       (eql (pathname-version a) :unspecific))))
  t)
(test-dir dir-newest "abcd/file.lisp.newest" ("abcd/file.lisp.3"))
(test-dir dir-newest2 "abcd/file.lisp.0" ("abcd/file.lisp.3"))
(test-dir dir-previous "abcd/file.lisp.-1" ("abcd/file.lisp.2"))
(test-dir dir-oldest "abcd/file.lisp.oldest" ("abcd/file.lisp.1"))

(test-dir dir-newest-unsp "abcd/thing.text.newest" ("abcd/thing.text"))
(test-dir dir-previous-unsp "abcd/thing.text.previous" ("abcd/thing.text.1"))
(test-dir dir-oldest-unsp "abcd/thing.text.oldest" ("abcd/thing.text.1"))

(test-dir dir-newest-sole "abcd/123.*.newest" ("abcd/123"))
(test-dir dir-previous-sole "abcd/123.*.previous" ())
(test-dir dir-oldest-sole "abcd/123.*.oldest" ("abcd/123"))
(test-dir dir-wild-and-newest "abcd/*.*.newest"
	  ("abcd/thing.text" "abcd/123" "abcd/file.lisp.3" "abcd/file.bin.2"
			     "d/3/" "d/3/x"))

(deftest user-homedir-pathname (let ((p (user-homedir-pathname)))
				 (values
				  (null (pathname-directory p :case :common))
				  (pathname-name p :case :common)
				  (pathname-type p :case :common)
				  (pathname-version p)))
  nil nil nil nil)