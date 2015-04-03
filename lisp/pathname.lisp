;;; CANONICALIZATION:
;;; 1. Components are stored in :common case using the same
;;;    case convensions as for symbols (in fact, symbols may someday
;;;    be used for components instead of strings).
;;; 2. Directories:
;;;     (:relative) => nil
;;;     :wild => (:absolute :wild-inferiors)
;;;     Removal of :back components is done only during merge.  This
;;;     means, for example, that (wild-pathname-p (make-pathname
;;;     :directory '(:absolute :wild :back))) returns true!
;;; 3. If some host has specific requirements (such as 3-letter
;;;    file types), then the canonicalize/localize methods for that
;;;    pathname class must be specialized so that it still represents
;;;    components in common canonical form.  For example, although
;;;    there is not yet anything which depends on it, the file types
;;;    "LISP", "TEXT", "C", "O", "BIN" are assumed to be canonical.  It
;;;    might be useful to define different hosts which localize "O"
;;;    in different ways: "EXE", "SPARC", "M68K", 

;;; Proposed enhancements:
;;; - In order to support network file access through ftp and
;;;   similar mechanims, we would like to someday make hosts be tree
;;;   structured like directories. 
;;; - In order to support various version control systems, we would
;;;   like to someday make version be tree structured like directories.
;;; - We would like someday to support cannonicalized home directories
;;;   such as (:absolute :home ...) or (:absolute (:home username)
;;;   ...). This allows us to specify a "home" directory component
;;;   which later gets a host merged in.  The actual directory would
;;;   be resolved only when needed by TRUENAME, PROBE-FILE, NAMESTRING
;;;   and DIRECTORY-NAMESTRING.


;;; !!! There are many places where we create temporary pathname
;;; objects:
;;;  - When coercing things to pathname using PATHNAME or
;;;    PARSE-NAMESTRING. 
;;;  - When creating result arguments in (un-)merge-pathnames
;;; We ought to consider defining a pathname resource utility so as to
;;; not create so much garbage.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utilities

;;; Prints like a structure so that when read, make-pathname will be
;;; called to recreate a similar pathname.  In order to obtain proper
;;; make-pathname behavior:
;;;  - print in local case by using accessor functions rather than
;;;    slot access (because :case argument isn't present at read) 
;;;  - give HOST and DEVICE so they don't come from
;;;    *default-pathname-defaults*  
;;;  - give version unless it is :newest
;;;  - give any other components only when they are not nil.
(defun print-pathname-readably (s pathname &rest ignore)
  (declare (ignore ignore))
  (let ((version (pathname-version pathname)))
    (format s (formatter "#S~:@<PATHNAME :HOST ~s :DEVICE ~s~
               ~@[ :DIRECTORY ~s~]~@[ :NAME ~s~]~
               ~@[ :TYPE ~s~]~:[ :VERSION ~s~;~*~]~:@>")
	    (pathname-host pathname)
	    (pathname-device pathname)
	    (pathname-directory pathname)
	    (pathname-name pathname)
	    (pathname-type pathname)
	    (eql version :newest)
	    version)))

(defvar *DEFAULT-PATHNAME-DEFAULTS*)

;;; PREDICATES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun wildcardp (thing)
  (typecase thing
    ((member nil :wild :wild-inferiors #\* #\?) t)
    (sequence (some #'wildcardp thing))
    (t nil)))

(defun pathname-component-equal (p1 p2 name)
  (pathname-item-equal p1 name
		       (pathname-component p1 name)
		       (pathname-component p2 name)))

;;; MATCHING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant *mnemonic-versions* '(:newest :oldest :previous :wild nil))
(defun fixed-version-p (v) (not (member v *mnemonic-versions*)))

;;; Determines if the part of source between start-s and end-s which
;;; exactly matches the regular expression regexp between start-r and
;;; end-r.  Regexp and source may be any sequence (list, string, etc.)
;;; Note that the concatenation of regexp's is a regexp.  The value
;;; returned for true is the index marking the end of that part of
;;; source which matches the first regular expression within regexp.
;;; Thus the value is suitable for use as the next start-s in a
;;; subsequent call to regexp.  

;;; This is written non-tail-recursive, but could be iterative!

;;; This does not use pathname-component-equal or -item-equal, so it
;;; will fail to match case-preserving, case-insensitive pathnames!!!

(defun regexp (source regexp start-s start-r end-s end-r)
  (macrolet ((end-source-p (index) `(= ,index end-s))
	     (end-regexp-p (index) `(= ,index end-r)))
    (let ((source-done-p (end-source-p start-s)))
      (if (end-regexp-p start-r)
	  (and source-done-p end-s)
	(let ((rest-source (unless source-done-p (1+ start-s)))
	      (rest-regexp (1+ start-r))
	      (key (elt regexp start-r)))
	  (case key
	    ((#\? :wild)	;Any single item.
	     (when (and rest-source
			(regexp source regexp
				rest-source rest-regexp end-s end-r))
	       rest-source))
	    ((#\* :wild-inferiors) ;0 or more items.
	     ;; Shortest possible match such that entire regexp matches.
	     (if (end-regexp-p rest-regexp)
		 end-s
	       (do ((nsource start-s (1+ nsource)))
		   ((end-source-p nsource) nil)
		 (when (regexp source regexp
			       nsource rest-regexp end-s end-r)
		   (return nsource)))))
	    (t (and (not source-done-p)
		    (let ((s (elt source start-s)))
		      (if (typep s 'sequence)
			  (regexp s key 0 0 (length s) (length key))
			(eql s key)))
		    (regexp source regexp
			    rest-source rest-regexp end-s end-r)
		    rest-source))))))))

(defun pathname-component-match (p1 p2 name)
  (pathname-item-match p1 name
		       (pathname-component p1 name)
		       (pathname-component p2 name)))


;;; NAMESTRINGS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun cached-namestring (pathname)
  (let ((pathname (pathname pathname)))
    (if (slot-boundp pathname 'namestring)
	(slot-value pathname 'namestring)
	(setf (slot-value pathname 'namestring)
	      (pathname-namestring pathname nil)))))

(defun namestring-error (pathname errorp)
  (when errorp
    (error 'file-error
	   :pathname pathname
	   :format-control
	   (formatter
	    "~/eclipse::print-pathname-readably/ cannot be converted into a string."))))

(defun local-case (pathname component name)
  (typecase component
    (string
     (with-output-to-string (s)
       (print-case component s :upcase
		   (readtable-case (pathname-readtable pathname name))
		   nil)))
    (cons (mapcar #'(lambda (comp)
		      (local-case pathname comp name))
		  component))
    (t component)))

;;; Convert an extended-string into a base-string, or return nil if
;;; impossible.   This should really use wctomb()!!!
(defun compactify-string (string)
  (loop with length = (length string)
	with new = (make-array length :element-type 'base-char)
	for i from 0 below length
	for char = (char string i)
	if (>= (char-int char) base-char-code-limit)
	return nil
	do (setf (schar new i) char)
	finally (return new)))

;;; Create a directory string.  If spacer is "/", ";", etc.
;;; leading is :absolute if a leading spacer indicates an absolute
;;; pathname, and :relative if a leading spacer indicates a relative
;;; pathname. 
(defun make-directory-namestring (spacer leading pathname errorp)
  (let ((dirs (pathname-local-component pathname :directory)))
    (when (or (search '(:absolute :up) dirs)
	      (search '(:absolute :back) dirs))
      (return-from make-directory-namestring
	(namestring-error pathname errorp)))
    (with-output-to-string (s)
      (when (eql leading (pop dirs))
	(write-string spacer s))
      (loop for dir in dirs
	    for str = (pathname-component-string
		       pathname dir :directory errorp)
	    unless str
	    do (return-from make-directory-namestring nil)
	    do (write-string str s)
	    do (write-string spacer s)))))

;;; PARSING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *logical-path*)			;instance of logical-pathname
;; list of pathname classes that are not stnadard-host-pathname
(defvar *nonstandard-host-syntax-classes*) 

;;; Defines the class that namestring will be parsed to.
;;; Use host if given.  Otherwise try to find a host in namestring.
;;; Otherwise, use the class of the defaults.
;;; Note that when trying to find a host in namestring, we check to
;;; make sure that any such host found is, in fact, registered as
;;; being a subtype of the class of pathname we used to parse.
  
(defun namestring-pathname-class (string host defaults start end)
  (or (when host
	(or (host-pathname-class host)
	    (error "Unknown host ~s." host)))
      ;; "host:xxxx" ...
      (let* ((host (pathname-parse-host *logical-path*
					string start end t))
	     (class (when host (host-pathname-class host nil))))
	(when host
	  (cond ((when class (subtypep class 'standard-host-pathname))
		 class)
		((and (eq host :wild)
		      (typep defaults 'standard-host-pathname))
		 (class-of defaults))
		((typep defaults 'standard-host-pathname)
		 (setq defaults nil)))))
      ;; other host syntaxes...
      (loop for class in *nonstandard-host-syntax-classes*
	    for host = (pathname-parse-host (class-prototype class)
					    string start end t)
	    for host-class = (when host
			       (host-pathname-class host nil))
	    when (and host-class (subtypep host-class class))
	    return host-class)
      (when defaults (class-of defaults))))

(defun illegal-parse-character (c)
  (error 'parse-error :format-control "Illegal character ~s."
	 :format-arguments (list c)))

(defparameter *pathname-parse-error-p* t)

;;; illegal-reader can be called directly or as a macro-character
;;; function for illegal characters.  When *pathname-parse-error-p* is nil, it
;;; silently unreads the illegal character and returns nil rather than
;;; actually signalling an error.
(defun illegal-reader (s c)
  (if *pathname-parse-error-p*
      (illegal-parse-character c)
    (unread-char c s))
  nil)

;;; Characters used to separate tokens can be assigned this function
;;; as a terminating macro-character.  It returns the stopper
;;; character itself.
(defun stopper-reader (s c) (declare (ignore s)) c)

;;; BUG ALERT!!!  

;;; We currently parse complex wildcard strings as ordinary strings,
;;; regardless of quoting.  Thus there is no way to include a literal
;;; #\* or #\? in a filename and not be expanded into a complex
;;; wildcard.  We should fix this by changing reads (i.e.
;;; read-preserving-whitespace, read-from-string) to a special
;;; complex-wildcard reader which creates a list of characters when it
;;; encounters unescaped complex wildcard characters, and replaces
;;; these characters with :wild, :wild-inferiors, etc.

;;; We then need to change the printer to print the "list-string"
;;; tokens as an unescaped string, and to print actual occurances of
;;; #\* and #\? characters using escapes.  This requires a change to
;;; the printer.  One issue is that namestrings passed to some
;;; operating systems should NOT have escape characters!  For example,
;;; the escape character systems we use for UNIX is really handled by
;;; the shell.  They characters should not appear when passed to
;;; kernel functions.

;;; Reads a token and returns it only if it ends in the specified
;;; stopper character (which is then absorbed).  Otherwise, this
;;; returns nil. (The unterminated token cannot be put back in the
;;; stream.)
(defun terminated-reader (s c &optional (alternate :not-specified))
  (let ((field (read-preserving-whitespace s nil)))
    (when (let ((char (peek-char nil s nil)))
	    (or (eql c char)
		(eql alternate char)))
      (read-char s nil)
      field)))

;;; This depends on reading terminating or illegal characters the same
;;; way we read eof.  We must read the whole next token and not just
;;; the next character because we must read at least two more
;;; characters to locate a :wild-inferiors token, and we would only be
;;; able to put one of them back to be read by the next token.  Also,
;;; we can't let :wild-inferiors be converted after parsing because
;;; the double wild character might have been quoted.
(defun double-reader (s c single double)
  (let* ((*pathname-parse-error-p* nil)
	 (next (read-preserving-whitespace s nil)))
    (cond ((null next) single)
	  ((eql next single) double)
	  ((eql next double) (make-array 3 :element-type (type-of c)
					 :initial-element c))
	  ((characterp next) (unread-char next s) single)
	  (t (with-output-to-string (s)
	       (write-char c s)
	       (princ next s))))))

;;; We really should allow :wild-inferiors only in directories!!!
(defun wild-reader (s c)
  (double-reader s c :wild :wild-inferiors))

(defun directory-dot-reader (s c)
  (double-reader s c :current :up))

(defmacro with-pathname-parsing ((stream-var pathname component-name
					     namestring start end
					     junk-allowed
					     &optional reversed)
				 &body body)
  (with-unique-names (terminal found)
    (rebinding (start)
      `(let* ((end (or end (length namestring)))
	      (*readtable* (pathname-readtable ,pathname ,component-name))
	      (*pathname-parse-error-p* (not ,junk-allowed))
	      ,terminal
	      (,found (with-input-from-string
			  (,stream-var ,namestring :index ,terminal
				       :start ,start :end ,end)
			,@body)))
	 (values ,found (if ,found ,terminal ,start)
		 ,@(when reversed `((if ,found (1- ,start) ,end))))))))


;;; For many pathname classes, a single unquoted char (such as / or ;)
;;; is used to terminate a directory components.  This function
;;; repeatedly uses terminated-reader to collect these components.  A
;;; leading char is taken to start an :absolute/:relative pathname as
;;; specified by the "leading" argument.  The "non-leading" argument
;;; should be the opposite.  The end of the successfully read string
;;; is returned as a second value.  
(defun directory-reader (pathname namestring start end junk-allowed
				  char leading non-leading
				  &optional (alternate :no-specified)
				  &aux dir)
  (when (and (< start (or end (length namestring)))
	     (let ((c (char namestring start)))
	       (or (eql char c)
		   (eql alternate c))))
    (incf start)
    (push leading dir))
  (do ((*pathname-parse-error-p* (not junk-allowed))
       (*readtable* (pathname-readtable pathname :directory))
       (start start) terminal
       (token t))
      ((null token)
       (cond ((not (dot-allowed-p pathname :directory)) nil)
	     ((string= "." namestring :start2 start :end2 end)
	      (incf start))
	     ((string= ".." namestring :start2 start :end2 end)
	      (incf start 2)
	      (if dir (push :up dir) (setq dir (list :up non-leading)))))
       (values (delete :current (nreverse dir)) start))
    (with-input-from-string (s namestring :index terminal
			       :start start :end end)
      (setq token (terminated-reader s char alternate)))
    (when token
      (unless dir (push non-leading dir))
      (push token dir)
      (setq start terminal))))


;;; MERGING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Returns a plist suitable for use with make-instance.
(defun merge-device (path devicep device host ddevice dhost)
  (cond (devicep `(:device ,device))
	((and host (pathname-item-equal path :host host dhost))
	 `(:device ,ddevice))))

;;; This flag is true when generating a truename, in which :up should
;;; be merged out as though it were :back.
(defparameter *generating-truename* nil)
(defun merge-directories (p d)
  (if (eq (car p) :relative)
      (let ((results (cons (or (pop d) :relative) nil)))
	(labels ((backp (dir)
		   (or (eq dir :back)
		       (and *generating-truename* (eq dir :up))))
		 (add (dir)
		   (if (and (backp dir)
			    (cdr results)
			    (not (backp (car results))))
		       (pop results)
		     (push dir results))))
	  (dolist (dir d) (add dir))
	  (dolist (dir (cdr p)) (add dir)))
	(nreverse results))
    (or p d)))

(defun unmerge-directories (p d pathname) ;Used by enough-namestring
  (let* ((p (if (eq (first p) :relative) (merge-directories p d) p))
	 (index (mismatch p d :test
			  #'(lambda (pc dc)
			      (pathname-item-equal pathname :directory pc dc))))
	 (n-to-back-up (if index (max 0 (- (length d) index)) 0)))
    (cond ((null index) nil)
	  ((and (> index 1)
		(or (zerop n-to-back-up)
		    ;; Can :back be used in namestring?
		    (pathname-component-string pathname :back :directory nil)))
	   `(:relative ,@(make-list n-to-back-up :initial-element :back)
		       ,@(nthcdr index p)))
	  (t p))))


;;; TRANSLATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun combine-subcomponents (&rest subs)
  (let ((key (find-if #'identity subs)))
    (if (listp key) (apply #'nconc subs)
      (apply #'concatenate (type-of key) subs))))

(defun wild-token-p (token)
  (case token
    ((nil :wild :wild-inferiors) t)
    (t nil)))

(defun canonicalize-comp (comp)
  (typecase comp
    ((or null (member :wild)) "*")
    (sequence comp)
    (t (list comp))))

;;; This does not use pathname-component-equal or -item-equal, so it
;;; will fail to match case-preserving, case-insensitive pathnames!!!
(defun translate-item (source from to)
  (if (wild-token-p to)
      (if (wild-token-p from) source
	  (typecase from
	    (string (translate-item source from "*"))
	    (list (translate-item source from '(:wild-inferiors)))
	    (t source)))
      (typecase to
	(sequence
	 (cond
	  ((wildcardp to)
	   (setf source (canonicalize-comp source) from (canonicalize-comp from))
	   (let ((end-s (length source)) (end-w (length from)) (end-m (length to)))
	     (do (result m (start-s 0) (start-w 0) (start-m 0))
		 ((null start-m) result)
	       (if (setq m (position-if #'wildcardp to
					:start start-m :end end-m))
		   (let* ((mastercard (elt to m))
			  (multi-m (member mastercard '(:wild-inferiors #\*)))
			  (wp (position-if #'wildcardp from
					   :start start-w :end end-w))
			  (w (or wp
				 (and multi-m start-w)
				 (error
				  "No wildcard in from-wildname ~s to match ~s in to-wildname."
				  (subseq from start-w) (subseq to m))))
			  (s (+ start-s (- w start-w)))
			  (s2 (regexp source from s w end-s end-w))
			  (wildcard (if wp (elt from wp) :wild-inferiors)))
		     (setq result (combine-subcomponents
				   result (subseq to start-m m)
				   (if (typep wildcard 'sequence)
				       (list (translate-item (elt source s)
							     wildcard
							     mastercard))
				       (subseq source s (if wp s2 end-s))))
			   start-s s2
			   start-m (1+ m)
			   start-w (1+ w)))
		   (setq result (combine-subcomponents
				 result (subseq to start-m end-m))
			 start-m nil)))))
	  (t to)))
	(t to))))

(defun translate-component (source-path from-path to-path slot)
  (let ((source (pathname-component source-path slot))
	(from (pathname-component from-path slot))
	(to (pathname-component to-path slot)))
    (if (eq slot :directory)
	;; This is bad, but it seems to be what the spec says...
	(let ((result (translate-item (cdr source)
				      (cdr from)
				      to)))
	  (if (and result
		   (not (member (car result) '(:absolute :relative))))
	      (cons (car source) result)
	    result))
      (translate-item source from to))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 23.1.6 PATHNAME FUNCTIONS

(defun PATHNAME-HOST (pathname &key (case :local))
  (let ((path (pathname pathname)))
    (ecase case
      (:common (pathname-component path :host))
      (:local (pathname-local-component path :host)))))

(defun PATHNAME-DEVICE (pathname &key (case :local))
  (let ((path (pathname pathname)))
    (ecase case
      (:common (pathname-component path :device))
      (:local (pathname-local-component path :device)))))

(defun PATHNAME-DIRECTORY (pathname &key (case :local))
  (let ((path (pathname pathname)))
    (ecase case
      (:common (pathname-component path :directory))
      (:local (pathname-local-component path :directory)))))

(defun PATHNAME-NAME (pathname &key (case :local))
  (let ((path (pathname pathname)))
    (ecase case
      (:common (pathname-component path :name))
      (:local (pathname-local-component path :name)))))

(defun PATHNAME-TYPE (pathname &key (case :local))
  (let ((path (pathname pathname)))
    (ecase case
      (:common (pathname-component path :type))
      (:local (pathname-local-component path :type)))))

(defun PATHNAME-VERSION (pathname)
  (pathname-component (pathname pathname) :version))

(defun HOST-NAMESTRING (pathname)
  (pathname-host-namestring (pathname pathname) t))

(defun DIRECTORY-NAMESTRING (pathname)
  (pathname-directory-namestring (pathname pathname) t))

(defun FILE-NAMESTRING (pathname)
  (pathname-file-namestring (pathname pathname) t))

(defun NAMESTRING (pathname)
  (or (cached-namestring pathname)
      (namestring-error pathname t)))
  
(defun ENOUGH-NAMESTRING (pathname &optional (defaults *default-pathname-defaults*))
  (let* ((def (pathname defaults))
	 (path (parse-namestring pathname nil def)))
    (namestring (pathname-unmerge path def))))

(defun MAKE-PATHNAME (&key (host nil hostp)
			   (device nil devicep)
			   (directory nil directoryp)
			   (name nil namep)
			   (type nil typep)
			   (version nil versionp)
			   defaults 
			   (case :local))
  (let* ((defaults (when defaults (pathname defaults)))
	 (def (or defaults *default-pathname-defaults*))
	 (dhost (pathname-component def :host))
	 (host (if hostp host dhost))
	 ;; If nil host, get local file system class.
	 ;; If UNKNOWN host, use generic pathname.
	 (class (host-pathname-class (or host :unspecific)
				     'pathname)))
    (ecase case
      (:common nil)
      (:local (let ((proto (class-prototype (find-class class))))
		(macrolet ((common (name)
				   `(when ,(make-name "~aP" name)
				      (setq ,name
					    (pathname-common-case
					     proto ,name ,(make-keyword name))))))
		  (common host)
		  (common device)
		  (common directory)
		  (common name)
		  (common type)
		  (common version)))))
    (apply #'make-instance class
	   :host host
	   :directory
	   (cond (directoryp
		  (etypecase directory
		    (list (unless (equal directory '(:relative))
			    directory))
		    (string `(:absolute ,directory))
		    ((eql :wild) '(:absolute :wild-inferiors))
		    (:unspecific :unspecific)))
		 (defaults (pathname-component defaults :directory)))
	   :name (cond (namep name)
		       (defaults (pathname-component defaults :name)))
	   :type (cond (typep type)
		       (defaults (pathname-component defaults :type)))
	   :version (cond (versionp version)
			  (namep :newest)
			  (t (or (when defaults
				   (pathname-component
				    defaults :version))
				 :newest)))
	   (merge-device def devicep device host
			 (pathname-component def :device)
			 dhost))))

        
(defun MERGE-PATHNAMES (pathname
			&optional (defaults *default-pathname-defaults*)
				  (default-version :newest))
  (let* ((def (pathname defaults))
	 (*default-pathname-defaults* def))
    (pathname-merge (pathname pathname) def default-version)))

;;; IWBNI we had a priorities list of pathname classes
;;; which we would try parsing with.  For example, we could then
;;; automatically distinguish "foo.c~" as a GNU pathname, and
;;; "//www.elwood.com" as a URL.  Fortunately, the behavior specified
;;; by the spec seems to preclude this.  It wreaks havoc with
;;; :junk-allowed, and could lead to great confusion regarding
;;; multiple potential parsings.
(defun PARSE-NAMESTRING (thing
			 &optional host defaults
			 &key (start 0) end junk-allowed)
  (etypecase thing
    (STRING (let* ((def (if defaults
			    (pathname defaults)
			    *default-pathname-defaults*))
		   (class1 (namestring-pathname-class
			    thing host def start end))
		   (class (typecase class1
			    (null (if junk-allowed
				      (return-from parse-namestring
					(values nil start))
				      (error 'parse-error :format-control
					     "~s is not a ~s host."
					     :format-arguments
					     (list
					      (pathname-parse-host
					       *logical-path* thing start end nil)
					      (class-name-of def)))))
			    (symbol (find-class class1))
			    (t class1)))
		   (proto (class-prototype class)))
	      (when host
		(setq host (pathname-common-case proto host :host)))
	      (multiple-value-setq (thing start)
		(pathname-parse-namestring
		 proto thing host start end junk-allowed))))
    (PATHNAME (when host
		(setq host (pathname-common-case thing host :host))))
    (FILE-STREAM (setq thing (file-stream-pathname thing))
		 (when host
		   (setq host (pathname-common-case thing host :host)))))
  (when (and host thing
	     (not (pathname-item-match thing :host host
				       (pathname-host thing :case :common))))
    (error 'parse-error :format-control
	   "Pathname host ~s does not match host argument ~s."
	   :format-arguments (list (pathname-host thing) host)))
  (values thing start))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 23.1.4 EXTENDED WILDCARDS

;;; We take the position (like pathname-match-p), that null components
;;; are wild.  Note, though that since parse-namestring does not do
;;; merging and therefore leaves fields blank, an unmerged namestring argument
;;; is likely to be wild SOMEWHERE.
(defun WILD-PATHNAME-P (pathname &optional field-key)
  (pathname-wild-p (pathname pathname) field-key))

(defun PATHNAME-MATCH-P (p1 p2)
  (pathname-match (pathname p1) (pathname p2)))
  
(defun TRANSLATE-PATHNAME (source from-wildname to-wildname)
  (setf source (pathname source)
	from-wildname (pathname from-wildname)
	to-wildname (pathname to-wildname))
  ;;Test required by ANSI.
  (unless (pathname-match-p source from-wildname)
    (error "Source ~s does not match from-wildname ~s." source from-wildname))
  (pathname-translate source from-wildname to-wildname))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                    PATHNAME PROTOCOL                         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass PATHNAME ()
  ((:host :initform nil :initarg :host)
   (:device :initarg :device)
   (:directory :initform nil :initarg :directory)
   (:name :initform nil :initarg :name)
   (:type :initform nil :initarg :type)
   (:version :initform nil :initarg :version)
   (namestring)
   (hash-code :initform nil)))

(defmethod SHARED-INITIALIZE :AFTER ((path PATHNAME) slot-names
				     &rest initargs)
  (declare (ignore slot-names initargs))
  (unless (slot-boundp path :device)
    (setf (slot-value path :device)
	  (pathname-default-device path))))

(defmethod MAKE-LOAD-FORM ((object PATHNAME) &optional environment)
  (make-load-form-saving-slots object :environment environment))

(defmethod PRINT-OBJECT ((pathname PATHNAME) stream)
  (let ((string (unless *print-readably* (cached-namestring pathname))))
    (cond (string (when *print-escape* (write-string "#P" stream))
		  (write string :stream stream :escape t))
	  ((or *print-escape* *print-readably*)
	   (print-pathname-readably stream pathname))
	  (t (namestring-error pathname t)))))

(let ((pathname (find-type 'pathname)))
  (defun PATHNAMEP (x) (typep x pathname)))

(defmethod PATHNAME ((pathname STRING))
  (values (parse-namestring pathname)))

(defmethod PATHNAME ((pathname PATHNAME)) pathname)

;;; ACCESS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Returns cannonicalized component.
;;; (eg. :host, :device, etc. in :common :case)
(defmethod PATHNAME-COMPONENT ((pathname PATHNAME) key)
  (slot-value pathname key))

;;; Returns component in :case :local.
(defmethod PATHNAME-LOCAL-COMPONENT ((pathname PATHNAME) component-name)
  (local-case pathname (pathname-component pathname component-name)
	      component-name))

(defmethod PATHNAME-DEFAULT-DEVICE ((pathname PATHNAME))
  :unspecific)

(defparameter *logical-pathname-readtable* (make-readtable))

(eval-when (:load-toplevel :execute)
  (setf (readtable-parse *logical-pathname-readtable*) nil)
  (dotimes (i base-char-code-limit)
    (let ((char (code-char i)))
      (unless (alphanumericp char)
	(set-macro-character char #'illegal-reader
			     nil *logical-pathname-readtable*))))
  
  (set-syntax-from-char #\- #\a *logical-pathname-readtable*)
  (set-macro-character #\* #'wild-reader t *logical-pathname-readtable*))

(defparameter *version-readtable* (copy-readtable *logical-pathname-readtable*))
(eval-when (:load-toplevel :execute)
  (setf (readtable-parse *version-readtable*) t))

(defmethod PATHNAME-READTABLE ((pathname PATHNAME) component-name)
  (case component-name
    (:version *version-readtable*)
    (t *logical-pathname-readtable*)))

;;; NAMESTRING ACCESS

;;; Called by pathname-xxx-namestring to translate components into
;;; appopriate strings.  Symbolic and other cannonicalized components
;;; must be translated into strings, and strings must be of the
;;; appropriate class.  If this can't be done then return nil or, if
;;; errorp, signal an error.

;;; We do not handle quoting of special characters.  For most systems,
;;; any quoting that we support is really mirroring some functionality
;;; of the command processor, not the kernal file-system functions.

(defmethod PATHNAME-COMPONENT-STRING ((pathname PATHNAME)
				      (component t)
				      (component-name t) errorp)
  (declare (ignore errorp))
  ;; Default method should never be called.
  (namestring-error pathname t))

(defmethod PATHNAME-COMPONENT-STRING ((pathname PATHNAME)
				      (component STRING)
				      component-name errorp)
  (declare (ignore component-name errorp))
  component)

(defmethod PATHNAME-COMPONENT-STRING ((pathname PATHNAME)
				      (component SYMBOL)
				      component-name errorp)
  (or (cdr (assoc component (pathname-key-map pathname component-name)))
      (namestring-error pathname errorp)))

(defmethod PATHNAME-COMPONENT-STRING ((pathname PATHNAME)
				      (component INTEGER)
				      (component-name (EQL :VERSION))
				      errorp)
  (declare (ignore errorp))
  (princ-to-string component))
  
(defconstant *standard-component-key-map*
  '((:wild . "*")
    (nil . "")
    (:unspecific . "")))

(defconstant *standard-directory-key-map*
  `((:wild-inferiors . "**") ,@*standard-component-key-map*))

(defmethod PATHNAME-KEY-MAP ((pathname PATHNAME) component-name)
  (case component-name
    (:directory *standard-directory-key-map*)
    (t *standard-component-key-map*)))

(defmethod PATHNAME-HOST-NAMESTRING ((pathname PATHNAME) &optional errorp)
  (pathname-component-string
   pathname (pathname-local-component pathname :host) :host errorp))

(defmethod PATHNAME-FILE-NAMESTRING ((pathname PATHNAME) &optional errorp)
  (let* ((*print-circle* nil)
	 (name (pathname-component-string
		pathname (pathname-local-component pathname :name)
		:host errorp))
	 (type (pathname-component-string
		pathname (pathname-local-component pathname :type)
		:type errorp))
	 (version (pathname-component-string
		   pathname
		   (pathname-local-component pathname :version)
		   :version errorp))
	 (typep (not (equal type "")))
	 (versionp (not (equal version ""))))
    (if (and name type version
	     (or typep (null versionp)))
	(with-output-to-string (s)
	  (princ name s)
	  (when typep (write-char #\. s) (princ type s))
	  (when versionp (write-char #\. s) (princ version s)))
	(namestring-error pathname errorp))))

(defmethod PATHNAME-NAMESTRING-FORMAT-CONTROL ((pathname PATHNAME))
  (formatter "~@[~a:~]~@[~a~]~@[~a~]~@[~a~]"))

(defmethod PATHNAME-NAMESTRING ((pathname PATHNAME) &optional errorp)
  (let* ((*print-circle* nil)
	 (host (pathname-host-namestring pathname errorp))
	 (device (pathname-component-string
		  pathname (pathname-local-component pathname :device)
		  :device errorp))
	 (directory (pathname-directory-namestring pathname errorp))
	 (file (pathname-file-namestring pathname errorp)))
      (when (and host device directory file)
	(format nil (pathname-namestring-format-control pathname)
		(unless (string= host "") host)
		(unless (string= device "") device)
		(unless (string= directory "") directory)
		(unless (string= file "") file)))))


;;; PATHNAME-COMMON-CASE is used by MAKE-PATHNAME to convert :case
;;; :local components to :case :common. 

(defmethod PATHNAME-COMMON-CASE ((pathname PATHNAME)
				 (component STRING)
				 component-name)
  (let ((*readtable* (pathname-readtable pathname component-name)))
    (read-from-string component)))

(defmethod PATHNAME-COMMON-CASE ((pathname PATHNAME) (component CONS) name)
  (mapcar #'(lambda (comp)
	      (pathname-common-case pathname comp name))
	  component))

(defmethod PATHNAME-COMMON-CASE ((pathname PATHNAME) component name)
  (declare (ignore name))
  component)

;;; PARSING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod DOT-ALLOWED-P ((pathname PATHNAME) component-name)
  (not (eql (get-macro-character #\. (pathname-readtable
				      pathname component-name))
	    #'illegal-reader)))

;;; PATHNAME PARSING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; pathname-parse-host, pathname-parse-device, etc., each read their
;;; own section, returning the canonicalized value (or nil) and the
;;; index marking the end of the SUCCESSFULLY parsed section of
;;; string.  (An error is signalled if junk-allowed is nil.) This may
;;; mean that more of the string was read but could not be parsed
;;; correctly for that particular component.  The next
;;; pathname-parse-xxx function will RE-READ any part of the string
;;; which was not successfully parsed.
;;;
;;; It is tempting to organize this so that the unsucessfully parsed
;;; tokens can be passed on to the next function so that they do not
;;; need to be re-read.  However, the new section might use a
;;; completely different readtable, so we really do have to re-read.

(defmethod PATHNAME-PARSE-HOST ((pathname PATHNAME) namestring
				start end junk-allowed)
  (with-pathname-parsing (s pathname :host namestring
			    start end junk-allowed)
			 (terminated-reader s #\:)))

(defmethod PATHNAME-PARSE-DEVICE ((pathname PATHNAME) namestring
				  start end junk-allowed)
  (declare (ignore namestring end junk-allowed))
  (values (pathname-default-device pathname) start))

;;; There is no default method for PATHNAME-PARSE-DIRECTORY

(defmethod PATHNAME-PARSE-NAME ((pathname PATHNAME) namestring
				start end junk-allowed)
  (with-pathname-parsing (s pathname :name namestring
			    start end junk-allowed)
			 (read s nil nil)))

;;; These two parse backwards, returning new end instead of new start
;;; as second value.

(defmethod PATHNAME-PARSE-TYPE ((pathname PATHNAME) namestring
				start end junk-allowed)  
  (let* ((dot (position #\. namestring :start start :end end
			:from-end t))
	 (next (when dot (1+ dot))))
    (cond ((and next (= next (or end (length namestring)))
		(dot-allowed-p pathname :type))
	   (values nil end dot))
	  ((and dot 
		(not (and (eql dot start)
			  (dot-allowed-p pathname :name))))
	   (with-pathname-parsing (s pathname :type namestring
				     (1+ dot) end junk-allowed t)
				  (read s nil nil)))
	  (t (values nil end end)))))

(defparameter *legal-versions* `(:unspecific :backup ,@*mnemonic-versions*))

;;; This could be extended to produce "tree-structured" versions.
;;; Just check (dot-allowed-p pathname :version) before recursing.
(defmethod PATHNAME-PARSE-VERSION ((pathname PATHNAME) namestring
				   start end junk-allowed)
  (let* ((dot (position #\. namestring :start start :end end
			:from-end t))
	 (version-dot (when dot
			(position #\. namestring :start start
				  :end dot))))
    (if (and version-dot
	     (or (> (- dot version-dot) 1)
		 (dot-allowed-p pathname :type)))
	(with-pathname-parsing (s pathname :version namestring
				  (1+ dot) end junk-allowed t)
            (let ((token (read s nil nil)))
	      (cond ((symbolp token)
		     (find token *legal-versions*			      
			   :test #'string-equal))
		    ((fixnump token)
		     (case token
		       (0 :newest)
		       (-1 :previous)
		       (-2 :oldest)
		       (t token))))))
	(values nil end end))))
  
(defmethod PATHNAME-PARSE-NAMESTRING ((pathname PATHNAME)
				      namestring dhost start end
				      junk-allowed)
  (setq end (or end (length namestring)))
  (multiple-value-bind (HOST START)
      (pathname-parse-host pathname namestring start end t)
    (multiple-value-bind (DEVICE START)
	(pathname-parse-device pathname namestring start end t)
      (multiple-value-bind (DIRECTORY START)
	  (pathname-parse-directory pathname namestring start end t)

	(multiple-value-bind (VERSION END TYPE-END)
	    (pathname-parse-version pathname namestring start end t)
	  
	  (multiple-value-bind (TYPE TYPE-TERMINAL NAME-END)
	      (pathname-parse-type pathname namestring
				   start type-end t)
	    (when (< type-terminal type-end)
	      (setq version nil end type-terminal))

	    (multiple-value-bind (NAME NAME-TERMINAL)
		(pathname-parse-name pathname namestring
				     start name-end junk-allowed)
	      (when (< name-terminal name-end)
		(setq version nil type nil end name-terminal))
	      
	      (values (make-instance (class-of pathname)
				     :host (or host dhost)
				     :device device ; may be nil!
				     :directory directory
				     :name name :type type :version version)
		      end))))))))
		  

;;; PREDICATES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Indicates if two items to be considered equal when considered as a
;;; component of a particular kind pathname.
(defmethod PATHNAME-ITEM-EQUAL ((pathname PATHNAME) (component-name t)
				item1 item2)
  ;; Includes structured components.
  (equal item1 item2))

(defmethod pathname-item-match ((pathname PATHNAME) component-name item1 item2)
  (when (or (member item2 '(nil :wild :wild-inferiors))
	    (pathname-item-equal pathname component-name item1 item2)
	    (and (typep item1 'sequence) (typep item2 'sequence)
		 (regexp item1 item2 0 0 (length item1) (length item2))))
    t))

;;; If PATHNAME-MATCH or PATHNAME-COMPONENTS-EQUAL (or EQUAL) are
;;; specialized as a whole, rather than specializing
;;; PATHNAME-ITEM-EQUAL and PATHNAME-ITEM-MATCH, then
;;; merging/unmerging may not work correctly
;;; and may also need to be specialized.  In addition, when no :device
;;; is explicitly given to MAKE-PATHNAME, PATHNAME-ITEM-EQUAL is used
;;; to determine if the host matches the default so that the default's
;;; device may be used.

;;; True if all componnts match pathname. Used to implement
;;; EQUAL, and to let us know that an existing pathname can be used
;;; instead of creating a new one.
(defmethod PATHNAME-COMPONENTS-EQUAL ((path pathname)
				      host device directory
				      name type version)
  (with-slots ((phost :host) (pdevice :device) (pdirectory :directory)
	       (pname :name) (ptype :type) (pversion :version)) path
     (and (pathname-item-equal path :host host phost)
	  (pathname-item-equal path :device device pdevice)
	  (pathname-item-equal path :directory directory pdirectory)
	  (pathname-item-equal path :name name pname)
	  (pathname-item-equal path :type type ptype)
	  (pathname-item-equal path :version version pversion))))

;;; Compare mnemonic versions (such as :newest) using the file system
;;; if necessary.  If p2 is wild, then it must have matched p1 in all
;;; other components, so we use p1 to resolve-version. If p1 is wild
;;; and we need it, we give up. 

(defmethod PATHNAME-MATCH ((p1 PATHNAME) (p2 PATHNAME)
			   &optional (versionp t))
  (and (pathname-component-match p1 p2 :host)
       (pathname-component-match p1 p2 :device)
       (pathname-component-match p1 p2 :directory)
       (pathname-component-match p1 p2 :name)
       (pathname-component-match p1 p2 :type)
       ;; Make sure this is last, because it can be expensive.
       (or (null versionp)
	   (let ((v1 (pathname-component p1 :version))
		 (v2 (pathname-component p2 :version)))
	     (or (pathname-item-match p1 :version v1 v2)
		 (let* ((fv1 (fixed-version-p v1))
			(fv2 (fixed-version-p v2))
			(resolved1 (cond (fv1 (unless fv2 p1))
					 ((wild-pathname-p p1) nil)
					 (t (resolve-version p1))))
			(resolved2 (and resolved1
					(cond (fv2 p2)
					      ((wild-pathname-p p2)
					       (resolve-version
						(make-pathname :version v2
							       :defaults p1)))
					      (t (resolve-version p2))))))
		   (and resolved1 resolved2
			(pathname-component-match resolved1 resolved2
						  :version))))))))


(defmethod EQUAL ((p1 PATHNAME) (p2 PATHNAME))
  (with-slots ((phost :host) (pdevice :device) (pdirectory :directory)
	       (pname :name) (ptype :type) (pversion :version)) p1
    (when (pathname-components-equal p1 phost pdevice pdirectory
				     pname ptype pversion)
      t)))

(defmethod EQUALP ((p1 PATHNAME) (p2 PATHNAME)) (equal p1 p2))

(defmethod EQUAL-HASH ((pathname PATHNAME))
  (with-slots (hash-code) pathname
    (or hash-code
	(setf hash-code
	      (merge-hash-codes
	       (pathname-component pathname :host)
	       (merge-hash-codes
		(pathname-component pathname :device)
		(merge-hash-codes
		 (pathname-component pathname :directory)
		 (merge-hash-codes
		  (pathname-component pathname :name)
		  (merge-hash-codes
		   (pathname-component pathname :type)
		   (pathname-component pathname :version))))))))))

(defmethod PATHNAME-WILD-P ((pathname PATHNAME) (component-name NULL))
  (or (pathname-wild-p pathname :host)
      (pathname-wild-p pathname :device)
      (pathname-wild-p pathname :directory)
      (pathname-wild-p pathname :name)
      (pathname-wild-p pathname :type)
      (pathname-wild-p pathname :version)))

(defmethod PATHNAME-WILD-P ((pathname PATHNAME) (component-name SYMBOL))
  (wildcardp (pathname-component pathname component-name)))

;;; TRANSLATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod PATHNAME-TRANSLATE ((source PATHNAME)
			       (from-wildname PATHNAME)
			       (to-wildname PATHNAME))
  (let ((host (translate-component source from-wildname to-wildname :host))
	(device (translate-component source from-wildname to-wildname :device))
	(directory (translate-component source from-wildname to-wildname :directory))
	(name (translate-component source from-wildname to-wildname :name))
	(type (translate-component source from-wildname to-wildname :type))
	(version (translate-component source from-wildname to-wildname :version)))
    (if (pathname-components-equal to-wildname host device directory
				   name type version)
	to-wildname
	(make-pathname
	 :case :common
	 :host host
	 :device device
	 :directory directory
	 :name name
	 :type type
	 :version version))))

;;; MERGING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod PATHNAME-MERGE ((path PATHNAME) (def PATHNAME) default-version)
  (with-slots ((phost :host) (pdevice :device) (pdirectory :directory)
	       (pname :name) (ptype :type) (pversion :version)) path
    (with-slots ((dhost :host) (ddevice :device) (ddirectory :directory)
		 (dname :name) (dtype :type) (dversion :version)) def
      (let* ((host (or phost dhost))
	     (device-plist (merge-device path pdevice pdevice host
					 ddevice dhost))
	     (directory (merge-directories pdirectory ddirectory))
	     (name (or pname dname))
	     (type (or ptype dtype))
	     (version (or pversion (unless pname dversion)
			  default-version)))
	(if (pathname-components-equal
	     path host (if device-plist (second device-plist)
			   (pathname-default-device path))
	     directory name type version)
	    path
	    (apply #'make-instance
		   (host-pathname-class host (class-of path))
		   :host host
		   :directory directory
		   :name name
		   :type type
		   :version version
		   device-plist))))))

;; Used by enough-namestring
(defmethod PATHNAME-UNMERGE ((path PATHNAME) (def PATHNAME))
  (with-slots ((phost :host) (pdevice :device) (pdirectory :directory)
	       (pname :name) (ptype :type) (pversion :version)) path
    (with-slots ((ddirectory :directory)) def
      (let* ((hosts-match-p (pathname-component-equal path def :host))
	     ;; Compute default-device now, before pulling things apart.
	     (devicep (if hosts-match-p
			  (pathname-component-equal path def :device)
			  (pathname-item-equal path :device pdevice

					       (pathname-default-device path))))
	     (rname (unless (pathname-component-equal path def :name)
		      pname)))
	(make-instance (class-of path)
		       :host (unless hosts-match-p phost)
		       :device (unless devicep pdevice)
		       :directory (unmerge-directories pdirectory ddirectory
						       path)
		       :name rname
		       :type (unless (pathname-component-equal path def :type)
			       ptype)
		       :version (unless (if rname
					    (eq pversion :newest)
					    (pathname-component-equal
					     path def :version))
				  pversion))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                       HOSTS                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The pathname class for hosts are accessed with
;;; host-pathname-class.  The value is a symbol naming a subclass of
;;; pathname.  The system defines:
;;;  - the local host refers to physical pathnames of the appropriate
;;;    class. 
;;;  - :unspecific and nil refer to the local host.
;;;  - "SYS" is a logical host.
;;; Pathnames can be constructed with nil or unknown hosts using the
;;; basic pathname class.  New logical-hosts are automatically defined
;;; by (setf logical-pathname-translations). 

;;; Note that hosts is not case sensitive.
(defparameter *hosts* (make-hash-table :test 'equalp))

;;; Maps from host name (string) to a pathname class.
(defun HOST-PATHNAME-CLASS (host &optional default)
  (gethash host *hosts* default))

(defsetf HOST-PATHNAME-CLASS update-host-pathname-class)
(defun update-host-pathname-class (host new-class)
  (let ((old-class (host-pathname-class host new-class)))
    (unless (subtypep old-class new-class)
      (warn "Redefining host ~s, class ~s, to class ~s."
	    host old-class new-class))
    (setf (gethash host *hosts*) new-class)))

;;; This mixin indicates that the class of pathname can be determined
;;; by looking for a leading "HOST-NAME:" in the namestring.
(defclass standard-host-pathname () ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                 23.1.5 LOGICAL PATHNAMES                     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant *logical-version-key-map*
  `((:newest . "NEWEST")
    (:previous . "PREVIOUS")
    (:oldest . "OLDEST")
    (:backup . "BACKUP")
    ,@*standard-component-key-map*))

(defclass LOGICAL-PATHNAME (standard-host-pathname pathname) ())

(defmethod PATHNAME-KEY-MAP ((pathname LOGICAL-PATHNAME) component-name)
  (case component-name
    (:directory *standard-directory-key-map*)
    (:version *logical-version-key-map*)
    (t *standard-component-key-map*)))

(defmethod PATHNAME-DIRECTORY-NAMESTRING ((pathname LOGICAL-PATHNAME)
					  &optional errorp)
  (make-directory-namestring ";" :relative pathname errorp))

(defmethod PATHNAME-PARSE-DIRECTORY ((pathname LOGICAL-PATHNAME)
				     namestring start end
				     junk-allowed)
  (directory-reader pathname namestring start end junk-allowed
		    #\; :relative :absolute))

(let ((logical-pathname (find-type 'logical-pathname)))
  (defun LOGICAL-PATHNAME-P (x) (typep x logical-pathname)))

(defparameter *logical-hosts* (make-hash-table :test 'equalp))
    
(defun LOGICAL-PATHNAME-TRANSLATIONS (host &optional (errorp t))
  (or (gethash host *logical-hosts*)
      (and errorp (error 'type-error :datum host :expected-type 'logical-host))))

(defsetf LOGICAL-PATHNAME-TRANSLATIONS update-logical-pathname-translations)

;;; We always coerce namestrings appearing in the translations rules
;;; at the time of the setf, not when accessed later.  Note that the
;;; parsing of physical namestrings may be effected by the type of
;;; host specified in *default-pathname-defaults*.

(defun update-logical-pathname-translations (host translations)
  (unless (subtypep (host-pathname-class host 'pathname)
		    'logical-pathname)
    (setf (host-pathname-class host) 'logical-pathname))
  (setf (gethash host *logical-hosts*)
    (mapcar #'(lambda (pair)
		(destructuring-bind (from-wild to-wild) pair
		  ;; If host is left null, translate-logical-pathname
		  ;; will spin.
		  (let ((to (pathname to-wild)))
		    (unless (pathname-host to :case :common)
		      (if (eq to to-wild) ; make copy
			  (setq to (make-pathname
				    :host :unspecific
				    :version (pathname-version to)
				    :defaults to))
			  (setf (slot-value to :host) :unspecific)))
		    (list (parse-namestring from-wild host)
			  to))))
	    translations)))

(defun LOGICAL-PATHNAME (pathname)
  (let ((p (pathname pathname)))
    (if (logical-pathname-translations (pathname-component p :host)
				       nil)
	p
	(error 'type-error :datum pathname :expected-type 'logical-pathname))))

(defun TRANSLATE-LOGICAL-PATHNAME (pathname)
  (setf pathname (pathname pathname))
  (let* ((rules (logical-pathname-translations
		 (pathname-host pathname :case :common) nil))
	 (rule (assoc pathname rules
		      :test #'pathname-match-p)))
    (if rules
	(if rule
	    (translate-logical-pathname
	     (pathname-translate pathname (first rule) (second rule)))
	  (error 'file-error :pathname pathname
		 :format-control "~a does not match any translation rules."))
      pathname)))

(defun LOAD-LOGICAL-PATHNAME-TRANSLATIONS (host)
  (declare (notinline load))
  (unless (logical-pathname-translations host nil)
    (funcall (symbol-function 'load)
	     (make-pathname :case :common :host "SYS"
			    :directory '(:absolute "SITE")
			    :name host :type "HOST" :version :newest))
    (logical-pathname-translations host)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                     PHYSICAL PATHNAMES                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass PHYSICAL-PATHNAME (pathname) ())

(defconstant *extended-directory-key-map*
  `((:up . "..") (:back . "..") ,@*standard-directory-key-map*))

(defconstant *extended-version-key-map*
  `((:newest . "") (:backup . "backup") ,@*standard-component-key-map*))

(defmethod PATHNAME-KEY-MAP ((pathname PHYSICAL-PATHNAME) component-name)
  (case component-name
    (:directory *extended-directory-key-map*)
    (:version *extended-version-key-map*)
    (t *standard-component-key-map*)))

;;; Note that the output of FORMAT and anything else that uses
;;; with-output-to-string is a simple-string, so
;;; pathname-component-strings should always be simple-strings.
;;; Our os-interface code expects this.  If someone creates their own
;;; pathname class that does things differently, they will need to
;;; make sure that their os-interface code works with complex-strings.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                     UNIX PATHNAMES                           ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; IWBNI we handled rerooting ala GNU-Emacs find-file: 
;;; "/foo//bar/baz" => #p"/bar/baz"
;;; IWBNI we canonicalized "~/" and "~username/" and resolved them
;;; only when producing namestrings and truenames.

(defclass UNIX-PATHNAME (standard-host-pathname physical-pathname) ())

(defmethod PATHNAME-COMPONENT-STRING ((pathname UNIX-PATHNAME)
				      (component EXTENDED-STRING)
				      (component-name t) errorp)
  (or (compactify-string component)
      (namestring-error pathname errorp)))

(defmethod PATHNAME-DIRECTORY-NAMESTRING ((pathname UNIX-PATHNAME)
					  &optional errorp)
  (make-directory-namestring "/" :absolute pathname errorp))

(defparameter *unix-pathname-readtable* (make-readtable))
(eval-when (:load-toplevel :execute)
  (setf (readtable-parse *unix-pathname-readtable*) nil)
  (setf (readtable-case *unix-pathname-readtable*) :invert)
  (set-macro-character #\* #'wild-reader t *unix-pathname-readtable*)
  (set-macro-character #\/ #'stopper-reader nil *unix-pathname-readtable*))

(defparameter *unix-host-readtable* (copy-readtable *unix-pathname-readtable*))
(defparameter *unix-directory-readtable* (copy-readtable *unix-pathname-readtable*))

(eval-when (:load-toplevel :execute)
  (set-macro-character #\: #'stopper-reader nil *unix-host-readtable*)
  (set-macro-character #\. #'directory-dot-reader t *unix-directory-readtable*))

(defmethod PATHNAME-READTABLE ((pathname UNIX-PATHNAME) component-name)
  (case component-name
    (:host *unix-host-readtable*)
    (:directory *unix-directory-readtable*)    
    (:version *version-readtable*)
    (t *unix-pathname-readtable*)))

(defmethod PATHNAME-PARSE-DIRECTORY ((pathname UNIX-PATHNAME)
				     namestring start end
				     junk-allowed)
  (directory-reader pathname namestring start end junk-allowed
		    #\/ :absolute :relative))

;;; IWBNI if we did GNU-UNIX.  weird version namestrings:
;;; "foo.c~1~" => #P"foo.c.1"
;;; "foo.c~" => #S(pathname host "GNU" name "foo" type "c"
;;;                version :previous)
;;; "#foo.c#" => #S(pathname host "GNU" name "foo" type "c"
;;;                 version :backup)
;;; "#%*mail*# => #S(pathname host "GNU" name "*mail*" type :unspecific
;;;                  version :backup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                     WINDOWS PATHNAMES                        ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass WINDOWS-PATHNAME (physical-pathname) ())

(defparameter *windows-pathname-readtable* (copy-readtable *unix-pathname-readtable*))
(defparameter *windows-host-readtable* (copy-readtable *unix-pathname-readtable*))
(defparameter *windows-device-readtable* (copy-readtable *unix-host-readtable*))
(defparameter *windows-directory-readtable* (copy-readtable *unix-pathname-readtable*))

(eval-when (:load-toplevel :execute)
  (setf (readtable-case *windows-pathname-readtable*) :upcase)
  (setf (readtable-case *windows-host-readtable*) :upcase)
  (setf (readtable-case *windows-device-readtable*) :upcase)
  (setf (readtable-case *windows-directory-readtable*) :upcase)
  (set-macro-character #\] #'stopper-reader nil *windows-host-readtable*)
  (set-macro-character #\\ #'stopper-reader nil *windows-directory-readtable*)
  (set-macro-character #\. #'directory-dot-reader t *windows-directory-readtable*))


;;; This should be removed once we get wide charcter os-interface
;;; stuff working!!!
(defmethod PATHNAME-COMPONENT-STRING ((pathname WINDOWS-PATHNAME)
				      (component EXTENDED-STRING)
				      (component-name t) errorp)
  (or (compactify-string component)
      (namestring-error pathname errorp)))

(defmethod PATHNAME-DIRECTORY-NAMESTRING ((pathname WINDOWS-PATHNAME)
					  &optional errorp)
  (make-directory-namestring "\\" :absolute pathname errorp))

(defmethod PATHNAME-NAMESTRING-FORMAT-CONTROL ((pathname WINDOWS-PATHNAME))
  (formatter "~@[[~a]~]~@[~a:~]~@[~a~]~@[~a~]"))

(defmethod PATHNAME-READTABLE ((pathname WINDOWS-PATHNAME) component-name)
  (case component-name
    (:host *windows-host-readtable*)
    (:device *windows-device-readtable*)
    (:directory *windows-directory-readtable*)
    (:version *version-readtable*)
    (t *windows-pathname-readtable*)))

(defmethod PATHNAME-PARSE-HOST ((pathname WINDOWS-PATHNAME) namestring
				start end junk-allowed)
  (with-pathname-parsing (s pathname :host namestring
			    start end junk-allowed)
			 (when (eq #\[ (peek-char t s nil nil))
			   (terminated-reader s #\]))))

(defmethod PATHNAME-PARSE-DEVICE ((pathname WINDOWS-PATHNAME) namestring
				start end junk-allowed)
  (with-pathname-parsing (s pathname :device namestring
			    start end junk-allowed)
			 (terminated-reader s #\:)))

(defmethod PATHNAME-PARSE-DIRECTORY ((pathname WINDOWS-PATHNAME)
				     namestring start end
				     junk-allowed)
  (directory-reader pathname namestring start end junk-allowed
		    #\/ :absolute :relative #\\))

;;; IWBNI we did URLs
;;; ex. "http://www.elwood.com/products/data.html"

  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                  INITIALIZATION                              ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *eclipse-path*)
(eval-when (:load-toplevel :execute)
  (setq *logical-path*
	(make-instance
	 (setf (host-pathname-class "SYS") 'logical-pathname)
	 :host "SYS"))
  (setq *nonstandard-host-syntax-classes*
	(list (finalize-inheritance (find-class 'windows-pathname))))
  (let* ((class (dolist (feature *features* 'pathname)
		  (case feature
		    (:unix (return 'unix-pathname))
		    (:windows (return 'windows-pathname)))))
	 (path (make-instance class :host :unspecific)))
    (setf (host-pathname-class (MACHINE-INSTANCE)) class
	  (host-pathname-class :UNSPECIFIC) class
	  *default-pathname-defaults* path))
  (let ((namestring (getcwd)))
    (setq *default-pathname-defaults*
	  (make-pathname :version :newest
			 :host :unspecific
			 :defaults (if namestring
				       (parse-namestring namestring :unspecific)
				       *default-pathname-defaults*))))
  ;; IWBNI we could determine what directory eclipse is running from
  ;; without resorting to environment variables.
  (setq *eclipse-path*
    (let ((home (getenv "ECLIPSEHOME")))
      (if home (parse-namestring home)
	(make-pathname :version nil
		       :defaults *default-pathname-defaults*))))
  (setf (logical-pathname-translations "SYS")
	`((,(make-instance 'logical-pathname :host "SYS" :device :unspecific
			  :directory '(:absolute "SITE" :wild-inferiors)
			  :name nil :type nil :version nil)
	   ,(merge-pathnames
	     (make-pathname :directory
			    '(:relative "SITE" :wild-inferiors)
			    :case :common :defaults *eclipse-path*)
	     *eclipse-path*))
	  (,(make-instance 'logical-pathname :host "SYS" :device :unspecific
			   :directory '(:absolute "SOURCE" :wild-inferiors)
			   :name nil :type nil :version nil)
	   ,(merge-pathnames
	     (make-pathname :directory
			    '(:relative "SRC" :wild-inferiors)
			    :case :common :defaults *eclipse-path*)
	     *eclipse-path*))))
  nil)
