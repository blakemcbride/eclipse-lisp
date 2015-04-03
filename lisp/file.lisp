;;; Truenames are expected to resolve the following ambiguities:
;;; - Logical pathnames are translated to physical.
;;; - Symbolic links in the file system are resolved to actual paths.
;;; - :up directory components are resolved using the actual file system. 
;;; - Mnemonic versions are resolved to actual versions.
;;; - No nil or wild components remain, though :unspecific is ok.

;;; Many of the functions here should be generic functions, so that
;;; they can be specialized by users for different pathname classes!

;;; !!! We do not currently resolve ~/ or ~username/ into an actual path.
;;; One possibility would be to PARSE ~/ into an actual pathname using
;;; user-homedir-pathname.  However: 1. We can't portably parse
;;; ~username/, and 2. Maybe such references to home directories
;;; should be cannonicalized so that when merged to a pathname on a
;;; different host, the directory changes.

(defun file-does-not-exist (pathname)
  (error 'file-not-found-error :pathname pathname))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; OPERATING SYSTEM NAMESTRINGS
;;;
;;; Different than namestrings used within lisp because it must
;;; exactly match how namestring are represented in the file system: 
;;; 1. Usually no host.
;;; 2. Might strip escape characters.
;;; 3. Might be length limited.
;;; 4. Must be a simple-string, usually a simple-base-string.
;;; 5. Must not be wild, or have mnemonic components.

;;; Should really be a generic-function, different for different
;;; pathname classes!

(defun os-namestring (physical-pathname)
  (coerce (namestring physical-pathname)
	  'simple-base-string))

;;; Used by DIRECTORY to determine a reasonable string to match
;;; against the leading characters of an operating system dirent
;;; name.  This is for efficiency: we create fewer junk pathnames.

(defun os-enough-namestring (physical-pathname relative-to)
  ;; These bindings are probably hairier than they need to be!!! 
  (let* ((version (pathname-version physical-pathname))
	 (fixedp (fixed-version-p version))
	 (name (enough-namestring
		(if fixedp physical-pathname
		    (make-pathname :version nil :defaults physical-pathname))
		relative-to))
	 (pos0 (position-if #'(lambda (c) (find c "?*")) name))
	 (pos1 (and pos0 (plusp pos0) (1- pos0)))
	 (pos2 (if (and pos1 (char= (char name pos1) #\.))
		   pos1 pos0))
	 (pos (if (or (eq version :newest) fixedp)
		  pos2
		  (let ((pos-dot (position #\. name :from-end t)))
		    (if pos2 (min pos2 pos-dot) pos-dot)))))
    (coerce (subseq name 0 pos) 'simple-base-string)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DIRECTORY PATHNAMES
;;;
;;; A directory pathname is one with a nil or :unspecific name, type,
;;; and version. 

;;; Makes sure that ALL components are filled in - even host.
(defun make-directory-path (directory defaults
			    &optional (comp :unspecific))
  (make-pathname :directory directory
		 :host (or (pathname-host defaults :case :common)
			   :unspecific)
		 :name comp :type comp :version comp
		 :case :common :defaults defaults))

(defun non-directory-p (pathname)
  (let ((subdir (pathname-name pathname :case :common)))
    (if (member subdir '(nil :unspecific :wild))
	nil
      subdir)))

(defun ensure-directory-pathname (pathname &optional (comp :unspecific))
  (let ((subdir (non-directory-p pathname)))
    (if subdir
	(make-directory-path
	 (append (pathname-directory pathname :case :common)
		 (list subdir))
	 pathname comp)
      pathname)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; RESOLVING TRUENAMES

(defparameter *default-link-defaults*
  (make-pathname :host nil :device nil :directory nil
		 :name :unspecific :type :unspecific
		 :version :unspecific))

;;; Does not properly resolve links in directories.  We need to
;;; traverse directory list to do this!  If we did this, we could
;;; then resolve :up at the same time, rather than using the
;;; *generating-truename* kluge.

;;; Depends on file-system specfic operations: file-type, follow-link.
;;; This would be more efficient if it manipulated file-system specific
;;; namestrings directly rather than pathnames.
(defun resolve-type (pathname follow-links-p)
  (case (file-type pathname)
    (:directory (ensure-directory-pathname pathname))
    (:link
     (if follow-links-p
	 (let* ((*generating-truename* t)
		(link (merge-pathnames
		       (merge-pathnames
			(parse-namestring
			 (charp-simple-base-string
			  (follow-link pathname))
			 (pathname-host pathname :case :common))
			*default-link-defaults* :unspecific)
		       pathname :unspecific)))
	   (if (eq follow-links-p :once)
	       link
	       (resolve-type link link)))
	 pathname))
    ((nil) nil)
    (t pathname)))

(defun clean-version (path)
  (if (fixed-version-p (pathname-version path))
      path
      (make-pathname :version :unspecific
		     :defaults path)))

(defun highest-version (paths)
  (reduce #'(lambda (a b &aux (v (pathname-version b)))
	      (max a (if (numberp v) v 0)))
	  paths :initial-value 0))

;;; An :unspecific version is always assumed to be :newest, otherwise,
;;; we look at numbers.  
(defun resolve-version1 (choices version)
  (let ((unspecific (find :unspecific choices
			  :key #'pathname-version)))
    (case version
      (:newest (or unspecific
		   (find (highest-version choices)
			 choices :key #'pathname-version)))
      (:oldest (or (reduce
		    #'(lambda (&optional a b)
			(when (and a b)
			  (if (< (pathname-version a)
				 (pathname-version b))
			      a b)))
		    (delete unspecific choices))
		   unspecific))
      (:previous (let* ((choices (delete unspecific choices))
			(newest (highest-version choices)))
		   (find (if unspecific newest (1- newest))
			 choices :key #'pathname-version))))))

(defun resolve-version (pathname)
  #+faster-but-not-right (clean-version pathname)
  (first (directory pathname :links nil
		    :if-does-not-exist nil)))

(let ((file-stream (find-type 'file-stream)))
  (defun probe-file1 (path errorp follow-links-p)
    (when (typep path file-stream)
      (return-from probe-file1 (stream-truename path)))
    (let* ((path (merge-pathnames path))
	   (file (translate-logical-pathname path)))
      (when (wild-pathname-p file)
	(error 'file-error :pathname file :format-control "~a is wild."))
      (unless (fixed-version-p (pathname-version file))
	(setq file (resolve-version file)))
      (or (and file (resolve-type file follow-links-p))
	  (and errorp (file-does-not-exist path))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                 PUBLIC FUNCTIONS                             ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; FILE INFORMATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Arguably, these two should take a links keyword to use as the third
;;; argument to probe-file1.

;;; The purpose of the call to probe-file1 is not so much to make sure
;;; that the file exists (though doing so makes us more independent of
;;; whether file-modified and file-owner return nil or signal errors
;;; in such cases), but rather to resolve logical pathnames and
;;; mnemonic version numbers.

(defun FILE-WRITE-DATE (filespec)
  (declare (special unix-to-universal-time))
  (let* ((true (probe-file1 filespec nil t))
	 (modified (when true
		     (file-modified true))))
    (when modified
      (+ modified unix-to-universal-time))))

(defun FILE-AUTHOR (filespec)
  (let ((true (probe-file1 filespec nil t)))
    (when true
      (charp-simple-base-string (file-owner true)))))

(defun PROBE-FILE (path) (probe-file1 path nil t))
(defun TRUENAME (path)  (probe-file1 path t t))

;; If filename is an open stream, this needs to side-effect the stream!!!
(defun RENAME-FILE (filename new-file)
  (let* ((file (merge-pathnames filename))
	 (new (merge-pathnames new-file file))
	 (true1 (truename file))
	 (translated2 (translate-logical-pathname new))
	 (name2 (or (probe-file translated2)
		    (clean-version translated2))))
    (unless (file-rename true1 name2)
      (error 'file-error :pathname name2
	     :format-control "~a cannot be renamed to ~a."
	     :format-arguments (list true1 name2)))
    (values new true1 (truename name2))))

;;; IWBNI this renamed filename to an invisible file on those systems
;;; (UNIX) which don't have reversible delete.
(defun DELETE-FILE (filename) (expunge-file filename))

;;; If delete-file renames file, then this needs to also delete the renamed
;;; file.
(defun expunge-file (filename)
  (let ((true1 (truename (merge-pathnames filename))))    
    (or (if (non-directory-p true1)
	    (file-delete true1)
	    (dir-delete true1))
	(error 'file-error :pathname true1 :format-control "~a cannot be deleted."))))
			   
(defun ENSURE-DIRECTORIES-EXIST (pathspec &key verbose)
  (let* ((fullpath (ensure-directory-pathname
		    (translate-logical-pathname
		     (merge-pathnames pathspec))))
	 (dirs (pathname-directory fullpath :case :common))
	 (created nil))
    (loop for i from 1 to (length dirs)
	  do
	  (let ((path (make-directory-path (subseq dirs 0 i)
					   fullpath)))
	    (unless (probe-file path)
	      (when verbose
		(format *standard-output* "~&Creating ~s.~%" path))
	      (setq created (make-dir path)))))
    (values fullpath created)))

(defun extend-directory (path sub pos wildcard)
  (let ((so-far (pathname-directory sub :case :common))
	(directory (pathname-directory path :case :common)))
    (when (> (length so-far) pos)
      (unless (eq wildcard :wild-inferiors) (incf pos))
      (make-pathname :directory (append so-far
					(subseq directory pos))      
		     :case :common :defaults path))))

(defun remove-wild (path)
  (make-pathname :directory (remove :wild-inferiors
				    (pathname-directory path :case :common)
				    :count 1)
		 :case :common :defaults path))

(defun DIRECTORY (pathspec &rest keys &key (links t) (if-does-not-exist :error))
  (let* ((path (merge-pathnames pathspec))
	 (file (translate-logical-pathname path))
	 (dir (pathname-directory file :case :common))
	 (host (pathname-host file :case :common))
	 (wild-pos (position-if #'wildcardp dir)))
    (if wild-pos
	(let* ((wildcard (nth wild-pos dir))
	       (complex (unless (keywordp wildcard)
			  (make-directory-path
			   (subseq dir 0 (1+ wild-pos)) file :wild))))
	  (nconc (when (eq wildcard :wild-inferiors)
		   (apply #'directory (remove-wild file)
			  :if-does-not-exist nil keys)) 
		 (mapcan
		  #'(lambda (sub)
		      (when (or (null complex)
				(pathname-match-p sub complex))
			(let ((extension (extend-directory 
					  file sub wild-pos wildcard)))
			  (when extension
			    (apply #'directory extension
				   :if-does-not-exist nil keys)))))
		  (apply #'directory
			 (make-directory-path
			  (subseq dir 0 wild-pos) file :wild)
			 keys))))
	(let ((dirpath (make-directory-path dir file)))
	  (unless (eq if-does-not-exist :error)
	    (unless (probe-file dirpath)
	      (ecase if-does-not-exist
		((nil) (return-from directory nil))
		(:create (ensure-directories-exist dirpath))
		(:error))))
	  (let ((d (open-dir dirpath))
		(version (pathname-version file))
		(stripped-name (os-enough-namestring file dirpath)))
	    (when (test d)
	      (unwind-protect
		  (do (ns paths)
		      ((null (setq ns (charp-simple-base-string
				       (read-dir d stripped-name))))
		       (let* ((pathnames 
			       (case version
				 ((nil :wild) paths)
				 ((:newest :oldest :previous)
				  (let (results)
				    (flet ((match (a b) (pathname-match a b nil)))
				      (dolist (path paths results)
					(unless (find path results :test #'match)
					  (let ((resolved (resolve-version1
							   (remove-if
							    #'(lambda (p) (not (match p path)))
							    paths)
							   version)))
					    (when resolved
					      (push resolved results))))))))
				 (t (delete version paths :test-not #'eql
					    :key #'pathname-version)))))
			 (do ((sub pathnames (cdr sub)))
			     ((null sub) pathnames)
			   (setf (car sub)
				 (let ((p (car sub)))
				   (or (resolve-type p links)
				       (warn "Could not resolve truename of ~s."
					     p)
				       p))))))
		    (let ((p (merge-pathnames
			      (parse-namestring ns host)
			      dirpath :unspecific)))
		      (when (pathname-match p path nil)
			(push p paths))))
		(close-dir d))))))))


;;; A list of entries.
;;; Each entry is an external-format identifier followed by 0 or more
;;; lists of sub-entries. 
;;; Each sub entry is an element-type specifier followed by up to
;;; three class names to be used when the direction is :input or :probe,
;;; :output, or :io, respectively.
;;; Any class name can be nil, meaning that the combination is not
;;; supported.
;;; The order of entries is not important (external-formats are tested
;;; with EQL), but subentries should be listed with the most specific
;;; element-type specifier first (element-types are tested with subtypep).
(defparameter *Supported-File-Classes*
  `((:DEFAULT
     (base-char
      ascii-file-input-stream
      ascii-file-output-stream
      ascii-file-bidirectional-stream)
     (character
      mb-file-input-stream
      mb-file-output-stream
      mb-file-bidirectional-stream)
     ((unsigned-byte 8)
      beu8-file-input-stream
      beu8-file-output-stream
      beu8-file-bidirectional-stream)
     ((unsigned-byte 16)
      beu16-file-input-stream
      beu16-file-output-stream
      beu16-file-bidirectional-stream)
     ((unsigned-byte 32)
      beu32-file-input-stream
      beu32-file-output-stream
      beu32-file-bidirectional-stream)
     ((signed-byte 8)
      bes8-file-input-stream
      bes8-file-output-stream
      bes8-file-bidirectional-stream)
     ((signed-byte 16)
      bes16-file-input-stream
      bes16-file-output-stream
      bes16-file-bidirectional-stream)
     ((signed-byte 32)
      bes32-file-input-stream
      bes32-file-output-stream
      bes32-file-bidirectional-stream))
    (:ASCII
     (base-char
      ascii-file-input-stream
      ascii-file-output-stream
      ascii-file-bidirectional-stream))
    (:UCS
     (ucs-2-char
      ucs-2-file-input-stream
      ucs-2-file-output-stream
      ucs-2-file-bidirectional-stream)
     (character				;should strictly be unicode-char
      ucs-4-file-input-stream
      ucs-4-file-output-stream
      ucs-4-file-bidirectional-stream))
    (:MULTI-BYTE
     (character				;should strictly be ucs-2-char/unicode-char
      mb-file-input-stream
      mb-file-output-stream
      mb-file-bidirectional-stream))
    (:UTF-8				;Synonym for multi-byte
     (character
      mb-file-input-stream
      mb-file-output-stream
      mb-file-bidirectional-stream))
    (:BIG-ENDIAN
     ((unsigned-byte 8)
      beu8-file-input-stream
      beu8-file-output-stream
      beu8-file-bidirectional-stream)
     ((unsigned-byte 16)
      beu16-file-input-stream
      beu16-file-output-stream
      beu16-file-bidirectional-stream)
     ((unsigned-byte 32)
      beu32-file-input-stream
      beu32-file-output-stream
      beu32-file-bidirectional-stream)
     ((signed-byte 8)
      bes8-file-input-stream
      bes8-file-output-stream
      bes8-file-bidirectional-stream)
     ((signed-byte 16)
      bes16-file-input-stream
      bes16-file-output-stream
      bes16-file-bidirectional-stream)
     ((signed-byte 32)
      bes32-file-input-stream
      bes32-file-output-stream
      bes32-file-bidirectional-stream))
    (:LITTLE-ENDIAN
     ((unsigned-byte 8)
      leu8-file-input-stream
      leu8-file-output-stream
      leu8-file-bidirectional-stream)
     ((unsigned-byte 16)
      leu16-file-input-stream
      leu16-file-output-stream
      leu16-file-bidirectional-stream)
     ((unsigned-byte 32)
      leu32-file-input-stream
      leu32-file-output-stream
      leu32-file-bidirectional-stream)
     ((signed-byte 8)
      les8-file-input-stream
      les8-file-output-stream
      les8-file-bidirectional-stream)
     ((signed-byte 16)
      les16-file-input-stream
      les16-file-output-stream
      les16-file-bidirectional-stream)
     ((signed-byte 32)
      les32-file-input-stream
      les32-file-output-stream
      les32-file-bidirectional-stream))))
    

(defmethod file-stream-class ((path pathname) (truename pathname) direction
			      external-format element-type)
  (case element-type
    (signed-byte (setq element-type '(signed-byte 32)))
    (unsigned-byte (setq element-type '(unsigned-byte 32))))
  (loop for (format . set) in *supported-file-classes*
	when (eql external-format format)
	return (loop for (type . classes) in set
		     with safe-type = (if (eq element-type :default)
					  (caar set)
					  element-type)
		     when (subtypep safe-type type)
		     return (or (case direction
				  ((:input :probe) (first classes))
				  (:output (second classes))
				  (:io (third classes)))
				(error "~s :direction is not supported ~
                               for :external-format ~s, :element-type ~s."
				       direction external-format element-type))
		     finally (error "~s is not a supported ~
                     :element-type (one of: ~s) for :external-format ~s."
				    element-type (mapcar #'first set)
				    external-format))
	finally (error "~s is not a supported :external-format."
		       external-format)))

(defun OPEN (filespec
	     &key (direction :input)
	     (external-format :default)
	     (element-type *default-element-type*)
	     (if-exists :error if-exists-p)
	     (if-does-not-exist
	      (case direction
		(:input :error)
		(:probe nil)
		(t (case if-exists
		     ((:overwrite :append) :error)
		     (t :create)))))
	     &aux (path (merge-pathnames filespec))
	     (translated (translate-logical-pathname path))
	     (truename (probe-file translated))
	     args)
  (macrolet ((add-opt (name value)
		      `(setq args `(,,name ,,value ,@args))))
    (if truename			;file exists
	(case direction
	  ((:output :io)
	   (flet ((rename (new-version)
			  (add-opt :renamed
				   (rename-file truename
						(make-pathname :version new-version
							       :defaults truename)))))
	     (ecase (cond (if-exists-p if-exists)
			  ((eq (pathname-version path) :newest) :new-version)
			  (t :error))
	       (:new-version
		(let ((version (pathname-version truename)))
		  (cond ((eq version :unspecific)
			 (rename (1+ (highest-version
				      (directory (make-pathname
						  :version :wild
						  :defaults truename))))))
			((fixed-version-p (pathname-version
					   translated))
			 (error 'file-error :pathname path
				:format-control
				"Tree structured versions not available for ~a."))
			(t (setq truename
				 (make-pathname :version (1+ version)
						:defaults truename))))))
	       (:rename (rename :backup))
	       (:rename-and-delete
		(rename :backup) (add-opt :cleanup :delete))
	       (:supersede
		(rename :backup) (add-opt :cleanup :expunge))
	       ((:overwrite :append) nil)
	       ((nil) (return-from open nil))
	       (:error (error 'file-error :pathname path
			      :format-control "~a already exists."))))))
	(ecase if-does-not-exist
	  (:create (setq translated (clean-version translated)))
	  ((nil) (return-from open nil))
	  (:error (file-does-not-exist path))))
    (let* ((class (file-stream-class path (or truename translated)
				     direction external-format element-type))
	   (stream (apply #'make-instance class
			 :pathname path
			 :translated-pathname translated
			 :truename truename
			 args)))
      (when (eq if-exists :append)
	(file-position stream (file-length stream)))
      (when (eql direction :probe) (close stream))
      stream)))

(defmethod shared-initialize :after ((stream file-input-stream) slots
				     &key truename translated-pathname)
  (when (initialize-slot-p stream 'input-fd slots)
    (let ((path (or truename translated-pathname)))
      (setf (slot-value stream 'input-fd)
	    (read-open path)))))

(defmethod shared-initialize :after ((stream file-output-stream) slots
				     &key truename translated-pathname)
  (when (initialize-slot-p stream 'output-fd slots)
    (let ((path (or truename translated-pathname)))
      (setf (slot-value stream 'output-fd)
	    (write-open path)))))

(defmethod shared-initialize :after ((stream buffered-file-bidirectional-stream)
				     slots &key truename translated-pathname)
  (when (or (initialize-slot-p stream 'input-fd slots)
	    (initialize-slot-p stream 'output-fd slots))
    (let ((path (or truename translated-pathname)))
      (with-slots (input-fd output-fd) stream
	(let ((fd (read-write-open path)))
	  (setf input-fd fd
		output-fd fd))))))


;;; Truename passed to make-instance is the truename BEFORE the file
;;; is opened, which might be nil if the file did not exist.  If the
;;; file is just now being created by opening, this method sets it to
;;; the correct name.
(defmethod initialize-instance :after ((stream file-stream) 
				       &key translated-pathname)
  (when translated-pathname
    (with-slots (truename) stream
      (setf truename (probe-file translated-pathname)))))



;;; LENGTH AND POSITION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; N.B.: FILE-LENGTH and FILE-POSITION are in units of :element-type,
;;; except for :multi-byte streams, which are in operating system
;;; byte units.

;;; The number of operating system bytes (as reported by
;;; file-discriptor-length) per Lisp element.
;;; NIL indicates that this factor is either unknown or non-constant.
;;; 1 might indicate that file-length/file-position works in units of
;;; operating system bytes, regardless of multi-byte or other mappings
;;; to Lisp elements.
(defmethod stream-element-size ((stream t)) nil)
(defmethod stream-element-size ((stream mb-file-stream)) 1)
(defmethod stream-element-size ((stream buffered-file-stream))
  (let ((element-type (stream-element-type stream)))
    (case element-type
      (base-char 1)
      (ucs-2-char 2)
      ((unicode-char character) 4)
      (t (when (and (consp element-type)
		    (member (car element-type) '(signed-byte
						 unsigned-byte)))
	   (/ (second element-type) 8))))))
    
;;; Returns in units of :element-type, except for :multi-byte, which
;;; uses operating system bytes.
(defmethod FILE-LENGTH ((stream file-stream))
  (let ((length (int-integer (file-descriptor-length
			      (file-stream-descriptor stream))))
	(factor (stream-element-size stream)))
    (when (and factor length)
      (/ length factor))))

;;; Computes actual object length.  For empty sequences, this is zero.
;;; Otherwise, this can only be done with output-streams using
;;; by temporarilly writing data and using get/set-file-position. 
(defun computed-file-sequence-length (stream object start end)
  (or (typecase object
	(null 0)
	(vector (when (zerop (length object))) 0))
      (let ((pos (when (output-stream-p stream) (get-file-position stream))))
	(when (and pos (set-file-position stream pos)
		   (typecase object
		     (sequence (stream-write-sequence
				stream object :start start :end end))
		     (character (stream-write-char stream object))
		     (integer (stream-write-byte stream object))))
	  (prog1 (- (get-file-position stream) pos)
	    (set-file-position stream pos))))))

;;; Assumes object is actually writable to stream.
(defmethod File-Sequence-Length ((stream FILE-STREAM) object
				 &key (start 0) end)
  (typecase object
    (sequence (- (or end (length object)) start))
    (character 1)
    (t (computed-file-sequence-length stream object start end))))

(defun FILE-STRING-LENGTH (stream object)
  (file-sequence-length stream object))

(defmethod Get-File-Position ((stream FILE-STREAM))
  (let ((size (stream-element-size stream)))
    (when size
      (/ (int-integer (file-descriptor-position (file-stream-descriptor stream)))
	 size))))

(defmethod Get-File-Position ((stream BUFFERED-FILE-INPUT-STREAM))
  (let ((os-pos (call-next-method)))
    (when os-pos
      (let ((buffer-pos (file-sequence-length
			 stream (input-buffer stream)
			 :start (input-buffer-position stream)
			 :end (input-buffer-end stream)))
	    (size (stream-element-size stream)))
	(when (and buffer-pos size)
	  (- (/ os-pos size) buffer-pos))))))

(defmethod Get-File-Position ((stream BUFFERED-FILE-OUTPUT-STREAM))
  (let ((os-pos (call-next-method)))
    (when os-pos
      (let ((buffer-pos (file-sequence-length stream (output-buffer stream)))
	    (size (stream-element-size stream)))
	(when (and buffer-pos size)
	  (+ (/ os-pos size) buffer-pos))))))
  
;;; Can't assume one element will be one byte.
(defmethod File-Sequence-Length ((stream MB-FILE-STREAM) object
			       &key (start 0) end)
  (computed-file-string-length stream object start end))
(defmethod Get-File-Position ((stream mb-input-stream))
  (when (>= (input-buffer-position stream)
	    (input-buffer-end stream))
    (call-next-method)))
(defmethod Get-File-Position :BEFORE ((stream mb-output-stream))    
  (finish-output stream))

(defmethod Set-File-Position :before ((stream buffered-file-input-stream)
				      (position t))
  (clear-input stream))

(defmethod Set-File-Position :before ((stream buffered-file-output-stream)
				      (position t))
  (finish-output stream))

(defmethod Set-File-Position ((stream file-stream) position)
  (test (file-descriptor-seek (file-stream-descriptor stream)
			      (fixnum-int
			       (case position
				 (:start 0)
				 (:end -1)
				 (t (* position (or (stream-element-size stream) 1))))))))

(defun FILE-POSITION (stream &optional (position nil position-p))
  (if position-p
      (set-file-position stream position)
      (get-file-position stream)))


;;; HOME DIRECTORIES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun parse-directory-namestring (namestring host)
  (ensure-directory-pathname (parse-namestring namestring host) nil))

(defmethod home-path (class host user)
  (declare (ignore class host user)) nil)

;;; On Unix, /etc/passwd has lines as follows:
;;;     user-id:x:x:x:x:home-directory:x
;;; This might not be valid under NIS, but NIS doesn't give us access
;;; to remote home directories.
#+not-yet  ;We don't yet support opening files on different hosts!
(defmethod home-path ((class (eql 'UNIX-PATHNAME)) host user)
  (with-open-file (f (make-pathname :host host
				    :directory '(:absolute "ETC")
				    :name "PASSWD"
				    :type :unspecific
				    :version :unspecific
				    :case :common)
		   :if-does-not-exist nil)
    (do (line)
	((null (setq line (read-line f nil nil))) nil)
      (let ((index (position #\: line)))
	(when (string= user line :end2 index)
	  (dotimes (n 4)
	    (setq index (position #\: line :start (1+ index))))
	  (return-from home-path
	    (parse-directory-namestring
	     (subseq line (incf index) (position #\: line :start index))
	     host)))))))
	  
(defun USER-HOMEDIR-PATHNAME (&optional host)
  (if (or (member host '(nil :unspecific))
	  (equalp host (machine-instance)))
      (parse-directory-namestring (or (uname (character-int #\h))
				      (getcwd))
				  :unspecific)
      #+not-yet (home-path (host-pathname-class host) host (user-id))))

