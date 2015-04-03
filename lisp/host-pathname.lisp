;;; To ec:int
(defun eclipse::integer-int (x) (the (signed-byte 32) x))

(defun get-path-ns (path)
  (eclipse::simple-base-string-charp (eclipse::os-namestring path)))

(defun eclipse::getcwd ()
  (multiple-value-bind (path problem)
      (ignore-errors (probe-file *default-pathname-defaults*))
    (when problem
      (setq path (make-pathname :name :unspecific
				:type :unspecific
				:version :unspecific
				:defaults (first (directory *default-pathname-defaults*)))))
    (namestring (truename path))))
    

(defun eclipse::file-descriptor-close (fd) (close fd))

(defun eclipse::simple-base-string-value (host-string)
  host-string)

(defun eclipse::file-modified (path)
  (let ((date (file-write-date (get-path-ns path))))
    (when date (eclipse::integer-int (- date 2208988800)))))
(defun eclipse::file-owner (path)
  (eclipse::simple-base-string-value
   (file-author (get-path-ns path))))
(defun eclipse::file-type (epath)
  (let* ((cstring (get-path-ns epath))
	 (path (pathname cstring))
	 (type (pathname-type path))
	 (version (pathname-version path)))
    (cond ((or (and type (not (eql type :unspecific)))
	       (and version (not (member version '(:unspecific :newest)))))
	   (let ((truename (probe-file path)))
	     (when truename
	       (if (equal (namestring truename) (namestring path))
		   :file
		   :link))))
	  ((or (member (pathname-name path) '(nil :unspecific))
	       (let* ((dir-string (concatenate 'cl:string cstring "/."))
		      (results (or (directory dir-string)
				   (directory
				    (setq dir-string
					  (concatenate 'cl:string cstring "/*"))))))
		 (when results
		   (equal (pathname-directory (first results))
			  (pathname-directory dir-string)))))
	   :directory)
	  (t (let ((results (directory cstring)))
	       (case (length results)
		 (0 nil)
		 (1 (let* ((truename (first results))
			   (nstr (namestring truename)))
		      (if (equal (if (find #\\ nstr)
				     (remove #\\ nstr)
				     nstr) cstring)
			  :file
			  :link)))
		 (t :directory)))))))
(defun eclipse::file-delete (path)
  (delete-file (get-path-ns path)))
(defun eclipse::dir-delete (path)
  (delete-file (get-path-ns path)))
(defun eclipse::file-rename (old new)
  (rename-file (get-path-ns old) (get-path-ns new)))
(defun eclipse::follow-link (path)
  (eclipse::simple-base-string-value
   (namestring (truename (get-path-ns path)))))

(defun eclipse::make-dir (path)
  (ensure-directories-exist (get-path-ns path)))


(let ((dirs (make-hash-table))
      (counter 0))
  (defun eclipse::open-dir (path)
    (let ((key (incf counter)))
      (setf (gethash key dirs)
	    (directory (concatenate 'cl:string
				    (get-path-ns path)
				    #+cmu "*.*.*")
		       #+cmu :follow-links #+cmu nil))
      key))
  (defun eclipse::close-dir (key)
    (remhash key dirs))
  (defun eclipse::read-dir (key name)
    (let ((entry (do* ((string (eclipse::simple-base-string-charp name))
		       (end (length string))
		       (entry (pop (gethash key dirs))
			      (pop (gethash key dirs))))
		     ((null entry) nil)
		   (let ((entry1 (if (pathname-name entry)
				     (file-namestring entry)
				     (first (last (pathname-directory entry))))))
		     (when (string= string entry1
				    :end2 (min end (length entry1)))
		       (return entry1))))))
      (if entry
	  (eclipse::simple-base-string-value entry)
	(eclipse::integer-int 0)))))

(defun eclipse::read-open (path)
  (open (get-path-ns path)))
(defun eclipse::write-open (path)
  (open (get-path-ns path)
	:direction :output :if-exists :overwrite :if-does-not-exist :create))
(defun eclipse::read-write-open (path)
  (open (get-path-ns path)
	:direction :io :if-exists :overwrite :if-does-not-exist :create))

(defun eclipse::file-descriptor-length (fd)
  (ignore-errors (eclipse::integer-int (file-length fd))))

(defun eclipse::file-descriptor-position (fd)
  (let ((p (file-position fd)))
    (eclipse::integer-int (or p -1))))

(defun eclipse::file-descriptor-seek (fd i)
  (let ((position (eclipse::int-integer i)))
    (if (file-position fd (if (= position -1)
			      :end
			      position))
	1 0)))

