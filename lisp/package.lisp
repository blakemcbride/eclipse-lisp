;;; Notes:
;;; - The local varible "package" refers to a package designator.
;;;   The local variable "pkg" refers to an actual package object.

;;; IWBNI we handled package locking, which would prohibit:
;;; - writing to any component of a locked package
;;; - writing to any component of symbol who's home package is locked.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; STRUCTURES                                                   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defstruct (PACKAGE
	    (:copier nil)
	    (:predicate packagep)
	    ;; Redundant, but it keeps us from calling MAKE-NAME
	    ;; before the package system is ready to intern anything.
	    (:conc-name package-)	
	    (:constructor
	     MAKE-PACKAGE1 (pretty-name other-names uses shadows
					internal-size external-size)))
  pretty-name
  (other-names nil :type list)
  (uses nil :type list)
  (used-by nil :type list)
  (internals (make-hash-table :test 'equal :size internal-size
			      :rehash-size 400)
	     :type hash-table)
  (externals (make-hash-table :test 'equal :size external-size
			      :rehash-size 400)
	     :type hash-table)
  (shadows nil :type list)
  (documentation nil))

(defmethod PRINT-OBJECT ((object PACKAGE) stream)
  (print-unreadable-object (object stream :type t)
    (princ (package-name object) stream)))

(defmethod documentation ((x PACKAGE) (doc-type (eql 't)))
  (package-documentation x))
(defmethod (setf documentation) (new-value (x PACKAGE) (doc-type (eql 't)))
  (setf (package-documentation x) new-value))

(defmethod make-load-form ((obj PACKAGE) &optional environment)
  (declare (ignore environment))
  `(pkg ,(package-name obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; UTILITIES                                                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *packages* (make-hash-table :test 'equal))
(defun add-pkg (name pkg) (open-address-string-sethash name *packages* pkg))
(defun remove-pkg (name) (remhash name *packages*))
(defvar *keyword-package* nil
  #+nn (add-pkg "KEYWORD" (make-package1 "KEYWORD" nil nil nil 0 1021)))

(defun FIND-PACKAGE (name)
  #+lisp-host
  (let ((pkg (if (cl:packagep name) name
		 (cl:find-package (host::string name)))))
    (when pkg (return-from find-package pkg)))
  (if (packagep name) name
      (open-address-string-gethash (string name) *packages* nil)))


;;; Return a valid package object based on a packge designator.
(defun pkg (designator)
  (or (find-package designator)
      (or (cerror "Create the package."
		  'type-error :datum designator :expected-type 'package)
	  (make-package (string designator)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; conflict resolution utilities

;;; Return a name which is NOT already in use by a package. 
(defun pkg-name (designator &aux (name (string designator)))
  (let ((pkg (find-package name)))
    (if pkg
	(restart-case (error 'package-error :package pkg
			     :format-control "~s already exists.")
	  (specify-name (new-name)
	      :report "Specify a new name to use."
	      :interactive (lambda ()
			     (declare (special *debug-io*))
			     (write-string "New package name: " *debug-io*)
			     (list (string (read *debug-io*))))
	    (pkg-name new-name))
	  (delete-package ()
	      :report "Delete existing package."
	    (delete-package pkg)
	    name))
      name)))

;;; Collect conflicting exported symbols from pkgs.
;;; Returns a list of conflicting symbols.
(defun collect-conflicts (symbol name pkgs shadows &aux conflicts)
  (dolist (pkg pkgs conflicts)
    (multiple-value-bind (sym found)
	(gethash name (package-externals pkg))
      (when (and found
		 (not (eq sym symbol))
		 (not (member sym shadows)))
	(pushnew sym conflicts)))))

;;; Ask user to pick between conflicting symbols.
;;; Returns the selected symbol.
(defun resolve-1-conflict (symbols pkg)
  (declare (special *debug-io*))
  (loop
    (cerror "Choose one of the symbols."
	    'package-error :package pkg
	    :format-control "~s: Conflict between ~s."
	    :format-arguments (list pkg symbols))
    (write-string "Symbol to shadowing-import: " *debug-io*)
    (let ((sym (read *debug-io*)))
      (if (member sym symbols)
	  (return sym)
	(format *debug-io*
		"~S is not one of the conflicting symbols." sym)))))

;;; Checks new packages-to-use for conflicts.  Arguments:
;;; NEW-PACKAGES is a list of potential new package designators.
;;; PKG is a package or name string used in error reporting.
;;; PKGS is current package-use-list.
;;; SHADOWS is current package-shadowing-symbols.
;;; Returns:
;;; - the entire set of packages to be used.
;;; - the entire set of shadowing symbols.
(defun check-uses (new-packages pkg pkgs shadows &aux syms)
  (dolist (new-package (list-arg new-packages))
    (let ((new-pkg (pkg new-package)))
      (unless (member new-pkg pkgs)
	(when pkgs
	  (let ((conflict-set nil))
	    (maphash #'(lambda (name sym)
			 (let ((conflicts (collect-conflicts
					   sym name pkgs shadows)))
			   (when conflicts
			     (push (cons sym conflicts) conflict-set))))
		     (package-externals new-pkg))
	    (when conflict-set
	      (restart-case
		  (error 'package-error :package pkg
			 :format-control
               "Using ~s in ~s causes the following sets of name conflicts: ~s."
	       :format-arguments (list new-pkg pkg conflict-set))
		(use-current-symbols ()
		    :report
		      "Prefer current symbols and shadow any new conflicts."
		  (setq syms (mapcar #'car conflict-set)))
		(choose-each ()
		    :report "Choose what to do about each symbol."
		  (dolist (conflicts conflict-set)
		    (push (resolve-1-conflict conflicts pkg) syms)))))))
	(push new-pkg pkgs))))
    (values pkgs (nreconc syms shadows)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 11.7 PACKAGE SYSTEM FUNCTIONS AND VARIABLES                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; *PACKAGE* is embedded.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PACKAGE FROBBING                                             ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun LIST-ALL-PACKAGES (&aux pkgs)
  (maphash #'(lambda (name pkg)
	       (declare (ignore name))
	       (pushnew pkg pkgs))
	    *packages*)
  pkgs)

(defun MAKE-PACKAGE (name &key nicknames (use #-lisp-host '("ECLIPSE"))
			       (#+cmu internal-size
				#-cmu (internal-size internal-size) 400)
			       (#+cmu external-size
				#-cmu (external-size external-size) 400)
		     &aux (pretty-name (pkg-name name))
			  (other-names (mapcar #'pkg-name nicknames)))
  (multiple-value-bind (uses shadows)
      (check-uses use pretty-name nil nil)
    (let ((pkg (make-package1 pretty-name other-names
			      uses shadows internal-size external-size)))
      (add-pkg pretty-name pkg)
      (dolist (name other-names) (add-pkg name pkg))
      (dolist (other uses) (push pkg (package-used-by other)))
      pkg)))


(defun PACKAGE-NAME (package)
  #+lisp-host
  (let ((pkg (pkg package)))
    (when (cl:packagep pkg) (return-from package-name (cl:package-name pkg))))
  (package-pretty-name (pkg package)))
(defun PACKAGE-NICKNAMES (package)
  #+lisp-host
  (let ((pkg (pkg package)))
    (when (cl:packagep pkg) (return-from package-nicknames (cl:package-nicknames pkg))))
  (package-other-names (pkg package)))
(defun PACKAGE-USE-LIST (package) (package-uses (pkg package)))
(defun PACKAGE-USED-BY-LIST (package) (package-used-by (pkg package)))
(defun PACKAGE-SHADOWING-SYMBOLS (package) (package-shadows (pkg package)))

(defun RENAME-PACKAGE (package new-name &optional new-nicknames)
  (let ((pkg (pkg package)))
    (remove-pkg (package-pretty-name pkg))
    (dolist (nickname (package-other-names pkg)) (remove-pkg nickname))
    (flet ((pkg-name (name) (if (eql (find-package name) pkg)
				name (pkg-name name))))
      (let ((pretty-name (pkg-name new-name))
	    (nicknames (mapcar #'pkg-name new-nicknames)))
	(add-pkg pretty-name pkg)
	(dolist (nickname nicknames) (add-pkg nickname pkg))
	(setf (package-pretty-name pkg) pretty-name)
	(setf (package-other-names pkg) nicknames)
	pkg))))

(defun USE-PACKAGE (new-packages &optional (package *package*))
  (let ((pkg (pkg package)))
    (multiple-value-bind (uses shadows)
	(check-uses new-packages pkg (package-uses pkg)
		    (package-shadows pkg))
      (dolist (other uses) (push pkg (package-used-by other)))
      (setf (package-uses pkg) uses)
      (setf (package-shadows pkg) shadows)
      t)))

(defun UNUSE-PACKAGE (packages &optional (package *package*))
  (let* ((pkg (pkg package))
	 (list (package-uses pkg))
	 (packages (list-arg packages)))
    (when (eq packages list) (setq packages (copy-list packages)))
    (dolist (other packages)
      (let ((other-pkg (pkg other)))
	(setq list (delete other-pkg list :count 1))
	(setf (package-used-by other-pkg)
	  (delete pkg (package-used-by other-pkg) :count 1))))
    (setf (package-uses pkg) list)
    t))

(defun DELETE-PACKAGE (package)
  (let ((pkg (find-package package)))
    (cond ((null pkg)
	   (cerror "Return nil."
		   'package-error :package package
		   :format-control "~s is not the name of a package.")
	   nil)
	  ((package-pretty-name pkg)
	   (let ((used-by (package-used-by pkg)))
	     (when used-by
	       (cerror "Unuse ~s in each of ~s."
		       (make-condition 'package-error :package pkg
				       :format-control "~s is used by ~s."
				       :format-arguments (list pkg used-by))
		       pkg used-by)
	       (dolist (other used-by) (unuse-package pkg other))))
	   (unuse-package (package-uses pkg) pkg)
	   (remove-pkg (package-pretty-name pkg))
	   (dolist (nickname (package-other-names pkg)) (remove-pkg nickname))
	   (setf (package-pretty-name pkg) nil)
	   (setf (package-other-names pkg) nil)
	   ;; We are also permitted, but not required, to unintern all
	   ;; the symbols which have this package as their
	   ;; home-package.  IWBNI we did this, because this makes it
	   ;; more likely that the package and symbols will get
	   ;; garbage collected.  For example, suppose an interned
	   ;; symbol is held by some non-garbage object.  GC will
	   ;; follow that symbol's home-package back to the package,
	   ;; and this, in turn, will cause call the symbols pointed
	   ;; to from the deleted package to not be GC'd.
	   t)
	  (t nil))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SYMBOL FROBBING UTILITIES                                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun make-new-internal (name pkg)
  (let ((sym (make-symbol name)))
    (symbol-package-setter pkg sym)
    (open-address-string-sethash
     name
     (cond ((eq pkg *keyword-package*)
	    (set-symbol-value-value sym sym)
	    (package-externals pkg))
	   (t (package-internals pkg)))
     sym)))

(defun unhome-symbol (symbol pkg)
  (when (eq pkg (symbol-package symbol))
    (symbol-package-setter nil symbol)))
(defun newhome-symbol (symbol pkg)
  (unless (symbol-package symbol)
    (symbol-package-setter pkg symbol)))

(defun find-present-symbol (name pkg)
  (let ((sym (open-address-string-gethash
	      name (package-internals pkg) not-found)))
    (if (eq sym not-found)
	(let ((sym (open-address-string-gethash
		    name (package-externals pkg) not-found)))
	  (if (eq sym not-found)
	      (values nil nil)
	      (values sym :external)))
	(values sym :internal))))

(defun find-inherited-symbol (string pkg)
  (dolist (pkg (package-uses pkg) (values nil nil))
    (multiple-value-bind (sym found)
	(gethash string (package-externals pkg))
      (when found (return (values sym :inherited))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SYMBOL FROBBING                                              ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun FIND-SYMBOL (name &optional (package *package*))
  (let ((pkg (pkg package))
	(name (string name)))
    #+lisp-host
    (when (cl:packagep pkg)
      (return-from find-symbol (cl:find-symbol (host::string name) pkg)))
    (multiple-value-bind (sym where) (find-present-symbol name pkg)
      (if where (values sym where)
	(find-inherited-symbol name pkg)))))

(defun INTERN (string &optional (package *package*))
  (let ((pkg (pkg package)))
    #+lisp-host
    (when (cl:packagep pkg)
      (return-from intern (cl:intern (host::string string) pkg)))
    (multiple-value-bind (sym found) (find-symbol string pkg)
      (if found
	  (values sym found)
	(values (make-new-internal string pkg) nil)))))
    
(defun SHADOW (symbols &optional (package *package*))
  (let ((pkg (pkg package)))
    (dolist (sym (list-arg symbols) t)
      (let ((name (string sym)))
	(multiple-value-bind (s w) (find-present-symbol name pkg)
	  (pushnew (if w s (make-new-internal name pkg))
		   (package-shadows pkg)))))))

(defun SHADOWING-IMPORT (symbols &optional (package *package*))
  (let* ((pkg (pkg package))
	 (internals (package-internals pkg))
	 (externals (package-externals pkg))
	 (shadows (package-shadows pkg)))
    (dolist (sym (list-arg symbols) t)
      (let ((name (symbol-name sym)))
	(multiple-value-bind (s w) (find-present-symbol name pkg)
	  (unless (and w (eq sym s))
	    (when w		;unintern existing symbol
	      (unhome-symbol s pkg)
	      (setf (package-shadows pkg)
		(delete s shadows :count 1))
	      (when (eq w :external)
		(remhash name externals)))
	    (setf (gethash name internals) sym)
	    (newhome-symbol sym pkg)))
	(pushnew sym (package-shadows pkg))))))

(defun UNEXPORT (symbols &optional (package *package*))
  (let ((pkg (pkg package)))
    (dolist (sym (list-arg symbols) t)
      (let ((name (symbol-name sym)))
	(multiple-value-bind (s w) (find-symbol name pkg)
	  (cond ((or (null w) (not (eq s sym)))
		 (error 'inaccessible-symbol-error
			:package pkg :symbol sym))
		((eq w :external)
		 (remhash name (package-externals pkg))
		 (setf (gethash name (package-internals pkg)) sym))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SYMBOL FROBBING with CONFLICT CHECKING                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun UNINTERN (symbol &optional (package *package*))
  (let* ((name (symbol-name symbol))
	 (pkg (pkg package))
	 (shadows (package-shadows pkg)))
    (or (when (member symbol shadows)
	  (let ((conflicts (collect-conflicts
			    symbol name (package-uses pkg) nil)))
	    (cond ((cdr conflicts) ; > 1 matching distinct symbol
		   (shadowing-import (resolve-1-conflict conflicts pkg)))
		  (t (setf (package-shadows pkg)
		       (delete symbol shadows :count 1))
		     nil))))
	(multiple-value-bind (s w) (find-present-symbol name pkg)
	  (when (eq s symbol)
	    (unhome-symbol symbol pkg)
	    (case w
	      (:internal (remhash name (package-internals pkg)))
	      (:external (remhash name (package-externals pkg)))))))))


(defun IMPORT (symbols &optional (package *package*))
  (let ((pkg (pkg package)) conflict-set syms shadows)
    (dolist (sym (list-arg symbols) t)
      (multiple-value-bind (s w) (find-symbol (symbol-name sym) pkg)
	(cond ((null w)
	       (let ((oops (find sym syms :test #'string=)))
		 (if oops (push (list oops sym) conflict-set)
		   (push sym syms))))
	      ((not (eq s sym)) (push (list s sym) conflict-set))
	      ((eq w :inherited) (push sym syms)))))
    (when conflict-set
      (restart-case
	  (error
	   "Import causes the following sets of name conflicts: ~s"
	   conflict-set)
	(use-current-symbols ()
	    :report "Prefer current symbols and shadow any new conflicts."
	  (setq shadows (mapcar #'car conflict-set)))
	(use-new-symbols ()
	    :report "Prefer newly imported symbols and unintern existing symbols."
	  (setq shadows (mapcar #'cadr conflict-set)))
	(choose-each ()
	    :report "Choose what to do about each symbol."
	  (dolist (conflicts conflict-set)
	    (push (resolve-1-conflict conflicts pkg) shadows)))))
    (let ((internals (package-internals pkg)))
      (dolist (sym syms)
	(setf (gethash (symbol-name sym) internals) sym)
	(newhome-symbol sym pkg)))
    (shadowing-import shadows pkg)))

(defun EXPORT (symbols &optional (package *package*))
  (let* ((pkg (pkg package))
	 (used-by (package-used-by pkg))
	 (internals (package-internals pkg))
	 (externals (package-externals pkg))
	 conflict-set syms imports)
    (dolist (sym (list-arg symbols) t)
      (let ((name (symbol-name sym)))
	(multiple-value-bind (s w) (find-symbol name pkg)
	  (declare (ignore s))
	  (unless (eq w :external)
	    (push sym syms)
	    (unless w
	      (cerror "Import the symbol."
		      'inaccessible-symbol-error :package pkg :symbol sym)
	      (pushnew sym imports))
	    (dolist (other used-by)
	      (multiple-value-bind (s w) (find-symbol name other)
		(when (and w (not (eq s sym)))
		  (push (list s sym) (getf conflict-set other)))))))))
    (when conflict-set
      (restart-case
	  (error
	   "Export causes the following sets of name conflicts: ~s"
	   conflict-set)
	(use-current-symbols ()
	    :report "Prefer current symbols and shadow any new conflicts."
	  (do ((conflict-set conflict-set (cddr conflict-set)))
	      ((null conflict-set))
	    (setf (cdr conflict-set) (mapcar #'car (cadr conflict-set)))))
	(use-new-symbols ()
	    :report "Prefer newly exported symbols and shadow existing symbols."
	  (do ((conflict-set conflict-set (cddr conflict-set)))
	      ((null conflict-set))
	    (setf (cdr conflict-set) (mapcar #'cadr (cadr conflict-set)))))
	(choose-each ()
	    :report "Choose what to do about each symbol."
	  (do ((conflict-set conflict-set (cddr conflict-set)))
	      ((null conflict-set))
	    (setf (cdr conflict-set)
	      (mapcar #'(lambda (conflicts)
			  (resolve-1-conflict conflicts (car conflict-set)))
		      (cadr conflict-set)))))))
    (import imports pkg)
    (do ((conflict-set conflict-set (cddr conflict-set)))
	((null conflict-set))
      (shadowing-import (cadr conflict-set) (car conflict-set)))
    (dolist (sym syms)
      (let ((name (symbol-name sym)))
	(remhash name internals)
	(setf (gethash name externals) sym)))
    t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SYMBOL COLLECTIONS                                           ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun FIND-ALL-SYMBOLS (name &aux syms)
  (maphash #'(lambda (pkg-name pkg)
	       (declare (ignore pkg-name))
	       (multiple-value-bind (s w)
		   (find-present-symbol name pkg)
		 (when w (pushnew s syms))))
	   *packages*)
  syms)
  
       
(macrolet
    ((def-do (name package-bindings iterator-bindings)
       `(defmacro ,name ((var &optional ,@package-bindings result-form)
			 &body body)
	  (let ((next (gensym "NEXT"))
		(more (gensym "MORE")))
	    `(with-package-iterator (,next ,@,iterator-bindings)
				    (loop (multiple-value-bind (,more ,var) (,next)
					    (unless ,more (return ,result-form))
					    ,@body)))))))
  (def-do DO-EXTERNAL-SYMBOLS ((package '*package*))
    `((pkg ,package) :external))
  (def-do DO-SYMBOLS ((package '*package*))
    `((pkg ,package) :external :internal :inherited))
  (def-do DO-ALL-SYMBOLS () '((list-all-packages) :external :internal)))


;;; There's no conforming, portable way to have a symbol accessible in
;;; one registered package which is not present in some registered
;;; package.  Therefore, do-all-symbols only need process present symbols.
			       
   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DEFPACKAGE                                                   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun safe-intern (name pkg)
  (multiple-value-bind (s w) (find-symbol name pkg)
    (cond (w s)
	  (t (cerror "Intern the symbol."
		     'inaccessible-symbol-error
		     :package pkg :symbol name)
	     (intern name pkg)))))

(defmacro DEFPACKAGE (name &rest options)
  (let ((pkg (gensym "PKG"))
	(sym (gensym "SYM"))
	documentation size nicknames use
	intern export shadow shadowing-import import)
    ;; PARSE OPTIONS
    (flet ((add-data (new old) (nreconc (mapcar #'string new) old)))
      (macrolet ((strings (name) `(setq ,name (add-data data ,name)))
		 (string-set (name)
			     `(let ((pkg (string (car data))))
				(setf (getf ,name pkg)
				      (add-data (cdr data) (getf ,name pkg)))))
		 (single (name val)
			 `(if ,name
			      (multiple-appearance-error ',name 'DEFPACKAGE)
			      (setq ,name ,val))))
	(dolist (option options)
	  (destructuring-bind (key . data) option
	    (ecase key
	      (:nicknames (strings nicknames))
	      (:use (strings use))
	      (:intern (strings intern))
	      (:export (strings export))
	      (:shadow (strings shadow))
	      (:shadowing-import-from (string-set shadowing-import))
	      (:import-from (string-set import))
	      (:documentation (single documentation (string (car data))))
	      (:size (single size (car data))))))))

    ;; CHECK CONSISTENCY
    (setq name (string name))
    (unless (assoc :use options) (setq use "CL"))
    (flet ((disjoint (s1 s2 name1 name2
			 &aux (conflict (intersection s1 s2 :test #'string=)))
		     (when conflict
		       (signal-program-error
			"Names ~s are present in both :~a and :~a options."
			conflict name1 name2))))
      (disjoint export intern :export :intern)
      (disjoint shadow intern :shadow :intern)
      (do ((data import (cddr data)) symbols)
	  ((endp data))
	(disjoint shadow (setq symbols (cadr data)) :shadow :import-from)
	(disjoint intern symbols :intern :import-from)
	(do ((data shadowing-import (cddr data)))
	    ((endp data))
	  (disjoint (cadr data) symbols :shadowing-import-from :import-from)))
      (do ((data shadowing-import (cddr data)) symbols)
	  ((endp data))
	(disjoint shadow (setq symbols (cadr data)) :shadow :shadowing-import-from)
	(disjoint intern symbols :intern :shadowing-import-from)))

    ;; GENERATE BODY
    (flet ((nullless-list (list op &optional (intern-op 'intern) (other-pkg pkg))
			 (if (find "NIL" list :test #'string=)
			     `(',(remove "NIL" list :test #'string=)
			       (,op (list (,intern-op "NIL" ,other-pkg)) ,pkg))
			     `(',list))))
      (macrolet ((import-set (op-name)
			     `(do ((forms nil) (data ,op-name))
				  ((endp data) forms)
				(let ((other-pkg (pop data)) (syms (pop data)))
				  (push `(dolist (,sym ,@(nullless-list syms ',op-name
									'safe-intern other-pkg))
					   (,',op-name (safe-intern ,sym ,other-pkg) ,pkg))
					forms))))
		 (op (list form) `(when ,list `(,,form))))
	`(eval-when (:compile-toplevel :load-toplevel :execute)
	   (let ((,pkg (find-package ,name)))
	     (if ,pkg (RENAME-PACKAGE ,pkg ,name ',nicknames)
		 (setq ,pkg (MAKE-PACKAGE ,name :nicknames ',nicknames
					  :use nil
					  #+cmu :internal-size
					  #-cmu 'internal-size
					  ,(or size
					       (max 211
						    (+ (length shadow)
						       (length intern))))
					  #+cmu :external-size
					  #-cmu 'external-size
					  ,(length export))))
	     ,@(op documentation `(setf (package-documentation x) ,documentation))
	     ,@(op shadow `(SHADOW ',shadow ,pkg))
	     ,@(import-set SHADOWING-IMPORT)
	     ,@(op use `(USE-PACKAGE ',use ,pkg))
	     ,@(import-set IMPORT)
	     ,@(op intern `(INTERN ',intern ,pkg))
	     ,@(op export `(dolist (,sym ,@(nullless-list export 'export))
			     (EXPORT (intern ,sym ,pkg) ,pkg)))
	     ,pkg))))))
       

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 11.8 MODULES                                                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *MODULES* nil)

(defun PROVIDE (module-name)
  (pushnew (string module-name) *modules* :test #'string=))

;; Return value is not specified by ANSI.  We return the name of the
;; file(s) loaded, or nil.
(defun REQUIRE (module-name &optional pathname-list
			    &aux (name (string module-name)))
  (declare (notinline load))
  (flet ((try (&rest keys)
	      (funcall (symbol-function 'load)
		       (apply #'make-pathname :name name :case :common keys)
		       :if-does-not-exist nil)))
    (cond ((member name *modules* :test #'string=) nil)
	  (pathname-list
	   (let ((loadf (symbol-function 'load)))
	     (dolist (path (list-arg pathname-list) pathname-list)
	       (funcall loadf path))))
	  ((try))
	  ((try :host "SYS" :directory "SITE"))
	  ((try :host "SYS" :directory "SOURCE"))
	  (t (error 'file-error :pathname name
		    :format-control "does not specify a pathname to load")))))
