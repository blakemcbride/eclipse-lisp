;;; See also: menus.lisp

;;; UTILITIES
(defun mapkeys (function table)
  (maphash #'(lambda (key value) (declare (ignore key))
	       (funcall function key))
	   table))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 27.2 COMMAND TABLES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Our reading of the spec makes these interpretations:
;;; 1. A command can be present in a table without a command-line
;;; name, menu or gesture.  However, because
;;; add-command-to-command-table removes old command-line names,
;;; menus, and keystrokes before adding new values, it is NOT possible
;;; to have a command-line name, menu or keystroke in a table without
;;; having the command defined in that same table.  On the other hand,
;;; an inheriting table might shadow that command, without shadowing
;;; the command-line name, menu or keystroke.

;;; COMMAND TABLE ERRORS
(define-condition COMMAND-TABLE-ERROR (simple-error)
  ((table :initarg :table)))

(define-condition command-table-table-error (command-table-error)
  ())
(define-condition command-table-command-error (command-table-error)
  ((command :initarg :command)))
(defmethod initialize-instance :after ((condition
					command-table-table-error)
				       &rest initargs)
  (init-format-arguments condition initargs 'table))
(defmethod initialize-instance :after ((condition
					command-table-command-error)
				       &rest initargs)
  (init-format-arguments condition initargs 'command 'table))

(define-condition COMMAND-TABLE-NOT-FOUND (command-table-table-error)
  ((format-control :initform "Command table ~s not found.")))
(define-condition COMMAND-TABLE-ALREADY-EXISTS (command-table-table-error)
  ((format-control :initform "Command table ~s already exists.")))
(define-condition COMMAND-NOT-PRESENT (command-table-command-error)
  ((format-control :initform "Command ~s not present in table ~s.")))
(define-condition COMMAND-NOT-ACCESSIBLE (command-table-command-error)
  ((format-control :initform "Command ~s not accessible in table ~s.")))
(define-condition COMMAND-ALREADY-PRESENT (command-table-command-error)
  ((format-control :initform "Command ~s already present in table ~s.")))

;;; COMMAND TABLES
;; There's really no reason to make this a defclass.  
(defstruct (COMMAND-TABLE
	    (:constructor make-command-table-no-error
			  (name &key inherit-from inherit-menu))
	    (:copier nil))
  (name nil :type symbol :read-only-p t)
  (inherit-from nil :read-only-p t)
  (inherit-menu nil)
  ;; maps command names to (command-line-name menu gesture)
  (commands (make-hash-table :test 'eq) :read-only-p t)
  ;; All the rest map to command names.
  (command-line-names (make-hash-table :test 'equalp) :read-only-p t)
  (gestures (make-hash-table :test 'eql) :read-only-p t)
  (menus (make-hash-table :test 'equalp) :read-only-p t))

(defun update-menu (table menu inherit-menu)
  (setf (command-table-inherit-menu table) inherit-menu)
  (loop for clause in meu
	do (apply #'add-menu-item-to-command-table table clause))
  table)

(defparameter *command-tables* (make-hash-table :test 'eq))
(defun FIND-COMMAND-TABLE (name &key (errop t))
  (if (command-table-p name)
      name
      (or (gethash name *command-tables*)
	  (and errorp (error 'command-table-not-found :table name)))))

(defun MAKE-COMMAND-TABLE (name &key inherit-from menu inherit-menu
				(errorp t)) 
  (when (and errorp (gethash name *command-tables*))
    (error 'command-table-already-exists :table name))
  (update-menu (setf (gethash name *command-tables*)
		     (make-command-table-no-error
		      name :inherit-from
		      (mapcar #'find-command-table inherit-from)))
	       menu inherit-menu))

(make-command-table 'global-command-table)
(make-command-table 'user-command-table :inherit-from 'global-command-table)

(defmacro DEFINE-COMMAND-TABLE (name &key (inherit-from
					   (find-command-table
					    'global-command-table)) 
				     menu inherit-menu)
  
  `(update-menu
    (or (find-command-table ',name nil)
	(make-command-table name :inherit-from ',inherit-from))
    ',menu ',inherit-menu))
	 


;;; MAPPING COMMAND-TABLES
(defmacro do-command-table-supers ((var list &optional check) &body body)
  (let* ((queue (gensym "COMMAND-TABLES"))
	 (updater `(append ,queue (command-table-inherit-from ,var))))
    (do ((,queue ,list
		 ,(if check
		      `(if ,check ,updater ,queue)
		      ,updater))
	 (var (car ,queue) (car ,queue)))
	((null ,var) nil)
      ,@body)))

(defmacro DO-COMMAND-TABLE-INHERITANCE ((command-table-var
					 command-table)
					&body body)
  `(do-command-table-supers (,command-table-var
			     (cons ,command-table nil))
			    ,@body))

(defun MAP-OVER-COMMAND-TABLE-COMMANDS (function command-table &key
						 (inherited t))
  (mapkeys function (command-table-commands command-table))
  (when inherited
    (do-command-table-supers (table (command-table-inherit-from
				     command-table))
			     (mapkeys function
				      (command-table-commands
				       table)))))

(defun MAP-OVER-COMMAND-TABLE-NAMES (function command-table &key
					      (inherited t))
  
  (maphash function (command-table-names command-table))
  (when inherited
    (do-command-table-supers (table (command-table-inherit-from
				     command-table))
			     (maphash function
				      (command-table-names
				       table)))))
				      
			   

(defun COMMAND-PRESENT-IN-COMMAND-TABLE-P (command-name command-table)
  (gethash command-name (command-table-commands (find-command-table
						command-table))))

(defun COMMAND-ACCESSIBLE-IN-COMMAND-TABLE-P (command-name
					      command-table)
  (let ((table (find-command-table command-table)))
    (if (gethash command-name (command-table-commands table))
	table
	(loop for super in (command-table-inherit-from table)
	      when (gethash command-name (command-table-commands
					  super))
	      return super))))

;;; This provides for the case where command-line-name is in one
;;; table, but the command
(defun FIND-COMMAND-FROM-COMMAND-LINE-NAME (name command-table &key
						 (errorp t))
  (do-command-table-inheritance (table command-table)
     (let ((command (gethash command-name
			     (command-table-command-line-names table))))
       (when command
	 (return-from find-command-from-command-line
	   (values command
		   (or (command-accessible-in-command-table-p command)
		       (return nil)))))))
  (when errorp (error 'command-not-accessible
		      :command (or command name)
		      :table command-table)))

;;; Where should command line names be added?  Here we assume the
;;; first table in which the command is present.
(defun COMMAND-LINE-NAME-FOR-COMMAND (command-name command-table &key
						   (errorp t)
						   &aux first)
  (do-command-table-inheritance (table command-table)
    (let ((data (gethash command-name (command-table-commands
				       table))))
      (when data
	(let ((command-line-name (first data)))
	  (cond (command-line-name
		 (return-from command-line-name-for-command
		   command-line-name))
		((null first) (setq first table)))))))
  (case errorp
    (:create (let ((name (command-name-from-symbol command-name)))
	       (cond (first
		      (setf (car (gethash command-name
					  (command-table-commands
					   table))) name) 
		      (setf (gethash name
				     (command-table-command-names table))
			    command-name))
		     (t (add-command-to-command-table
			 command-name command-table :name name)))))
    ((t) (error 'command-not-accessible :table command-table
		:command command-name))))

(defun command-table-complete-input (command-table string action
						   &key frame)
  (error "command-table-complete-input is not implement."))


;;; ADDING AND REMOVING ENTRIES
(defun REMOVE-COMMAND-FROM-COMMAND-TABLE (command-name command-table
						       &key (errorp t))
  (let* ((table (find-command-table command-table))
	 (commands (command-table-commands table))
	 (old-data (gethash command-name commands)))
    (if old-data
	(destructuring-bind (command-line-name menu gesture)
	    old-data
	  (remhash command-line-name
		   (command-table-command-line-names table))
	  (remhash menu
		   (command-table-menus table))
	  (remhash gesture
		   (command-table-gestures table))
	  (remhash command-name commands)
	  t)
	(when errorp
	  (error 'command-not-present :table command-table
		 :command command-name)))))
  
(defun ADD-COMMAND-TO-COMMAND-TABLE (command-name command-table &key
						  name menu keystroke
						  (errorp t))
  (let* ((table (find-command-table command-table))
	 (commands (command-table-commands table)))
    (when (gethash command-name commands)
      (if errorp
	  (error 'command-already-present :table command-table
		 :command command-name)
	  (remove-command-from-command-table command-name table :errorp nil)))
    (when name
      (let ((string (if (eq name t)
			(setq name (command-name-from-symbol
				    command-name))
			name)))
	(setf (gethash name (command-table-command-line-names
			     table))
	      command-name)))
    (when menu
      (etypecase menu
	(null)
	(string (setf (gethash menu (command-table-menus table)) command-name))
	((eq t) (setf (gethash (setq menu
				     (or name
					 (command-name-from-symbol
					  command-name))) 
			       (command-table-menus table))
		      command-name))
	(cons (setq menu (apply #'add-menu-item-to-command-table table menu)))))
    (etypecase keystroke
      (null)
      (string (setf (gethash keystroke (command-table-gestures table))
		    command-name)))
    (setf (gethash command-name commands) (list name menu keystroke))
    command-name))
       

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 27.1 COMMANDS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant *UNSUPPLIED-ARGUMENTS-MARKER* (make-symbol 'unsupplied-argument))
(defun COMMAND-NAME (command) (car command))
(defun COMMAND-ARGUMENTS (command) (cdr command))
(defun PARITIAL-COMMAND-P (command)
  (not (null (member *unsupplied-arguments-marker (command-arguments command)
		     :test 'eq))))

(defun COMMAND-NAME-FROM-SYMBOL (symbol)
  (pretty-symbol-name symbol "COM-"))

(defmacro DEFINE-COMMAND (name-and-options arguments &body body)
  (let* ((command-name (bindingform-name name-and-options))
	 (command-parser (gensym command-name)))
    (destructuring-bind (&key command-table name menu keystroke)
	(bindingform-value name-and-options)
      (multiple-value-bind (command-args parser-code)
	  (convert-presentation-arguments arguments)
	#-not-yet (declare (ignore parser-code))
	`(progn
	   (defun ,command-name ,command-args ,@body)
	   #+not-yet(defun ,command-parser () ,parser-code)
	   ,(when command-table
	      `(add-command-to-command-table
		,command-name ,command-table
		:name ',name :menu ',menu :keystroke ',keystroke))
	   (setf (get ',command-name 'command-parser)
		 ',command-parser)
	   ',command-name)))))

(defun generate-parser-code (parameter keyp type-specifier &key default
				       default-type display-default
				       prompt documentation
				       (mentioned-default nil mentioned-default-p)
				       (when nil whenp)
				       (gesture nil gesturep))
  #-not-yet (declare (ignore parameter keyp type-specifier &key default
				       default-type display-default
				       prompt documentation))
  (when (and (not keyp)
	     (or mentioned-default-p whenp gestruep))
    (signal-program-error
     ":WHEN, :GESTURE and :MENTIONED-DEFAULT are only valid for keyword arguments.")))
  
(defun convert-presentation-arguments (lambda)
  (let (command-args parser keyp)
    (dolist (descrition lambda)
      (if (eq description '&key)
	  (setq keyp (push description command-args))
	  (destructuring-bind (parameter type-specifier &rest keys)
	      description
	    #-not-yet (declare (ignore type-specifier default
				       default-type display-default
				       prompt documentation))
	    (push parameter command-args)
	    (push (apply #'generate-parser-code parameter keyp
			 type-specifier keys) parser))))
    (values (nreverse command-args) parser)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 27.3 COMMAND MENUS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; menu-item: (type value . options)
;;; menu: associates menu-names to command menu-items (in a command-table)

(defun COMMAND-MENU-ITEM-TYPE (menu-item) (car menu-item))
(defun COMMAND-MENU-ITEM-VALUE (menu-item) (cadr menu-item))
(defun COMMAND-MENU-ITEM-OPTIONS (menu-item) (cddr menu-item))

(defun ADD-MENU-ITEM-TO-COMMAND-TABLE
  (command-table string type value
		 &key documentation (after :end) keystroke text-style
		 (errorp  t))
  )

(defun REMOVE-MENU-ITEM-FROM-COMMAND-TABLE (command-table string &key
							  (errorp t))
  (let ((table (find-command-table command-table)))
    (or (remhash string (command-table-menus table))
	(when errorp
	  (error 'command-not-present :table command-table
		 :command string)))))
	 
(defun FIND-MENU-ITEM (menu-name command-table &key (errorp t))
  (let ((table (find-command-table table)))
    (do-command-table-supers (table (cons table (command-table-inherit-from
						 table))
				    (command-table-inherit-menu table))
        (let ((found (gethash menu-name (command-table-menus table))))
	  (when found
	    (return-from find-menu-item
	      (values found table)))))
    (when errorp
      (error 'command-not-accessible :table command-table
	     :command-name menu-name))))

(defgeneric DISPLAY-COMMAND-TABLE-MENU
  (command-table stream &key max-width max-height n-rows n-columns
		 x-spacing y-spacing initial-spacing row-wise
		 cell-align-x cell-align-y move-cursor))

(defmethod DISPLAY-COMMAND-TABLE-MENU (command-table (stream
						      file-output-stream)
						     &key)
  (error "DISPLAY-COMMAND-TABLE-MENU is not implemented yet."))

(defun MENU-CHOOSE-COMMAND-FROM-COMMAND-TABLE (command-table &rest keys)
  (values (apply #'menu-choose <get items form command-table>
		 keys)))

  
						      