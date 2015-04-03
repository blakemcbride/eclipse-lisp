;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 7.8 ITERATION                                                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun eclipse::do-body (varlist endlist body bind step block)
  (multiple-value-bind (decl code) (eclipse:find-declarations body t)
    (let* ((inits ())
	   (steps ())
	   (l1 (gensym "ITERATE")))
      ;; Parse the varlist to get inits and steps.
      (dolist (v varlist)
	(etypecase v
	  (symbol (push v inits))
	  (cons (ecase (length v)
		  (1 (push (first v) inits))
		  (2 (push v inits))
		  (3 (push (list (first v) (second v)) inits)
		     (setq steps (list* (third v) (first v) steps)))))))
      ;; And finally construct the new form.
      `(block ,BLOCK
	 (,bind ,(nreverse inits)
		(declare ,@decl)
		(tagbody
		 ,L1
		 (eclipse:when ,(car endlist)
			       (return-from ,BLOCK (progn ,@(cdr endlist))))
		 (tagbody ,@code)
		 (,step ,@(nreverse steps)) 
		 (go ,L1)))))))

(defmacro eclipse:DO (varlist endlist &body body)
  (eclipse::do-body varlist endlist body 'let 'eclipse:psetq nil))

(defmacro eclipse:DO* (varlist endlist &body body)
  (eclipse::do-body varlist endlist body 'let* 'setq nil))

;;; Some feel that a new binding for var should be created on each
;;; iteration, so as to avoid incorect declarations.  The spec says
;;; that rebinding or assignment is equally exceptable, but that the
;;; scope of the binding includes the end test, at which time the var
;;; is bound to nil.  Our assignment implementation is, therefore,
;;; correct. 
(defmacro eclipse:DOLIST ((var listform &optional resultform) &rest tagbody)
  (let ((list (gensym "SUBLIST")))
    `(eclipse:do* ((,list ,listform (cdr ,list))
	   (,var (car ,list) (car ,list)))
	 ((eclipse:endp ,list) ,resultform)
       ,@tagbody)))

(defmacro eclipse:DOTIMES ((var countform &optional resultform) &rest tagbody)
  (let ((end-number (gensym "END")))
    `(eclipse:do* ((,end-number ,countform)
	   (,var 0 (eclipse::add-integer ,var 1)))
	 ((eclipse::ge-integer ,var ,end-number) ,resultform)
       (declare (ignorable ,var) (type integer ,var))
       ,@tagbody)))


;;; 7.8.5 THE "PROGRAM FEATURE"
(defmacro eclipse:PROG (variables &rest body)
  (multiple-value-bind (decls body) (eclipse:find-declarations body t)
    `(block nil (let ,variables (declare ,@decls)
		     (tagbody ,@body)))))

(defmacro eclipse:PROG* (variables &rest body)
  (multiple-value-bind (decls body) (eclipse:find-declarations body t)
    `(block nil (let* ,variables (declare ,@decls)
		      (tagbody ,@body)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 7.10 MULTIPLE VALUES                                             ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro eclipse:MULTIPLE-VALUE-LIST (value-form)
  `(multiple-value-call #'eclipse:list ,value-form))

(defmacro eclipse:MULTIPLE-VALUE-BIND (varlist value-form &body body)
  (if (cdr varlist)
      (let ((values (gensym "VALUES")))
	`(let* ((,values (eclipse:multiple-value-list ,value-form))
	       ,@(loop for var in varlist
		       for ref = values then `(setq ,values (cdr ,values))
		       collect `(,var (car ,ref))))
	   (declare (dynamic-extent ,values))
	   ,@body))
    `(let ((,(car varlist) ,value-form))
       ,@body)))

(defmacro eclipse:MULTIPLE-VALUE-SETQ (varlist value-form)
  (if varlist
      (let ((temps (eclipse::make-symbols varlist)))
	`(eclipse:multiple-value-bind ,temps ,value-form
				      ,@(mapcar #'(lambda (var temp)
						    `(setq ,var ,temp))
						varlist temps)
				      ,(car temps)))
      `(values ,value-form)))

(eval-when (:load-toplevel)		;see notes for ;destructuring-bind!!!
  (defmacro eclipse:NTH-VALUE (n form)
    (if (constantp n)
	(let ((dummy-list nil)
	      (keeper (gensym "KEEPER")))
	  ;; We build DUMMY-LIST, a list of variables to bind to useless
	  ;; values, then we explicitly IGNORE those bindings and return
	  ;; KEEPER, the only thing we're really interested in right now.
	  (dotimes (i n) (push (gensym "IGNORE-") dummy-list))
	  `(eclipse:multiple-value-bind (,@dummy-list ,keeper)
					,form
					(declare (ignore ,@dummy-list))
					,keeper))
	`(nth ,n (multiple-value-list ,form)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 10.1 THE PROPERTY LIST                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defsetf eclipse:SYMBOL-PLIST eclipse::set-symbol-plist)

(define-setf-expander eclipse:GETF (place prop &optional default &environment env)
  (multiple-value-bind (temps values stores set get)
      (eclipse:get-setf-expansion place env)
    (let ((newval (gensym "NEWVAL"))
	  (ptemp (gensym "PTEMP"))
	  (def-temp (when default (gensym "DEF-TEMP"))))
      (values `(,@temps ,ptemp ,@(when default `(,def-temp)))
	      `(,@values ,prop ,@(when default `(,default)))
	      `(,newval)
	      `(let ((,(car stores) (eclipse::putf ,get ,ptemp ,newval)))
		 ,set
		 ,newval)
	      `(eclipse:getf ,get ,ptemp ,@(when default `(,def-temp)))))))

(eval-when (:load-toplevel)			;see notes for ;;destructuring-bind!!!
  (defmacro eclipse:REMF (place indicator &environment env)
    (multiple-value-bind (dummies vals newval setter getter)
	(eclipse:get-setf-expansion place env)
      (do* ((d dummies (cdr d))
	    (v vals (cdr v))
	    (let-list nil)
	    (ind-temp (gensym "IND-TEMP"))
	    (local1 (gensym "LOCAL"))
	    (local2 (gensym "LOCAL")))
	  ((null d)
	   (push (list (car newval) getter) let-list)
	   (push (list ind-temp indicator) let-list)
	   `(let* ,(nreverse let-list)
	      (do ((,local1 ,(car newval) (cddr ,local1))
		   (,local2 nil ,local1))
		  ((atom ,local1) nil)
		(cond ((atom (cdr ,local1))
		       (error "Odd-length property list in REMF."))
		      ((eq (car ,local1) ,ind-temp)
		       (cond (,local2
			      (rplacd (cdr ,local2) (cddr ,local1))
			      (return t))
			     (t (setq ,(car newval) (cddr ,(car newval)))
				,setter
				(return t))))))))
	(push (list (car d) (car v)) let-list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 24.3 SPECIAL FORMS FOR EXHAUSTIVE CASE ANALYSIS              ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro eclipse:ETYPECASE (keyform &rest clauses)
  (let ((form (gensym)))
    `(let ((,form ,keyform))
       (eclipse:typecase ,form
	 ,@clauses
	 (otherwise
	  (eclipse:error 'eclipse:type-error :datum ,form
		 :expected-type
		 '(eclipse:or ,@(mapcar #'first clauses))))))))


(defmacro eclipse:ECASE (&whole whole keyform &rest clauses)
  (let ((form (gensym))
	(types (mapcan #'(lambda (clause &aux (keylist (first clause)))
			   (if (listp keylist)
			       (copy-list keylist)
			       (list keylist)))
		       clauses)))
    (if clauses
	`(let ((,form ,keyform))
	   (eclipse:case ,form
	     ,@clauses
	     (t (eclipse:error 'eclipse:type-error :datum ,form
		       :expected-type '(eclipse:member ,@types)))))
      (eclipse:signal-program-error
       :format-string "No clauses in ~s."
       :format-arguments (list whole)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 21.2 CREATING NEW STREAMS                                   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Arguably, the body of these should be wrapped in an unwind-protect
;;; with a clean-up form that does (close ,var).  Then however, we
;;; would need to initialize the streams from the resource pool as
;;; open, which there is no protocol for.
(defmacro eclipse:WITH-OUTPUT-TO-STRING ((var &optional string
					      &key (element-type ''eclipse:character))
					 &body body)
  (multiple-value-bind (decls body) (eclipse:find-declarations body t)
    `(eclipse:using-resource (,var eclipse::output-buffer ,element-type ,string)
       (declare ,@decls)
       ,@body
       ,@(unless string `((eclipse:get-output-stream-string ,var))))))

(defmacro eclipse:WITH-INPUT-FROM-STRING ((var string &key index (start 0) end)
				  &body body)
  (multiple-value-bind (decls body) (eclipse:find-declarations body t)
    `(eclipse:using-resource (,var eclipse::input-buffer ,string ,start ,end)
       (declare ,@decls)
       ,@(if index
	     `((multiple-value-prog1
		   (progn ,@body)
		 (setq ,index (eclipse::input-buffer-position ,var))))
	     body))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 22 INPUT/OUTPUT                                              ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro eclipse:PRINT-UNREADABLE-OBJECT ((object stream &key type identity)
					   &body body)
  (let ((object-var (gensym "OBJECT"))
	(stream-var (gensym "STREAM"))
	(type-var (gensym "TYPEP"))
	(identity-var (gensym "IDENTITYP"))
	(bodyp (not (null body))))
    `(let ((,object-var ,object)
	   (,stream-var (eclipse::output-stream ,stream))
	   (,type-var ,type)
	   (,identity-var ,identity))
       (eclipse::print-unreadable-object-header
	,object-var ,stream-var ,type-var ,identity-var ,bodyp)
       ,@body
       (eclipse::print-unreadable-object-trailer
	,object-var ,stream-var ,identity-var ,bodyp))))

