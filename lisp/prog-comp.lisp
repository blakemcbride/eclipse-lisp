(let (#+excl(excl::*enable-package-locked-errors* nil))
  (defmacro eclipse:lambda (lambda-list &body forms)
    `(function (lambda ,lambda-list ,@forms))))


(defun eclipse::parse-lambda (lambda-list whole-name
				 &key macro default varargs
				 &aux tmp
				 (bindings nil)
				 checks decls
				 (counter 0))
  (labels
      ((bad-key (key) (eclipse:signal-program-error
		       "Misplaced ~s in lambda-list." key))
       (set-tmp (val)
		(unless tmp
		  (setq tmp (make-symbol (symbol-name whole-name))))
		`(setq ,tmp ,val))
       (simple-binding (v init) (push (list v init) bindings))
       (add-var (v init)
		(cond ((symbolp v) (simple-binding v init))
		      (t (unless macro
			   (warn "Non-ANSI destructuring of normal function lambda-list."))
			 (let ((whole (gensym)))
			   (simple-binding whole init)
			   (parse v whole)))))
       (parse (vl whole &aux rest-var
		  (argsp (if varargs
			     `(eclipse::vp ,whole)
			     whole))
		  (argspop (if varargs
			       `(eclipse::vpop ,whole)
			       `(pop ,whole)))
		  (argslist (if varargs
				`(eclipse::vargs ,whole)
				whole)))
	      (flet ((check-missing-rest ()
					 (unless rest-var
					   (let ((arg (make-symbol "KEYS")))
					     (add-var (setf rest-var arg)
						      argslist)
					     (push `(dynamic-extent ,arg) decls)))))
		(do (v keys whole? optional? rest? key? allow-other-keys? aux?)
		    ((not (consp vl))	;Dotted lambda-list or end.
		     (when vl		;Dotted lambda-list
		       (when rest? (bad-key 'dotted-list)) (setq rest? t)
		       (simple-binding vl whole))
		     ;; Now ended, add checks.
		     (cond ((and key? (not allow-other-keys?))
			    (check-missing-rest)
			    (push `(eclipse:check-keys ,rest-var ',keys) checks))
			   ((not rest?)
			    (push `(eclipse::when ,argsp
				     (eclipse:extra-args ,argslist))
				  checks))))
		  (case (setq v (pop vl))
		    (&whole (when (or whole? (not macro)) (bad-key '&whole))
			    (add-var (pop vl) argslist)
			    (setq whole? t))
		    (&optional (when optional? (bad-key v))
			       (setq optional? t))
		    ((&rest &body) (when rest? (bad-key v))
		     (add-var (setf rest-var (pop vl)) argslist)
		     (setq optional? t rest? t))
		    (&key (when key? (bad-key '&key))
			  (setq key? t optional? t rest? t))
		    (&allow-other-keys (when (or (not key?) allow-other-keys?)
					 (bad-key '&allow-other-keys))
				       (setq allow-other-keys? t))
		    (&aux (when aux? (bad-key '&aux))
			  (setq aux? t optional? t))
		    (otherwise
		     (incf counter)
		     (let ((default default) sv)
		       (when (and optional? (listp v))
			 (destructuring-bind (v1 &optional (d1 default) sv1) v
			   (setq v v1 default d1 sv sv1)))
		       (cond
			(aux? (add-var v default))
			(key? 
			 (check-missing-rest)
			 (let (key)
			   (if (listp v)
			       (setq key (first v) v (second v))
			       (setq key (eclipse:make-keyword (symbol-name v))))
			   (add-var v `(if ,(set-tmp `(eclipse:key-arg ',key ,rest-var))
					   (car ,tmp) ,default))
			   (when sv (add-var sv tmp))
			   (push key keys)))
			(t (add-var v `(if ,(if sv
						(set-tmp argsp)
						argsp)
					   ,argspop
					   ,(if optional?
						default
						`(eclipse:missing-args ,counter))))
			   (when sv (add-var sv tmp)))))))))))
    (parse lambda-list whole-name)
    (setq bindings (nreverse bindings))
    (let ((full-bindings (if tmp
			     (cons (list tmp 'eclipse::unbound-flag) bindings)
			     bindings)))
      (values full-bindings (nreverse checks) decls bindings))))