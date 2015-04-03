(defun eclipse:qualifier-match (pattern qualifiers)
  ;; typecase here would have trouble bootstrapping
  (cond ((eq pattern '*) t)
	((consp pattern)
	 (and (eclipse:qualifier-match (car pattern) (car qualifiers))
	      (eclipse:qualifier-match (cdr pattern) (cdr qualifiers))))
	(t (equal pattern qualifiers))))

(define-method-combination STANDARD ()
  ((around (:around))
   (before (:before))
   (primary () :required t)
   (after (:after) :order :most-specific-last))
  (flet ((call-methods (methods)
		       (loop for method in methods
			     collect `(call-method ,method ()))))
    (let* ((main `(call-method ,(car primary) ,(rest primary)))
	   (main-after (if after
			   `(multiple-value-prog1 ,main
			      ,@(call-methods after))
			   main))
	   (form (if before
		    `(progn ,@(call-methods before)
			    ,main-after)
		    main-after)))
      (if around
	  `(call-method ,(car around)
			(,@(rest around)
			   (make-method ,form)))
	  form))))

(define-method-combination + :identity-with-one-argument t)
(define-method-combination AND :identity-with-one-argument t)
(define-method-combination OR :identity-with-one-argument t)
(define-method-combination MAX :identity-with-one-argument t)
(define-method-combination MIN :identity-with-one-argument t)
(define-method-combination PROGN :identity-with-one-argument t)
(define-method-combination APPEND :identity-with-one-argument t)
(define-method-combination NCONC :identity-with-one-argument t)
(define-method-combination LIST)

(let* ((standard (find-method-combination-type 'standard))
       (instance (progn (finalize-inheritance standard)
			(make-instance standard))))
  (set-tagged-instance-class *standard-method-combination* standard)
  (set-tagged-instance-wrapper *standard-method-combination*
			       (tagged-instance-wrapper instance))
  (set-standard-instance-slots *standard-method-combination*
			       (standard-instance-slots instance))
  (setf (gethash '(standard) (class-instances standard))
	*standard-method-combination*)
  (macrolet
      ((set-sealed (&rest types)
		   `(progn ,@(loop for type in types
				   collect
				   `(setf (host::static-access
					   (find-method-combination-type
					    ',type)
					   sealed class) t)))))
    (set-sealed standard + and or max min progn append nconc list)))
	
	