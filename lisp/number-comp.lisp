
(eclipse:define-modify-macro eclipse:INCF (&optional (delta 1)) eclipse:add)
(eclipse:define-modify-macro eclipse:DECF (&optional (delta 1)) eclipse:subt)

(eclipse:define-setf-expander eclipse:LDB (&environment env bytespec int)
  (multiple-value-bind (temps vals stores store-form access-form)
      (eclipse:get-setf-expansion int env)
    (let ((btemp (gensym))
	  (store (gensym))
	  (stemp (first stores)))
      (values `(,btemp ,@temps)
	      `(,bytespec ,@vals)
	      `(,store)
	      `(let ((,stemp (eclipse:dpb ,store ,btemp ,access-form)))
	         ,store-form ,store)
	      `(eclipse:ldb ,btemp ,access-form)))))

(eclipse:define-setf-expander eclipse:MASK-FIELD (&environment env bytespec int)
  (multiple-value-bind (temps vals stores store-form access-form)
      (eclipse:get-setf-expansion int env)
    (let ((btemp (gensym))
	  (store (gensym))
	  (stemp (first stores)))
      (values `(,btemp ,@temps)
	      `(,bytespec ,@vals)
	      `(,store)
	      `(let ((,stemp (eclipse:deposit-field ,store ,btemp ,access-form)))
	         ,store-form ,store)
	      `(eclipse:mask-field ,btemp ,access-form)))))

;;; Until the compiler gets smarter, these are crucially important because:
;;; 1. The &rest args in the non-2arg case conses.
;;; 2. The dolist in the non-2arg case calls endp.
(macrolet ((def2arg (op 2op ident singlep)
	     `(eclipse:define-compiler-macro ,op (&whole form &rest args)
		(case (length args)
		  ,@(when singlep `((0 ,ident)))
		  (1 ,(if singlep
			  '(car args)
			  ``(,',2op ,',ident ,@args)))
		  (2 `(,',2op ,@args))
		  (t form)))))
  (def2arg eclipse:+ eclipse::add 0 t)
  (def2arg eclipse:- eclipse::subt 0 nil)
  (def2arg eclipse:* eclipse::mult 1 t)
  (def2arg eclipse:/ eclipse::div 1 nil))

;;; The 3arg case is fairly common for checking whether something is
;;; between two others.  
(macrolet ((def2arg (op 2op)
	     `(eclipse:define-compiler-macro ,op (&whole form &rest args)
		(case (length args)
		  (1 (first args))
		  (2 `(,',2op ,@args))
		  (3 (destructuring-bind (low mid high) args
		       (let ((lowv (gensym "LOW"))
			     (midv (gensym "MID"))
			     (highv (gensym "HIGH")))
			 `(let ((,lowv ,low)
				(,midv ,mid)
				(,highv ,high))
			    (and (,',2op ,lowv ,midv)
				 (,',2op ,midv ,highv))))))
		  (t form)))))
  (def2arg eclipse:= eclipse::eq-number)
  (def2arg eclipse:< eclipse::lt)
  (def2arg eclipse:<= eclipse::le)
  (def2arg eclipse:> eclipse::gt)
  (def2arg eclipse:>= eclipse::ge))
  
					     