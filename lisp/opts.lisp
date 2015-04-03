;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                       TYPES                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; These are defined here so as to be used as much as possible.
(eclipse:define-compiler-macro eclipse:class-name-of (x)
  `(eclipse::instance-tag ,x))

(eclipse:define-compiler-macro eclipse:class-precedence-list-of (x)
  `(eclipse::instance-cpl ,x))

(eclipse:define-compiler-macro eclipse::find-list-eq (item list)
  `(eclipse::fast-find ,item ,list))

(eclipse:define-compiler-macro eclipse::find-slot-definition (class slot-name)
  `(eclipse::fast-find-slot ,class ,slot-name))

;;; If test, test-not, key are all either quoted or not supplied, then
;;; we can improve things:
;;; 1. If list is quoted and one item: (when TEST list)
;;; 2. If list is quoted and more than one item (cond (TEST1 list) ...)
;;; 3. If list is evaluated: loop.
(eclipse:define-compiler-macro eclipse:MEMBER (&whole form item list &rest keys
						      &key test test-not key
						      &allow-other-keys)
  (block member
    (labels ((canonicalize-function (function key)
				    (unless (consp function)
				      (return-from member form))
				    (remf keys key)
				    (destructuring-bind (&optional form-key name &rest more)
					function
				      (when (or more (not name))
					(return-from member form))
				      (case form-key
					(quote name)
					((eclipse:function #-machine-compile
							   cl:function)
					 (if (symbolp name) name
					     (return-from member form))))))
	     (make-pred (item-form element-form)
			`(,(or test-not test) ,item-form
			  ,(if (eq key 'eclipse:identity)
			       element-form
			       `(,key ,element-form))))
	     (make-test (item-form element-form result-form)
			`(,(if test-not 'unless 'when)
			  ,(make-pred item-form element-form)
			  ,result-form))
	     (make-clause (item-form element-form result-form)
			  (let ((pred (make-pred item-form
						 element-form)))
			    (if test-not
				`((not ,pred) ,result-form)
				`(,pred ,result-form)))))
      (setq test (canonicalize-function
		  (cond (test test)
		      ((and (constantp item)
			    (eclipse::eq-test-p (eval item))) ''eq)
		      ((and (constantp list)
			    (every #'eclipse::eq-test-p (eval list))) ''eq)
		      (t ''eclipse:eql))
		  :test))
      (setq key (canonicalize-function (or key ''eclipse:identity) :key))
      (if test-not
	  (setq test-not (canonicalize-function test-not :test-not))
	  (remf keys :test-not))
      (if keys form
	  (if (eclipse:car-eq list 'quote)
	      (let ((items (second list)))
		(if (cdr items)
		    (let ((item-var (gensym "ITEM")))
		      `(let ((,item-var ,item))
			 (cond ,@(loop for elts on items
				       collect (make-clause item-var
							    `',(car elts)
							    `',elts)))))
		    (make-test item `',(car items) list)))
	      (let ((sub (gensym "SUB"))
		    (item-var (gensym "ITEM"))
		    (block (gensym "MEMBER")))
		`(block ,block
		   (do ((,item-var ,item)
			(,sub ,list (cdr ,sub)))
		       ((endp ,sub) nil)
		     ,(make-test item-var `(car ,sub) `(return-from ,block ,sub))))))))))
