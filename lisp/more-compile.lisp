;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; STREAMS
(defmacro WITH-OPEN-STREAM ((var stream) &body body)
  (let ((abortp (gensym "ABORTED")))
    (multiple-value-bind (decls body) (find-declarations body t)
      `(let ((,var ,stream)
	     (,abortp t))
	 (declare ,@decls)
	 (unwind-protect
	     (multiple-value-prog1
		 (progn ,@body)
	       (setq ,abortp nil))
	   (when ,var
	     (close ,var :abort ,abortp)))))))

(defmacro WITH-OPEN-FILE ((stream &rest filespec) &body body)
  `(with-open-stream (,stream (open ,@filespec))
     (declare (dynamic-extent ,stream))
     ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; RESOURCES
(defmacro Defresource (name lambda-list &key
			    constructor
			    initializer
			    deinitializer
			    matcher initial-copies)
  (let* ((resource (gensym "RESOURCE"))
	 (object (gensym "OBJECT"))
	 (parameter-names (extract-parameter-names lambda-list))
	 (declaration `(declare (ignorable ,@parameter-names))))
    `(let ((,resource
	    (make-resource
	     :name ',name
	     :constructor
	     #'(lambda (,resource ,@lambda-list)
		 (let ((,object ,constructor))
		   (push (cons ,object
			       (list ,@parameter-names))
			 (resource-parameters ,resource))
		   ,object))
	     :initializer
	     ,(if initializer
		  `#'(lambda (,name ,@lambda-list) ,declaration ,initializer)
		  '#'identity-rest)
	     :deinitializer
	     ,(if deinitializer
		  `#'(lambda (,name ,resource)
		       (destructuring-bind ,parameter-names
			   (get-resource-parameters ,name ,resource)
			 (declare (ignorable ,@parameter-names))
			 ,deinitializer))
		  '#'identity-rest)
	     :matcher
	     ,(if matcher
		  `#'(lambda (,name ,resource ,@lambda-list)
		       (declare (ignore ,resource)
				(ignorable ,name))
		       ,declaration
		       ,matcher)
		  '#'default-resource-matcher))))
       (setf (get ',name 'resource) ,resource)
       (dotimes (n (or ,initial-copies 0))
	 (deallocate-resource
	  ',name (allocate-resource ',name)))
       ',name)))

;;; CLIM doesn't define wither using-resource returns any of the
;;; values returned by body.  We do the safest thing and return all
;;; values. 
(defmacro Using-Resource ((variable name &rest parameters)
			  &body body)
  (multiple-value-bind (decls body) (find-declarations body t)
    `(let ((,variable (allocate-resource ',name ,@parameters)))
       ,@(when decls `((declare ,@decls)))
       (multiple-value-prog1
	   (progn ,@body)
	 (when ,variable (deallocate-resource ',name ,variable))))))

;;; Don't return any values from body.
(defmacro Using-Resource0 ((variable name &rest parameters)
			  &body body)
  `(let ((,variable (allocate-resource ',name ,@parameters)))
     ,@body
     (deallocate-resource ',name ,variable)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ITERATORS 
(defvar not-found)

(defmacro WITH-HASH-TABLE-ITERATOR ((name hash-table) &body body)
  (let ((table (gensym "TABLE"))
	(limit (gensym "LIMIT"))
	(index (gensym "INDEX")))
    `(let* ((,table ,hash-table)
	    (,limit (hash-table-n-buckets ,table))
	    (,index 0))
       (macrolet ((,name ()
			 `(let ((index (first-non-empty-index ,',table ,',index)))
			    (unless (>= index ,',limit)
			      (setf ,',index (1+ index))
			      (values t
				      (hash-table-key ,',table index)
				      (hash-table-value ,',table index))))))
	 ,@body))))

(defmacro WITH-PACKAGE-ITERATOR ((name package-list &rest symbol-types)
				 &body body)
  (unless symbol-types (missing-args 'symbol-type))
  (let ((table (gensym "TABLE"))
	(limit (gensym "LIMIT"))
	(index (gensym "INDEX"))
	(pkg-list (gensym "PACKAGE-LIST"))
	(pkg (gensym "PACKAGE"))
	(remaining-types (gensym "REMAINING-TYPES"))
	(remaining-uses (gensym "REMAINING-USES"))
	(accessibility (gensym "ACESSIBILITY"))
	(next-symbol (gensym "NEXT-SYMBOL"))
	(next-table (gensym "NEXT-TABLE")))
    `(let ((,pkg-list (list-arg ,package-list))
	   ,pkg ,accessibility
	   (,remaining-uses nil)
	   (,remaining-types nil)
	   (,table nil)
	   (,limit 0)
	   (,index 0))
       (labels ((,next-symbol ()
		  (loop for index = (if ,table
					(first-non-empty-index ,table ,index)
					not-found)
			if (< index ,limit)
			do (let ((symbol (hash-table-value ,table index)))
			     (setq ,index (1+ index))
			     (unless (and (eq ,accessibility :inherited)
					  (nth-value 1 (gethash (symbol-name symbol)
								(package-internals ,pkg))))
			       (return symbol)))
			else unless (,next-table) return not-found))
		(,next-table ()
	           (cond (,remaining-uses
			  (setq ,table (package-externals (pop ,remaining-uses))
				,index 0
				,limit (hash-table-n-buckets ,table)))
			 (,remaining-types
			  (ecase (setq ,accessibility (pop ,remaining-types))
			    (:internal (setq ,table (package-internals ,pkg)))
			    (:external (setq ,table (package-externals ,pkg)))
			    (:inherited
			     (setq ,remaining-uses (package-uses ,pkg))
			     (return-from ,next-table (,next-table))))
			  (setq ,index 0
				,limit (hash-table-n-buckets ,table)))
			 (,pkg-list (setq ,remaining-types ',symbol-types
					  ,pkg (pkg (pop ,pkg-list)))
				    (,next-table)))))
	 (macrolet ((,name ()
			   `(let ((symbol (,',next-symbol)))
			      (unless (eq symbol not-found)
				(values t
					symbol
					,',accessibility
					,',pkg)))))
	   (when ,pkg-list (,next-table))
	   ,@body)))))


