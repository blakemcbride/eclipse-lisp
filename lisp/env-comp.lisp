;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                 DECLARATION HANDLERS                         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro DEFINE-DECLARATION (decl-name lambda-list &body form)
  `(setf (gethash ',decl-name *declaration-handlers*)
     (function ,(parse-macro decl-name lambda-list form))))

;;; At one point I thought that since we introduce a new scope when we
;;; find a special declaration (see LOCALLY SPECIAL RULE), we need to
;;; take care not to include them when we're already special (globally
;;; or otherwise)!!! See #+was code.

(define-declaration SPECIAL (&rest variables
				   #+was &environment #+was env)
  (values :variable
	  (mapcar #'(lambda (variable) `(,variable special t)) variables)
	  #+was
	  (mapcan #'(lambda (variable)
		      (unless (eq (variable-information variable env) :special)
			`((,variable special t))))
		  variables)))

(macrolet ((declaration-handler (name type &key (key name) (value t))
	     `(define-declaration ,name (&rest variables)
		(values ,type
			(mapcar #'(lambda (variable)
				    (list variable ',key ',value))
				variables)))))
  (declaration-handler IGNORE :variable)
  (declaration-handler IGNORABLE :variable)
  (declaration-handler DYNAMIC-EXTENT :variable)
  (declaration-handler INLINE :function
		       :value inline)
  (declaration-handler NOTINLINE :function
		       :key inline
		       :value notinline))
				     
;;; IWBNI the following three intersected types as they were added to the
;;; declaration.  As it stands, one must use all the keys returned by
;;; variable/function-information in the declaration alist.
(macrolet ((declaration-handler (name type)
	     `(define-declaration ,name (type &rest variables)
		(values ,type
			(mapcar #'(lambda (variable)
				    (list variable ',name type))
				variables)))))
  (declaration-handler TYPE :variable)
  (declaration-handler FTYPE :function))

(define-declaration TYPE-abbreviation (&whole form &rest variables)
  (values :variable
	  (mapcar #'(lambda (variable)
		      (list variable 'type (car form)))
		  variables)))

;;; This needs to handle data which is not in alist form.
(define-declaration OPTIMIZE (&rest data)
  (flet ((item (name data)
	   (let ((found (find name data
			      :key #'(lambda (obj)
				       (if (consp obj) (car obj)
					 obj)))))
	     (if (listp found) found (list found 3)))))
    (values :declare
	    (delete nil
		    (list 'optimize
			  (item 'speed data)
			  (item 'safety data)
			  (item 'compilation-speed data)
			  (item 'space data)
			  (item 'debug data))))))


(define-declaration DECLARATION (&rest names &environment env)
  ;; In Eclipse, this does not successful bar ALL non-proclamation uses!
  (when env (error 'control-error
		   :format-control "~s can only be used as a proclamation."
		   :format-arguments `((declaration ,@names))))
  (dolist (name names)
    (setf (gethash name *declaration-handlers*)
	  #'(lambda (&rest ignore) (declare (ignore ignore))))))
  

