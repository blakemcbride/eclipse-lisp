(defvar *file-compiler-env-interned-objects* nil)

(defstruct (FILE-COMPILER-ENV (:include compiler-env))
  (constants (make-hash-table :test #'equal))
  (basic-constants *file-compiler-env-interned-objects*)
  (external-constants (make-hash-table :test #'eq)))

(setq *file-compiler-env-interned-objects*
      (initialize-common-interned-objects (make-file-compiler-env)))

(defstruct (BYTE-FILE-COMPILER-ENV (:include file-compiler-env)))


(defmethod INTERN-CONSTANT ((dynamic-env file-compiler-env) (obj ec:void))
  obj)

(defmethod INTERN-CONSTANT ((dynamic-env file-compiler-env) obj)
  (walk-literal dynamic-env obj))

(defmethod maybe-add-external-variable ((dynamic-env file-compiler-env)
					variable)
  (add-external-variable dynamic-env variable))


(defmethod WALK-INSTRUCTIONS ((dynamic-env file-compiler-env)
			      instructions (s t) (loader t))
  (let (items)
    (maphash #'(lambda (key val)
		 (declare (ignore key))
		 (push val items))
	     (file-compiler-env-constants dynamic-env))
    (list items
	  (compiler-env-functions dynamic-env)
	  (combine-results dynamic-env
			   (compiler-env-inits dynamic-env)
			   instructions))))

