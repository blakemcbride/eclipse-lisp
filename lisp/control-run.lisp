;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                ERROR CHECKING                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-bad-key (args keys)
  (loop for key in args by #'cddr
	unless (and (symbolp key)
		    (or (eq key :all-other-keys) ;Always allowed
			(member key keys)))
	return key))

;;; Two passes are required because :allow-other-keys could be at end.
(defun check-keys (args keys &optional error-fmt &rest error-args)
  (unless (getf args :allow-other-keys)
    (let ((bad (find-bad-key args keys)))
      (when bad (apply #'key-not-allowed bad error-fmt error-args)))))

;;; These two are in runtime library because they are useful in the MOP. 

(defun eclipse:signal-program-error (&optional (fmt "") &rest args)
  (error 'program-error :format-control fmt :format-arguments args))

(defun eclipse::multiple-appearance-error (name &optional context)
  (eclipse:signal-program-error
   "~s appears more than once~@[ in ~a~]." name context))

