(in-package :eclipse)

(defun EVAL-FEATURE (feature)
  (declare (special *features*))
  (unless *read-suppress*		;This is the change.
    (if (atom feature)
	(member feature *features*)
	(destructuring-bind (key . rest) feature
	  (ecase key
	    (:not (not (apply #'eval-feature rest)))
	    (:and (every #'eval-feature rest))
	    (:or (some #'eval-feature rest)))))))

;;; Need to redefine because Eclipse C source doesn't reference through
;;; symbol-function of eval-feature.  
(defun FEATURE-READER (s c bad &optional reverse-p)
  (bad-reader-parameter s bad c)
  (if (xor (eval-feature (let ((*package* (find-package :keyword)))
			   (read s t nil t)))
	   reverse-p)
      (read s t nil t)
      (let ((*read-suppress* t)) (read s t nil t) (values))))

;;; Ditto.
(defun NOT-FEATURE-READER (s c bad)
  (feature-reader s c bad t))

;;; Update the readtables.
(set-dispatch-macro-character #\# #\+ #'feature-reader *standard-readtable*)
(set-dispatch-macro-character #\# #\- #'not-feature-reader *standard-readtable*)
(set-dispatch-macro-character #\# #\+ #'feature-reader *readtable*)
(set-dispatch-macro-character #\# #\- #'not-feature-reader *readtable*)

