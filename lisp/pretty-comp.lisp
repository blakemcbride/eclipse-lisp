;;; The references to the function and block named LOGICAL-BLOCK and
;;; args should really be renamed using with-unique-names.
;;; Unfortunately, the implementation of FORMAT depends on the name of
;;; the block and args list being known.

(defvar eclipse::argsv (make-symbol "ARGS"))
(defvar eclipse::logical-block (make-symbol "LOGICAL-BLOCK"))

(defmacro eclipse:PPRINT-LOGICAL-BLOCK ((stream-symbol object
					       &key (prefix "" prefixp)
					       (per-line-prefix "" per-line-p)
					       (suffix ""))
				&body body)
  (when (and prefixp per-line-p)
    (eclipse::signal-program-error
     "Both :prefix ~s and :per-line-prefix ~s given pprint-logical-block."
     prefix per-line-prefix))
  (case stream-symbol
    ((nil) (setq stream-symbol 'eclipse:*standard-output*))
    ((t) (setq stream-symbol 'eclipse:*terminal-io*)))
  (multiple-value-bind (decls body) (eclipse:find-declarations body t)
      `(flet ((,eclipse::logical-block (,eclipse::argsv ,stream-symbol)
                 (declare ,@decls (ignorable ,eclipse::argsv))
		 (macrolet ((eclipse:pprint-pop ()
			     '(if (eclipse::print-element-aborted-p
				   ,eclipse::argsv
				   ,stream-symbol)
				  (return-from ,eclipse::logical-block nil)
				  (pop ,eclipse::argsv)))
			    (eclipse:pprint-exit-if-list-exhausted ()
			     '(unless ,eclipse::argsv
				(return-from ,eclipse::logical-block nil))))
		   ,@body)))
	 (eclipse::pprint-toplevel ,object (eclipse::output-stream ,stream-symbol)
			  ,(if prefixp prefix per-line-prefix) ,suffix
			  ,(not (null per-line-p)) #',eclipse::logical-block))))

(defmacro eclipse:PPRINT-POP ()
  (eclipse::signal-program-error
   "PPRINT-POP used outside of PPRINT-LOGICAL-BLOCK."))

(defmacro eclipse:PPRINT-EXIT-IF-LIST-EXHAUSTED ()
  (eclipse::signal-program-error
   "PPRINT-EXIT-IF-LIST-EXHAUSTED used outside of PPRINT-LOGICAL-BLOCK."))


