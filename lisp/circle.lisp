
(defun atomicp (object)
  (or (numberp object)			
      (characterp object)
      (and (symbolp object)		;Intern takes care of sharing.
	   (or (symbol-package object)
	       (not (or *print-readably* *print-gensym*))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CIRCULARITY DETECTION
;;; 
;;; Two passes are made through printing. During each pass,
;;; PRINT-CIRCULAR-P must be called exactly once on each object which
;;; is to be checked for circularity and sharing.
;;; - The first pass is used to identify which objects are repeated,
;;;   and to assign #n= indices.  
;;; - The second pass is where the labels are actually printed.
;;;   PRINT-CIRCULAR-P prints #n= and #n# labels as needed.
;;;
;;; During both passes, PRINT-CIRCULAR-P returns true if the object is
;;; a second occurence (within the current pass) of a shared object
;;; (i.e. if #n# was appropriate).  This indicates that nothing
;;; further should be printed by the caller for this object at this
;;; time.
;;;
;;; When the interior-cdr-p argument to PRINT-CIRCULAR-P is true, then
;;; any printing of #n# during the second pass is preceded by ". ".
;;;
;;; The two passes are distinguished by *CURRENT-CIRCLE*.  During the
;;; first pass, *current-circle* holds the negated value of the most
;;; recently used #n= index (initally 0).  During the second pass,
;;; *current-circle* is nil.
;;;
;;; During both passes, *CIRCULARITY-MAP* maps objects to
;;; indicies:
;;; - The first time an object is encoutered during the first pass,
;;;   the value is set to 0.
;;; - The next time the same object is encountered during the first
;;;   pass, *current-circle* is decremented and the value is assigned
;;;   in the hash table.
;;; - After the first time the object is actually printed during the
;;;   second pass (i.e. #n= is printed and print-circle-p returns
;;;   true), the value is negated.

;;; BUG! ANSI requires ciruclarity detection to start over if some
;;; print function starts writing to a new stream.  This means that
;;; *current-circle* and *circularity-map* should really be accessed
;;; through a function that takes stream as an argument.

(defparameter *CURRENT-CIRCLE* nil)
(defparameter *CIRCULARITY-MAP* nil)

(defresource CIRCULARITY-MAP ()
  :constructor (cons (make-broadcast-stream)
		     (make-hash-table :test 'eq))
  :deinitializer (clrhash (cdr circularity-map)))

;;; Execute body twice in a context where a new ciruclarity-map is
;;; used.  Stream-var is the stream to which output will go, and is
;;; rebound during the fast pass.

(defmacro WITH-TWO-CIRCULARITY-PASSES ((stream-var) &body body)
  (with-unique-names (map sink)
    `(using-resource (,map CIRCULARITY-MAP)
       ;; dotted destructuring-bind is broken on CMUCL...
       (let ((,sink (car ,map))
	     (*circularity-map* (cdr ,map)))
	 (let ((*current-circle* 0)
	       (,stream-var ,sink))
	   ,@body)
	 ,@body))))

(defun PRINT-CIRCULAR-P (object stream &optional interior-cdr-p)
  (unless (atomicp object)
    (let ((id (gethash object *circularity-map*)))
      (if *current-circle*
	  (cond ((null id) 
		 (setf (gethash object *circularity-map*) 0)
		 nil)
		(t (when (zerop id)
		     (setf (gethash object *circularity-map*)
			   (decf *current-circle*)))
		   t))
	  (cond ((null id)		;reconsed write-toplevel calls
		 nil)
		((zerop id) nil)
		((minusp id)		;print #n=
		 (when interior-cdr-p (write-string ". " stream))
		 (write-char #\# stream)
		 (print-fixnum10
		  (setf (gethash object *circularity-map*) (- id))
		  stream) 
		 (write-char #\= stream)
		 (when interior-cdr-p	;pprint parens, etc.
		   ;; Not really down a level.
		   (let ((*current-level* (1- *current-level*)))
		     (dispatching-print	;not circular again...
		      (cons (car object) (cdr object))
		      stream))
		   t))
		(t			;positive, print #n#
		 (when interior-cdr-p (write-string ". " stream))
		 (write-char #\# stream)
		 (print-fixnum10 id stream)
		 (write-char #\# stream)
		 t))))))

