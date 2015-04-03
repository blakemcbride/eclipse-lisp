;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                        PRETTY                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro LP<-BP (xp &optional (ptr nil))
  (unless ptr (setq ptr `(buffer-ptr ,xp)))
  `(+ ,ptr (charpos ,xp)))
(defmacro TP<-BP (xp)
  `(+ (buffer-ptr ,xp) (buffer-offset ,xp)))
(defmacro BP<-LP (xp ptr)
  `(- ,ptr (charpos ,xp)))
(defmacro BP<-TP (xp ptr)
  `(- ,ptr (buffer-offset ,xp)))
;This does not tell you the line position you were at when the TP
;was set, unless there have been no newlines or indentation output 
;between ptr and the current output point.
(defmacro LP<-TP (xp ptr)
  `(LP<-BP ,xp (BP<-TP ,xp ,ptr)))

;We don't use adjustable vectors or any of that, because we seldom have
;to actually extend and non-adjustable vectors are a lot faster in
;many Common Lisps.

(defmacro check-size (xp vect ptr &optional (accessor vect))
  (let* ((min-size (symbol-value (make-name "~a-MIN-SIZE" vect)))
	 (entry-size (symbol-value (make-name "~a-ENTRY-SIZE" vect))))
    `(when (and (> ,ptr ,(- min-size entry-size)) ;seldom happens
		(> ,ptr (- (length (,accessor ,xp)) ,entry-size)))
       (let* ((old (,accessor ,xp))
	      (new (make-array (+ ,ptr ,(if (= entry-size 1) 50
					    (* 10 entry-size)))
			       :element-type (array-element-type old))))
	 (replace new old)
	 (setf (,accessor ,xp) new)))))

(defmacro section-start (xp) `(aref (block-stack ,xp) (block-stack-ptr ,xp)))

(defmacro prefix-ptr (xp)
  `(aref (prefix-stack ,xp) (prefix-stack-ptr ,xp)))
(defmacro suffix-ptr (xp)
  `(aref (prefix-stack ,xp) (+ (prefix-stack-ptr ,xp) 1)))
(defmacro non-blank-prefix-ptr (xp)
  `(aref (prefix-stack ,xp) (+ (prefix-stack-ptr ,xp) 2)))
(defmacro initial-prefix-ptr (xp)
  `(aref (prefix-stack ,xp) (+ (prefix-stack-ptr ,xp) 3)))
(defmacro section-start-line (xp)
  `(aref (prefix-stack ,xp) (+ (prefix-stack-ptr ,xp) 4)))

(defmacro Qtype   (xp index) `(aref (queue ,xp) ,index))
(defmacro Qkind   (xp index) `(aref (queue ,xp) (1+ ,index)))
(defmacro Qpos    (xp index) `(aref (queue ,xp) (+ ,index 2)))
(defmacro Qdepth  (xp index) `(aref (queue ,xp) (+ ,index 3)))
(defmacro Qend    (xp index) `(aref (queue ,xp) (+ ,index 4)))
(defmacro Qoffset (xp index) `(aref (queue ,xp) (+ ,index 5)))
(defmacro Qarg    (xp index) `(aref (queue ,xp) (+ ,index 6)))
(defmacro Qnext (index) `(+ ,index #.queue-entry-size))

; The next function scans the queue looking for things it can do.
;it keeps outputting things until the queue is empty, or it finds
;a place where it cannot make a decision yet.

(defmacro maybe-too-large (xp Qentry)
  `(let ((limit (stream-line-length ,xp)))
     (when (eql (line-limit ,xp) (line-no ,xp)) ;prevents suffix overflow
       (decf limit 2) ;3 for " .." minus 1 for space (heuristic)
       (when (not (minusp (prefix-stack-ptr ,xp)))
	 (decf limit (suffix-ptr ,xp))))
     (cond ((Qend ,xp ,Qentry)
	    (> (LP<-TP ,xp (Qpos ,xp (+ ,Qentry (Qend ,xp ,Qentry)))) limit))
	   ((or force-newlines? (> (LP<-BP ,xp) limit)) T)
	   (T (return nil)))))	;wait until later to decide.

(defmacro misering? (xp)
  `(and *print-miser-width*
	(<= (- (stream-line-length ,xp) (initial-prefix-ptr ,xp)) *print-miser-width*)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                        DESCRIBE                              ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro describe-components ((object stream &key
				       (prefix "")
				       (type `(type-of ,object)))
			       components
			       &body body)
  `(pprint-logical-block (stream nil)
     (describe-header ,object ,stream ,type)
     ,@(when components
	 (let ((forms nil))
	   (dolist (component components
			      `((write-char #\: ,stream)
				,@(nreverse forms)))
	     (destructuring-bind (name &optional
				       (selector
					`(,(make-name "~a~a" prefix name) ,object)))
		 (if (consp component) component (list component))
	       (push `(format ,stream
			      #+not-used
			      "~%  ~@<~/eclipse::list-description/: ~/eclipse::list-description/~:>"
			      "~%  ~@<~s: ~s~:>"
			      ',name ,selector)
		     forms)))))
     ,@body
     (describe-trailer stream)))
