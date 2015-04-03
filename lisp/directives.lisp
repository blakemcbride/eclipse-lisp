;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;               COMPILED FORMAT                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(declaim (special *string* *used-args* *used-outer-args* *used-initial*
		  *get-arg-carefully* *inner-end* *outer-end* *at-top*))


(defmethod FORMAT ((stream STREAM) (processor STRING) &rest args)
  (apply #'format stream
	 (process-format-string processor nil)
	 args))

(define-condition FORMAT-ERROR (parse-error) ())
(defun format-error (msg i)
  (error 'format-error
	 :format-control "~A~%~V@T|~%~S"
	 :format-arguments (list msg (1+ i) *string*)))


(defparameter *format-string-cache* (make-hash-table :test #'eq))

(defun process-format-string (string-or-fn force-fn?)
  (cond ((not (stringp string-or-fn)) string-or-fn) ;called from ~? too.
	(*format-string-cache*
	 (let ((value (gethash string-or-fn *format-string-cache*)))
	   (when (or (not value) (and force-fn? (stringp value)))
	     (setq value (maybe-compile-format-string string-or-fn force-fn?))
	     (setf (gethash string-or-fn *format-string-cache*) value))
	   value))
	(t (maybe-compile-format-string string-or-fn force-fn?))))


(defparameter *fn-table* (make-hash-table))	;Used to access fns for commands.

;Each of the format handler functions expect to get called with two
;arguments start and end.  Start points to the first character after
;the ~ marking the command.  End points to the first character after
;the command.  This includes the matching end command for paired
;commands.

(defmacro def-format-handler (char args &body body)
  (let ((name (make-name "FORMAT-~A" char)))
    `(progn
       (defun ,name ,args ,@ body)
       (setf (gethash (char-upcase ,char) *fn-table*) (function ,name))
       (setf (gethash (char-downcase ,char) *fn-table*) (function ,name))
       ',name)))

;Definitions of the forms used in the code created by PARSE.
;Note these functions assume the stream is in the var XP and is an xp stream,

; INITIAL holds the initial value of ARGS (for ~@*).
;Initial is always bound to (args) if it is bound at all.
;Note this uses args, but only when actually binding

(defun initial () (setq *used-initial* T) 'init)

(defmacro bind-initial (&body code)
  `(let* ((*used-initial* nil)
	  (body (progn ,@ code)))
     (if *used-initial* (make-binding-form 'init (args) body) body)))

; ARGS holds the current argument list
;The val bound to args must always be computed (to use it up) even if args is not used.

(defun args () (setq *used-args* T) argsv)

(defmacro bind-args (doit? val &body code)
  (if (eq doit? T)
      `(let* ((val ,val)
	      (*used-args* nil)
	      (body (progn ,@ code)))
	 (if *used-args* (make-binding-form argsv val body) (cons val body)))
      `(flet ((code () ,@ code))
	 (if (not ,doit?) (code) ;important bindings not done if not doit?
	     (let* ((val ,val)
		    (*used-args* nil)
		    (body (code)))
	       (if *used-args* (make-binding-form argsv val body) (cons val body)))))))

(defun outer-args () (setq *used-outer-args* T) 'outer-args)

(defmacro bind-outer-args (&body code)
  `(let* ((*used-outer-args* nil)
	  (body (progn ,@ code)))
     (if *used-outer-args* (make-binding-form 'outer-args (args) body) body)))

(defmacro maybe-bind (doit? var val &body code)
  `(let ((body (progn ,@ code)))
     (if ,doit? (make-binding-form ,var ,val body) body)))

(defun make-binding-form (var value body)
  `((let ((,var ,value)) ,@ body)))

(defun num-args () `(length ,(args)))

;;; Water's XP code did something special here to turn off circularity
;;; detection when *at-top* is true.  See handle-logical-block.

(defun get-arg ()
  (cond (*get-arg-carefully*
	 (args)
	 '(pprint-pop))
	 (t `(pop ,(args)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This computes ahead of time what the write-string function does at run-time. 
(defun pretty-literal (start end)
  (let ((sub-end nil) next-newline (result nil))
    (loop (setq next-newline
		(position #\newline *string* :start start :end end))
	  (setq sub-end (if next-newline next-newline end))
	  (when (< start sub-end)
	    (push (if (= start (1- sub-end))
		      `(write-char++ ,(aref *string* start) xp)
		      `(write-string++
			,(safe-subseq *string* start sub-end)
			xp ,0 ,(- sub-end start)))
		  result))
	  (when (null next-newline) (return nil))
	  (push `(stream-newline xp :unconditional) result)
	  (setq start (1+ sub-end)))
    (if (null (cdr result)) (car result) (cons 'progn (nreverse result)))))

(defun simple-literal (start end)
  `(stream-write-string xp ,(safe-subseq *string* start end)))

(defparameter *format-literal* #'simple-literal)

(defun expand-format-string (*string* &optional force-fn?)
  (declare (ignore force-fn?))
  `(lambda (xp &rest ,argsv)
     ,@(bind-initial
	`((block top
	    ,@(let ((*get-arg-carefully* nil)
		    (*at-top* t)
		    (*inner-end* 'top)
		    (*outer-end* 'top))
		(compile-format 0 (length *string*))))))
     ,(args)))

(defun maybe-compile-format-string (string force-fn?) ;should compile???
  #+machine-compile
  (compile nil (expand-format-string string force-fn?))
  #-machine-compile
  (case user::*eval*
    ((trace nil) (let* ((form (expand-format-string string force-fn?))
			(lambda (when (car-eq form 'lambda) form)))
		   (unless (consp lambda)
		     (case (car lambda)
		       ((lambda cl:lambda))
		       (t (setq lambda nil))))
		   (cl:format t "*** cl:eval ~s => ~s.~%" string form)
		   (cond (lambda (cl:compile nil lambda))
			 (t (cl:format t "*** not a lambda form.~%")
			    (cl:eval form)))))
    (t (cl:eval (expand-format-string string force-fn?)))))

(defmacro FORMATTER (string)
  (expand-format-string string))

;This is available for putting on #".
(defun format-string-reader (stream sub-char arg)
  (bad-reader-parameter stream arg sub-char)
  (unread-char sub-char stream)
  (let ((string (read stream))
	(*default-format-function-package* (package-name *package*)))
    (unless *read-suppress* 
      (expand-format-string string))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun position-not-in (set start)
  (position-if-not #'(lambda (c) (find c set)) *string* :start start))


(defun params-end (start) ;start points just after ~
  (let ((j start) (end (length *string*)))
    (loop
      (setq j (position-not-in "+-0123456789,Vv#:@" j))
      (when (null j) (format-error "Missing directive." (1- start)))
      (when (not (eq (aref *string* j) #\')) (return j))
      (incf j)
      (if (= j end) (format-error "No character after '." (1- j)))
      (incf j))))

;Only called after correct parse is known.

(defun directive-start (end) ;end points at characters after params
  (loop
    (setq end (position #\~ *string* :end end :from-end T))
    (when (or (zerop end) (not (eq (aref *string* (1- end)) #\')))
      (return end))
    (decf end)))

(defun next-directive1 (start end)
  (let ((i (position #\~ *string* :start start :end end)) j)
    (when i
      (setq j (params-end (1+ i)))
      (when (char= (aref *string* j) #\/)
	(setq j (position #\/ *string* :start (1+ j) :end end))
 	(when (null j)
	  (format-error "Matching / missing" (position #\/ *string* :start start)))))
    (values i j)))

(defun next-directive (start end)
  (let (i j ii k count c close
	(pairs '((#\( . #\)) (#\[ . #\]) (#\< . #\>) (#\{ . #\}))))
    (multiple-value-setq (i j) (next-directive1 start end))
    (when i
      (setq c (aref *string* j))
      (setq close (cdr (assoc c pairs)))
      (when close
	(setq k j count 0)
	(loop
	  (multiple-value-setq (ii k) (next-directive1 k end))
	  (when (null ii) (format-error "No matching close directive." j))
	  (when (eql (aref *string* k) c) (incf count))
	  (when (eql (aref *string* k) close) (decf count)
	    (when (minusp count) (setq j k) (return nil))))))
    (values c i j)))

;breaks things up at ~; directives.

(defun chunk-up (start end)
  (let ((positions (list start)) (spot start))
    (loop
      (multiple-value-bind (c i j) (next-directive spot end)
	(declare (ignore i))
	(when (null c) (return (nreverse (cons end positions))))
	(when (eql c #\;) (push (1+ j) positions))
	(setq spot j)))))



(defun num-args-in-args (start &optional (err nil))
  (let ((n 0) (i (1- start)) c)
    (loop
      (setq i (position-not-in "+-0123456789," (1+ i)))
      (setq c (aref *string* i))
      (cond ((or (char= c #\V) (char= c #\v)) (incf n))
	    ((char= c #\#) 
	     (when err
	       (format-error "# not allowed in ~<...~> by (formatter \"...\")." start))
	     (return nil))
	    ((char= c #\') (incf i))
	    (T (return n))))))

(defun compile-format (start end)
  (let ((result nil))
    (prog (c i j fn)
      L (multiple-value-setq (c i j) (next-directive start end))
      (when (if (null c) (< start end) (< start i))
	(push (funcall *format-literal* start (if i i end)) result))
      (when (null c) (return (nreverse result)))
      (when (char= c #\newline)
	(multiple-value-bind (colon atsign)
	    (parse-params (1+ i) nil :nocolonatsign T)
	  (when atsign (push '(stream-newline xp :unconditional) result))
	  (incf j)
	  (when (not colon)
	    (setq j (position-if-not
		      #'(lambda (c)
			  (or (char= c #\tab) (char= c #\space)))
		      *string* :start j :end end))
	    (when (null j) (setq j end)))
	  (setq start j)
	  (go L)))
      (setq fn (gethash c *fn-table*))
      (when (null fn) (format-error "Unknown format directive." j))
      (incf j)
      (push (funcall fn (1+ i) j) result)
      (setq start j)
      (go L))))

;This gets called with start pointing to the character after the ~ that
;starts a command.  Defaults, is a list of default values for the
;parameters.  Max is the maximum number of parameters allowed.  Nocolon,
;noatsign, nocolonatsign can be used to specify what colon atsign
;combinations are permitted. Parse params returns three values, colon?,
;atsign? and a list of code chunks that correspond to the parameters
;specified.

(defun parse-params (start defaults &key (max (length defaults))
		     (nocolon nil) (noatsign nil) (nocolonatsign nil))
  (let ((colon nil) (atsign nil) (params nil) (i start) j c)
    (loop
      (setq c (aref *string* i))
      (cond ((or (char= c #\V) (char= c #\v)) (push (get-arg) params) (incf i))
	    ((char= c #\#) (push (num-args) params) (incf i))
	    ((char= c #\') (incf i) (push (aref *string* i) params) (incf i))
	    ((char= c #\,) (push nil params))
	    (T (setq j (position-not-in "+-0123456789" i))
	       (if (= i j) (return nil))
	       (push (parse-integer *string* :start i :end j :radix 10.) params)
	       (setq i j)))
      (if (char= (aref *string* i) #\,) (incf i) (return nil)))
    (setq params (nreverse params))
    (do ((ps params (cdr ps))
	 (ds defaults (cdr ds))
	 (nps nil))
	((null ds) (setq params (nreconc nps ps)))
      (push (cond ((or (null ps) (null (car ps))) (car ds))
		  ((not (consp (car ps))) (car ps))
		  (T `(cond (,(car ps)) (T ,(car ds)))))
	    nps))
    (if (and max (< max (length params))) (format-error "Too many parameters." i))
    (loop
      (setq c (aref *string* i))
      (cond ((char= c #\:)
	     (if colon (format-error "Two colons specified." i))
	     (setq colon T))
	    ((char= c #\@)
	     (if atsign (format-error "Two atsigns specified." i))
	     (setq atsign T))
	    (T (return nil)))
      (incf i))
    (if (and colon nocolon) (format-error "Colon not permitted." i))
    (if (and atsign noatsign) (format-error "Atsign not permitted." i))
    (if (and colon atsign nocolonatsign)
	(format-error "Colon and atsign together not permitted." i))
    (values colon atsign params)))

;Only called if correct parse already known.
(defun colonp (j) ;j points to directive name
  (or (eql (aref *string* (1- j)) #\:)
      (and (eql (aref *string* (1- j)) #\@)
	   (eql (aref *string* (- j 2)) #\:))))

(defun atsignp (j) ;j points to directive name
  (or (eql (aref *string* (1- j)) #\@)
      (and (eql (aref *string* (1- j)) #\:)
	   (eql (aref *string* (- j 2)) #\@))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;               FORMAT DIRECTIVES                              ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *default-format-function-package* :user)

(def-format-handler #\/ (start end)
  (multiple-value-bind (colon atsign params) (parse-params start nil :max nil)
    (let* ((whole-name-start (1+ (params-end start)))
	   (colon-pos (position #\: *string* :start whole-name-start :end (1- end)))
	   (pkg (find-package
		  (if colon-pos
		      (string-upcase (subseq *string* whole-name-start colon-pos))
		      *default-format-function-package*)))
	   (name-start (cond ((null colon-pos) whole-name-start)
			     ((and (< colon-pos (1- end))
				   (char= #\: (aref *string* (1+ colon-pos))))
			      (+ colon-pos 2))
			     (T (1+ colon-pos))))
	   (fn (intern (string-upcase (subseq *string* name-start (1- end))) pkg)))
      (if (not (find-if #'consp params))
	  `(funcall (symbol-function ',fn) xp ,(get-arg) ,colon ,atsign ,@ params)
	  (let ((vars (mapcar #'(lambda (arg)
				  (declare (ignore arg))
				  (gentemp))
			      params)))
	    `(let ,(mapcar #'list vars params)
	       (funcall (symbol-function ',fn) xp ,(get-arg) ,colon ,atsign ,@ vars)))))))

(defun format-parameter-binding-p (value)
  (consp value))
(defun format-parameter-binding (name value)
  (when (format-parameter-binding-p value) `((,name ,value))))
(defun format-parameter (name value)
  (if (format-parameter-binding-p value) name value))
  
;;; It is not clear how to combine multiline output with non-zero
;;; mincol.  For example, what does it mean to use up at least mincol
;;; colums if the output of the argument ends up on two lines due to
;;; internal newlines?  We try to do "the right thing" for the left
;;; justified case, where we look make sure that (>= (- start-column
;;; end-column) mincol), even if start-colum and end-column are on
;;; different lines.  For the right justfied case, we just write the
;;; argument to a string and use the its total length.

(defun make-padded-formatter (start end escapep)
  (declare (ignore end))
  (multiple-value-bind (colon atsign params)
      (parse-params start '(0 1 0 #\space))
    (destructuring-bind (mincol colinc minpad padchar) params
      (let* ((padp (not (eql mincol 0)))
	     (get-minpad (format-parameter 'minpad minpad))
	     (get-padchar (format-parameter 'padchar padchar))
	     (atsign (and padp atsign)))
	`(let ((*print-escape* ,escapep)
	       ,@(unless escapep '((*print-readably* nil)))
	       ,@(format-parameter-binding 'mincol mincol)
	       ,@(format-parameter-binding 'colinc colinc)
	       ,@(format-parameter-binding 'minpad minpad)
	       ,@(format-parameter-binding 'padchar padchar)
	       (arg ,(get-arg)))
	   (let (,@(when padp
		      (if atsign
			  `((string ,(let ((basic '(write-to-string arg)))
				       (if colon
					   `(if arg ,basic "()")
					 basic))))
			'((start-col (stream-line-column xp))))))
	     ,@(unless atsign
		 (let ((basic '(write-toplevel arg xp)))
		   `(,(if colon
			  `(if arg ,basic (write-string "()" xp))
			basic))))
	     ,@(unless (eql minpad 0)
		 `((multiple-chars1 xp ,get-minpad ,get-padchar)))
	     ,@(when padp
		 `((pad-chars xp (+ ,get-minpad
				    ,(if atsign
					 '(length string)
				       '(- (stream-line-column xp)
					   start-col)))
			      ,(format-parameter 'mincol mincol)
			      ,(format-parameter 'colinc colinc)
			      ,get-padchar)
		   ,@(when atsign
		       `((write-string string xp :start 0
				       :end (length string))))))))))))

(def-format-handler #\A (start end)
  (make-padded-formatter start end nil))
(def-format-handler #\S (start end)
  (make-padded-formatter start end t))

(def-format-handler #\C (start end)
  (declare (ignore end))
  (multiple-value-bind (colon atsign) (parse-params start nil)
    (if colon
	`(print-character-pretty ,(get-arg) xp nil ,atsign)
      (if atsign
	  `(prin1 ,(get-arg) xp)
	`(write-char ,(get-arg) xp)))))
  

  
(defun make-radix-formatter1 (colon atsign base params)
  (destructuring-bind (mincol padchar commachar comma-interval)
      params
    (let* ((padp (not (eql mincol 0)))
	   (basic `(format-grouped-integer
		    ,(format-parameter 'commachar commachar)
		    ,(format-parameter 'comma-interval comma-interval)
		    ,(get-arg) ,base ,atsign ,colon ,(if padp 's 'xp))))
      `(let (,@(format-parameter-binding 'mincol mincol)
	       ,@(format-parameter-binding 'padchar padchar)
	       ,@(format-parameter-binding 'commachar commachar)
	       ,@(format-parameter-binding 'comma-interval comma-interval)
	       ,@(when padp
		   `((string (with-output-to-string (s) ,basic)))))
	 ,@(when padp
	     `((pad-chars xp (length string)
			  ,(format-parameter 'mincol mincol)
			  1 ,(format-parameter 'padchar padchar))))
	 ,(if padp
	      `(write-string string xp :start 0 :end (length string))
	    basic)))))

(defun make-radix-formatter (base start end)
  (declare (ignore end))
  (multiple-value-bind (colon atsign params)
      (parse-params start '(0 #\space #\, 3))
    (make-radix-formatter1 colon atsign base params)))
	  
(def-format-handler #\D (start end)
  (make-radix-formatter 10 start end))
(def-format-handler #\B (start end)
  (make-radix-formatter 2 start end))
(def-format-handler #\O (start end)
  (make-radix-formatter 8 start end))
(def-format-handler #\X (start end)
  (make-radix-formatter 16 start end))

(def-format-handler #\R (start end)
  (if (find-if-not #'(lambda (c) (member c '(#\: #\@)))
		   *string* :start start :end (1- end))
      (multiple-value-bind (colon atsign params)
	  (parse-params start '(10 0 #\space #\, 3))
	(make-radix-formatter1 colon atsign
				(first params) (rest params)))
    (multiple-value-bind (colon atsign)
	(parse-params start nil)
      (if atsign
	  (if colon
	      `(format-print-old-roman xp ,(get-arg))
	    `(format-print-roman xp ,(get-arg)))
	(if colon
	    `(format-print-ordinal xp ,(get-arg))
	  `(format-print-cardinal xp ,(get-arg)))))))


;;; These should be reviewed to see if they can be integrated
;;; better!!!  As it stands, they come from CMUCL

(def-format-handler #\F (start end)
  (declare (ignore end))
  (multiple-value-bind (colon atsign params)
      (parse-params start '(nil nil nil nil #\space) :nocolon t)
    (declare (ignore colon))
    (destructuring-bind (w d k overflowchar padchar) params
      `(format-fixed xp ,(get-arg) ,w ,d ,k ,overflowchar ,padchar ,atsign))))



(def-format-handler #\E (start end)
  (declare (ignore end))
  (multiple-value-bind (colon atsign params)
      (parse-params start '(nil nil nil 1 nil #\space nil) :nocolon t)
    (declare (ignore colon))
    (destructuring-bind (w d e k overflowchar padchar exponentchar) params
      `(format-exponential xp ,(get-arg) ,w ,d ,e ,k ,overflowchar
			   ,padchar ,exponentchar ,atsign))))


(def-format-handler #\G (start end)
  (declare (ignore end))
  (multiple-value-bind (colon atsign params)
      (parse-params start '(nil nil nil 1 nil #\space nil) :nocolon t)
    (declare (ignore colon))
    (destructuring-bind (w d e k overflowchar padchar exponentchar) params
      `(format-exponential xp ,(get-arg) ,w ,d ,e ,k ,overflowchar
			   ,padchar ,exponentchar ,atsign))))


(def-format-handler #\$ (start end)
  (declare (ignore end))
  (multiple-value-bind (colon atsign params)
      (parse-params start '(2 1 0 #\space))
    (destructuring-bind (d n w padchar) params
      `(format-dollars xp ,(get-arg) ,d ,n ,w ,padchar ,colon ,atsign))))

(defun format-dollars (stream number d n w pad colon atsign)
  (if (rationalp number) (setq number (coerce number 'single-float)))
  (if (floatp number)
      (let* ((signstr (if (minusp number) "-" (if atsign "+" "")))
	     (signlen (length signstr)))
	(multiple-value-bind (str strlength ig2 ig3 pointplace)
			     (flonum-to-string number nil d nil)
	  (declare (ignore ig2 ig3))
	  (when colon (write-string signstr stream))
	  (dotimes (i (- w signlen (- n pointplace) strlength))
	    (write-char pad stream))
	  (unless colon (write-string signstr stream))
	  (dotimes (i (- n pointplace)) (write-char #\0 stream))
	  (write-string str stream)))
      (format-write-field stream
			  (decimal-string number)
			  w 1 0 #\space t)))


;Format directives that get open coded "P%&~|T*?^"

(def-format-handler #\P (start end) (declare (ignore end))
  (multiple-value-bind (colon atsign) (parse-params start nil)
  (let ((arg (if colon `(car (backup-in-list 1 ,(initial) ,(args))) (get-arg))))
    (if atsign
	`(if (not (eql ,arg 1))
	     (write-string "ies" xp :start 0 :end 3)
	   (write-char #\y xp))
	`(if (not (eql ,arg 1)) (write-char #\s XP))))))

(def-format-handler #\% (start end) (declare (ignore end))
  (multiple-newlines start :unconditional))

(def-format-handler #\& (start end) (declare (ignore end))
  (multiple-newlines start :fresh))

(defun multiple-newlines (start kind)
  (multiple-value-bind (colon atsign params) 
      (parse-params start '(1) :nocolon T :noatsign T)
      (declare (ignore colon atsign))
    (if (eql (car params) 1) `(stream-newline xp ,kind)
	`(multiple-newlines1 xp ,kind ,(car params)))))

(def-format-handler #\| (start end) (declare (ignore end))
  (multiple-chars start #\page))

(def-format-handler #\~ (start end) (declare (ignore end))
  (multiple-chars start #\~))

(defun multiple-chars (start char)
  (multiple-value-bind (colon atsign params)
      (parse-params start '(1) :nocolon t :noatsign t)
      (declare (ignore colon atsign))
    (if (eql (car params) 1) `(write-char ,char xp)
	`(multiple-chars1 xp ,(car params) ,char))))


(def-format-handler #\T (start end) (declare (ignore end))
  (multiple-value-bind (colon atsign params) (parse-params start '(1 1))
    `(stream-tab xp ,(if colon
			 (if atsign :section-relative :section)
			 (if atsign :line-relative :line))
		 ,(pop params) ,(pop params))))

(def-format-handler #\* (start end) (declare (ignore end))
  (if (atsignp (params-end start))
      (multiple-value-bind (colon atsign params)
	  (parse-params start '(0) :nocolon t)
	  (declare (ignore colon atsign))
	`(setq ,argsv (backup-to ,(car params) ,(initial) ,(args))))
      (multiple-value-bind (colon atsign params)
	  (parse-params start '(1))
	  (declare (ignore atsign))
	`(setq ,argsv
	       ,(if colon `(backup-in-list ,(car params) ,(initial) ,(args))
		    `(nthcdr ,(car params) ,(args)))))))


(def-format-handler #\? (start end) (declare (ignore end))
  (multiple-value-bind (colon atsign) (parse-params start nil :nocolon t)
    (declare (ignore colon))
    (if atsign
	`(let ((fn (process-format-string ,(get-arg) T)))
	   (setq ,argsv (apply fn xp ,(args))))
      `(apply #'format xp ,(get-arg) ,(get-arg)))))

(def-format-handler #\^ (start end) (declare (ignore end))
  (multiple-value-bind (colon atsign params)
      (parse-params start nil :max 3 :noatsign t)
      (declare (ignore atsign))
    `(if ,(cond ((null params) `(null ,(if colon `(cdr ,(outer-args)) (args))))
		(t `(do-complex-^-test ,@ params)))
	 (return-from ,(if colon *outer-end* *inner-end*) nil))))

(defun do-complex-^-test (a1 &optional (a2 nil) (a3 nil))
  (cond (a3 (and (<= a1 a2) (<= a2 a3)))
	(a2 (= a1 a2))
	(t (= 0 a1))))

;delimited pairs of format directives. "(){}[]<>;"

(def-format-handler #\[ (start end)
  (multiple-value-bind (colon atsign params)
      (parse-params start nil :max 1 :nocolonatsign T)
    (setq start (1+ (params-end start)))
    (let* ((chunks (chunk-up start end))
	   (innards (do ((ns chunks (cdr ns))
			 (ms (cdr chunks) (cdr ms))
			 (result nil))
			((null ms) (nreverse result))
		      (push (compile-format (car ns) (directive-start (car ms)))
			    result))))
      (cond (colon (when (not (= (length innards) 2))
		     (format-error "Wrong number of clauses in ~:[...~]." (1- start)))
		   `(cond ((null ,(get-arg)) ,@ (car innards))
			  (T ,@ (cadr innards))))
	    (atsign (when (not (= (length innards) 1))
		      (format-error "Too many clauses in ~@[...~]." (1- start)))
		    `(cond ((car ,argsv) ,@ (car innards)) (T ,(get-arg))))
	    (T (let* ((j -1) (len (- (length chunks) 2))
		      (else? (colonp (1- (nth len chunks)))))
		 `(case ,(if params (car params) (get-arg))
		    ,@(mapcar #'(lambda (unit)
				  (incf j)
				  `(,(if (and else? (= j len)) T j) ,@ unit))
			      innards))))))))

(def-format-handler #\( (start end)
  (multiple-value-bind (colon atsign) (parse-params start nil)
    (setq start (1+ (params-end start)))
    (setq end (directive-start end))
    `(stream-case-print xp
			,(cond ((and colon atsign) :UP)
			       (colon :CAP1)
			       (atsign :CAP0)
			       (T :DOWN))
			#'(lambda (xp)
			    ,@(compile-format start end)))))

(def-format-handler #\; (start end) (declare (ignore start))
  (format-error "~; appears out of context." (1- end)))
(def-format-handler #\] (start end) (declare (ignore start))
  (format-error "Unmatched closing directive." (1- end)))
(def-format-handler #\) (start end) (declare (ignore start))
  (format-error "Unmatched closing directive." (1- end)))
(def-format-handler #\> (start end) (declare (ignore start))
  (format-error "Unmatched closing directive." (1- end)))
(def-format-handler #\} (start end) (declare (ignore start))
  (format-error "Unmatched closing directive." (1- end)))

(def-format-handler #\{ (start end)
  (multiple-value-bind (colon atsign params)
      (parse-params start '(-1) :max 1)
    (let* ((force-once (colonp (1- end)))
	   (n (car params))
	   (bounded (not (eql n -1))))
      (setq start (1+ (params-end start)))
      (setq end (directive-start end))
      (car (maybe-bind bounded 'N n ;must be outermost if is V or #
	     (maybe-bind (not (> end start)) 'FN  ;must be second
			 `(process-format-string ,(get-arg) T)
	       (bind-args (not atsign) (get-arg)
		 `((prog () ,@(if force-once '((go S)))
		       L (unless ,(args) (return nil))
		       ,@(if force-once '(S))
			 ,@(if bounded '((if (= N 0) (return nil) (decf N))))
			 ,@(bind-outer-args
			     (bind-args colon (get-arg)
			       (bind-initial
				 (let ((*get-arg-carefully*
					(and *get-arg-carefully* atsign))
				       (*at-top* (and *at-top* atsign))
				       (*outer-end* nil)
				       (*inner-end* nil))
				   (if (not colon)
				       (if (not (> end start))
					   `((setq ,argsv (apply FN xp ,(args))))
					   (compile-format start end))
				       (let ((*inner-end* 'inner))
					 `((block inner
					     ,@(if (not (> end start))
						   `((setq ,argsv (apply FN xp ,(args))))
						   (compile-format start end))))))))))
			 (go L))))))))))

(def-format-handler #\< (start end)
  (if (colonp (1- end))
      (handle-logical-block start end)
      (handle-standard-< start end)))


(defun handle-standard-< (start end)
  (multiple-value-bind (colon atsign params)
      (parse-params start '(0 1 0 #\space))
    (destructuring-bind (mincol colinc minpad padchar) params
      (setq start (1+ (params-end start)))
      (let* ((chunks (chunk-up start end))
	     (innards (do ((ns chunks (cdr ns))
			   (ms (cdr chunks) (cdr ms))
			   (*inner-end* 'segments)
			   (result nil))
			  ((null ms) (return (nreverse result)))
			(push `(push (with-output-to-string (xp)
				       ,@(compile-format
					  (car ns)
					  (directive-start (car ms))))
				     strings)
			      result))))
	(multiple-value-bind (colon1 atsign1 params)
	    (parse-params (1+ (position #\~ *string* :start (first chunks)
					:end (second chunks)
					:from-end t))
			  '(0 (or (stream-line-length xp) 72))
			  :noatsign t) 
	  (declare (ignore atsign1))
	  (destructuring-bind (n w) params
	    `(let ((mincol ,mincol)
		   (colinc ,colinc)
		   (minpad ,minpad)
		   (padchar ,padchar)
		   ,@(if colon1
			 `((strings (list (block segments ,(second (pop innards)))))
			   (n ,n)
			   (w ,w))
		       '((strings nil))))
	       (block segments ,@innards)
	       (setq strings (nreverse strings))
	       (format-justify xp
			       ,@(if colon1 '((pop strings) n w)
				   '(nil 0 0))
			       strings ,colon ,atsign mincol colinc minpad padchar))))))))

;;;;;;;;;;;;;;;;;;;;;;;

;The pretty-printing directives. "_IW<:>"

(def-format-handler #\_ (start end) (declare (ignore end))
  (multiple-value-bind (colon atsign) (parse-params start nil)
    `(pprint-newline ,(cond ((and colon atsign) :mandatory)
			     (colon :fill)
			     (atsign :miser)
			     (T :linear)) XP)))

(def-format-handler #\I (start end) (declare (ignore end))
  (multiple-value-bind (colon atsign params)
      (parse-params start '(0) :noatsign T)
    (declare (ignore atsign))
    `(pprint-indent ,(if colon :current :block) ,(car params) XP)))

(def-format-handler #\W (start end) (declare (ignore end))
  (multiple-value-bind (colon atsign) (parse-params start nil)
    (cond ((not (or colon atsign)) `(write-toplevel ,(get-arg) XP))
	  (T `(let (,@(if colon '((*print-pretty* T)))
		      ,@(if atsign '((*print-level* nil) (*print-length* nil))))
		(write-toplevel ,(get-arg) XP))))))

(defresource padded-format-string (length)
  :constructor (make-array length :element-type 'character
			   :fill-pointer t :adjustable t)
  :initializer (setf (fill-pointer padded-format-string) 0)
  :matcher (>= (array-dimension padded-format-string 0) length))

;;; ANSI says: "Other than the difference in its argument, ~@<...~:>
;;; is exactly the same as ~<...~:> except that circularity detection
;;; is not applied if ~@<...~:> is encountered at top level in a
;;; format string. This ensures that circularity detection is applied
;;; only to data lists, not to format argument lists."  As long as our
;;; implementation always conses up fresh &rest lists, then their is
;;; no need to bypass the ciruclarity detection!  If this changes,
;;; then when *at-top*, we will have to either turn circularity
;;; detection through some flag or copy-list of the args binding.

(defun handle-logical-block (start end)
  (multiple-value-bind (colon atsign) (parse-params start nil)
    (setq start (1+ (params-end start)))
    (let* ((chunks (chunk-up start end))
	   (on-each-line?
	    (and (cddr chunks) (atsignp (1- (cadr chunks)))))
	   (prefix
	    (cond ((cddr chunks) (pop chunks)
		   (safe-subseq *string* start (directive-start (car chunks))))
		  (colon "(")
		  (t "")))
	   (suffix
	    (cond ((cddr chunks)
		   (safe-subseq *string* (cadr chunks)
				(directive-start (caddr chunks))))
		  (colon ")")
		  (t ""))))
      (when (cdddr chunks) (format-error "Too many subclauses in ~<...~:>." (1- start)))
      (when (and prefix (or (find #\~ prefix) (find #\newline prefix)))
	(format-error "Prefix in ~<...~:> must be a literal string without newline." start))
      (when (and suffix (or (find #\~ suffix) (find #\newline suffix)))
	(format-error "Suffix in ~<...~:> must be a literal string without newline."
	     (cadr chunks)))
      (car (bind-args T (if atsign
			    `(prog1 ,(args) (setq ,(args) nil))
			    (get-arg))
		      (bind-initial
		       `((pprint-logical-block (xp ,(args)
						   ,(if on-each-line?
							:per-line-prefix
							:prefix)
						   ,prefix
						   :suffix ,suffix)
			  ,@(or (let ((*get-arg-carefully* T)
				      (*at-top* (and *at-top* atsign))
				      (*inner-end* logical-block)
				      (*outer-end* logical-block)
				      (sub-start (car chunks))
				      (sub-end (directive-start (cadr chunks)))
				      (*format-literal* #'pretty-literal))
				  (if (atsignp (1- end))
				      (let ((source *string*))
					(using-resource (*string* padded-format-string
								  (* 2 (- sub-end sub-start)))
					  (fill-transform-string source *string*
								 sub-start
								 sub-end)
					  (compile-format 0 (length *string*))))
				      (compile-format sub-start
						      sub-end)))
				'((declare (ignore xp))))))))))))

(defun push-fill (s)
  (vector-push-extend #\~ s)
  (vector-push-extend #\: s)
  (vector-push-extend #\_ s)
  s)

(defun fill-transform-string (source copy start end)
  (loop with insertp = nil and suppressp = nil
	for i from start below end
	for c = (char source i)
	do (cond ((char= c #\space)
		  (unless (or insertp suppressp
			      (setq suppressp (and (> i 1) (string= "~
" source :start2 (- i 2) :end2 i)))
			      (setq insertp t))))
		 (insertp (push-fill copy)
			  (setq insertp nil))
		 (t (setq suppressp nil)))
	do (vector-push-extend c copy)
	finally (return (if insertp
			    (push-fill copy)
			    copy))))

