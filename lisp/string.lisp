;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                ACCESS UTILITIES                              ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; These all take fixnum indices and do bounds checking.
(defun base-schar (string index)
  (char-character
   (base-char-elt string (check-simple-index string index))))
(defun set-base-schar (string index character)
  (set-base-char-elt string (check-simple-index string index)
		     (character-char character))
  character)

(defun extended-schar (string index)
  (wchar-character
   (extended-char-elt string (check-simple-index string index))))
(defun set-extended-schar (string index character)
  (set-extended-char-elt string (check-simple-index string index)
			 (character-wchar character))
  character)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgeneric SCHAR (string index))

(defmethod schar ((string SIMPLE-BASE-STRING) (index integer))
  (base-schar string index))

(defmethod schar ((string SIMPLE-EXTENDED-STRING) (index integer))
  (extended-schar string index))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgeneric (setf SCHAR) (character string index))

(defmethod (setf schar) ((character character)
			 (string SIMPLE-BASE-STRING) (index integer))
  (set-base-schar string index character))

(defmethod (setf schar) ((character character)
			  (string SIMPLE-EXTENDED-STRING) (index integer))
  (set-extended-schar string index character))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgeneric CHAR (string index))

(defmethod char ((string SIMPLE-BASE-STRING) (index integer))
  (base-schar string index))

(defmethod char ((string SIMPLE-EXTENDED-STRING) (index integer))
  (extended-schar string index))

(defmethod char ((string STRING) (index integer))
  (char (simple-array-contents string)
	(+ (complex-array-offset string) index)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgeneric (setf CHAR) (character string index))

(defmethod (setf char) ((character character)
			(string SIMPLE-BASE-STRING) (index integer))
  (set-base-schar string index character))

(defmethod (setf char) ((character character)
			(string SIMPLE-EXTENDED-STRING) (index integer))
  (set-extended-schar string index character))

(defmethod (setf char) ((character character)
			(string STRING) (index integer))
  (setf (char (simple-array-contents string)
	      (+ (complex-array-offset string) index))
	character))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 18.2 STRING COMPARISON                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun compare-strings (f key
			string1 string2
			&key (start1 0) (start2 0) end1 end2)
  (with-simple-vector ((string1 (string string1))
		       start1 end1 length offset1)
    (with-simple-vector ((string2 (string string2))
			 start2 end2)
      (do ((index1 start1 (1+ index1))
	   (index2 start2 (1+ index2)))
	  ((or (= index1 end1) (= index2 end2))
	   (and (funcall f (- end1 start1) (- end2 start2)) (- index1 offset1)))
	(let ((i1 (funcall key (fast-aref string1 index1)))
	      (i2 (funcall key (fast-aref string2 index2))))
	  (when (/= i1 i2)
	    (return (and (funcall f i1 i2) (- index1 offset1)))))))))

(macrolet ((def-comp (name f key)
	       `(defun ,name (&rest args)
		  (declare (dynamic-extent args))
		  (apply #'compare-strings
			 (function ,f) (function ,key) args))))
  (def-comp STRING=	=	char-int)
  (def-comp STRING/=  	/=	char-int)
  (def-comp STRING<   	<	char-int)
  (def-comp STRING<=  	<=	char-int)
  (def-comp STRING>   	>	char-int)
  (def-comp STRING>=  	>=	char-int)
  (def-comp STRING-EQUAL	=	char-key)
  (def-comp STRING-NOT-EQUAL  	/=	char-key)
  (def-comp STRING-LESSP   	<	char-key)
  (def-comp STRING-NOT-GREATERP	<=	char-key)
  (def-comp STRING-GREATERP  	>	char-key)
  (def-comp STRING-NOT-LESSP  	>=	char-key)
  )

(defun leading-string= (lead string &key (start 0) end
			&aux (max (+ start (length lead))))
  (string= string lead :start1 start :end1 (if end (min end max) max)))
(defun leading-string-equal (lead string &key (start 0) end
			     &aux (max (+ start (length lead))))
  (string-equal string lead :start1 start :end1 (if end (min end max) max)))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 18.3 STRING CONSTRUCTION AND MANIPULATION                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun MAKE-STRING (size &key (initial-element nil initial-element-p)
			      (element-type 'character))
  (initialize-vector size element-type
		     initial-element initial-element-p
		     nil nil nil))

(defun STRING-LEFT-TRIM (char-bag string
			 &aux (s (string string))
			      (l (length s)))
  (do ((index 0 (1+ index)))
      ((or (= index l)
	   (not (find (char s index) char-bag)))
       (subseq s index l))))

(defun STRING-RIGHT-TRIM (char-bag string
			   &aux (s (string string))
				(l (length s)))
  (do ((index l p)
       (p (1- l) (1- p)))
      ((or (= index 0)
	   (not (find (char s p) char-bag)))
       (subseq s 0 index))))


(defun STRING-TRIM (char-bag string
			   &aux (s (string string))
				(l (length s)))
  (let ((start (do ((index 0 (1+ index)))
		   ((or (= index l)
			(not (find (char s index) char-bag)))
		    index))))
    (subseq s start
	    (do ((index l p)
		 (p (1- l) (1- p)))
		((or (= index start)
		     (not (find (char s p) char-bag)))
		 index)))))


(defun STRING-UPCASE (string &key (start 0) end)
  (let* ((string (string string))
	 (l (length string))
	 (end (or end l))
	 (new (make-string l :element-type (array-element-type string))))
    (dotimes (i start) (setf (char new i) (char string i)))
    (do ((i start (1+ i)))
	((= i end))
      (setf (char new i) (char-upcase (char string i))))
    (do ((i end (1+ i)))
	((= i l))
      (setf (char new i) (char string i)))
    new))

(defun STRING-DOWNCASE (string &key (start 0) end)
  (let* ((string (string string))
	 (l (length string))
	 (end (or end l))
	 (new (make-string l
			   :element-type
			   (array-element-type string))))
    (dotimes (i start) (setf (char new i) (char string i)))
    (do ((i start (1+ i)))
	((= i end))
      (setf (char new i) (char-downcase (char string i))))
    (do ((i end (1+ i)))
	((= i l))
      (setf (char new i) (char string i)))
    new))

(defun STRING-CAPITALIZE (string &key (start 0) end)
  (let* ((string (string string))
	 (l (length string))
	 (end (or end l))
	 (new (make-string l
			   :element-type
			   (array-element-type string)))
	 (new-word-p t))
    (dotimes (i start) (setf (char new i) (char string i)))
    (do ((i start (1+ i)))
	((= i end))
      (setf (char new i)
	(let ((c (char string i)))
	  (cond ((not (alphanumericp c))
		 (setq new-word-p t)
		 c)
		(new-word-p
		 (setq new-word-p nil)
		 (char-upcase c))
		(t (char-downcase c))))))
    (do ((i end (1+ i)))
	((= i l))
      (setf (char new i) (char string i)))
    new))

(defun NSTRING-UPCASE (string &key (start 0) end)
  (let* ((string (string string))
	 (end (or end (length string))))
    (do ((i start (1+ i)))
	((= i end))
      (setf (char string i) (char-upcase (char string i))))
    string))

(defun NSTRING-DOWNCASE (string &key (start 0) end)
  (let* ((string (string string))
	 (end (or end (length string))))
    (do ((i start (1+ i)))
	((= i end))
      (setf (char string i) (char-downcase (char string i))))
    string))

(defun NSTRING-CAPITALIZE (string &key (start 0) end)
  (let* ((string (string string))
	 (end (or end (length string)))
	 (new-word-p t))
    (do ((i start (1+ i)))
	((= i end))
      (setf (char string i)
	(let ((c (char string i)))
	  (cond ((not (alphanumericp c)) (setq new-word-p t)
					 c)
		(new-word-p (setq new-word-p nil)
			    (char-upcase c))
		(t (char-downcase c))))))
    string))

(defun STRING (x)
  (etypecase x
    (string x)
    (symbol (symbol-name x))
    (character (make-string 1 :initial-element x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                     PRINTING                                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod PRINT-OBJECT ((string string) stream)
  (cond ((or *print-escape* *print-readably*)
	 (write-char #\" stream)
	 (dotimes (i (length string))
	   (let ((c (char string i)))
	     (when (find c "\\\"") (write-char #\\ stream))
	     (write-char c stream)))
	 (write-char #\" stream))
	(t (write-string string stream))))

  