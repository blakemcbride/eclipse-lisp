;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 23.3 PRESENTATION METHODS

(define-default-presentation-method PRESENT (object type stream view &key)
  (declare (ignore view))
  (write object :stream stream)
  object)

(define-default-presentation-method ACCEPT (type stream view &key)
  (declare (ignore view))
  (let ((object (read stream nil nil)))
    (cond ((presentation-typep object type) object)
          (t (handle-input-error object type)))))

(define-default-presentation-method DESCRIBE-PRESENTATION-TYPE (type stream plural-count &key)
  (declare (ignore plural-count))
  (with-presentation-type-decoded (type-name) type
    (let ((some-vowels '(#\a #\e #\i #\o))
          (name-string (symbol-name type-name)))
      (cond ((member (char name-string 0) some-vowels :test #'char-equal)
             (write-string "an " stream))
            (t (write-string "a " stream)))
      (write-string (string-downcase name-string) stream))
    type))

(define-default-presentation-method PRESENTATION-TYPE-SPECIFIER-P (type &key)
  t)

(define-default-presentation-method PRESENTATION-TYPEP (object type)
  (with-presentation-type-decoded (type-name) type
    (typep object type-name)))

(define-default-presentation-method PRESENTATION-SUBTYPEP (type putative-supertype &key)
  (with-presentation-type-decoded (supertype-name) putative-supertype
    (with-presentation-type-decoded (type-name) type
      (cond ((presentation-type-superior-p type-name supertype-name) t)
            (t nil)))))

;;; MAP-OVER-PRESENTATION-TYPE-SUPERTYPES!!!

(define-default-presentation-method ACCEPT-PRESENT-DEFAULT
  (type stream view default default-supplied-p present-p query-identifier &key)
  (declare (ignore present-p query-identifier))
  (cond (default-supplied-p (present default type :stream stream :view view))
        (t (present type 'presentation-type-specifier :stream stream :view view))))

(define-default-presentation-method handle-input-error (object type stream view &key)
  (declare (ignore object stream view))
  (values nil nil))


;;; presentation-type-history, presentation-default-preprocessor,
;;; presentation-refined-position-test, highlight-presentation


;;; 23.8.2 BASIC PRESENTATION TYPES
;;; BOOLEAN
(define-presentation-method PRESENTATION-TYPEP (value (type BOOLEAN))
  (declare (ignore value)) 
  t)

(define-presentation-method ACCEPT ((type BOOLEAN) stream (view TEXTUAL-view) &key)
  (let ((value (read-token stream :parse nil)))
    (cond ((string-equal value "NO") (setq value nil))
          ((string-equal value "NIL") (setq value nil))
          (t (setq value t)))
    (unless (presentation-typep value type)
      (handle-input-error value type))
    value))

(define-presentation-method PRESENT (value (type BOOLEAN) stream (view TEXTUAL-VIEW) &key)
  (cond (value (write-string "Yes" stream))
        (t (write-string "No" stream)))
  value)

;;; SYMBOL
(define-presentation-method ACCEPT ((type SYMBOL) stream (view TEXTUAL-VIEW) &key)
  (let* ((string (read-token stream :parse nil))
         (end (length string))
         (colon-pos (position #\: string :end end :from-end t))
         (package (find-package 
                    (cond ((and colon-pos
				(char-equal #\:
					    (char string (the fixnum (1- colon-pos)))))
                           (subseq string 0 (the fixnum (1- colon-pos))))
                          (colon-pos 
                           (subseq string 0 colon-pos))
                          (t nil))))
         (symbol-name (subseq string (the fixnum (1+ (or colon-pos -1))) end)))
    (declare (dynamic-extent end colon-pos package))
    (multiple-value-bind (symbol returned)
        (cond (package (intern symbol-name package))
              (t (intern symbol-name)))
      (declare (ignore returned))
      symbol)))

(define-presentation-method PRESENT (symbol (type SYMBOL) stream (view TEXTUAL-VIEW)
					    &key acceptably)
  (cond (acceptably
         (multiple-value-bind (found status) 
             (find-symbol (symbol-name symbol))
           (declare (ignore found))
           (write-string (package-name (symbol-package symbol)) stream)
           (case status
             (:external (write-char #\: stream))
             (t (write-string "::" stream)))
           (write-string  (symbol-name symbol) stream)))
        (t (write-string (symbol-name symbol) stream)))
  symbol)

;;; NULL
(define-presentation-method PRESENTATION-TYPEP (object (type NULL))
  (not object))

(define-presentation-method ACCEPT ((type NULL) stream (view TEXTUAL-VIEW) &key)
  (let* ((value (read-token stream :parse nil))
         (end (length value)))
    (when (or (string-equal value "NONE" :start1 0 :end1 end :start2 0 :end2 4)
              (string-equal value "NIL" :start1 0 :end1 end :start2 0 :end2 3))
      (setq value nil))
    (unless (presentation-typep value type)
      (handle-input-error value type))
    value))

(define-presentation-method PRESENT (object (type NULL) stream (view TEXTUAL-VIEW) &key)
  (declare (ignore object))
  (write-string "None" stream)
  nil)

(define-presentation-method DESCRIBE-PRESENTATION-TYPE ((type NULL) stream plural-count &key)
  (declare (ignore plural-count))
  (write-string "None" stream))

;;; KEYWORD
(define-presentation-method ACCEPT ((type KEYWORD) stream (view TEXTUAL-VIEW) &key)
  (let* ((string (read-token stream :parse nil))
         (end (length string))
         (colon-pos (position #\: string :end end :from-end t))
         (symbol (cond (colon-pos 
                        (intern (subseq string (the fixnum (1+ colon-pos)) end) :keyword))
                       (t (intern string :keyword)))))
    (declare (dynamic-extent end colon-pos))
    (unless symbol 
      (handle-input-error string type))
    symbol))

(define-presentation-method PRESENT (keyword (type KEYWORD) stream (view TEXTUAL-VIEW) &key)
  (write keyword :stream stream)
  keyword)

;;; 23.8.2 NUMERIC PRESENTATION TYPES
(define-presentation-method PRESENTATION-TYPEP (number (type COMPLEX))
  (or (null type)
      (and (presentation-typep (realpart number) type)
           (presentation-typep (imagpart number) type))))

(define-presentation-method PRESENTATION-TYPEP (number (type REAL))
  (with-presentation-type-decoded (type-name) type
    (and (typep number type-name)
         (or (null low) 
             (>= number low))
         (or (null high) 
             (<= number high)))))

(define-presentation-method PRESENT (number (type REAL) stream (view TEXTUAL-VIEW) &key)
  (write number :base base :radix radix :stream stream)
  number)

(define-presentation-method DESCRIBE-PRESENTATION-TYPE :AFTER ((type REAL)
							       stream
							       plural-count &key) 
  (declare (ignore plural-count))
  (when (or low high)
    (cond ((and low high) 
	   (write-string " between " stream)
	   (present low 'real :stream stream)
	   (write-string " and " stream)
	   (present high 'real :stream stream))
	  (low
	   (write-string " greater than " stream)
	   (present low 'real :stream stream))
	  (high
	   (write-string " less than " stream)
	   (present high 'real :stream stream)))))

(define-presentation-method PRESENTATION-TYPEP (number (type INTEGER))
  (and (typep number 'integer)
       (or (null low) 
           (>= number low))
       (or (null high) 
           (<= number high))))

(define-presentation-method PRESENTATION-TYPEP (number (type FLOAT))
  (and (typep number 'float)
       (or (null low) 
           (>= number low))
       (or (null high) 
           (<= number high))))

;;; 23.8.3 CHARACTER AND STRING PRESENTATION TYPES
(define-presentation-method PRESENT (character (type CHARACTER) stream
					       (view TEXTUAL-VIEW) &key)
  (write-char character stream))

(define-presentation-method ACCEPT ((type CHARACTER) stream (view TEXTUAL-VIEW) &key)
  (let ((char (read-char stream nil nil)))
    (unless (presentation-typep char type)
      (handle-input-error char type))
    char))

(define-presentation-method PRESENTATION-TYPEP (string (type BASIC-STRING))
  (typep string 'string))

(define-presentation-method ACCEPT ((type BASIC-STRING) stream (view TEXTUAL-VIEW) &key)
  (let ((string (read-line stream nil nil)))
    (unless (presentation-typep string type)
      (handle-input-error string type))
    string))

(define-presentation-method PRESENT (string (type BASIC-STRING) stream
					    (view TEXTUAL-VIEW) &key)
  (write-string string stream)
  string)

(define-presentation-method PRESENTATION-TYPEP (string (type STRING))
  (and (typep string 'string)
       (cond (length (eql length (length string)))
             (t t))))

(define-presentation-method PRESENTATION-TYPEP (string (type BOUNDED-STRING))
  (and (typep string 'string)
       (cond (max-length (<= (length string) max-length))
             (t t))))

(define-presentation-method DESCRIBE-PRESENTATION-TYPE ((type BOUNDED-STRING)
							stream plural-count &key) 
  (declare (ignore plural-count))
  (cond (max-length 
         (write-string "a string with maximum length " stream)
         (present max-length 'integer :stream stream))
        (t (write-string "a string" stream))))

;;; 23.8.4 PATHNAME PRESENTATION TYPE
(define-presentation-method PRESENT (pathname (type PATHNAME) stream
					      (view TEXTUAL-VIEW)
					      &key) 
  (when pathname(write-string (namestring pathname) stream))
  pathname)

(define-presentation-method ACCEPT ((type PATHNAME) stream (view TEXTUAL-VIEW)
				    &key 
                                    (default *default-pathname-defaults*))
  (let* ((string (read-line stream nil nil))
         (pathname (when string (pathname string))))
    (when (and pathname default  merge-default)
      (setq pathname 
            (merge-pathnames pathname
                             (make-pathname :host (pathname-host default)
                                            :device (pathname-device default)
                                            :directory (pathname-directory default)
                                            :name (pathname-name default)
                                            :type default-type
                                            :version default-version)
                             default-version)))
    (unless (presentation-typep pathname type)
      (handle-input-error pathname type))
    pathname))

(define-presentation-method PRESENTATION-TYPEP (pathname (type EXISTING-PATHNAME))
  (when pathname
    (probe-file pathname)))

(define-presentation-method DESCRIBE-PRESENTATION-TYPE ((type EXISTING-PATHNAME)
							stream plural-count &key)
  (declare (ignore plural-count))
  (write-string "a pathname corresponding to an existing file" stream))

;;; 23.8.5 "ONE-OF" AND SOME-OF" PRESENTATION TYPES
;; no actual completion right now.  one-of and some-of types can still be used, though.
(define-presentation-method PRESENT (object (type COMPLETION) stream
					    (view TEXTUAL-VIEW) &key) 
  (write-string (funcall name-key object) stream)
  object)

(define-presentation-method PRESENTATION-TYPEP (value (type MEMBER))
  (member value elements :test #'eql))

(define-presentation-method PRESENTATION-TYPEP (value (type MEMBER-SEQUENCE))
  (member value sequence :test test))

(define-presentation-method PRESENTATION-TYPEP (value (type MEMBER-ALIST))
  (member value alist :test test :key value-key))

(define-presentation-method PRESENT (object (type SUBSET-COMPLETION)
					    stream (view TEXTUAL-VIEW)
					    &key) 
  (cond ((and object (atom object))
         (write-string (funcall name-key object) stream)
         object)
        (object (write-string (funcall name-key (car object)) stream)
                (loop for item in (cdr object)
                      do (when separator (write-char separator stream))
                      do (when echo-space (write-char #\Space stream))
                      do (write-string (funcall name-key item) stream))
                object)
        (t (present object 'null :stream stream :view +textual-view+))))

(define-presentation-method PRESENTATION-TYPEP (object (type SUBSET))
  (cond ((null object) t)
        ((atom object) (member object elements :test #'eql))
        ((listp object) (subsetp object elements :test #'eql))
        (t nil)))

(define-presentation-method PRESENTATION-TYPEP (object (type SUBSET-SEQUENCE))
  (cond ((null object) t)
        ((atom object) (member object sequence :test test))
        ((listp object) (subsetp object sequence :test test))
        (t nil)))

(define-presentation-method PRESENTATION-TYPEP (object (type SUBSET-ALIST))
  (cond ((null object) t)
        ((atom object) (member object alist :test test :key value-key))
        ((listp object) (subsetp object alist :test test :key value-key))
        (t nil)))

;;; 23.8.6 Sequence Presentation Types
(define-presentation-method PRESENTATION-TYPEP (sequence (type SEQUENCE))
  (and (typep sequence 'sequence)
       (every #'(lambda (item) (presentation-typep item type)) sequence)))

(define-presentation-method PRESENT (sequence (type SEQUENCE) stream
					      (view TEXTUAL-VIEW)
					      &key) 
  (cond (sequence
         (let ((last (1- (length sequence)))
               (pos 0))
           (map nil
                #'(lambda (item)
                    (present item type :stream stream :view view)
                    (unless (= pos last)
                      (when separator (write-char separator stream))
                      (when echo-space (write-char #\Space stream)))
                    (incf pos))
                sequence)))
        (t (present sequence 'null :stream stream :view view)))
  sequence)

(define-presentation-method ACCEPT ((type SEQUENCE) stream (view TEXTUAL-VIEW) &key)
  (flet ((accept-element (stream)
           (loop for char = (peek-char nil stream nil nil t)
                 when (or (null char) (char-equal char separator)
			  (member char '(#\tab #\Return #\Linefeed)))
                   do (return (accept-from-string type (coerce result 'string) :view view))
                 else collect char into result
                      and do (read-char stream nil nil t))))
    (declare (inline accept-element))
    (let ((sequence
            (loop for char = (peek-char nil stream nil nil t)
                  when (or (null char) (member char '(#\tab #\Return #\Linefeed)))
                    do (return result)
                  else when (char-equal char separator)
                         do (progn (read-char stream nil nil t)
                                   (when (and echo-space (char-equal
							  (peek-char
							   nil stream
							   nil nil t)
							  #\Space))
                                     (read-char stream nil nil t)))
                  else 
                    collect (accept-element stream) into result)))
      (unless (presentation-typep sequence `(sequence ,type))
        (handle-input-error sequence `(sequence ,type)))
      sequence)))

(define-presentation-method DESCRIBE-PRESENTATION-TYPE :AFTER ((type
								SEQUENCE)
							       stream plural-count &key)
  (declare (ignore plural-count))
  (when type
    (write-string " of which each element is " stream)
    (describe-presentation-type type stream)))

(define-presentation-method PRESENTATION-TYPEP (sequence (type SEQUENCE-ENUMERATED))
  (when (eql (length sequence) (length types))
    (loop for item in sequence
          for item-type in types
          unless (presentation-typep item item-type)
            do (return nil)
          finally (return t))))

(define-presentation-method PRESENT (sequence (type SEQUENCE-ENUMERATED)
					      stream (view TEXTUAL-VIEW)
					      &key) 
  (cond (sequence
         (present (car sequence) (car types) :stream stream :view view)
         (loop for item in (cdr sequence)
               for item-type in (cdr types)
               do (when separator (write-char separator stream))
               do (when echo-space (write-char #\Space stream))
               do (present item item-type :stream stream :view view)))
        (t (present sequence 'null :stream stream sequence view)))
  sequence)

(define-presentation-method ACCEPT ((type SEQUENCE-ENUMERATED) stream
				    (view TEXTUAL-VIEW) &key) 
  (flet ((accept-element (stream type)
           (loop for char = (peek-char nil stream nil nil t)
                 when (or (null char) (char-equal char separator)
			  (member char '(#\tab #\Return #\Linefeed)))
                   do (return (accept-from-string type
						  (coerce result 'string) :view view))
                 else collect char into result
                      and do (read-char stream nil nil t))))
    (declare (inline accept-element))
    (let* ((types-list (copy-list types))
           (sequence
             (loop for char = (peek-char nil stream nil nil t)
                   when (or (null char) (member char '(#\tab #\Return #\Linefeed)))
                     do (return result)
                   else when (char-equal char separator)
                          do (progn (read-char stream nil nil t)
                                    (when (and echo-space (char-equal
							   (peek-char
							    nil stream
							    nil nil t)
							   #\Space))
                                      (read-char stream nil nil t)))
                   else 
                     collect (accept-element stream (pop types-list)) into result)))
      (unless (presentation-typep sequence type)
        (handle-input-error sequence type))
      sequence)))

(define-presentation-method PRESENTATION-TYPEP (sequence (type MIXED-SEQUENCE))
  (consp sequence))

(define-presentation-method PRESENT (sequence (type MIXED-SEQUENCE)
					      stream (view
						      TEXTUAL-VIEW)
					      &key) 
  "sequence may contain any type of elements, good for presenting a nested list of misc objects."
  (flet ((present-item (item)
           (typecase item
             (cons (present item 'mixed-sequence :stream stream :view view))
             (t (present item (presentation-type-of item) :stream stream :view view)))))
    (declare (inline present-item))
    (cond (sequence
           (present-item (car sequence))
           (loop for item in (cdr sequence)
                 do (when separator (write-char separator stream))
                 do (when echo-space (write-char #\Space stream))
                 do (present-item item)))
          (t (present sequence 'w3p:null :stream stream :view view)))
    sequence))

;;; 23.8.7 "META" PRESENTATION TYPES
(define-presentation-method PRESENTATION-TYPEP (object (type OR))
  (some #'(lambda (type) (presentation-typep object type)) types))

(define-presentation-method ACCEPT ((type OR) stream (view TEXTUAL-VIEW)
				    &key default default-type) 
  "since this isn't interactive, need to store input.  catch parse errrors"
  (let ((string (read-line stream nil nil t)))
    (block loop-attemps
      (loop for type in types
            do (block try-type
                 (handler-bind ((presentation-parse-error
				 #'(lambda (condition)
				     (declare (ignore condition))
				     (return-from try-type (values nil nil)))))
                   (with-input-from-string (string-stream string)
					   (with-presentation-type-decoded
					    (type-name parameters options) type
					    (multiple-value-bind 
						(returned-object returned-type)
						(funcall-presentation-generic-function
						 accept type-name parameters options 
						 type string-stream view 
						 :default default :default-type default-type)
					      (return (values
						       returned-object
						       (or returned-type type))))))))
            finally (handle-input-error string type)))))

(define-presentation-method PRESENT (object (type OR) stream
					    (view TEXTUAL-VIEW) &key) 
  (loop for type in types 
        when (presentation-typep object type)
	do (return (present object type :stream stream :view view))
        finally (present object (presentation-type-of object)
			 :stream stream :view view)))

(define-presentation-method PRESENTATION-TYPEP (object (type AND))
  (labels ((verify-type (type)
             (cond ((atom type) (presentation-typep object type))
                   ((eql 'satisfies (first type)) (funcall (second type) object))
                   ((eql 'not (first type)) (not (verify-type (second type))))
                   (t (presentation-typep object type)))))
    (and (presentation-typep object (first types))
         (loop for type in (cdr types)
               unless (verify-type type) return nil
               finally (return t)))))

(define-presentation-method ACCEPT ((type AND) stream (view TEXTUAL-VIEW) &key)
  (let ((value (accept (first types) :stream stream :view view)))
    (unless (presentation-typep value type)
      (handle-input-error value type))))

(define-presentation-method PRESENT (object (type AND) stream
					    (view TEXTUAL-VIEW) &key) 
  (present object (first types) :stream stream :view view)
  (values object type))

;;; 23.8.8 COMPOUND PRESENTATION TYPES
(define-presentation-method PRESENTATION-TYPEP (object (type TOKEN-OR-TYPE))
  (or (presentation-typep object type)
      (when (or (symbolp object) (stringp object))
        (let ((token-string (etypecase object
                              (symbol (symbol-name object))
                              (string object))))
          (member token-string tokens :test #'string-equal :key #'(lambda (item) 
                                                                    (etypecase item 
                                                                      (symbol (symbol-name item)) 
                                                                      (string item))))))))

(define-presentation-method ACCEPT ((type TOKEN-OR-TYPE) stream
				    (view TEXTUAL-VIEW) &key)
  (let ((value-string (read-token stream :parse nil)))
    (cond ((presentation-typep value-string `(token-or-type ,tokens ,type))
           value-string)
          (t (multiple-value-bind (returned-object returned-type)
                 (accept-from-string type value-string :view view)
               (unless returned-type
                 (handle-input-error value-string type))
               returned-object)))))

(define-presentation-method PRESENT (object (type TOKEN-OR-TYPE)
					    stream
					    (view TEXTUAL-VIEW) &key)
  (cond ((presentation-typep object type) (present object type :stream stream :view view))
        ((symbolp object) (write-string (string-upcase (symbol-name object)) stream))
        ((stringp object) (write-string (string-upcase object) stream))))

(define-presentation-method PRESENTATION-TYPEP (object (type NULL-OR-TYPE))
  (or (presentation-typep object type)
      (presentation-typep object 'null)))

(define-presentation-method ACCEPT ((type NULL-OR-TYPE) stream
				    (view TEXTUAL-VIEW) &key)
  (let ((string (read-line stream nil nil t)))
    (cond ((string-equal string "NONE") (values nil 'NULL))
          (t (multiple-value-bind 
               (returned-object returned-type)
                 (block try-type
                   (handler-bind ((presentation-parse-error
                                    #'(lambda (condition)
                                        (declare (ignore condition))
                                        (return-from try-type (values nil nil)))))
                     (with-input-from-string (string-stream string)
                       (with-presentation-type-decoded (type-name parameters options) type
                         (multiple-value-bind 
                           (returned-object returned-type)
                             (funcall-presentation-generic-function accept type-name parameters options 
                                                                    type string-stream view)
                           (values returned-object (or returned-type type)))))))
               (cond (returned-type (values returned-object returned-type))
                     (t (handle-input-error string type))))))))

(define-presentation-method PRESENT (object (type NULL-OR-TYPE) stream
					    (view TEXTUAL-VIEW) &key)
  (cond ((null object) (present object 'null :stream stream :view view))
        (t (present object type :stream stream :view view))))

(define-presentation-method DESCRIBE-PRESENTATION-TYPE ((type NULL-OR-type)
							stream plural-count
							&key) 
  (declare (ignore plural-count))
  (write-string "either " stream)
  (describe-presentation-type type stream)
  (write-string ", or " stream)
  (describe-presentation-type 'null stream))

(define-presentation-method PRESENTATION-TYPEP (object (type TYPE-OR-STRING))
  (or (presentation-typep object type)
      (presentation-typep object 'string)))

(define-presentation-method ACCEPT ((type TYPE-OR-STRING) stream
				    (view TEXTUAL-VIEW) &key)
  (let ((string (read-line stream nil nil t)))
    (multiple-value-bind 
      (returned-object returned-type)
        (block try-type
          (handler-bind ((presentation-parse-error
                           #'(lambda (condition)
                               (declare (ignore condition))
                               (return-from try-type (values nil nil)))))
            (with-input-from-string (string-stream string)
              (with-presentation-type-decoded (type-name parameters options) type
                (multiple-value-bind 
                  (returned-object returned-type)
                    (funcall-presentation-generic-function accept type-name parameters options 
                                                           type string-stream view)
                  (values returned-object (or returned-type type)))))))
      (cond (returned-type (values returned-object returned-type))
            (t (values string 'type-or-string))))))

(define-presentation-method PRESENT (object (type TYPE-OR-STRING)
					    stream
					    (view TEXTUAL-VIEW) &key)
  (cond ((presentation-typep object type) (present object type :stream stream :view view))
        ((stringp object) (write-string object stream))
        (t (write object :stream stream))))

;;; 23.8.9 LISP EXPRESSION PRESENTATION TYPES
(define-presentation-method PRESENTATION-TYPEP (object (type EXPRESSION))
  (declare (ignore object))              
  t)                                  

(define-presentation-method PRESENT (expression (type EXPRESSION)
						stream
						(view TEXTUAL-VIEW) &key acceptably)
  (cond (acceptably (prin1 expression stream))
        (t (princ expression stream)))
  expression)
