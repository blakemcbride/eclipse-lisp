(defun default-name-key (arg)
  (typecase arg
    (string arg)
    (null "NIL")
    (cons (string (car arg)))
    (symbol (symbol-name arg))
    (t (princ-to-string arg))))

(defun default-alist-key (item)
  (typecase item
    (atom item)
    (t (cdr item))))

(defun never (&rest args)
  (declare (ignore args))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 23.8 STANDARD PRESENTATION TYPES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 23.8.2 BASIC PRESENTATION TYPES
(define-presentation-type T ())

(define-presentation-type BOOLEAN () 
  :inherit-from t)

;; BLANK-AREA, *NULL-PRESENTATION*!!!

(define-presentation-type SYMBOL () 
  :inherit-from t)

(define-presentation-type NULL () 
  :inherit-from 'symbol)

(define-presentation-type KEYWORD () 
  :inherit-from 'symbol)


;;; 23.8.2 NUMERIC PRESENTATION TYPES
(define-presentation-type NUMBER () 
  :inherit-from t)

(define-presentation-type COMPLEX (&optional type) 
  :inherit-from 'number)

(define-presentation-type REAL (&optional low high)
  :inherit-from 'number 
  :options
  ((base 10) 
   (radix nil)))

(define-presentation-type RATIONAL (&optional low high) 
  :inherit-from `((real ,low ,high) 
		  :base ,base 
		  :radix ,radix)
  :options
  ((base 10) 
   (radix nil)))

(define-presentation-type INTEGER (&optional low high) 
  :inherit-from `((rational ,low ,high) 
                  :base ,base 
                  :radix ,radix)
  :options
  ((base 10) 
   (radix nil)))

(define-presentation-type fixnum (&optional (low most-negative-fixnum)
					    (high most-positive-fixnum)) 
  :inherit-from `((integer ,low ,high) 
                  :base ,base 
                  :radix ,radix)
  :options
  ((base 10) 
   (radix nil)))

(define-presentation-type RATIO (&optional low high) 
  :inherit-from `((rational ,low ,high) 
                  :base ,base 
                  :radix ,radix)
  :options
  ((base 10) 
   (radix nil)))

(define-presentation-type FLOAT (&optional low high)
  :inherit-from `((real ,low ,high) 
                  :base ,base 
                  :radix ,radix)
  :options
  ((base 10) 
   (radix nil)))

(define-presentation-type single-float 
                          (&optional (low most-negative-single-float)
				     (high most-positive-double-float)) 
  :inherit-from `((float ,low ,high) 
                  :base ,base 
                  :radix ,radix)
  :options
  ((base 10) 
   (radix nil)))

(define-presentation-type double-float 
                          (&optional (low most-negative-double-float)
				     (high most-positive-double-float)) 
  :inherit-from `((float ,low ,high) 
                  :base ,base 
                  :radix ,radix)
  :options
  ((base 10) 
   (radix nil)))


;;; 23.8.3 CHARACTER AND STRING PRESENTATION TYPES
(define-presentation-type CHARACTER () 
  :inherit-from t)

(define-presentation-type basic-string () 
  :inherit-from t
  :description " a basic, unbounded string")

(define-presentation-type STRING (&optional length) 
  :inherit-from 'basic-string)

(define-presentation-type bounded-string (&optional max-length) 
  :inherit-from 'basic-string 
  :description "a string that can be parameterized by a maximum length.")


;;; 23.8.4 PATHNAME PRESENTATION TYPE
(define-presentation-type PATHNAME () 
  :inherit-from t 
  :options
  (default-type
    (default-version :newest) 
    (merge-default t)))

(define-presentation-type existing-pathname () 
  :inherit-from 'pathname
  :options
  (default-type
    (default-version :newest) 
    (merge-default t)))


;;; 23.8.5 "ONE-OF" AND SOME-OF" PRESENTATION TYPES
(define-presentation-type COMPLETION (&optional (sequence nil)
						&key (test #'eql)
						(value-key #'identity)) 
  :inherit-from t
  :options
  ((name-key #'default-name-key) 
   (documentation-key #'never)
   (partial-completers '(#\Space))))

(define-presentation-type MEMBER (&rest elements) 
  :inherit-from `((completion ,elements) 
                  :name-key ,name-key 
                  :documentation-key ,documentation-key 
                  :partial-completers ,partial-completers)
  :options
  ((name-key #'default-name-key)
   (documentation-key #'never)
   (partial-completers '(#\Space))))

(define-presentation-type MEMBER-SEQUENCE (sequence &key (test #'eql)) 
  :inherit-from `((completion ,sequence :test ,test) 
                  :name-key ,name-key 
                  :documentation-key ,documentation-key 
                  :partial-completers ,partial-completers)
  :options
  ((name-key #'default-name-key)
   (documentation-key #'never)
   (partial-completers '(#\Space))))

(define-presentation-type MEMBER-ALIST (alist &key (test #'eql)
					      (value-key #'default-alist-key))
  :inherit-from `((completion ,alist :test ,test :value-key ,value-key) 
                  :name-key ,name-key 
                  :documentation-key ,documentation-key 
                  :partial-completers ,partial-completers)
  :options
  ((name-key #'default-name-key)
   (documentation-key #'never)
   (partial-completers '(#\Space))))

(define-presentation-type SUBSET-COMPLETION (&optional (sequence nil) &key (test #'eql)
						       (value-key #'identity))
  :inherit-from t 
  :options
  ((name-key #'default-name-key)
   (documentation-key #'never)
   (partial-completers '(#\Space))
   (separator #\,) 
   (echo-space t)))

(define-presentation-type SUBSET (&rest elements) 
  :inherit-from `((subset-completion ,elements)
                  :name-key ,name-key
                  :documentation-key ,documentation-key
                  :partial-completers ,partial-completers
                  :separator ,separator
                  :echo-space ,echo-space)
  :options
  ((name-key #'default-name-key)
   (documentation-key #'never)
   (partial-completers '(#\Space))
   (separator #\,) 
   (echo-space t)))

(define-presentation-type SUBSET-SEQUENCE (sequence &key (test #'eql)) 
  :inherit-from `((subset-completion ,elements :test ,test)
                  :name-key ,name-key
                  :documentation-key ,documentation-key
                  :partial-completers ,partial-completers
                  :separator ,separator
                  :echo-space ,echo-space)
  :options
  ((name-key #'default-name-key)
   (documentation-key #'never)
   (partial-completers '(#\Space))
   (separator #\,) 
   (echo-space t)))

(define-presentation-type SUBSET-ALIST (alist &key (test #'eql)
					      (value-key #'default-alist-key))
  :inherit-from `((subset-completion ,elements :test ,test :value-key ,value-key)
                  :name-key ,name-key
                  :documentation-key ,documentation-key
                  :partial-completers ,partial-completers
                  :separator ,separator
                  :echo-space ,echo-space)
  :options
  ((name-key #'default-name-key)
   (documentation-key #'never)
   (partial-completers '(#\Space))
   (separator #\,)
   (echo-space t)))

;;; 23.8.6 SEQUENCE PRESENTATION TYPES
(define-presentation-type SEQUENCE (type) 
  :inherit-from t 
  :options
  ((separator #\,) 
   (echo-space t)))

(define-presentation-type SEQUENCE-ENUMERATED (&rest types) 
  :inherit-from t 
  :options
  ((separator #\,) 
   (echo-space t)))

(define-presentation-type mixed-sequence () 
  :inherit-from t 
  :options
  ((separator #\,) 
   (echo-space t)) 
  :description
  "A sequence of atoms of mixed type, as well as other mixed-sequences.")

;;; 23.8.7 "META" PRESENTATION TYPES
(define-presentation-type OR (&rest types) 
  :inherit-from t)

(define-presentation-type AND (&rest types) 
  :inherit-from t)


;;; 23.8.8 COMPOUND PRESENTATION TYPES
(define-presentation-type TOKEN-OR-TYPE (tokens type) 
  :inherit-from t)

(define-presentation-type NULL-OR-TYPE (type) 
  :inherit-from t)

(define-presentation-type TYPE-OR-STRING (type) 
  :inherit-from t)

;;; 23.8.9 LISP EXPRESSION PRESENTATION TYPES
(define-presentation-type EXPRESSION () 
  :inherit-from t
  :options (auto-activate))

(define-presentation-type FORM () 
  :inherit-from 'expression)
