;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 10.1 THE PROPERTY LIST                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; GET
(deftest get
    (progn (setf (symbol-plist 'plist-symbol) '(bar t baz 3 hunoz "Huh?"))
	   (values (get 'plist-symbol 'baz)
		   (get 'plist-symbol 'hunoz)
		   (get 'plist-symbol 'zoo)
		   (get 'plist-symbol 'zoo 2)))
 3 "Huh?" nil 2)

(deftest setf-get
    (progn
      (remprop 'clyde 'species)
      (values  (get 'clyde 'species)
	       (setf (get 'clyde 'species) 'elephant)
	       (get 'clyde 'species)))
  nil elephant elephant)

(deftest setf-get-default
    (progn
      (remprop 'get-test 'token-stack)
      (values (push 'next-item
		    (get 'get-test 'token-stack '(initial-item)))
	      (get 'get-test 'token-stack)))
  (next-item initial-item) (next-item initial-item))
      
(deftest setf-get-evals-default
    (let ((effect nil))
      (setf (get 'get-test 'token-stack) '(something))
      (values (push 'else
		    (get 'get-test 'token-stack
			 (setq effect '(default))))
	      (get 'get-test 'token-stack)
	      effect))
  (else something) (else something) (default))

;;; REMPROP
(deftest remprop
    (progn (setf (symbol-plist 'remprop-test)
	     (list 'a 1 'b 2 'c 3 'a 99))
	   (values (null (remprop 'remprop-test 'a))
		   (copy-list (symbol-plist 'remprop-test))
		   (null (remprop 'remprop-test 'b))
		   (null (remprop 'remprop-test 'a))
		   (remprop 'remprop-test 'b)))
  nil (b 2 c 3 a 99) nil nil nil)

;;; SYMBOL-PLIST (see above)

;;; GETF
(deftest GETF
    (let ((x nil) (effect nil))
      (values (getf x 'prop1)
	      (getf x 'prop1 7)
	      (getf x 'prop1)
	      (setf (getf x 'prop1) 'val1)
	      (eq (getf x 'prop1) 'val1)
	      (getf x 'prop1)
	      (getf x 'prop1 (setq effect 7))
	      x
	      effect))
  nil 7 nil val1 t val1 val1 (prop1 val1) 7)

(deftest getf-evals-default
    (let ((plist '()))
      (incf (getf plist 'count 0))
      plist)
  (count 1))

(deftest muliple-setf-getf
    (let ((x nil))
      (setf (getf x 'prop1) 'val1)
      (setf (getf x 'prop1) 'val1)
      x)
  (prop1 val1))
  

;;; REMF
(deftest remf (let ((x (cons nil nil)))
		(setf (getf (car x) 'prop1) 'val1)
		(values (not (remf (car x) 'prop1))
			(remf (car x) 'prop1)))
  nil nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 10.2 THE PRINT-NAME                                          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SYMBOL-NAME
(deftest symbol-name-interned (symbol-name 'temp) "TEMP")
(deftest symbol-name-key (symbol-name :start) "START")
(deftest symbol-name-uninterned-lc (symbol-name (make-symbol "foo")) "foo")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 10.3 CREATING SYMBOLS                                        ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MAKE-SYMBOL
(deftest make-symbol
    (let* ((temp-string "temp")
	   (temp-symbol (make-symbol temp-string)))
      (values
       (not (null (string= temp-string (symbol-name temp-symbol))))
       (cl:find-symbol "temp")
       (eq temp-symbol (make-symbol temp-string))
       (symbol-plist temp-symbol)
       (boundp temp-symbol)
       (fboundp temp-symbol))) 
  t nil nil nil nil nil)
	
;;; COPY-SYMBOL
(deftest copy-symbol
    (let ((fred 'fred-smith)
	  (plist '(a b)))
      (setf (symbol-value fred) 3)
      (setf (symbol-function fred) #'(lambda (x) x))
      (setf (symbol-plist fred) plist)
      (let ((fred-clone-1a (copy-symbol fred))
	    (fred-clone-1b (copy-symbol fred nil))
	    (fred-clone-2a (copy-symbol fred t))
	    (fred-clone-2b (copy-symbol fred t)))
	(values (eq fred fred-clone-1a)
		(eq fred-clone-1a fred-clone-1b)
		(eq fred-clone-2a fred-clone-2b)
		(eq fred-clone-1a fred-clone-2a)
		(symbol-value fred)
		(boundp fred-clone-1a)
		(and (boundp fred-clone-2a) (symbol-value fred-clone-2a))
		(setf (symbol-value fred-clone-2a) 4)
		(symbol-value fred)
		(symbol-value fred-clone-2a)
		(and (boundp fred-clone-2b) (symbol-value fred-clone-2b))
		(boundp fred-clone-1a)
		(fboundp fred-clone-1a)
		(and (fboundp fred-clone-2a) (funcall fred-clone-2a 9))
		(symbol-plist fred-clone-1a)
		(symbol-plist fred-clone-2a)
		(eq (symbol-plist fred-clone-2a) plist)
		)))
  nil nil nil nil 3 nil 3 4 3 4 3 nil nil 9 nil (a b) nil)

;;; GENSYM
(deftest gensym
    (let ((*gensym-counter* 50))
      (let ((sym1 (gensym))
	    (sym2 (gensym 100))
	    (sym3 (gensym 100))
	    (sym4 (gensym "T")))
	(values (symbol-name sym1)
		(symbol-name sym2)
		(symbol-package sym1)
		(eq sym2 sym3)
		(symbol-name sym4))))
  "G50" "G100" nil nil "T51")

;;; GENTEMP
(deftest gentemp
    (let* ((a (gentemp))
	   (b (gentemp "FOO"))
	   (next (concatenate 'string "FOO"
			      (princ-to-string
			       (1+ (parse-integer (symbol-name b) :start 3)))))
	   (found (find-symbol next))
	   (c (gentemp "FOO"))
	   (found2 (not (find-symbol next)))
	   (d (gentemp)))
      (values found found2 (eq c b) (eq d a) (char (symbol-name d) 0)))
  nil nil nil nil #\T)
(deftest gentemp-pkg (eq (symbol-package (gentemp "foo" :user))
			 (find-package "USER"))
  t)

;;; KEYWORDP
(deftest keywordp-t
    (keywordp (intern "START" (find-package :keyword))) t)
(deftest keywordp-nil
    (keywordp (intern "START" (find-package :user)))
  nil)