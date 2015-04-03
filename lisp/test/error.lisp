;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 24.1 GENERAL ERROR-SIGNALING FUNCTIONS                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest error-simple
  (handler-case (error "This should not be printed.")
    (simple-error () nil))
  nil)

(deftest error-condition-name
  (handler-case (error 'file-error :pathname "foo.lisp")
    (file-error () nil))
  nil)

(deftest error-condition
  (handler-case (error (make-condition 'file-error :pathname "foo.lisp"))
    (file-error () nil))
  nil)

;;; CERROR
(deftest cerror-condition
  (handler-case (cerror "keep going" (make-condition 'file-error :pathname "foo.lisp"))
    (file-error () nil))
  nil)
(deftest cerror-has-continue-restart
  (handler-bind ((file-error #'(lambda (c)
				 (invoke-restart (find-restart 'continue c)))))
    (cerror "keep going" (make-condition 'file-error :pathname "foo.lisp")))
  nil)

  
;;; WARN
(deftest warn-simple
  (handler-case (warn "This should not be printed.")
    (simple-warning () nil))
  nil)

(deftest warn-condition-name
  (handler-case (warn 'warning)
    (warning () nil))
  nil)

(deftest warn-condition
  (handler-case (warn (make-condition 'warning))
    (warning () nil))
  nil)

(deftest muffled-warn-no-further-signalling
  (handler-case
      (handler-case (warn "foo")
	(simple-warning (c)
	    (let ((r (find-restart 'muffle-warning c)))
	      (when r (invoke-restart r)))))
    (file-error () (error "Should not be here.")))
  nil)

;;; BREAK !!!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 24.2 SPECIALIZED ERROR-SIGNALING FORMS AND MACROS            ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CHECK-TYPE
(deftest check-type
  (let ((x 'foo) returned-value)
    (handler-bind ((type-error (lambda (c) (store-value 42 c))))
      (setq returned-value (check-type x (integer 0 *) "a positive integer")))
    (values x returned-value))
  42 nil)


;;; ASSERT !!!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 24.3 SPECIAL FORMS FOR EXHAUSTIVE CASE ANALYSIS              ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ETYPECASE
(defun etypecase-test (x)
  (let* ((counter 0)
	 (value (etypecase (progn (incf counter) x)
		  (float "a float")
		  (null "a symbol ...")
		  (list "a list"))))
    (and (= counter 1) value)))
(deftest etypecase-nil (etypecase-test nil) "a symbol ...")
(deftest etypecase-list (etypecase-test '(a b)) "a list")
(deftest etypecase-float (etypecase-test 7.0f0) "a float")
(deftest etypecase-condition
  (handler-case (etypecase-test 42)
    (type-error (c) (values (type-error-datum c)
			    (sort (copy-list (rest (type-error-expected-type c)))
				  #'string<
				  :key #'symbol-name))))
  42 (float list null))

;;; CTYPECASE
(defun ctypecase-test (x)
  (let (returned-value)
    (handler-bind ((type-error (lambda (c) (store-value 17.0f0 c))))
      (setq returned-value
	    (ctypecase x
		       (float "a float")
		       (null "a symbol ...")
		       (list "a list"))))
    (values x returned-value)))

(deftest ctypecase-nil (ctypecase-test nil) nil "a symbol ...")
(deftest ctypecase-list (ctypecase-test '(a b)) (a b) "a list")
(deftest ctypecase-float (ctypecase-test 7.0f0) 7.0f0 "a float")
(deftest ctypecase-handled (ctypecase-test 9) 17.0f0 "a float")

;;; ECASE
(defun ecase-test (x)
  (let* ((counter 0)
	 (value (ecase (progn (incf counter) x)
		  (a 1)
		  ((1 2) 2)
		  ((b c) 3))))
    (and (= counter 1) value)))
(deftest ecase-1 (ecase-test 'a) 1)
(deftest ecase-2 (ecase-test 2) 2)
(deftest ecase-condition
  (handler-case (ecase-test 42)
    (type-error (c) (values (type-error-datum c)
			    (sort (copy-list (rest (type-error-expected-type c)))
				  #'string<
				  :key #'(lambda (x)
					   (if (symbolp x)
					       (symbol-name x)
					     (princ-to-string x)))))))
  42 (1 2 a b c))

;;; CCASE
(defun ccase-test (x)
  (let (returned-value)
    (handler-bind ((type-error (lambda (c) (store-value 'b c))))
	(setq returned-value
			(ccase x
			  (a 1)
			  ((1 2) 2)
			  ((b c) 3))))
    (values x returned-value)))
(deftest ccase-1 (ccase-test 'a) a 1)
(deftest ccase-2 (ccase-test 2) 2 2)
(deftest ccase-3 (ccase-test 9) b 3)
