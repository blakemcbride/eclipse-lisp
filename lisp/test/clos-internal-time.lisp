(host::locally-fast

(defun table-test ()
  (let ((generic-function (fdefinition 'a1)))
    (dotimes (i *outer-times*)
      (cl:time (cl:dotimes (j *inner-times*)
			   (emf-table generic-function))))))

(defmacro explain (&body body)
  `(locally (declare (:explain :calls)) ,@body))

(defmacro explain-more (&body body)
  `(locally (declare (:explain :calls :types)) ,@body))

(defmacro noexplain (&body body)
  `(locally (declare (:explain :nocalls :notypes)) ,@body))

;;; Allegro can't infer the declaration by itself.
(defmacro mask-part (x &optional (mask host::hash-mask))
  `(the (integer 0 ,mask) (cl:logand ,x ,mask)))

(defun emfun-test ()
  (let ((generic-function (fdefinition 'foo-bar))
	(n 1)
	(args (list 3)))
    (dotimes (i *outer-times*)
      (cl:time (cl:dotimes (j *inner-times*)
			   (let ((table (emf-table generic-function)))
			     (apply-function (emf-table-get table n args) args)))))))
(defun emfun-test1 ()
  (let ((f (standard-discriminating-function (fdefinition 'foo-bar) 1))
	(args (list 3)))
    (dotimes (i *outer-times*)
      (cl:time (cl:dotimes (j *inner-times*)
			   (apply-function f args))))))
)

;(defmethod foo-bar ((x integer)) 0)

;(setq mf (method-function (first (generic-function-methods (fdefinition 'foo-bar)))))

(defun time-foo-mf (f n)
  (let ((args '(3)))
    (cl:time (cl:dotimes (i n) (cl:funcall f args nil)))))
  

(defun clos-internal-time ()
  (cl:format t "~2&EMFUN ") (emfun-test)
  (cl:format t "~2&EMFUN1 ") (emfun-test1))

