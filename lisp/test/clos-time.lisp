(cl:declaim (optimize (speed 1) (safety 1) (debug 1) (space 1)))

(defun fun (x) (declare (ignorable x)) 0)
(defun fun2 (x y) (declare (ignorable x y)) 0)
(defun apply-f (f) (apply f 1 2 nil))
(defun funcall-f (f) (funcall f 1 2))

(defclass c1 ()
  ((x :initform 0
      :accessor a1
      :accessor a2
      :accessor a3
      :accessor o1)))
(defclass c2 (c1)
  ())
(defclass c3 (c1)
  ())

(defmethod g1 ((f c1)) 0)
(defmethod g2 ((f c1)) 0)
(defmethod g2 ((b c2)) 0)

(defmethod o1 ((f c2)) 1)
(defmethod m1 ((a c1) (b c2)) 0)
(defmethod m1 ((b c2) (a c1)) 1)

(defvar *outer-times* 3)
(defvar *inner-times* 10000)
(defmacro clos-time-test (&body body)
  `(let ((i1 (make-instance 'c1))
	 (i2 (make-instance 'c2))
	 (i3 (make-instance 'c3)))
     (declare (ignorable i1 i2 i3))
     (dotimes (i *outer-times*)
       (cl:time (cl:dotimes (j *inner-times*) ,@body)))))

(defun fun-test () (clos-time-test (fun i1)))
(defun apply-test () (clos-time-test (apply-f #'fun2)))
(defun funcall-test () (clos-time-test (funcall-f #'fun2)))
(defun a1-test () (clos-time-test (a1 i1)))
(defun a2-test () (clos-time-test (a2 i2)
				  (a2 i2)))
(defun a3-test () (clos-time-test (a3 i2)
				  (a3 i2)
				  (a3 i3)))
(defun o1-test () (clos-time-test (o1 i1)
				  (o1 i2)))

(defun g1-test () (clos-time-test (g1 i1)))
(defun g2-test () (clos-time-test (g2 i2)
				  (g2 i2)))

(defun m1-test () (clos-time-test (m1 i1 i2)
				  (m1 i2 i1)))

(defun clos-time ()
  (cl:format t "~2&FUN ") (fun-test)
  (cl:format t "~2&APPLY ") (apply-test)
  (cl:format t "~2&FUNCALL ") (funcall-test)
  (cl:format t "~2&A1 ") (a1-test)
  (cl:format t "~2&A2 ") (a2-test)
  (cl:format t "~2&A3 ") (a3-test)
  (cl:format t "~2&O1 ") (o1-test)
  (cl:format t "~2&G1 ") (g1-test)
  (cl:format t "~2&G2 ") (g2-test)
  (cl:format t "~2&M1 ") (m1-test))
  
  