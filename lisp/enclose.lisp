(defun ENCLOSE (lambda &optional env name)
  (let ((closure (make-instance 'interpreted-function
				:lambda lambda :env env :name name)))
    (declare (notinline make-interpreter))
    ;;#+machine-compile ???!!!
    (when (fboundp 'make-interpreter)
      (set-funcallable-instance-function
       closure (funcall (symbol-function 'make-interpreter) closure)))
    closure))
