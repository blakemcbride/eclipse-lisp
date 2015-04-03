;;; Temporary stuff

#|
(defun xxfind-package (name)
  (if (cl:packagep name) name
      (cl:find-package (host::string name))))

(defun xxintern (s &optional (p *package*))
  (let ((clp (xxfind-package p)))
    (cl:intern (host::string s) clp)))
    
(defun xxfind-symbol (s &optional (p *package*))
  (let ((clp (xxfind-package p)))
    (cl:find-symbol (host::string s) clp)))
|#
    
(defun enclose (lambda-form &optional env)
  (declare (ignore env))  
  (cl:coerce lambda-form 'cl:function))



#+defined-in-reader
(defun whitespace-char-p (char)
  (member char '(#\space #\newline #\tab #\return #\page)))
