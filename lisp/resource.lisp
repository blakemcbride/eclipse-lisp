(defun extract-parameter-names (lambda-list)
  (loop for parameter in lambda-list
	for var = (unless (lambda-list-keyword-p parameter)
		    (bindingform-name parameter))
	for suppliedp = (when (consp parameter) (third parameter))
	when var
	collect (if (consp var)	(second var) var) into parameters
	when suppliedp collect suppliedp into parameters
	finally (return parameters)))


(defun find-resource (name &optional (errorp t))
  (or (get name 'resource)
      (when errorp
	(error "No resource named ~s." name))))

(defstruct (resource
	    ;; Redundant, but it keeps us from calling MAKE-NAME
	    ;; before FORMAT is ready to build anything.
	    (:conc-name resource-)
	    (:predicate nil)
	    (:copier nil))
  name
  constructor
  initializer
  deinitializer
  matcher
  pool
  in-use
  parameters)

(defun identity-rest (x &rest ignore)
  (declare (ignore ignore) (dynamic-extent ignore))
  x)

(defun get-resource-parameters (object resource)
  (cdr (assoc object (resource-parameters resource))))

(defun default-resource-matcher (object resource &rest parameters)
  (equal (get-resource-parameters object resource)
	 parameters))

(defun Allocate-Resource (name &rest parameters)
  (let* ((resource (find-resource name))
	 (matcher (resource-matcher resource))
	 (object
	  (dolist (item (resource-pool resource)
			(apply (resource-constructor resource)
			       resource parameters))
	    (when (apply matcher item resource parameters)
	      (setf (resource-pool resource)
		    (delete item (resource-pool resource)))
	      (return item)))))
    (apply (resource-initializer resource) object parameters)
    (push object (resource-in-use resource))
    object))

(defun Deallocate-Resource (name object)
  (let ((resource (find-resource name)))
    (funcall (resource-deinitializer resource) object resource)
    (setf (resource-in-use resource)
	  (delete object (resource-in-use resource)))
    (push object (resource-pool resource)))
  object)

(defun Clear-Resource (name)
  (let ((resource (find-resource name)))
    (setf (resource-pool resource) nil)
    (setf (resource-in-use resource) nil)
    (setf (resource-parameters resource) nil)
    name))

(defun Map-Resource (function name)
  (let ((resource (find-resource name)))
    (dolist (item (resource-pool resource))
      (funcall function item nil name))
    (dolist (item (resource-in-use resource))
      (funcall function item t name))))


