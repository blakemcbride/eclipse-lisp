(in-package :eclipse)

(defun my-function (required-arg &optional (optional-arg 1))
  (let ((c (some-function required-arg))
	(d (some-function optional-arg)))
    (if c
	a
	(throw d optional-arg))))