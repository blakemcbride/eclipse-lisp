;;; Non-clos versions of sequence functions that are used within CLOS
;;; when defining or using methods.

;;; Used by congruent-lambda-p
(defun find-list-eq-end (item list end &optional key)
  (loop for elt in list
	repeat (or end most-positive-fixnum)
	when (eq item (key-item key elt)) return item))

;;; Used by compute-standard-effective-method-function 
(defun nreverse-list (sequence)
  (do ((tail (cdr sequence) (cdr tail))
       (new nil sub)
       (sub sequence tail))
      ((atom sub) new)
    (rplacd sub new)))


;;; SORT is used by SORT-APPLICABLE-METHODS
;;; Quicksorting lists.
(defun compare (x y pred key)
  (funcall pred (key-item key x) (key-item key y)))

;;; All the conses get recycled!
;;; See "A Linear Logic Quicksort" by H.G. Baker.
(defun highlow (list item high low pred key)
  (if list
      (destructuring-bind (first . rest) list
	(setf (car list) first)
	(if (compare first item pred key)
	    (highlow rest item high (rplacd list low) pred key)
	  (highlow rest item (rplacd list high) low pred key)))
    (values high low)))

(defun quicksort-list (list sorted pred key)
  (if list
      (destructuring-bind (item . unsorted) list
	(multiple-value-bind (high low)
	    (highlow unsorted item nil nil pred key)
	  (setf (car list) item
		(cdr list) (quicksort-list high sorted pred key))
	  (quicksort-list low list pred key)))
      sorted))
