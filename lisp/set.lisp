;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 15.5 USING LISTS AS SETS                                     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
(defun MEMBER-IF (predicate list &key key)
  (member1 'none list predicate nil key))
(defun MEMBER-IF-NOT (predicate list &key key)
  (member1 'none list nil predicate key))

(defun TAILP (sublist list)
  (do ((list list (cdr list)))
      ((atom list) (eql list sublist))
    (when (eql sublist list)
      (return t))))

(macrolet ((def-set-op (name init pred
			&optional second-loop (op '(push elt res)))
	       `(defun ,name (list1 list2 &key test test-not (key #'identity)
			      &aux (res ,init))
		  (dolist (elt list1)
		    (,pred (member1 (funcall key elt) list2 test test-not key)
			   ,op))
		  ,second-loop
		  res)))
  (def-set-op UNION list2 unless)
  (def-set-op INTERSECTION nil when)
  (def-set-op SET-DIFFERENCE nil unless)
  (def-set-op SET-EXCLUSIVE-OR nil unless
    ;; Note pains to ensure proper order of arguments to test/test-not. 
    (dolist (elt list2)
      (dolist (elt1 (the list list1) (push elt res))
	(when (satisfies-the-test (funcall key elt1) elt test test-not key)
	  (return)))))
  (def-set-op SUBSETP t unless nil (return-from subsetp nil)))

(macrolet ((def-set-op (name pred &optional tail)
	       `(defun ,name (list1 list2 &key test test-not
					       (key #'identity)
			      &aux first last)
		  (do ((sub list1 (cdr sub)))
		      ((endp sub)
		       (if last (rplacd last ,tail)
			 (setq first ,tail))
		       first)
		    (,pred (member1 (funcall key (car sub))
				    list2 test test-not key)
			   (if last (rplacd last sub) (setq first sub))
			   (setq last sub))))))
  (def-set-op NUNION unless list2)
  (def-set-op NINTERSECTION when)
  (def-set-op NSET-DIFFERENCE unless))

;;; Based on CMUCL.
;;; The outer loop examines list1 while the inner loop examines list2. If an
;;; element is found in list2 "equal" to the element in list1, both are
;;; spliced out. When the end of list1 is reached, what is left of list2 is
;;; tacked onto what is left of list1.  The splicing operation ensures that
;;; the correct operation is performed depending on whether splice is at the
;;; top of the list or not

(defun NSET-EXCLUSIVE-OR (list1 list2 &key test test-not (key #'identity))
  (do ((list1 list1)
       (list2 list2)
       (x list1 (cdr x))
       (splicex ()))
      ((endp x)
       (if (null splicex)
	   (setq list1 list2)
	   (rplacd splicex list2))
       list1)
    (do ((y list2 (cdr y))
	 (splicey ()))
	((endp y) (setq splicex x))
      (cond ((satisfies-the-test (funcall key (car x))
				 (car y)
				 test test-not key)
	     (if (null splicex)
		 (setq list1 (cdr x))
		 (rplacd splicex (cdr x)))
	     (if (null splicey) 
		 (setq list2 (cdr y))
		 (rplacd splicey (cdr y)))
	     (return ()))			; assume lists are really sets
	    (t (setq splicey y))))))
