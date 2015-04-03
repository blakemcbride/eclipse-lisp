;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 15.1 CONSES                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun TREE-EQUAL (x y &rest keys &key test test-not key)
  (declare (dynamic-extent keys))
  (or (and (consp x) (consp y)
	   (apply #'tree-equal (car x) (car y) keys)
	   (apply #'tree-equal (cdr x) (cdr y) keys))
      (and (null x) (null y))
      (satisfies-the-test x y test test-not key)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 15.2 LISTS                                                   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun COPY-TREE (object)
  (if (consp object)
      (cons (copy-tree (car object))
	    (copy-tree (cdr object)))
    object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 15.4 SUBSTITUTION OF EXPRESSIONS                             ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; See Steele.
(defun subst1 (new old tree test test-not key copy
	       &aux (alist (eq old 'from-alist)))
  (cond ((when alist 
	   (setf alist (assoc (if key (funcall key tree) tree)
			    new :test test :test-not test-not)))
	 (cdr alist))
	((unless alist
	   (satisfies-the-test old tree test test-not key)) new)
	((atom tree) tree)
	(t (let ((car (subst1 new old (car tree) test test-not key copy))
		 (cdr (subst1 new old (cdr tree) test test-not key copy)))
	     (if (and (eq car (car tree))
		      (eq cdr (cdr tree)))
		 tree
	       (cond (copy (cons car cdr))
		     (t (rplaca tree car)
			(rplacd tree cdr)
			tree)))))))
(defun SUBST (new old tree &key test test-not key)
  (subst1 new old tree test test-not key t))

(defun SUBST-IF (new test tree &key key)
  (subst1 new 'none tree test nil key t))

(defun SUBST-IF-NOT (new test tree &key key)
  (subst1 new 'none tree nil test key t))

(defun NSUBST (new old tree &key test test-not key)
  (subst1 new old tree test test-not key nil))

(defun NSUBST-IF (new test tree &key key)
  (subst1 new 'none tree test nil key nil))

(defun NSUBST-IF-NOT (new test tree &key key)
  (subst1 new 'none tree nil test key nil))

(defun SUBLIS (alist tree &key test test-not key)
  (subst1 alist 'from-alist tree test test-not key t))

(defun NSUBLIS (alist tree &key test test-not key)
  (subst1 alist 'from-alist tree test test-not key nil))

