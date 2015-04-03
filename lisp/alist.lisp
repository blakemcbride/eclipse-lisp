;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 15.2 LISTS                                                   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun COPY-ALIST (list)
  (conc list #'(lambda (new)
		 (cons (if (consp new)
			   (cons (car new)
				 (cdr new))
			   new)
		       nil))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 15.6 ASSOCIATION LISTS                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ACONS (key datum alist)
  (cons (cons key datum) alist))

(defun PAIRLIS (keys data &optional (alist '()))
  (do ((x keys (cdr x))
       (y data (cdr y)))
      ((or (endp x) (endp y))
       (if (or x y)
	   (error "Unmatched keys or data.")
	 alist))
    (setq alist (acons (car x) (car y) alist))))

(defassoc rassoc1 cdr)

(defun ASSOC-IF (predicate a-list &key key)
  (assoc1 'none a-list predicate nil key))
(defun ASSOC-IF-NOT (predicate a-list &key key)
  (assoc1 'none a-list nil predicate key))
(defun RASSOC (item a-list &key test test-not key)
  (rassoc1 item a-list test test-not key))
(defun RASSOC-IF (predicate a-list &key key)
  (rassoc1 'none a-list predicate nil key))
(defun RASSOC-IF-NOT (predicate a-list &key key)
  (rassoc1 'none a-list nil predicate key))

