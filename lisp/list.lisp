;;; We might want to rewrite some of these to use the stream collector
;;; utilties!!!  

;;; Should use key-item instead of (key #'identity)/(funcall key
;;; elt1), or better yet, incorporate double keying into
;;; satisfies-the-test!!!

;;; CONS, CAR, CDR, RPLACA, RPLACD are are primitive

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 15.1 CONSES                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun CAAR (list) (car (car list)))
(defun CADR (list) (car (cdr list)))
(defun CDAR (list) (cdr (car list)))
(defun CDDR (list) (cdr (cdr list)))
(defun CAAAR (list) (car (car (car list))))
(defun CAADR (list) (car (car (cdr list))))
(defun CADAR (list) (car (cdr (car list))))
(defun CADDR (list) (car (cdr (cdr list))))
(defun CDAAR (list) (cdr (car (car list))))
(defun CDADR (list) (cdr (car (cdr list))))
(defun CDDAR (list) (cdr (cdr (car list))))
(defun CDDDR (list) (cdr (cdr (cdr list))))
(defun CAAAAR (list) (car (car (car (car list)))))
(defun CAAADR (list) (car (car (car (cdr list)))))
(defun CAADAR (list) (car (car (cdr (car list)))))
(defun CAADDR (list) (car (car (cdr (cdr list)))))
(defun CADAAR (list) (car (cdr (car (car list)))))
(defun CADADR (list) (car (cdr (car (cdr list)))))
(defun CADDAR (list) (car (cdr (cdr (car list)))))
(defun CADDDR (list) (car (cdr (cdr (cdr list)))))
(defun CDAAAR (list) (cdr (car (car (car list)))))
(defun CDAADR (list) (cdr (car (car (cdr list)))))
(defun CDADAR (list) (cdr (car (cdr (car list)))))
(defun CDADDR (list) (cdr (car (cdr (cdr list)))))
(defun CDDAAR (list) (cdr (cdr (car (car list)))))
(defun CDDADR (list) (cdr (cdr (car (cdr list)))))
(defun CDDDAR (list) (cdr (cdr (cdr (car list)))))
(defun CDDDDR (list) (cdr (cdr (cdr (cdr list)))))
  
;;; CONS is embedded.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 15.2 LISTS                                                   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; See Steele.
(defun LIST-LENGTH (list)
  (do ((n 0 (+ n 2))
       (fast list (cddr fast))
       (slow list (cdr slow)))
      (nil)
    (when (endp fast) (return n))
    (when (endp (cdr fast)) (return (+ n 1)))
    (when (and (eq fast slow) (> n 0)) (return nil))))


(defun NTH (n list) (car (nthcdr n list)))
(defun FIRST (list) (car list))
(defun SECOND (list) (cadr list))
(defun THIRD (list) (caddr list))
(defun FOURTH (list) (cadddr list))
(defun FIFTH (list) (car (cddddr list)))
(defun SIXTH (list) (cadr (cddddr list)))
(defun SEVENTH (list) (caddr (cddddr list)))
(defun EIGHTH (list) (cadddr (cddddr list)))
(defun NINTH (list) (car (cddddr (cddddr list))))
(defun TENTH (list) (cadr (cddddr (cddddr list))))
(defun REST (list) (cdr list))

(macrolet ((nth-op (n list op)
	     `(dotimes (x ,n ,list)
		(declare (type fixnum x))
		(if (endp list)
		    (return nil)
		  (setf list (,op list))))))
  (defun NTHCDR (n list) (nth-op n list cdr))
  (defun nthcar (n list) (nth-op n list car)))

;; Avoids arithmetic
(defun last1 (list)
  (if (consp list)
      (do ((l list next)
	   (next (cdr list) (cdr next)))
	  ((atom next) l))
      list))

(defun LAST (list &optional n)
  (if n
      (do ((l list (cdr l))
	   (r list)
	   (i 0 (1+ i)))
	  ((atom l) r)
	(when (>= i n) (pop r)))
      (last1 list)))

(defun LIST (&rest args) args)
(defun LIST* (arg &rest others)		;This definition does not use APPLY.
  (cond ((atom others) arg)
	((atom (cdr others)) (cons arg (car others)))
	(t (do ((x others (cdr x)))
	       ((null (cddr x)) (rplacd x (cadr x))))
	   (cons arg others))))

(defun MAKE-LIST (size &key initial-element &aux (list nil))
  (dotimes (n size list)
    (push initial-element list)))

;;; *** IWBNI we didn't have to call last on results we may have
;;; just traversed.  This can be cleaned up!!!
;;; In partcilar, it ought to be a macro and not use funcall.
(defun conc (list processf &optional last-processf
	     &aux new newlist last)
  (flet ((pf (more?) (if (and last-processf (not more?))
			 last-processf
		       processf)))
    (when list
      (setf newlist (funcall (pf (cdr list)) (pop list))
	    last (last newlist))
      (do ((sub list (cdr sub)))
	  ((atom sub)
	   (when sub (setf (cdr (last last)) sub))
	   newlist)
	(setf new (funcall (pf (cdr sub)) (car sub))
	      (cdr (last last)) new)))))

(macrolet ((pop-nulls (list)
	     `(loop (when (or (car ,list) (null ,list))  (return))
		(setf ,list (cdr ,list)))))
  (defun APPEND (&rest lists)
    (pop-nulls lists)
    (conc lists #'copy-list #'identity))
  (defun NCONC (&rest lists)
    (pop-nulls lists)
    (conc lists #'identity)))

(defun COPY-LIST (list)
  (conc list #'list))

(defun REVAPPEND (x y)
  (dolist (item x y) (push item y)))

(defun NRECONC (x y)
  (do (cons)
      ((not (consp x)) y)
    (setf cons x
	  x (cdr x)
	  (cdr cons) y
	  y cons)))


;;; IWBNI this didn't make two traversals of the list.
(defun BUTLAST (list &optional (n 1))
  (subseq list 0 (max 0 (- (length list) n))))

;;; IWBNI this didn't make two traversals of the list.
(defun NBUTLAST (list &optional (n 1))
  (let ((size (- (length list) n)))
    (when (plusp size)
      (rplacd (nthcdr (1- size) list) nil)
      list)))

(defun LDIFF (list sublist)
  (do* ((list list (cdr list))
	(result (list ()))
	(splice result))
      ((or (null list) (eq list sublist)) (cdr result))
    (setq splice (cdr (rplacd splice (list (car list)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 15.3 ALTERATION OF LIST STRUCTURE                            ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; RPLACA, RPLACD are embedded.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 15.5 USING LISTS AS SETS                                     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
(defun member1 (item list test test-not key)
  (do ((list list (cdr list)))
      ((endp list) nil)
    (when (satisfies-the-test item (car list) test test-not key)
      (return list))))

(defun MEMBER (item list &key test test-not key)
  (member1 item list test test-not key))

;;; Note that key is applied to item!
(defun ADJOIN (item list &key test test-not (key #'identity))
  (if (member1 (funcall key item) list test test-not key)
      list
    (cons item list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 15.6 ASSOCIATION LISTS                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defassoc assoc1 car)
(defun ASSOC (item a-list &key test test-not key)
  (assoc1 item a-list test test-not key))
