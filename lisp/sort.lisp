;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 14.5 SORTING AND MERGING                                     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Stable merges: list-merge is destructive (i.e. non-consing)
;;; Note that the order of arguments to the predicate is in violation
;;; of the ANSI.  The spec is wrong and should be changed.

;;; Handles list or vector sequences.  
(defmethod vector-merge (result-vector sequence1 sequence2 pred key)
  (loop with elt1 = nil and elt2 = nil
	with listp1 = (listp sequence1)
	and listp2 = (listp sequence2)

	with state1 = (if listp1 sequence1 0)
	and state2 = (if listp2 sequence2 0)

	with end1 = (if listp1 nil (length sequence1))
	and end2 = (if listp2 nil (length sequence2))

	for index from 0 below (length result-vector)

	if (or (eql state1 end1)
	       (unless (eql state2 end2)
		 (compare (setq elt2 (if listp2 (car state2)
					 (elt sequence2 state2)))
			  (setq elt1 (if listp1 (car state1)
					 (elt sequence1 state1)))
			  pred key)))
	do (setf (elt result-vector index) elt2
		 state2 (if listp2 (cdr state2) (1+ state2)) )
	else
	do (setf (elt result-vector index) elt1
		 state1 (if listp1 (cdr state1) (1+ state1))))
  result-vector)
	

;;; Second value is used by list-mergesort.
(defun list-merge (list1 list2 pred key)
  (do* ((result (cons nil nil))
	(splice result))
      ((or (null list1) (null list2))
       (rplacd splice (or list1 list2))
       (values (cdr result)
	       (last splice)))
    (let ((lesser-list (if (compare (first list2)
				    (first list1)
				    pred key)
			   (prog1 list2 (pop list2))
			 (prog1 list1 (pop list1)))))
      (setq splice
	(setf (cdr splice) lesser-list)))))

  

(defun MERGE (result-type sequence1 sequence2 predicate &key key)
  (let* ((type (expand-type result-type))
	 (stype (canonicalize-sequence-type-specifier type)))
    (check-typef
     (if (consp stype)
	 (vector-merge (make-vector (second stype)
				    (+ (length sequence1) (length sequence2)))
		       sequence1 sequence2 predicate key)
	 ;; No reason not to coerce to list, since we'll need the cons
	 ;; cells anyway. 
	 (list-merge (coerce-to-list sequence1)
		     (coerce-to-list sequence2)
		     predicate key))
     type)))

;;; Bottom-up merge sort ala Sedgewick. 
(defun list-mergesort (list pred key)
  (do ((head (cons :header list))
       (n 1 (* 2 n))                       
       unsorted		    
       list1 list2 temp
       last)
      ((eq list1 (cdr head)) list1)
    (setf unsorted (cdr head))
    (setf last head)
    (loop
     (let ((n-1 (1- n)))
       (setf list1 unsorted)
       (setq temp (nthcdr n-1 list1))
       (cond (temp
	      (setf list2 (cdr temp))
	      (setf (cdr temp) nil)
	      (setf temp (nthcdr n-1 list2))
	      (cond (temp
		     (setf unsorted (cdr temp))
		     (setf (cdr temp) nil))
		    (t (setf unsorted nil)))
	      (multiple-value-bind (merged-head merged-last)
		  (list-merge list1 list2 pred key)
		(setf (cdr last) merged-head)
		(setf last merged-last))
	      (when (null unsorted) (return)))
	     (t  (setf (cdr last) list1)
		 (return)))))))

(defun STABLE-SORT (sequence predicate &key key)
  (if (listp sequence)
      (list-mergesort sequence predicate key)
    ;; We still want to use mergesort for vectors, since its the
    ;; fastest stable sort.  Unfortunately, merge sort of vectors is a
    ;; real pain, so we cons up a list to work with.
    (replace sequence
	     (list-mergesort (coerce-to-list sequence)
			     predicate key))))

;;; An alternative for stable vector sort might be something like the
;;; following, but in-place-vector-merge would be difficult (swapping???)
;;; and might require a stack anyway, so we might as well use the
;;; technique above. 
#|(defun vector-mergesort1 (v left right pred key)
  (if (< left right)
      (let ((i (truncate (+ left right) 2)))
	(vector-mergesort1 v left i pred key)
	(vector-mergesort v (1+ i) right pred key)
	(in-place-vector-merge v left right i pred key))
    v))|#


;;; Quicksorting vectors.
;;; Baase style two-pointer split loop (see Baker).
(defun split1 (v i j x pred key)		;hole is at v(i)
  (if (= i j) i			
    (let ((vj (aref v j)))			;Copy elt to vj
      (cond ((compare vj x pred key)
	     (setf (aref v i) vj)
	     (split2 v (1+ i) j x pred key))
	    (t (split1 v i (1- j) x pred key))))))

(defun split2 (v i j x pred key)		;hole is at v(j)
  (if (= i j) i
    (let ((vi (aref v i)))			;Copy elt to vi
      (cond ((not (compare vi x pred key))
	     (setf (aref v j) vi)
	     (split1 v i (1- j) x pred key))
	    (t (split2 v (1+ i) j x pred key))))))
	  

(defun quicksort-vector (v left right pred key)
  (if (< left right)
      (let* ((x (aref v left))	                ;Create a hole at left
	     (i (split1 v left (1- right) x
			pred key)))             ;Do partition
	(setf (aref v i) x)	                ;Put x back into hole
	(quicksort-vector v left i pred key)
	(quicksort-vector v (1+ i) right pred key))
    v))

(defun SORT (sequence predicate &key key)
  (if (listp sequence)
      (quicksort-list sequence nil predicate key)
      (quicksort-vector sequence 0 (length sequence) predicate key)))
