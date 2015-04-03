;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 15.1 CONSES                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CAR, CDR, etc.
(defparameter cr-null 'nil)
(defparameter cr-tree '((((1 . 2) 3 . 4) (5 . 6) 7 . 8) ((9 . 10) 11 . 12) (13 . 14) 15 . 16))

(deftest car-null (car cr-null) nil)
(deftest cdr-null (cdr cr-null) nil)
(deftest caar-null (caar cr-null) nil)
(deftest cadr-null (cadr cr-null) nil)
(deftest cdar-null (cdar cr-null) nil)
(deftest cddr-null (cddr cr-null) nil)
(deftest caaar-null (caaar cr-null) nil)
(deftest caadr-null (caadr cr-null) nil)
(deftest cadar-null (cadar cr-null) nil)
(deftest caddr-null (caddr cr-null) nil)
(deftest cdaar-null (cdaar cr-null) nil)
(deftest cdadr-null (cdadr cr-null) nil)
(deftest cddar-null (cddar cr-null) nil)
(deftest cdddr-null (cdddr cr-null) nil)
(deftest caaaar-null (caaaar cr-null) nil)
(deftest caaadr-null (caaadr cr-null) nil)
(deftest caadar-null (caadar cr-null) nil)
(deftest caaddr-null (caaddr cr-null) nil)
(deftest cadaar-null (cadaar cr-null) nil)
(deftest cadadr-null (cadadr cr-null) nil)
(deftest caddar-null (caddar cr-null) nil)
(deftest cadddr-null (cadddr cr-null) nil)
(deftest cdaaar-null (cdaaar cr-null) nil)
(deftest cdaadr-null (cdaadr cr-null) nil)
(deftest cdadar-null (cdadar cr-null) nil)
(deftest cdaddr-null (cdaddr cr-null) nil)
(deftest cddaar-null (cddaar cr-null) nil)
(deftest cddadr-null (cddadr cr-null) nil)
(deftest cdddar-null (cdddar cr-null) nil)
(deftest cddddr-null (cddddr cr-null) nil)

(deftest car-tree (car cr-tree) (((1 . 2) 3 . 4) (5 . 6) 7 . 8)) 
(deftest cdr-tree (cdr cr-tree) (((9 . 10) 11 . 12) (13 . 14) 15 . 16)) 
(deftest caar-tree (caar cr-tree) ((1 . 2) 3 . 4)) 
(deftest cadr-tree (cadr cr-tree) ((9 . 10) 11 . 12)) 
(deftest cdar-tree (cdar cr-tree) ((5 . 6) 7 . 8)) 
(deftest cddr-tree (cddr cr-tree) ((13 . 14) 15 . 16)) 
(deftest caaar-tree (caaar cr-tree) (1 . 2)) 
(deftest caadr-tree (caadr cr-tree) (9 . 10)) 
(deftest cadar-tree (cadar cr-tree) (5 . 6)) 
(deftest caddr-tree (caddr cr-tree) (13 . 14)) 
(deftest cdaar-tree (cdaar cr-tree) (3 . 4)) 
(deftest cdadr-tree (cdadr cr-tree) (11 . 12)) 
(deftest cddar-tree (cddar cr-tree) (7 . 8)) 
(deftest cdddr-tree (cdddr cr-tree) (15 . 16)) 
(deftest caaaar-tree (caaaar cr-tree) 1) 
(deftest caaadr-tree (caaadr cr-tree) 9) 
(deftest caadar-tree (caadar cr-tree) 5) 
(deftest caaddr-tree (caaddr cr-tree) 13) 
(deftest cadaar-tree (cadaar cr-tree) 3) 
(deftest cadadr-tree (cadadr cr-tree) 11) 
(deftest caddar-tree (caddar cr-tree) 7) 
(deftest cadddr-tree (cadddr cr-tree) 15) 
(deftest cdaaar-tree (cdaaar cr-tree) 2) 
(deftest cdaadr-tree (cdaadr cr-tree) 10) 
(deftest cdadar-tree (cdadar cr-tree) 6) 
(deftest cdaddr-tree (cdaddr cr-tree) 14) 
(deftest cddaar-tree (cddaar cr-tree) 4) 
(deftest cddadr-tree (cddadr cr-tree) 12) 
(deftest cdddar-tree (cdddar cr-tree) 8) 
(deftest cddddr-tree (cddddr cr-tree) 16) 

;;; CONS
(deftest cons-pair (cons 'a 'b) (a . b))
(deftest cons-head (cons 'a '(b c d)) (a b c d))

;;; TREE-EQUAL
(let ((tree1 '(1 (1 2)))
      (tree2 (list 1 (list 1 2)))
      (tree3 (cons 1 (cons 1 (cons 2 nil)))))
  (deftest tree-equal (tree-equal tree1 tree2) t)
  (deftest tree-eql (eql tree1 tree2) nil)
  (deftest tree-not-equal (tree-equal tree1 tree3) nil))


(deftest tree-equal2
    (let ((tree1 '('a ('b 'c)))
	  (tree2 (list ''a (list ''b ''c))))
      (tree-equal tree1 tree2 :test 'eq))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 15.2 LISTS                                                   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ENDP
(deftest end-cons (endp '(1 . 2)) nil)
(deftest end-null (not (endp ())) nil)

;;; LIST-LENGTH
(deftest list-length0 (list-length '()) 0)
(deftest list-length4 (list-length '(a b c d)) 4)
(deftest list-length3 (list-length '(a (b c) d)) 3)
(deftest list-length-nil
    (let ((x (list 'a 'b 'c)))
      (rplacd (last x) x)
      (list-length x))
  nil)

;;; NTH
(defparameter nth-list '(0 1 2 3 4 5 6 7 8 9 10))
(deftest nth0 (nth 0 nth-list) 0)
(deftest nth1 (nth 1 nth-list) 1)
(deftest nth1000 (nth 1000 nth-list) nil)

;;; FIRST, SECOND, THIRD, ... TENTH
(deftest first (first nth-list) 0)
(deftest second (second nth-list) 1)
(deftest third (third nth-list) 2)
(deftest fourth (fourth nth-list) 3)
(deftest fifth (fifth nth-list) 4)
(deftest sixth (sixth nth-list) 5)
(deftest seventh (seventh nth-list) 6)
(deftest eighth (eighth nth-list) 7)
(deftest ninth (ninth nth-list) 8)
(deftest tenth (tenth nth-list) 9)

;;; REST
(deftest rest-null (rest nil) nil)
(deftest rest (rest nth-list) (1 2 3 4 5 6 7 8 9 10))

;;; NTHCDR
(deftest nthcdr0 (nthcdr 0 '(a b c)) (a b c))
(deftest nthcdr2 (nthcdr 2 '(a b c)) (c))
(deftest nthcdr4 (nthcdr 4 '(a b c)) ())
(deftest nthcdr1 (nthcdr 1 '(0 . 1)) 1)
(deftest nthcdr-null-0 (nthcdr 0 nil) nil)
(deftest nthcdr-null-3 (nthcdr 3 nil) nil)

;;; LAST
(deftest last-null (last nil) nil)
(deftest last-list (last '(1 2 3)) (3))
(deftest last-dot (last '(1 2 . 3)) (2 . 3))
(deftest mod-last
    (let* ((x (list 'a 'b 'c 'd))
	   (car-last (car (last x))))
      (rplacd (last x) (list 'e 'f))
      (list car-last (last x)))
  (d (f)))
(deftest last0 (last nth-list 0) ())
(deftest last1 (last nth-list 1) (10))
(deftest last2 (last nth-list 2) (9 10))
(deftest last3 (last nth-list 3) (8 9 10))
(deftest last12 (last nth-list 12) (0 1 2 3 4 5 6 7 8 9 10))
(deftest last13 (last nth-list 13) (0 1 2 3 4 5 6 7 8 9 10))
(deftest last-pair0 (last '(a . b) 0) b)
(deftest last-pair1 (last '(a . b) 1) (a . b))
(deftest last-pair2 (last '(a . b) 2) (a . b))

;;; LIST
(deftest list (list 3 4 'a (car '(b . c)) (+ 6 -2)) (3 4 a b 4))
(deftest list-null (list) ())
;;; LIST*
(deftest list* (list* 'a 'b 'c 'd) (a b c . d))
(deftest list*1 (list* 'a 'b 'c '(d e f)) (a b c d e f))
(deftest list*-null (list* 'a 'b nil) (a b))
(deftest list*x (list* 9) 9)

;;; MAKE-LIST
(deftest make-list5 (make-list 5) (nil nil nil nil nil))
(deftest make-list3 (make-list 3 :initial-element 'rah) (rah rah rah))
(deftest make-list0 (make-list 0) nil)
(deftest make-list0-ignored (make-list 0 :initial-element 99) nil)

;;; APPEND
(deftest append (append '(a b c) '(d e f) '() '(g)) (a b c d e f g))
(deftest append-dot (append '(a b c) 'd) (a b c . d))
(deftest append-empty (append) nil)
(deftest append-null (append nil) nil)
(deftest append-null2 (append nil nil) nil)
(deftest append-single (append 'a) a)
(deftest append-single2 (append nil 'a) a)
(deftest append2 (append nil '(a b c) '(d e f) '() '(g)) (a b c d e f g))
(deftest append-copies-args-except-last
    (let* ((x '(a b c))
	   (y '(d e f))
	   (result (append x y)))
      (list (eq x result)
	    (eq y (cdddr result))))
  (nil t))

;;; COPY-LIST
(deftest copy-list (copy-list '(1 2 3 4 5)) (1 2 3 4 5))
(deftest copy-list-null (copy-list nil) nil)
(deftest copy-list-dot (copy-list '(a . b)) (A . B))
(deftest copy-list-dot2 (copy-list '(1 2 3 . 4)) (1 2 3 . 4))
(deftest copy-list-copies-top-level
    (let* ((l '((1 . a) 2 3 4 5))
	   (copy (copy-list l)))
      (list (eql l copy)
	    (equal l copy)
	    (eql (car l) (car copy))))
  (nil t t))

;;; COPY-ALIST
(deftest copy-alist (copy-alist '(1 2 3 4 5)) (1 2 3 4 5))
(deftest copy-alist-null (copy-alist nil) nil)
(deftest copy-alist-dot (copy-alist '(a . b)) (A . B))
(deftest copy-alist-dot2 (copy-alist '(1 2 3 . 4)) (1 2 3 . 4))
(deftest copy-alist-copies-top-level
    (let* ((l '((1 . (a z)) frog (2 . b) (3 . 4)))
	   (copy (copy-alist l)))
      (list (eql l copy)
	    (equal l copy)
	    (eql (car l) (car copy))
	    (eql (cdar l) (cdar copy))))
  (nil t nil t))

;;; COPY-TREE
(deftest copy-tree (copy-tree '(1 2 3 4 5)) (1 2 3 4 5))
(deftest copy-tree-null (copy-tree nil) nil)
(deftest copy-tree-dot (copy-tree '(a . b)) (A . B))
(deftest copy-tree-dot2 (copy-tree '(1 2 3 . 4)) (1 2 3 . 4))
(deftest copy-tree-copies-all-levels
    (let* ((l '((1 . (a z)) frog (2 . b) (3 . 4)))
	   (copy (copy-tree l)))
      (list (eql l copy)
	    (equal l copy)
	    (eql (car l) (car copy))
	    (eql (cdar l) (cdar copy))))
  (nil t nil nil))

;;; REVAPPEND
(deftest revappend-dot (revappend '(a b c) 'd) (c b a . d))
(deftest revappend-nil2 (revappend nil nil) nil)
(deftest revappend-nil-atom (revappend nil 'a) a)
(deftest revappend-nil-list (revappend nil '(a b c)) (a b c))
(deftest revappend (revappend '(a b c) '(d e f)) (c b a d e f))
(deftest revappend-side-effects
    (let* ((x '(a b c))
	   (y '(d e f))
	   (z (revappend x y)))
      (list x (eq y (cdddr z))))
  ((a b c) t))

;;; NCONC
(deftest nconc-none (nconc) nil)
(deftest nconc-x (nconc 9) 9)
(deftest nconc-nil (nconc nil) nil)
(deftest nconc-nil2 (nconc nil nil) nil)
(deftest nconc
    (let* ((x (list 'a 'b 'c))
	   (y (list 'd 'e))
	   (z '(f g)))
      (nconc x y z)
      x)
  (a b c d e f g))
(deftest nconc-x-nil (nconc (list 'a 'b) nil) (a b))
(deftest nconc-nil-x (nconc nil (list 'a 'b)) (a b))

;;; NRECONC
#-(and cmu (not eclipse))
(progn
  (deftest nreconc-dot (nreconc (list 'a 'b 'c) 'd) (c b a . d))
  (deftest nreconc-nil-atom (nreconc nil 'a) a))
(deftest nreconc-nil2 (nreconc nil nil) nil)
(deftest nreconc-nil-list (nreconc nil '(a b c)) (a b c))
(deftest nreconc (nreconc (list 'a 'b 'c) '(d e f)) (c b a d e f))
(deftest nreconc-side-effects
    (let* ((x (list 'a 'b 'c))
	   (y '(d e f))
	   (z (nreconc x y)))
      (list z (eq y (cdddr z))))
  ((c b a d e f) t))

;;; push!!!
;;; PUSHNEW!!!
;;; POP!!!

;;; BUTLAST
(deftest butlast-nil (butlast nil) nil)
(deftest butlast1 (butlast '(a)) nil)
(deftest butlast (butlast '(a b c d)) (a b c))
(deftest butlast2 (butlast '(a b c d) 2) (a b))
(deftest butlast3 (butlast '(a b c d) 3) (a))
(deftest butlast4 (butlast '(a b c d) 4) ())
(deftest butlast5 (butlast '(a b c d) 5) ())

;;; NBUTLAST
(deftest nbutlast-nil (nbutlast nil) nil)
(deftest nbutlast1 (nbutlast '(a)) nil)
(deftest nbutlast (nbutlast (list 'a 'b 'c 'd)) (a b c))
(deftest nbutlast2 (nbutlast (list 'a 'b 'c 'd) 2) (a b))
(deftest nbutlast3 (nbutlast (list 'a 'b 'c 'd) 3) (a))
(deftest nbutlast4 (nbutlast '(a b c d) 4) ())
(deftest nbutlast5 (nbutlast '(a b c d) 5) ())
(deftest nbutlast-effects
    (let ((x (list 'a 'b 'c 'd)))
      (nbutlast x)
      x)
  (a b c))

;;; LDIFF
(deftest ldiff (let ((x '(a b c d e)))
		 (ldiff x (cdddr x)))
  (a b c))
(deftest ldiff-none (let ((x '(a b c d)))
		      (ldiff x (list 'c 'd)))
  (a b c d))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 15.3 ALTERATION OF LIST STRUCTURE                            ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; RPLACA, RPLACD
(deftest rplaca (let* ((c (cons 1 2))
		       (r (rplaca c 3)))
		  (list (eq r c) r))
  (t (3 . 2)))
(deftest rplacd (let* ((c (cons 1 2))
		       (r (rplacd c 3)))
		  (list (eq r c) r))
  (t (1 . 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 15.4 SUBSTITUTION OF EXPRESSIONS                             ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SUBST, SUBST-IF, SUBST-IF-NOT
(deftest subst1
    (subst 'tempest 'hurricane '(shakespeare wrote (the hurricane)))
  (shakespeare wrote (the tempest)))
(deftest subst2
    (subst 'foo 'nil '(shakespeare wrote (twelfth night)))
  (shakespeare wrote (twelfth night . foo) . foo))
(deftest subst3
    (subst '(a . cons) '(old . pair) '((old . psice) ((old . shoes) old . pair) (old . pair))
	    :test #'equal)
  ((old . psice) ((old . shoes) a . cons) (a . cons)))
(defparameter subst-tree '(1 (1 2) (1 2 3) (1 2 3 4)))
(deftest subst4 (subst "two" 2 subst-tree) (1 (1 "two") (1 "two" 3) (1 "two" 3 4)))
;; For Eclipse
(deftest subst5 (eq (subst "five" 5 subst-tree) subst-tree) t)
(deftest subst-if (subst-if 5 #'listp subst-tree) 5)
(deftest subst-if2 (subst-if '(x) #'consp subst-tree) (x))
(deftest subst-if-not (subst-if-not '(x) #'atom subst-tree) (x))
(deftest subst-key (subst 'x 3 subst-tree
			  :key #'(lambda (y) (and (listp y) (third y))))
  (1 (1 2) x x))

;;; NSUBST, NSUBST-IF, NSUBST-IF-NOT
;; For Eclipse
(deftest nsubst5 (eq (nsubst "five" 5 subst-tree) subst-tree) t)
(deftest nsubst4
    (let ((subst-tree (list 1 (list 1 2) (list 1 2 3) (list 1 2 3 4))))
      (and (eq (nsubst "two" 2 subst-tree) subst-tree)
	   subst-tree))
  (1 (1 "two") (1 "two" 3) (1 "two" 3 4)))
(deftest nsubst-if (nsubst-if 5 #'listp subst-tree) 5)
(deftest nsubst-if2 (subst-if '(x) #'consp subst-tree) (x))
(deftest nsubst-if-not (subst-if-not '(x) #'atom subst-tree) (x))
(deftest nsubst-key
    (let ((subst-tree (list 1 (list 1 2) (list 1 2 3) (list 1 2 3 4))))
      (and (eq (nsubst 'x 3 subst-tree
			  :key #'(lambda (y) (and (listp y) (third y)))) subst-tree)
	   subst-tree))
  (1 (1 2) x x))

;;; SUBLIS
(deftest sublis 
    (sublis '((x . 100) (z . zprime)) '(plus x (minus g z x p) 4 . x))
  (plus 100 (minus g zprime 100 p) 4 . 100))
(deftest sublis-again
    (sublis '(((+ x y) . (- x y)) ((- x y) . (+ x y)))
	    '(* (/ (+ x y) (+ x p)) (- x y))
	    :test #'equal)
  (* (/ (- X Y) (+ X P)) (+ X Y)))
(defparameter sublis-tree '(1 (1 2) ((1 2 3)) (((1 2 3 4)))))
(defparameter sublis-tree2 '("one" ("one" "two") (("one" "Two" "three"))))
(deftest sublis1 (sublis '((3 . "three")) sublis-tree)
  (1 (1 2) ((1 2 "three")) (((1 2 "three" 4)))))
(deftest sublis2
    (sublis '((t . "string"))
	    (sublis '((1 . "") (4 . 44)) sublis-tree)
	    :key #'stringp)
  ("string" ("string" 2) (("string" 2 3)) ((("string" 2 3 44)))))
(deftest sublis3
    (sublis '(("two" . 2)) sublis-tree2 :test #'equal)
  ("one" ("one" 2) (("one" "Two" "three"))))
(deftest sublis4
    (sublis '((t . 'temp)) sublis-tree
	    :key #'(lambda (x) (or (atom x) (< (list-length x) 3))))
  ('temp 'temp quote temp))

(deftest nsublis1
    (let ((sublis-tree (list 1 (list 1 2) (list (list 1 2 3))
			     (list (list (list 1 2 3 4))))))
      (and (eq (nsublis '((3 . "three")) sublis-tree) sublis-tree)
	   sublis-tree))
  (1 (1 2) ((1 2 "three")) (((1 2 "three" 4)))))
(deftest nsublis2
    (let ((sublis-tree (list 1 (list 1 2) (list (list 1 2 3))
			     (list (list (list 1 2 3 4))))))
      (and (eq (nsublis '((t . "string"))
			(nsublis '((1 . "") (4 . 44)) sublis-tree)
			:key #'stringp)
	       sublis-tree)
	   sublis-tree))
  ("string" ("string" 2) (("string" 2 3)) ((("string" 2 3 44)))))
(deftest nsublis3
    (let ((sublis-tree2 (list "one" (list "one" "two")
			      (list (list"one" "Two" "three")))))
      (and (eq (nsublis '(("two" . 2)) sublis-tree2 :test #'equal)
	       sublis-tree2)
	   sublis-tree2))
  ("one" ("one" 2) (("one" "Two" "three"))))
(deftest nsublis4
    (let ((sublis-tree (list 1 (list 1 2) (list (list 1 2 3))
			     (list (list (list 1 2 3 4))))))
      (and (eq (nsublis '((t . 'temp)) sublis-tree
			:key #'(lambda (x) (or (atom x) (< (list-length x) 3))))
	       sublis-tree)
	   sublis-tree))
  ('temp 'temp quote temp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 15.5 USING LISTS AS SETS                                     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MEMBER, MEMBER-IF, MEMBER-IF-NOT
(deftest member1 (member 'snerd '(a b c d)) nil)
(deftest member2 (member 'snerd nil) nil)
(deftest member3 (member 'a '(g (a y) c a d e a f)) (a d e a f))
(deftest member4 (member 'b '((a 1) (b 2) (c 3)) :key #'car)
  ((b 2) (c 3)))
(deftest member5 (member #\B '(#\a #\b #\c) :test #'char-equal)
  (#\b #\c))
(deftest member6 (member #\B '(#\a #\b #\c) :test-not #'char-not-equal)
  (#\b #\c))
(deftest member-if (member-if #'numberp '(a #\Space 5 foo)) (5 foo))
(deftest member-if2 (member-if #'numberp '((a) (#\Space) (5) (foo))
			       :key #'car)
  ((5) (foo)))

(deftest member-if-not (member-if-not #'symbolp '(a #\Space 5 foo))
  (#\space 5 foo))
(deftest member-if-not2 (member-if-not #'symbolp '((a) (#\Space) (5) (foo))
				       :key #'car)
  ((#\space) (5) (foo)))

;;; TAILP
(deftest tailp (let ((x '(a b c d e)))
		 (tailp (cdddr x) x))
  t)
(deftest tailp-none (let ((x '(a b c d)))
		      (tailp (list 'c 'd) x))
  nil)


;;; ADJOIN
(deftest adjoin1 (adjoin 'a nil) (a))
(deftest adjoin2 (adjoin '(test-item 1) nil) ((test-item 1)))
(deftest adjoin3 (adjoin (list 'test-item 1) '((test-item 1)))
  ((test-item 1) (test-item 1)))
(deftest adjoin4 (adjoin (list 'test-item 1) '((test-item 1)) :test #'equal)
  ((test-item 1)))
(deftest adjoin5 (adjoin (list 'test-item 1) '((test-item 1)) :test-not
			 #'(lambda (x y) (not (equal x y))))
  ((test-item 1)))
(deftest adjoin6 (adjoin (list 'test-item 1) '((test-item 1)) :key #'cadr)
  ((test-item 1)))

;;; UNION
(deftest union1 (union nil nil) nil)
(deftest union2 (union nil '(a)) (a))
(deftest union3 (union '(a) nil) (a))
(deftest union4 (set-exclusive-or (union '(a b c) '(f a d)) '(a b c d f)) nil)
(deftest union5
    (let ((s (set-exclusive-or (union '((x 5) (y 6))
				      '((z 2) (x 4))
				      :key #'car)
			       '((Y 6) (Z 2) )
			       :key #'car)))
      (and (eq (caar s) 'x)
	   (= (length s) 1)))
  t)
(deftest union6
    (set-exclusive-or (union '(#\a #\b #\c) '(#\B #\A))
		      '(#\a #\b #\c #\A #\B)) nil)
(deftest union7
    (set-exclusive-or (union '(#\a #\b #\c) '(#\B #\A) :test #'char-equal)
		      '(#\a #\b #\c) :test #'char-equal) nil)
(deftest union8
    (set-exclusive-or (union '(#\a #\b #\c) '(#\B #\A) :test-not #'char-not-equal)
		      '(#\a #\b #\c) :test #'char-equal) nil)

;;; INTERSECTION
(deftest intersection1 (intersection nil nil) nil)
(deftest intersection2 (intersection nil '(a)) nil)
(deftest intersection3 (intersection '(a) nil) nil)
(deftest intersection4 (intersection '(a b c) '(f a d)) (a))
(deftest intersection5
    (let ((s (intersection '((a 1) (b 2)) '((b 3)) :key #'car)))
      (and (eq (caar s) 'b)
	   (= (length s) 1))) t)
(deftest intersection6
    (set-exclusive-or (intersection '(#\a #\b #\c) '(#\D #\B #\A)
				    :test #'char-equal)
		      '(#\a #\b)
		      :test #'char-equal)
  nil)
(deftest intersection7
    (set-exclusive-or (intersection '(#\a #\b #\c) '(#\D #\B #\A)
				    :test-not #'char-not-equal)
		      '(#\a #\b)
		      :test #'char-equal)
  nil)

;;; SET-DIFFERENCE
(deftest set-difference1 (set-difference nil nil) nil)
(deftest set-difference2 (set-difference '(a) nil) (a))
(deftest set-difference3 (set-difference nil '(a)) nil)
(deftest set-difference4
    (set-exclusive-or (set-difference '(a b c d) '(d b))
		      '(a c))
  nil)
(deftest set-difference5
    (let ((s (set-difference '((a 1) (b 2)) '((b 3)) :key #'car)))
      (and (eq (caar s) 'a)
	   (= (length s) 1))) t)
(deftest set-difference6
    (set-difference '(#\a #\b #\c) '(#\D #\B #\A)
		    :test #'char-equal)
  (#\c))
(deftest set-difference7
    (set-difference '(#\a #\b #\c) '(#\D #\B #\A)
		    :test-not #'char-not-equal)
  (#\c))

#+not-yet(deftest set-difference
    (set-exclusive-or (set-difference
		       '("stawberry" "chocolate" "banana" "lemon" "pistachio" "rhubarb")
		       '(#\c #\w)
		       :test #'(lambda (s c) (find c s)))
		      '("rhubarb" "lemon" "banana")
		      :test #'equal)
  nil)

    
;;; SET-EXCLUSIVE-OR
(deftest sxor1 (set-exclusive-or '(a) nil) (a))
(deftest sxor2 (set-exclusive-or nil '(a)) (a))
(deftest sxor3 (set-exclusive-or '(a) '(a)) ())
(deftest sxor4 (set-exclusive-or '(a b) '(b a)) ())
(deftest sxor5 (let ((s (set-exclusive-or '(a) '(b))))
		 (and (member 'a s)
		      (member 'b s)
		      (= (length s) 2))) t)
(deftest sxor6
    (let ((s (set-exclusive-or '(a b c) '(b d c))))
      (and (member 'a s)
	   (member 'd s)
	   (= (length s) 2))) t)
(deftest sxor7
    (set-exclusive-or '((a 1) (b 2))
		      '((b 3))
		      :key #'car)
  ((A 1)))
(deftest sxor8 (set-exclusive-or '(#\a #\b #\c)
				 '(#\B #\A)
				 :test #'char-equal)
  (#\c))
(deftest sxor9 (set-exclusive-or '(#\a #\b #\c)
				 '(#\B #\A)
				 :test-not #'char-not-equal)
  (#\c))

;;; SUBSETP
(let ((cosmos '(1 "a" (1 2)))
      (l2 '((1) (2))))
  (deftest subset1 (subsetp '(1) cosmos) t)
  (deftest subset2 (subsetp (list (list 1 2)) cosmos) nil)
  (deftest subset3 (subsetp (list (list 1 2)) cosmos :test 'equal) t)
  (deftest subset4 (subsetp (list (list 1) (list 2)) l2) nil)
  (deftest subset5 (subsetp (list (list 1) (list 2)) l2 :key #'car) t))


;;; NUNION
(deftest nunion1 (nunion nil nil) nil)
(deftest nunion2 (nunion nil (list 'a)) (a))
(deftest nunion3 (nunion (list 'a) nil) (a))
(deftest nunion4
    (set-exclusive-or (nunion (list 'a 'b 'c)
			      (list 'f 'a 'd))
		      '(a b c d f))
  nil)
(deftest nunion5
    (let ((s (set-exclusive-or (nunion (list '(x 5) '(y 6))
				       (list '(z 2) '(x 4))
				      :key #'car)
			       '((Y 6) (Z 2) )
			       :key #'car)))
      (and (eq (caar s) 'x)
	   (= (length s) 1)))
  t)
(deftest nunion6
    (set-exclusive-or (nunion (list #\a #\b #\c) (list #\B #\A))
		      '(#\a #\b #\c #\A #\B)) nil)
(deftest nunion7
    (set-exclusive-or (nunion (list #\a #\b #\c) (list #\B #\A)
			      :test #'char-equal)
		      '(#\a #\b #\c) :test #'char-equal) nil)
(deftest nunion8
    (set-exclusive-or (nunion (list #\a #\b #\c) (list #\B #\A)
			      :test-not #'char-not-equal)
		      '(#\a #\b #\c) :test #'char-equal) nil)

;;; NINTERSECTION
(deftest nintersection1 (nintersection nil nil) nil)
(deftest nintersection2 (nintersection nil '(a)) nil)
(deftest nintersection3 (nintersection (list 'a) nil) nil)
(deftest nintersection4 (nintersection (list 'a 'b 'c) '(f a d)) (a))
(deftest nintersection5
    (let ((s (nintersection (list '(a 1) '(b 2)) '((b 3)) :key #'car)))
      (and (eq (caar s) 'b)
	   (= (length s) 1))) t)
(deftest nintersection6
    (set-exclusive-or (nintersection (list #\a #\b #\c) '(#\D #\B #\A)
				    :test #'char-equal)
		      '(#\a #\b)
		      :test #'char-equal)
  nil)
(deftest nintersection7
    (set-exclusive-or (nintersection (list #\a #\b #\c) '(#\D #\B #\A)
				    :test-not #'char-not-equal)
		      '(#\a #\b)
		      :test #'char-equal)
  nil)

;;; NSET-DIFFERENCE
(deftest nset-difference1 (nset-difference nil nil) nil)
(deftest nset-difference2 (nset-difference (list 'a) nil) (a))
(deftest nset-difference3 (nset-difference nil '(a)) nil)
(deftest nset-difference4
    (set-exclusive-or (nset-difference (list 'a 'b 'c 'd) '(d b))
		      '(a c))
  nil)
(deftest nset-difference5
    (let ((s (nset-difference (list '(a 1) '(b 2)) '((b 3))
			      :key #'car)))
      (and (eq (caar s) 'a)
	   (= (length s) 1))) t)
(deftest nset-difference6
    (nset-difference (list #\a #\b #\c) '(#\D #\B #\A)
		    :test #'char-equal)
  (#\c))
(deftest nset-difference7
    (nset-difference (list #\a #\b #\c) '(#\D #\B #\A)
		    :test-not #'char-not-equal)
  (#\c))

#+not-yet(deftest nset-difference
    (set-exclusive-or (nset-difference
		       (list "stawberry" "chocolate" "banana" "lemon" "pistachio" "rhubarb")
		       '(#\c #\w)
		       :test #'(lambda (s c) (find c s)))
		      '("rhubarb" "lemon" "banana")
		      :test #'equal)
  nil)

    
;;; NSET-EXCLUSIVE-OR
(deftest nsxor1 (nset-exclusive-or (list 'a) nil) (a))
(deftest nsxor2 (nset-exclusive-or nil (list 'a)) (a))
(deftest nsxor3 (nset-exclusive-or (list 'a) (list 'a)) ())
(deftest nsxor4 (nset-exclusive-or (list 'a 'b) (list 'b 'a)) ())
(deftest nsxor5 (let ((s (nset-exclusive-or (list 'a) (list 'b))))
		 (and (member 'a s)
		      (member 'b s)
		      (= (length s) 2))) t)
(deftest nsxor6
    (let ((s (nset-exclusive-or (list 'a 'b 'c) (list 'b 'd 'c))))
      (and (member 'a s)
	   (member 'd s)
	   (= (length s) 2))) t)
(deftest nsxor7
    (nset-exclusive-or (list '(a 1) '(b 2))
		       (list '(b 3))
		      :key #'car)
  ((A 1)))
(deftest nsxor8 (nset-exclusive-or (list #\a #\b #\c)
				   (list #\B #\A)
				 :test #'char-equal)
  (#\c))
(deftest nsxor9 (nset-exclusive-or (list #\a #\b #\c)
				   (list #\B #\A)
				 :test-not #'char-not-equal)
  (#\c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 15.6 ASSOCIATION LISTS                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ACONS
(deftest acons (acons 'a 1 nil) ((a . 1)))
(deftest acons1 (acons 'a 1 3) ((a . 1) . 3))

;;; PAIRLIS
(deftest pairlis (pairlis '(1) '(2)) ((1 . 2)))
(deftest pairlis1
    (set-exclusive-or
     (pairlis '(one two) '(1 2) '((three . 3) (four . 19)))
     '((ONE . 1) (TWO . 2) (THREE . 3) (FOUR . 19))
     :test #'equal)
  nil)

;;; ASSOC
(deftest assoc (assoc 'r '((a . b) (c . d) (r . x) (s . y) (r . z)))
  (r . x))
(deftest assoc-missing (assoc 'goo '((foo . bar) (zoo . goo))) nil)
(deftest assoc-lists (assoc '2 '((1 a b c) (2 b c d) (-7 x y z)))
  (2 b c d))
(deftest assoc-test (assoc 3 '((1 . a) (2 . b) (4 . d) (5 . e)) :test #'<)
  (4 . d))
(deftest assoc-test-not
    (assoc 3 '((1 . a) (2 . b) (4 . d) (5 . e)) :test-not #'>)
  (4 . d))
(deftest assoc-key
    (assoc 3 '(((1 . x) . a) ((2 . x) . b) ((3 . x) . c) ((4 . x) . d) ((5 . x) . e))
	   :key #'car)
  ((3 . x) . c))
(deftest assoc-test-key
    (assoc 3 '(((1 . x) . a) ((2 . x) . b) ((4 . x) . d) ((5 . x) . e))
	   :test #'< :key #'car)
  ((4 . x) . d))
(deftest assoc-test-not-key
    (assoc 3 '(((1 . x) . a) ((2 . x) . b) ((4 . x) . d) ((5 . x) . e))
	   :test-not #'> :key #'car)
  ((4 . x) . d))

;;; ASSOC-IF
(deftest assoc-if (assoc-if #'(lambda (x) (eq x 'r))
			    '((a . b) (c . d) (r . x) (s . y) (r . z)))
  (r . x))
(deftest assoc-if-missing (assoc-if #'(lambda (x) (eq x 'goo))
				    '((foo . bar) (zoo . goo))) nil)
(deftest assoc-if-lists (assoc-if #'(lambda (x) (= x 2))
				  '((1 a b c) (2 b c d) (-7 x y z)))
  (2 b c d))
(deftest assoc-if-test (assoc-if #'(lambda (x) (< 3 x))
				 '((1 . a) (2 . b) (4 . d) (5 . e)))
  (4 . d))
(deftest assoc-if-key
    (assoc-if #'(lambda (x) (= x 3))
	      '(((1 . x) . a) ((2 . x) . b) ((3 . x) . c) ((4 . x) . d) ((5 . x) . e))
	   :key #'car)
  ((3 . x) . c))
(deftest assoc-if-test-key
    (assoc-if #'(lambda (x) (< 3 x))
	      '(((1 . x) . a) ((2 . x) . b) ((4 . x) . d) ((5 . x) . e))
	      :key #'car)
  ((4 . x) . d))

;;; ASSOC-IF-NOT
(deftest assoc-if-not-test-not
    (assoc-if-not #'(lambda (x) (> 3 x))
		  '((1 . a) (2 . b) (4 . d) (5 . e)))
  (4 . d))
(deftest assoc-if-not-test-not-key
    (assoc-if-not #'(lambda (x) (> 3 x))
		  '(((1 . x) . a) ((2 . x) . b) ((4 . x) . d) ((5 . x) . e))
		  :key #'car)
  ((4 . x) . d))

;;; RASSOC
(deftest rassoc (rassoc 'a '((a . b) (b . c) (c . a) (z . a)))
  (c . a))
(deftest rassoc-missing (rassoc 'r '((a . b) (c . d) (r . x) (s . y) (r . z)))
  nil)
(deftest rassoc2 (rassoc 'goo '((foo . bar) (zoo . goo))) (zoo . goo))
(deftest rassoc-lists-missing (rassoc '2 '((1 a b c) (2 b c d) (-7 x y z)))
  nil)
(deftest rassoc-lists-missing2 (rassoc 'b '((1 a b c) (2 b c d) (-7 x y z)))
  nil)
;; Note that compiler can coelesce lists (or sublists).
(deftest rassoc-lists-missing3 (rassoc (list 'b 'c 'd) '((1 a b c) (2 b c d) (-7 x y z)))
  nil)
(deftest rassoc-lists (rassoc '(b c d) '((1 a b c) (2 b c d) (-7 x y z))
			      :test #'equal)
  (2 b c d))
(deftest rassoc-test (rassoc 3 '((a . 1) (b . 2) (d . 4) (e . 5)) :test #'<)
  (d . 4))
(deftest rassoc-test-not
    (rassoc 3 '((a . 1) (b . 2) (d . 4) (e . 5)) :test-not #'>)
  (d . 4))
(deftest rassoc-key
    (rassoc 3 '((a 1 x) (b 2 x) (c 3 x) (d 4 x) (e 5 x))
	   :key #'car)
  (c 3 x))
(deftest rassoc-test-key
    (rassoc 3 '((a 1 x) (b 2 x) (d 4 x) (e 5 x))
	   :test #'< :key #'car)
  (d 4 x))
(deftest rassoc-test-not-key
    (rassoc 3 '((a 1 x) (b 2 x) (d 4 x) (e 5 x))
	   :test-not #'> :key #'car)
  (d 4 x))

;;; RASSOC-IF
(deftest rassoc-if (rassoc-if #'(lambda (x) (eq x 'a))
			      '((a . b) (b . c) (c . a) (z . a)))
  (c . a))
(deftest rassoc-if-missing (rassoc-if #'(lambda (x) (eq x 'r))
				      '((a . b) (c . d) (r . x) (s . y) (r . z)))
  nil)
(deftest rassoc-if2 (rassoc-if #'(lambda (x) (eq x 'goo))
			       '((foo . bar) (zoo . goo))) (zoo . goo))
(deftest rassoc-if-lists-missing (rassoc-if #'(lambda (x) (eq x 2))
					    '((1 a b c) (2 b c d) (-7 x y z)))
  nil)
(deftest rassoc-if-lists-missing2 (rassoc-if #'(lambda (x) (eq x 'b))
					     '((1 a b c) (2 b c d) (-7 x y z)))
  nil)
;; Note that compiler can coelesce lists (or sublists).
(deftest rassoc-if-lists (rassoc-if #'(lambda (x) (equal x '(b c d)))
				    '((1 a b c) (2 b c d) (-7 x y z)))
  (2 b c d))
(deftest rassoc-if-test (rassoc-if #'(lambda (x) (> x 3))
				   '((a . 1) (b . 2) (d . 4) (e . 5)))
  (d . 4))
(deftest rassoc-if-key
    (rassoc-if #'(lambda (x) (= x 3))
	       '((a 1 x) (b 2 x) (c 3 x) (d 4 x) (e 5 x))
	       :key #'car)
  (c 3 x))
(deftest rassoc-if-test-key
    (rassoc-if #'(lambda (x) (> x 3))
	       '((a 1 x) (b 2 x) (d 4 x) (e 5 x))
	       :key #'car)
  (d 4 x))

;;; RASSOC-IF-NOT
(deftest rassoc-if-not-test-not
    (rassoc-if-not #'(lambda (x) (> 3 x))
		   '((a . 1) (b . 2) (d . 4) (e . 5)))
  (d . 4))
(deftest rassoc-if-not-test-not-key
    (rassoc-if-not #'(lambda (x) (> 3 x))
		   '((a 1 x) (b 2 x) (d 4 x) (e 5 x))
		   :key #'car)
  (d 4 x))
