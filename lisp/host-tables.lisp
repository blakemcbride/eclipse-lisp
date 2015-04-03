;;; We use a open-addressing hash table with linear probing.  We never
;;; delete entries, just overwrite them or clear the entire table.
;;; When inserting an entry, we grow the table (i.e. rehash its
;;; contents in a larger table) by 75% or 50 slots, which ever is
;;; smaller.

;;; In a host Lisp, a "table" is a cons of two parallel simple
;;; vectors.  NIL keys represent empty places in the table.

;;; We use table sizes that are a power of two so that that C rem
;;; operations can be faster.

;;; Potential Optimization: We could save heap space by using
;;; different kinds of tables, depending on what effective methods
;;; have been stored, as in PCL.  For example:
;;;
;;; - Only one method ever defined: (i.e. emf-table-set called just
;;;   once) Let the keys table be of size 2 (one blank) or just store
;;;   the argument class list itself as the keys table.
;;;
;;; - One effective-method: (i.e. emf-table-set called several times,
;;;   but always with the same em-function) Let the values table just
;;;   be the effective method.  (ISSUE: how do we recognize non-eq
;;;   but effectively the same closures?  Cache them in
;;;   compute-effective-method?) 


(defconstant max-hash-table-size (floor most-positive-fixnum 65))
(deftype eclipse::emf-table () 'hash-structure)

(defun eclipse::make-emf-table ()
  (eclipse::make-open-address-hash-table nil 40 30 50))

(defun eclipse::clear-emf-table (table)
  (eclipse::open-address-clrhash table))

(declaim (inline eclipse::emf-table-set))
(locally-fast

(defmacro emf-hash (hasher list length)
  `(loop with size fixnum = max-hash-table-size
	 and list = ,list
	 and length fixnum = ,length
	 with h fixnum = (rem (the eclipse::index
				(,hasher (car list)))
			      size)
	 if (<= length 1) do (return h)
	 do (setq list (cdr list)
		  length (the eclipse:index (1- length))
		  h (rem (the eclipse::index
			   (eclipse::merge-hash-codes h (,hasher (car list))))
			 size))))

(defmacro emf-test (this-keylist accessor keylist length)
  `(loop				
    ;;repeat length			; REPEAT is not optimized in CMU:
    for counter fixnum downfrom (- ,length 1) to 0
    for x in ,keylist
    and wrapper in ,this-keylist
    unless (eq (,accessor x) wrapper)
    do (return nil)
    finally (return t)))

(defmacro eclipse::emf-table-get (table length arglist)
  `(block eclipse::emf-table-get
     (locally (declare (type eclipse::emf-table ,table) (type eclipse:index ,length)
		       (type cons ,arglist))
       (macrolet
	   ((hash1 (object)
		   `(the eclipse:index;; EXCL can't infer the type by itself
		      (let ((index (eclipse::wrapper-hash-key
				    (eclipse::object-wrapper ,object))))
			(declare (type eclipse:index index))
			(if (zerop index)
			    (return-from eclipse::emf-table-get
			      (eclipse::update-instances-if-needed ,',arglist))
			    index))))
	    (hash-list (arglist)
		       `(emf-hash hash1 (the cons ,arglist) ,',length))
	    (test (this-key key)
		  `(emf-test ,this-key eclipse::object-wrapper ,key ,',length)))
       
	 (open-address-gethash hash-list test ,arglist ,table nil)))))


;;; It is a shame that length is ignored in emf-table-set and then
;;; recalculated in emf-table-sethash.  A C implementation might use a
;;; static file global.
(defun emf-table-sethash (key table new-value)
  (declare (type eclipse::emf-table table) (type list key))
  (let ((length (length key)))
    (declare (type eclipse:index length))
    (macrolet
	((ident (x) x)
	 (hash-list (arglist)
		    `(emf-hash eclipse::wrapper-hash-key ,arglist length))
	 (test (this-key key)
	       `(emf-test ,this-key ident ,key length)))
      (open-address-sethash hash-list test key table new-value
			    emf-table-sethash))))
		  
(defun eclipse::emf-table-set (table length classes value)
  (declare #+nn(type eclipse:index length)
	   (ignore length))
  (emf-table-sethash
   (eclipse::class-wrappers classes)
   table value))

)

