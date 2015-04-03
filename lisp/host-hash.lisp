;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                          HASH CODES                          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#-cmu
(declaim (inline eclipse::eq-hash eclipse::eql-hash))

(defconstant hash-mask #xFFFFFFF)

;;; Allegro can't infer the declaration by itself
;;; (and it doesn't make constants available at compile-time-either).
(defmacro mask-part (x &optional (mask #xFFFFFFF))
  `(the (integer 0 ,mask) (cl:logand ,x ,mask)))

(locally-fast


(defmacro eclipse::merge-hash-codes (x y)
  `(mask-part (+ (mask-part (* 64 (mask-part ,x #x3fffff)))
		 ,y)))

(defun eclipse::object-address (x)
  #+cmu (kernel:get-lisp-obj-address x)
  #+excl (excl::pointer-to-address x))

;;; Should be object-address, but that doesn't work when implementation moves data.
(defun eclipse::eq-hash (x) (sxhash x))
(defun eclipse::string-hash (x) (sxhash (string x)))

#+now-defined-in-kernel
(defun eclipse::eql-hash (x)			;dylan:object-hash
  (cl:typecase x
    (fixnum (eclipse::eq-hash x))
    (single-float (multiple-value-bind (s e n)
		      (integer-decode-float (the single-float x))
		    (eclipse::merge-hash-codes
		     (eclipse::merge-hash-codes e n) s)))
    (double-float (multiple-value-bind (s e n)
		      (integer-decode-float (the double-float x))
		    (eclipse::merge-hash-codes
		     (eclipse::merge-hash-codes e n)
		     (logand s hash-mask))))
    (bignum (loop with hash = 0 and xint = (eclipse::bignum-xint x)
		  for i below (eclipse::xint-size xint)
		  do (setq hash (eclipse::merge-hash-codes
				 hash
				 (eclipse::eq-hash (digitref xint i))))
		  finally (return hash)))
    (ratio (eclipse::merge-hash-codes
	    (eclipse::eql-hash (eclipse::ratio-numerator x))
	    (eclipse::eql-hash (eclipse::ratio-numerator x))))
    (complex (eclipse::merge-hash-codes
	      (eclipse::eql-hash (complex-realpart x))
	      (eclipse::eql-hash (complex-imagpart x))))
    (t (eclipse::eq-hash x))))

)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; OPEN ADDRESSING

;;; FOR ALL OPEN ADDRESS HASH-TABLES
  
(locally-fast

(defstruct (hash-structure
	    (:copier nil)
	    (:predicate nil)
	    (:include host::built-in-instance)
	    (:print-function host::instance-printer)
	    (:constructor make-open-address-hash-table1
			  (host::class size rehash-size n-buckets)))
  keys values
  (n-buckets 0 :type eclipse::index)
  (size 0 :type eclipse::index)
  (count 0 :type eclipse::index)
  (rehash-size 0 :type real))

(defmacro eclipse::open-address-hash-table-keys (x) `(hash-structure-keys ,x))
(defmacro eclipse::open-address-hash-table-values (x) `(hash-structure-values ,x))
(defmacro eclipse::open-address-hash-table-n-buckets (x) `(hash-structure-n-buckets ,x))
(defmacro eclipse::open-address-hash-table-size (x) `(hash-structure-size ,x))
(defmacro eclipse::open-address-hash-table-count (x) `(hash-structure-count ,x))
(defmacro eclipse::open-address-hash-table-rehash-size (x) `(hash-structure-rehash-size ,x))

(defun eclipse::open-address-hash-table-keys-setter (new x)
  (setf (hash-structure-keys x) new))
(defun eclipse::open-address-hash-table-values-setter (new x)
  (setf (hash-structure-values x) new))
(defun eclipse::open-address-hash-table-n-buckets-setter (new x)
  (setf (hash-structure-n-buckets x) new))
(defun eclipse::open-address-hash-table-size-setter (new x)
  (setf (hash-structure-size x) new))
(defun eclipse::open-address-hash-table-count-setter (new x)
  (setf (hash-structure-count x) new))
(defun eclipse::open-address-hash-table-rehash-size-setter (new x)
  (setf (hash-structure-rehash-size x) new))
)

(defun eclipse::open-address-clrhash (table)
  (let ((n (eclipse::open-address-hash-table-n-buckets table)))
    (eclipse::open-address-hash-table-keys-setter
     (make-array n :initial-element '.empty.) table)
    (eclipse::open-address-hash-table-values-setter
     (make-array n) table)
    (eclipse::open-address-hash-table-count-setter 0 table))
  table)

;;; The following return or accept INDEX parameters.
(defun eclipse::make-open-address-hash-table (class size rehash-size n-buckets)
  (let* ((class (or class eclipse::open-address-hash-table-classobj))
	 (table (make-open-address-hash-table1 class size rehash-size n-buckets)))
    (eclipse::set-tagged-instance-wrapper
     table (eclipse::static-class-wrapper class))
    (eclipse::open-address-clrhash table)
    table))

;;; This could be defined in Lisp except that it occurs so early.
;;; n-buckets is a FIXNUM
;;#+not-needed
(defun eclipse::make-eq-hash-table (n-buckets)
  (eclipse::make-open-address-hash-table
   nil (eclipse:fixnum-index (floor (* .8 n-buckets)))
   200 (eclipse:fixnum-index n-buckets)))

(defun eclipse::open-address-maphash (function hash-table)
  (loop with keys = (eclipse::open-address-hash-table-keys hash-table)
	and values = (eclipse::open-address-hash-table-values hash-table)
	for index fixnum from 0 below (eclipse::open-address-hash-table-n-buckets hash-table)
	for key = (svref keys index)
	do (case key
	     ((.empty. .removed.))
	     (t (funcall function key (svref values index))))))


(defmacro with-open-address-key ((hash-function test table index key &optional (keys 'keys))
				 &key empty removed found)
  `(loop with ,keys = (eclipse::open-address-hash-table-keys ,table)
	 and n-buckets fixnum = (eclipse::open-address-hash-table-n-buckets ,table)
	 with original-index fixnum = (rem (the eclipse:index (,hash-function ,key)) n-buckets)
	 for again = nil then t
	 for ,index fixnum = original-index
	 then (rem (the eclipse:index (1+ ,index)) n-buckets)
	 for this-key = (svref ,keys ,index)
	 when (and again (= ,index original-index))
	 do (warn "~s (~s buckets): ~s ~s wrapped at index ~s.~%"
		  ,table n-buckets ',hash-function ',test ,index)
	 and do (setq this-key '.empty.)
	 do (case this-key
	      (.empty. ,empty)
	      (.removed. ,removed)
	      (t (when (,test this-key ,key) ,found)))))

(defmacro open-address-gethash (hasher test key table default)
  `(with-open-address-key
    (,hasher ,test ,table index ,key)
    :empty (return ,default)
    :found (return (svref (eclipse::open-address-hash-table-values ,table)
			  index))))

(defmacro open-address-remhash (hasher test key table)
  `(with-open-address-key
    (,hasher ,test ,table index ,key keys)
    :empty (return nil)
    :found (progn (setf (svref keys index) '.removed.)
		  (eclipse::open-address-hash-table-count-setter
		   (1- (eclipse::open-address-hash-table-count ,table)) ,table)
		  (return t))))

(defmacro open-address-rehash (table setter)
  `(let ((keys (eclipse::open-address-hash-table-keys ,table))
	 (values (eclipse::open-address-hash-table-values ,table))
	 (n-buckets (eclipse::open-address-hash-table-n-buckets ,table)))
     (eclipse::resize-hash-table ,table)
     (loop for index fixnum from 0 below n-buckets
	   for this-key = (svref keys index)
	   do (case this-key
		((.empty. .removed.))
		(t (,setter this-key ,table (svref values index)))))
     ,table))

(defmacro open-address-sethash (hasher test key table new-value setter)
  `(let ((count (eclipse::open-address-hash-table-count ,table)))
     (macrolet ((set-val ()
			 `(setf (svref (eclipse::open-address-hash-table-values
					,',table) index)
				,',new-value))
		(set-key ()
			 `(progn (setf (svref keys index) ,',key)
				 (eclipse::open-address-hash-table-count-setter
				  (1+ count) ,',table))))
       (with-open-address-key
	(,hasher ,test ,table index ,key keys)
	:empty (return
		(cond ((>= count (eclipse::open-address-hash-table-size ,table))
		       (open-address-rehash ,table ,setter)
		       (,setter ,key ,table ,new-value))
		      (t (set-key)
			 (set-val))))
	:removed (progn (set-key)
			(return (set-val)))
	:found (return (set-val))))))

(defun eclipse::string-eq (x y) (eclipse::string= x y))

;;; SPECIFIC TO EACH HASH-TABLE-TEST
(locally-fast

(defun eclipse::open-address-eq-gethash (key table default)
  (open-address-gethash eclipse::eq-hash eclipse:eq key table default))
(defun eclipse::open-address-eql-gethash (key table default)
  (open-address-gethash eclipse::eql-hash eclipse:eql key table default))
(defun eclipse::open-address-equal-gethash (key table default)
  (open-address-gethash eclipse::equal-hash eclipse:equal key table default))
(defun eclipse::open-address-equalp-gethash (key table default)
  (open-address-gethash eclipse::equalp-hash eclipse:equalp key table default))
(defun eclipse::open-address-string-gethash (key table default)
  (open-address-gethash eclipse::string-hash eclipse::string-eq key table default))

(defun eclipse::open-address-eq-remhash (key table)
  (open-address-remhash eclipse::eq-hash eclipse:eq key table))
(defun eclipse::open-address-eql-remhash (key table)
  (open-address-remhash eclipse::eql-hash eclipse:eql key table))
(defun eclipse::open-address-equal-remhash (key table)
  (open-address-remhash eclipse::equal-hash eclipse:equal key table))
(defun eclipse::open-address-equalp-remhash (key table)
  (open-address-remhash eclipse::equalp-hash eclipse:equalp key table))

(defun eclipse::open-address-eq-sethash (key table new-value)
  (open-address-sethash eclipse::eq-hash eclipse:eq key table new-value
			eclipse::open-address-eq-sethash))
(defun eclipse::open-address-eql-sethash (key table new-value)
  (open-address-sethash eclipse::eql-hash eclipse:eql key table new-value
			eclipse::open-address-eql-sethash))
(defun eclipse::open-address-equal-sethash (key table new-value)
  (open-address-sethash eclipse::equal-hash eclipse:equal key table new-value
			eclipse::open-address-equal-sethash))
(defun eclipse::open-address-equalp-sethash (key table new-value)
  (open-address-sethash eclipse::equalp-hash eclipse:equalp key table new-value
			eclipse::open-address-equalp-sethash))
(defun eclipse::open-address-string-sethash (key table new-value)
  (open-address-sethash eclipse::string-hash eclipse::string-eq key table new-value
			eclipse::open-address-equalp-sethash))

(defun eclipse::open-address-eq-rehash (table)
  (open-address-rehash table eclipse::open-address-eq-sethash))
(defun eclipse::open-address-eql-rehash (table)
  (open-address-rehash table eclipse::open-address-eql-sethash))
(defun eclipse::open-address-equal-rehash (table)
  (open-address-rehash table eclipse::open-address-equal-sethash))
(defun eclipse::open-address-equalp-rehash (table)
  (open-address-rehash table eclipse::open-address-equalp-sethash))

;;; The following return or accept FIXNUM parameters.
(defun eclipse::first-non-empty-index (hash-table index)
  (loop with keys = (eclipse::open-address-hash-table-keys hash-table)
	for index fixnum from (eclipse:fixnum-index index)
	below (eclipse::open-address-hash-table-n-buckets hash-table)
	do (case (svref keys index)
	     ((.empty. .removed.))
	     (t (return index)))
	finally (return index)))

(defun eclipse::hash-table-key (table index)
  (svref (eclipse::open-address-hash-table-keys table)
	 (eclipse:fixnum-index index)))
(defun eclipse::hash-table-value (table index)
  (svref (eclipse::open-address-hash-table-values table)
	 (eclipse:fixnum-index index)))


)
