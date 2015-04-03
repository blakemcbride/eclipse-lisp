;;; For now, at least, anything which might appear in potentially
;;; interpreted code, such as macroexpansions, must be a lisp:function.

;;; We exect the following from the host, whether it is in C, Java, or
;;; another Lisp:

(deftype eclipse::indices () 't)

#+eclipse
(progn
  (deftype wrapper () 't)
  (deftype slots () 't)
  (deftype emf-table () 't)
  (deftype tagged-instance () 't)
  (deftype standard-instance () 'tagged-instance)
  (deftype funcallable-standard-instance () 'standard-instance)
  (declaim (special *package*)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; RETURNING C PRIMITIVES
(declaim
 ;; ec:VOID
 (ftype (ec:macro (ec:int)		ec:void "<stdlib.h>")	ec:exit)
 (ftype (ec:function (ec:int)		ec:void) eclipse::catch-interrupts
							millisleep)
 (ftype (ec:function (eclipse::open-address-hash-table) 	ec:void)
	eclipse::open-address-eq-rehash eclipse::open-address-eql-rehash
	eclipse::open-address-equal-rehash eclipse::open-address-equalp-rehash)
 (ftype (ec:function (eclipse::emf-table) ec:void)	eclipse::clear-emf-table)
 (ftype (ec:function (t) 		t)		eclipse::drain
							eclipse::flush-input
							eclipse::flush-output
							eclipse::poll
							eclipse::interactivep
							eclipse::file-descriptor-close)

 ;; ec:INT
 (ftype (ec:macro (fixnum) 		ec:int)		eclipse::fixnum-int)
 (ftype (ec:macro (integer) 		ec:int)		eclipse::integer-int)
 (ftype (ec:macro (character) 		ec:int)		eclipse:character-int)
 (ftype (ec:macro (character) 		ec:wint_t)	eclipse:character-wint)
 (ftype (ec:macro (t)			ec:int)		eclipse::object-address
							eclipse::object-word)
 (ftype (ec:macro (ec:wint_t) 	ec:int "\"chars.h\"")	ec:iswprint ec:iswalpha
							ec:iswupper ec:iswlower
							ec:iswalnum)
 (ftype (ec:macro (ec:wint_t) 	ec:wint_t "\"chars.h\"")
							ec:towupper ec:towlower)
 ;;!!!(ftype (ec:function ()			ec:long)	eclipse::run-time)
 ;;!!!(ftype (ec:function (t)		ec:long)	eclipse::real-time)
 ;; file-descriptor-xxx uses c:lseek which uses c:off_t.
 (ftype (ec:function (t) 		ec:int) 	eclipse::file-descriptor-position
							eclipse::file-descriptor-length)
 (ftype (ec:function (t ec:int) 	ec:int)		eclipse::file-descriptor-seek)

 ;; ec:CHAR
 (ftype (ec:macro (base-char) 		ec:char)	eclipse::character-char)
 (ftype (ec:macro (eclipse:simple-base-string eclipse:index)
		  			ec:char) 	eclipse::base-char-elt)
 (ftype (ec:macro (eclipse:simple-base-string eclipse:index ec:char)
		  			ec:char) 	eclipse::set-base-char-elt)

 ;; ec:WCHAR_T
 (ftype (ec:macro (character) 		ec:wchar_t)	eclipse::character-wchar)
 ;; It is an error to call either of the next two with a simple-base-string.
 (ftype (ec:macro (eclipse::simple-extended-string eclipse:index)
		  			ec:wchar_t) 	eclipse::extended-char-elt)
 (ftype (ec:macro (eclipse::simple-extended-string eclipse:index ec:wchar_t)
		  			ec:wchar_t) 	eclipse::set-extended-char-elt)

 ;; ec:FLOAT
 (ftype (ec:macro (single-float) 	ec:float)	eclipse:single-float-float)

 ;; ec:DOUBLE
 (ftype (ec:macro (double-float) 	ec:double)	eclipse:double-float-double)
 (ftype (ec:macro (single-float) 	ec:double)	eclipse:single-float-double)

 ;; eclipse:CHARP
 (ftype (ec:macro () 			eclipse:charp)	eclipse::eclipse-version)
 (ftype (ec:macro (eclipse:simple-base-string) eclipse:charp) eclipse::simple-base-string-charp)
 (ftype (ec:function (eclipse:pathname)	eclipse:charp) 	eclipse::file-owner
							eclipse::follow-link)
 ;; argument is a directory descriptor returned by open-dir.
 (ftype (ec:function (t eclipse:simple-string) eclipse:charp) eclipse::read-dir)

 ;; SLOTS
 (ftype (ec:function (fixnum)		eclipse::slots) eclipse::make-slots)
 (ftype (ec:function (&rest t)		eclipse::slots) eclipse::make-static-slots))
#+eclipse (declaim
 (ftype (ec:macro (eclipse::standard-instance)		  
		  		eclipse::slots) 	eclipse::standard-instance-slots)
 (ftype (ec:macro (eclipse::standard-instance eclipse::slots)
		  		eclipse::slots) 	eclipse::set-standard-instance-slots)
 (ftype (ec:macro (eclipse::open-address-hash-table)
		  			eclipse::slots) eclipse::open-address-hash-table-values)
 (ftype (ec:macro (eclipse::slots eclipse::open-address-hash-table)
		  			eclipse::slots)
					        eclipse::open-address-hash-table-values-setter)

 ;; INDICES
 (ftype (ec:macro (eclipse::open-address-hash-table)	eclipse::indices)
							eclipse::open-address-hash-table-keys)
 (ftype (ec:macro (eclipse::indices eclipse::open-address-hash-table)
		  			eclipse::indices)
						eclipse::open-address-hash-table-keys-setter)
 
 ;; WRAPPER
 (ftype (ec:macro (t) 		eclipse::wrapper) 	eclipse::object-wrapper)
 (ftype (ec:macro (eclipse::tagged-instance)
		  		eclipse::wrapper) 	eclipse::tagged-instance-wrapper))
(declaim
 (ftype (ec:function (list)	eclipse::wrapper)	eclipse::make-wrapper)
 (ftype (ec:macro (eclipse::tagged-instance eclipse::wrapper)
		  		eclipse::wrapper) 	eclipse::set-tagged-instance-wrapper)

 ;; STANDARD-INSTANCES
 (ftype (ec:function (t eclipse::wrapper eclipse::slots)
		     eclipse::standard-instance)
	                                eclipse::make-standard-instance-from-slots
		   			eclipse::make-funcallable-standard-instance-from-slots)
 (ftype (ec:function (fixnum t eclipse::wrapper)
		     eclipse::standard-instance)	eclipse::make-standard-instance)
 (ftype (ec:function (fixnum t eclipse::wrapper)
		     eclipse::funcallable-standard-instance)
						eclipse::make-funcallable-standard-instance)
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; RETURNING TYPES THAT MIGHT OR MIGHT NOT BE C PRIMITIVES
(declaim
 ;; BOOLEAN
 (ftype (ec:macro (t) 			boolean)	eclipse::unboundp)
 (ftype (ec:macro (eclipse::wrapper) 	boolean)	eclipse::wrapper-invalid-p)
 (ftype (ec:function (eclipse:pathname eclipse:pathname) t)	eclipse::file-rename)
 (ftype (ec:function (eclipse:pathname)	t)		eclipse::file-delete
							eclipse::dir-delete
							eclipse::make-dir)
 ;; argument is a directory descriptor returned by open-dir.
 (ftype (ec:function (t) 		t) 		eclipse::close-dir)

 ;; INDEX
 (ftype (ec:function (t)		eclipse:index)	eclipse:integer-index)
 (ftype (ec:macro (t)			fixnum)		eclipse::eq-hash)
 (ftype (ec:function (t)		fixnum)		eclipse::string-hash)
 (ftype (ec:macro (eclipse:simple-bit-vector eclipse:index)
		     			eclipse:index)	eclipse::indexref
							eclipse::bit-elt)
 (ftype (ec:macro (eclipse:simple-bit-vector eclipse:index eclipse:index)
		     			eclipse:index)	eclipse::set-indexref
							eclipse::set-bit-elt)
 (ftype (ec:macro (eclipse::simple-basic-vector) eclipse:index) eclipse::vector-size)
 ;; It is an error to call either of the next two on a vector.
 (ftype (ec:macro (eclipse:array)	eclipse:index)	eclipse::simple-array-rank)
 (ftype (ec:macro (eclipse:array eclipse:index) eclipse:index)
							eclipse::simple-array-dimension))
#+eclipse (declaim
 (ftype (ec:macro (fixnum)		eclipse:index)	eclipse:fixnum-index)
 (ftype (ec:macro (fixnum fixnum)	fixnum)		eclipse::merge-hash-codes)
 (ftype (ec:macro (eclipse::wrapper)	eclipse:index)	eclipse::wrapper-hash-key)
 (ftype (ec:macro (eclipse::open-address-hash-table)
		  			eclipse:index)
							eclipse::open-address-hash-table-n-buckets
							eclipse::open-address-hash-table-size
							eclipse::open-address-hash-table-count)
 (ftype (ec:macro (eclipse:index eclipse::open-address-hash-table)
		  			eclipse:index)
						eclipse::open-address-hash-table-n-buckets-setter
						eclipse::open-address-hash-table-size-setter
						eclipse::open-address-hash-table-count-setter))
(declaim
 ;; XINT  (i.e. a bignum integer pretending to be a simple-bit-vector)
 (ftype (ec:function (fixnum)		eclipse:simple-bit-vector)
		     					eclipse::make-xint)
 (ftype (ec:function (eclipse:simple-bit-vector ec:int) eclipse:simple-bit-vector)
		     					eclipse::nload-xint)
 (ftype (ec:function (eclipse:simple-bit-vector fixnum) eclipse:simple-bit-vector)
		     					eclipse::trim-xint)
 (ftype (ec:function (integer)		eclipse:simple-bit-vector)	
							eclipse::integer-xint)
 (ftype (ec:macro (bignum)		 eclipse:simple-bit-vector)
							eclipse::bignum-xint)

 ;; directory-descriptor (open-dir) or file-descriptor
 (ftype (ec:function (eclipse:pathname)	t)		eclipse::open-dir
							eclipse::read-open
							eclipse::write-open
							eclipse::read-write-open)
 
 ;; EMF-TABLE
 (ftype (ec:function () 		eclipse::emf-table) eclipse::make-emf-table))
#+eclipse (declaim
 (ftype (ec:macro (eclipse::funcallable-standard-instance)
		  			eclipse::emf-table) eclipse::emf-table))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; RETURNING LISP OBJECT
(declaim
 ;; NULL
 (ftype (ec:function (eclipse:function eclipse::open-address-hash-table)
		     			null)		eclipse::open-address-maphash) 
 ;; CHARACTER
 (ftype (ec:macro (ec:int) 		character)	eclipse:int-character)
 (ftype (ec:macro (ec:wint_t) 		character)	eclipse:wint-character)
 (ftype (ec:macro (ec:char) 		character)	eclipse::char-character)
 (ftype (ec:macro (ec:wchar_t) 		character)	eclipse::wchar-character)
 
 ;; INTEGER
 (ftype (ec:function (t eclipse:string fixnum fixnum)
		     			fixnum)
		eclipse::write-ascii eclipse::write-mb
		eclipse::write-ucs2 eclipse::write-ucs4
		eclipse::write-beu8 eclipse::write-beu16 eclipse::write-beu32
		eclipse::write-leu8 eclipse::write-leu16 eclipse::write-leu32
		eclipse::write-bes8 eclipse::write-bes16 eclipse::write-bes32
		eclipse::write-les8 eclipse::write-les16 eclipse::write-les32)
 (ftype (ec:function (t eclipse:string fixnum fixnum)
		     			fixnum)
		eclipse::read-ascii 
		eclipse::read-ucs2 eclipse::read-ucs4
		eclipse::read-beu8 eclipse::read-beu16 eclipse::read-beu32
		eclipse::read-leu8 eclipse::read-leu16 eclipse::read-leu32
		eclipse::read-bes8 eclipse::read-bes16 eclipse::read-bes32
		eclipse::read-les8 eclipse::read-les16 eclipse::read-les32)
 (ftype (ec:function (t eclipse:string fixnum fixnum eclipse:string)
		     			fixnum)		eclipse::read-mb)
 (ftype (ec:macro (eclipse::complex-array) fixnum)	eclipse::complex-array-offset
							eclipse::complex-array-fill-pointer)
 (ftype (ec:macro (eclipse::complex-array fixnum) fixnum) eclipse::set-complex-array-fill-pointer)
 (ftype (ec:macro (eclipse::simple-bit-vector) fixnum)	eclipse::xint-size)
 (ftype (ec:macro (ec:int) 		fixnum)		eclipse:int-fixnum)
 (ftype (ec:macro (eclipse:index) 	fixnum)		eclipse:index-fixnum)
 (ftype (ec:macro (fixnum fixnum)	fixnum)		eclipse::rem-fixnum
							eclipse::truncate-fixnum
							eclipse::logand-fixnum
							eclipse::logxor-fixnum
							eclipse::logior-fixnum)
 (ftype (ec:function (fixnum fixnum)	fixnum)		eclipse::ash-fixnum)
 (ftype (ec:macro (fixnum)		fixnum)		eclipse::lognot-fixnum)
 (ftype (ec:function (fixnum)		fixnum)		eclipse::logcount-fixnum
							eclipse::length-fixnum)
 (ftype (ec:function (integer)		fixnum)		eclipse::digit-low
							eclipse::digit-high)
 (ftype (ec:macro (eclipse:simple-bit-vector eclipse:index)
		     			fixnum)		eclipse::digitref)
 (ftype (ec:macro (eclipse:simple-bit-vector eclipse:index fixnum)
		     			fixnum)		eclipse::set-digitref)
 (ftype (ec:macro (eclipse:simple-bit-vector) fixnum) eclipse::xint-fixnum)

 (ftype (ec:function (eclipse::open-address-hash-table fixnum)
		     			fixnum)		eclipse::first-non-empty-index
							eclipse::hash-table-key)
 
 (ftype (ec:macro (eclipse:simple-bit-vector) bignum) eclipse::xint-bignum)
 (ftype (ec:function (ec:int) 		integer)	eclipse:int-integer)
 (ftype (ec:macro (ratio) 		integer)	eclipse:ratio-numerator
							eclipse:ratio-denominator)
 (ftype (ec:function (integer integer)	integer)	eclipse::add-integer-integer
							eclipse::subt-integer-integer
							eclipse::mult-integer-integer)
 (ftype (ec:function (single-float)	integer)	eclipse::truncate-single-float)
 (ftype (ec:function (double-float)	integer)	eclipse::truncate-double-float)
 (ftype (function ()		(or integer null))	eclipse::machine-id)
 (ftype (ec:function ()			integer)	eclipse::unix-time)
 ;; uses c:stat, which produces c:time_t
 (ftype (ec:function (eclipse:pathname)	(or integer null)) eclipse::file-modified)
 
 ;; ratio
 (ftype (ec:function (integer integer)	ratio)		eclipse:make-ratio)

 ;; SINGLE-FLOAT
 (ftype (ec:function (ec:float) 	single-float)	eclipse:float-single-float)
 (ftype (ec:function (ec:double) 	single-float)	eclipse:double-single-float)
 (ftype (ec:function (single-float single-float) single-float)
	eclipse::add-single-float-single-float eclipse::subt-single-float-single-float
	eclipse::mult-single-float-single-float eclipse::div-single-float-single-float)
 (ftype (ec:function (integer single-float) single-float)
	eclipse::add-integer-single-float eclipse::subt-integer-single-float
	eclipse::mult-integer-single-float eclipse::div-integer-single-float)
 (ftype (ec:function (ratio single-float) single-float)
	eclipse::add-ratio-single-float eclipse::subt-ratio-single-float
	eclipse::mult-ratio-single-float eclipse::div-ratio-single-float)
 (ftype (ec:function (single-float integer) single-float)
	eclipse::add-single-float-integer eclipse::subt-single-float-integer
	eclipse::mult-single-float-integer eclipse::div-single-float-integer
	eclipse::scale-float-single-float-integer)
 (ftype (ec:function (single-float ratio) single-float)
	eclipse::add-single-float-ratio eclipse::subt-single-float-ratio
	eclipse::mult-single-float-ratio eclipse::div-single-float-ratio)
 (ftype (ec:function (integer)		single-float)
	eclipse::exp-integer eclipse::sqrt1-integer eclipse::log-integer
	eclipse::sin-integer eclipse::asin1-integer
	eclipse::sinh-integer eclipse::asinh-integer
	eclipse::cos-integer eclipse::acos1-integer
	eclipse::cosh-integer eclipse::acosh1-integer
	eclipse::tan-integer eclipse::atan1-integer
	eclipse::tanh-integer eclipse::atanh1-integer)
 (ftype (ec:function (ratio)		single-float)
	eclipse::exp-ratio eclipse::sqrt1-ratio eclipse::log-ratio
	eclipse::sin-ratio eclipse::asin1-ratio
	eclipse::sinh-ratio eclipse::asinh-ratio
	eclipse::cos-ratio eclipse::acos1-ratio
	eclipse::cosh-ratio eclipse::acosh1-ratio
	eclipse::tan-ratio eclipse::atan1-ratio
	eclipse::tanh-ratio eclipse::atanh1-ratio)
 (ftype (ec:function (single-float)		single-float)
	eclipse::exp-single-float eclipse::sqrt1-single-float eclipse::log-single-float
	eclipse::sin-single-float eclipse::asin1-single-float
	eclipse::sinh-single-float eclipse::asinh-single-float
	eclipse::cos-single-float eclipse::acos1-single-float
	eclipse::cosh-single-float eclipse::acosh1-single-float
	eclipse::tan-single-float eclipse::atan1-single-float
	eclipse::tanh-single-float eclipse::atanh1-single-float
	eclipse::fceiling-single-float eclipse::ffloor-single-float
	eclipse::ftruncate-single-float eclipse::fround-single-float)
 ;; Note: no expt2-xxx-integer
 (ftype (ec:function (integer integer)	single-float)	eclipse::atan2-integer-integer)
 (ftype (ec:function (single-float integer) single-float)
							eclipse::atan2-single-float-integer)
 (ftype (ec:function (integer ratio)	single-float)	eclipse::expt2-integer-ratio
							eclipse::atan2-integer-ratio)
 (ftype (ec:function (ratio integer)	single-float)	eclipse::atan2-ratio-integer)
 (ftype (ec:function (ratio ratio)	single-float)	eclipse::expt2-ratio-ratio
							eclipse::atan2-ratio-ratio)
 (ftype (ec:function (single-float ratio) single-float)	eclipse::expt2-single-float-ratio
							eclipse::atan2-single-float-ratio)
 (ftype (ec:function (integer single-float)	single-float)
							eclipse::expt2-integer-single-float
							eclipse::atan2-integer-single-float)
 (ftype (ec:function (ratio single-float)	single-float)	eclipse::expt2-ratio-single-float
							eclipse::atan2-ratio-single-float)
 (ftype (ec:function (single-float single-float) single-float)
							eclipse::expt2-single-float-single-float
							eclipse::atan2-single-float-single-float)
 (ftype (ec:function (single-float single-float) single-float)
						eclipse::float-sign2-single-float-single-float)
 (ftype (ec:function (double-float single-float) single-float)
						eclipse::float-sign2-double-float-single-float)
 ;; DOUBLE-FLOAT
 (ftype (ec:function (ec:double) 	double-float)	eclipse:double-double-float)
 (ftype (ec:function (single-float double-float) double-float)
	eclipse::add-single-float-double-float eclipse::subt-single-float-double-float
	eclipse::mult-single-float-double-float eclipse::div-single-float-double-float)
 (ftype (ec:function (double-float single-float) double-float)
	eclipse::add-double-float-single-float eclipse::subt-double-float-single-float
	eclipse::mult-double-float-single-float eclipse::div-double-float-single-float)
 (ftype (ec:function (double-float double-float) double-float)
	eclipse::add-double-float-double-float eclipse::subt-double-float-double-float
	eclipse::mult-double-float-double-float eclipse::div-double-float-double-float)
 (ftype (ec:function (integer double-float) double-float)
	eclipse::add-integer-double-float eclipse::subt-integer-double-float
	eclipse::mult-integer-double-float eclipse::div-integer-double-float)
 (ftype (ec:function (ratio double-float) double-float)
	eclipse::add-ratio-double-float eclipse::subt-ratio-double-float
	eclipse::mult-ratio-double-float eclipse::div-ratio-double-float)
 (ftype (ec:function (double-float integer) double-float)
	eclipse::add-double-float-integer eclipse::subt-double-float-integer
	eclipse::mult-double-float-integer eclipse::div-double-float-integer
	eclipse::scale-float-double-float-integer)
 (ftype (ec:function (double-float ratio) double-float)
	eclipse::add-double-float-ratio eclipse::subt-double-float-ratio
	eclipse::mult-double-float-ratio eclipse::div-double-float-ratio)
 (ftype (ec:function (double-float)		double-float)
	eclipse::exp-double-float eclipse::sqrt1-double-float eclipse::log-double-float
	eclipse::sin-double-float eclipse::asin1-double-float
	eclipse::sinh-double-float eclipse::asinh-double-float
	eclipse::cos-double-float eclipse::acos1-double-float
	eclipse::cosh-double-float eclipse::acosh1-double-float
	eclipse::tan-double-float eclipse::atan1-double-float
	eclipse::tanh-double-float eclipse::atanh1-double-float
	eclipse::fceiling-double-float eclipse::ffloor-double-float
	eclipse::ftruncate-double-float eclipse::fround-double-float)
 (ftype (ec:function (double-float ratio) double-float)	eclipse::expt2-double-float-ratio
							eclipse::atan2-double-float-ratio)
 (ftype (ec:function (integer double-float)	double-float)
							eclipse::expt2-integer-double-float
							eclipse::atan2-integer-double-float)
 (ftype (ec:function (ratio double-float)	double-float)	eclipse::expt2-ratio-double-float
							eclipse::atan2-ratio-double-float)
 (ftype (ec:function (double-float integer)	double-float)
							eclipse::atan2-double-float-integer)
 (ftype (ec:function (double-float double-float) double-float)
						eclipse::expt2-double-float-double-float
						eclipse::atan2-double-float-double-float
						eclipse::float-sign2-double-float-double-float)
 (ftype (ec:function (single-float double-float) double-float)
						eclipse::expt2-single-float-double-float
						eclipse::atan2-single-float-double-float
						eclipse::float-sign2-single-float-double-float)
 (ftype (ec:function (double-float single-float) double-float)
						eclipse::expt2-double-float-single-float
						eclipse::atan2-double-float-single-float)
 
 ;; REAL
 (ftype (ec:macro (complex)		real)		eclipse:complex-realpart
							eclipse:complex-imagpart))
#+eclipse (declaim
 (ftype (ec:macro (eclipse::open-address-hash-table)	real)
					eclipse::open-address-hash-table-rehash-size)
 (ftype (ec:macro (real eclipse::open-address-hash-table) real)
					eclipse::open-address-hash-table-rehash-size-setter))

(declaim
 ;; COMPLEX
 (ftype (ec:function (real real) 	complex)	eclipse:make-complex)

 ;; SYMBOL
 (ftype (function (eclipse:string)	symbol)		eclipse:make-symbol)
 (ftype (ec:function (eclipse:pathname)	symbol)		eclipse::file-type)
 (ftype (ec:function (ec:int)		symbol)		eclipse::fpe-class))
#+eclipse (declaim
 (ftype (ec:macro (t)			symbol)		eclipse::instance-tag)

 ;; LIST
 (ftype (ec:macro (t)			list)		eclipse::instance-cpl))
(declaim
 (ftype (function (t t)			cons)		cons)
 (ftype (function (&rest t)		list)		eclipse:list)
 (ftype (function (symbol)		list)		eclipse:symbol-plist)
 (ftype (ec:function (symbol list)	list)		eclipse::set-symbol-plist)
 (ftype (ec:macro (eclipse::wrapper) 	list) 		eclipse::wrapper-obsolete-slots)

 ;; simple basic vectors
 (ftype (ec:function (eclipse:index)	eclipse:simple-base-string) eclipse::make-base-char-vector)
 (ftype (ec:function (eclipse:index)	eclipse::simple-extended-string)
							eclipse::make-extended-char-vector)
 (ftype (ec:function (eclipse:index)	eclipse:simple-vector) eclipse::make-general-vector)
 (ftype (ec:function (eclipse:index)	eclipse:simple-bit-vector) eclipse::make-bit-vector)

 ;; STRING
 (ftype (ec:function (eclipse:charp) simple-base-string) eclipse::charp-simple-base-string)
 (ftype (function (symbol)		eclipse:string)	eclipse:symbol-name)
 (ftype (function ()			(or eclipse:string null))	eclipse::getcwd)
 (ftype (function (eclipse:string)	(or eclipse:string null))	eclipse::getenv)
 (ftype (ec:function (ec:int)		eclipse:string) eclipse::uname)

 ;; ARRAY
 ;; It is an error to call this on a vector.
 (ftype (ec:macro (eclipse:array)	eclipse:array)	eclipse::simple-array-contents)
 (ftype (ec:function (eclipse:index list eclipse::simple-basic-vector)
		     			eclipse:simple-array) eclipse::make-simple-array)
 (ftype (ec:function (t eclipse:index list eclipse:array fixnum fixnum)
		     			eclipse:array) eclipse::make-complex-array)
 (ftype (ec:function (eclipse::complex-array eclipse:array eclipse:index list fixnum fixnum)
		     			eclipse::complex-array) eclipse::update-array)

 ;; PACKAGE
 (ftype (ec:macro (symbol) (or eclipse:package null)) 	eclipse::symbol-package-value)
 (ftype (ec:macro ((or eclipse:package null) symbol) eclipse:package)
							eclipse::symbol-package-setter)

 ;; HASH-TABLE
 (ftype (ec:function (eclipse::open-address-hash-table) eclipse::open-address-hash-table)
							eclipse::open-address-clrhash)
 (ftype (ec:function (t eclipse:index real eclipse:index) eclipse::open-address-hash-table)
							eclipse::make-open-address-hash-table)
 ;; FUNCTION
 (ftype (ec:function (eclipse::emf-table fixnum list function)
		  			function) 	eclipse::emf-table-set)
 (ftype (ec:macro (eclipse::funcallable-standard-instance)
		  		function) eclipse::funcallable-standard-instance-function)
 (ftype (ec:function (eclipse::funcallable-standard-instance function)
		  		function) eclipse::set-funcallable-standard-instance-function)
 (ftype (ec:function (fixnum t t &rest t) eclipse:function)	eclipse::make-closure)
 (ftype (function (symbol eclipse:function)	eclipse:function)
							eclipse::make-macro-function
							eclipse::macro-function-function)
	 
 ;; (OR FUNCTION NULL)
 (ftype (ec:macro (symbol) (or eclipse:function null))	eclipse::symbol-function-value
							eclipse::symbol-setf-function-value
							eclipse::setf-expander)
 (ftype (ec:macro (symbol (or eclipse:function null)) (or eclipse:function null))
		  					eclipse::set-symbol-function-value
							eclipse::set-symbol-setf-function-value
							eclipse::set-setf-expander))
#+eclipse (declaim
 (ftype (ec:function (eclipse::emf-table fixnum list)
		     (or function null))		eclipse::emf-table-get)
 (ftype (ec:function (t list)			eclipse::slot-definition)
							eclipse::fast-find-slot))

(declaim
 ;; t (boolean)
 (ftype (ec:macro (ec:int)			t)	eclipse::test)
 (ftype (ec:macro (t)				t)	eclipse::presentp)
 (ftype (function (t)				t)	eclipse:fixnump)
 (ftype (ec:macro (fixnum fixnum)	t)		eclipse::logbitp-fixnum
 							eclipse::logtest-fixnum)
 (ftype (ec:macro (fixnum)		t)		eclipse::signp-digit)
 (ftype (ec:function (integer integer)	t)
	eclipse::eq-integer-integer
	eclipse::gt-integer-integer eclipse::lt-integer-integer
	eclipse::ge-integer-integer eclipse::le-integer-integer)
 (ftype (ec:function (integer single-float)	t)
	eclipse::eq-integer-single-float
	eclipse::gt-integer-single-float eclipse::lt-integer-single-float
	eclipse::ge-integer-single-float eclipse::le-integer-single-float)
 (ftype (ec:function (integer double-float)	t)
	eclipse::eq-integer-double-float
	eclipse::gt-integer-double-float eclipse::lt-integer-double-float
	eclipse::ge-integer-double-float eclipse::le-integer-double-float)
 (ftype (ec:function (single-float integer)	t)
	eclipse::eq-single-float-integer
	eclipse::gt-single-float-integer eclipse::lt-single-float-integer
	eclipse::ge-single-float-integer eclipse::le-single-float-integer)
 (ftype (ec:function (single-float single-float)	t)
	eclipse::eq-single-float-single-float
	eclipse::gt-single-float-single-float eclipse::lt-single-float-single-float
	eclipse::ge-single-float-single-float eclipse::le-single-float-single-float)
 (ftype (ec:function (single-float double-float)	t)
	eclipse::eq-single-float-double-float
	eclipse::gt-single-float-double-float eclipse::lt-single-float-double-float
	eclipse::ge-single-float-double-float eclipse::le-single-float-double-float)
 (ftype (ec:function (double-float integer)	t)
	eclipse::eq-double-float-integer
	eclipse::gt-double-float-integer eclipse::lt-double-float-integer
	eclipse::ge-double-float-integer eclipse::le-double-float-integer)
 (ftype (ec:function (double-float single-float)	t)
	eclipse::eq-double-float-single-float
	eclipse::gt-double-float-single-float eclipse::lt-double-float-single-float
	eclipse::ge-double-float-single-float eclipse::le-double-float-single-float)
 (ftype (ec:function (double-float double-float)	t)
	eclipse::eq-double-float-double-float
	eclipse::gt-double-float-double-float eclipse::lt-double-float-double-float
	eclipse::ge-double-float-double-float eclipse::le-double-float-double-float)
 (ftype (function (t)			t)		eclipse::macro-function-p)
 (ftype (ec:function (t eclipse::open-address-hash-table) t)
	eclipse::open-address-eq-remhash eclipse::open-address-eql-remhash
	eclipse::open-address-equal-remhash eclipse::open-address-equalp-remhash))
 
 ;; T (objects)
#+eclipse (declaim 
 (ftype (ec:macro (eclipse::slots eclipse:index) t)	eclipse::get-slot)
 (ftype (ec:macro (eclipse::slots eclipse:index t) t)	eclipse::set-slot)
 (ftype (ec:macro (eclipse::tagged-instance) 	t)	eclipse::tagged-instance-class)
 (ftype (ec:function (t list)			t)	eclipse::fast-find)) 
(declaim
 (ftype (ec:macro (eclipse::tagged-instance t)	t)	eclipse::set-tagged-instance-class)
 (ftype (ec:macro (eclipse::wrapper)		t)	eclipse::make-wrapper-obsolete)
 (ftype (ec:macro (t)				t)	eclipse::vp eclipse::vpop
							eclipse::vargs
							eclipse::vlist
							eclipse::get-object-arg)
 (ftype (ec:macro (ec:int)			t)	eclipse::word-object)
 (ftype (function (list)			t)	eclipse:car eclipse:cdr)
 (ftype (ec:macro (eclipse:simple-vector eclipse:index) t) eclipse::general-elt)
 (ftype (ec:macro (eclipse:simple-vector eclipse:index t) t) eclipse::set-general-elt)
 (ftype (ec:function (eclipse::open-address-hash-table fixnum)
		     				t)	eclipse::hash-table-value)
 (ftype (ec:macro (symbol)			t)	eclipse::symbol-value-value)
 (ftype (ec:macro (symbol t)			t)	eclipse::set-symbol-value-value)
 (ftype (ec:macro (t)		 		eclipse::standard-instance)
							eclipse::object-class)
 (ftype (ec:function (symbol symbol t)		t)	eclipse::system-property
							eclipse::system-property-setter)
 (ftype (ec:function (t eclipse::open-address-hash-table t) t)
	eclipse::open-address-eq-gethash eclipse::open-address-eq-sethash
	eclipse::open-address-eql-gethash eclipse::open-address-eql-sethash
	eclipse::open-address-equal-gethash eclipse::open-address-equal-sethash
	eclipse::open-address-equalp-gethash eclipse::open-address-equalp-sethash)
 (ftype (ec:function (eclipse:string eclipse::open-address-hash-table symbol) symbol)
	eclipse::open-address-string-gethash eclipse::open-address-string-sethash)

 ;; never return
 (ftype (function (t)			t)		eclipse::extra-args
							eclipse::missing-args)
 
 ;; VALUES
 (ftype (function (&rest t)			(values &rest t)) eclipse:values)
 (ftype (function (eclipse::function-designator &rest t)
		  				(values &rest t)) eclipse:apply)
 (ftype (function (single-float) (values single-float integer single-float))
							eclipse::decode-single-float)
 (ftype (function (double-float) (values double-float integer double-float))
							eclipse::decode-double-float)
 (ftype (ec:function () (values integer integer)) 	eclipse::gc-data)
 (ftype (ec:function (integer) (values integer boolean)) eclipse::get-timezone)
 )

#+eclipse (declaim
 (ftype (function (function &rest t)		(values &rest t))
							eclipse::apply-function
							eclipse::funcall-function))
