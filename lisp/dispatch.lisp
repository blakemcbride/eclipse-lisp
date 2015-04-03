;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                    DISPATCHING                               ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Heavily modified from Water's original code.  Original copyright
;;; is at end of file.

(defstruct (PPRINT-DISPATCH
	    (:conc-name nil) (:copier nil) (:predicate nil)
	    (:print-object
	     (lambda (table stream)
	       (print-unreadable-object (table stream :type t :identity t)))))
  (conses-with-cars (make-hash-table :test #'eq) :type hash-table)
  (dispatch-classes (make-hash-table :test #'eq) :type hash-table)
  (dispatch-others nil :type list))

(defstruct (dispatch-entry
	    (:conc-name entry-)
	    (:copier nil)
	    (:predicate nil))
  (priority nil)
  (pprint-function nil)				;pprint function
  (type-spec nil))

;;; Initial print dispatch table.  Initialized at end of default-dispatch.lisp
(defparameter *ipd* (make-pprint-dispatch))
;;; Value is changed by to a new copy of the modified *ipd* in default-dispatch.lisp
(defparameter *print-pprint-dispatch* (make-pprint-dispatch))

(defun COPY-PPRINT-DISPATCH (&optional (table *print-pprint-dispatch*))
  (when (null table) (setq table *IPD*))
  (make-pprint-dispatch
   :conses-with-cars (copy-hash-table (conses-with-cars table))
   :dispatch-classes (copy-hash-table (dispatch-classes table))
   :dispatch-others (copy-list (dispatch-others table))))

(let ((t-type (find-class 't))
      (limited-cons-type (find-type 'limited-cons-type))
      (type-type (find-type 'type)))
  (defun specifier-category (spec)
    (cond ((and (car-eq spec 'cons)	;(cons ([eql | member ] <x>))
		(null (cddr spec))
		(let* ((second (second spec))
		       (key (and (consp second) (car second))))
		  (when (or (eql key 'member) (eql key 'eql))
		    (null (cddr second)))))
	   'cons-with-car)
	  ((eq spec 'cons) 'other)
	  ((eq spec 't) 'other)
	  ((let ((type (when (symbolp spec) (find-type spec nil))))
	     (and type (not (derived-type-p type))))
	   'class-type)
	  ((and (typep spec limited-cons-type)
		(eq (cons-cdr-type spec) t-type)
		(eql-specializer-p (cons-car-type spec)))
	   'cons-with-car-metaobject)
	  ((typep spec type-type)
	   'type-metaobject)
	  (T 'other))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun SET-PPRINT-DISPATCH (type-specifier function
			    &optional (priority 0) (table *print-pprint-dispatch*))
  (etypecase priority
    (real (set-pprint-dispatch+ type-specifier function priority table))))

(defun enter-in-subtable (entry key subtable)
  (if entry
      (setf (gethash key subtable) entry)
      (remhash key subtable)))

(defun enter-in-others (entry type-specifier table priority)
  (if entry
      (do ((others (dispatch-others table) (cdr others))
	   (last nil others)
	   old-entry)
	  ((or (null others)
	       (priority-> priority (entry-priority
				     (setq old-entry (car
						      others)))))
	   (let ((new-tail (cons entry
				 (delete type-specifier others
					 :test #'equal
					 :key #'entry-type-spec))))
	     (if last
		 (setf (cdr last) new-tail)
		 (setf (dispatch-others table) new-tail))))
	(when (equal type-specifier (entry-type-spec old-entry))
	  (return (setf (car others) entry))))
      (setf (dispatch-others table)
	    (delete type-specifier (dispatch-others table)
		    :test #'equal
		    :key #'entry-type-spec))))

(defun set-pprint-dispatch+ (type-specifier function priority table)
  (let* ((category (specifier-category type-specifier))
	 (entry (when function
		  (make-dispatch-entry :priority priority
				       :pprint-function function
				       :type-spec type-specifier))))
    (case category
      (cons-with-car
       (enter-in-subtable entry (cadadr type-specifier) (conses-with-cars table)))
      (cons-with-car-metaobject
       (enter-in-subtable entry
			  (eql-specializer-object (cons-car-type type-specifier))
			  (conses-with-cars table)))
      (class-type
       (enter-in-subtable entry (find-type type-specifier) (dispatch-classes table)))
      (type-metaobject
       (enter-in-others entry (type-name type-specifier) table priority))
      (t (enter-in-others entry type-specifier table priority))))
  nil)

;;; Initial table entries (which always have lower priority) have
;;; priorities that are wrapped in a list.
(defun priority-> (x y)			
  (if (consp x)
      (if (consp y)
	  (> (car x) (car y))
	  nil)
      (if (consp y)
	  T
	  (> x y))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun PPRINT-DISPATCH (object &optional (table *print-pprint-dispatch*))
  (when (null table) (setq table *IPD*))  
  (let ((fn (get-printer object table)))
    (values (or fn #'print-object-reversed-args) (not (null fn)))))

(defun print-object-reversed-args (s object) (print-object object s))

;;; We assume the the initial table has:
;;; 1. Only list form priorities, greater than (-100)
;;; 2. no dispatch-others entry that has a higher priority than an
;;;    overlapping conses-with-cars or dispatch-classes.
(defun get-printer (object table)
  (let* ((entry1 (if (consp object)
		     (gethash (car object) (conses-with-cars table))
		     (loop with best = nil and priority = '(-100)
			   and classes = (dispatch-classes table)
			   for class in (class-precedence-list-of object)
			   for entry = (gethash class classes)
			   when (and entry (priority-> (entry-priority entry) priority))
			   do (setq best entry priority (entry-priority entry))
			   finally (return best))))
	 (entry (if entry1
		    (loop with priority = (entry-priority entry1)
			  for other in (dispatch-others table)
			  for other-priority = (entry-priority other)
			  until (priority-> priority other-priority)
			  when (fits object other) return other
			  finally (return entry1))
		    (find object (dispatch-others table)
			  :test #'fits))))
    (when entry (entry-pprint-function entry))))

(defun fits (obj entry) (typep obj (entry-type-spec entry)))

;------------------------------------------------------------------------

;Copyright Massachusetts Institute of Technology, Cambridge, Massachusetts.

;Permission to use, copy, modify, and distribute this software and its
;documentation for any purpose and without fee is hereby granted,
;provided that this copyright and permission notice appear in all
;copies and supporting documentation, and that the name of M.I.T. not
;be used in advertising or publicity pertaining to distribution of the
;software without specific, written prior permission. M.I.T. makes no
;representations about the suitability of this software for any
;purpose.  It is provided "as is" without express or implied warranty.

;    M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
;    ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
;    M.I.T. BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
;    ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
;    WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
;    ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
;    SOFTWARE.

;------------------------------------------------------------------------


