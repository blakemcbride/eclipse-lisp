
(defparameter *curent-level* 0)
(defparameter *curent-length* 0)

;;; Funcalling this redoes the last xp printing that was abbreviated.
(defparameter *last-abbreviated-printing*
  #'(lambda (&optional stream) (declare (ignore stream)) nil))
(defparameter *abbreviation-happened* nil)

(defun print-object-aborted-p (object stream)
  (cond ((and *print-level* (>= *current-level* *print-level*))
	 (write-char #\# stream)
	 (setq *abbreviation-happened* '*print-level*))
	(*print-circle*
	 (print-circular-p object stream nil))))

;;; Among other things, when circlep is true, this does circularity on
;;; OBJECT, not on an element of object.  For normal checking of
;;; interior cdrs by PPRINT-POP, the first time this is called,
;;; *current-length* will be 0, and the (top level) object will have
;;; already been tested for circularity by pprint-logical-block.
;;; Because the default value of circlep in later calls will depend on
;;; *current-length* getting advanced, we must increment
;;; *current-length* even if we are not comparing against
;;; *print-length*.

(defun print-element-aborted-p (object stream &optional (listp t)
				       (circlep (plusp *current-length*)))
  (incf *current-length*)
  (cond ((and listp (not (listp object)))
	 (write-string ". " stream)
	 (write-toplevel object stream))
	((and *print-length* (> *current-length* *print-length*))
	 (write-string "..." stream)
	 (setq *abbreviation-happened* '*print-length*))
	((and circlep *print-circle*)
	 (print-circular-p object stream listp))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; We extend the CLOS STREAM protocol with the following:
(defgeneric STREAM-NEWLINE (stream kind))
(defgeneric STREAM-INDENT (stream kind n))
(defgeneric STREAM-TAB (stream kind colnum colinc))
(defgeneric STREAM-PPRINT (stream object printer
				  &key prefix suffix per-line-p))

(defmethod stream-newline ((stream stream) kind)
  (case kind
    (:unconditional (stream-terpri stream))
    (:fresh (stream-fresh-line stream))))

(defmethod stream-indent ((stream stream) kind n)
  (declare (ignore kind n)) nil)

;;; The smallest number >= x that is a multiple of colinc
;;;  (* colinc (floor (+ x (1- colinc)) colinc))
(defun tab-position (current relativep colnum colinc)
  (if (zerop colinc)
      (if relativep
	  (+ current colnum)
	  (max colnum current))
      (cond (relativep
	     (* colinc (floor (+ current colnum colinc -1) colinc)))
	    ((> colnum current) colnum)
	    (T (+ colnum
		  (* colinc
		     (floor (+ current (- colnum) colinc) colinc)))))))

(defmethod stream-tab ((stream stream) kind colnum colinc)
  (stream-advance-to-column
   stream
   (tab-position (or (stream-line-column stream) 0)
		 (case kind
		   (:line nil)
		   (:line-relative t)
		   (t (return-from stream-tab nil)))
		 colnum colinc)))

(defresource PRETTY-PRINTING-STREAM (stream)
  :constructor (make-instance 'pretty-printing-stream
			      :stream stream)
  :initializer (initialize-pp pretty-printing-stream stream)
  :matcher t)
  

;;; If we don't already have a pretty printing stream, create one.
(defmethod stream-pprint ((stream STREAM) object printer
			  &rest args)
  (if (and *print-circle* (null *circularity-map*))

      (with-two-circularity-passes (stream)
	(apply #'stream-pprint stream object printer args))
      
      (using-resource (xp PRETTY-PRINTING-STREAM stream)
	(let ((*abbreviation-happened* nil))
	  (catch 'line-limit-abbreviation-exit
	    (apply #'stream-pprint xp object printer args))
	  (when (catch 'line-limit-abbreviation-exit
		  (attempt-to-output xp nil T) nil)
	    (attempt-to-output xp T T))
	  (when *abbreviation-happened* 
	    (setq *last-abbreviated-printing*
		  (lambda (&optional (str stream))
		    (apply #'stream-pprint str object printer args))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TOP LEVEL FUNCTIONAL INTERFACE

(defun PPRINT-TOPLEVEL (object stream
			       prefix suffix per-line-p
			       printer)
  (if (listp object)
      (stream-pprint stream object printer
		     :prefix prefix :suffix suffix
		     :per-line-p per-line-p)
      (write-toplevel object stream))
  nil)

(defun PPRINT-NEWLINE (kind &optional stream)
  (stream-newline (output-stream stream)
		  (ecase kind
		    ((:linear :miser :fill :mandatory) kind)))
  nil)

(defun PPRINT-INDENT (relative-to n &optional stream)
  (stream-indent (output-stream stream)
		 (ecase relative-to
		   ((:block :current) relative-to))
		 n)
  nil)

(defun PPRINT-TAB (kind colnum colinc &optional stream)
  (stream-tab (output-stream stream)
	      (ecase kind
		((:line :section :line-relative :section-relative) kind))
	      colnum colinc)
  nil)

(defun pprint-style (stream list colonp style tabsize
		       &aux (prefix (if colonp "(" ""))
		       (suffix (if colonp ")" "")))
  (pprint-logical-block (stream list :prefix prefix :suffix suffix)
    (pprint-exit-if-list-exhausted)
    (loop (write-toplevel (pprint-pop) stream)
	  (pprint-exit-if-list-exhausted)
	  (write-char++ #\space stream)
	  (when tabsize (stream-tab stream :section-relative 0 tabsize))
	  (stream-newline stream style))))

(defun PPRINT-LINEAR (s list &optional (colon? T) atsign?)
  (declare (ignore atsign?))
  (pprint-style s list colon? :linear nil))

(defun PPRINT-FILL (s list &optional (colon? T) atsign?)
  (declare (ignore atsign?))
  (pprint-style s list colon? :fill nil))

(defun PPRINT-TABULAR (s list &optional (colon? T) atsign? (tabsize nil))
  (declare (ignore atsign?))
  (pprint-style s list colon? :fill (or tabsize 16)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CASE-MODE STREAMS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Char-mode can be one of the following:
;;;   NIL no changes to characters output.
;;;   :UP CHAR-UPCASE used.
;;;   :DOWN CHAR-DOWNCASE used.
;;;   :CAP0 capitalize next alphanumeric letter then switch to :DOWN.
;;;   :CAP1 capitalize next alphanumeric letter then switch to :CAPW
;;;   :CAPW downcase letters.  When a word break letter found, switch
;;;      to :CAP1. 
;;; It is possible for ~(~) to be nested in a format string, but note
;;; that each mode specifies what should happen to every letter.
;;; Therefore, inner nested modes never have any effect.  You can just
;;; ignore them.

(defclass CASE-MODE-STREAM (fundamental-character-output-stream
			    encapsulating-stream)
  ((CHAR-MODE :initform nil
	      :initarg :char-mode
	      :accessor char-mode))) 

(defmethod STREAM-WRITE-CHAR ((xp CASE-MODE-STREAM)
			      (char character))
  (stream-write-char (encapsulating-stream-stream xp)
		     (handle-char-mode xp char)))

(defun handle-char-mode (xp char)
  (case (char-mode xp)
    (:CAP0 (cond ((not (alphanumericp char)) char)
		 (T (setf (char-mode xp) :DOWN)
		    (char-upcase char))))
    (:CAP1 (cond ((not (alphanumericp char)) char)
		 (T (setf (char-mode xp) :CAPW)
		    (char-upcase char))))
    (:CAPW (cond ((alphanumericp char) (char-downcase char))
		 (T (setf (char-mode xp) :CAP1)
		    char)))
    (:UP (char-upcase char))
    (:DOWN (char-downcase char))
    (t char)))

(defresource CASE-MODE-STREAM (base-stream mode)
  :matcher t
  :constructor (make-instance 'case-mode-stream)
  :initializer (setf (encapsulating-stream-stream case-mode-stream) base-stream
		     (char-mode case-mode-stream) mode))

(defmethod stream-case-print ((stream CASE-MODE-STREAM) mode printer)
  (declare (ignore mode))
  (funcall printer stream))

(defmethod stream-case-print ((stream STREAM) mode printer)
  (using-resource (new case-mode-stream stream mode)
    (stream-case-print new mode printer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; XP CODE
;;;
;;; This part based on Dick Waters' XP code.
;;;   (copyright at bottom of this file)
;;; 
;;; The spec is not clear on when global varibles like *print-pretty*,
;;; *print-right-margin*, *print-lines* and *print-miser-width* should
;;; be examined.  For all but the last of these, XP caches the values
;;; when the xp stream is being set up.  IWBNI they were examined
;;; while enqueuing.  XP references *print-miser-width* when going
;;; over the queue.
;;;
;;; The spec is also not clear about whether things like length, level
;;; and circularity labels must be on a per-stream basis.  For
;;; example, what happens when some print method opens up another
;;; stream? 
;;;
;;; + suffix means that the stream is known to be an XP stream, all
;;; inputs are mandatory, and no error checking has to be done.  Suffix
;;; ++ additionally means that the output is guaranteed not to contain a
;;; newline char.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Various read-time parameters are defined at the bottom of printer.lisp.

(defclass PRETTY-PRINTING-STREAM (case-mode-stream
				  buffered-character-output-stream)
  ((LINE-LIMIT :accessor line-limit) ;;If non-NIL the max number of lines to print.
   (LINE-NO :accessor line-no) ;;number of next line to be printed.
   (DEPTH-IN-BLOCKS :accessor depth-in-blocks)
   ;;Number of logical blocks at QRIGHT that are started but not ended.
   (BLOCK-STACK :initform (make-array #.block-stack-min-size)
		:accessor block-stack)
   (BLOCK-STACK-PTR :accessor block-stack-ptr)
   ;;This stack is pushed and popped in accordance with the way blocks are 
   ;;nested at the moment they are entered into the queue.  It contains the 
   ;;following block specific value.
   ;;SECTION-START total position where the section (see AIM-1102)
   ;;that is rightmost in the queue started.
   (OUTPUT-BUFFER :initform (make-array #.buffer-min-size
					:element-type 'character))
   (CHARPOS :accessor charpos)
   (BUFFER-PTR :accessor buffer-ptr)
   (BUFFER-OFFSET :accessor buffer-offset)
   ;;This is a vector of characters (eg a string) that builds up the
   ;;line images that will be printed out.  BUFFER-PTR is the
   ;;buffer position where the next character should be inserted in
   ;;the string.  CHARPOS is the output character position of the
   ;;first character in the buffer (non-zero only if a partial line
   ;;has been output).  BUFFER-OFFSET is used in computing total lengths.
   ;;It is changed to reflect all shifting and insertion of prefixes so that
   ;;total length computes things as they would be if they were 
   ;;all on one line.  Positions are kept three different ways
   ;; Buffer position (eg BUFFER-PTR)
   ;; Line position (eg (+ BUFFER-PTR CHARPOS)).  Indentations are stored in this form.
   ;; Total position if all on one line (eg (+ BUFFER-PTR BUFFER-OFFSET))
   ;;  Positions are stored in this form.
   (QUEUE :initform (make-array #.queue-min-size) :accessor queue)
   (QLEFT :accessor qleft) (QRIGHT :accessor qright)
   ;;This holds a queue of action descriptors.  QLEFT and QRIGHT
   ;;point to the next entry to dequeue and the last entry enqueued
   ;;respectively.  The queue is empty when
   ;;(> QLEFT QRIGHT).  The queue entries have several parts:
   ;;QTYPE one of :NEWLINE/:IND/:START-BLOCK/:END-BLOCK
   ;;QKIND :LINEAR/:MISER/:FILL/:MANDATORY or :UNCONDITIONAL/:FRESH
   ;; or :BLOCK/:CURRENT
   ;;QPOS total position corresponding to this entry
   ;;QDEPTH depth in blocks of this entry.
   ;;QEND offset to entry marking end of section this entry starts. (NIL until known.)
   ;; Only :start-block and non-literal :newline entries can start sections.
   ;;QOFFSET offset to :END-BLOCK for :START-BLOCK (NIL until known).
   ;;QARG for :IND indentation delta
   ;;     for :START-BLOCK suffix in the block if any.
   ;;                      or if per-line-prefix then cons of suffix and
   ;;                      per-line-prefix.
   ;;     for :END-BLOCK suffix for the block if any.
   (PREFIX :initform (make-array #.buffer-min-size :element-type 'character)
	   :accessor prefix)
   ;;this stores the prefix that should be used at the start of the line
   (PREFIX-STACK :initform (make-array #.prefix-stack-min-size)
		 :accessor prefix-stack)
   (PREFIX-STACK-PTR :accessor prefix-stack-ptr)
   ;;This stack is pushed and popped in accordance with the way blocks 
   ;;are nested at the moment things are taken off the queue and printed.
   ;;It contains the following block specific values.
   ;;PREFIX-PTR current length of PREFIX.
   ;;SUFFIX-PTR current length of pending suffix
   ;;NON-BLANK-PREFIX-PTR current length of non-blank prefix.
   ;;INITIAL-PREFIX-PTR prefix-ptr at the start of this block.
   ;;SECTION-START-LINE line-no value at last non-literal break at this level.
   (SUFFIX :initform (make-array #.buffer-min-size :element-type 'character)
	   :accessor suffix)))
    
(defmethod stream-pprint ((stream PRETTY-PRINTING-STREAM) object printer
			  &key prefix suffix per-line-p)
  (unless (print-object-aborted-p object stream)
    (let ((*current-level* (1+ *current-level*))
	  (*current-length* 0))
      (start-block stream prefix per-line-p suffix)
      (unwind-protect (funcall printer object stream)
	(end-block stream suffix)))))

;;; See eclipse-compile2.lisp for macro definitions, including:
;;; lp<-bp and friends
;;; check-size
;;; section-start xxx-ptr, section-start-line
;;; Qxxx
;;; maybe-too-large
;;; misering?


;;; This assumes all all pending output on one line, which isn't
;;; always right.  Perhaps we should force-output first?
(defmethod STREAM-LINE-COLUMN ((stream PRETTY-PRINTING-STREAM))
  (TP<-BP stream))

(defmethod STREAM-WRITE-CHAR ((xp PRETTY-PRINTING-STREAM)
			      (char character))
  (if (eql char #\newline)
      (stream-terpri xp)
      (write-char++ char xp))
  char)

(defmethod STREAM-TERPRI ((xp PRETTY-PRINTING-STREAM))
  (stream-newline xp :unconditional)
  nil)

;;; Because force-output forces newlines, prin1, princ, etc call this,
;;; which only forces streams which are safe.
(defmethod maybe-force-output ((stream PRETTY-PRINTING-STREAM))
  nil)

;;; Causes the stream to be pessimistic and insert newlines wherever
;;; it might have to, when forcing the partial output out.  This is so
;;; that things will be in a consistent state if output continues to
;;; the stream later.
(defmethod STREAM-FORCE-OUTPUT :BEFORE ((stream PRETTY-PRINTING-STREAM))
  (attempt-to-output stream T T))

(defmethod STREAM-CLEAR-OUTPUT :BEFORE ((stream PRETTY-PRINTING-STREAM))
  (let ((*current-circle* 0))		;prevent visible output
    (attempt-to-output stream T T)))

;;; This arguably ought to be defined instead on BUFFERED-OUTPUT-STREAM.
(defmethod STREAM-FINISH-OUTPUT :BEFORE ((stream PRETTY-PRINTING-STREAM))
  (stream-force-output stream))


(defmethod STREAM-FRESH-LINE ((stream PRETTY-PRINTING-STREAM))
  (attempt-to-output stream T T) ;ok because we want newline
  (when (not (zerop (LP<-BP stream)))
    (stream-newline stream :fresh)
    T))

(defmethod STREAM-WRITE-SEQUENCE ((stream PRETTY-PRINTING-STREAM)
				  (sequence STRING) &key
				  (start 0) end)
  (stream-write-string stream sequence start end))

(defmethod STREAM-WRITE-STRING ((xp PRETTY-PRINTING-STREAM)
				(string STRING) &optional
				(start 0) end)
  (let ((sub-end nil)
	(end (or end (length string)))
	next-newline)
    (loop (setq next-newline
		(position #\newline string :test #'char= :start start :end end))
	  (setq sub-end (if next-newline next-newline end))
	  (write-string++ string xp start sub-end)
	  (when (null next-newline) (return nil))
	  (stream-newline xp :unconditional)
	  (setq start (1+ sub-end)))))



(defun push-block-stack (xp)
  (incf (block-stack-ptr xp) #.block-stack-entry-size)
  (check-size xp block-stack (block-stack-ptr xp)))

(defun pop-block-stack (xp)
  (decf (block-stack-ptr xp) #.block-stack-entry-size))


(defun push-prefix-stack (xp)
  (let ((old-prefix 0) (old-suffix 0) (old-non-blank 0))
    (when (not (minusp (prefix-stack-ptr xp)))
      (setq old-prefix (prefix-ptr xp)
	    old-suffix (suffix-ptr xp)
	    old-non-blank (non-blank-prefix-ptr xp)))
    (incf (prefix-stack-ptr xp) #.prefix-stack-entry-size)
    (check-size xp prefix-stack (prefix-stack-ptr xp))
    (setf (prefix-ptr xp) old-prefix)
    (setf (suffix-ptr xp) old-suffix)
    (setf (non-blank-prefix-ptr xp) old-non-blank)))

(defun pop-prefix-stack (xp)
  (decf (prefix-stack-ptr xp) #.prefix-stack-entry-size))


;we shift the queue over rather than using a circular queue because
;that works out to be a lot faster in practice.  Note, short printout
;does not ever cause a shift, and even in long printout, the queue is
;shifted left for free every time it happens to empty out.

(defun enqueue (xp type kind &optional arg)
  (incf (Qright xp) #.queue-entry-size)
  (when (> (Qright xp) #.(- queue-min-size queue-entry-size))
    (replace (queue xp) (queue xp) :start2 (Qleft xp) :end2 (Qright xp))
    (setf (Qright xp) (- (Qright xp) (Qleft xp)))
    (setf (Qleft xp) 0))
  (check-size xp queue (Qright xp))
  (setf (Qtype xp (Qright xp)) type)
  (setf (Qkind xp (Qright xp)) kind)
  (setf (Qpos xp (Qright xp)) (TP<-BP xp))
  (setf (Qdepth xp (Qright xp)) (depth-in-blocks xp))
  (setf (Qend xp (Qright xp)) nil)
  (setf (Qoffset xp (Qright xp)) nil)
  (setf (Qarg xp (Qright xp)) arg))


(defun INITIALIZE-PP (xp stream)
  (setf (slot-value xp 'line-length)
	(max 0 (or *print-right-margin*
		   (stream-line-length stream)
		   *default-right-margin*)))
  (setf (slot-value xp 'stream) stream)
  (setf (line-limit xp) *print-lines*)
  (setf (line-no xp) 1)
  (setf (depth-in-blocks xp) 0)
  (setf (block-stack-ptr xp) 0)
  (setf (charpos xp) (or (stream-line-column stream) 0))
  (setf (section-start xp) 0)
  (setf (buffer-ptr xp) 0)
  (setf (buffer-offset xp) (charpos xp))
  (setf (Qleft xp) 0)
  (setf (Qright xp) #.(- queue-entry-size))
  (setf (prefix-stack-ptr xp) #.(- prefix-stack-entry-size))
  xp)


(defmethod STREAM-CASE-PRINT ((stream PRETTY-PRINTING-STREAM) mode printer)
  (declare (ignore printer))
  (let ((new-mode-p (not (char-mode stream))))
    (when new-mode-p (setf (char-mode stream) mode))
    (unwind-protect (call-next-method)      
      (when new-mode-p (setf (char-mode stream) nil)))))

;;; All characters output in char-mode are passed through
;;; handle-char-mode.  However, it must be noted that on-each-line
;;; prefixes are only processed in the context of the first place they
;;; appear.  They stay the same later no matter what.  Also
;;; non-literal newlines do not count as word breaks.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;note this checks (> BUFFER-PTR LINE-LENGTH) instead of (> (LP<-BP) LINE-LENGTH)
;this is important so that when things are longer than a line they
;end up getting printed in chunks of size LINE-LENGTH.

(defun write-char++ (char xp)
  (when (> (buffer-ptr xp) (stream-line-length xp))
    (force-some-output xp))
  (let ((new-buffer-end (1+ (buffer-ptr xp))))
    (check-size xp buffer new-buffer-end output-buffer)
    (setf (char (output-buffer xp) (buffer-ptr xp))
	  (handle-char-mode xp char))    
    (setf (buffer-ptr xp) new-buffer-end)))

(defun force-some-output (xp)
  (attempt-to-output xp nil nil)
  (when (> (buffer-ptr xp) (stream-line-length xp)) ;only if printing off end of line
    (attempt-to-output xp T T)))

(defun write-string++ (string xp start end)
  (when (> (buffer-ptr xp) (stream-line-length xp))
    (force-some-output xp))
  (write-string+++ string xp start end))

;never forces output; therefore safe to call from within output-line.

(defun write-string+++ (string xp start end) 
  (let ((new-buffer-end (+ (buffer-ptr xp) (- end start))))
    (check-size xp buffer new-buffer-end output-buffer)
    (do ((buffer (output-buffer xp))
	 (i (buffer-ptr xp) (1+ i))
	 (j start (1+ j)))
	((= j end))
      (setf (char buffer i)
	    (handle-char-mode xp (char string j))))
    (setf (buffer-ptr xp) new-buffer-end)))

(defmethod stream-tab ((xp pretty-printing-stream) kind colnum colinc)
  (when *print-pretty*
    (let ((indented? nil) (relative? nil))
      (case kind
	(:section (setq indented? T))
	(:line-relative (setq relative? T))
	(:section-relative (setq indented? T relative? T)))
      (let* ((current
	      (if (not indented?) (LP<-BP xp)
		  (- (TP<-BP xp) (section-start xp))))
	     (new (tab-position current relative? colnum colinc))
	     (length (- new current)))
	(when (plusp length)
	  (when (char-mode xp) (handle-char-mode xp #\space))
	  (let ((end (+ (buffer-ptr xp) length)))
	    (check-size xp buffer end output-buffer)
	    (fill (output-buffer xp) #\space :start (buffer-ptr xp) :end end)
	    (setf (buffer-ptr xp) end)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod stream-newline ((xp pretty-printing-stream) kind)
  (when *print-pretty*
    (enqueue xp :newline kind)
    (do ((ptr (Qleft xp) (Qnext ptr)))	;find sections we are ending
	((not (< ptr (Qright xp))))	;all but last
      (when (and (null (Qend xp ptr))
		 (not (> (depth-in-blocks xp) (Qdepth xp ptr)))
		 (member (Qtype xp ptr) '(:newline :start-block)))
	(setf (Qend xp ptr) (- (Qright xp) ptr))))
    (setf (section-start xp) (TP<-BP xp))
    (when (and (member kind '(:fresh :unconditional)) (char-mode xp))
      (handle-char-mode xp #\newline))
    (when (member kind '(:fresh :unconditional :mandatory))
      (attempt-to-output xp T nil))))

(defun start-block (xp prefix-string on-each-line? suffix-string)
  (when prefix-string (write-string++ prefix-string xp 0 (length prefix-string)))
  (if (and (char-mode xp) on-each-line?)
      (setq prefix-string
	    (subseq (output-buffer xp) (- (buffer-ptr xp) (length prefix-string))
		    (buffer-ptr xp))))
  (push-block-stack xp)
  (enqueue xp :start-block nil
	   (if on-each-line? (cons suffix-string prefix-string) suffix-string))
  (incf (depth-in-blocks xp))	      ;must be after enqueue
  (setf (section-start xp) (TP<-BP xp)))

(defun end-block (xp suffix)
  (unless (eq *abbreviation-happened* '*print-lines*)
    (when suffix (stream-write-string xp suffix 0 (length suffix)))
    (decf (depth-in-blocks xp))
    (enqueue xp :end-block nil suffix)
    (do ((ptr (Qleft xp) (Qnext ptr))) ;looking for start of block we are ending
	((not (< ptr (Qright xp))))    ;all but last
      (when (and (= (depth-in-blocks xp) (Qdepth xp ptr))
		 (eq (Qtype xp ptr) :start-block)
		 (null (Qoffset xp ptr)))
	(setf (Qoffset xp ptr) (- (Qright xp) ptr))
	(return nil)))	;can only be 1
    (pop-block-stack xp)))

(defmethod stream-indent ((xp pretty-printing-stream) kind n)
  (when *print-pretty*
    (enqueue xp :ind kind n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;If flush-out? is T and force-newlines? is NIL then the buffer,
;prefix-stack, and queue will be in an inconsistent state after the call.
;You better not call it this way except as the last act of outputting.

(defun attempt-to-output (xp force-newlines? flush-out?)
  (do () ((> (Qleft xp) (Qright xp))
	  (setf (Qleft xp) 0)
	  (setf (Qright xp) #.(- queue-entry-size))) ;saves shifting
    (case (Qtype xp (Qleft xp))
      (:ind
       (unless (misering? xp)
	 (set-indentation-prefix xp
	   (case (Qkind xp (Qleft xp))
	     (:block (+ (initial-prefix-ptr xp) (Qarg xp (Qleft xp))))
	     (T ; :current
	       (+ (LP<-TP xp (Qpos xp (Qleft xp)))
		  (Qarg xp (Qleft xp)))))))
       (setf (Qleft xp) (Qnext (Qleft xp))))
      (:start-block
       (cond ((maybe-too-large xp (Qleft xp))
	      (push-prefix-stack xp)
	      (setf (initial-prefix-ptr xp) (prefix-ptr xp))
	      (set-indentation-prefix xp (LP<-TP xp (Qpos xp (Qleft xp))))
	      (let ((arg (Qarg xp (Qleft xp))))
		(when (consp arg) (set-prefix xp (cdr arg)))
		(setf (initial-prefix-ptr xp) (prefix-ptr xp))
		(cond ((not (listp arg)) (set-suffix xp arg))
		      ((car arg) (set-suffix xp (car arg)))))
	      (setf (section-start-line xp) (line-no xp)))
	     (T (incf (Qleft xp) (Qoffset xp (Qleft xp)))))
       (setf (Qleft xp) (Qnext (Qleft xp))))
      (:end-block (pop-prefix-stack xp) (setf (Qleft xp) (Qnext (Qleft xp))))
      (T ; :newline
       (when (case (Qkind xp (Qleft xp))
	       (:fresh (not (zerop (LP<-BP xp))))
	       (:miser (misering? xp))
	       (:fill (or (misering? xp)
			  (> (line-no xp) (section-start-line xp))
			  (maybe-too-large xp (Qleft xp))))
	       (T T)) ;(:linear :unconditional :mandatory) 
	 (output-line xp (Qleft xp))
	 (setup-for-next-line xp (Qleft xp)))
       (setf (Qleft xp) (Qnext (Qleft xp))))))
  (when flush-out? (flush xp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;this can only be called last!

(defun flush (xp)
  (unless *current-circle*
    (write-string (output-buffer xp) (encapsulating-stream-stream xp)
		  :end (buffer-ptr xp)))
  (incf (buffer-offset xp) (buffer-ptr xp))
  (incf (charpos xp) (buffer-ptr xp))
  (setf (buffer-ptr xp) 0))

;This prints out a line of stuff.

(defun output-line (xp Qentry)
  (let* ((out-point (BP<-TP xp (Qpos xp Qentry)))
	 (last-non-blank (position #\space (output-buffer xp) :test-not #'char=
				   :from-end T :end out-point))
	 (end (cond ((member (Qkind xp Qentry) '(:fresh :unconditional)) out-point)
		    (last-non-blank (1+ last-non-blank))
		    (T 0)))
	 (line-limit-exit (and (line-limit xp) (not (> (line-limit xp) (line-no xp))))))
    (when line-limit-exit
      (setf (buffer-ptr xp) end)          ;truncate pending output.
      (write-string+++ " .." xp 0 3)
      (reverse-string-in-place (suffix xp) 0 (suffix-ptr xp))
      (write-string+++ (suffix xp) xp 0 (suffix-ptr xp))
      (setf (Qleft xp) (Qnext (Qright xp)))
      (setq *abbreviation-happened* '*print-lines*)
      (throw 'line-limit-abbreviation-exit T))
    (incf (line-no xp))
    (unless *current-circle*
      (write-line (output-buffer xp) (encapsulating-stream-stream xp)
		  :end end))))

(defun setup-for-next-line (xp Qentry)
  (let* ((out-point (BP<-TP xp (Qpos xp Qentry)))
	 (prefix-end
	   (cond ((member (Qkind xp Qentry) '(:unconditional :fresh))
		  (non-blank-prefix-ptr xp))
		 (T (prefix-ptr xp))))
	 (change (- prefix-end out-point)))
    (setf (charpos xp) 0)
    (when (plusp change)                  ;almost never happens
      (check-size xp buffer (+ (buffer-ptr xp) change) output-buffer))
    (replace (output-buffer xp) (output-buffer xp) :start1 prefix-end
	     :start2 out-point :end2 (buffer-ptr xp))
    (replace (output-buffer xp) (prefix xp) :end2 prefix-end)
    (incf (buffer-ptr xp) change)
    (decf (buffer-offset xp) change)
    (when (not (member (Qkind xp Qentry) '(:unconditional :fresh)))
      (setf (section-start-line xp) (line-no xp)))))

(defun set-indentation-prefix (xp new-position)
  (let ((new-ind (max (non-blank-prefix-ptr xp) new-position)))
    (setf (prefix-ptr xp) (initial-prefix-ptr xp))
    (check-size xp prefix new-ind)
    (when (> new-ind (prefix-ptr xp))
      (fill (prefix xp) #\space :start (prefix-ptr xp) :end new-ind))
    (setf (prefix-ptr xp) new-ind)))

(defun set-prefix (xp prefix-string)
  (replace (prefix xp) prefix-string
	   :start1 (- (prefix-ptr xp) (length prefix-string)))
  (setf (non-blank-prefix-ptr xp) (prefix-ptr xp)))

(defun set-suffix (xp suffix-string)
  (let* ((end (length suffix-string))
	 (new-end (+ (suffix-ptr xp) end)))
    (check-size xp suffix new-end)
    (do ((i (1- new-end) (1- i)) (j 0 (1+ j))) ((= j end))
      (setf (char (suffix xp) i) (char suffix-string j)))
    (setf (suffix-ptr xp) new-end)))

(defun reverse-string-in-place (string start end)
  (do ((i start (1+ i)) (j (1- end) (1- j))) ((not (< i j)) string)
    (let ((c (char string i)))
      (setf (char string i) (char string j))
      (setf (char string j) c))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
