;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;22.3 OUTPUT FUNCTIONS                                         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 22.3.1 OUTPUT TO CHARACTER STREAMS

(defparameter *os* (make-string-output-stream))
(defparameter *ot* (make-two-way-stream
		    (make-string-input-stream "")
		    (make-string-output-stream)))
(defparameter *oe* (make-echo-stream
		    (make-string-input-stream "")
		    (make-string-output-stream)))
(defparameter *osyn* (make-synonym-stream '*os*))
(defparameter *ob* (make-broadcast-stream
		    *os* *ot* *oe* *osyn*))
(defun get-broadcasts ()
  (list (get-output-stream-string *os*)
	(get-output-stream-string
	 (two-way-stream-output-stream *ot*))
	(get-output-stream-string
	 (echo-stream-output-stream *oe*))
	(get-output-stream-string
	 (symbol-value (synonym-stream-symbol *osyn*)))))

;;; WRITE-CHAR
(deftest write-char-string
    (values (write-char #\a *os*)
	    (write-char #\space *os*)
	    (write-char #\b *os*)
	    (get-output-stream-string *os*))
  #\a #\space #\b "a b")
(deftest write-char-broadcast
    (values (write-char #\a *ob*)
	    (write-char #\space *ob*)
	    (write-char #\b *ob*)
	    (get-broadcasts))
  #\a #\space #\b ("aa  bb" "a b" "a b" ""))

;;; TERPRI
(deftest terpri-string
    (values (terpri *os*)
	    (get-output-stream-string *os*))
  nil "
")
(deftest terpri-broadcast
    (values (terpri *ob*)
	    (get-broadcasts))
  nil ("

"
	"
"
	"
"
	""))

;;; FRESH-LINE
(deftest freshline-string
    (values (write-string "some text" *os*)
	    (fresh-line *os*)
	    (fresh-line *os*)
	    (write-char #\m *os*)
	    (fresh-line *os*)
	    (write-char #\n *os*)
	    (terpri *os*)
	    (fresh-line *os*)
	    (write-char #\o *os*)
	    (write-char #\newline *os*)
	    (fresh-line *os*)
	    (write-string "more text" *os*)
	    (get-output-stream-string *os*))
  "some text" t nil #\m t #\n nil nil #\o #\newline nil "more text" "some text
m
n
o
more text")
(deftest freshline-broadcast
    (values (write-string "some text" *ob*)
	    (fresh-line *ob*)
	    (fresh-line *ob*)
	    (write-char #\m *ob*)
	    (fresh-line *ob*)
	    (write-char #\n *ob*)
	    (terpri *ob*)
	    (fresh-line *ob*)
	    (write-char #\o *ob*)
	    (write-char #\newline *ob*)
	    (fresh-line *ob*)
	    (write-string "more text" *ob*)
	    (get-broadcasts))
  "some text" nil nil #\m nil #\n nil nil #\o #\newline nil "more text" ("some textsome text
mm
nn

oo

more textmore text"
									 "some text
m
n
o
more text"
									 "some text
m
n
o
more text"
									 ""))


;;; WRITE-STRING
(deftest write-string-string
    (values (write-string "books" *os* :end 4)
	    (write-string "worms" *os*)
	    (get-output-stream-string *os*))
  "books" "worms" "bookworms")
(deftest write-string-broadcast
    (values (write-string "books" *ob* :end 4)
	    (write-string "worms" *ob*)
	    (get-broadcasts))
  "books" "worms"
  ("bookbookwormsworms" "bookworms" "bookworms" ""))


;;; WRITE-LINE
(deftest write-line-string
    (values (write-char #\* *os*)
	    (write-line "test12" *os* :end 5)
	    (write-line "**test2" *os* :start 1)
	    (write-char #\* *os*)
	    (get-output-stream-string *os*))
  #\* "test12" "**test2" #\*
  "*test1
*test2
*")
;; The behavior of write-line on broacast streams which, at some point
;; share streams, is debatable.  A strict reading of the spec says
;; that the string is output on stream, and then a newline is output.
;; We interpret this to mean that for a broadcast stream, the string
;; is output on all streams, and then the newline is output on all streams.
(deftest write-line-broadcast
    (values (write-char #\* *ob*)
	    (write-line "test12" *ob* :end 5)
	    (write-line "**test2" *ob* :start 1)
	    (write-char #\* *ob*)
	    (get-broadcasts))
  #\* "test12" "**test2" #\*
  ("**test1test1

*test2*test2

**"
   "*test1
*test2
*"
   "*test1
*test2
*"
   ""))

;;; FORCE-OUTPUT, FINISH-OUTPUT, CLEAR-OUTPUT
(deftest control-output-string
    (values (clear-output *os*)
	    (write-string "ab" *os*)
	    (force-output *os*)
	    (write-string "cd" *os*)
	    (finish-output *os*)
	    (write-string "ef" *os*)
	    (get-output-stream-string *os*))
    nil "ab" nil "cd" nil "ef" "abcdef")
(deftest control-output-broadcast
    (values (clear-output *ob*)
	    (write-string "ab" *ob*)
	    (force-output *ob*)
	    (write-string "cd" *ob*)
	    (finish-output *ob*)
	    (write-string "ef" *ob*)
	    (get-broadcasts))
    nil "ab" nil "cd" nil "ef" 
    ("ababcdcdefef" "abcdef" "abcdef" ""))

(deftest with-output-to-string-resize
  (let ((s (make-array 5 :element-type 'base-char
					  :initial-contents "abcXX"
					  :fill-pointer 3
					  :adjustable t)))
      (with-output-to-string (s s)
	(write-string "defg" s))
      s)
  "abcdefg")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 22.2 INPUT FUNCTIONS                                         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 22.2.1 INPUT FROM CHARACTER STREAMS

(defparameter null-output-stream (make-broadcast-stream))

;;; READ-CHAR
(deftest read-char-string
    (let ((is (make-string-input-stream "0123")))
      (values (read-char is)
	      (read-char is)
	      (read-char is)
	      (read-char is)
	      (read-char is nil 'the-end)))
  #\0 #\1 #\2 #\3 the-end)
(defvar *iss*)
(deftest read-char-concat
    (let* ((*iss* (make-string-input-stream "3"))
	   (is (make-concatenated-stream
		(make-string-input-stream "0")
		(make-two-way-stream
		 (make-string-input-stream "1")
		 null-output-stream)
		(make-echo-stream
		 (make-string-input-stream "2")
		 *os*)
		(make-synonym-stream '*iss*))))
      (values (read-char is)
	      (read-char is)
	      (read-char is)
	      (read-char is)
	      (read-char is nil 'the-end)
	      (get-output-stream-string *os*)))
  #\0 #\1 #\2 #\3 the-end "2")

;;; READ-CHAR
(deftest unread-char-string
    (let ((is (make-string-input-stream "0123")))
      (values (unread-char (read-char is) is) (read-char is)
	      (unread-char (read-char is) is)(read-char is)
	      (unread-char (read-char is) is) (read-char is)
	      (unread-char (read-char is) is) (read-char is)
	      (read-char is nil 'the-end)))
  nil #\0 nil #\1 nil #\2 nil #\3 the-end)
(deftest unread-char-concatenated
    (let* ((*iss* (make-string-input-stream "3"))
	   (is (make-concatenated-stream
		(make-string-input-stream "0")
		(make-two-way-stream
		 (make-string-input-stream "1")
		 null-output-stream)
		(make-echo-stream
		 (make-string-input-stream "2")
		 *os*)
		(make-synonym-stream '*iss*))))
      (values (unread-char (read-char is) is) (read-char is)
	      (unread-char (read-char is) is) (read-char is)
	      (unread-char (read-char is) is) (read-char is)
	      (unread-char (read-char is) is) (read-char is)
	      (read-char is nil 'the-end)
	      (get-output-stream-string *os*)))
  nil #\0 nil #\1 nil #\2 nil #\3 the-end "2")
(deftest unread-char-echo
    (let ((is (make-echo-stream
	       (make-string-input-stream "abc

def")
	       *os*)))
      (values (unread-char (read-char is) is) (read-char is)
	      (unread-char (read-char is) is) (read-line is)
	      (unread-char (read-char is) is) (read-line is)
	      (unread-char #\newline is) (read-line is)
	      (unread-char (read-char is) is) (listen is)
	      (clear-input is) (listen is) (read-char is nil 'eof)
	      (get-output-stream-string *os*)))
  nil #\a nil "bc" nil "" nil "" nil t nil nil eof  "abc

d")

;;; READ-CHAR-NO-HANG
(deftest read-char-no-hang-string
    (let ((is (make-string-input-stream "0")))
      (values (read-char-no-hang is)
	      (read-char-no-hang is nil 'the-end)))
  #\0 the-end)
(deftest read-char-no-hang-concat
    (let* ((*iss* (make-string-input-stream "3"))
	   (is (make-concatenated-stream
	       (make-string-input-stream "0")
	       (make-two-way-stream
		(make-string-input-stream "1")
		null-output-stream)
	       (make-echo-stream
		(make-string-input-stream "2")
		*os*)
	       (make-synonym-stream '*iss*))))
      (values (read-char-no-hang is)
	      (read-char-no-hang is)
	      (read-char-no-hang is)
	      (read-char-no-hang is)
	      (read-char-no-hang is nil 'the-end)
	      (get-output-stream-string *os*)))
  #\0 #\1 #\2 #\3 the-end "2")

;;; READ-LINE
(deftest read-line-string
    (let* ((a "line 1
 line 2")
	   (input-stream (make-string-input-stream a)))
      (multiple-value-call #'values
	(read-line input-stream)
	(read-line input-stream)
	(read-line input-stream nil nil)))
  "line 1" nil " line 2" t NIL t)

(deftest read-line-concatenated
    (let* ((*iss* (make-string-input-stream "2
line3"))
	   (is (make-concatenated-stream
	       (make-string-input-stream "line")
	       (make-two-way-stream
		(make-string-input-stream "1
")
		null-output-stream)
	       (make-echo-stream
		(make-string-input-stream " line")
		*os*)
	       (make-synonym-stream '*iss*))))
      (multiple-value-call #'values
	(read-line is)
	(read-line is)
	(read-line is)
	(read-line is nil nil)
	(get-output-stream-string *os*)))
  "line1" nil " line2" nil "line3" t NIL t " line")

;;; PEEK-CHAR
(deftest peek-char-string
    (let ((is (make-string-input-stream "   
  	1 2 3 4 5")))
      (values (peek-char t is)
	      (peek-char #\4 is)
	      (peek-char nil is)))
  #\1 #\4 #\4)
(deftest peek-char-concatenated
    (let* ((*iss* (make-string-input-stream " 4 5"))
	   (is (make-concatenated-stream
	       (make-string-input-stream "    
 ")
	       (make-two-way-stream
		(make-string-input-stream "	1")
		null-output-stream)
	       (make-echo-stream
		(make-string-input-stream "2 3")
		*os*)
	       (make-synonym-stream '*iss*))))
      (values (peek-char t is)
	      (peek-char #\4 is)
	      (peek-char nil is)
	      (get-output-stream-string *os*)))
  #\1 #\4 #\4 "2 3")

;;; CLEAR-INPUT
;;; We define clear-input on non-interactive streams to position the
;;; stream at end of file.  
(deftest clear-input-string
    (let ((stream (make-string-input-stream "abc")))
      (values (clear-input stream)
	      (read-char stream nil 'eof)))
  nil eof)

(deftest clear-input-concatenated
    (let* ((*iss* (make-string-input-stream "4"))
	   (stream (make-concatenated-stream
	       (make-string-input-stream "1")
	       (make-two-way-stream
		(make-string-input-stream "2")
		null-output-stream)
	       (make-echo-stream
		(make-string-input-stream "3")
		*os*)
	       (make-synonym-stream '*iss*))))
      (values (clear-input stream)
	      (read-char stream nil 'eof)))
  nil eof)

;;; LISTEN
(deftest listen-string
    (let ((stream (make-string-input-stream "abc")))
      (values (listen stream)
	      (clear-input stream)
	      (listen stream)))
  t nil nil)
(deftest listen-concatenated
    (let* ((*iss* (make-string-input-stream "4"))
	   (is (make-concatenated-stream
	       (make-string-input-stream "1")
	       (make-two-way-stream
		(make-string-input-stream "2")
		null-output-stream)
	       (make-echo-stream
		(make-string-input-stream "3")
		*os*)
	       (make-synonym-stream '*iss*))))
      (values (listen is) (read-char is)
	      (listen is) (read-char is)
	      (listen is) (read-char is)
	      (listen is) (read-char is)
	      (listen is) (read-char is nil)
	      (get-output-stream-string *os*)))
  t #\1 t #\2 t #\3 t #\4 nil nil "3")


;;; WITH-OPEN-STREAM !!!
;;; MAKE-STRING-OUTPUT-STREAM !!!
;;; GET-OUPTUT-STREAM-STRING !!!
;;; WITH-OUTPUT-TO-STRING !!!
;;; WITH-INPUT-FROM-STRING !!!
;;; CLOSE !!!


#+example
(let ((s (make-file-output-stream *standard-output*
				  :interactive-p t)))
  (write-sequence "abcd" s)
  (write-sequence "xxefghyy" s :start 2 :end 6)
  (write-char #\i s)
  (write-string "xxjklyy" s :start 2 :end 5)
  (write-line "mno" s)
  (finish-output s))

#+example
(setf is (make-file-input-stream *standard-input*
				 :interactive-p t)
      seq (make-array 64 :element-type 'base-char))
#+example
(read-sequence seq is)