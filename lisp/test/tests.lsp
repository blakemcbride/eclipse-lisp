;; Test-Suiten ablaufen lassen:

#+CLISP
(defmacro with-ignored-errors (&rest forms)
  (let ((b (gensym)))
    `(BLOCK ,b
       (LET ((*ERROR-HANDLER*
               #'(LAMBDA (&REST ARGS) (RETURN-FROM ,b 'ERROR))
            ))
         ,@forms
     ) )
) )

#+AKCL
(defmacro with-ignored-errors (&rest forms)
  (let ((b (gensym))
        (h (gensym)))
    `(BLOCK ,b
       (LET ((,h (SYMBOL-FUNCTION 'SYSTEM:UNIVERSAL-ERROR-HANDLER)))
         (UNWIND-PROTECT
           (PROGN (SETF (SYMBOL-FUNCTION 'SYSTEM:UNIVERSAL-ERROR-HANDLER)
                        #'(LAMBDA (&REST ARGS) (RETURN-FROM ,b 'ERROR))
                  )
                  ,@forms
           )
           (SETF (SYMBOL-FUNCTION 'SYSTEM:UNIVERSAL-ERROR-HANDLER) ,h)
     ) ) )
) )

#+ALLEGRO
(defmacro with-ignored-errors (&rest forms)
  (let ((r (gensym)))
    `(LET ((,r (MULTIPLE-VALUE-LIST (EXCL:ERRORSET (PROGN ,@forms)))))
       (IF (CAR ,r) (VALUES-LIST (CDR ,r)) 'ERROR)
     )
) )

#-(or CLISP AKCL ALLEGRO)
(defmacro with-ignored-errors (&rest forms)
  (let ((b (gensym)))
    `(BLOCK ,b
       (HANDLER-BIND
         ((ERROR #'(LAMBDA (CONDITION) (RETURN-FROM ,b 'ERROR))))
         ,@forms
     ) )
) )

#-(or eclipse ALLEGRO)
(defun merge-extension (type filename)
  (merge-pathnames type filename)
)
#+(or eclipse ALLEGRO)
(defun merge-extension (type filename)
  (merge-pathnames (make-pathname :type (subseq (string type) 1)) filename)
)

(defun do-test (stream log &optional (ignore-errors t))
  (let ((eof "EOF"))
    (loop
      (let ((form (read stream nil eof))
            (result (read stream nil eof)))
        (when (or (eq form eof) (eq result eof)) (return))
        (print form)
        (let ((my-result
                (if ignore-errors
                  (with-ignored-errors (eval form)) ; return ERROR on errors
                  (eval form) ; don't disturb the condition system when testing it!
             )) )
          (cond ((eql result my-result)
                 (format t "~%EQL-OK: ~S" result)
                )
                ((equal result my-result)
                 (format t "~%EQUAL-OK: ~S" result)
                )
                ((equalp result my-result)
                 (format t "~%EQUALP-OK: ~S" result)
                )
                (t
                 (format t "~%FEHLER!! ~S sollte ~S sein!" my-result result)
                 (format log "~%Form: ~S~%SOLL: ~S~%~A: ~S~%"
                             form result
                             #+CLISP "CLISP" #+AKCL "AKCL" #+ALLEGRO "ALLEGRO"
                             my-result
                ))
) ) ) ) ) )

(defun do-errcheck (stream log)
  (let ((eof "EOF"))
    (loop
      (let ((form (read stream nil eof))
            (errtype (read stream nil eof)))
        (when (or (eq form eof) (eq errtype eof)) (return))
        (print form)
        (let ((my-result (nth-value 1 (ignore-errors (eval form)))))
          (multiple-value-bind (typep-result typep-error)
              (ignore-errors (typep my-result errtype))
            (cond ((and (not typep-error) typep-result)
                   (format t "~%OK: ~S" errtype)
                  )
                  (t
                   (format t "~%FEHLER!! ~S statt ~S !" my-result errtype)
                   (format log "~%Form: ~S~%SOLL: ~S~%~A: ~S~%"
                               form errtype
                               #+CLISP "CLISP" #+AKCL "AKCL" #+ALLEGRO "ALLEGRO"
                               my-result
                  ))
) ) ) ) ) ) )

(defun run-test (testname
                 &optional (tester #'do-test)
                 &aux (logname (merge-extension ".erg" testname))
                      log-empty-p)
  (with-open-file (s (merge-extension ".tst" testname) :direction :input)
    (with-open-file (log logname :direction :output
                                 #+ANSI-CL :if-exists #+ANSI-CL :new-version)
      (let ((*package* *package*)
            (*print-pretty* nil))
        (funcall tester s log)
      )
      (setq log-empty-p (zerop (file-length log)))
  ) )
  (when log-empty-p (delete-file logname))
  (values)
)

(defun run-all-tests ()
  (mapc #'run-test
        '( #-AKCL               "alltest"
                                "array"
                                "backquot"
           #-AKCL               "characters"
           #+(or CLISP ALLEGRO) "clos"
                                "eval20"
                                "format"
           #+CLISP              "genstream"
           #+XCL                "hash"
                                "hashlong"
                                "iofkts"
                                "lambda"
                                "lists151"
                                "lists152"
                                "lists153"
                                "lists154"
                                "lists155"
                                "lists156"
           #+(or CLISP ALLEGRO) "loop"
                                "macro8"
                                "map"
           #+(or CLISP ALLEGRO) "mop"
                                "number"
           #+CLISP              "number2"
           #-(or AKCL ALLEGRO)  "pack11"
           #+(or XCL CLISP)     "path"
           #+XCL                "readtable"
                                "setf"
                                "steele7"
           #-ALLEGRO            "streams"
                                "streamslong"
                                "strings"
           #-AKCL               "symbol10"
                                "symbols"
           #+XCL                "tprint"
           #+XCL                "tread"
                                "type"
  )      )
  #+(or CLISP ALLEGRO)
  (run-test "conditions" #'(lambda (stream log) (do-test stream log nil)))
  (run-test "excepsit" #'do-errcheck)
  t
)

