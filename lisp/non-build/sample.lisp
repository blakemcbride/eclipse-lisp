(in-package :user)			;The initializer for the
					;generated code BINDS *package*

;;; Here's some top-level code that gets executed at load time in
;;; Lisp, or at initialization time in the generated C code.
(print 'hello)				;In the generated code,
(write-line "This is a string")		;literals are initialized  
(print 42)				;before user code.

(let ((x (foo 42))			;Call to unknown function.
      (y (bar "This is a string")))	;Literals are coelsced.
  (when (baz x)
    (print y)))
      

;;; A function with fairly complicated argument parsing.
(defun foo-bar (x &optional (y 1) &rest keys
		  &key (a 'base-char) &allow-other-keys)
  (multiple-value-call 'some-function
    (block foo
      (if (minusp y)
	  (setq y x)
	  (return-from foo (values y keys))))
    (baz y)
    42
    #'(lambda (arg)			;A closure over a.
	(make-array arg :element-type a)))) ;Keyword arg.

