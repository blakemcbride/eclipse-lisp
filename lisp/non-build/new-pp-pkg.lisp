(cl:in-package :cl)

#+try
(progn
  (cl:load "new-pp-pkg.lisp")
  (cl:in-package :pp)
  (cl:compile-file "new-circle") (cl:load "new-circle")
  (cl:compile-file "new-pretty") (cl:load "new-pretty")
  )

(defpackage :pp
  (:use :eclipse)
  (:shadow
		pprint-logical-block pprint-tab pprint-newline pprint-indent
		pprint-fill pprint-linear pprint-newline
		write-toplevel PPRINT-TABULAR)
  (:import-from :eclipse
   atomicp print-fixnum10 vars
   with-unique-names dispatching-print *current-level* *current-length*
   *locating-circularities* *abbreviation-happened*
   *LAST-ABBREVIATED-PRINTING* *DEFAULT-RIGHT-MARGIN*
   nil BUFFERED-CHARACTER-OUTPUT-STREAM
   encapsulating-stream
   encapsulating-stream-stream
   STREAM-LINE-LENGTH line-length output-buffer stream-line-column))