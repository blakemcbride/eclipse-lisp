(defpackage :eclipse-test
  #+eclipse (:use :eclipse)
  (:nicknames :et)
  (:export deftest get-test do-test rem-test
	   rem-all-tests do-tests pending-tests
	   continue-testing *test*
	   *do-tests-when-defined* gives-error)
  #+eclipse
  (:import-from :eclipse *last-abbreviated-printing*)
  ;; host bug: nil fails to be exported from eclipse
  #+lisp-host (:import-from :cl nil))		

;; host bug
#+lisp-host
(import '(nil) :eclipse-test)


#+lisp-host
(do-external-symbols (sym :eclipse-test)
  (import sym :eclipse)
  (import sym :host))

#|
(dolist (sym `(deftest get-test do-test rem-test
	       rem-all-tests do-tests pending-tests
	       continue-testing *test*))
  (import (intern (symbol-name sym) :eclipse) :eclipse-test))
|#