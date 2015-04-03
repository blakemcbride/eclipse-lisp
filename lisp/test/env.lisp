(defun variable-information1 (name &optional env)
  (multiple-value-bind (kind binding decs)
      (variable-information name env)
    (values kind (not (null binding))
	    (loop for (dec val) on decs by #'cddr
		  when (member dec '(ignore ignorable type))
		  nconc (list dec val)))))
(defun function-information1 (name &optional env ignore-decs)
  (multiple-value-bind (kind binding decs)
      (function-information name env)
    (values kind (not (null binding))
	    (unless ignore-decs decs))))

(defparameter e1
    (augment-environment
     nil
     :variable '(local local2 local3 special-binding)
     :symbol-macro '((symbol-macro some-definition))
     :declare '((double-float pi)
		(fixnum unknown special-binding)
		(type (or fixnum null) *print-level*)
		(special special-binding))))

(defparameter e2
    (augment-environment
     e1
     :variable '(special-binding local2 *print-level*)
     :declare '((type number local)
		(special local2 local3))))

(defparameter e3
    (augment-environment
     e2
     :declare '((fixnum local local3)
		)))

(defparameter e4
    (augment-environment
     e3
     :declare '((special local3))))

(deftest no-variable-binding (variable-information1 (gensym)) nil nil nil)
(deftest typed-no-variable-binding (variable-information1 'unknown e1) nil nil (type fixnum))
(deftest constant-nil (variable-information1 'pi) :constant nil nil)
(deftest constant-type (variable-information1 'pi e1) :constant nil (type double-float))
(deftest keyword (variable-information1 :foo nil) :constant nil nil)
(deftest special-nil (variable-information1 '*print-level*) :special nil nil)
(deftest special-type (variable-information1 '*print-level* e1) :special nil (type (or fixnum null)))
(deftest local-variable (variable-information1 'local e1) :lexical t nil)
(deftest symbol-macro (variable-information1 'symbol-macro e1) :symbol-macro t nil)
  
(deftest typed-local (variable-information1 'local e2) :lexical t (type number))
;; IMPLEMENTATION NOTE: We return all applicable type declarations.
;; To get the intersected  type behavior described in CLtL2, one must
;; intersect all the types in the plist.
(deftest subtyped-local (variable-information1 'local e3) :lexical t (type fixnum type number))

(deftest special-binding-env (variable-information1 'special-binding e1) :special t (type fixnum))
(deftest local-shadowing-special (variable-information1 'special-binding e2) :lexical t nil)
(deftest special-binding-shadowing-local (variable-information1 'local2 e2) :special t ())
(deftest special-shadowing-local (variable-information1 'local3 e2) :special t ())
(deftest special-accumulates-types (variable-information1 'local3 e3) :special t (type fixnum))
(deftest ignores-superflous-specials (variable-information1 'local3 e4) :special t (type fixnum))
(deftest rebinding-special (variable-information1 '*print-level* e2) :special t ())
(deftest rebinding-double-special
  (variable-information1
   'a (augment-environment (augment-environment
			    nil :variable '(a) :declare '((special a)))
			   :variable '(a) :declare '((special a))))
  :special t nil)


(defparameter fe1
    (augment-environment nil :function '(a-function another-function another)
			 :macro '((baz baz-def))
			 :declare '((inline a-function))))
(defparameter fe2
    (augment-environment fe1
			 :function '(baz)
			 :macro '((another another-def))
			 :declare '((notinline a-function)
				    (dynamic-extent #'another-function))))


(deftest no-function (function-information1 (gensym)) nil nil nil)
(deftest global-function (function-information1 'list nil t) :function nil nil)
(deftest local-function (function-information1 'a-function fe1) :function t (inline inline))
(deftest shadowing-notinline (getf (nth-value 2 (function-information1 'a-function fe2)) 'inline) notinline)
(deftest dynamic-function (function-information1 'another-function fe2) :function t (dynamic-extent t))
(deftest macro-shadowing-function (function-information1 'another fe2) :macro t nil)
(deftest function-shadowing-macro (function-information1 'baz fe2) :function t nil)

(defparameter de1
    (augment-environment nil :declare '((optimize (speed 2) (space 3))
					(optimize (debug 0)))))
(deftest optimize1 (assoc 'speed (declaration-information 'optimize de1)) (speed 2))
(deftest optimize2 (assoc 'space (declaration-information 'optimize de1)) (space 3))
(deftest optimize3 (assoc 'debug (declaration-information 'optimize de1)) (debug 0))
(deftest optimize4 (assoc 'safety (declaration-information 'optimize de1)) (safety 1))

(defparameter ce1
    (augment-environment nil
			 :declare '((type float x)
				    (ftype (function (fixnum) fixnum) a-function)
				    (optimize (speed 2)))))
(deftest combined-type (variable-information1 'x ce1) nil nil (type float))
(deftest combined-ftype (function-information1 'a-function ce1) nil nil
	 (ftype (function (fixnum) fixnum)))
(deftest combined-opt (assoc 'speed (declaration-information 'optimize ce1)) (speed 2))

(setf (global-declaration 'zzz 'eclipse::global-variable 'eclipse::type) 'fixnum)
(setf (global-declaration 'zzz 'eclipse::global-function 'eclipse::ftype) '(function (fixnum) fixnum))

(defparameter local-decs (augment-environment nil
					      :declare '((ignore zzz #'zzz))))
							  
(deftest global-variable-declaration (variable-information1 'zzz)
  nil nil (type fixnum))
(deftest global-function-declaration (function-information1 'zzz)
  nil nil (ftype (function (fixnum) fixnum)))
(deftest unshadowed-global-variable-declaration (variable-information1 'zzz local-decs)
  nil nil (ignore t type fixnum))
(deftest unshadowed-global-function-declaration (function-information1 'zzz local-decs)
  nil nil (ignore t ftype (function (fixnum) fixnum)))
