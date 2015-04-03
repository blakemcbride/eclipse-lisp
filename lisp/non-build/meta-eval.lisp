;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BINDING VALUE SEMANTICS AND META-EVALUATION
#|
Bindings and literals are evalution metaobjects -- they cannot be seen
or used by general user code.

During the course of code processing, it may be necessary to
meta-evaluate code -- i.e. partially evaluate it using evaluation
metaobjects as stand-ins for the actual value that will appear at
run-time.  Thus meta-evaluation is a "normal" lisp interpreter except
that some objects -- specifically, evaluation metaobjects -- are not
treated as self-evaluating atoms, but rather as objects that meta-evaluate
to something else.  For example, an environment binding may
meta-evaluate to the binding-value, if known.  A compile-file literal
metaobject may meta-evaluate to the original lisp objects.

Because evaluation metaobjects are not supposed to appear in
interpreted code, it might be possible to just add evaluation rules
for these objects to the normal lisp interpreter.  However, it will be
cleaner, and may help us avoid compilation or other bootstrapping
circularity problems if we defined a separate meta-evaluation code
walker, which inherits from the lisp-interpreter.

In any case, the values which may be stored in the binding-value of
processing-time environment bindings, should be values which may be
meta-evaluated.  In the case of an interpreter, the values should
always be real lisp objects.  In a compiler, they may include other
bindings, lists including other bindings, etc.
|#

;;; If env.lisp is changed to have the initial value of bindings be
;;; unbound-flag instead of 'unbound-flag, this can be go away.
(defun unboundp1 (x) (eq x 'unbound-flag))

(defstruct (meta-evaluator (:include lisp-interpreter)))

(defparameter *meta-evaluator* (make-meta-evaluator))

(defmethod EVALUATE ((object BINDING) (processor META-EVALUATOR)
		     environment continuation)
  (declare (ignore environment))
  (resume continuation processor 
	  (let ((value (binding-value object)))
	    (if (unboundp1 value)
		object
		value))))


;;; Until we understand partial evaluation better, the only place
;;; where we will use the meta-evalator is in simplifying function
;;; calls at compile-time.

