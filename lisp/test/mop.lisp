;;; N.B.: File "mop-defs" must be LOADED before this file is compiled.

;;; 1.2 The Basic Backstage Structures

(defclass rectangle ()
  ((height :initform 0.0 :initarg :height)
   (width :initform 0.0 :initarg :width)))

(defclass color-mixin ()
  ((cyan :initform 0 :initarg :cyan)
   (magenta :initform 0 :initarg :magenta)
   (yellow :initform 0 :initarg :yellow)))

(defclass color-rectangle (color-mixin rectangle)
  ((clearp :initform (y-or-n-p "But is it transparent?")
	   :initarg :clearp :accessor clearp)))

(defun vertical-stroke (h w) (format t "~&STROKE ~s ~s" h w))
(defun set-brush-color (c m y) (format t "~&COLOR ~s ~s ~s" c m y))

(defmethod paint ((x rectangle))
  (vertical-stroke (slot-value x 'height)
		   (slot-value x 'width)))

(defmethod paint :before ((x color-mixin))
  (set-brush-color (slot-value x 'cyan)
		   (slot-value x 'magenta)
		   (slot-value x 'yellow)))

(defmethod paint ((x color-rectangle))
  (unless (clearp x) (call-next-method)))

(defclass color-chart (rectangle color-mixin) ())

(defparameter door
  (make-instance 'color-rectangle
	       :width 38 :height 84 :cyan 60 :yellow 55 :clearp nil))

(deftest door-data
  (list (class-name (class-of door))
	(slot-value door 'height)
	(slot-value door 'width)
	(slot-value door 'cyan)
	(slot-value door 'magenta)
	(slot-value door 'yellow)
	(slot-value door 'clearp))
  (COLOR-RECTANGLE 84 38 60 0 55 nil))

(deftest color-rectangle-is-standard
  (class-name (find 'color-rectangle (subclasses (find-class 'standard-object))
		    :key #'class-name))
  color-rectangle)

;;; 2.2 Browsing Classes
;;; 2.2.2 Regenerating Class Definitions

(deftest rectangle-def
  (generate-defclass (find-class 'rectangle))
  (DEFCLASS RECTANGLE
    (STANDARD-OBJECT)
    ((HEIGHT :INITFORM 0.0 :INTARG :HEIGHT)
     (WIDTH :INITFORM 0.0 :INTARG :WIDTH))))

;;; 2.2.3 Displaying Inherited Information

(deftest color-rectangle-def
  (generate-defclass* (find-class 'color-rectangle))
  (DEFCLASS* COLOR-RECTANGLE
    (COLOR-RECTANGLE COLOR-MIXIN RECTANGLE STANDARD-OBJECT T)
    ((HEIGHT :INITFORM 0.0 :INTARG :HEIGHT :INHERITED-FROM RECTANGLE)
     (WIDTH :INITFORM 0.0 :INTARG :WIDTH :INHERITED-FROM RECTANGLE)
     (CYAN :INITFORM 0 :INTARG :CYAN :INHERITED-FROM COLOR-MIXIN)
     (MAGENTA :INITFORM 0 :INTARG :MAGENTA :INHERITED-FROM COLOR-MIXIN)
     (YELLOW :INITFORM 0 :INTARG :YELLOW :INHERITED-FROM COLOR-MIXIN)
     (CLEARP :INITFORM (Y-OR-N-P "But is it transparent?") :INTARG :CLEARP
	     :reader clearp :writer (setf clearp)))))

;;; 2.2.4 Ordering of Classes in Multiple Inheritance

(deftest class-precedence-list
  (mapcar #'class-name (class-precedence-list1 (find-class 'color-rectangle)))
  (COLOR-RECTANGLE COLOR-MIXIN RECTANGLE STANDARD-OBJECT T))

(deftest class-precedence-list2
  (mapcar #'class-name (class-precedence-list1 (find-class 'color-chart)))
  (COLOR-CHART RECTANGLE COLOR-MIXIN STANDARD-OBJECT T))

(deftest rectangle-order
  (in-order-p (find-class 'color-mixin)
	      (find-class 'rectangle))
  nil)
(deftest standard-object-order
  (in-order-p (find-class 'standard-object)
	      (find-class 't))
  t)

;;; 2.3 Browsing Generic Functions
;;; 2.3.1 Regenerating Generic Function and Method Definitions

(deftest generic-function-def 
  (generate-generic-function 'paint :show-body t)
  ;; :show-body only works in closette.
  ((DEFGENERIC PAINT (X))
   (DEFMETHOD PAINT ((X COLOR-RECTANGLE))
     #|(BLOCK PAINT (UNLESS (CLEARP X) (CALL-NEXT-METHOD)))|#)
   (DEFMETHOD PAINT :BEFORE ((X COLOR-MIXIN))
     #|(BLOCK PAINT
	    (SET-BRUSH-COLOR (SLOT-VALUE X 'CYAN)
			     (SLOT-VALUE X 'MAGENTA)
			     (SLOT-VALUE X 'YELLOW)))|#)
   (DEFMETHOD PAINT ((X RECTANGLE))
     #|(BLOCK PAINT
	    (VERTICAL-STROKE (SLOT-VALUE X 'HEIGHT) (SLOT-VALUE X 'WIDTH)))|#)))

(deftest generic-function-def2
  (generate-generic-function '(setf clearp) :show-body t)
  ((DEFGENERIC #1=(SETF CLEARP) (eclipse::NEW-VALUE eclipse::instance))
   (DEFMETHOD #1# (eclipse::NEW-VALUE (eclipse::INSTANCE COLOR-RECTANGLE))
     #|(SETF (SLOT-VALUE OBJECT 'CLEARP) NEW-VALUE)|#)))

;;; 2.3.2 Finding All Generic Functions

(deftest paint-is-generic
  (when (typep (fdefinition 'paint) 'generic-function) 'paint)
  #+AMOP-example
  (generic-function-name (find 'paint (all-generic-functions)
			       :key #'generic-function-name))
  paint)

;;; 2.3.3 Finding Relevant Generic Functions

(deftest relevant-generic-functions
  (set-exclusive-or
   (mapcar #'generic-function-name
	   (relevant-generic-functions (find-class 'color-rectangle)
				       (find-class 'standard-object)
				       :elide-accessors-p nil))
   '(clearp paint (setf clearp))
   :test #'equal)
  nil)

;;; 2.3.4 Finding All Slot Accessors

(deftest relevant-generic-functions2
  (mapcar #'generic-function-name
	  (relevant-generic-functions (find-class 'color-rectangle)
				      (find-class 'standard-object)
				      :elide-accessors-p t))
  (paint))

;;; ???
(defclass position1 () (x y))
(defclass cad-element (position1) ())
(defclass display-element (position1) ())
(defclass displayable-cad-element (display-element cad-element) ())

(deftest has-diamond (has-diamond-p (find-class 'position1)) t)

;;; 2.4 Programmatic Creation of New Classes

(defclass circle () ())
(defclass orange () ())
(defclass magenta () ())
(defclass top-labeled () ())
(defclass bottom-labeled () ())

(deftest programmatic-classes
  (let ((i1 (make-programmatic-instance '(circle orange top-labeled)))
	(i2 (make-programmatic-instance '(circle magenta bottom-labeled)))
	(i3 (make-programmatic-instance '(circle orange top-labeled))))
    (set-difference
     (mapcar #'(lambda (i) (class-name (class-of i))) (list i1 i2 i3))
     (mapcar #'class-name (class-direct-subclasses (find-class 'circle)))))
  nil)

;;; 3.4 Class Precedence Lists
;;; 3.4.1 Alternative Class Precednece Lists

(defclass a () ())
(defclass b () ())
(defclass c () ())
(defclass s (a b) ())
(defclass r (a c) ())
(defclass q (s r) ())
(defclass q-flavors (s r) () (:metaclass flavors-class))
(defclass a-flavor () () (:metaclass flavors-class))
(defclass q-loops (s r) () (:metaclass loops-class))

(deftest metaclass-cpl
  (list (mapcar #'class-name (class-precedence-list1 (find-class 'q-flavors)))
	(mapcar #'class-name (class-precedence-list1 (find-class 'q-loops)))
	(mapcar #'class-name (class-precedence-list1 (find-class 'q)))
	(mapcar #'class-name (class-precedence-list1 (find-class 'pie))))
  ((Q-FLAVORS S A B R C STANDARD-OBJECT T)
   (Q-LOOPS S B R A C STANDARD-OBJECT T)
   (Q S R A C B STANDARD-OBJECT T)
   (PIE APPLE FRUIT CINNAMON SPICE FOOD STANDARD-OBJECT T)))

;;; 3.5 Slot Inheritance
(defclass credit-rating ()
  ((level :attributes (date-set time-set)))
  (:metaclass attributes-class))

(defclass monitored-credit-rating (credit-rating)
  ((level :attributes (last-checked interval)))
  (:metaclass attributes-class))

(deftest credit
  (let ((credit (make-instance 'credit-rating)))
    (values (setf (slot-attribute credit 'level 'date-set) "12/15/90")
	    (slot-attribute credit 'level 'date-set)
	    (slot-value credit 'all-attributes)
	    (slot-value (make-instance 'monitored-credit-rating) 'all-attributes)))
  "12/15/90"
  "12/15/90"
  ((LEVEL (DATE-SET . "12/15/90") (TIME-SET)))
  ((LEVEL (LAST-CHECKED) (INTERVAL) (DATE-SET) (TIME-SET))))

;;; Exercise 3.2

(defclass mumble-c1 ()
  ((foo :initform 100))
  (:metaclass encapsulated-class))
(defclass mumble-c2 (mumble-c1)
  ((foo :initform 200))
  (:metaclass encapsulated-class))
(defmethod mumble ((o mumble-c1)) (private-slot-value o 'foo (find-class 'mumble-c1)))
(defmethod mumble ((o mumble-c2))
  (+ (private-slot-value o 'foo (find-class 'mumble-c2))
     (call-next-method)))

(deftest mumble
  (values (mumble (make-instance 'mumble-c1))
	  (mumble (make-instance 'mumble-c2)))
  100 300)


;;; 3.7.1 Monitoring Slot Access
(defclass monitored-foo ()
  ((slot1 :accessor monitored-foo-slot1 :initarg :slot1)
   (slot2 :accessor monitored-foo-slot2 :initform 200))
  (:metaclass monitored-class))

(deftest monitored-class
  (progn (reset-slot-access-history)
	 (let ((i (make-instance 'monitored-foo :slot1 100)))
	   (values (setf (slot-value i 'slot1) (monitored-foo-slot2 i))
		   (incf (monitored-foo-slot1 i))
		   (mapcar #'(lambda (item)
			       (list (first item)
				     (third item)))
			   (slot-access-history)))))
  200
  201
  ((SET-SLOT-VALUE SLOT1) (SLOT-BOUNDP SLOT2) (SET-SLOT-VALUE SLOT2)
   (SLOT-VALUE SLOT2) (SET-SLOT-VALUE SLOT1) (SLOT-VALUE SLOT1)
   (SET-SLOT-VALUE SLOT1)))

;;; 3.8 Instance Allocation
(defclass biggy ()
  (a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 
      n1 o1 p1 q1 r1 s1 t1 u1 v1 w1 x1 y1 z1
      a2 b2 c2 d2 e2 f2 g2 h2 i2 j2 k2 l2 m2 
      n2 o2 p2 q2 r2 s2 t2 u2 v2 w2 #|x2 y2 z2
      a3 b3 c3 d3 e3 f3 g3 h3 i3 j3 k3 l3 m3 
      n3 o3 p3 q3 r3 s3 t3 u3 v3 w3 x3 y3 z3
      a4 b4 c4 d4 e4 f4 g4 h4 i4 j4 k4 l4 m4 
      n4 o4 p4 q4 r4 s4 t4 u4 v4 w4 x4 y4 z4|#)
  (:metaclass dynamic-slot-class))

(deftest biggy
  (let ((b (make-instance 'biggy)))
    (values (setf (slot-value b 'a2) 99)
	    (slot-value b 'a2)))
  99 99)

;;; Exercise 3.4
(defclass movable-rectangle (rectangle)
  ((previous-height :allocation :dynamic)
   (previous-width :allocation :dynamic))
  (:metaclass dynamic-slot-class))

(deftest movable-rectangle-allocation
  (let ((r (make-instance 'movable-rectangle)))
    (values (slot-value r 'height)
	    (slot-boundp r 'previous-height)
	    (setf (slot-value r 'previous-height) 9)
	    (slot-value r 'previous-height)))
  0.0 NIL 9 9)

;;; Exercise 3.5
(defclass labeled-rectangle (rectangle)
  ((font :initform 'old-english-12
	 :allocation :xclass))
  (:metaclass class-slot-class))


(deftest labeled-rectangle-allocation
  (let ((lr1 (make-instance 'labeled-rectangle))
	(lr2 (make-instance 'labeled-rectangle)))
    (values (setf (slot-value lr1 'font) 'times-roman-10)
	    (slot-value lr2 'font)))
  TIMES-ROMAN-10 TIMES-ROMAN-10)

#|
(defgeneric ack (x)
  (:generic-function-class counting-gf)
  (:method-class counting-method))

(defmethod ack :before ((x standard-object))
  (declare (ignore x)) nil)
(defmethod ack (x)
  (declare (ignore x)) t)

(deftest call-count
  (values (setf (call-count (function ack)) 0)
	  (mapcar #'(lambda (m) (setf (call-count m) 0)) ;obsolete!
		  (generic-function-methods (function ack)))
	  (ack (make-instance 'standard-object))
	  (ack 1)
	  (call-count (function ack))
	  (mapcar #'call-count
		  (sort (generic-function-methods (function ack))
			#'< :key #'(lambda (m) (length (method-qualifiers m))))))
  0 (0 0) t t 2 (2 1))
|#
