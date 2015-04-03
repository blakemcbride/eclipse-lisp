;;; Either we must make sure that GC code is thread-safe, or malloc
;;; calls must lock.  In the latter casee, we must be certain that the
;;; scheduler code does not cons.

;;; We need to make sure that when all processes are blocked, we don't
;;; consume processor resources.  This implies some sort of
;;; Allegro-like process-wait-for-input-available which will be
;;; handled not by having the scheduler test things, but by waking up
;;; via SIGIO.  Alternatively, perhaps it would be enough just to have
;;; the default process go to (operating system) sleep.  Then the
;;; usual sheduler alarm will wake up from sleep for testing, then go
;;; to sleep again.  In any case, it is NOT a good idea to leave IO
;;; blocked (by the operating system), because then OS-blocked
;;; processes will consume Lisp quantums doing nothing.

;;; Make sure process-yield, -wait can be called within without-scheduling.

;;; Exiting Lisp should kill all processes.  process-kill should make sure the
;;; process gets activated.

;;; SPECIAL BINDING:
;;; 1. Switching stack groups must keep track of the bindings it
;;;    creates so that the old values can be restored when the stack
;;;    group is unswitched.
;;; 2. We need a way for lisp code to access the "stored old value":
;;;    a. It might want to change the global value -- any (setf
;;;       symbol-value) would just change a stack's dynamic binding,
;;;       and there's no way for code to know if the binding is
;;;       dynamically bound.
;;;    b. code might want to perform introspection about whether a
;;;       binding is dynamically bound, etc.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PROCESS DATABASE

(defparameter *multiprocessing-p* t)
(defparameter *processes* nil)
(defparameter *default-stack-size* 512)
(defparameter *default-process-quantum* 1)

;;; Processes use their own structure to form a chain, beginning at
;;; *current-process*, *wait-queue*
(defparameter *current-process* nil)
(defparameter *wait-queue* nil)

;;; This process will receive input in preference to any other process
;;; waiting on input.  This is initially the top-level Lisp listener.
;;; Invoke-debugger BINDS this to the process in which the debugger is
;;; running. 
(defparameter *focussed-process* nil)

;;; Lucid defines *keyboard-interrupt-process* as the process to which
;;; keyboard interrupts should go (initially the initial process).  If
;;; the process is not a live process, or another interrupt occurs
;;; before the first is handled, a new process is created which
;;; handles the interrupt.

(defun process-queue-list (queue)
  (loop for process = queue
	then (process-next process)
	while process
	collect process))
(defun process-queue-find (name queue)
  (loop for process = queue
	then (process-next process)
	while process
	when (equalp (process-name process name))
	return process))

(defun ALL-PROCESSES () *processes*)
(defun CURRENT-PROCESS () *current-process*)
(defun ACTIVE-PROCESSES ()
  (nreconc (process-queue-list *current-process*)
	   (process-queue-list *wait-queue*)))

(defun FIND-PROCESS (name &optional (errorp t))
  (or (process-queue-find name *current-process*)
      (process-queue-find name *wait-queue*)
      (when errorp (error "No process named ~s." name))))

(defun SHOW-ALL-PROCESSES (&optional (stream *standard-output*)
				     verbose)
  ;; IWBNI we sorted on state/priority/name
  (dolist (process (all-processes))
    (show-process process stream verbose)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SCHEDULER

(defmacro WITHOUT-SCHEDULING (&body body)
  `(let ((*multiprocessing-p* nil))
     ,@body))

(defmacro ATOMIC-INCF (reference &optional (increment 1)) ;clim
  `(without-scheduling (the fixnum (incf (the fixnum ,reference) ,increment))))
(defmacro ATOMIC-DECF (reference &optional (increment 1)) ;clim
  `(without-scheduling (the fixnum (decf (the fixnum ,reference) ,increment))))

;;; We allow lower priority processes to run occasionally by using the
;;; following scheme: When a process does somehow get scheduled and
;;; then yields, its priority is decremented/incremented towards zero
;;; before it is added to the active process queue.  If the priority
;;; reaches zero, then its original priority is restored.  Since
;;; priorities never get bumped until after the process has been
;;; scheduled, negative priority process will never run while any
;;; positive priority process are active -- this includes active but
;;; waiting processes!  

(defun process-allow-schedule () (process-yield)) ;ilu
(defun PROCESS-YIELD (&aux repeatp)		;clim
  (without-scheduling
   (let* ((process (current-process))
	  (priority (process-current-priority process)))
     (setq *current-process* (process-next process))
     (setf (process-current-priority process)
	   (cond ((zerop priority) (process-priority process))
		 ((plusp priority) (1- priority))
		 (t (1+ priority))))
     (process-activate process)
     (setq repeatp process (current-process))))
  (scheduler repeatp)
  t)

;;; Returns true if focus is already handled (i.e. other waiting
;;; processes can run).
(defun handle-focus ()
  (let* ((process *focussed-process*)
	 (function (when process
		     (process-wait-function process))))
    (not (when (and function
		    (apply function (process-wait-arguments process)))
	   (process-schedule process)))))
	
      

(defun insert-process-after (previous-process new-process
					      &optional udpatep)
  (when updatep (setf (process-next new-process) updatep))
  (if previous
      (setf (process-next previous) new-process)
      (setf *current-process* new-process)))

(defun process-deactivate (process)
  (let ((currentp (eq process (current-process))))
    (without-scheduling
     (loop for proc = *current-process* then (process-next proc)
	   and previous = nil then proc
	   when (eq process proc)
	   do (return (insert-process-after previous
					    (process-next proc)))))
    (when currentp (scheduler))))

(defun process-activate (process)
  (without-scheduling
   (loop with priority = (process-current-priority process)
	 for proc = *current-process* then (process-next proc)
	 and previous = nil then proc
	 when (< (process-current-priority proc) priority)
	 do (return
	     (insert-process-after previous process
				   (process-next proc)))
	 finally (insert-process-after previous process))))

;;; In order to allow for wait functions to be run very quickly,
;;; without a context switch, ILU says wait functions are run in the
;;; context of the scheduler, which is never defined.
;;; However, this leaves the following issues:
;;;  1. What handers are available during the wait call.
;;;  2. Should interrupts be run before the wait test? In what
;;;  context?
;;;  3. What if the wait function or interrupts change state of
;;;  things? 

;;; Repeatp is a boolean indicating that the new current-process is
;;; the same as the one that just ran.  This optimization helps us
;;; avoid an expensive context switch in the common case where there
;;; is one (relatively) high priority process.

(defun scheduler (&optional repeatp)
  (when *multiprocessing-p*
    (let ((process (current-process)))
      (unless repeatp (switch-context process))
      (let ((interrupts (process-interrupts process)))
	(when interrupts
	  (setf (process-interrupts process) nil)
	  (dolist (interrupt interrupts)
	    (apply #'funcall interrupt))))
      (let ((wait (process-wait-function process)))
	(when wait
	  (if (apply wait (process-wait-args process))
	      (setf (process-wait-function process) nil
		    (process-wait-args process) nil
		    (process-whostate process)
		    (pop (process-whostack process)))
	      (process-yield)))))))

(defun signal-killed-process (process)
  (error 'control-error
	 :format-control "Attempt to run killed process ~s."
	 :format-arguments (list process)))
  
(defun make-stack (size bindings))	;!!!
(defun switch-context (process)	;!!!
  (let ((stack (process-stack process)))
    (unless stack
      (process-interrupt (process-next process)
			 #'signal-killed-process process)
      (process-yield process))
    ;; guts go here
    ))
  
(defun exit-process (&optional restartp) ;!!!
  (let ((process (current-process)))
    (cond (restartp
	   (apply #'funcall (process-initial-form process)))
	  (t (process-deactivate process)
	     (setf (process-stack process) nil
		   *processes* (delete process *processes*))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PROCESS DATA STRUCTURE

(defstruct (PROCESS
	    (:predicate PROCESSP)
	    (:copier nil)
	    (:constructor MAKE-INACTIVE-PROCESS)
	    (:print-object
	     (lambda (process stream)
	       (print-unreadable-object (object stream :identity t :type t)
		 (princ (process-name process) stream)
		 (write-char #\space stream)
		 (princ (or (process-whostate process)
			    (process-state process))
			stream)))))
  (name nil :read-only t)
  (whostate nil)
  (whostack nil)
  (initial-form nil :read-only t)
  (wait-function nil)
  (wait-args nil)
  (quantum *default-process-quantum*)
  (priority 0)
  (current-priority 0)
  (run-reasons nil)
  (arrest-reasons nil)
  (stack nil)
  (interrupts nil)
  (next nil))

(defun PROCESS-ALIVE-P (process) (not (null (process-stack process))))

(defun PROCESS-ACTIVE-P (process)
  (and (process-run-reasons process)
       (null (process-arrest-reasons process))))

(defun PROCESS-STATE (process)
  (cond ((not (process-alive-p process)) :killed)
	((process-active-p process) :active)
	(t :inactive)))

(defun SHOW-PROCESS (&optional (process (current-process))
			       (stream *standard-output*) verbose)
  (unless (processp process)
    (setf process (find-process (if (symbolp process)
				    (string-downcase process)
				    process)
				t)))
  (pprint-logical-block (stream nil)
    (format stream "~a ~@_~a ~@_~s ~2i"
	    (process-name process) process
	    (process-state process))
    (when verbose
      (format stream "~%~@<Initial-form: ~2i~@_~s~:>"
	      (process-initial-form process)))
    (flet ((show (name value)
		 (when (or verbose value)
		   (format stream "~%~@<~a: ~2i~@_~s~:>" name
			   value))))
      (show "Whostate" (process-whostate process))
      (show "Priority" (process-priority process))
      (show "Quantum" (process-quantum process))
      (show "Run reasons" (process-run-reasons process))
      (show "Arrest reasons" (process-arrest-reasons process))
      (show "Wait function" (process-wait-function process))
      (show "Wait arguments" (process-wait-args process)))))

(defun MAKE-PROCESS (function &rest keys &key name &allow-other-keys) ;clim
  (fork-process keys function))
			      
(defun FORK-PROCESS (name-or-key-list function &rest args)
  (let* ((plist (when (consp name-or-key-list)
		  name-or-key-list))
	 (priority (getf plist :priority 0))
	 (process (make-inactive-process
		   :name (if plist
			     (or (getf plist :name) (gensym "PROCESS"))
			     name-or-key-list)
		   :whostate (get plist :whostate)
		   :initial-form (cons function args)
		   :quantum (or (getf plist :quantum) 1)
		   :priority priority 
		   :current-priority priority
		   :run-reasons (or (getf plist :name) (list :start))
		   :arrest-reasons nil
		   :stack (make-stack (getf plist :stack-size
					    *default-stack-size*)
				      (getf plist :bindings
					    *default-process-bindings*)))))
    (push process *processes*)
    (when (process-active-p process)
      (process-activate process))
    process))

(defun PROCESS-ADD-ARREST-REASON (process reason)
  (without-scheduling
   (let ((deactivatep (process-active-p process)))
     (push reason (process-arrest-reasons process))
     (when deactivatep (process-deactivate process))))
  process)

(defun PROCESS-ADD-RUN-REASON (process reason)
  (without-scheduling
   (let ((reasons (process-run-reasons process)))
     (unless (or reasons
		 (process-arrest-reasons process))
       (process-activate process))
     (setf (process-run-reasons process) (cons reason reasons))))
  process)

(defun PROCESS-REVOKE-ARREST-REASON (process reason)
  (without-scheduling
   (setf (process-arrest-reasons process)
	 (delete reason (process-arrest-reasons process)))
   (when (process-active-p process) (process-activate process)))
  process)

(defun PROCESS-REVOKE-RUN-REASON (process reason)
  (without-scheduling
   (let ((deactivatep (process-active-p process)))
     (setf (process-run-reasons process)
	   (delete reason (process-run-reasons process)))
     (when deactivatep (process-deactivate process))))
  process)

(defun disable-process (process) (process-disable process)) ;clim
(defun PROCESS-DISABLE (process)
  (without-scheduling
   (when (process-active-p process)
     (process-deactivate process)
     (setf (process-run-reasons process) nil
	   (process-arrest-reasons process) nil)))
  process)

(defun enable-process (process) (process-enable process)) ;clim
(defun PROCESS-ENABLE (process)
  (without-shcheduling
   (unless (process-active-p process)
     (setf (process-run-reasons process) (list :enable)
	   (process-arrest-reasons process) nil)
     (process-activate process)))
  process)


(defun PROCESS-WAIT (whostate function &rest args)
  (without-scheduling
   (unless (and (apply function args)
		(handle-focus))
     (let ((process (current-process)))
       (push (process-whostate process)
	     (process-whostack process))
       (setf (process-whostate process) whostate
	     (process-wait-function process) function
	     (process-wait-args process) args))
     (process-yield)))
  ;; IWBNI we could return what the wait function returned.
  t)					;indicates success


(defun PROCESS-PRESET (process function &rest args) ;CL-HTTP/miniproc
  !!!)

(defun RESTART-PROCESS (process)	;clim
  (process-interrupt (process-enable process)
		     #'exit-process t))

(defun DESTROY-PROCESS (process) (process-kill process)) ;clim
(defun PROCESS-KILL (process)
  (process-interrupt (process-enable process)
		     #'exit-process))

(defun PROCESS-INTERRUPT (process function &rest args)
  (without-interrupts
   (push (cons function args) (process-interrupts process))))


(defmacro UNWIND-PROCESS (form &rest cleanups) ;CL-HTTP/miniproc
  !!!)

(defmacro PROCESS-LOOP (&key name context with declare initially while
			     until do finally preset)	;CL-HTTP/miniproc
  !!!)
(defun PROCESS-ITERATE (process &key name context initially while do
				finally preset)	;CL-HTTP/miniproc
  !!!)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TIMERS

;;; MUST return nil if timed out, true otherwise!!!
(defun PROCESS-WAIT-WITH-TIMEOUT (whostate timeout function &rest args)
  (let ((expiration (+ timeout (get-universal-time)))
	(expiredp nil))
    (process-wait whostate #'(lambda (&rest args)
			       (if (>= (get-universal-time) timeout)
				   (setq expiredp t)
				   (apply function args)))
		  args)
    (not expiredp)))

(defun PROCESS-SLEEP (seconds)		;CL-HTTP/miniproc
  (process-wait-with-timeout "Sleeping" seconds #'identity t))

(defun LISP-SLEEP (seconds)
  (let ((s (ceiling (* seconds 1000))))
    (etypecase s
      (fixnum (millisleep (fixnum-int s)))))
  nil)

(defun SLEEP (seconds)
  (if *multiprocessing-p*
      (process-sleep seconds)
      (lisp-sleep seconds)))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LOCKS

;;; There are two ways to implement waiting for blocked locks:
;;;
;;; PROCESS-WAIT: The wait form is (process-lock-locker #<lock>),
;;; which means that the process will keep retesting each time it
;;; scheduled until someone releases the lock.  Thus high priority
;;; processes are likely to see newly released locks before low ones.

#+wait-locks
(progn
  (defun lock-wait (lock whostate timeout)
    (if timeout
	(process-wait-with-timeout whostate timeout #'process-lock-locker lock)
	(process-wait whostate #'process-lock-locker lock)))
  (defun lock-free (lock) lock)
  )

;;; DEACTIVATION: A lock maintains a queue queue of blocked processes
;;; for each lock.  When a lock is released, the first queued process
;;; is reactivated.  This might be more efficient and "fairer", but it
;;; is not clear if any user code could depend on blocked processes
;;; appearing active.  Allegro apparently uses this technique.

#-wait-locks
(progn
  (defun lock-wait (lock whostate timeout)
    (when timeout (error "Lock timeouts are not supported."))
    (let ((process (current-process)))
      (without-scheduling
       (push process (process-lock-queue lock)))
      (process-add-arrest-reason process whostate)
      lock))
  (defun lock-free (lock &aux process)
    (without-scheduling
     (setq process (pop (process-lock-queue lock))))
    (process-revoke-run-reason process (process-whostate process))))

(defstruct (PROCESS-LOCK
	    (:copier nil)
	    (:constructor MAKE-PROCESS-LOCK (&key (name "Lock")))
	    (:constructor MAKE-LOCK (&optional (name "Lock"))) ;clim
	    (:constructor MAKE-RECURSIVE-LOCK (&optional (name "Lock")))) ;clim
  (name nil)
  (locker nil)
  #-wait-locks
  (queue nil))
  

;;; Are there any legitimate norecurive cases where the lock is
;;; already owned by ocker and we still want to block?  For example,
;;; what if the locker is not the current process?
(defun PROCESS-LOCK (lock &optional (locker (current-process))
			  (whostate (process-lock-name lock))
			  timeout (norecursive t)
			  &aux owner)
  (without-scheduling
   (setq owner (process-lock-locker lock))
   (cond ((null owner)
	  (setf (process-lock-locker lock) process))
	 ((eq locker owner)
	  (setq owner (if norecursive :error :recursive)))))
  (case owner
    ((nil) lock)
    (:recursive nil)			;Used by with-process-lock
    (:error (error "~a already owns ~s." locker lock))
    (t (lock-wait lock whostate timeout))))
		 
(defun PROCESS-UNLOCK (lock &optional (locker (current-process))
			    (errorp t))
  (without-scheduling
   (let ((owner (process-lock-locker lock)))
     (when (or (null errorp) (null owner)
	       (eq locker owner))
       (setf (process-lock-locker lock) nil
	     errorp nil))))
  (if errorp
      (error "~s is not owned by ~s." lock locker)
      (lock-free lock)))

(defmacro WITH-PROCESS-LOCK ((lock &key norecursive
				   (whostate nil whostatep))
			     &body body)
  (with-unique-names (locker unlockp)
    (rebinding (lock)
      `(let (,unlockp (,locker (current-process)))
	 (unwind-protect
	     (progn
	       (setq ,unlockp
		     (process-lock ,lock ,locker
				   ,(if whostatep
					whostate
					`(process-lock-name ,lock))
				   ,norecursive))
	       ,@body)
	   ;; Check helps if we get killed before acquiring lock
	   (when (and ,unlockp (eq (process-lock-locker ,lock) ,locker))
	     (process-unlock ,lock ,locker)))))))

(defmacro WITH-LOCK-HELD ((lock &optional (whostate nil whostatep)) ;clim
			  &body body)	
  `(with-process-lock (,lock ,@(when whostatep `((:whostate ,whostate)))
			     :norecursive t) ,@body))

(defmacro WITH-RECURSIVE-LOCK-HELD ((lock &optional (whostate nil whostatep)) ;clim
			  &body body)	
  `(with-process-lock (,lock ,@(when whostatep `((:whostate ,whostate)))
			     :norecursive nil) ,@body))