;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 23.3 PRESENTATION METHODS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-presentation-generic-function present-method 
  PRESENT (type-key parameters options object type stream view &key &allow-other-keys)
  (:documentation "generic function for present methods."))

(define-presentation-generic-function accept-method 
  ACCEPT (type-key parameters options type stream view &key &allow-other-keys)
  (:documentation "generic function for accept methods."))

(define-presentation-generic-function describe-presentation-type-method
  DESCRIBE-PRESENTATION-TYPE (type-key parameters options type stream plural-count
				       &key &allow-other-keys)
  (:documentation "generic function for presentation-type-specifier-p methods."))

(define-presentation-generic-function presentation-type-specifier-p-method
  PRESENTATION-TYPE-SPECIFIER-P (type-key parameters options type &key &allow-other-keys)
  (:documentation "generic function for presentation-type-specifier-p methods."))

(define-presentation-generic-function presentation-typep-method
  PRESENTATION-TYPEP (type-key parameters object type)
  (:documentation "generic function for presentation-typep methods."))

(define-presentation-generic-function presentation-subtypep-method
  PRESENTATION-SUBTYPEP (type-key type putative-supertype &key &allow-other-keys)
  (:documentation "generic function for presentation-subtypep methods."))

(define-presentation-generic-function accept-present-default-method
  ACCEPT-PRESENT-DEFAULT (type-key parameters options type stream view default
				   default-supplied-p present-p
				   query-identifier &key &allow-other-keys)
  (:documentation "generic function for accept-present-default methods."))


(define-presentation-generic-function handle-input-error-method
  handle-input-error (type-key parameters options failed-input type stream view
			       &key &allow-other-keys)
  (:documentation "generic function for input-not-of-required-type methods."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 23.3.4 PRESENTATION TYPE FUNCTIONS

(defun DESCRIBE-PRESENTATION-TYPE (type &optional (stream *standard-output*) (plural-count 1))
  (check-type type presentation-type-specifier)
  (with-presentation-type-decoded
   (type-name parameters options) type
   (cond (stream
	  (funcall-presentation-generic-function
	   describe-presentation-type type-name parameters options 
	   type stream plural-count))
	 (t (with-output-to-string (string-stream)
	      (funcall-presentation-generic-function
	       describe-presentation-type type-name parameters options 
	       type string-stream plural-count))))))

(defun PRESENTATION-TYPE-SPECIFIER-P (type)
  (check-type type presentation-type-specifier)
  (with-presentation-type-decoded (type-name parameters options) type
    (funcall-presentation-generic-function
     presentation-type-specifier-p type-name parameters options
     type)))

(defun PRESENTATION-TYPEP (object type)
  (check-type type presentation-type-specifier)
  (with-presentation-type-decoded (type-name parameters nil) type
    (funcall-presentation-generic-function
     presentation-typep type-name parameters object type)))

(defun PRESENTATION-SUBTYPEP (type putative-supertype)
  (check-type type presentation-type-specifier)
  (check-type putative-supertype presentation-type-specifier)
  (with-presentation-type-decoded (type-name) type
    (funcall-presentation-generic-function
     presentation-subtypep type-name type putative-supertype)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 23.4 TYPED OUTPUT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; WITH-OUTPUT-AS-PRESENTATION!!!

(defun PRESENT (object &optional (type (presentation-type-of object))
                       &key
                       (stream *standard-output*)
                       (view (stream-default-view stream))
                       acceptably
                       (for-context-type type)
                       &allow-other-keys)
  (etypecase type (presentation-type-specifier))
  (with-presentation-type-decoded (type-name parameters options) type
    (multiple-value-bind (returned-object returned-type)
        (funcall-presentation-generic-function present type-name parameters options object 
                                               type stream view :acceptably acceptably 
                                               :for-context-type for-context-type)
      (values returned-object (or returned-type type)))))

;;; STREAM-PRESENT!!!

(defun PRESENT-TO-STRING (object &optional (type (presentation-type-of object))
                                 &key
                                 (view +textual-view+)
                                 acceptably
                                 (for-context-type type)
                                 string
                                 index
                                 &allow-other-keys)
  (cond (string (when index (setf (fill-pointer string) index))
                (with-output-to-string (stream string)
                  (present object type :stream stream :view view :acceptably acceptably 
                           :for-context-type for-context-type))
                (values string (fill-pointer string)))
        (t (with-output-to-string (stream)
             (present object type :stream stream :view view :acceptably acceptably 
                      :for-context-type for-context-type)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 23.5 CONTEXT-DEPENDENT (TYPED) INPUT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; *INPUT-CONTEXT*, INPUT-CONTEXT-TYPE, WITH-INPUT-CONTEXT!!!

(defun ACCEPT (type &key 
                    (stream *standard-input*) 
                    (view (stream-default-view stream)) 
                    (default nil default-supplied-p)
                    (default-type type)
                    (prompt t)
                    (prompt-mode ':normal)
                    (display-default prompt)
                    query-identifier
                    insert-default
                    present-p
                    (active-p t)
                    (error-stream *standard-output*)
                    &allow-other-keys)
  (etypecase type (presentation-type-specifier))
  (when present-p
    (return-from accept
      (accept-present-default type stream view default
			      default-supplied-p present-p
			      query-identifier
			      :prompt prompt :prompt-mode prompt-mode
			      :display-default display-default 
                              :insert-default insert-default
			      :active-p active-p))) 
  (unless active-p 
    (return-from accept
      (values default default-type)))
  (handler-bind ((presentation-parse-error
		  #'(lambda (condition)
		      (with-slots (object type stream args) condition
			(setq view (or (slot-value condition 'view) view))
			(with-presentation-type-decoded
			 (type-name parameters options) type
			 (return-from accept
			   (funcall-presentation-generic-function
			    handle-input-error
			    type-name parameters options 
			    object type error-stream view)))))))
    (with-presentation-type-decoded
     (type-name parameters options) type
     (multiple-value-bind (returned-object returned-type)
	 (funcall-presentation-generic-function accept type-name
						parameters options
						type stream view  
						:default default
						:default-type default-type)
       (values returned-object (or returned-type type))))))

;;; stream-accept, accept1!!!
(defun ACCEPT-FROM-STRING (type string
                                &key 
                                (view +textual-view+) 
                                default 
                                (default-type type) 
                                (start 0)
                                end
                                (error-stream *standard-output*)
                                &allow-other-keys)
  (when string
    (with-input-from-string (stream string :start start :end end)
      (accept type :stream stream :view view :default default
	      :default-type default-type :error-stream error-stream))))

;;; prompt-for-accept, prompt-for-accept-1!!! 

(defun ACCEPT-PRESENT-DEFAULT (type stream view default
				    default-supplied-p present-p
				    query-identifier &key 
                                    (prompt t)
                                    (prompt-mode ':normal)
                                    (display-default prompt)
                                    insert-default
                                    (active-p t)
                                    default-string)
  (with-presentation-type-decoded
   (type-name parameters options) type
   (multiple-value-bind (returned-object returned-type)
       (funcall-presentation-generic-function
	accept-present-default type-name parameters options type
	stream view default default-supplied-p present-p
	query-identifier :prompt prompt :prompt-mode prompt-mode
	:display-default display-default :insert-default insert-default
	:active-p active-p :default-string default-string)
     (values returned-object (or returned-type type)))))

  

;;; When called inside accept, signals that accept should return the
;;; results of a handle-input-error method.
(defun handle-input-error (object type &rest args &key stream view)
  (check-type type presentation-type-specifier)
  (signal 'handle-input-error :object object :type type :stream stream :view view :args args)
  (error 'handle-input-error :object object :type type :stream stream :view view :args args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 23.6 VIEWS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; VIEWP!!!

(define-presentation-view-class VIEW
  :description "The basic view class.")

(define-presentation-view-class TEXTUAL-VIEW
  :inherits-from (view))

(define-presentation-view-class TEXTUAL-MENU-VIEW
  :inherits-from (textual-view))

(define-presentation-view-class TEXTUAL-DIALOG-VIEW
  :inherits-from (textual-view))

(define-presentation-view +TEXTUAL-VIEW+ textual-view)
(define-presentation-view +TEXTUAL-MENU-VIEW+ textual-menu-view)
(define-presentation-view +TEXTUAL-DIALOG-VIEW+ textual-dialog-view)


;;; Should these be accessors on extended-input/output-stream???
(defvar *stream-default-views* (make-hash-table))
(defmethod STREAM-DEFAULT-VIEW (stream)
  (gethash stream *stream-default-views*))
(defmethod (SETF STREAM-DEFAULT-VIEW) (view stream)
  (setf (gethash stream *stream-default-views*) view))

(progn
  (setf (stream-default-view *standard-input*) +textual-view+)
  (setf (stream-default-view *standard-output*) +textual-view+))

