(defgeneric MENU-CHOOSE
  (items &key associated-window printer presentation-type text-style
	 foreground background default-item label cache
	 unique-id id-test chache-value cache-test max-width max-height n-rows n-columns
	 x-spacing y-spacing initial-spacing row-wise
	 cell-align-x cell-align-y move-cursor scroll-bars pointer-documentation))

(defmethod MENU-CHOOSE (items &rest keys)
  (apply #'frame-manager-menu-choose
	 nil				;This is decidedly not correct!!!
	 keys))

(defgeneric FRAME-MANAGER-MENU-CHOOSE
  (frame-manager items &key associated-window printer
		 presentation-type text-style foreground background
		 default-item label cache unique-id id-test
		 chache-value cache-test max-width max-height n-rows
		 n-columns x-spacing y-spacing initial-spacing
		 row-wise cell-align-x cell-align-y move-cursor
		 scroll-bars pointer-documentation))

;;; elements of items: 
;;;   dispaly-object-and-value-object  OR
;;;   (disply-object . atomic-value-object)     OR
;;;   (display-object . options) where
;;;      options: :value :style Lpitems :documentation :active :type

(defmethod FRAME-MANAGER-MENU-CHOOSE (frame-manager items &key)
  
