(in-package :roto-mortar)

(defclass angle-indicator-item (drawn-item)
  ((geometry :initform nil)
   (angle :initarg :initial-angle)
   (position :initarg :position)
   (angular-velocity :initarg :angular-velocity))
  (:default-initargs :initial-angle -90.0
                     :angular-velocity -30.0))

(defmethod update-item :after ((drawn-item angle-indicator-item) elapsed)
  (with-slots (visible angle angular-velocity) drawn-item
    (when visible
      (incf angle (* angular-velocity elapsed)))))

(defmethod draw ((drawn-item angle-indicator-item) screen)
  (with-slots (geometry visible position angle) drawn-item
    (when visible
      (with-slots (geometry) geometry
	(with-slots (x3d:translation x3d:scale x3d:rotation) geometry
	  (gl:with-pushed-matrix
	    (apply #'gl:translate x3d:translation)
	    (apply #'gl:scale x3d:scale)
	    (apply #'gl:rotate x3d:rotation)
	    (apply #'gl:translate position)
	    (gl:rotate angle 0.0 0.0 1.0)
	    (draw-as-shadow-volume geometry screen)))))))

(defclass distance-indicator-item (drawn-item)
  ((geometry :initform nil)
   (scale :initarg :initial-scale)
   (angle :initarg :initial-angle)
   (angular-velocity :initarg :angular-velocity)
   (position :initarg :position)
   (is-aiming :initform nil))
  (:default-initargs :initial-angle (* pi 30/180)
		     :angular-velocity 20.0))

(defun calculate-scale-for-angle (item)
  (with-slots (angle position) item
    (let ((vv +muzzle-velocity+)
	  (gg +gravity-constant+)
	  (zz (third position))
	  (cp (cos angle))
	  (sp (sin angle))
	  (fudge (/ 53.32)))
      (* vv cp (/ gg) fudge (+ (* vv sp)
			       (sqrt (+ (* vv vv sp sp) (* 2 gg zz))))))))

(defmethod initialize-instance :after ((drawn-item distance-indicator-item) &key)
  (with-slots (scale) drawn-item
    (setf scale (calculate-scale-for-angle drawn-item))))

(defmethod update-item :after ((drawn-item distance-indicator-item) elapsed)
  (with-slots (visible is-aiming angle scale angular-velocity) drawn-item
    (when (and is-aiming visible)
      (incf angle (* elapsed angular-velocity pi 1/180))
      (when (or (and (minusp angular-velocity)
		     (< angle (* pi 45/180)))
		(and (plusp angular-velocity)
		     (< (* pi 85/180) angle)))
	(setf angular-velocity (- angular-velocity)))
      (setf scale (calculate-scale-for-angle drawn-item)))))

(defmethod draw ((drawn-item distance-indicator-item) screen)
  (with-slots (geometry visible position scale) drawn-item
    (when visible
      (with-slots (geometry) geometry
	(with-slots (x3d:translation x3d:scale x3d:rotation) geometry
	  (gl:with-pushed-matrix
	    (apply #'gl:translate x3d:translation)
	    (apply #'gl:scale x3d:scale)
	    (apply #'gl:rotate x3d:rotation)
	    (apply #'gl:translate position)
	    (gl:scale scale scale 1.0)
	    (draw-as-shadow-volume geometry screen)))))))

(defclass main-menu-screen (screen)
  ((angle-1 :initform (make-instance 'angle-indicator-item
				     :angular-velocity -30.0
				     :position '(3.657 -27.515 2.0)))
   (angle-2 :initform (make-instance 'angle-indicator-item
				     :visible nil
				     :angular-velocity 37.0
				     :position '(31.102 -6.968 2.0)))
   (distance-1 :initform (make-instance 'distance-indicator-item
				        :angular-velocity 20.0
					:position '(3.657 -27.515 2.0)))
   (distance-2 :initform (make-instance 'distance-indicator-item
					:visible nil
					:angular-velocity 27.0
					:position '(31.102 -6.968 2.0)))
   (ground-geometry :initform (load-x3d-item #P"scene-baked.x3d"))
   (missile-geometry :initform (load-x3d-item #P"cube.x3d"))
   (alien-geometry :initform (load-x3d-item #P"torus.x3d"))
   (aliens-eliminated :initform 0)
   (aliens-got-through :initform 0)
   (warned :initform 0)
   (bail :initform nil)))

(defmethod alien-got-through ((screen main-menu-screen))
  (with-slots (aliens-got-through aliens-eliminated overlays bail) screen
    (incf aliens-got-through)
    (when (= 5 aliens-got-through)
      (push (make-message-overlay #P"billy-bob.png"
				  :lines (list "It's time to get out of here."
					       "We're being overrun."
					       (format nil "You got ~S aliens"
						       aliens-eliminated))
				  :timeout 8
				  :timeout-callback #'(lambda (ii ss)
							(declare (ignore ii))
							(with-slots (bail) ss
							  (setf bail t))))
	    overlays))))

(defmethod alien-eliminated ((screen main-menu-screen))
  (with-slots (aliens-eliminated overlays) screen
    (incf aliens-eliminated)
    (when (= 0 (mod aliens-eliminated 4))
      (push (make-message-overlay #P"billy-bob.png"
				  :lines (list "You're doing great."
					       (format nil "You got ~S aliens"
						       aliens-eliminated))
				  :timeout 4)
	    overlays))))

(defvar +warning-lines+ '(
"The KPs fried most of our weapons controls."
"   I got one of the mortars spinning."
"   You can fire it with that there mouse button."
))

(defvar +warning-lines-2+ '(
"'Course, that button also does yer gun elevation."
"   Hold down the mouse button to set elevation"
"   then release it to fire."
))

(defmethod load-screen progn ((screen main-menu-screen))
  (let ((angle-geom (load-x3d-shadow-volume #P"angle.x3d"))
	(dist-geom (load-x3d-shadow-volume #P"distance.x3d"))
	(base  (load-x3d-item #P"base-baked.x3d")))
    (with-slots (items overlays distance-1 distance-2 angle-1 angle-2
		       ground-geometry) screen
      (with-slots (geometry) angle-1    (setf geometry angle-geom))
      (with-slots (geometry) angle-2    (setf geometry angle-geom))
      (with-slots (geometry) distance-1	(setf geometry dist-geom))
      (with-slots (geometry) distance-2	(setf geometry dist-geom))
      (push distance-1 items)
      (push distance-2 items)
      (push angle-1 items)
      (push angle-2 items)
      (push base items)
      (push ground-geometry items))))

(defun find-closest-zz (xx yy screen)
  (with-slots (ground-geometry) screen
    (with-slots (geometry) ground-geometry
      (with-slots (x3d:meshes x3d:scale x3d:translation) geometry
	(with-slots (x3d:original-coordinates) (first x3d:meshes)
	  (let ((sx (first x3d:scale))
		(sy (second x3d:scale))
		(sz (third x3d:scale))
		(ox (first x3d:translation))
		(oy (second x3d:translation))
		(oz (third x3d:translation)))
	    (labels ((dist (px py)
		       (let ((dx (- xx (+ (* sx px) ox)))
			     (dy (- yy (+ (* sy py) oy))))
			 (+ (* dx dx) (* dy dy))))
		     (find-it (coords &optional (best-dist 10000.0)
				                (best-zz 0.0))
		       (cond
			 ((null coords) best-zz)
			 (t (let* ((cc (first coords))
				   (dd (dist (first cc) (second cc))))
			      (if (< dd best-dist)
				  (find-it (rest coords) dd
					   (+ (* sz (third cc)) oz))
				  (find-it (rest coords)
					   best-dist best-zz)))))))
	      (find-it x3d:original-coordinates))))))))

(defun drop-an-alien (screen)
  (let* ((aa (random pi))
	 (rr (- (random 50.0) 25.0))
	 (xx (* rr (cos aa)))
	 (yy (* rr (sin aa)))
	 (zz (find-closest-zz xx yy screen)))
    (with-slots (alien-geometry items) screen
      (push (make-instance 'alien
			   :screen screen
			   :geometry alien-geometry
			   :initial-position (list xx yy zz))
	    items))))

(defun fix-a-mortar (screen)
  (let ((broken nil))
    (with-slots (angle-1 distance-1 angle-2 distance-2) screen
      (with-slots (visible) angle-1
	(unless visible
	  (push (list angle-1 distance-1 "west") broken)))
      (with-slots (visible) angle-2
	(unless visible
	  (push (list angle-2 distance-2 "north") broken))))
    (when broken
      (let* ((mortar (nth (random (length broken)) broken))
	     (angle (first mortar))
	     (distance (second mortar))
	     (line (format nil "I just fixed the ~A mortar.~%"
			   (third mortar))))
	(with-slots (visible) angle
	  (setf visible t))
	(with-slots (visible) distance
	  (setf visible t))
	(with-slots (overlays) screen
	  (push (make-message-overlay #P"billy-bob.png" :lines (list line))
		overlays))))))

(defun break-a-mortar (screen)
  (with-slots (angle-1 distance-1 angle-2 distance-2) screen
    (with-slots ((visible-1 visible)) angle-1
      (with-slots ((visible-2 visible)) angle-2
	(when (and visible-1 visible-2)
	  (let ((mortar (case (random 2)
			  (0  (list angle-1 distance-1 "west"))
			  (1  (list angle-2 distance-2 "north")))))
	    (let ((angle (first mortar))
		  (distance (second mortar))
		  (line (format nil "The ~A mortar just failed again.~%"
				(third mortar))))
	      (with-slots (visible) angle
		(setf visible nil))
	      (with-slots (visible) distance
		(setf visible nil))
	      (with-slots (overlays) screen
		(push (make-message-overlay #P"billy-bob.png"
					    :lines (list line))
		      overlays)))))))))

(defvar +base-alien-drops-per-second+ (/ 1.0 8.0))
(defvar +mortar-fixes-per-second+ (/ 1.0 45.0))
(defvar +mortar-failures-per-second+ (/ 1.0 30.0))

(defmethod update-screen progn ((screen main-menu-screen) elapsed)
  (declare (ignore elapsed))
  (with-slots (warned elapsed-time overlays t1-angle bail) screen
    (when (and (= warned 0) (<= 1 elapsed-time))
      (push (make-message-overlay #P"billy-bob.png"
				  :timeout 14
				  :lines +warning-lines+)
	    overlays)
      (setf warned 1))
    (when (and (= warned 1) (<= 17 elapsed-time))
      (push (make-message-overlay #P"billy-bob.png"
				  :timeout 14
				  :lines +warning-lines-2+)
	    overlays)
      (setf warned 2))
    (when (< -20.0 elapsed-time)
      (let ((ll (* elapsed +base-alien-drops-per-second+)))
	(when (< (exp (- ll)) (random 1.0))
	  (drop-an-alien screen)))
      (let ((ll (* elapsed +mortar-fixes-per-second+)))
	(when (< (exp (- ll)) (random 1.0))
	  (fix-a-mortar screen)))
      (let ((ll (* elapsed +mortar-failures-per-second+)))
	(when (< (exp (- ll)) (random 1.0))
	  (break-a-mortar screen))))
    (when bail
      (make-instance 'start-screen))))

(defmethod unload-screen progn ((screen main-menu-screen))
  (with-slots (items) screen
    (setf items nil)))

(defmethod mouse-down progn ((screen main-menu-screen) button)
  (with-slots (distance-1 distance-2) screen
    (with-slots ((is-aiming-1 is-aiming)) distance-1
      (with-slots ((is-aiming-2 is-aiming)) distance-2
	(when (and (eql button :left-button) (not is-aiming-1))
	  (setf is-aiming-1 t
		is-aiming-2 t)))))
  nil)

(defmethod mouse-up progn ((screen main-menu-screen) button)
  (with-slots (angle-1 distance-1 angle-2 distance-2) screen
    (with-slots ((is-aiming-1 is-aiming) (visible-1 visible)) distance-1
      (with-slots ((is-aiming-2 is-aiming) (visible-2 visible)) distance-2
	(when (and (eql button :left-button) is-aiming-1)
	  (when visible-1 (start-missile screen angle-1 distance-1))
	  (when visible-2 (start-missile screen angle-2 distance-2))
	  (setf is-aiming-1 nil
		is-aiming-2 nil)))))
  nil)
