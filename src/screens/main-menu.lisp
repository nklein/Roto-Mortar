(in-package :roto-mortar)

(defclass angle-indicator-item (drawn-item)
  ((geometry :initform nil)
   (angle :initarg :initial-angle)
   (position :initarg :position)
   (angular-velocity :initarg :angular-velocity))
  (:default-initargs :position '(3.657 -27.515 0.0)
                               #+not '(31.102  -6.968 0.0)
                     :initial-angle 0.0
                     :angular-velocity 30.0))

(defmethod update-item :after ((drawn-item angle-indicator-item) elapsed)
  (with-slots (angle angular-velocity) drawn-item
    (incf angle (* angular-velocity elapsed))))

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
   (position :initarg :position))
  (:default-initargs :position '(3.657 -27.515 0.0)
                               #+not '(31.102  -6.968 0.0)
                     :initial-scale 1.0))

(defmethod update-item :after ((drawn-item distance-indicator-item) elapsed)
  (declare (ignore elapsed-time))
  (with-slots (scale elapsed-time) drawn-item
    (setf scale (+ 0.6 (* 0.4 (cos (* 10/180 elapsed-time pi)))))))

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
  ((angle :initform (make-instance 'angle-indicator-item))
   (distance :initform (make-instance 'distance-indicator-item))
   (warned :initform nil)))

(defvar +warning-lines+ '(
"The KPs fried most of our weapons controls."
"   I got one of the mortars spinning."
"   You can fire it with that there mouse button."
"   Of course, that also controls the gun elevation."
))

(defmethod load-screen progn ((screen main-menu-screen))
  (let ((angle-geom (load-x3d-shadow-volume #P"angle.x3d"))
	(dist-geom (load-x3d-shadow-volume #P"distance.x3d"))
	(scene (load-x3d-item #P"scene-baked.x3d"))
	(base  (load-x3d-item #P"base-baked.x3d")))
    (with-slots (items overlays distance angle) screen
      (with-slots (geometry) angle
	(setf geometry angle-geom))
      (with-slots (geometry) distance
	(setf geometry dist-geom))
      (push distance items)
      (push angle items)
      (push base items)
      (push scene items))))

(defmethod update-screen progn ((screen main-menu-screen) elapsed)
  (declare (ignore elapsed))
  (with-slots (warned elapsed-time overlays t1-angle) screen
    (when (and (not warned) (<= 1 elapsed-time))
      (push (make-message-overlay #P"billy-bob.png"
				  :timeout 14
				  :lines +warning-lines+)
	    overlays)
      (setf warned t)))
  nil)

(defmethod unload-screen progn ((screen main-menu-screen))
  (with-slots (items) screen
    (setf items nil)))
