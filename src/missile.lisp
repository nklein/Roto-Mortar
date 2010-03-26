(in-package :roto-mortar)

(defvar +muzzle-velocity+ 20.0)
(defvar +gravity-constant+ 9.8)

(defclass missile (drawn-item)
  ((screen :initarg :screen)
   (geometry :initarg :geometry)
   (initial-position :initarg :initial-position)
   (position)
   (x-velocity :initarg :x-velocity)
   (y-velocity :initarg :y-velocity)
   (z-velocity :initarg :initial-z-velocity)))

(defmethod initialize-instance :after ((drawn-item missile) &key)
  (with-slots (position initial-position) drawn-item
    (setf position (copy-seq initial-position))))

(defmethod update-item :after ((item missile) elapsed)
  (with-slots (initial-position position elapsed-time screen
	       x-velocity y-velocity z-velocity) item
    (let ((gg +gravity-constant+))
      (setf (first position)  (+ (* elapsed-time x-velocity)
				 (first initial-position))
	    (second position) (+ (* elapsed-time y-velocity)
				 (second initial-position))
	    (third position)  (+ (* elapsed-time elapsed-time gg -0.5)
				 (* elapsed-time z-velocity)
				 (third initial-position)))
      (when (minusp (third position))
	(with-slots (items) screen
	  (setf items (remove item items)))))))

(defmethod draw ((drawn-item missile) screen)
  (with-slots (geometry visible position) drawn-item
    (when visible
      (with-slots (geometry) geometry
	(with-slots (x3d:translation x3d:scale x3d:rotation) geometry
	  (gl:with-pushed-matrix
	    (apply #'gl:translate x3d:translation)
	    (apply #'gl:scale x3d:scale)
	    (apply #'gl:rotate x3d:rotation)
	    (apply #'gl:translate position)
	    (draw geometry screen)))))))

(defun start-missile (screen angle-indicator distance-indicator)
  (with-slots (missile-geometry items) screen
    (with-slots ((theta angle) position) angle-indicator
      (with-slots ((phi angle)) distance-indicator
	(let ((cp (- (cos phi)))
	      (theta (* pi theta (/ 180.0)))
	      (vv +muzzle-velocity+))
	  (push (make-instance 'missile
			       :screen screen
			       :geometry missile-geometry
			       :initial-position (copy-seq position)
			       :x-velocity (* cp (cos theta) vv)
			       :y-velocity (* cp (sin theta) vv)
			       :initial-z-velocity (* (sin phi) vv))
		items)))))
  nil)
