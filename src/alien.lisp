(in-package :roto-mortar)

(defclass alien (drawn-item)
  ((screen :initarg :screen)
   (geometry :initarg :geometry)
   (position :initarg :initial-position)
   (x-velocity :initarg :x-velocity)
   (y-velocity :initarg :y-velocity)
   (last-checked-z :initform 0.0))
  (:default-initargs :x-velocity (+ (random 0.5) 0.2)
                     :y-velocity (- (+ (random 0.5) 0.2))))

(defmethod update-item :after ((item alien) elapsed)
  (with-slots (position screen x-velocity y-velocity last-checked-z) item
    (incf (first position) (* elapsed x-velocity))
    (incf (second position) (* elapsed y-velocity))
    (incf last-checked-z elapsed)
    (when (< 0.75 last-checked-z)
      (setf (third position) (find-closest-zz (first position)
					      (second position)
					      screen)
	    last-checked-z 0.0))
    (when (< (second position) -20.0 20.0 (first position))
      (alien-got-through screen)
      (with-slots (items) screen
	(setf items (remove item items)))))
  nil)

(defmethod draw ((drawn-item alien) screen)
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
