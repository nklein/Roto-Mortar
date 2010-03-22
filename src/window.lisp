(in-package :roto-mortar)

(defclass roto-mortar-window (glut:window)
  ((animating :initform nil)
   (keys-down :initform 0))
  (:default-initargs :width 800
                     :height 600
		     :title "Roto Mortar — © 2010 nklein software"
		     :mode '(:double :rgb :depth)))

(defmethod glut:display-window :before ((w roto-mortar-window))
  (gl:clear-color 0 0 0 0)
  (gl:cull-face :back)
  (gl:front-face :ccw)
  (gl:shade-model :smooth)
  (gl:blend-func :one :zero)
  (gl:enable :blend :texture-2d)
  (glut:enable-tick w 33)
  (with-slots (animating) w
    (setf animating t)))

(defmethod glut:tick ((w roto-mortar-window))
  (with-slots (animating) w
    (when animating
      (glut:post-redisplay))))

(defmethod glut:display ((w roto-mortar-window))
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  (gl:clear :color-buffer-bit)
  (glut:swap-buffers)
  (gl:flush))

(defmethod glut:reshape ((w roto-mortar-window) width height)
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:frustum -1 1 -1 1 1.5 500)
  (gl:matrix-mode :modelview))

(defmethod glut:keyboard ((w roto-mortar-window) key xx yy)
  (declare (ignore xx yy))
  (with-slots (keys-down) w
    (incf keys-down))
  (cond
    ((eql key #\q) (glut:close w))))

(defmethod glut:keyboard-up ((w roto-mortar-window) key xx yy)
  (declare (ignore xx yy))
  (with-slots (keys-down) w
    (decf keys-down)))
