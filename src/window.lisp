(in-package :roto-mortar)

(defun get-time ()
  (/ (get-internal-real-time) internal-time-units-per-second))

(defclass roto-mortar-window (glut:window)
  ((screen    :initform (make-instance 'main-menu-screen))
   (last-tick :initform (get-time))
   (keys-down :initform 0))
  (:default-initargs :width 800
                     :height 600
		     :title "Roto Mortar — © 2010 nklein software"
		     :mode '(:double :rgb :depth :stencil)))

(defmethod glut:display-window :before ((w roto-mortar-window))
  (gl:clear-color 0 0 0 0)
  (gl:cull-face :back)
  (gl:front-face :cw)
  (gl:shade-model :smooth)
  (gl:blend-func :one :zero)
  (gl:enable :blend :texture-2d)
  (gl:enable :depth-test)
  (gl:depth-func :lequal)
  (with-slots (screen last-tick) w
    (setf last-tick (get-time))
    (load-screen screen))
  (glut:enable-tick w 10))

(defgeneric possible-new-screen (w screen-or-nil)
  (:method ((w roto-mortar-window) (screen (eql nil)))
    (declare (ignore w screen)))
  (:method ((w roto-mortar-window) (new-screen screen))
    (with-slots (screen) w
      (unload-screen screen)
      (setf screen new-screen)
      (load-screen screen))))

(defmethod glut:tick ((w roto-mortar-window))
  (with-slots (screen last-tick) w
    (let ((elapsed (- (get-time) last-tick)))
      (incf last-tick elapsed)
      (possible-new-screen w (update-screen screen elapsed))))
  (glut:post-redisplay))

(defmethod glut:display ((w roto-mortar-window))
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (with-slots (screen) w
    (draw-screen screen))
  (glut:swap-buffers)
  (gl:flush))

(defmethod glut:reshape ((w roto-mortar-window) width height)
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (glu:perspective 48.1 (/ width height) 0.1 100)
  (glu:look-at 42.707 -30.273 10.528  17.638 -13.781 3.567  0.0 0.0 1.0)
  (gl:matrix-mode :modelview))

(defmethod glut:mouse ((w roto-mortar-window) button state x y)
  (declare (ignore xx yy))
  (with-slots (screen) w
    (case state
      (:down (possible-new-screen w (mouse-down screen button)))
      (:up   (possible-new-screen w (mouse-up   screen button))))))

(defmethod glut:keyboard ((w roto-mortar-window) key xx yy)
  (declare (ignore xx yy))
  (with-slots (screen) w
    (cond
      ((eql key #\q) (unload-screen screen))
                     (glut:close w))
    (t (possible-new-screen w (key-down screen key)))))

(defmethod glut:keyboard-up ((w roto-mortar-window) key xx yy)
  (declare (ignore xx yy))
  (with-slots (screen) w
    (cond
      ((not (eql key #\q)) (possible-new-screen w (key-up screen key))))))
