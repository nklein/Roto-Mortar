(in-package :roto-mortar)

(defclass roto-mortar-window (glut:window)
  ((screen    :initarg :screen)
   (last-tick :initform (get-time))
   (keys-down :initform 0)
   (projection-mode :initarg :projection-mode))
  (:default-initargs :width 800
                     :height 600
		     :title "Roto Mortar — © 2010 nklein software"
		     :mode '(:double :rgb :depth :stencil)
		     :screen (make-instance 'start-screen)
		     :projection-mode :perspective))

(defun reset-projection (w width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (with-slots (projection-mode) w
    (case projection-mode
      (:perspective  (glu:perspective 48.1 (/ width height) 0.1 100)
		     (glu:look-at 42.707 -30.273 10.528
				  17.638 -13.781  3.567
				   0.000   0.000  1.000)
		     (gl:matrix-mode :modelview)
		     (gl:load-identity)
		     (gl:rotate 90.0 1.0 0.0 0.0)
		     (gl:enable :depth-test))
      (:orthographic (let ((offset (/ (- 800 (* 600 width (/ height))) 2)))
		       (gl:ortho offset (- 800 offset) 0 600 -400 400))
		     (gl:matrix-mode :modelview)
		     (gl:load-identity)
		     (gl:disable :depth-test)))))

(defun perspective-mode (window)
  (with-slots (projection-mode) window
    (unless (eql projection-mode :perspective)
      (setf projection-mode :perspective)
      (with-slots (glut:width glut:height) window
	(reset-projection window glut:width glut:height)))))

(defun orthographic-mode (window)
  (with-slots (projection-mode) window
    (unless (eql projection-mode :orthographic)
      (setf projection-mode :orthographic)
      (with-slots (glut:width glut:height) window
	(reset-projection window glut:width glut:height)))))
