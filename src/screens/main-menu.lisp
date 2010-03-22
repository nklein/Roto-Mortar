(in-package :roto-mortar)

(defclass main-menu-screen (screen)
  ())

(defmethod load-screen progn ((screen main-menu-screen))
  (let ((cube (load-x3d-item #P"cube.x3d")))
    (when cube
      (with-slots (items) screen
	(push cube items)))))

(defmethod unload-screen progn ((screen main-menu-screen))
  (with-slots (items) screen
    (setf items nil)))
