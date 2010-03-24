(in-package :roto-mortar)

(defclass main-menu-screen (screen)
  ())

(defmethod load-screen progn ((screen main-menu-screen))
  (let ((scene (load-x3d-item #P"scene.x3d"))
	(base  (load-x3d-item #P"base.x3d"))
	(angle (load-x3d-shadow-volume #P"angle.x3d"))
	(dist  (load-x3d-shadow-volume #P"distance.x3d")))
    (with-slots (items) screen
      (push dist items)
      (push angle items)
      (push base items)
      (push scene items))))

(defmethod unload-screen progn ((screen main-menu-screen))
  (with-slots (items) screen
    (setf items nil)))
