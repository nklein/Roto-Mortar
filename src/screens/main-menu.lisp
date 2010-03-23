(in-package :roto-mortar)

(defclass main-menu-screen (screen)
  ())

(defmethod load-screen progn ((screen main-menu-screen))
  (let ((scene (load-x3d-item #P"scene.x3d"))
	(base  (load-x3d-item #P"base.x3d")))
    (with-slots (items) screen
      (push scene items)
      (push base items))))

(defmethod unload-screen progn ((screen main-menu-screen))
  (with-slots (items) screen
    (setf items nil)))
