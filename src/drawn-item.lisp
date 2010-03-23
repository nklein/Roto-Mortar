(in-package :roto-mortar)

(defclass drawn-item ()
  ((visible :initarg :visible))
  (:default-initargs :visible t))

(defgeneric draw (drawn-item screen))

(defun expand-coordinates-from-indexes (mesh)
  (with-slots (x3d:coordinate-indexes x3d:texture-coordinate-indexes
               x3d:coordinates        x3d:texture-coordinates) mesh
    (labels ((expand (ccs iis)
	       (mapcar #'(lambda (ii) (nth ii ccs)) iis)))
      (setf x3d:coordinates
	    (mapcar #'(lambda (iis) (expand x3d:coordinates iis))
		    x3d:coordinate-indexes))
      (setf x3d:texture-coordinates
	    (mapcar #'(lambda (iis) (expand x3d:texture-coordinates iis))
		    x3d:texture-coordinate-indexes)))))

(defun load-x3d-item (filename)
  (let ((x3d (x3d:parse filename)))
    (when x3d
      (with-slots (x3d:shape-list) x3d
	(let ((item (first x3d:shape-list)))
	  (with-slots (x3d:meshes) item
	    (mapc #'expand-coordinates-from-indexes x3d:meshes))
	  item)))))

(defmethod draw ((item x3d:x3d-geometry-object) screen)
  (with-slots (x3d:translation x3d:scale x3d:rotation
			       x3d:diffuse-color
			       x3d:texture x3d:meshes) item
    (gl:with-pushed-matrix
      (apply #'gl:translate x3d:translation)
      (apply #'gl:scale x3d:scale)
      (apply #'gl:rotate x3d:rotation)
      (with-slots (elapsed-time) screen
	(gl:rotate (* 20 elapsed-time) 1.0 1.0 1.0))
      (gl:with-pushed-attrib (:current-bit)
	(apply #'gl:color x3d:diffuse-color)
	(gl:bind-texture :texture-2d x3d:texture)
	(mapc #'(lambda (mm) (draw mm screen)) x3d:meshes)))))

(defmethod draw ((item x3d:x3d-mesh) screen)
  (with-slots (x3d:coordinates x3d:texture-coordinates) item
    (labels ((emit-vert (vv tt)
	       (gl:tex-coord (first tt) (second tt))
	       (gl:vertex (first vv) (second vv) (third vv))))
      (mapc #'(lambda (vvs tts)
		(case (length vvs)
		  (3 (gl:with-primitives :triangles
		       (mapc #'emit-vert vvs tts)))
		  (4 (gl:with-primitives :quads
		       (mapc #'emit-vert vvs tts)))
		  (t (gl:with-primitives :polygon
		       (mapc #'emit-vert vvs tts)))))
	    x3d:coordinates x3d:texture-coordinates))))
