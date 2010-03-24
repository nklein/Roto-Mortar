(in-package :roto-mortar)

(defclass drawn-item ()
  ((visible :initarg :visible)
   (is-shadow-volume  :initarg :is-shadow-volume)
   (geometry :initarg :geometry))
  (:default-initargs :visible t
                     :is-shadow-volume nil
                     :geometry nil))

(defgeneric draw (drawn-item screen))
(defgeneric draw-as-shadow-volume (drawn-item screen))

(defun expand-coordinates-from-indexes (mesh)
  (with-slots (x3d:coordinate-indexes x3d:texture-coordinate-indexes
               x3d:coordinates        x3d:texture-coordinates) mesh
    (labels ((expand (ccs iis)
	       (mapcar #'(lambda (ii) (nth ii ccs)) iis))
	     (expand-tex (ccs iis)
	       (mapcar #'(lambda (uv) (list (first uv) (- 1 (second uv))))
		       (expand ccs iis))))
      (setf x3d:coordinates
	    (mapcar #'(lambda (iis) (expand x3d:coordinates iis))
		    x3d:coordinate-indexes))
      (when (slot-boundp mesh 'x3d:texture-coordinates)
	(setf x3d:texture-coordinates
	      (mapcar #'(lambda (iis)
			  (expand-tex x3d:texture-coordinates iis))
		      x3d:texture-coordinate-indexes))))))

(defun load-x3d-item (filename &rest args)
  (let ((x3d (x3d:parse filename)))
    (when x3d
      (with-slots (x3d:shape-list) x3d
	(let ((item (first x3d:shape-list)))
	  (with-slots (x3d:meshes) item
	    (mapc #'expand-coordinates-from-indexes x3d:meshes))
	  (apply #'make-instance 'drawn-item :geometry item args))))))

(defun load-x3d-shadow-volume (filename &rest args)
  (apply #'load-x3d-item filename :is-shadow-volume t args))

(defmethod draw ((item drawn-item) screen)
  (with-slots (geometry is-shadow-volume) item
    (if is-shadow-volume
	(draw-as-shadow-volume geometry screen)
	(draw geometry screen))))

(defmethod draw-as-shadow-volume ((item drawn-item) screen)
  (with-slots (geometry) item
    (draw-as-shadow-volume geometry screen)))

(defmethod draw ((item x3d:x3d-geometry-object) screen)
  (with-slots (x3d:translation x3d:scale x3d:rotation
			       x3d:diffuse-color x3d:transparency
			       x3d:texture x3d:meshes) item
    (gl:with-pushed-matrix
      (gl:rotate 90.0 1.0 0.0 0.0)
      (apply #'gl:translate x3d:translation)
      (apply #'gl:scale x3d:scale)
      (apply #'gl:rotate x3d:rotation)
      (gl:with-pushed-attrib (:current-bit)
	(gl:color (first x3d:diffuse-color)
		  (second x3d:diffuse-color)
		  (third x3d:diffuse-color)
		  x3d:transparency)
	(when (slot-boundp item 'x3d:texture)
	  (gl:bind-texture :texture-2d x3d:texture))
	(mapc #'(lambda (mm) (draw mm screen)) x3d:meshes)))))

(defmethod draw ((item x3d:x3d-mesh) screen)
  (with-slots (x3d:coordinates x3d:texture-coordinates) item
    (labels ((emit-vert (vv vv-again)
	       (declare (ignore vv-again))
	       (gl:vertex (first vv) (second vv) (third vv)))
	     (emit-tex-vert (vv tt)
	       (gl:tex-coord (first tt) (second tt))
	       (gl:vertex (first vv) (second vv) (third vv))))
      (let ((func (if (slot-boundp item 'x3d:texture-coordinates)
		      #'emit-tex-vert
		      #'emit-vert))
	    (texs (if (slot-boundp item 'x3d:texture-coordinates)
		      x3d:texture-coordinates
		      x3d:coordinates)))
	  (mapc #'(lambda (vvs tts)
		    (case (length vvs)
		      (3 (gl:with-primitives :triangles
			   (mapc func vvs tts)))
		      (4 (gl:with-primitives :quads
			   (mapc func vvs tts)))
		      (t (gl:with-primitives :polygon
			   (mapc func vvs tts)))))
		x3d:coordinates texs)))))

(defmethod draw-as-shadow-volume ((item x3d:x3d-geometry-object) screen)
  (with-slots (x3d:translation x3d:scale x3d:rotation
			       x3d:diffuse-color x3d:transparency
			       x3d:texture x3d:meshes) item
    (gl:with-pushed-matrix
      (gl:rotate 90.0 1.0 0.0 0.0) 
      (apply #'gl:translate x3d:translation)
      (apply #'gl:scale x3d:scale)
      (apply #'gl:rotate x3d:rotation)
      (gl:translate  3.657 -27.515 0.0)
      #+not
      (gl:translate 31.102  -6.968 0.0)
      (gl:with-pushed-attrib (:current-bit :enable-bit
					   :color-buffer-bit
					   :depth-buffer-bit
					   :stencil-buffer-bit
					   :polygon-bit)
	(gl:color (first x3d:diffuse-color)
		  (second x3d:diffuse-color)
		  (third x3d:diffuse-color)
		  x3d:transparency)

	(gl:color-mask nil nil nil nil)
	(gl:enable :depth-test)
	(gl:depth-mask nil)
	(gl:disable :cull-face)

	(gl:enable :stencil-test)
	(gl:stencil-mask 1)
	(gl:clear-stencil 0)
	(gl:clear :stencil-buffer-bit)

	(gl:enable :polygon-offset-fill)
	(gl:polygon-offset 0.0 100.0)

	(gl:stencil-func :always #x01 #x01)
	(gl:stencil-op :keep :invert :keep)
	(gl:polygon-offset 0.0 150.0)
	(mapc #'(lambda (mm) (draw-as-shadow-volume mm screen)) x3d:meshes)

	(gl:enable :cull-face)
	(gl:color-mask t t t t)
	(gl:depth-mask t)
	(gl:disable :depth-test)
	(gl:stencil-func :equal #x01 #x01)
	(gl:polygon-offset 0.0 175.0)
	(mapc #'(lambda (mm) (draw-as-shadow-volume mm screen)) x3d:meshes)))))

(defmethod draw-as-shadow-volume ((item x3d:x3d-mesh) screen)
  (with-slots (x3d:coordinates x3d:texture-coordinates) item
    (labels ((emit-vert (vv)
	       (gl:vertex (first vv) (second vv) (third vv))))
      (mapc #'(lambda (vvs)
		(case (length vvs)
		  (3 (gl:with-primitives :triangles
		       (mapc #'emit-vert vvs)))
		  (4 (gl:with-primitives :quads
		       (mapc #'emit-vert vvs)))
		  (t (gl:with-primitives :polygon
		       (mapc #'emit-vert vvs)))))
	    x3d:coordinates))))
