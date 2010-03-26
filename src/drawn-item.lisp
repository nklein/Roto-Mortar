(in-package :roto-mortar)

(defclass drawn-item ()
  ((visible :initarg :visible)
   (is-shadow-volume  :initarg :is-shadow-volume)
   (geometry :initarg :geometry)
   (elapsed-time :initarg :elapsed-time))
  (:default-initargs :visible t
                     :is-shadow-volume nil
                     :geometry nil
		     :elapsed-time 0.0))

(defclass drawn-image-item (drawn-item)
  ((texture :initarg :texture)
   (width :initarg :width)
   (height :initarg :height)))

(defclass message-overlay (drawn-item)
  ((icon :initarg :icon)
   (timeout :initarg :timeout)
   (timeout-callback :initarg :timeout-callback)
   (lines :initarg :lines)))

(defmethod update-item ((drawn-item drawn-item) elapsed)
  (with-slots (elapsed-time) drawn-item
    (incf elapsed-time elapsed)))

(defun expand-coordinates-from-indexes (mesh)
  (with-slots (x3d:coordinate-indexes x3d:texture-coordinate-indexes
               x3d:coordinates        x3d:texture-coordinates
	       x3d:original-coordinates) mesh
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

(defun load-image (filename width height)
  (make-instance 'drawn-image-item
		 :texture (x3d:get-png-as-texture-id filename)
		 :width width
		 :height height))

(defun make-message-overlay (icon-filename &key (timeout 5)
			                        timeout-callback
			                        lines)
  (make-instance 'message-overlay
		 :icon (load-image icon-filename 100 100)
		 :timeout timeout
		 :timeout-callback timeout-callback
		 :lines lines))

(defmethod draw ((item drawn-image-item) screen)
  (with-slots (visible texture width height) item
    (when visible
      (gl:with-pushed-attrib (:current-bit :texture-bit)
	(gl:color 1 1 1 0)
	(gl:bind-texture :texture-2d texture)
	(gl:with-primitives :quads
	  (gl:tex-coord 0 1) (gl:vertex 0 0)
	  (gl:tex-coord 0 0) (gl:vertex 0 height)
	  (gl:tex-coord 1 0) (gl:vertex width height)
	  (gl:tex-coord 1 1) (gl:vertex width 0))))))

(defmethod draw ((item drawn-item) screen)
  (with-slots (geometry is-shadow-volume) item
    (if is-shadow-volume
	(draw-as-shadow-volume geometry screen)
	(with-slots (x3d:translation x3d:scale x3d:rotation) geometry
	  (gl:with-pushed-matrix
	    (apply #'gl:translate x3d:translation)
	    (apply #'gl:scale x3d:scale)
	    (apply #'gl:rotate x3d:rotation)
	    (draw geometry screen))))))

(defmethod draw-as-shadow-volume ((item drawn-item) screen)
  (gl:with-pushed-attrib (:enable-bit)
    (gl:disable :texture-2d)
    (with-slots (geometry) item
      (with-slots (x3d:translation x3d:scale x3d:rotation) geometry
	(gl:with-pushed-matrix
	  (apply #'gl:translate x3d:translation)
	  (apply #'gl:scale x3d:scale)
	  (apply #'gl:rotate x3d:rotation)
	  (draw-as-shadow-volume geometry screen))))))

(defmethod draw ((item x3d:x3d-geometry-object) screen)
  (with-slots (x3d:translation x3d:scale x3d:rotation
			       x3d:diffuse-color x3d:transparency
			       x3d:texture x3d:meshes) item
    (gl:with-pushed-attrib (:current-bit)
      (gl:color (first x3d:diffuse-color)
		(second x3d:diffuse-color)
		(third x3d:diffuse-color)
		x3d:transparency)
      (when (slot-boundp item 'x3d:texture)
	(gl:bind-texture :texture-2d x3d:texture))
      (mapc #'(lambda (mm) (draw mm screen)) x3d:meshes))))

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
  (with-slots (x3d:diffuse-color x3d:transparency x3d:texture x3d:meshes) item
    (gl:with-pushed-attrib (:current-bit :enable-bit
					 :color-buffer-bit
					 :depth-buffer-bit
					 :stencil-buffer-bit
					 :polygon-bit)
      (gl:disable :texture-2d)
      (gl:color (first x3d:diffuse-color)
		(second x3d:diffuse-color)
		(third x3d:diffuse-color)
		x3d:transparency)

      (gl:color-mask nil nil nil nil)
      (gl:enable :depth-test)
      (gl:depth-mask nil)

      (gl:enable :stencil-test)
      (gl:stencil-mask #xff)
      (gl:clear-stencil 0)
      (gl:clear :stencil-buffer-bit)

      (gl:enable :cull-face)
      (gl:enable :polygon-offset-fill)

      (gl:cull-face :front)
      (gl:stencil-func :always #x00 #xff)
      (gl:stencil-op :keep :incr :keep)
      (gl:polygon-offset 0.0 100.0)
      (mapc #'(lambda (mm) (draw-as-shadow-volume mm screen)) x3d:meshes)

      (gl:cull-face :back)
      (gl:stencil-func :always #x00 #xff)
      (gl:stencil-op :keep :decr :keep)
      (gl:polygon-offset 0.0 125.0)
      (mapc #'(lambda (mm) (draw-as-shadow-volume mm screen)) x3d:meshes)

      (gl:disable :cull-face)
      (gl:color-mask t t t t)
      (gl:depth-mask t)
      (gl:disable :depth-test)
      (gl:stencil-func :notequal #x00 #xff)
      (gl:polygon-offset 0.0 150.0)
      (mapc #'(lambda (mm) (draw-as-shadow-volume mm screen)) x3d:meshes))))

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

(defmethod draw ((item message-overlay) screen)
  (with-slots (visible elapsed-time icon timeout timeout-callback lines) item
    (when visible
      (with-slots (overlays) screen
	(when (<= timeout elapsed-time)
	  (when timeout-callback
	    (funcall timeout-callback item screen))
	  (setf overlays (remove item overlays))))
      (gl:with-pushed-matrix
	(gl:translate 0 -100 0)
	(draw icon screen)
	(gl:translate 110 100 0)
	(gl:with-pushed-attrib (:current-bit)
	  (mapc #'(lambda (ll)
		    (gl:translate 0 -12 0)
		    (gl:color 0 0 0 0.8)
		    (gl:translate 2 -2 0)
		    (draw-string ll :size 22 :align :left)
		    (gl:translate  -2 2 0)
		    (gl:color 0.5 0.5 1 1)
		    (draw-string ll :size 22 :align :left)
		    (gl:translate 0 -14 0))
		lines)))
      (gl:translate 0 -110 0))))
