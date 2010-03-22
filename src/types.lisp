(defpackage :x3d
  (:use :common-lisp)
  (:export #:x3d-property
             #:name
             #:x3d-name
             #:value
             #:x3d-value
           #:x3d-mesh
             #:texture-coordinate-indexes
             #:x3d-texture-coordinate-indexes
             #:coordinate-indexes
             #:x3d-coordinate-indexes
             #:coordinates
             #:x3d-coordinates
             #:texture-coordinates
             #:x3d-texture-coordinates
           #:x3d-geometry-object
             #:translation
             #:x3d-translation
             #:scale
             #:x3d-scale
             #:rotation
             #:x3d-rotation
             #:diffuse-color
             #:x3d-diffuse-color
             #:specular-color
             #:x3d-specular-color
             #:shininess
             #:x3d-shininess
             #:transparency
             #:x3d-transparency
             #:texture
             #:x3d-texture
             #:meshes
             #:x3d-meshes
           #:x3d-x3d
             #:meta-properties
             #:x3d-meta-properties
             #:ground-color
             #:x3d-ground-color
             #:sky-color
             #:x3d-sky-color
             #:shape-list
             #:x3d-shape-list
           #:parse))

(in-package :x3d)

(defclass x3d-property ()
  ((name :initarg :name :accessor x3d-name :type string)
   (value :initarg :value :accessor x3d-value :type string)))

(defclass x3d-mesh ()
  ((texture-coordinate-indexes :initarg :texture-coordinate-indexes :accessor x3d-texture-coordinate-indexes :type list)
   (coordinate-indexes :initarg :coordinate-indexes :accessor x3d-coordinate-indexes :type list)
   (coordinates :initarg :coordinates :accessor x3d-coordinates :type list)
   (texture-coordinates :initarg :texture-coordinates :accessor x3d-texture-coordinates :type list)))

(defclass x3d-geometry-object ()
  ((translation :initarg :translation :accessor x3d-translation :type string)
   (scale :initarg :scale :accessor x3d-scale :type string)
   (rotation :initarg :rotation :accessor x3d-rotation :type string)
   (diffuse-color :initarg :diffuse-color :accessor x3d-diffuse-color :type string)
   (specular-color :initarg :specular-color :accessor x3d-specular-color :type string)
   (shininess :initarg :shininess :accessor x3d-shininess :type real)
   (transparency :initarg :transparency :accessor x3d-transparency :type real)
   (texture :initarg :texture :accessor x3d-texture :type png:image)
   (meshes :initarg :meshes :accessor x3d-meshes :type list :initform nil)))

(defclass x3d-x3d ()
  ((meta-properties :initarg :meta-properties :accessor x3d-meta-properties :type list :initform nil)
   (ground-color :initarg :ground-color :accessor x3d-ground-color :type string)
   (sky-color :initarg :sky-color :accessor x3d-sky-color :type string)
   (shape-list :initarg :shape-list :accessor x3d-shape-list :type list :initform nil)))
