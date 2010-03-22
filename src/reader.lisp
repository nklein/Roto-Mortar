(in-package :x3d)

;;; =================================================================
;;; boiler-plate cxml handler stuff
;;; =================================================================
(defclass sax-handler (sax:default-handler)
  ((root-path :initform nil :initarg :root-path)
   (root-type :initform nil :initarg :root-type)
   (items :initform nil)
   (buffers :initform nil)
   (paths :initform nil)
   (last-item :initform nil)))

(defmethod initialize-instance :after ((handler sax-handler) &key)
  (with-slots (root-path) handler
    (setf root-path (intern root-path :keyword))))

(defgeneric start (handler item path)
  (:documentation "This is called at the opening of each xml tag")
  (:method-combination progn)
  (:method progn (handler item path)
           (declare (ignore handler item path))))

(defgeneric data (handler item path value)
  (:documentation "This is called with attributes and text contents of tags")
  (:method-combination progn)
  (:method progn (handler item path value)
           (declare (ignore handler item path value))))

(defgeneric end (handler item path)
  (:documentation "This is called at the closing of each xml tag")
  (:method-combination progn)
  (:method progn (handler item path)
           (declare (ignore handler item path))))

(defun add-to-path (initial separator new)
  (intern (concatenate 'string (when initial (symbol-name initial))
                               separator
                               new)
          :keyword))

(defun push-path (handler separator new)
  (with-slots (paths) handler
    (push (add-to-path (first paths) separator new) paths)))

(defmethod sax:start-element ((handler sax-handler)
                              namespace-uri
                              local-name
                              qname
                              attributes)
  (declare (ignore namespace-uri qname))
  (with-slots (root-path root-type paths items buffers) handler
    (push-path handler "/" local-name)
    (when (and (null items) (eql (first paths) root-path))
      (push nil paths)
      (push (make-instance root-type) items))
    (push nil buffers)
    (start handler (first items) (first paths))
    (dolist (attr attributes)
      (with-accessors ((attr-name sax:attribute-local-name)
                       (attr-value sax:attribute-value)) attr
        (data handler (first items)
                      (add-to-path (first paths) "@" attr-name)
                      attr-value)))))

(defmethod sax:characters ((handler sax-handler) data)
  (with-slots (buffers) handler
    (push data (first buffers))))

(defmethod sax:end-element ((handler sax-handler)
                            namespace-uri
                            local-name
                            qname)
  (declare (ignore namespace-uri local-name qname))
  (with-slots (root-path items paths buffers last-item) handler
    (let ((text-contents (apply #'concatenate 'string
                                              (nreverse (pop buffers)))))
      (data handler (first items)
                    (add-to-path (first paths) "/" ".")
                    text-contents))
    (pop paths)
    (when (eql (first paths) root-path)
      (setf last-item (first items)))
    (end handler (second items) (first paths))))

(defmethod sax:end-document ((handler sax-handler))
  (with-slots (last-item) handler
    last-item))

(defun parse (source &key (root-path "/X3D")
                          (root-type 'x3d-x3d))

  (let ((handler (make-instance 'sax-handler :root-path root-path
                                             :root-type root-type)))
    (typecase source
      (pathname (with-open-file (stream (merge-pathnames source
							 roto-mortar:*data-directory*)
					:element-type 'unsigned-byte)
                  (cxml:parse-stream stream handler)))
      (t (cxml:parse source handler)))))

;;; =================================================================
;;; x3d-property struct
;;; =================================================================
(defmethod data progn ((handler sax-handler) (item x3d-property) path _value)
  (with-slots (name value) item
    (case path
      (:|@name| (setf name _value))
      (:|@content| (setf value _value)))))

;;; =================================================================
;;; x3d-mesh struct
;;; =================================================================
(defun parse-coordinate-indexes (string &optional (position 0)
				                  indexes)
  (let ((eof (gensym "EOF-")))
    (multiple-value-bind (object position)
	(read-from-string string nil eof :start position)
      (cond
	((eql object eof) (nreverse (rest indexes)))
	((and (typep object 'integer)
	      (< object 0))     (parse-coordinate-indexes
				        string
					position
					(cons nil
					      (cons (nreverse (first indexes))
						    (rest indexes)))))
	((and (typep object 'integer)
	      (<= 0 object))    (parse-coordinate-indexes
				        string
					position
					(cons (cons object (first indexes))
					      (rest indexes))))
	(t (parse-coordinate-indexes string position indexes))))))

(defun parse-coordinates (string dimensions &optional (position 0)
				                      coordinates)
  (let ((eof (gensym "EOF-")))
    (multiple-value-bind (object position)
	(read-from-string string nil eof :start position)
      (cond
	((eql object eof) (nreverse (rest coordinates)))
	((typep object 'real)
	      (parse-coordinates string dimensions position
				 (if (= (1- dimensions)
					(length (first coordinates)))
				     (cons nil
					   (cons (nreverse
						    (cons object
							  (first coordinates)))
						 (rest coordinates)))
				     (cons (cons object (first coordinates))
					   (rest coordinates)))))
	(t (parse-coordinates string dimensions position coordinates))))))

(defmethod data progn ((handler sax-handler) (item x3d-mesh) path value)
  (with-slots (texture-coordinate-indexes coordinate-indexes coordinates texture-coordinates) item
    (case path
      (:|@texCoordIndex| (setf texture-coordinate-indexes
			       (parse-coordinate-indexes (remove #\, value))))
      (:|@coordIndex| (setf coordinate-indexes
			    (parse-coordinate-indexes (remove #\, value))))
      (:|/Coordinate@point| (setf coordinates
				  (parse-coordinates (remove #\, value) 3)))
      (:|/TextureCoordinate@point|
	                    (setf texture-coordinates
				  (parse-coordinates (remove #\, value) 2))))))

;;; =================================================================
;;; x3d-geometry-object struct
;;; =================================================================
(defun read-png (basename)
  (with-open-file (input (merge-pathnames basename
					  roto-mortar:*data-directory*)
			 :element-type '(unsigned-byte 8))
    (let* ((image (png:decode input))
	   (texture (first (gl:gen-textures 1)))
	   (width (png:image-width image))
	   (height (png:image-height image))
	   (format (case (png:image-channels image)
		     (1 :luminance)
		     (2 :luminance-alpha)
		     (3 :rgb)
		     (4 :rgba)))
	   (type (case (png:image-bit-depth image)
		   (8 :unsigned-byte)
		   (16 :unsigned-short))))
      (gl:bind-texture :texture-2d texture)
      (gl:tex-parameter :texture-2d :texture-min-filter :linear)
      (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
      (gl:tex-image-2d :texture-2d 0 format width height 0 format type
		       (make-array (list (array-total-size image))
				   :displaced-to image
				   :element-type (array-element-type image)))
      texture)))

(defvar *known-png-files* (make-hash-table :test #'equalp))
(defun get-png-as-texture-id (basename)
  (let ((id (gethash basename *known-png-files*)))
    (unless id
      (setf id (read-png basename))
      (setf (gethash basename *known-png-files*) id))
    id))

(defun read-real-array (string &optional (position 0) values)
  (let ((eof (gensym "EOF-")))
    (multiple-value-bind (object position)
	(read-from-string string nil eof :start position)
      (cond
	((eql object eof) (nreverse values))
	((typep object 'real) (read-real-array string position
					       (cons object values)))
	(t (read-real-array string position values))))))

(defun read-rotation (string)
  (let ((raw (nreverse (read-real-array string))))
    (cons (first raw)
	  (nreverse (rest raw)))))

(defmethod data progn ((handler sax-handler) (item x3d-geometry-object) path value)
  (with-slots (translation scale rotation diffuse-color specular-color shininess transparency texture) item
    (case path
      (:|@translation| (setf translation (read-real-array value)))
      (:|@scale| (setf scale (read-real-array value)))
      (:|@rotation| (setf rotation (read-rotation value)))
      (:|/Shape/Appearance/Material@diffuseColor| (setf diffuse-color (read-real-array value)))
      (:|/Shape/Appearance/Material@specularColor| (setf specular-color (read-real-array value)))
      (:|/Shape/Appearance/Material@shininess| (setf shininess (read-from-string value)))
      (:|/Shape/Appearance/Material@transparency| (setf transparency (read-from-string value)))
      (:|/Shape/Appearance/ImageTexture@url| (setf texture (get-png-as-texture-id value))))))

(defmethod start progn ((handler sax-handler) (item x3d-geometry-object) path)
  (declare (ignore item))
  (with-slots (paths items) handler
    (case path
      (:|/Shape/IndexedFaceSet|
             (push nil paths)
             (push (make-instance 'x3d-mesh) items)))))

(defmethod end progn ((handler sax-handler) (item x3d-geometry-object) path)
  (with-slots (paths items) handler
    (case path
      (:|/Shape/IndexedFaceSet|
             (pop paths)
             (with-slots (meshes) item
               (setf meshes
                     (append meshes (list (pop items)))))))))

;;; =================================================================
;;; x3d-x3d struct
;;; =================================================================
(defmethod data progn ((handler sax-handler) (item x3d-x3d) path value)
  (with-slots (ground-color sky-color) item
    (case path
      (:|/Scene/Background@groundColor| (setf ground-color value))
      (:|/Scene/Background@skyColor| (setf sky-color value)))))

(defmethod start progn ((handler sax-handler) (item x3d-x3d) path)
  (declare (ignore item))
  (with-slots (paths items) handler
    (case path
      (:|/head/meta|
             (push nil paths)
             (push (make-instance 'x3d-property) items))
      (:|/Scene/Transform|
             (push nil paths)
             (push (make-instance 'x3d-geometry-object) items)))))

(defmethod end progn ((handler sax-handler) (item x3d-x3d) path)
  (with-slots (paths items) handler
    (case path
      (:|/head/meta|
             (pop paths)
             (with-slots (meta-properties) item
               (setf meta-properties
                     (append meta-properties (list (pop items))))))
      (:|/Scene/Transform|
             (pop paths)
             (with-slots (shape-list) item
               (setf shape-list
                     (append shape-list (list (pop items)))))))))

