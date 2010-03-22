(asdf:defsystem #:roto-mortar
  :depends-on (#:cxml #:png #:cl-opengl #:cl-glu #:cl-glut)
  :components ((:module "src"
		:serial t
		:components ((:file "package")
		             (:file "globals")
			     (:file "types")
			     (:file "reader")
			     (:file "window")
			     (:file "main")))))
