(asdf:defsystem #:roto-mortar
  :depends-on (#:cxml #:png #:cl-opengl #:cl-glu #:cl-glut #:zpb-ttf)
  :components ((:module "src"
		:components ((:file "package")
		             (:file "globals" :depends-on ("package"))
			     (:file "generics" :depends-on ("package"))
			     (:file "window-class" :depends-on ("package"))
			     (:file "types" :depends-on ("package"))
			     (:file "reader" :depends-on ("globals"
							  "types"
							  "package"))
			     (:file "font" :depends-on ("package"
			                                "globals"))
			     (:file "drawn-item" :depends-on ("package"
							      "globals"
							      "generics"
							      "types"
			                                      "reader"))
			     (:file "screen" :depends-on ("window-class"
							  "generics"
							  "drawn-item"
							  "package"))
			     (:file "window" :depends-on ("window-class"
							  "screen"
							  "package"))
			     (:module "screens"
			      :depends-on ("window-class"
					   "screen"
					   "font"
					   "package")
			      :components ((:file "main-menu")
					   (:file "start"
					          :depends-on ("main-menu"))))
			     (:file "main" :depends-on ("screens"
							"window"
							"package"))))))
