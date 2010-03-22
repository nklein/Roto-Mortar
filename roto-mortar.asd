(asdf:defsystem #:roto-mortar
  :depends-on (#:cxml #:png)
  :components ((:module "src"
		:serial t
		:components ((:file "package")
			     (:file "types")
			     (:file "reader")))))
