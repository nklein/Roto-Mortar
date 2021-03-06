#!/usr/local/bin/sbcl --script

#+sbcl
(declaim (sb-ext:muffle-conditions t))

(load #P"./systems/asdf.lisp")
(setf asdf:*central-registry* (list *default-pathname-defaults*))

(dolist (package '(#P"alexandria-2009-08-13/"
		   #P"babel_0.3.0/"
	           #P"cffi_0.10.5/"
	           #P"cl-opengl/"
		   #P"cl-png-0.6/"
		   #P"closure-common-2008-11-30/"
		   #P"cxml-2008-11-30/"
		   #P"puri-1.5.1/"
		   #P"trivial-features_0.5/"
		   #P"trivial-gray-streams-2008-11-02/"
		   #P"zpb-ttf-1.0/"))
  (pushnew (merge-pathnames package #P"./systems/")
	   asdf:*central-registry*))

(with-output-to-string (*error-output-not*)
  (with-output-to-string (*standard-output-not*)
    (asdf:operate 'asdf:load-op :roto-mortar :verbose nil)))

(in-package :roto-mortar)
(save-and-die)
