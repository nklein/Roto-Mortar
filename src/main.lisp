(in-package :roto-mortar)

(defun main (args)
  (glut:display-window (make-instance 'roto-mortar-window))
  (setf cl-glut::*glut-initialized-p* nil))
