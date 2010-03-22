(in-package :roto-mortar)

(defun main ()
  (setf *font*
        (zpb-ttf:open-font-loader (merge-pathnames #P"okolaksRegular.ttf"
						   *data-directory*)))
  (glut:display-window (make-instance 'roto-mortar-window)))

(defun save-and-die ()
  (setf cl-glut::*glut-initialized-p* nil)
  (sb-ext:save-lisp-and-die #P"roto-mortar"
                            :toplevel #'main
                            :executable t
                            :save-runtime-options t
                            :purify t))
