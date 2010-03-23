(in-package :roto-mortar)

(defclass start-screen (screen)
  ((title-position :initform (list 0.0 100.0 -5.0))))

(defmethod update-screen progn ((screen start-screen) elapsed)
  (with-slots (elapsed-time title-position) screen
    (let ((pp (min elapsed-time 1.0)))
      (setf (second title-position) (* 400.0 (- 1.0 pp))
	    (third title-position)  (* 200.0 (- 1.0 pp))))
    (when (< 5.0 elapsed-time)
      (make-instance 'main-menu-screen))))

(defmethod draw-screen progn ((screen start-screen))
  (with-slots (title-position) screen
    (gl:with-pushed-matrix
      (gl:scale -1.0 1.0 1.0)
      (apply #'gl:translate title-position)
      (gl:with-pushed-attrib (:current-bit)
	(gl:color 0.8 0.8 0.2)
	(draw-string "Roto Mortar" :size 64)))))
