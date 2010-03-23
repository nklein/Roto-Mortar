(in-package :roto-mortar)

(defclass start-screen (screen)
  ((title-position :initform (list 0.0 400.0 200.0))
   (title-angle :initform 180.0)))

(defmethod update-screen progn ((screen start-screen) elapsed)
  (with-slots (elapsed-time title-position title-angle) screen
    (let ((pp (* (min elapsed-time 0.5) 2))
	  (aa (min 4.0 (max 0.0 (- elapsed-time 0.5)))))
      (setf (second title-position) (+ (* 500.0 (- 1.0 pp))
				       (*  50.0 pp))
	    (third title-position)  (* 200.0 (- 1.0 pp))
	    title-angle             (+ 180.0 (* 90.0 aa))))
    (when (< 7.5 elapsed-time)
      (make-instance 'main-menu-screen))))

(defmethod draw-screen progn ((screen start-screen))
  (with-slots (title-position title-angle) screen
    (gl:with-pushed-matrix
      (apply #'gl:translate title-position)
      (gl:rotate title-angle 0.0 1.0 0.0)
      (gl:with-pushed-attrib (:current-bit)
	(gl:color 0.8 0.8 0.2)
	(draw-string "Roto Mortar" :size 96)))))

(defmethod mouse-up progn ((screen start-screen) button)
  (declare (ignore button))
  (with-slots (elapsed-time) screen
    (when (< 1.5 elapsed-time)
      (make-instance 'main-menu-screen))))
