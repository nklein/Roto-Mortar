(in-package :roto-mortar)

(defclass start-screen (screen)
  ((title-position :initform (list 400.0 500.0 0.0))
   (title-angle :initform 0.0)
   (warned :initform nil)))

(defvar +start-lines+ '(
"Hey, commander.  The KPs are really slapping us."
"   If you have to bail, just press 'Q'.  Okay?"
))

(defmethod update-screen progn ((screen start-screen) elapsed)
  (with-slots (elapsed-time title-position title-angle overlays warned) screen
    (let ((pp (* (min (max (- elapsed-time 0.5) 0.0) 0.5) 2))
	  (aa (min 4.0 (max 0.0 (- elapsed-time 1.5)))))
      (setf (second title-position) (+ (* 500.0 (- 1.0 pp))
				       (* 200.0 pp))
	    title-angle             (* 90.0 aa))
      (when (and (not warned)
		 (< 2.0 elapsed-time))
	(push (make-message-overlay #P"billy-bob.png"
				    :lines +start-lines+)
	      overlays)
	(setf warned t)))
    (when (< 7.5 elapsed-time)
      (make-instance 'main-menu-screen))))

(defmethod draw-screen progn ((screen start-screen) window)
  (orthographic-mode window)
  (with-slots (title-position title-angle) screen
    (gl:with-pushed-matrix
      (apply #'gl:translate title-position)
      (gl:rotate title-angle 0.0 1.0 0.0)
      (gl:with-pushed-attrib (:current-bit)
	(gl:color 0.8 0.8 0.2 1.0)
	(draw-string "Roto Mortar" :size 96)))))

(defmethod mouse-up progn ((screen start-screen) button)
  (declare (ignore button))
  (with-slots (elapsed-time) screen
    (when (< 1.5 elapsed-time)
      (make-instance 'main-menu-screen))))
