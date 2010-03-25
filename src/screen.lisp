(in-package :roto-mortar)

(defclass screen ()
  ((items :initform nil)
   (overlays :initform nil)
   (elapsed-time :initform 0.0)))

(defgeneric load-screen (screen)
  (:method-combination progn :most-specific-last)
  (:method progn ((screen screen))
    (with-slots (elapsed-time) screen
      (setf elapsed-time 0.0))
    (update-screen screen 0.0)))

(defgeneric update-screen (screen elapsed)
  (:method-combination progn :most-specific-last)
  (:method progn ((screen screen) (elapsed real))
    (with-slots (elapsed-time items overlays) screen
      (incf elapsed-time elapsed)
      (mapc #'(lambda (ii) (update-item ii elapsed)) items)
      (mapc #'(lambda (ii) (update-item ii elapsed)) overlays))
    nil))

(defgeneric draw-screen (screen window)
  (:method-combination progn :most-specific-last)
  (:method progn ((screen screen) window)
     (with-slots (items overlays) screen
       (perspective-mode window)
       (mapc #'(lambda (ii) (draw ii screen)) items)
       (orthographic-mode window)
       (gl:with-pushed-matrix
	 (gl:translate 10 590 0)
	 (mapc #'(lambda (ii) (draw ii screen)) overlays)))
     nil))

(defgeneric unload-screen (screen)
  (:method-combination progn :most-specific-first)
  (:method progn ((screen screen))
    (declare (ignore screen))))

(defgeneric key-down (screen key)
  (:method-combination progn :most-specific-last)
  (:method progn ((screen screen) (key character))
    (declare (ignore screen key))))

(defgeneric key-up (screen key)
  (:method-combination progn :most-specific-last)
  (:method progn ((screen screen) (key character))
    (declare (ignore screen key))))

(defgeneric mouse-down (screen button)
  (:method-combination progn :most-specific-last)
  (:method progn ((screen screen) button)
    (declare (ignore screen button))))

(defgeneric mouse-up (screen key)
  (:method-combination progn :most-specific-last)
  (:method progn ((screen screen) button)
    (declare (ignore screen key))))
