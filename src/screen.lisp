(in-package :roto-mortar)

(defclass screen ()
  ((items :initform nil)
   (elapsed-time :initform 0.0)))

(defgeneric load-screen (screen)
  (:method-combination progn :most-specific-last)
  (:method progn ((screen screen))
    (with-slots (elapsed-time) screen
      (setf elapsed-time 0.0))))

(defgeneric update-screen (screen elapsed)
  (:method-combination progn :most-specific-last)
  (:method progn ((screen screen) (elapsed real))
    (with-slots (elapsed-time) screen
      (incf elapsed-time elapsed))
    nil))

(defgeneric draw-screen (screen)
  (:method-combination progn :most-specific-last)
  (:method progn ((screen screen))
     (with-slots (items) screen
       (mapc #'(lambda (ii) (draw ii screen)) items))
     nil))

(defgeneric unload-screen (screen)
  (:method-combination progn :most-specific-first)
  (:method progn ((screen screen))
    (declare (ignore screen))))

(defgeneric key-down (screen key)
  (:method-combination progn :most-specific-first)
  (:method progn ((screen screen) (key character))
    (declare (ignore screen key))))

(defgeneric key-up (screen key)
  (:method-combination progn :most-specific-first)
  (:method progn ((screen screen) (key character))
    (declare (ignore screen key))))
