(in-package :roto-mortar)

(defgeneric draw (drawn-item screen))
(defgeneric draw-as-shadow-volume (drawn-item screen))

(defgeneric update-item (drawn-item elapsed))