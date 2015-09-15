(in-package #:clglbo)

(defun add-entity (components-list)
  (setf *entities*
        (append *entities* (list components-list))))
(defun add-entities (&rest entities)
  (mapcar #'add-entity entities))

(defun remove-entity (n)
  (setf (getf (nth n *entities*) :remove?) t))
(defun remove-entities (&rest entity-nums)
  (mapcar #'remove-entity entity-nums))

(defun get-component (component components)
  (getf components component))
(defmacro set-component (component value components)
  `(setf ,components (plist-set ,components ,component ,value)))
;; (defun (setf get-component) (value component components)
;;   (setf components (plist-set components component value)))

(defun update-entities ()
  ;; (setf *entities* (mapcar #'copy-hash-table *entities*))
  t)
