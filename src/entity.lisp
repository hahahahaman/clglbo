(in-package #:clglbo)

(defun add-entity (components-list)
  (1- (length (setf *entities*
                    (append *entities* (list components-list))))))
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
  (setf *entities* (remove-if (lambda (e) (getf e :remove?)) *entities*))
  ;; (setf *entities* (mapcar #'copy-hash-table *entities*))
  t)

(defun get-entity (n)
  (nth n *entities*))
(defun (setf get-entity) (value n)
  (setf (nth n *entities*) value))

(defun find-entity (predicate)
  (iter (for e in *entities*) (for i from 0)
    (when (funcall predicate e)
      (return (values i e)))))

(defun set-entity-component (component value predicate)
  (let ((e (find-entity predicate)))
    (values e (set-component component value (get-entity e)))))
