(in-package #:clglbo)

(let ((id 0))
  (defun make-entity (components &optional (entities *entities*))
    (incf id)
    (values (fset:with entities id components) id)))

;; (defun add-entity (entity &optional (entities *entities*))
;;   (append entities (list entity)))

;; (defun add-entities (entity-list &optional (entities *entities*))
;;   (append entities entity-list))

(defun remove-entity (id &optional (entities *entities*))
  (less entities id))
(defmacro remove-entities (ids &optional (entities *entities*))
  `(-> entities ,@(mapcar (lambda (id) `(less ,i)) ids)))

(defun get-component (component id &optional (entities *entities*))
  (@ (@ entities id) component))
(defun (setf get-component) (value component id &optional (entities *entities*))
  (with (@ entities id) component value))

;; (defmacro set-component (component entity-id value)
;;   `(setf ,components (plist-set ,components ,component ,value)))
;; (defun (setf get-component) (value component entity-id &optional (entities *entities*))
;;   (setf (cdr (nth entity-id entities))))
;; ;; (defun (setf get-component) (value component components)
;; ;;   (setf components (plist-set components component value)))

(defun update-entities (&optional (changes nil) (entities *entities*))
  (mapcar #'funcall changes)
  ;; (setf *entities* (remove-if (lambda (e) (getf e :remove?)) *entities*))
  ;; (setf *entities* (mapcar #'copy-hash-table *entities*))
  t)

(defun get-entity (id &optional (entities *entities*))
  (@ entities id))
(defun (setf get-entity) (value id &optional (entities *entities*))
  (with (@ entities id) value))
