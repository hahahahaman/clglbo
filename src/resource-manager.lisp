;;;; resource-manager.lisp

(in-package #:clglbo)

(defclass resource-manager ()
  ((resources
    :type list
    :initarg :resources
    :accessor resources))
  (:default-initargs
   :resources nil))

(defclass program-manager (resource-manager)
  ())

(defclass texture-manager (resource-manager)
  ())

(defgeneric load-resource (name resource manager )
  (:documentation "Loads RESOURCE into MANAGER. Can be retrieved with NAME."))
(defgeneric get-resource (name manager )
  (:documentation "Returns RESOURCE with key NAME, if it can be found, otherwise nil."))
(defgeneric clear-resources (manager)
  (:documentation "Cleans up all resources and empties RESOURCES."))

(defmethod initialize-instance :after ((manager resource-manager) &key)
  t)

(defmethod load-resource (name resource (manager resource-manager))
  (push (cons name resource) (resources manager)))

(defun load-texture (name resource &optional (mananger *texture-manager*))
  (load-resource name resource mananger))
(defun load-program (name resource &optional (mananger *program-manager*))
  (load-resource name resource mananger))

(defmethod get-resource (name (manager resource-manager))
  (let ((resource (cdr (assoc name (resources manager)
                              :test (if (stringp name) #'string= #'eql)))))
    (if resource
        resource
        (warn "\"~a\" not found in ~a.~%" name manager))))

(defun get-texture (name &optional (manager *texture-manager*))
  (get-resource name manager))
(defun get-program (name &optional (manager *program-manager*))
  (get-resource name manager))

(defmethod clear-resources :after ((manager resource-manager))
  (setf (resources manager) '()))

(defmethod clear-resources ((manager program-manager))
  ;; (mapcar (lambda (resource) (gl:delete-program (id (cdr resource))) (resources manager)))
  (iter (for (name . resource) in (resources manager))
    (gl:delete-program (id resource))))

(defmethod clear-resources ((manager texture-manager))
  (gl:delete-textures
   (mapcar (lambda (resource) (id (cdr resource))) (resources manager))
   ;; (iter (for (name . resource) in (resources manager))
   ;;   (collect (id resource)))
   ))

