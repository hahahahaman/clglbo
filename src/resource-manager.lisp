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

(defgeneric load-resource (manager name resource)
  (:documentation "Loads RESOURCE into MANAGER. Can be retrieved with NAME."))
(defgeneric get-resource (manager name)
  (:documentation "Returns RESOURCE with key NAME, if it can be found, otherwise nil."))
(defgeneric clear-resources (manager)
  (:documentation "Cleans up all resources and empties RESOURCES."))

(defmethod initialize-instance :after ((manager resource-manager) &key)
  t)

(defmethod load-resource ((manager resource-manager) name resource)
  (push (cons name resource) (resources manager)))

(defmethod get-resource ((manager resource-manager) name)
  (let ((resource (cdr (assoc name (resources manager)
                              :test (if (stringp name) #'string= #'eql)))))
    (if resource
        resource
        (warn "\"~a\" not found in ~a.~%" name manager))))

(defmethod clear-resources :after ((manager resource-manager))
  (setf (resources manager) '()))

(defmethod clear-resources ((manager program-manager))
  (iter (for (name . resource) in (resources manager))
    (gl:delete-program (id resource))))

(defmethod clear-resources ((manager texture-manager))
  (gl:delete-textures
   (iter (for (name . resource) in (resources manager))
     (collect (id resource)))))

