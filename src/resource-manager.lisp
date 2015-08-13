;;;; resource-manager.lisp

(in-package #:clglbo)

(defclass resource-manager ()
  ((resources
    :type hash-table
    :initform (make-hash-table)
    :accessor resources)))

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
  (setf (gethash name (resources manager)) resource))

(defmethod get-resource ((manager resource-manager) name)
  (gethash name (resources manager)))

(defmethod clear-resources ((manager resource-manager))
  (setf (resources manager) (make-hash-table)))

(defmethod clear-resources ((manager program-manager))
  (iter (for (name resource) in-hashtable (resources manager))
    (gl:delete-program (id resource)))
  (call-next-method))

(defmethod clear-resources ((manager texture-manager))
  (gl:delete-textures
   (iter (for (name resource) in-hashtable (resources manager))
     (collect (id resource))))
  (call-next-method))
