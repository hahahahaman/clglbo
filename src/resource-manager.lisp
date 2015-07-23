;;;; resource-manager.lisp

(in-package #:clglbo)

(defclass resource-manager ()
  ((resources
    :type hash-table
    :initform (make-hash-table)
    :reader resources)))

(defclass shader-manager (resource-manager)
  ())

(defclass texture-manager (resource-manager)
  ())

(defgeneric load-resource (manager name resource))
(defgeneric get-resource (manager name))
(defgeneric clear-resources (manager))

(defmethod load-resource ((manager resource-manager) name resource)
  (setf (gethash name (resources manager)) resource))

(defmethod get-resource ((manager resource-manager) name)
  (gethash name (resources manager)))

(defmethod clear-resources ((manager resource-manager))
  (setf (resources manager) (make-hashtable)))

(defmethod clear-resources ((manager shader-manager))
  (iter (for (name resource) in-hashtable (resources manager))
        (gl:delete-shader resource))
  (call-next-method))

(defmethod clear-resources ((manager texture-manager))
  (gl:delete-textures
   (iter (for (name resource) in-hashtable (resources manager))
         (collect resource)))
  (call-next-method))

(defun make-shader (vert-path frag-path &optional geo-path)
  (let ((shader (make-instance 'shader)))
    (compile-shader shader vert-path frag-path geo-path)
    shader))

(defun make-texture (filepath &optional alpha)
  (declare (boolean alpha))
  (let ((texture (make-instance 'texture2d)))
    (with-slots (internal-format image-format) texture
      (when alpha
        (setf internal-fromat :rgba
              image-format :rgba))
      (let* ((data (cl-soil:load-image filepath image-format))
             (image (first data))
             (width (second data))
             (height (third data)))
        (generate-texture2d texture width height image)
        (cl-soil:free-image-data image)))
    texture))
