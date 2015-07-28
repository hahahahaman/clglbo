;;;; texture.lisp

(in-package #:clglbo)

(defclass texture2d ()
  ((id
    :accessor id
    :type unsigned-byte
    :initarg :id)
   (width
    :type unsigned-byte
    :initarg :width)
   (height
    :type unsigned-byte
    :initarg :height)
   (internal-format
    :initarg :internal-format)
   (image-format
    :initarg :image-format)
   (wrap-s
    :initarg :wrap-s)
   (wrap-t
    :initarg :wrap-t)
   (filter-min
    :initarg :filter-min)
   (filter-max
    :initarg :filter-max))
  (:default-initargs
   :width 0
   :height 0
   :internal-format :rgb
   :image-format :rgb
   :wrap-s :repeat
   :wrap-t :repeat
   :filter-min :linear
   :filter-max :linear))

(defmethod initialize-instance :after ((tex texture2d) &key)
  (setf (id tex) (elt (gl:gen-textures 1) 0)))

(defun texture2d-generate (texture tex-width tex-height image)
  (with-slots (width height id
               wrap-s wrap-t
               filter-min filter-max
               internal-format image-format) texture
    (setf width tex-width
          height tex-height)
    (gl:bind-texture :texture-2d id)
    (gl:tex-image-2d :texture-2d 0 internal-format width height 0 image-format :unsigned-byte image)
    (gl:tex-parameter :texture-2d :texture-wrap-s wrap-s)
    (gl:tex-parameter :texture-2d :texture-wrap-t wrap-t)
    (gl:tex-parameter :texture-2d :texture-min-filter filter-min)
    (gl:tex-parameter :texture-2d :texture-mag-filter filter-max)
    (gl:bind-texture :texture-2d 0)))

(defun texture2d-bind (texture)
  (declare (texture2d texture))
  (gl:bind-texture :texture-2d (id texture)))

(defun make-texture (filepath &optional (alpha t))
  (declare (boolean alpha))
  (let ((texture (make-instance 'texture2d)))
    (with-slots (internal-format image-format) texture
      (when alpha
        (setf internal-format :rgba
              image-format :rgba))
      (let* ((data (cl-soil:load-image filepath image-format))
             (image (first data))
             (width (second data))
             (height (third data)))
        (texture2d-generate texture width height image)
        (cl-soil:free-image-data image)))
    texture))
