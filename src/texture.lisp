;;;; texture.lisp

(in-package #:clglbo)

(defclass texture2d ()
  ((id
    :accessor id
    :type unsigned-byte
    :initarg :id)
   (width
    :type unsigned-byte
    :accessor width
    :initarg :width)
   (height
    :type unsigned-byte
    :accessor height
    :initarg :height)
   (internal-format
    :accessor internal-format
    :initarg :internal-format)
   (image-format
    :accessor image-format
    :initarg :image-format)
   (wrap-s
    :accessor wrap-s
    :initarg :wrap-s)
   (wrap-t
    :accessor wrap-t
    :initarg :wrap-t)
   (filter-min
    :accessor filter-min
    :initarg :filter-min)
   (filter-max
    :accessor filter-max
    :initarg :filter-max))
  (:default-initargs
   :id (elt (gl:gen-textures 1) 0)
   :width 0
   :height 0
   :internal-format :rgb
   :image-format :rgb
   :wrap-s :repeat
   :wrap-t :repeat
   :filter-min :linear
   :filter-max :linear))

(defmethod initialize-instance :after ((tex texture2d) &key)
  t)

(defmethod texture2d-generate ((tex texture2d) tex-width
                               tex-height image)
  (with-slots (width height id
               wrap-s wrap-t
               filter-min filter-max
               internal-format image-format) tex
    (setf width tex-width
          height tex-height)
    (gl:bind-texture :texture-2d id)
    (gl:tex-image-2d :texture-2d 0 internal-format
                     width height 0 image-format
                     :unsigned-byte image)
    (gl:tex-parameter :texture-2d :texture-wrap-s wrap-s)
    (gl:tex-parameter :texture-2d :texture-wrap-t wrap-t)
    (gl:tex-parameter :texture-2d :texture-min-filter filter-min)
    (gl:tex-parameter :texture-2d :texture-mag-filter filter-max)
    (gl:bind-texture :texture-2d 0)))

(defmethod bind ((tex texture2d))
  (gl:bind-texture :texture-2d (id tex)))

(defun make-texture2d (filepath &optional (alpha t))
  "Returns texture2d instance."
  (declare (boolean alpha))
  (let ((texture2d (make-instance 'texture2d)))
    (with-accessors ((internal-format internal-format)
                     (image-format image-format)) texture2d
      (when alpha
        (setf internal-format :rgba
              image-format :rgba))
      (let* ((data (cl-soil:load-image filepath image-format))
             (image (first data))
             (width (second data))
             (height (third data)))
        (texture2d-generate texture2d width height image)
        (cl-soil:free-image-data image)))
    texture2d))
