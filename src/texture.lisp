;;;; texture.lisp

(in-package #:clglbo)

(defclass texture2d ()
  ((id
    :accessor id
    :type unsigned-byte)
   (width
    :initform 0
    :type unsigned-byte)
   (height
    :initform 0
    :type unsigned-byte)
   (internal-format
    :initform :rgb)
   (image-format
    :initform :rgb)
   (wrap-s
    :initform :repeat)
   (wrap-t
    :initform :repeat)
   (filter-min
    :initform :linear)
   (filter-max
    :initform :linear)))

(defmethod initialize-instance :after ((tex texture2d) &key)
  (setf (id tex) (gl:gen-textures 1)))

(defun generate (texture tex-width tex-height data)
  (declare (texture2d texture) (unsigned-byte width height))
  (with-slots (width height id
               wrap-s wrap-t
               filter-min filter-max
               internal-format image-format) texture
    (setf width tex-width
          height tex-height)
    (gl:bind-texture :texture-2d id)
    (gl:tex-image-2d :texture-2d 0 :rgb width height 0 image-format :unsigned-byte data)
    (gl:tex-parameter :texture-2d :texture-wrap-s wrap-s)
    (gl:tex-parameter :texture-2d :texture-wrap-t wrap-t)
    (gl:tex-parameter :texture-2d :texture-min-filter filter-min)
    (gl:tex-parameter :texture-2d :texture-mag-filter filter-max)
    (gl:bind-texture :texture-2d 0)))

(defun bind (texture)
  (declare (texture2d texture))
  (gl:bind-texture :texture-2d (id texture)))
