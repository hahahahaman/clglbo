;;;; texture.lisp

(in-package #:clglbo)

(defclass texture2d ()
  ((id)
   (width)
   (height)
   (internal-format)
   (image-format)
   (wrap-s)
   (wrap-t)
   (filter-min)
   (filter-max)))

(defun generate (texture width height data))

(defun bind (texture))
