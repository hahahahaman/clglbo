#||
window.lisp

Simplify glfw based windows
||#

(in-package #:clglbo)

(defclass window ()
  ((title
    :initarg :title)
   (width
    :initarg :width)
   (height
    :initarg :height))
  (:default-initargs
   :title "window"
   :width *width*
   :height *height*))

(defgeneric window-load-textures (window))
(defgeneric window-load-shaders (window))
(defgeneric window-load-rest (window))

(defgeneric window-update (window))
(defgeneric window-render (window))
(defgeneric window-run (window))

(defgeneric window-cleanup (window))

(defmethod initialize-instance :after ((window window) &key)
  t)
