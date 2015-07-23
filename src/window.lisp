#||
window.lisp

Simplify glfw based windows
||#

(in-package #:clglbo)

(defclass window ()
  ((title)))

(defgeneric load-textures (window))
(defgeneric load-shaders (window))
(defgeneric load-rest (window))

(defgeneric update-window (window))
(defgeneric render-window (window))
(defgeneric run-window (window))

(defgeneric cleanup-window (window))

(defmethod initialize-instance :after ((window glfw-window) &key (title ""))
  (setf (slot-value window 'title) title))
