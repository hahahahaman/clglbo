(in-package #:clglbo)

(defclass object ()
  ((id
    :initarg :id
    :accessor id)
   (position-component
    :accessor position-component
    :initarg :position-component)
   (size-component
    :accessor size-component
    :initarg :size-component)
   (render-component
    :accessor render-component
    :initarg :render-component)
   (physics-component
    :accessor physics-component
    :initarg :physics-component)
   (state-component
    :accessor state-component
    :initarg :state-component))
  (:default-initargs
   :id nil
   :position-component nil
   :size-component nil
   :render-component nil
   :state-component nil))

(defmethod initialize-instance :after ((obj object) &key))
