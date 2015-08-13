(in-package #:clglbo)

(defclass object ()
  ((id
    :initarg :id)
   (position-component
    :initarg :position-component)
   (velocity-component
    :initarg :velocity-component)
   (render-component
    :initarg :render-component)
   (state-component
    :initarg :state-component))
  (:default-initargs
   :id nil
   :position-component nil
   :velocity-component nil
   :render-component nil
   :state-component nil))

(defmethod initialize-instance :after ((obj object) &key))
