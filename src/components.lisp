(in-package #:clglbo)

(defstruct position-component vec)

(defstruct size-component vec)

(defstruct move-componenet accel vel)

(defstruct physics-component collision-obj)

(defstruct render-component sprite color rotation)

(defstruct input-component)

(defstruct brick-component solid-p)

(defstruct ball-component)

(defstruct player-component)

(defclass static-image ()
  ((position-component
    :initarg :position-component
    :accessor position-component)
   (size-component
    :initarg :size-component
    :accessor size-component)
   (render-component
    :initarg :render-component
    :accessor render-component))
  (:default-initargs
   :position-component (make-position-component :vec (vec2 0.0 0.0))
   :size-component (make-size-component :vec (vec2 800.0 600.0))
   :render-component (make-render-component :color (vec4 1.0 1.0 1.0 1.0)
                                            :rotation 0.0)))

(defmethod add-object ((obj static-image) (world world) (entity-id fixnum))
  (with-slots ((pos position-component) (size size-component)
               (rend render-component)) obj
    (add-components world entity-id pos size rend)))
