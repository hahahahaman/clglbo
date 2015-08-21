(in-package #:clglbo)

(defstruct position-component vec)

(defstruct size-component vec)

(defstruct physics-component acceleration velocity collision-type)

(defstruct render-component sprite color rotation)

(defstruct state-component solid-p destroyed-p)
