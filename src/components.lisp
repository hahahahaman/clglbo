(in-package #:clglbo)

(defstruct (position-component) vec)

(defstruct (velocity-component) vec)

(defstruct (render-component) sprite color rotation size)

(defstruct (state-component) solid-p destroyed-p)
