;;;; globals.lisp

(in-package #:clglbo)

(defparameter *dt* 0.0)
(defparameter *previous-time* 0.0)

(defparameter *keys-pressed* nil)
(defparameter *buttons-pressed* nil)

;; glfw call globals

(defparameter *width* 800)
(defparameter *height* 600)

(defparameter *cursor-callback-p* nil)
(defparameter *scroll-callback-p* nil)

(defparameter *cursor-x* (/ *width* 2.0))
(defparameter *cursor-y* (/ *height* 2.0))
(defparameter *scroll-x* (/ *width* 2.0))
(defparameter *scroll-y* (/ *height* 2.0))
(defparameter *first-mouse* t)
(defparameter *last-x* (/ *width* 2.0))
(defparameter *last-y* (/ *height* 2.0))
