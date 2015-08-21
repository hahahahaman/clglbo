;;;; globals.lisp

(in-package #:clglbo)

;;; modes

;; debug mode, default nil
(defparameter *debug* nil)
;; paused
(defparameter *paused* nil)

;;; screen constants
;; not really much use, just keeps value holders
(defparameter *width* 800)
(defparameter *height* 600)

;;; delta time
;; *DT* keeps track of the time since last frame, in seconds
;; *PREVIOUS-TIME* gives the time, in seconds, of the previous frame
;; since the start of the program, and (glfw:get-time) returns the
;; current time
(defparameter *dt* 0.0)
(defparameter *previous-time* 0.0)

;;; actions
;; p-lists that keep track of the current actions on keys and buttons
(defparameter *key-actions* ())
(defparameter *mouse-button-actions* ())

;;; singletons
;; singletons used for resource management and rendering
(defparameter *program-manager* nil)
(defparameter *texture-manager* nil)
(defparameter *sprite-renderer* nil)

;;systems
(defparameter *render-system* nil)
(defparameter *physics-system* nil)

;;; cursor position values
(defparameter *cursor-callback-p* nil) ;; cursor has been moved
(defparameter *first-mouse* t) ;; checks if first time cursor has been moved

;; current cursor position
(defparameter *cursor-x* (/ *width* 2.0))
(defparameter *cursor-y* (/ *height* 2.0))

;; previous cursor position
(defparameter *last-x* (/ *width* 2.0))
(defparameter *last-y* (/ *height* 2.0))

;; the scroll wheel has been used
(defparameter *scroll-callback-p* nil)

;; number of ticks of the scroll wheel
(defparameter *scroll-x* (/ *width* 2.0))
(defparameter *scroll-y* (/ *height* 2.0))
