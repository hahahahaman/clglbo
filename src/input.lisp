;;;; input.lisp


(in-package #:clglbo)

;;; glfw callback
;;; changes global variables when glfw action is recorded

;; keys pressed
(glfw:def-key-callback key-callback (window key scancode action mod-keys)
  (declare (ignore window scancode mod-keys))
  (setf (getf *key-actions* key) action)

  ;; trouble getting :repeat events, so keep key pressed until :release
  (cond ((eq action :press)
         (setf (getf *key-pressed* key) t))
        ((eq action :release)
         (setf (getf *key-pressed* key) nil))))

;; mouse button pressed
(glfw:def-mouse-button-callback mouse-callback (window button action mod-keys)
  (declare (ignore window mod-keys))
  (setf (getf *mouse-button-actions* button) action)

  (cond ((eq action :press)
         (setf (getf *mouse-button-pressed* button) t))
        ((eq action :release)
         (setf (getf *mouse-button-pressed* button) nil))))

;; cursor movement
(glfw:def-cursor-pos-callback cursor-callback (window x y)
  (declare (ignore window))
  (cond
    ;; first time cursor moved, initialize *last-x* and *last-y*
    (*first-mouse*
         (setf *last-x* x
               *last-y* y
               *first-mouse* nil))
    ;; set current cursor position
    (t
     (setf *cursor-callback-p* t
           *cursor-x* x
           *cursor-y* y))))

;; scroll wheel
(glfw:def-scroll-callback scroll-callback (window x y)
  (declare (ignore window))
  ;; set scroll wheel movement
  (setf *scroll-callback-p* t
        *scroll-x* x
        *scroll-y* y))
