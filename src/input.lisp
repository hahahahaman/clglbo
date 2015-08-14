;;;; input.lisp


(in-package #:clglbo)

;;; glfw callback
;;; changes global variables when glfw action is recorded

;; keys pressed
(glfw:def-key-callback key-callback (window key scancode action mod-keys)
  (declare (ignore window scancode mod-keys))
  (setf (getf *key-actions* key) action)
  ;; (print *key-actions*)
  ;; (cond ((eq action :press)
  ;;        (setf (getf *key-actions* key) :press))
  ;;       ((eq action :release)
  ;;        (setf (getf *key-actions* key) :release)))
  )

;; mouse button pressed
(glfw:def-mouse-button-callback mouse-callback (window button action mod-keys)
  (declare (ignore window mod-keys))
  (setf (getf *mouse-button-actions* button) action)
  ;; (print *mouse-button-actions*)
  ;; (cond ((eq action :press)
        ;;  (setf (getf *mouse-button-actions* button) :press))
        ;; ((eq action :release)
        ;;  (setf (getf *mouse-button-actions* button) :release)))
  )

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
