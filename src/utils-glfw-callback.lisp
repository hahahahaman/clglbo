;;;; utils-glfw-callback.lisp

;;;; glfw callback
;;;; changes global variables when glfw action is recorded

(in-package #:clglbo)

;; keys pressed
(glfw:def-key-callback key-callback (window key scancode action mod-keys)
  (declare (ignore window scancode mod-keys))
  (cond
    ;; close window when ESC pressed
    ((and (eq key :escape) (eq action :press))
     (glfw:set-window-should-close))
    ;;when something pressed, put key into *keys-pressed*
    ((eq action :press)
     (pushnew key *keys-pressed*))
    ;; when key is released remove from key from *keys-pressed*
    ((eq action :release)
     (alexandria:deletef *keys-pressed* key))))

;; mouse button pressed
(glfw:def-mouse-button-callback mouse-callback (window button action mod-keys)
  (declare (ignore window mod-keys))
  (cond ((eq action :press)
         (pushnew button *buttons-pressed*))
        ((eq action :release)
          (alexandria:deletef *buttons-pressed* button))))

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
