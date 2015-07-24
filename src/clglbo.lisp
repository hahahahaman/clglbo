;;;; clglbo.lisp

(in-package #:clglbo)

;;; "clglbo" goes here. Hacks and glory await!

(defun clglbo()
  (let ((breakout (make-instance 'breakout-window)))
    (window-run breakout)))
