;;;; clglbo.lisp

(in-package #:clglbo)

;;; "clglbo" goes here. Hacks and glory await!

(defun clglbo()
  (setf *default-pathname-defaults* (asdf:system-source-directory :clglbo))
  (let ((breakout (make-instance 'breakout-window)))
    (run breakout)))
