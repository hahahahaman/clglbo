;;; game.lisp

(in-package #:clglbo)

(defenum:defenum *game-state*
                 ((+game-active+ 0)
                  +game-menu+
                  +game-win+))

(defclass game ()
  ((state
    :type (unsigned-byte 32)
    :initarg :state)
   (width
    :type (unsigned-byte 32)
    :initarg :width)
   (height
    :type (unsigned-byte 32)
    :initarg :height))
  (:default-initargs
   :state +game-active+
   :width 0
   :height 0))

(defgeneric game-process-input (game dt))
(defgeneric game-update (game dt))
(defgeneric game-render (game))

(defmethod initialize-instance :after ((game game) &key)
  t)

(defmethod game-process-input ((game game) dt)
  t)

(defmethod game-update ((game game) dt)
  t)

(defmethod game-render ((game game))
  t)
