;;; game.lisp

(in-package #:clglbo)

(defenum:defenum Game-State
                 ((+game-active+ 0)
                  +game-menu+
                  +game-win+))

(defclass game ()
  ((state
    :type (unsigned-byte 32)
    :initarg :state)
   (keys
    :type list
    :initarg :keys)
   (width
    :type (unsigned-byte 32)
    :initarg :width)
   (height
    :type (unsigned-byte 32)
    :initarg :height))
  (:default-initargs
   :state +game-active+
   :keys nil
   :width 0
   :height 0))

(defgeneric process-input (game dt))
(defgeneric update-game (game dt))
(defgeneric render-game (game))

(defmethod initialize-instance :after ((g game) &key)
  t)

(defmethod process-input ((game game) dt)
  t)

(defmethod update-game ((game game) dt)
  t)

(defmethod render-game ((game game))
  t)
