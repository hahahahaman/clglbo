;;; game.lisp

(in-package #:clglbo)

(defenum:defenum Game-State
                 ((+game-active+ 0)
                  +game-menu+
                  +game-win+))

(defclass game ()
  ((state
    :initform +game-active+
    :type (unsigned-byte 3))
   (keys
    :initform nil
    :type list)
   (width
    :type (unsigned-byte 32)
    :initarg :width)
   (height
    :type (unsigned-byte 32)
    :initarg :height)))

(defgeneric process-input (GAME DT))
(defgeneric update (GAME DT))
(defgeneric render (GAME))

(defmethod initialize-instance :after ((g game) &key)
  t)
