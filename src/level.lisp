(in-package #:clglbo)

(defclass level ()
  ((bricks
    :initarg :bricks)))

(defmethod initialize-instance :after ((level level) &key)
  )

(defmethod init ((level level) data width height))

(defun make-level (filename)
  )

(defmethod draw ((level level) (renderer sprite-renderer))
  )
