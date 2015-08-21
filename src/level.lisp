(in-package #:clglbo)

(defclass level ()
  ((bricks
    :accessor bricks
    :initarg :bricks)
   (world
    :initarg :world))
  (:default-initargs
   :bricks (make-array 0 :fill-pointer 0 :adjustable 0)))

(defmethod load-level ((level level))
  (with-slots (bricks world) level
    (iter (for brick in-vector bricks)
      (setf (id brick) (make-entity world))
      (with-accessors ((pos position-component)
                       (size size-component)
                       (phy physics-component)
                       (rend render-component)
                       (state state-component)) brick
        (add-components world (id brick) pos size phy rend state)))))
