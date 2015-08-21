(in-package #:clglbo)

(defsystem render-system (position-component size-component render-component))

(defmethod update-system ((world world) (system render-system) dt)
  (with-components ((pos position-component)
                    (size size-component)
                    (rend render-component)) world system
    (sprite-render *sprite-renderer*
                   (render-component-sprite rend)
                   (position-component-vec pos)
                   (size-component-vec size)
                   (render-component-rotation rend)
                   (render-component-color rend))))

(defclass physics-system ()
  ((dependencies
    :initarg :dependencies
    :type cons
    :accessor dependencies)
   (time-step
    :initarg :time-step
    :type single-float
    :accessor time-step)
   (time-accumulator
    :initarg :time-accumulator
    :type single-float
    :accessor time-accumulator))
  (:default-initargs
   :dependencies '(position-component physics-component)
   :time-accumulator 0.0))

;; (defmethod update-system ((world world) (system physics-system) dt)
;;   (with-accessors ((accumulator time-accumulator)) system
;;     (with-components ((pos position-component) (phy physics-component)) world system
;;       (let ((previous-pos (position-component-vec pos))
;;             (previous-velocity (physics-component-velocity phy))
;;             (previous-acceleration (physics-component-acceleration phy)))
;;         ))))
