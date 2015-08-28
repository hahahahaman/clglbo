(in-package #:clglbo)

;; (defsystem render-system (position-component size-component render-component))

(defclass render-system (system)
  ((time-step
    :initarg :time-step
    :reader time-step)
   (time-accumulator
    :initarg :time-accumulator
    :accessor time-accumulator))
  (:default-initargs
   :dependencies '(position-component size-component render-component)
   :time-step (/ 1.0 60.0)
   :time-accumulator 0.0))

(defmethod update-system ((world world) (system render-system) dt)
  (with-slots (time-step time-accumulator) system
    (incf time-accumulator dt)

    ;; when enough time has passed render
    (when (>= time-accumulator time-step)
      (with-components ((pos position-component)
                        (size size-component)
                        (rend render-component)) world system
        (sprite-render *sprite-renderer*
                       (render-component-sprite rend)
                       (position-component-vec pos)
                       (size-component-vec size)
                       (render-component-rotation rend)
                       (render-component-color rend)))
      (decf time-accumulator time-step))))

(defclass movement-system (system)
  ((time-step
    :initarg :time-step
    :reader time-step)
   (time-accumulator
    :initarg :time-accumulator
    :accessor time-accumulator))
  (:default-initargs
   :dependencies '(position-component move-component)
   :time-step (/ 1.0 60.0)
   :time-accumulator 0.0))

(defmethod update-system ((world world) (system movement-system) dt)
  (with-slots (time-step time-accumulator) system
    (incf time-accumulator dt)
    (iter (while (>= time-accumulator time-step))
      (with-components ((pos position-component) (move move-component)) world system
        ;; sympletic euler integration
        ;; this is more accurate usage of newton's equations
        ;; http://www.niksula.hut.fi/~hkankaan/Homepages/gravity.html
        (let* ((a (move-component-accel move))
               (vec-type (type-of a))
               (add (lambda (v1 v2)
                      (map vec-type
                           (lambda (x y) (+ x y))
                           v1 v2)))
               (mul (lambda (v1 f)
                      (map vec-type
                           (lambda (x) (* x f))
                           v1)))
               (a/2 (funcall mul a (* 0.5 time-step))))
          ;; v += a/2 * dt
          ;; p += v * dt
          ;; v += a/2 * dt
          (setf (move-component-vel move) (funcall add (move-component-vel move) a/2)
                (position-component-vec pos) (funcall add
                                                      (position-component-vec pos)
                                                      (mul (move-component-vel move) time-step))
                (move-component-vel move) (funcall add (move-component-vel move) a/2)))))))

(defclass player-movement-system (movement-system)
  ()
  (:default-initargs
   :dependencies '(position-component size-component movement-component player-component)))

(defmethod update-system :after ((world world) (system player-movement-system) dt)
  (with-components ((pos position-component)) world system
    (cond ((< (aref (position-component-vec pos) 0) 0.0)
           (setf (aref (position-component-vec pos) 0) 0.0))
          ((>= (+ (aref (position-component-vec pos) 0)
                  (aref (size-component-vec size) 0)) *width*)
           (setf (aref (position-component-vec pos) 0))))))

(defsystem input-system (move-component input-component))

(defmethod update-system ((world world) (system input-system) dt)
  (with-components ((move move-component)) world system
    (let ((a-down? (key-down-p :a))
          (d-down? (key-down-p :d)))
      (setf (aref (move-component-accel move) 0) (cond ((and a-down? d-down?) 0.0)
                                                       (a-down? -100.0)
                                                       (d-down? 100.0)
                                                       (t 0.0))))))

;; (defclass physics-system (system)
;;   ((dependencies
;;     :initarg :dependencies
;;     :type cons
;;     :accessor dependencies)
;;    (time-step
;;     :initarg :time-step
;;     :type single-float
;;     :accessor time-step)
;;    (time-accumulator
;;     :initarg :time-accumulator
;;     :type single-float
;;     :accessor time-accumulator))
;;   (:default-initargs
;;    :dependencies '(position-component physics-component)
;;    :time-step (/ 1.0 100.0)
;;    :time-accumulator 0.0))

;; (defmethod update-system ((world world) (system physics-system) dt)
;;   (with-accessors ((accumulator time-accumulator)) system
;;     (with-components ((pos position-component) (phy physics-component)) world system
;;       (let ((previous-pos (position-component-vec pos))
;;             (previous-velocity (physics-component-velocity phy))
;;             (previous-acceleration (physics-component-acceleration phy)))
;;         ))))
