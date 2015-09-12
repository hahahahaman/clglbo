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
      (with-components ((pos-comp position-component)
                        (size-comp size-component)
                        (rend-comp render-component)) world system
        (with-slots (pos) pos-comp
          (with-slots (size) size-comp
            (with-slots (sprite color rotation) rend-comp
             (sprite-render *sprite-renderer*
                            sprite
                            pos
                            size
                            rotation
                            color)))))
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
   :time-step (/ 1.0 120.0)
   :time-accumulator 0.0))

(defmethod update-system ((world world) (system movement-system) dt)
  (with-slots (time-step time-accumulator) system
    (incf time-accumulator dt)

    (with-components ((pos-comp position-component)
                      (move-comp move-component)) world system
      (with-slots (accel vel) move-comp
        (with-slots (pos) pos-comp
          (iter (while (>= time-accumulator time-step))
            (let* ((vec-type (type-of accel))
                   ;; constant acceleration is assumed
                   (a/2 (vec-mul accel (* 0.5 time-step))))
              ;; sympletic euler integration
              ;; more accurate usage of newton's equations
              ;; http://www.niksula.hut.fi/~hkankaan/Homepages/gravity.html
              ;; v += a/2 * dt
              ;; p += v * dt
              ;; v += a/2 * dt
              (setf vel (vec-add vel a/2)
                    pos (vec-add pos (vec-mul vel time-step))
                    vel (vec-add vel a/2)))
            (decf time-accumulator time-step)))))))

(defclass player-movement-system (movement-system)
  ()
  (:default-initargs
   :dependencies '(position-component size-component
                   movement-component player-component)))

(defmethod update-system :after ((world world) (system player-movement-system) dt)
  (with-components ((pos-comp position-component) (size-comp size-component)) world system
    (with-slots (pos) pos-comp
      (with-slots (size) size-comp
        ;; restrict player x position from 0 to *WIDTH*
        (cond ((< (x-val pos) 0.0)
               (setf (aref pos 0) 0.0))
              ((> (+ (x-val pos) (x-val size)) *width*)
               (setf (aref pos 0) (cfloat *width*))))))))

(defsystem input-system (move-component input-component))

(defmethod update-system ((world world) (system input-system) dt)
  (with-components ((move-comp move-component)) world system
    (with-slots (accel) move-comp
      (let ((a-down? (key-down-p :a))
            (d-down? (key-down-p :d)))
        ;; A and D key behavior
        (setf (aref accel 0) (cond ((and a-down? d-down?) 0.0)
                                   (a-down? -100.0)
                                   (d-down? 100.0)
                                   (t 0.0)))))))

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
