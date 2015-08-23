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

(defmacro make-level (filename world level-width level-height)
  (alexandria:once-only (filename world level-width level-height)
   `(with-open-file (file ,filename :direction :input)
      (if file
          (let* ((level (make-instance 'level :world ,world))
                 (file-data (read file))
                 (width (getf file-data :width))
                 (height (getf file-data :height))
                 (data (getf file-data :data))
                 (unit-width (cfloat (/ ,level-width width)))
                 (unit-height (cfloat (/ ,level-height height))))
            (iter (for i from 0 below height)
              (iter (for j from 0 below width)
                (let ((brick (elt data (+ (* i width) j))))
                  (when (> brick 0)
                    (let ((pos-comp (make-position-component
                                     :vec (kit.glm:vec2 (* unit-width j)
                                                        (- *height* (* unit-height (1+ i))))))
                          (size-comp (make-size-component
                                      :vec (kit.glm:vec2 unit-width unit-height)))
                          (rend-comp (make-render-component
                                      :sprite (cond ((= brick 1)
                                                     (get-resource *texture-manager* "block-solid"))
                                                    (t
                                                     (get-resource *texture-manager* "block")))
                                      :color (case brick
                                               (1 (kit.glm:vec4 0.8 0.8 0.7 1.0))
                                               (2 (kit.glm:vec4 0.2 0.6 1.0 1.0))
                                               (3 (kit.glm:vec4 0.0 0.7 0.0 1.0))
                                               (4 (kit.glm:vec4 0.8 0.8 0.4 1.0))
                                               (5 (kit.glm:vec4 1.0 0.5 0.0 1.0)))
                                      :rotation 0.0))
                          (phy-comp (make-physics-component
                                     :acceleration 0.0
                                     :velocity 0.0
                                     :collision-type :aabb))
                          (state-comp (make-state-component :solid-p (= brick 1))))
                      (vector-push-extend (make-instance 'object
                                                         :id -1
                                                         :position-component pos-comp
                                                         :size-component size-comp
                                                         :render-component rend-comp
                                                         :physics-component phy-comp
                                                         :state-component state-comp)
                                          (bricks level)))))))
            level)
          (error "~a could not be opened.~%" ,filename)))))
