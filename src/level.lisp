(in-package #:clglbo)

(defclass level ()
  ((bricks
    :accessor bricks
    :initarg :bricks))
  (:default-initargs
   :bricks (make-array 0 :fill-pointer 0 :adjustable 0)))

;; (defmethod load-level ((level level))
;;   (with-slots (bricks world) level
;;     (iter (for brick in-vector bricks)
;;       (setf (id brick) (make-entity world))
;;       (with-accessors ((pos position-component)
;;                        (size size-component)
;;                        (phy physics-component)
;;                        (rend render-component)
;;                        (state state-component)) brick
;;         (add-components world (id brick) pos size phy rend state)))))

;; (defmacro make-level (filename world level-width level-height)
;;   (alexandria:once-only (filename world level-width level-height)
;;     `(with-open-file (file ,filename :direction :input)
;;        (if file
;;            (let* ((level (make-instance 'level :world ,world))
;;                   (file-data (read file))
;;                   (width (getf file-data :width))
;;                   (height (getf file-data :height))
;;                   (data (getf file-data :data))
;;                   (unit-width (cfloat (/ ,level-width width)))
;;                   (unit-height (cfloat (/ ,level-height height))))
;;              (iter (for i from 0 below height)
;;                (iter (for j from 0 below width)
;;                  (let ((brick (elt data (+ (* i width) j))))
;;                    (when (> brick 0)
;;                      (let ((pos-comp (make-position-component
;;                                       :vec (kit.glm:vec2 (* unit-width j)
;;                                                          (- *height* (* unit-height (1+ i))))))
;;                            (size-comp (make-size-component
;;                                        :vec (kit.glm:vec2 unit-width unit-height)))
;;                            (rend-comp (make-render-component
;;                                        :sprite (cond ((= brick 1)
;;                                                       (get-resource *texture-manager* "block-solid"))
;;                                                      (t
;;                                                       (get-resource *texture-manager* "block")))
;;                                        :color (case brick
;;                                                 (1 (kit.glm:vec4 0.8 0.8 0.7 1.0))
;;                                                 (2 (kit.glm:vec4 0.2 0.6 1.0 1.0))
;;                                                 (3 (kit.glm:vec4 0.0 0.7 0.0 1.0))
;;                                                 (4 (kit.glm:vec4 0.8 0.8 0.4 1.0))
;;                                                 (5 (kit.glm:vec4 1.0 0.5 0.0 1.0)))
;;                                        :rotation 0.0))
;;                            (phy-comp (make-physics-component
;;                                       :acceleration 0.0
;;                                       :velocity 0.0
;;                                       :collision-type :aabb))
;;                            (state-comp (make-state-component :solid-p (= brick 1))))
;;                        (vector-push-extend (make-instance 'object
;;                                                           :id -1
;;                                                           :position-component pos-comp
;;                                                           :size-component size-comp
;;                                                           :render-component rend-comp
;;                                                           :physics-component phy-comp
;;                                                           :state-component state-comp)
;;                                            (bricks level)))))))
;;              level)
;;            (error "~a could not be opened.~%" ,filename)))))

(defun make-level (filename level-width level-height)
  (with-open-file (file filename)
    (cond (file
           (let* ((level (make-instance 'level))
                  (file-data (read file))
                  (width (getf file-data :width))
                  (height (getf file-data :height))
                  (data (getf file-data :data))
                  (unit-width (cfloat (/ level-width width)))
                  (unit-height (cfloat (/ level-height height))))
             (iter (for i from 0 below height)
               (iter (for j from 0 below width)
                 (let ((brick (elt data (+ (* i width) j))))
                   (when (> brick 0)
                     (with-slots (bricks) level
                       (vector-push-extend
                        (map (:pos (vec2 (* unit-width j) (* unit-height i)))
                             (:size (vec2 unit-width unit-height))
                             (:texture (cond ((= brick 1)
                                              (get-texture "block-solid"))
                                             (t (get-texture "block"))))
                             (:color (case brick
                                       (1 (kit.glm:vec4 0.8 0.8 0.7 1.0))
                                       (2 (kit.glm:vec4 0.2 0.6 1.0 1.0))
                                       (3 (kit.glm:vec4 0.0 0.7 0.0 1.0))
                                       (4 (kit.glm:vec4 0.8 0.8 0.4 1.0))
                                       (5 (kit.glm:vec4 1.0 0.5 0.0 1.0))))
                             (:rotation 0.0)
                             (:brick-solid-p (= brick 1)))
                        bricks))))))
             level))
          (t (error "~a could not be opened.~%" filename)))))

(defun load-level (level &optional (width *width*) (height *height*) (entities *entities*))
  (with-slots (bricks) level
    ;; background
    (flet ((add-entity (components)
             (setf entities (make-entity components entities))))

      (add-entity (map (:pos (vec2 0.0 0.0))
                       (:texture (get-texture "background"))
                       (:size (vec2 (cfloat width) (cfloat height)))
                       (:color (vec4 1.0 1.0 1.0 1.0))
                       (:rotation 0.0)))

      ;; level bricks
      (iter (for brick in-vector bricks) (add-entity brick))

      (let* ((w 120.0)
             (h 25.0)
             (r 30.0)
             (ppos (vec2 (/ (- *width* w) 2.0) (- *height* h 10.0)))
             (bpos (vec2 (/ (- *width* r) 2.0) (- (aref ppos 1) r 10.0))))
        ;; paddle
        (add-entity (map (:pos ppos)
                         (:size (vec2 w h))
                         (:texture (get-texture "paddle"))
                         (:color (vec4 1.0 1.0 1.0 1.0))
                         (:rotation 0.0)
                         (:vel (vec2 0.0 0.0))
                         (:player t)))
        ;; ball
        (add-entity (map (:pos bpos)
                         (:size (vec2 r r))
                         (:texture (get-texture "face"))
                         (:color (vec4 1.0 1.0 1.0 1.0))
                         (:rotation 0.0)
                         (:vel (vec2 0.0 0.0))
                         (:follow-player t)
                         (:init-vel (vec2 100.0 -350.0))
                         (:ball t))))
      entities)))

(defun level-complete-p (&optional (entities *entities*))
  (= 0 (length
        (find-entities
         (lambda (id comps)
           (declare (ignore comps))
           (let ((brickp (nth-value 1 (get-component :brick-solid-p id)))
                 (solidp (get-component :brick-solid-p id)))
             (and brickp (not solidp))))
         entities))))


