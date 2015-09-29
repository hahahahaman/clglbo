;;;; collision.lisp
;;;; some physics collision code

(in-package :clglbo)

(defstruct aabb min max)
(defstruct circle (radius 1.0) center)

(defgeneric collidep (a b)
  (:documentation "Returns true if collision between A and B, else false."))

(defmethod collidep ((a aabb) (b aabb))
  ;; (declare (optimize (speed 3) (safety 0)))
  (not (or
        ;; a is too far right
        (> (x-val (aabb-min a)) (x-val (aabb-max b)))
        ;; b is too far right
        (> (x-val (aabb-min b)) (x-val (aabb-max a)))
        ;; a is too far down
        (> (y-val (aabb-min a)) (y-val (aabb-max b)))
        ;; b is too far down
        (> (y-val (aabb-min b)) (y-val (aabb-max a))))))

(defmethod collidep ((a circle) (b circle))
  (let ((r (+ (circle-radius a) (circle-radius b)))
        (dist^2 (+ (square (- (x-val (circle-center b))
                              (x-val (circle-center a))))
                   (square (- (y-val (circle-center b))
                              (y-val (circle-center a)))))))
    ;; if the distance between the circles is less than the sum of their radii
    ;; then there is a collision
    ;; optimized version to use squares of the values
    (> (* r r) dist^2)))

;; (defmethod collidep ((a aabb) (c circle))
;;   (let* ((cx (x-val (circle-center c)))
;;          (cy (y-val (circle-center c)))
;;          (radius (circle-radius c))
;;          (c-aabb (make-aabb :min (vec2 (- cx radius) (- cy radius))
;;                             :max (vec2 (+ cx radius) (+ cy radius))))
;;          (aw/2 (/ (- (x-val (aabb-max a)) (x-val (aabb-min a))) 2.0))
;;          (ah/2 (/ (- (y-val (aabb-max a)) (y-val (aabb-min a))) 2.0))
;;          (sin-pi/4 0.707107)
;;          (a-circle (make-circle
;;                     :radius (/ ah/2 sin-pi/4)
;;                     :center (vec2-add (aabb-min a)
;;                                       (vec2 aw/2 ah/2)))))
;;     (and (collidep c-aabb a)
;;          (collidep a-circle c))))

(defmethod collidep ((a aabb) (c circle))
  (let* ((half-extent (vec2-div (vec2-sub (aabb-max a) (aabb-min a)) 2.0))
         (a-center (vec2-add (aabb-min a) half-extent))
         (clamped-dist (vec2-clamp (vec2-sub (circle-center c) a-center)
                                   (vec2-mul half-extent -1.0)
                                   half-extent))
         (closest-point (vec2-add a-center clamped-dist))
         (dist (vec2-add (circle-center c) (vec2-mul closest-point -1.0)))
         (dist-length^2 (+ (square (x-val dist)) (square (y-val dist)))))
    (< dist-length^2 (square (circle-radius c)))))
(defmethod collidep ((c circle) (a aabb))
  (collidep a c))

;; (defun collide (id other-id &optional (entities *entities*))
;;   (cond ((get-component :ballp id entities)
;;          (lambda ()
;;            (setf *entities* (set-component :vel id (vec2 0.0 0.0)))))
;;         ((and (get-component :brickp id entities)
;;               (get-component :ballp other-id entities))
;;          (lambda ()
;;            (setf *entities* (remove-entity id entities))))))

;; (defun collide (ball-id block-id &optional (entities *entities*))
;;   (let ((result
;;           (list (lambda ()
;;                   (setf *entities*
;;                         (set-component :vel ball-id (vec2 0.0 0.0)))))))
;;     (when (not (get-component :brick-solid-p block-id))
;;       (alexandria:appendf
;;        result
;;        (list (lambda ()
;;                (setf *entities* (remove-entity block-id entities))))))
;;     result))
(defun collide (id other-id &optional (entities *entities*))
  (let ((this (get-entity id entities))
        (other (get-entity other-id entities)))
    (cond ((get-entity-component :playerp this)
           (lambda ()
             (setf *entities* (set-component :vel id (vec2 0.0 0.0)))))
          ((and (get-entity-component :brickp this)
                (not (get-entity-component :brick-solidp this))
                (get-entity-component :ballp other))
           (lambda ()
             (setf *entities* (remove-entity id))))
          (t (lambda ())))))

(defun make-collision-obj (type &optional (pos (vec2 0.0 0.0)) (size (vec2 1.0 2.0)))
  (cond ((eql type :aabb)
         (make-aabb :min pos :max (vec2-add pos size)))
        ((eql type :circle)
         (make-circle :center (vec2-add pos (vec2-div size 2.0))
                      :radius (/ (the single-float (x-val size)) 2.0)))
        (t (error "Invalid collision type: ~a" type))))

