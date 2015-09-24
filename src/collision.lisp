;;;; collision.lisp
;;;; some physics collision code

(in-package :clglbo)

(defstruct aabb min max)
(defstruct circle (radius 1.0) center)

(defgeneric collidep (a b)
  (:documentation "Returns true if collision between A and B, else false."))

(defmethod collidep ((a aabb) (b aabb))
  (not (or
        ;; a is too far right
        (> (x-val (aabb-min a)) (x-val (aabb-max) b))
        ;; b is too far right
        (> (x-val (aabb-min b)) (x-val (aabb-max) a))
        ;; a is too far down
        (> (y-val (aabb-min a)) (y-val (aabb-max) b))
        ;; b is too far down
        (> (y-val (aabb-min b)) (y-val (aabb-max) a)))))

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

;; pretty sure this works, but let's do something else
;; (defmethod collide-p ((a aabb) (c circle))
;;   (let* ((c-x (x-val (circle-center c)))
;;          (c-y (y-val (circle-center c)))
;;          (radius (circle-radius c))
;;          (c-aabb (make-aabb :min (vec2 (- c-x radius) (- c-y radius))))
;;          (a-w (- (x-val (aabb-max a)) (x-val (aabb-min a))))
;;          (a-h (- (y-val (aabb-max a)) (y-val (aabb-min a))))
;;          (sin-pi/4 0.707107)
;;          (a-circle (make-circle
;;                     :radius (/ a-h sin-pi/4)
;;                     :center (vec-add (aabb-min a)
;;                                      (vec2 (/ a-w 2.0) (/ a-h 2.0))))))
;;     (and (collide-p c-aabb a)
;;          (collide-p a-circle c))))

(defmethod collidep ((a aabb) (c circle))
  (let* ((half-extent (vec-add (aabb-max a) (vec-mul (aabb-min a) -1.0)))
         (a-center (vec-add (aabb-min a) half-extent))
         (clamped-dist (vec-clamp (vec-add (circle-center c) (vec-mul a-center -1.0))
                                  (vec-mul half-extent -1.0)
                                  half-extent))
         (closest-point (vec-add a-center clamped-dist))
         (dist (vec-add (circle-center c) (vec-mul closest-point -1.0)))
         (dist-length^2 (+ (square (x-val dist)) (square (y-val dist)))))
    (< dist-length^2 (square (circle-radius c)))))

(defun collide (id other-id &optional (entities *entities*))
  (cond ((get-component :ballp id entities)
         (lambda ()
           (setf *entities* (set-component :vel id (vec2 0.0 0.0)))))
        ((and (get-component :brickp id entities)
              (get-component :ballp other-id entities))
         (lambda ()
           (setf *entities* (remove-entity id entities))))))

(defun make-collision-obj (type)
  (cond ((eql type :aabb)
         (make-aabb :min a-pos :max (vec-add a-pos a-size)))
        ((eql a-collision-type :circle)
         (make-circle :center (vec-add a-pos (vec-div a-size 2.0))
                      :radius (/ (x-val a-size) 2.0)))))

(defun entity-collisions (&optional (changes *destructive-changes*) (entities *entities*))
  (let ((new-changes nil))
    (do-map (a-id a-entity entities)
      (declare (ignore a-entity))
      (let ((a-size (get-component :size a-id))
            (a-pos (get-component :pos a-id))
            (a-collision-type (get-component :collision-type a-id))))
      ;; a has right components
      (when (and a-size a-pos a-collision-type)
        ;; loop through all the other entities
        (do-map (b-id b-entity entities)
          (declare (ignore b-entity))
          (let ((b-size (get-component :size b-id))
                (b-pos (get-component :pos b-id))
                (b-collision-type (get-component :collision-type b-id)))
            ;; a and b are different and b has right components
            (when (and (not (eql a-id b-id))
                       b-size b-pos b-collision-type)
              (when (collidep (make-collision-obj a-collision-type)
                              (make-collision-obj b-collision-type))
                (let ((a-collide (collide a-id b-id entities))
                      (b-collide (collide b-id a-id entities)))
                  (when a-collide
                    (push a-collide new-changes))
                  (when b-collide
                    (push b-collide new-changes)))))))))
    (append changes (reverse new-changes))))
