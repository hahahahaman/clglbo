;;;; collision.lisp
;;;; some physics collision code

(in-package :clglbo)

(defstruct aabb min max)
(defstruct circle (radius 1.0 :type single-float) center)

(defgeneric collide-p (a b)
  (:documentation "Returns true if collision between A and B, else false."))

(defmethod collide-p ((a aabb) (b aabb))
  (flet ((x (vec) (aref vec 0))
         (y (vec) (aref vec 1)))
    (not (or
          ;; a is too far right
          (> (x (aabb-min a)) (x (aabb-max) b))
          ;; b is too far right
          (> (x (aabb-min b)) (x (aabb-max) a))
          ;; a is too far up
          (> (y (aabb-min a)) (y (aabb-max) b))
          ;; b is too far up
          (> (y (aabb-min b)) (y (aabb-max) a))))))

(defmethod collide-p ((a circle) (b circle))
  (let ((r (+ (circle-radius a) (circle-radius b)))
        (dist^2 (+ (expt (- (aref (circle-center b) 0)
                            (aref (circle-center a) 0)) 2)
                   (expt (- (aref (circle-center b) 1)
                            (aref (circle-center a) 1)) 2))))
    ;; if the distance between the circles is less than the sum of their radii
    ;; then there is a collision
    ;; optimized version to use squares of the values
    (> (* r r) dist^2)))
