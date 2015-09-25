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
;;                     :center (vec2-add (aabb-min a)
;;                                       (vec2 (/ a-w 2.0) (/ a-h 2.0))))))
;;     (and (collide-p c-aabb a)
;;          (collide-p a-circle c))))

(defmethod collidep ((a aabb) (c circle))
  (let* ((half-extent (vec2-add (aabb-max a) (vec2-mul (aabb-min a) -1.0)))
         (a-center (vec2-add (aabb-min a) half-extent))
         (clamped-dist (vec-clamp (vec2-add (circle-center c) (vec2-mul a-center -1.0))
                                  (vec2-mul half-extent -1.0)
                                  half-extent))
         (closest-point (vec2-add a-center clamped-dist))
         (dist (vec2-add (circle-center c) (vec2-mul closest-point -1.0)))
         (dist-length^2 (+ (square (x-val dist)) (square (y-val dist)))))
    (< dist-length^2 (square (circle-radius c)))))
(defmethod collidep ((c circle) (a aabb))
  (collidep a c))

(defun collide (id other-id &optional (entities *entities*))
  (cond ((get-component :ballp id entities)
         (lambda ()
           (setf *entities* (set-component :vel id (vec2 0.0 0.0)))))
        ((and (get-component :brickp id entities)
              (get-component :ballp other-id entities))
         (lambda ()
           (setf *entities* (remove-entity id entities))))))

(defun make-collision-obj (id type &optional (entities *entities*))
  (let ((pos (get-component :pos id entities))
        (size (get-component :size id entities)))
    (cond ((eql type :aabb)
           (make-aabb :min pos :max (vec2-add pos size)))
          ((eql type :circle)
           (make-circle :center (vec2-add pos (vec2-div size 2.0))
                        :radius (/ (the single-float (x-val size)) 2.0)))
          (t (error "Invalid collision type: ~a" type)))))

(defun entity-collisions (&optional (changes *destructive-changes*) (entities *entities*))
  ;; (declare (optimize (speed 3) (safety 0)))
  (let ((new-changes nil)
        (balls (find-entities (lambda (x y) (declare (ignore y))
                                (get-component :ballp x entities))
                              entities))
        (bricks (find-entities (lambda (x y) (declare (ignore y))
                                 (get-component :brickp x entities))
                               entities)))
    (iter (for (the fixnum a-id) in balls)
      (let* ((a-size (the simple-array (get-component :size a-id)))
             (a-pos (the simple-array (get-component :pos a-id)))
             (a-collision-type (get-component :collision-type a-id)))
        (when (and a-size a-pos a-collision-type)
          (iter (for (the fixnum b-id) in bricks) 
            (let* ((b-size (the simple-array (get-component :size b-id)))
                   (b-pos (the simple-array (get-component :pos b-id)))
                   (b-collision-type (get-component :collision-type b-id)))
              ;; a and b are different and b has right components
              (when (and b-size b-pos b-collision-type)
                (when (collidep (make-collision-obj a-id a-collision-type)
                                (make-collision-obj b-id b-collision-type))
                  (let ((a-collide (collide a-id b-id entities))
                        (b-collide (collide b-id a-id entities)))
                    (when a-collide
                      (push a-collide new-changes))
                    (when b-collide
                      (push b-collide new-changes))))))))))
    (append changes (reverse new-changes))))
