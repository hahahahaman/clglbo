;;; game.lisp

(in-package #:clglbo)

(defenum:defenum *game-state*
                 ((+game-active+ 0)
                  +game-menu+
                  +game-win+))

(defclass game ()
  ((state
    :type (unsigned-byte 32)
    :initarg :state)
   (width
    :initarg :width
    :reader width)
   (height
    :initarg :height
    :reader height)
   (world
    :type world
    :accessor world
    :initarg :world))
  (:default-initargs
   :state +game-active+
   :width *width*
   :height *height*
   :world (make-world)))

;; (defgeneric game-process-input (game dt))
;; (defgeneric game-update (game dt))
;; (defgeneric game-render (game))
;; (defgeneric game-init (game))

(defmethod init ((game game))
  (with-accessors ((width width) (height height) (world world)) game
    (let ((program (make-program #p"./data/shaders/sprite.v.glsl"
                                 #p"./data/shaders/sprite.f.glsl")))
      (setf *program-manager* (make-instance 'program-manager)
            *texture-manager* (make-instance 'texture-manager)
            *sprite-renderer* (make-instance 'sprite-renderer :program program))
      (load-resource *program-manager* "sprite" program)
      (load-resource *texture-manager*
                     "face"
                     (make-texture2d "./data/images/awesomeface.png" t))

      ;;use current program
      (use program)

      ;;set image uniform to texture0
      (gl:uniformi (get-uniform program "image") 0)

      ;; set projection matrix
      (gl:uniform-matrix-4fv (get-uniform program "projection")
                             (vector (kit.glm:ortho-matrix 0.0
                                                           (cfloat (width game))
                                                           (cfloat (height game))
                                                           0.0
                                                           -1.0 1.0))
                             ;; (vector (kit.math:perspective-matrix (kit.glm:deg-to-rad 45.0)
                             ;;                               (cfloat (/ *width* *height*))
                             ;;                               -2.1
                             ;;                               100.0))
                             nil))

    ;; qua relate stuff
    (let ((e (make-entity world))
          (pos (make-position-component :vec (kit.glm:vec2 100.0 200.0)))
          (render (make-render-component :sprite (get-resource *texture-manager* "face")
                                         :color (kit.glm:vec4 1.0 1.0 1.0 1.0)
                                         :rotation (kit.glm:deg-to-rad 40.0)
                                         :size (kit.glm:vec2 200.0 300.0)))
          (render-sys (make-instance 'render-system)))
      (add-components world e pos render)
      (add-systems world render-sys)
      (system-add-entities world render-sys e))))

(defmethod process-input ((game game))
  t)

(defmethod update ((game game))
  (let ((pos-comp (entity-component (world game) 0 'position-component))
        (rend-comp (entity-component (world game) 0 'render-component)))
    (setf (position-component-vec pos-comp)
          (kit.glm:vec2 (cfloat (+ 300 (* 200 (sin (glfw:get-time)))))
                        (cfloat (+ 200 (* 200 (cos (glfw:get-time))))))
          ;; (kit.glm:vec2 100.0 100.0)
          (render-component-size rend-comp) (kit.glm:vec2 100.0 100.0)
          (render-component-color rend-comp) (kit.glm:vec4 0.0 0.0 1.0 1.0)))
  (update-world (world game) *dt*))

(defmethod render ((game game))
  (gl:clear-color 0.0 0.0 0.0 1.0)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  ;; (sprite-render *sprite-renderer*
  ;;                (test-get-resource)
  ;;                (kit.glm:vec2 100.0 400.0)
  ;;                (kit.glm:vec2 200.0 300.0)
  ;;                (kit.glm:deg-to-rad 40.0)
  ;;                (kit.glm:vec3 1.0 1.0 0.0))
  )

(defun test-get-resource ()
  (get-resource *texture-manager* "face"))
