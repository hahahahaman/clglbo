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
   (levels
    :initarg :levels
    :accessor levels)
   (current-level
    :initarg :current-level
    :accessor current-level)
   (player
    :initarg :player
    :accessor player)
   (world
    :type world
    :accessor world
    :initarg :world))
  (:default-initargs
   :state +game-active+
   :width *width*
   :height *height*
   :world (make-world)
   :levels nil
   :current-level 3))

;; (defgeneric game-process-input (game dt))
;; (defgeneric game-update (game dt))
;; (defgeneric game-render (game))
;; (defgeneric game-init (game))

(defmethod init ((game game))
  (with-accessors ((width width) (height height) (world world)
                   (levels levels) (current-level current-level)
                   (player player)) game
    (let ((program (make-program #p"./data/shaders/sprite.v.glsl"
                                 #p"./data/shaders/sprite.f.glsl")))
      (setf *program-manager* (make-instance 'program-manager)
            *texture-manager* (make-instance 'texture-manager)
            *sprite-renderer* (make-instance 'sprite-renderer :program program))
      (load-resource *program-manager* "sprite" program)

      ;; textures
      (load-resource *texture-manager*
                     "face"
                     (make-texture2d "./data/images/awesomeface.png" t))
      (load-resource *texture-manager*
                     "background"
                     (make-texture2d "./data/images/background.jpg" nil))
      (load-resource *texture-manager*
                     "block"
                     (make-texture2d "./data/images/block.png" nil))
      (load-resource *texture-manager*
                     "block-solid"
                     (make-texture2d "./data/images/block_solid.png" nil))
      (load-resource *texture-manager*
                     "paddle"
                     (make-texture2d "./data/images/paddle.png" t))

      (setf levels (list (make-level "./data/levels/one" world width (* height 0.5))
                         (make-level "./data/levels/two" world width (* height 0.5))
                         (make-level "./data/levels/three" world width (* height 0.5))
                         (make-level "./data/levels/four" world width (* height 0.5))))

      ;;use current program
      (use program)

      ;;set image uniform to texture0
      (gl:uniformi (get-uniform program "image") 0)

      ;; set projection matrix
      (gl:uniform-matrix-4fv
       (get-uniform program "projection")
       (vector
        ;; left, right, bottom, top, near, far
        (kit.glm:ortho-matrix 0.0
                              (cfloat (width game))
                              0.0
                              (cfloat (height game))
                              -1.0 1.0))
       ;; (vector (kit.math:perspective-matrix (kit.glm:deg-to-rad 45.0)
       ;;                               (cfloat (/ *width* *height*))
       ;;                               -2.1
       ;;                               100.0))
       nil))

    ;; qua related stuff
    ;; (let ((e (make-entity world))
    ;;       (pos (make-position-component :vec (kit.glm:vec2 100.0 200.0)))
    ;;       (size (make-size-component :vec (kit.glm:vec2 200.0 300.0)))
    ;;       (render (make-render-component :sprite (get-resource *texture-manager* "face")
    ;;                                      :color (kit.glm:vec4 1.0 1.0 1.0 1.0)
    ;;                                      :rotation (kit.glm:deg-to-rad 40.0)))
    ;;       (render-sys (make-instance 'render-system)))
    ;;   (add-components world e pos size render)
    ;;   (add-systems world render-sys)
    ;;   (system-add-entities world render-sys e))
    (load-level (elt levels current-level))
    (let* ((render-sys (make-instance 'render-system))
           (e (make-entity world))
           (pos (make-position-component :vec (vec2 100.0 10.0)))
           (size (make-size-component :vec (vec2 200.0 100.0)))
           (rend (make-render-component :sprite (get-resource *texture-manager* "paddle")
                                        :color (vec4 1.0 1.0 1.0 1.0)
                                        :rotation 0.0))
           (phy (make-physics-component :acceleration (vec2 0.0 0.0) :velocity (vec2 0.0 0.0)
                                        :collision-type :aabb)))
      (setf player (make-instance 'object
                                  :id e
                                  :physics-component phy
                                  :render-component rend
                                  :size-component size
                                  :position-component pos))
      (add-components world e pos size rend phy)
      (add-systems world render-sys)
      (system-add-entities world render-sys e))
    (initialize-systems world)))

(defmethod handle-input ((game game))
  ;; keys
  (cond ((key-action-p :escape :press)
         (glfw:set-window-should-close)))

  ;; mouse buttons
  (cond ((mouse-button-action-p :left :press)
         (setf *paused* (not *paused*))))

  (with-accessors ((player player)) game
    (when (key-down-p :a)
      (decf (aref (position-component-vec (position-component player)) 0) (cfloat (* 400.0 *dt*))))
    (when (key-down-p :d)
      (incf (aref (position-component-vec (position-component player)) 0) (cfloat (* 400.0 *dt*))))))

(defun blah (name)
  (get-resource *texture-manager* name))

(defmethod update ((game game))
  ;; (let ((pos-comp (entity-component (world game) 0 'position-component))
  ;;       (size-comp (entity-component (world game) 0 'size-component))
  ;;       (rend-comp (entity-component (world game) 0 'render-component)))
  ;;   (setf (position-component-vec pos-comp)
  ;;         (kit.glm:vec2 (cfloat (+ 300 (* 200 (sin (glfw:get-time)))))
  ;;                       (cfloat (+ 200 (* 200 (cos (glfw:get-time))))))
  ;;         ;; (kit.glm:vec2 100.0 100.0)
  ;;         (size-component-vec size-comp) (kit.glm:vec2 100.0 100.0)
  ;;         (render-component-color rend-comp) (kit.glm:vec4 0.0 0.0 1.0 1.0)))

  (when (not *paused*)
    (gl:clear-color 0.0 0.0 0.0 1.0)
    ;; (gl:clear :color-buffer-bit :depth-buffer-bit)
    (gl:clear :color-buffer-bit)

    (sprite-render *sprite-renderer* (get-resource *texture-manager* "background")
                   (kit.glm:vec2 0.0 0.0)
                   (kit.glm:vec2 (cfloat (width game)) (cfloat (height game)))
                   0.0
                   (kit.glm:vec4 1.0 1.0 1.0 1.0))

    ;; (sprite-render *sprite-renderer* (blah "block-solid")
    ;;                (kit.glm:vec2 100.0 100.0)
    ;;                (kit.glm:vec2 100.0 100.0)
    ;;                (kit.glm:deg-to-rad 00.0)
    ;;                (kit.glm:vec4 1.0 1.0 1.0 1.0))

    (update-world (world game) *dt*)
    ))

(defmethod render ((game game))
  )
