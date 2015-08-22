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
   :current-level 0))

;; (defgeneric game-process-input (game dt))
;; (defgeneric game-update (game dt))
;; (defgeneric game-render (game))
;; (defgeneric game-init (game))

(defmethod init ((game game))
  (with-accessors ((width width) (height height) (world world)
                   (levels levels) (current-level current-level)) game
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
      (setf levels (list (make-level "./data/levels/one" world width (* height 0.5))
                         (make-level "./data/levels/two" world width (* height 0.5))
                         (make-level "./data/levels/three" world width (* height 0.5))
                         (make-level "./data/levels/four" world width (* height 0.5))))
      ;; (print (foo*))

      ;; (print levels)
      ;;use current program
      (use program)

      ;;set image uniform to texture0
      (gl:uniformi (get-uniform program "image") 0)

      ;; set projection matrix
      (gl:uniform-matrix-4fv (get-uniform program "projection")
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
    (let ((render-sys (make-instance 'render-system)))
      (add-systems world render-sys))
    (initialize-systems world)))

(defmethod handle-input ((game game))
  ;; keys
  (cond ((key-action-p :escape :press)
         (glfw:set-window-should-close)))

  ;; mouse buttons
  (cond ((mouse-button-action-p :left :press)
         (setf *paused* (not *paused*)))))

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
    (gl:clear :color-buffer-bit :depth-buffer-bit)

    (sprite-render *sprite-renderer* (get-resource *texture-manager* "block-solid")
                   (kit.glm:vec2 (random 800.0) (random 600.0))
                   (kit.glm:vec2 100.0 100.0)
                   0.0
                   (kit.glm:vec4 1.0 1.0 1.0 1.0))

    ;; update qua world
    (update-world (world game) *dt*)

    ;; background
    ;; (sprite-render *sprite-renderer* (get-resource *texture-manager* "background")
    ;;                (kit.glm:vec2 0.0 0.0)
    ;;                (kit.glm:vec2 (cfloat (width game)) (cfloat (height game)))
    ;;                0.0
    ;;                (kit.glm:vec4 1.0 1.0 1.0 1.0))
    ))

(defmethod render ((game game))
  )

;; (eval-when (:compile-toplevel)
;;   (print "compile-toplevel"))
;; (eval-when (:load-toplevel)
;;   (print "load-toplevel"))
;; (eval-when (:execute)
;;   (print "execute"))
