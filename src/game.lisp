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
    :reader height))
  (:default-initargs
   :state +game-active+
   :width 0
   :height 0))

(defgeneric game-process-input (game dt))
(defgeneric game-update (game dt))
(defgeneric game-render (game))
(defgeneric game-init (game))

(defmethod game-init ((game game))
  (setf *program-manager* (make-instance 'program-manager)
        *texture-manager* (make-instance 'texture-manager))
  (let ((program (make-program #p"./data/shaders/sprite.v.glsl"
                               #p"./data/shaders/sprite.f.glsl")))
    (load-resource *program-manager* "sprite" program)
    (program-use program)
    (gl:uniformi (program-get-uniform program "image") 0)
    (gl:uniform-matrix-4fv (program-get-uniform program "projection")
                           (kit.glm:ortho-matrix 0.0
                                                 (cfloat (width game))
                                                 (cfloat (height game))
                                                 0.0
                                                 -1.0 1.0)
                           nil)
    (setf *sprite-renderer* (make-instance 'sprite-renderer :program program))
    (load-resource *texture-manager* "face"
                   (make-texture "./data/images/awesomeface.png" nil))))

(defmethod game-process-input ((game game) dt)
  t)

(defmethod game-update ((game game) dt)
  t)

(defmethod game-render ((game game))
  (sprite-render *sprite-renderer* (get-resource *texture-manager* "face")
                 (kit.glm:vec2 200.0 200.0)
                 (kit.glm:vec2 300.0 400.0)
                 (kit.glm:deg-to-rad 45.0)
                 (kit.glm:vec3 1.0 1.0 0.0)))
