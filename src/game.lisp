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

(defmethod initialize-instance :after ((game game) &key)
  (setf *shader-manager* (make-instance 'shader-manager)
        *texture-manager* (make-instance 'texture-manager))
  (let ((shader (make-shader "data/shaders/sprite.v.glsl"
                             "data/shaders/sprite.f.glsl")))
    (load-resource *shader-manager* "sprite" shader)
    (shader-use shader)
    (gl:uniformi (shader-get-uniform shader "image") 0)
    (gl:uniform-matrix-4fv (shader-get-uniform shader "projection")
                           (kit.glm:ortho-matrix 0.0
                                                 (cfloat (width game))
                                                 (cfloat (height game))
                                                 0.0 -1.0 1.0))
    (setf *sprite-renderer* (make-instance 'sprite-renderer :shader shader))
    (load-resource *texture-manager* "face"
                   (make-texture "data/images/awesomeface.png" t))))

(defmethod game-process-input ((game game) dt)
  t)

(defmethod game-update ((game game) dt)
  t)

(defmethod game-render ((game game))
  (sprite-render *sprite-renderer* (get-resource *texture-manager* "face")
                 (kit.glm:vec2 200.0 200.0)
                 (kit.glm:vec2 300.0 400.0)
                 (kit.glm:deg-to-rad 45.0)
                 (kit.glm:vec3 0.0 1.0 0.0)))
