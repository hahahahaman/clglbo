;;; sprite-renderer.lisp

(in-package #:clglbo)

(defclass sprite-renderer ()
  ((shader
    :type shader
    :initarg :shader)
   (quad-vao
    :type unsigned-byte
    :initarg :quad-vao))
  (:default-initargs
   :shader nil))

(defmethod initialize-instance :after ((renderer sprite-renderer) &key)
  (with-slots (quad-vao) renderer
    (let ((vbo (car (gl:gen-buffers 1)))
          (verts (sequence-to-gl-array
                  ;;      Pos     Tex
                  (vector 0.0 1.0 0.0 1.0
                          1.0 0.0 1.0 0.0
                          0.0 0.0 0.0 0.0

                          0.0 1.0 0.0 1.0
                          1.0 1.0 1.0 1.0
                          1.0 0.0 1.0 0.0)
                  :float)))
      (setf quad-vao (gl:gen-vertex-array))
      (gl:bind-buffer :array-buffer vbo)
      (gl:buffer-data :array-buffer :static-draw verts)

      (gl:bind-vertex-array quad-vao)
      (gl:enable-vertex-attrib-array 0)
      (gl:vertex-attrib-pointer 0 4 :float nil (sizeof* :float 4) 0)
      (gl:bind-buffer :array-buffer 0)
      (gl:bind-vertex-array 0)

      (gl:free-gl-array verts))))

(defun sprite-render (sprite-renderer texture position
                      &optional
                        (size (kit.glm:vec2 10 10))
                        (rotate 0.0)
                        (color (kit.glm:vec3 1.0 1.0 1.0)))
  (with-slots (shader quad-vao) sprite-renderer
    (shader-use shader)
    (let ((model (kit.glm:matrix*
                  (kit.glm:translate (concat-vecs position 0.0))
                  (kit.glm:translate* (* 0.5 (aref size 0))
                                      (* 0.5 (aref size 1))
                                      0.0)
                  (kit.glm:rotate* 0.0 0.0 (cfloat rotate))
                  (kit.glm:translate* (* -0.5 (aref size 0))
                                     (* -0.5 (aref size 1))
                                     0.0)
                  (kit.glm:scale (concat-vecs size 1.0)))))
      (gl:uniform-matrix-4fv (shader-get-uniform shader "model") (vector model) nil))
    (gl:uniformfv (shader-get-uniform shader "spriteColor") color)

    (gl:active-texture :texture0)
    (texture-bind texture)

    (gl:bind-vertex-array quad-vao)
    (gl:draw-arrays :triangles 0 6)
    (gl:bind-vertex-array 0)))
