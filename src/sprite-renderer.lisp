;;; sprite-renderer.lisp

(in-package #:clglbo)

(defclass sprite-renderer ()
  ((program
    :type program
    :initarg :program)
   (quad-vao
    :type unsigned-byte
    :initarg :quad-vao))
  (:default-initargs
   :program nil
   :quad-vao 0))

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

      (gl:free-gl-array verts)
      (gl:delete-buffers (list vbo)))
    (trivial-garbage:finalize renderer
                     (lambda ()
                       (gl:delete-vertex-arrays (list quad-vao))))))

(defun sprite-render (sprite-renderer texture position
                      &optional
                        (size (kit.glm:vec2 1.0 1.0))
                        (rotate 0.0)
                        (color (kit.glm:vec3 1.0 1.0 1.0)))
  (with-slots (program quad-vao) sprite-renderer
    (program-use program)

    (let ((model (kit.glm:matrix*
                  (kit.glm:translate (concat-vecs position 0.0))
                  (kit.glm:translate* (cfloat (* 0.5 (aref size 0)))
                                      (cfloat (* 0.5 (aref size 1)))
                                      0.0)
                  (kit.glm:rotate* 0.0 0.0 (cfloat rotate))
                  (kit.glm:translate* (cfloat (* -0.5 (aref size 0)))
                                      (cfloat (* -0.5 (aref size 1)))
                                      0.0)
                  (kit.glm:scale (concat-vecs size 1.0)))))
      (gl:uniform-matrix-4fv (gl:get-uniform-location (id program) "model") model nil))
    (gl:uniformfv (program-get-uniform program "spriteColor") color)

    (gl:active-texture :texture0)
    (texture2d-bind texture)

    (gl:bind-vertex-array quad-vao)
    (gl:draw-arrays :triangles 0 6)
    (gl:bind-vertex-array 0)))
