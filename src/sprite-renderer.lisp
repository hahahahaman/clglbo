;;; sprite-renderer.lisp

(in-package #:clglbo)

(defclass renderer ()
  ((program
    :type program
    :accessor program
    :initarg :program)
   (vao
    :type unsigned-byte
    :accessor vao
    :initarg :vao))
  (:default-initargs
   :program nil
   :vao (gl:gen-vertex-array)))

(defclass sprite-renderer (renderer)
  ())

(defmethod initialize-instance :after ((renderer sprite-renderer) &key)
  (with-accessors ((vao vao)) renderer
    ;; delete qua-vao after garbage collected

    (let ((vbo (car (gl:gen-buffers 1))))
      ;; use vao from renderer
      (gl:bind-vertex-array vao)

      ;; upload buffer data from vertex buffer object
      (gl:bind-buffer :array-buffer vbo)
      (with-sequence-to-gl-array (verts
                                  ;;      Pos     Tex
                                  (vector 0.0 0.0 0.0 0.0
                                          1.0 0.0 1.0 0.0
                                          0.0 1.0 0.0 1.0
                                          1.0 1.0 1.0 1.0)
                                  :float)
        (gl:buffer-data :array-buffer :static-draw verts))

      (gl:enable-vertex-attrib-array 0)
      (gl:vertex-attrib-pointer 0 4 :float nil (sizeof* :float 4) 0)
      (gl:bind-buffer :array-buffer 0)
      (gl:bind-vertex-array 0)

      ;; clean up
      ;; (gl:free-gl-array verts)
      (gl:delete-buffers (list vbo)))))

(defmethod sprite-render ((texture2d texture2d)
                          (position vector)
                          &optional
                            (size (kit.glm:vec2 10.0 10.0))
                            (color (kit.glm:vec4 1.0 1.0 1.0 1.0))
                            (rotate 0.0)
                            (renderer *sprite-renderer*))
  (with-accessors ((program program) (vao vao)) renderer
    (use program)

    (gl:uniformfv (get-uniform program "spriteColor") color)
    (let ((model (kit.glm:matrix*
                  ;;finally move to POSITION
                  (kit.glm:translate (kit.glm:vec3 (aref position 0) (aref position 1) 0.0))
                  ;; move top left to 0.0, 0.0
                  (kit.glm:translate* (cfloat (* 0.5 (aref size 0)))
                                      (cfloat (* 0.5 (aref size 1)))
                                      0.0)
                  ;; rotate around the z-axis
                  (kit.glm:rotate* 0.0 0.0 (cfloat rotate))
                  ;; move sprite so that its center is at 0.0, 0.0, 0.0
                  (kit.glm:translate* (cfloat (* -0.5 (aref size 0)))
                                      (cfloat (* -0.5 (aref size 1)))
                                      0.0)
                  ;; scale first, z axis remain constant since 2d
                  (kit.glm:scale (kit.glm:vec3 (aref size 0) (aref size 1) 0.0)))))

      ;; set model uniform
      (gl:uniform-matrix-4fv (get-uniform program "model") (vector model) nil))

    ;; bind TEXTURE2D
    (gl:active-texture :texture0)
    (bind texture2d)

    ;; draw
    (gl:bind-vertex-array vao)
    (gl:draw-arrays :triangle-strip 0 4)
    (gl:bind-vertex-array 0)))
