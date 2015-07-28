;;;; shader.lisp

(in-package #:clglbo)

(defclass program ()
  ((id
    :type %gl:uint
    :accessor id)))
#|
(defgeneric use (SHADER))
(defgeneric compile (SHADER))
(defgeneric get-attrib (SHADER NAME))
(defgeneric get-uniform (SHADER NAME))
|#

(defun load-shader-file (filepath shader-type)
  (let ((shader (gl:create-shader shader-type))
        (code (read-entire-file filepath))
        (status (cffi:foreign-alloc :int)))
    (gl:shader-source shader code)
    (gl:compile-shader shader)

    (%gl:get-shader-iv shader :compile-status status)
    ;; (format t "compile-status: ~a~%"(cffi:mem-ref status :int))

    ;; 1 means success
    (if (not (= (cffi:mem-ref status :int) 1))
        (progn
          (cffi:foreign-free status)
          (print (gl:get-shader-info-log shader))

          (gl:delete-shader shader)
          (setf shader 0)

          (error "Shader compile-time error, \"~a\"~% error code: ~a~%" filepath
                 (cffi:mem-ref status :int)))
        (progn
          (cffi:foreign-free status)
          shader))))

(defmethod initialize-instance ((program program) &key)
  (setf (id program) (gl:create-program))
  ;; (format t "gl:create-program, id : ~a~%" (id program)))
   )

(defun program-use (program)
  (gl:use-program (id program)))

(defun program-compile (program vert-path frag-path &optional (geo-path nil))
  (with-slots (id) program
    (let ((vert (load-shader-file vert-path :vertex-shader))
          (frag (load-shader-file frag-path :fragment-shader))
          (geo (if geo-path
                   (load-shader-file geo-path :geometry-shader)
                   nil)))

      (gl:attach-shader id vert)
      (gl:attach-shader id frag)
      (if geo (gl:attach-shader id geo))

      (gl:link-program id)

      ;; no longer need shaders after link
      (gl:detach-shader id vert)
      (gl:detach-shader id frag)
      (if geo (gl:detach-shader id geo))

      ;; free up shaders so no leak
      (gl:delete-shader vert)
      (gl:delete-shader frag)
      (if geo (gl:delete-shader geo))

      ;; error handling for program
      (let ((status (cffi:foreign-alloc :int)))
        (%gl:get-program-iv id :link-status status)
        ;; (format t "link-status: ~a~%"(cffi:mem-ref status :int))

        (when (not (= (cffi:mem-ref status :int) 1))
          (cffi:foreign-free status)

          (print (gl:get-program-info-log id))
          ;;program no longer needed
          ;;detaches all shaders
          ;;(gl:delete-program id)

          (error "Program link-time error: ~a~%" (gl:get-program-info-log id)))

        (cffi:foreign-free status)))))

(defun program-get-attrib (program name)
  (let ((attrib (gl:get-attrib-location (id program) name)))
    (if (eql attrib -1)
        (error "Program attribute not found: ~a~%" name)
        attrib)))

(defun program-get-uniform (program name)
  (declare (program program) (string name))
  (let ((uniform (gl:get-uniform-location (id program) name)))
    (if (eql uniform -1)
        (error "Program uniform not found: ~a~%" name)
        uniform)))

(defun make-program (vert-path frag-path &optional (geo-path nil))
  (let ((program (make-instance 'program)))
    (program-compile program vert-path frag-path geo-path)
    program))
