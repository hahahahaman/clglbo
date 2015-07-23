;;;; shader.lisp

(in-package #:clglbo)

(defclass shader ()
  ((id
    :type unsigned-byte
    :accessor id)))
#|
(defgeneric use (SHADER))
(defgeneric compile (SHADER))
(defgeneric get-attrib (SHADER NAME))
(defgeneric get-uniform (SHADER NAME))
|#

(defun use (shader)
  (declare (type shader shader))
  (gl:use-program (id s)))

(defun check-compile-errors (obj &optional shader-p)
  "Checks Shader Program errors."
  (declare (type unsigned-byte obj) (type boolean shader-p))
  (let ((status (cffi:foreign-alloc :int)))
    (if shader-p
        (progn
          (%gl:get-shader-iv obj :compile-status status)
          ;; 0 means failure
          (when (eql (cffi:mem-ref status :int) 0)
            (error "Shader compile-time error: ~a~%" (gl:get-shader-info-log obj))))
        (progn
          (%gl:get-program-iv obj :link-status status)
          (when (eql (cffi:mem-ref status :int) 0)
            (error "Program link-time error: ~a~%" (gl:get-program-info-log obj)))))
    (cffi:foreign-free status)))

(defun load-shader-file (filepath shader-type)
  (let ((shader (gl:create-shader shader-type))
        (code (read-entire-file filepath))
        (status (cffi:foreign-alloc :init)))
    (gl:shader-source shader code)
    (gl:compile-shader shader)
    (check-compile-errors shader t)))

(defun compile (shader vert-path frag-path &optional geo-path)
  (declare (type shader shader)
           (type string vert-path frag-path))
  (let ((vert (load-shader-file vert-path :vertex-shader))
        (frag (load-shader-file frag-path :fragment-shader))
        (geo (if geo-path
                 (load-shader-file geo-path :geometry-shader)
                 nil)))
    (setf (id shader) (gl:create-program))
    (gl:attach-shader (id shader) vert)
    (gl:attach-shader (id shader) frag)
    (when geo-path
      (gl:attach-shader (id shader) geo))
    (gl:link-program (id shader))
    (check-compile-errors (id shader))
    (gl:delete-shader vert)
    (gl:delete-shader frag)
    (when geo-path
      (gl:delete-shader geo))))

(defun get-attrib (shader name)
  (declare (type shader shader))
  (let ((attrib (gl:get-attrib-location (id s) name)))
    (if (eql attrib -1)
        (error "Program attribute not found: ~a~%" name)
        attrib)))

(defun get-uniform (shader name)
  (let ((uniform (gl:get-uniform-location (id s) name)))
    (if (eql uniform -1)
        (error "Program uniform not found: ~a~%" name)
        uniform)))
