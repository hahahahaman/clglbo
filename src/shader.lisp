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

(defun shader-use (shader)
  (declare (type shader shader))
  (gl:use-program (id s)))

(defun shader-compile (shader vert-path frag-path &optional geo-path)
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

(defun shader-get-attrib (shader name)
  (declare (type shader shader))
  (let ((attrib (gl:get-attrib-location (id s) name)))
    (if (eql attrib -1)
        (error "Program attribute not found: ~a~%" name)
        attrib)))

(defun shader-get-uniform (shader name)
  (let ((uniform (gl:get-uniform-location (id s) name)))
    (if (eql uniform -1)
        (error "Program uniform not found: ~a~%" name)
        uniform)))
