;;;; utils.lisp

(in-package #:clglbo)

(defun read-entire-file (file)
  "Returns a string with the text content of a file."
  (let ((in (open file :if-does-not-exist nil))
        (str ""))
    (when in
      (loop for line = (read-line in nil)
            while line do (setf str (concatenate 'string str (format nil "~a~%" line))))
      (close in)
      (return-from read-entire-file str)))
  (error "Unable to find file ~a.~%" file))

(defun sequence-to-gl-array (sequence type)
  (let ((gl-arr (gl:alloc-gl-array type (length sequence))))
    (dotimes (i (length sequence) gl-arr)
      (setf (gl:glaref gl-arr i) (elt sequence i)))))

(defun cfloat (n)
  (coerce n 'single-float))

(defun sizeof (type)
  (cffi-sys:%foreign-type-size type))
