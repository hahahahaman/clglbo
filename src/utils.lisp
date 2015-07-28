;;;; utils.lisp

(in-package #:clglbo)

;; system stuff
(defun update-dt ()
  (setf *dt* (- (glfw:get-time) *previous-time*)
        *previous-time* (glfw:get-time)))

(defun key-pressed-p (key)
  (not (eql (find key *keys-pressed*) nil)))

;; type utils
(defun concat-vecs (&rest vecs)
  (let* ((len 0)
        (vec (apply #'concatenate 'vector
                    (mapcar (lambda (x) (if (typep x 'sequence)
                                            (progn
                                              (incf len (length x))
                                              x)
                                            (progn
                                              (incf len 1)
                                              (list x))))
                            vecs))))
    (coerce vec `(simple-array single-float (,len)))))

(defun sequence-to-gl-array (sequence type)
  (let ((gl-arr (gl:alloc-gl-array type (length sequence))))
    (dotimes (i (length sequence) gl-arr)
      (setf (gl:glaref gl-arr i) (elt sequence i)))))

(defun cfloat (n)
  (coerce n 'single-float))

(defun sizeof (type)
  (cffi-sys:%foreign-type-size type))

(defun sizeof* (type multiply)
  (* (sizeof type) multiply))

;; file io

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

;;glfw callback
;;changes global variable

(glfw:def-key-callback key-callback (window key scancode action mod-keys)
  (declare (ignore window scancode mod-keys))
  (when (and (eq key :escape) (eq action :press))
    (glfw:set-window-should-close))
  (when (eq action :press)
    (pushnew key *keys-pressed*))
  (when (eq action :release)
    (alexandria:deletef *keys-pressed* key)))

(glfw:def-mouse-button-callback mouse-callback (window button action mod-keys)
  (declare (ignore window mod-keys))
  (if (eq action :press)
      (pushnew button *buttons-pressed*))
  (when (eq action :release)
    (alexandria:deletef *buttons-pressed* button)))

(glfw:def-cursor-pos-callback cursor-callback (window x y)
  (declare (ignore window))
  (when *first-mouse*
    (setf *last-x* x
          *last-y* y
          *first-mouse* nil))
  (setf *cursor-callback-p* t
        *cursor-x* x
        *cursor-y* y))

(glfw:def-scroll-callback scroll-callback (window x y)
  (declare (ignore window))
  ;; (process-mouse-scroll camera (cfloat y))
  (setf *scroll-callback-p* t
        *scroll-x* x
        *scroll-y* y))
