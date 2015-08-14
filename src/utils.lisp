;;;; utils.lisp

;;;; various utility functions

(in-package #:clglbo)

;;; global system specific
(defun clear-actions ()
  "Clears the input actions from last frame."
  (setf *key-actions* nil
        *mouse-button-actions* nil))
(defun update-dt ()
  "Called in the main loop, updates *dt* and *previous-time*"
  (setf *dt* (- (glfw:get-time) *previous-time*)
        *previous-time* (glfw:get-time)))
(defun update-globals ()
  "A single function that encompasses global updates"
  (clear-actions)
  (update-dt))

(defun key-action-p (key action)
  "Returns true if KEY is in *key-actions* and its state is EQ to ACTION."
  (let ((state (getf *key-actions* key)))
    (and (not (null state)) ;; if STATE is not null then key must have been found
         (eq action state))))

(defun mouse-button-action-p (button action)
  "Returns true if KEY is in *mouse-button-actions* and its state is EQ to ACTION."
  (let ((state (getf *mouse-button-actions* button)))
    (and (not (null state)) ;; if STATE is not null then button be active
         (eq action state))))

;;; type utils
(defun concat-vecs (&rest vecs)
  "Creates of single-float simple-array from lone values, lists, and/or vectors."
  (let* ((len 0) ;; keeps track of simple-array length
         (vec (apply #'concatenate ;; combine all sequences, lists and vectors
                     'vector ;; output a vector

                     ;; ensure all of the elements of VECS are of type sequence
                     (mapcar
                      (lambda (x)
                        (cond ((typep x 'sequence)
                               (incf len (length x))
                               x) ;; keep track of length
                              (t
                               (incf len 1)
                               (list (coerce x 'single-float)))))
                      vecs))))
    ;; finally output simple-array
    (coerce vec `(simple-array single-float (,len)))))

(defun sequence-to-gl-array (sequence type)
  "Creates a gl-array from a lisp sequence, with elements of type TYPE.
Remember to free gl-array afterwards."
  (let ((gl-arr (gl:alloc-gl-array type (length sequence))))
    (dotimes (i (length sequence)
                gl-arr)
      (setf (gl:glaref gl-arr i) (elt sequence i)))))

(defmacro with-sequence-to-gl-array ((var sequence type) &body body)
  `(let ((,var (sequence-to-gl-array ,sequence ,type)))
     ,@body
     (gl:free-gl-array ,var)))

(defun cfloat (n)
  "Coerce N to single-float. Just makes the function shorter."
  (coerce n 'single-float))

(defun sizeof (type)
  "Gives to foreign-type-size of TYPE. Used with cffi stuff, like opengl."
  (cffi-sys:%foreign-type-size type))

(defun sizeof* (type multiple)
  "Multiply sizeof TYPE, by MULTIPLE"
  (* (sizeof type) multiple))

;;; file io

(defun read-sexp-from-file (filename)
  "Reads all sexp from file FILENAME, returning a list of all collected."
  (with-open-file (file filename
                        :direction :input
                        :if-does-not-exist
                        (error "Unable to find file ~a.~%" file))
    (with-standard-io-syntax
      (let ((*read-eval* nil))
        (iter (for sexp = (read file nil))
              (while sexp)
              (collect sexp))))))

(defun read-entire-file (filename)
  "Returns a string with the entire content of a FILENAME, including whitespaces."
  (with-open-file (file filename :direction :input)
    (if file
        (let ((str ""))
          (iter
            (for line = (read-line file nil nil))
            (while line)
            (setf str (concatenate 'string str (format nil "~a~%" line))))
          str)
        (error "Unable to open file ~a.~%" file))))
