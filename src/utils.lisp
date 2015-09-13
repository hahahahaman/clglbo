;;;; utils.lisp

;;;; various utility functions

(in-package #:clglbo)

(let* ((max-samples 1000)
       (samples (make-array max-samples
                            :element-type 'double-float
                            :initial-element (/ 1.0d0 +max-fps+)))
       (sample-index 0))
  (defun average-fps ()
    (setf (aref samples sample-index) *dt*
          sample-index (mod (1+ sample-index) max-samples))
    ;; (print *dt*)
    (/ max-samples (reduce #'+ samples))))

;;; global system specific
(defun clear-actions ()
  "Clears the input actions from last frame."
  (setf *key-actions* nil
        *mouse-button-actions* nil))

(defun update-dt ()
  "Called in the main loop, updates time globals."
  (setf *dt* (- (glfw:get-time) *previous-time*)
        *previous-time* (glfw:get-time))

  ;; (incf *total-frames*)

  ;; prevent unruly time steps from breaking game
  (setf *dt* (max 0.0d0 (min 0.25d0 *dt*))))

(defun cap-fps ()
  "Cap frame rate, preventing resource hogging"
  (let ((frame-diff (- (+ (/ 1.0d0 +max-fps+) *previous-time*) (glfw:get-time))))
    (when (> frame-diff 0)
      (sleep frame-diff)
      ;; (print frame-diff)
      )))

(defun update-globals ()
  "A single function that encompasses global updates"
  (clear-actions)
  (update-dt)
  (cap-fps))

(defun update-window-title (window title)
  (cl-glfw3:set-window-title (format nil "~A | fps: ~A" title
                                     (round (average-fps)))
                             window))

(defun initialize-globals ()
  ;; (setf *dt* 0.01
  ;;       *previous-time* 0.0
  ;;       *total-dt* 0.0
  ;;       *total-frames* 0.0
  ;;       *average-fps* 0.0
  ;;       *cursor-callback-p* nil
  ;;       *scroll-callback-P* nil
  ;;       *cursor-x* (/ *width* 2.0)
  ;;       *cursor-y* (/ *height* 2.0)
  ;;       *scroll-x* (/ *width* 2.0)
  ;;       *scroll-y* (/ *height* 2.0)
  ;;       *last-x* (/ *width* 2.0)
  ;;       *last-y* (/ *height* 2.0)
  ;;       *first-mouse* t
  ;;       *paused* nil
  ;;       *debug* nil)
  (iter (for (var-symbol func) on *global-setfs* by #'cddr)
    (funcall func)))

(defun key-action-p (key action)
  "Returns true if KEY is in *key-actions* and its state is EQ to ACTION."
  (let ((state (getf *key-actions* key)))
    (and (not (null state)) ;; if STATE is not null then key must have been found
         (eq action state))))

(defun key-down-p (key)
  (or (key-action-p key :press) (key-action-p key :repeat)))

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

(defun vec-add (v1 v2)
  "Returns a vector of the same type as V1, which is a component-wise sum of
V1 and V2."
  (map (type-of v1)
       (lambda (x y) (+ x y))
       v1 v2))

(defun vec-mul (v1 f)
  "Returns a vector with the same type as V1, which has components multiplied by F."
  (map (type-of v1)
       (lambda (x) (* x f))
       v1))

(defun x-val (vec)
  (aref vec 0))
(defun y-val (vec)
  (aref vec 1))
(defun z-val (vec)
  (aref vec 2))

;; (defun (setf x-val) (value vec)
;;   (setf (aref vec 0) value))
;; (defun (setf y-val) (value vec)
;;   (setf (aref vec 1) value))
;; (defun (setf z-val) (value vec)
;;   (setf (aref vec 2) value))

;; (defun get-slot (object &rest nested-slot-names)
;;   "Recursively getting slot-value of slot-value."
;;   (iter (with current = object) (for s in nested-slot-names)
;;     (setf current (slot-value current s))
;;     (finally (return current))))

(defmacro get-slot (object &rest nested-slot-names)
  (iter (with current = object)
        (for s in nested-slot-names)
        (setf current `(slot-value ,current ,s))
        (finally (return current))))

;; (defun (setf get-slot) (value object &rest nested-slot-names)
;;   (iter (with current = object)
;;     (for s in nested-slot-names)
;;     (setf current `(slot-value ,current ,s))
;;     (finally (return (setf current value)))))

;;; file io

(defun read-sexp-from-file (filename)
  "Reads all sexp from file FILENAME, returning a list of all collected."
  (with-open-file (file filename :direction :input)
    (with-standard-io-syntax
      (let ((*read-eval* nil))
        (iter (for sexp = (read file nil))
              (while sexp)
              (collect sexp))))))

(defun read-entire-file (filename)
  "DEPRECATED. Returns a string with the entire content of a FILENAME, including
whitespaces."
  (with-open-file (file filename :direction :input)
    (if file
        (let ((str ""))
          (iter
            (for line = (read-line file nil nil))
            (while line)
            (setf str (concatenate 'string str (format nil "~a~%" line))))
          str)
        (error "Unable to open file ~a.~%" file))))

;; swank stuff
(defmacro continuable (&body body)
  "Helper macro that we can use to allow us to continue from an
  error. Remember to hit C in slime or pick the restart so errors don't kill the app."
  `(restart-case
       (progn ,@body) (continue () :report "Continue")))

(defun update-swank ()
  (ignore-errors
   "Called from within the main loop, this keep the lisp repl running"
   (continuable
    (let ((connection (or swank::*emacs-connection* (swank::default-connection))))
      (when connection
        (swank::handle-requests connection t))))))

;; copy instances

(defgeneric copy-instance (object &rest initargs &key &allow-other-keys)
  (:documentation "Makes and returns a shallow copy of OBJECT.

  An uninitialized object of the same class as OBJECT is allocated by
  calling ALLOCATE-INSTANCE.  For all slots returned by
  CLASS-SLOTS, the returned object has the
  same slot values and slot-unbound status as OBJECT.

  REINITIALIZE-INSTANCE is called to update the copy with INITARGS.")
  (:method ((object standard-object) &rest initargs &key &allow-other-keys)
    (let* ((class (class-of object))
           (copy (allocate-instance class)))
      (dolist (slot-name (mapcar #'sb-mop:slot-definition-name
                                 (sb-mop:class-slots class)))
        (when (slot-boundp object slot-name)
          (setf (slot-value copy slot-name)
                (slot-value object slot-name))))
      (apply #'reinitialize-instance copy initargs))))

;; Threading macro from clojure
(defmacro -> (x &optional (form nil form-supplied-p) &rest more)
  (if form-supplied-p
      (if more
          `(-> (-> ,x ,form) ,@more)
          (if (listp form)
              `(,(car form) ,x ,@(cdr form))
              (list form x)))
      x))

(defun drop-nth (n list)
  (append (subseq list 0 n) (nthcdr (1+ n) list)))

(defun add-nth (n elem list)
  (append (subseq list 0 n) (cons elem (nthcdr n list))))

(defun set-nth (n elem list)
  (append (subseq list 0 n) (cons elem (nthcdr (1+ n) list))))

