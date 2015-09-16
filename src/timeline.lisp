;;;; timeline.lisp

(in-package #:clglbo)

;; timeline = vector of tracked-vars each frame
;; tracked-vars = plist of lambdas that get the value of a tracked var

(defmacro var-keyword (var)
  `(intern (string ',var) :keyword))
(defmacro var-keyword-macro (var)
  ``(intern (string ',,var) :keyword))

(defmacro track-var (var)
  "Adds a var to *TRACKED-VARS*, with a keyword symbol of VAR as the key and a lambda
that returns a list that has stuff that can update *TIMELINE*."
  `(let ((keyword (var-keyword ,var)))
     (setf (getf *tracked-vars* keyword)
           (lambda ()
             (list
              ;; keyword ,var
              :setter
              (let ((var-value ,var))
                (lambda () (setf ,var var-value))))))))

(defmacro track-vars (&rest vars)
  (let ((expr '(progn)))
    (iter (for v in vars)
      (setf expr (append expr `((track-var ,v)))))
    expr))

(defmacro untrack-vars (&rest vars)
  `(setf *tracked-vars*
         (remove-from-plistf *tracked-vars*
                             ,@(mapcar
                                (lambda (v) (var-keyword-macro v))
                                vars))))

(defun update-timeline ()
  "Add plist of tracked values to the current frame"
  (incf *current-frame*)
  (incf *max-frame-index*)
  (vector-push-extend
   (iter (for (var-keyword func) on *tracked-vars* by #'cddr)
     (collect (funcall func)))
   *timeline*))

(defun goto-frame (n)
  ;; constrain between 0 and *MAX-FRAME-INDEX*
  (setf *current-frame* n)
  (when (< *current-frame* 0)
    (pause-pressed)
    (setf *current-frame* 0))
  (when (> *current-frame* *max-frame-index*)
    (pause-pressed)
    (setf *current-frame* *max-frame-index*))

  ;; go through tracked-vars list of that frame setting all values
  (mapcar (lambda (tracked-var) (funcall (getf tracked-var :setter)))
          (aref *timeline* *current-frame*)))

(defun pause-pressed ()
  (when (not (eql *time-travel-state* +time-paused+))
    (setf *time-travel-state* +time-paused+)))

(defun play-pressed ()
  (when (not (eql *time-travel-state* +time-play+))
    ;; erase the future
    (iter (for i from *current-frame* to *max-frame-index*)
      (setf (aref *timeline* i) nil))
    (setf *time-travel-state* +time-play+
          (fill-pointer *timeline*) *current-frame*
          *max-frame-index* *current-frame*)))

;;; rewind and fast-forward
(defglobal *time-speed-multiplier* (vector 1 2 4 8 16 32))
(defglobal *time-speed-index* 0)

(defun forward-pressed ()
  (cond ((not (eql *time-travel-state* +time-forward+))
         (setf *time-travel-state* +time-forward+
               *time-speed-index* 0))
        (t
         (setf *time-speed-index* (mod (1+ *time-speed-index*)
                                       (length *time-speed-multiplier*))))))

(let ((timestep (/ 1.0 60.0d0))
      (accum 0.0d0))
  (defun forward-time ()
    (incf accum *dt*)
    (iter (while (>= accum timestep))
      (goto-frame (+ *current-frame*
                     (aref *time-speed-multiplier* *time-speed-index*)))
      (decf accum timestep))))

(defun rewind-pressed ()
  (cond ((not (eql *time-travel-state* +time-rewind+))
         (setf *time-travel-state* +time-rewind+
               *time-speed-index* 0))
        (t
         (setf *time-speed-index* (mod (1+ *time-speed-index*)
                                       (length *time-speed-multiplier*))))))

(let ((timestep (/ 1.0d0 60.0d0))
      (accum 0.0d0))
  (defun rewind-time ()
    (incf accum *dt*)
    (iter (while (>= accum timestep))
      (goto-frame (- *current-frame*
                     (aref *time-speed-multiplier* *time-speed-index*)))
      (decf accum timestep))))
