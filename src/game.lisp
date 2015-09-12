;;; game.lisp

(in-package #:clglbo)

(defenum:defenum *game-state*
                 ((+game-active+ 0)
                  +game-menu+
                  +game-win+))

;; timeline = vector of tracked-vars each frame
;; tracked-vars = plist of lambdas that get the value of a tracked var
;; entities = list of components
;; components = plist

(defglobal *current-frame* 0)
(defglobal *total-frames* 0)
(defglobal *timeline*
    (make-array 1000000 :element-type 'list
                        :initial-element nil
                        :adjustable t
                        :fill-pointer 0))

(defglobal *tracked-vars* nil)

(defmacro track-var (var)
  "Adds avar to *TRACKED-VARS*, with a keyword symbol of VAR as the key and a lambda
that returns a list that has stuff that can update *TIMELINE*."
  `(let ((var-keyword (intern (string ',var) :keyword)))
     (setf (getf *tracked-vars* var-keyword)
           (lambda ()
             (list var-keyword ,var
                   :setter
                   (let ((var-value ,var))
                     (lambda () (setf ,var var-value))))))))

(defmacro track-vars (&rest vars)
  (let ((expr '(progn)))
    (iter (for v in vars)
      (setf expr (append expr `((track-var ,v)))))
    expr))

(defmacro untrack-vars (&rest var-keywords)
  `(alexandria:remove-from-plistf *tracked-vars* ,@var-keywords))

(defun update-timeline ()
  "Add plist of tracked values to the current frame"
  (incf *current-frame*)
  (incf *total-frames*)
  (vector-push-extend
   (iter (for (var-symbol func) on *tracked-vars* by #'cddr)
     (collect (funcall func)))
   *timeline*))

(defvar *entities* nil)
(defun add-entity (components)
  (push components *entities*))
(defun remove-entity (n)
  (setf (getf (nth n *entities*) :remove?) t))

(defclass game ()
  ((state
    :type (unsigned-byte 32)
    :initarg :state)
   (width
    :initarg :width
    :reader width)
   (height
    :initarg :height
    :reader height)
   (levels
    :initarg :levels
    :accessor levels)
   (current-level
    :initarg :current-level
    :accessor current-level))
  (:default-initargs
   :state +game-active+
   :width *width*
   :height *height*
   :levels nil
   :current-level 3))

;; (defgeneric game-process-input (game dt))
;; (defgeneric game-update (game dt))
;; (defgeneric game-render (game))
;; (defgeneric game-init (game))

(defmethod init ((game game))
  (with-accessors ((width width) (height height) (world world)
                   (levels levels) (current-level current-level)
                   (player player)) game
    (let ((program (make-program #p"./data/shaders/sprite.v.glsl"
                                 #p"./data/shaders/sprite.f.glsl")))
      (setf *program-manager* (make-instance 'program-manager)
            *texture-manager* (make-instance 'texture-manager)
            *sprite-renderer* (make-instance 'sprite-renderer :program program))
      (load-resource *program-manager* "sprite" program)

      ;; textures
      (load-resource *texture-manager*
                     "face"
                     (make-texture2d "./data/images/awesomeface.png" t))
      (load-resource *texture-manager*
                     "background"
                     (make-texture2d "./data/images/background.jpg" nil))
      (load-resource *texture-manager*
                     "block"
                     (make-texture2d "./data/images/block.png" nil))
      (load-resource *texture-manager*
                     "block-solid"
                     (make-texture2d "./data/images/block_solid.png" nil))
      (load-resource *texture-manager*
                     "paddle"
                     (make-texture2d "./data/images/paddle.png" t))

      ;; (push (cons "background" (get-resource *texture-manager* "background")) (texs game))

      ;; (setf levels (list (make-level "./data/levels/one" world width (* height 0.5))
      ;;                    (make-level "./data/levels/two" world width (* height 0.5))
      ;;                    (make-level "./data/levels/three" world width (* height 0.5))
      ;;                    (make-level "./data/levels/four" world width (* height 0.5))))

      ;;use current program
      (use program)

      ;;set image uniform to texture0
      (gl:uniformi (get-uniform program "image") 0)

      ;; set projection matrix
      (gl:uniform-matrix-4fv
       (get-uniform program "projection")
       (vector
        ;; left, right, bottom, top, near, far
        (kit.glm:ortho-matrix 0.0
                              (cfloat (width game))
                              0.0
                              (cfloat (height game))
                              -1.0 1.0))
       ;; (vector (kit.math:perspective-matrix (kit.glm:deg-to-rad 45.0)
       ;;                               (cfloat (/ *width* *height*))
       ;;                               -2.1
       ;;                               100.0))
       nil))

    ;;; timeline
    ;; (track-var *entities*)
    (track-vars *dt* *entities*)

    ;; qua related stuff
    ;; (load-level (elt levels current-level))
    ;; (let* ((render-sys (make-instance 'render-system))
    ;;        (e (make-entity world))
    ;;        (pos (make-position-component :vec (vec2 100.0 10.0)))
    ;;        (size (make-size-component :vec (vec2 200.0 100.0)))
    ;;        (rend (make-render-component :sprite (get-resource *texture-manager* "paddle")
    ;;                                     :color (vec4 1.0 1.0 1.0 1.0)
    ;;                                     :rotation 0.0))
    ;;        (phy (make-physics-component :acceleration (vec2 0.0 0.0) :velocity (vec2 0.0 0.0)
    ;;                                     :collision-type :aabb)))
    ;;   (setf player (make-instance 'object
    ;;                               :id e
    ;;                               :physics-component phy
    ;;                               :render-component rend
    ;;                               :size-component size
    ;;                               :position-component pos))
    ;;   (add-components world e pos size rend phy)
    ;;   (add-systems world render-sys)
    ;;   (system-add-entities world render-sys e))
    ;; (let ((render-sys (make-instance 'render-system))
    ;;       (background (make-instance 'renderable))
    ;;       (background-id (make-entity world)))
    ;;   (setf (get-slot background 'render-component 'sprite) (get-resource *texture-manager* "background")
    ;;         (aref (get-slot background 'size-component 'size) 0) 800.0
    ;;         (aref (get-slot background 'size-component 'size) 1) 600.0)
    ;;   ;; (add-components world background-id
    ;;   ;;                 (position-component background)
    ;;   ;;                 (size-component background)
    ;;   ;;                 (render-component background))
    ;;   (add-clump world background-id background)
    ;;   (add-systems world render-sys))
    ;; (initialize-systems world)
    ))

(defmethod handle-input ((game game))
  ;;debugging
  (when (or *key-actions* *mouse-button-actions*)
    (format t "keys : ~a | mouse : ~a~%" *key-actions* *mouse-button-actions*))

  ;; keys
  (cond ((key-action-p :escape :press)
         (glfw:set-window-should-close)))

  ;; mouse buttons
  (cond ((key-action-p :p :press)
         (setf *paused* (not *paused*))
         (if *paused* (print "paused") (print "unpaused")))))

(defmethod update ((game game))
  (when (not *paused*)
    (gl:clear-color 0.0 0.0 0.0 1.0)
    ;; (gl:clear :color-buffer-bit :depth-buffer-bit)
    (gl:clear :color-buffer-bit)

    (update-timeline)

    ;; (sprite-render *sprite-renderer* (get-resource *texture-manager* "face")
    ;;                (kit.glm:vec2 0.0 0.0)
    ;;                (kit.glm:vec2 (cfloat (width game)) (cfloat (height game)))
    ;;                0.0
    ;;                (kit.glm:vec4 (cfloat (+ 0.5 (* 0.5 (sin (glfw:get-time)))))
    ;;                              (cfloat ( + 0.5 (* 0.5 (cos (glfw:get-time)))))
    ;;                              0.0
    ;;                              1.0))

    ;; (update-world (world game) *dt*)
    ))

(defmethod render ((game game))
  )
