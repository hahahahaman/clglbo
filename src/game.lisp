;; game.lisp

(in-package #:clglbo)

(defenum:defenum *enum-game-state*
                 ((+game-active+ 0)
                  +game-menu+
                  +game-win+))

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
   :current-level 0))

(defmethod init ((game game))
  (with-accessors ((width width) (height height) 
                   (levels levels) (current-level current-level)) game
    (let ((program (make-program #p"./data/shaders/sprite.v.glsl"
                                 #p"./data/shaders/sprite.f.glsl")))
      (setf *program-manager* (make-instance 'program-manager)
            *texture-manager* (make-instance 'texture-manager)
            *sprite-renderer* (make-instance 'sprite-renderer :program program))
      (load-program "sprite" program)

      ;; textures
      (load-texture "face"
                    (make-texture2d "./data/images/awesomeface.png" t))
      (load-texture "background"
                    (make-texture2d "./data/images/background.jpg" nil))
      (load-texture "block"
                    (make-texture2d "./data/images/block.png" nil))
      (load-texture "block-solid"
                    (make-texture2d "./data/images/block_solid.png" nil))
      (load-texture "paddle"
                    (make-texture2d "./data/images/paddle.png" t))

      (setf levels (list (make-level "./data/levels/one" width (* height 0.5))
                         (make-level "./data/levels/two" width (* height 0.5))
                         (make-level "./data/levels/three" width (* height 0.5))
                         (make-level "./data/levels/four" width (* height 0.5))))

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
                              (cfloat (height game))
                              0.0
                              -1.0 1.0))
       ;; (vector (kit.math:perspective-matrix (kit.glm:deg-to-rad 45.0)
       ;;                               (cfloat (/ *width* *height*))
       ;;                               -2.1
       ;;                               100.0))
       nil))

    ;;; timeline
    ;; (track-var *dt*)
    (track-vars *entities*)
    ;; (untrack-vars *entities* *dt*)

    (load-level (nth current-level levels))))

(defun handle-player-input ()
  (flet ((handle-player (components)
           (let ((player (get-component :player components))
                 (follow-player (get-component :follow-player components))
                 (vel (get-component :vel components))
                 ;; (accel (get-component :accel components))
                 )
             (cond ((and (or player follow-player) vel)
                    (cond ((key-pressed-p :left) 
                           (set-component :vel (vec2 -200.0 0.0) components))
                          ((key-pressed-p :right)
                           (set-component :vel (vec2 200.0 0.0) components))
                          (t (set-component :vel (vec2 0.0 0.0) components)))
                    (when (and follow-player (get-component :ball components)
                               (key-pressed-p :up))
                      (set-component :follow-player nil components)
                      (set-component :vel (get-component :init-vel components) components))
                    components)
                   (t components)))))
    (setf *entities* (mapcar #'handle-player *entities*))))

(defmethod handle-input ((game game))
  ;;debugging
  (when (or *key-actions* *mouse-button-actions*)
    (format t "keys : ~a | mouse : ~a~%" *key-actions* *mouse-button-actions*))

  ;; keys
  (when (key-action-p :escape :press)
    (glfw:set-window-should-close))

  (when (key-action-p :q :press)
    (rewind-pressed))
  (when (key-action-p :w :press)
    (forward-pressed))
  (when (key-action-p :e :press)
    (pause-pressed))
  (when (key-action-p :r :press)
    (play-pressed))

  (when (eql *time-travel-state* +time-play+)
    ;; (when (key-action-p :c :press)
    ;;   (setf *entities* nil))

    ;; ;; mouse buttons
    ;; (when (mouse-button-pressed-p :left)
    ;;   (let ((w 50.0)
    ;;         (h 50.0))
    ;;     (add-entity (list :pos (vec2 (- (cfloat *cursor-x*) (/ w 2.0))
    ;;                                  (- (cfloat *cursor-y*) (/ h 2.0)))
    ;;                       :size (vec2 w h)
    ;;                       :color (vec4 (random 1.0) (random 1.0) (random 1.0) 1.0)
    ;;                       :rotation 0.0
    ;;                       :texture (get-texture "face")))))
    (handle-player-input)
    ))

(defun move-entities ()
  (flet ((move (components)
           (let ((pos (get-component :pos components))
                 (vel (get-component :vel components)))
             (cond ((and pos vel)
                    (set-component :pos (vec-add pos (vec-mul vel (cfloat *dt*)))
                                   components))
                   (t components)))))
    (setf *entities* (mapcar #'move *entities*))))

(defmethod update ((game game))
  (cond ((eql *time-travel-state* +time-play+) 
         (move-entities)
         (update-timeline)
         (update-entities))
        ((eql *time-travel-state* +time-rewind+)
         (rewind-time))
        ((eql *time-travel-state* +time-forward+)
         (forward-time))))

(defun render-entities ()
  (flet ((render-entity (components)
           (let ((pos (get-component :pos components))
                 (size (get-component :size components))
                 (color (get-component :color components))
                 (rotation (get-component :rotation components))
                 (texture (get-component :texture components)))
             (when (and pos size color rotation texture)
               (sprite-render texture
                              pos
                              size
                              color
                              rotation)))))
    (mapcar #'render-entity *entities*)))

(defmethod render ((game game))
  (when (not (eql *time-travel-state* +time-paused+))
    (gl:clear-color 0.0 0.0 0.0 1.0)
    ;; (gl:clear :color-buffer-bit :depth-buffer-bit)
    (gl:clear :color-buffer-bit)
    (render-entities)))
