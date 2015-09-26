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

      (setf levels (vector (make-level "./data/levels/one" width (* height 0.5))
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
    (track-vars *entities*)

    (setf *entities* (load-level (aref levels current-level)))))

(defun next-level (game &optional (level-incf 1))
  (with-slots (levels current-level width height) game
    (setf current-level (mod (+ current-level level-incf) (length levels)))
    (when (< current-level (length levels))
      (setf *entities* (load-level (aref levels current-level) width height (empty-map))))))

(defun handle-player-input (&optional (entities *entities*))
  (flet ((playablep (entity)
           (and (or (@ entity :playerp)
                    (@ entity :follow-playerp))
                (@ entity :vel)))
         (handle-player (id entity) 
           (cond ((key-pressed-p :left)
                  (lambda ()
                    (setf *entities*
                          (set-component :vel id (vec2 -200.0 0.0)))))
                 ((key-pressed-p :right)
                  (lambda ()
                    (setf *entities*
                          (set-component :vel id (vec2 200.0 0.0)))))
                 ((key-pressed-p :up)
                  (lambda ()
                    (setf *entities*
                          (set-component :vel id (vec2 0.0 -200.0)))))
                 ((key-pressed-p :down)
                  (lambda ()
                    (setf *entities*
                          (set-component :vel id (vec2 0.0 200.0)))))
                 ((and (key-action-p :space :press)
                       (@ entity :ballp))
                  (lambda ()
                    (setf *entities*
                          (set-component :vel id (@ entity :init-vel)))))
                 (t (lambda ()
                      (setf *entities*
                            (set-component :vel id (vec2 0.0 0.0))))))))
    (let ((new-changes ()))
      (do-map (id entity entities)
        (when (playablep entity)
          (push (handle-player id entity) new-changes)))
      (reverse new-changes))))

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

    ;;level debugging
    (when (key-action-p :n :press)
      (next-level game))
    (when (key-action-p :b :press)
      (next-level game -1))
    (when (key-action-p :m :press)
      (next-level game 0))

    (alexandria:appendf *destructive-changes* (handle-player-input))))

(defun entity-collisions (&optional (entities *entities*))
  ;; (declare (optimize (speed 3) (safety 0)))
  (let ((new-changes ())
        (balls ())
        (blocks ()))
    (do-map (id entity entities)
      (let ((pos (@ entity :pos))
            (size (@ entity :size))
            (type (@ entity :collision-type)))
        (when (and pos size type)
          (cond ((or (@ entity :brickp) (@ entity :playerp))
                 (push (cons id (make-collision-obj type pos size)) blocks))
                ((@ entity :ballp)
                 (push (cons id (make-collision-obj type pos size)) balls))))))
    (iter (for (ball-id . ball-obj) in balls)
      (iter (for (block-id . block-obj) in blocks)
        (when (collidep ball-obj block-obj)
          (push (collide ball-id block-id entities) new-changes))))
    (reverse new-changes)))

(defun move-entities (&optional (dt *dt*) (entities *entities*))
  (flet ((moveablep (id entity)
           (declare (ignore id))
           (and (@ entity :pos)
                (@ entity :vel)))
         (move (id entity)
           (lambda ()
             (setf *entities*
                   (set-component :pos id
                                  (vec2-add (@ entity :pos)
                                            (vec2-mul (@ entity :vel)
                                                      (cfloat dt))))))))
    ;; (do-map (id entity entities)
    ;;   ())
    (get-map-keys 'list (image #'move
                               (filter #'moveablep entities)))))

(defmethod update ((game game))
  (cond ((eql *time-travel-state* +time-play+) 
         (alexandria:appendf *destructive-changes* (move-entities))
         (alexandria:appendf *destructive-changes* (entity-collisions))
         (update-entities)
         (update-timeline)
         (setf *destructive-changes* nil))

        ((eql *time-travel-state* +time-rewind+)
         (rewind-time))
        ((eql *time-travel-state* +time-forward+)
         (forward-time))))

(defun render-entities (&optional (entities *entities*))
  (flet ((render-entity (id)
           (let ((pos (get-component :pos id))
                 (size (get-component :size id))
                 (color (get-component :color id))
                 (rotation (get-component :rotation id))
                 (texture (get-component :texture id)))
             (when (and pos size color rotation texture)
               (sprite-render texture
                              pos
                              size
                              color
                              rotation)))))
    (do-map (id entity entities)
      (declare (ignore entity))
      (render-entity id))))

(defmethod render ((game game))
  (when (not (eql *time-travel-state* +time-paused+))
    (gl:clear-color 0.0 0.0 0.0 1.0)
    ;; (gl:clear :color-buffer-bit :depth-buffer-bit)
    (gl:clear :color-buffer-bit)
    (render-entities)))
