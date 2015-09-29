;; game.lisp

(in-package #:clglbo)

(defenum:defenum *enum-game-state*
                 ((+game-active+ 0)
                  +game-menu+
                  +game-win+))

(defglobal *deadp* nil)

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
    (track-vars *entities* *deadp*)

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
           (cond ((and (key-pressed-p :left)
                       (@ entity :playerp))
                  (lambda ()
                    (setf *entities*
                          (set-component :vel id (vec2 -200.0 0.0)))))
                 ((and (key-pressed-p :right)
                       (@ entity :playerp))
                  (lambda ()
                    (setf *entities*
                          (set-component :vel id (vec2 200.0 0.0)))))
                 ((and (key-pressed-p :up)
                       (@ entity :playerp))
                  (lambda ()
                    (setf *entities*
                          (set-component :vel id (vec2 0.0 -200.0)))))
                 ((and (key-pressed-p :down)
                       (@ entity :playerp))
                  (lambda ()
                    (setf *entities*
                          (set-component :vel id (vec2 0.0 200.0)))))
                 ((and (key-action-p :space :press)
                       (@ entity :follow-playerp))
                  (lambda ()
                    ;; (setf *entities*
                    ;;       (set-component :vel id (@ entity :init-vel)))
                    (let ((e (get-entity id)))
                      ;; (setf *entities*
                      ;;       (with *entities* id (-> e
                      ;;                               (with :vel (@ e :init-vel))
                      ;;                               (with :follow-playerp nil))))
                      (setf *entities* (set-component :vel id (@ e :init-vel))
                            *entities* (set-component :follow-playerp id nil))
                      )))
                 (t (lambda ()
                      (setf *entities*
                            (set-component :vel id (vec2 0.0 0.0))))))))
    (let ((new-changes ())
          (paddle nil)
          (ball-id nil)
          (ball nil))
      (do-map (id entity entities)
        (when (playablep entity)
          (push (handle-player id entity) new-changes)
          (when (@ entity :playerp)
            (setf paddle entity)))
        (when (and (@ entity :ballp)
                   (@ entity :follow-playerp))
          (setf ball entity
                ball-id id)))
      (when (and paddle ball)
        (let* ((paddle-pos (@ paddle :pos))
               (paddle-size (@ paddle :size)) 
               (ball-size (@ ball :size))
               (new-ball-pos (vec2-add paddle-pos
                                       (vec2 (/ (- (x-val paddle-size)
                                                   (x-val ball-size)) 2.0)
                                             (- (+ (y-val ball-size) 10.0))))))
          (push (lambda ()
                  (let ((ball (@ *entities* ball-id)))
                    (setf *entities* (with *entities* ball-id
                                           (with ball :pos new-ball-pos)))))
                new-changes)))
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
  (when (not *deadp*)
    (when (key-action-p :r :press)
      (play-pressed)))

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

(defun move-entities (&optional (dt *dt*) (entities *entities*))
  (let ((move-changes ())
        (col-changes ())
        (dt (cfloat dt)))
    (flet ((move-validp (id new-pos size type)
             (let ((obj (make-collision-obj type new-pos size))
                   (result t))
               (do-map (other-id other-entity entities)
                 (let ((other-type (@ other-entity :collision-type))
                       (other-pos (@ other-entity :pos))
                       (other-size (@ other-entity :size)))
                   (when (and other-type other-pos
                              other-size (not (eql id other-id)))
                     (let ((other-obj (make-collision-obj
                                       other-type
                                       other-pos
                                       other-size)))
                       (cond ((collidep obj other-obj)
                              (push (collide id other-id entities) col-changes)
                              (push (collide other-id id entities) col-changes)
                              (setf result nil))
                             ((or (< (x-val new-pos) 0.0)
                                  (< (y-val new-pos) 0.0)
                                  (> (+ (x-val new-pos) (x-val size)) *width*)
                                  (> (+ (y-val new-pos) (y-val size)) *height*))
                              (when (and (@ (@ entities id) :ballp)
                                         (> (+ (y-val new-pos) (y-val size))
                                            *height*))
                                (push (lambda ()
                                        (setf *deadp* t)) move-changes)
                                (push (lambda ()
                                        (pause-pressed)) move-changes))
                              (setf result nil)))))))
               result)))
      (do-map (id entity entities)
        (let ((pos (@ entity :pos))
              (size (@ entity :size))
              (col-type (@ entity :collision-type))
              (vel (@ entity :vel))
              (ballp (@ entity :ballp)))
          (when (and pos vel) ;; moveable
            (cond (col-type ;; collidable
                   ;; x pass
                   (when (not (zerop (x-val vel)))
                     (cond ((move-validp id (vec2 (+ (x-val pos) (* (x-val vel) dt))
                                                  (y-val pos))
                                         size col-type)
                            ;; can move
                            (push (lambda ()
                                    (let* ((current-entity (get-entity id))
                                           (current-pos (@ current-entity :pos)))
                                      (setf *entities*
                                            (set-component
                                             :pos
                                             id
                                             (vec2 (+ (x-val current-pos)
                                                      (* (x-val vel) dt))
                                                   (y-val current-pos))))))
                                  move-changes))
                           (ballp
                            ;; hit something
                            (push (lambda ()
                                    (let ((current-vel (get-component :vel id)))
                                      (setf *entities*
                                            (set-component
                                             :vel
                                             id
                                             (vec2 (- (x-val current-vel))
                                                   (y-val current-vel))))))
                                  move-changes))))
                   ;; y pass
                   (cond ((move-validp id
                                       (vec2 (x-val pos)
                                             (+ (y-val pos) (* (y-val vel) dt)))
                                       size col-type)
                          ;; can move
                          (push (lambda ()
                                  (let* ((current-entity (get-entity id))
                                         (current-pos (@ current-entity :pos)))
                                    (setf *entities*
                                          (set-component
                                           :pos
                                           id
                                           (vec2 (x-val current-pos)
                                                 (+ (y-val current-pos)
                                                    (* (y-val vel) dt)))))))
                                move-changes))
                         (ballp
                          ;; hit something
                          (push (lambda ()
                                  (setf *entities*
                                        (set-component
                                         :vel
                                         id
                                         (vec2 (x-val vel)
                                               (- (y-val vel))))))
                                move-changes))))
                  ;; ghost, just move
                  (t
                   (push
                    (lambda ()
                      (setf *entities*
                            (set-component :pos id
                                           (vec2-add (@ entity :pos)
                                                     (vec2-mul (@ entity :vel)
                                                               dt)))))
                    move-changes)))))))
    (append move-changes col-changes)))

(defmethod update ((game game))
  (cond ((eql *time-travel-state* +time-play+) 
         (alexandria:appendf *destructive-changes* (move-entities))
         ;; (alexandria:appendf *destructive-changes* (entity-collisions))
         (update-entities)
         (update-timeline)
         ;; (print *destructive-changes*)
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
