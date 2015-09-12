(in-package :clglbo)

(defgeneric add-clump (world entity-id clump)
  (:documentation "A clump is a collection of components, this method adds the
components in CLUMP to ENTITY-ID in WORLD."))

(defmethod add-clump ((world world) (entity-id integer) (clump t)))

(defclump object ()
  ((position-component
    (make-position-component :pos (vec2 0.0 0.0)))
   (size-component
    (make-size-component :size (vec2 10.0 10.0)))))

(defmethod add-clump :before ((world world) (entity-id fixnum) (clump object))
  (print "object")
  (add-components world entity-id
                  (get-slot clump 'position-component)
                  (get-slot clump 'size-component)))

(defclump renderable (object)
  ((render-component
    (make-render-component :color (vec4 1.0 1.0 1.0 1.0)
                           :rotation 0.0))))

(defmethod add-clump :before ((world world) (entity-id fixnum) (clump renderable))
  (print "renderable")
  (add-components world entity-id (get-slot clump 'render-component)))

(defclump moveable (object)
  ((move-component (make-move-component :accel (vec2 0.0 0.0) :vel (vec2 0.0 0.0)))))

(defmethod add-clump :before ((world world) (entity-id fixnum) (clump moveable))
  (print "moveable")
  (add-components world entity-id (get-slot clump 'move-component)))

(let ((player-width 200.0))
  (defclump paddle (renderable moveable)
      ((position-component
        (make-position-component :pos (vec2 (/ (- *width* player-width) 2.0) 50.0)))
       (size-component
        (make-size-component :size (vec2 player-width 40.0)))
       (input-component)
       (player-component))))

(defmethod add-clump :before ((world world) (entity-id fixnum) (clump paddle))
  (print "paddle")
  (add-components world entity-id
                  (get-slot clump 'input-component)
                  (get-slot clump 'player-component)))
