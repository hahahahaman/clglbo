(in-package #:clglbo)

(defsystem render-system (position-component render-component))

(defmethod update-system ((world world) (system render-system) dt)
  (with-components ((pos position-component) (rend render-component)) world system
    (sprite-render *sprite-renderer*
                   (render-component-sprite rend)
                   (position-component-vec pos)
                   (render-component-size rend)
                   (render-component-rotation rend)
                   (render-component-color rend))))
