(in-package #:clglbo)

(defclass breakout-window (window)
  ((breakout
    :type game
    :accessor breakout
    :initarg :breakout))
  (:default-initargs
   :breakout (make-instance 'game :width *width* :height *height*)))

(defmethod cleanup ((window breakout-window))
  (clear-resources *program-manager*)
  (clear-resources *texture-manager*)
  t)

(defmethod run ((window breakout-window))
  (with-accessors ((title title)
                   (width width)
                   (height height)
                   (breakout breakout)) window
    (glfw:with-init-window (:title title
                            :width width
                            :height height
                            :opengl-forward-compat t
                            :opengl-profile :opengl-core-profile
                            :context-version-major 3
                            :context-version-minor 3
                            :decorated t
                            :resizable nil
                            ;;full screen mode
                            ;; :monitor (glfw:get-primary-monitor)
                            ;; :refresh-rate 60
                            )
      ;; (glfw:swap-interval 1)
      (setf %gl:*gl-get-proc-address* #'glfw:get-proc-address)

      ;; initialize
      (unless (gl::features-present-p (>= :glsl-version 3.3))
        ;;destroys the window cuz of unwind-protect
        (return-from run nil))

      (initialize-globals)
      (init breakout)

      (gl:enable :blend)
      (gl:disable :depth-test)
      (gl:blend-func :src-alpha :one-minus-src-alpha)

      (glfw:set-key-callback 'key-callback)
      (glfw:set-mouse-button-callback 'mouse-callback)
      (glfw:set-cursor-position-callback 'cursor-callback)
      (glfw:set-scroll-callback 'scroll-callback)
      ;; (glfw:set-input-mode :cursor :disabled)

      (iter (until (glfw:window-should-close-p))
        (update-swank)
        ;; give some fps data in title
        (update-window-title cl-glfw3:*window* title)

        (glfw:poll-events)

        (handle-input breakout)
        (render breakout)
        (update breakout)

        (glfw:swap-buffers)
        (update-globals))

      (cleanup window))))
