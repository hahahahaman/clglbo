(in-package #:clglbo)

(defclass breakout-window (window)
  ((breakout
    :type game
    :accessor breakout
    :initarg :breakout))
  (:default-initargs
   :breakout (make-instance 'game :width *width* :height *height*)))

(defmethod cleanup ((window breakout-window))
  (clear-resources *texture-manager*)
  (clear-resources *program-manager*)
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
                            :resizable nil)
      (setf %gl:*gl-get-proc-address* #'glfw:get-proc-address)
      (glfw:set-key-callback 'key-callback)
      (glfw:set-mouse-button-callback 'mouse-callback)
      (glfw:set-cursor-position-callback 'cursor-callback)
      (glfw:set-scroll-callback 'scroll-callback)
      ;; (glfw:set-input-mode :cursor :disabled)

      ;; initialize

      (unless (gl::features-present-p (>= :glsl-version 3.3))
        ;;destroys the window cuz of unwind-protect
        (return-from run nil))

      (gl:enable :blend :depth-test)
      (gl:blend-func :src-alpha :one-minus-src-alpha)

      (init breakout)

      (iter (until (glfw:window-should-close-p))
        (update-dt)
        (glfw:poll-events)

        (render breakout)
        (update breakout)

        (glfw:swap-buffers))

      (cleanup window))))
