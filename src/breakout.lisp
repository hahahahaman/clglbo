(in-package #:clglbo)

(defclass breakout-window (window)
  ((breakout
    :type game
    :initarg :breakout))
  (:default-initargs
   :breakout (make-instance 'game)))

(defmethod window-cleanup ((window breakout-window))
  t)

(defmethod window-run ((window breakout-window))
  (with-slots (title width height breakout) window
    (with-slots (breakout) window
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
          (return-from window-run nil))

        (gl:enable :blend :depth-test)
        (gl:blend-func :src-alpha :one-minus-src-alpha)

        ;; (window-load-shaders window)
        ;; (window-load-textures window)
        ;; (window-load-rest window)

        (iter (until (glfw:window-should-close-p))
          #|do (load-shaders window)
          do (load-textures window)
          do (load-rest window)|#
          (update-dt)
          (glfw:poll-events)
          (game-process-input breakout *dt*)
          (game-update breakout *dt*)

          (gl:clear-color 0.0 0.0 0.0 1.0)
          (gl:clear :color-buffer-bit)
          (game-render breakout)
          (glfw:swap-buffers))

        (window-cleanup window)))))
