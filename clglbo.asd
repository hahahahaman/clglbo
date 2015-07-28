;;;; clglbo.asd

(asdf:defsystem #:clglbo
  :description "Describe clglbo here"
  :author "Ed Ye <hahahadude@gmail.com>"
  :license "Licenceless Rider"
  :depends-on (#:cl-opengl
               #:cl-glfw3
               #:cl-soil
               #:glkit
               #:trivial-garbage
               #:defenum
               #:qua)
  :serial t
  :components ((:module src
                :components ((:file "package")
                             (:file "globals")
                             (:file "utils")
                             (:file "program")
                             (:file "texture")
                             (:file "resource-manager")
                             (:file "sprite-renderer")
                             (:file "game")
                             (:file "window")
                             (:file "breakout")
                             (:file "clglbo")))))

