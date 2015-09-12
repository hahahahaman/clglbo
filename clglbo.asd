;;;; clglbo.asd

(asdf:defsystem #:clglbo
  :description "breakout?"
  :author "hahahahaman <hahahadude@gmail.com>"
  :license "Licenceless Rider"
  :depends-on (#:cl-opengl
               #:cl-glfw3
               #:cl-soil
               #:fset
               #:glkit
               #:trivial-garbage
               #:defenum
               #:qua)
  :serial t
  :components ((:module src
                :components ((:file "package")
                             (:file "globals")
                             (:file "utils")
                             (:file "input")
                             (:file "program")
                             (:file "texture")
                             (:file "resource-manager")
                             (:file "sprite-renderer")
                             (:file "components")
                             (:file "systems")
                             (:file "clump")
                             (:file "clump-objects")
                             (:file "level")
                             (:file "game")
                             (:file "window")
                             (:file "breakout")
                             (:file "clglbo")))))

