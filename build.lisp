(require 'sb-posix)

;; (load #P"~/quicklisp/setup.lisp")
(push (merge-pathnames "./" *default-pathname-defaults*)
      asdf:*central-registry*)
(push (merge-pathnames "lib/" *default-pathname-defaults*)
      asdf:*central-registry*)

(ql:quickload :clglbo) ;; here

(sb-ext:save-lisp-and-die "bo.bin"
                          :toplevel (lambda ()
                                      (sb-posix:putenv
                                       (format nil "SBCL_HOME=~A"
                                               #.(sb-ext:posix-getenv "SBCL_HOME")))
                                      (clglbo:clglbo) ;; here
                                      0)
                          :executable t)

