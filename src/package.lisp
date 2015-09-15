;;;; package.lisp

(defpackage #:clglbo
  (:use #:cl #:iter ;; #:qua
        #:alexandria)
  (:import-from :kit.glm :vec2 :vec3 :vec4)
  (:export :clglbo))

