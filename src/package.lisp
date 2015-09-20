;;;; package.lisp

(defpackage #:clglbo
  (:use #:cl #:iter #:fset)
  (:import-from :kit.glm #:vec2 #:vec3 #:vec4)
  ;; (:import-from :fset :lookup :@ :map)
  (:shadowing-import-from :iter #:iter #:while)
  (:shadowing-import-from :fset
                          ;; Shadowed type/constructor names
                          #:set #:map
                          ;; Shadowed set operations
                          #:union #:intersection #:set-difference #:complement
                          ;; Shadowed sequence operations
                          #:first #:last #:subseq #:reverse #:sort #:stable-sort
                          #:reduce
                          #:find #:find-if #:find-if-not
                          #:count #:count-if #:count-if-not
                          #:position #:position-if #:position-if-not
                          #:remove #:remove-if #:remove-if-not
                          #:substitute #:substitute-if #:substitute-if-not
                          #:some #:every #:notany #:notevery
                          #:with)
  (:export :clglbo))

