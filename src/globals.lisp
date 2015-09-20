;;;; globals.lisp

(in-package #:clglbo)

(defvar *global-setfs* nil)

(defmacro defglobal (var &optional (val nil valp) (doc nil docp))
  (append `(progn)
          (if (and valp docp)
              `((defvar ,var ,val ,doc))
              `((defvar ,var ,val)))
          `((setf (getf *global-setfs* (intern (string ',var) :keyword))
                  (lambda () (setf ,var ,val))))))

;;; modes

;; debug mode, default nil
(defglobal *debug* nil)
;; ;; paused
;; (defglobal *paused* nil)

;;; screen constants
;; not really much use, just keeps value holders
(defglobal *width* 800)
(defglobal *height* 600)

;;; delta time
;; *DT* keeps track of the time since last frame, in seconds
;; *PREVIOUS-TIME* gives the time, in seconds, of the previous frame
;; since the start of the program, and (glfw:get-time) returns the
;; current time
(defconstant +max-fps+ 150)
(defglobal *dt* 0.02d0)
(defglobal *previous-time* 0.0)

;; (defglobal *previous-dt* 0.01)
;; (defglobal *average-dt* 0.01)

;;; time travel

(defenum:defenum *enum-time-travel-state* ((+time-play+ 0)
                                           +time-paused+
                                           +time-rewind+
                                           +time-forward+))

(defglobal *time-travel-state* +time-play+)
(defglobal *current-frame* 0)
(defglobal *max-frame-index* 0)
(defglobal *timeline*
    (make-array 500000 :element-type 'list
                       :initial-element nil
                       :adjustable t
                       :fill-pointer 0))

(defglobal *tracked-vars* nil)

;;; actions
;; p-lists that keep track of the current actions on keys and buttons
(defglobal *key-actions* ())
(defglobal *mouse-button-actions* ())
(defglobal *key-pressed* ())
(defglobal *mouse-button-pressed* ())

;;; singletons
;; singletons used for resource management and rendering
(defglobal *program-manager* nil)
(defglobal *texture-manager* nil)
(defglobal *sprite-renderer* nil)

;;systems
;; (defglobal *render-system* nil)
;; (defglobal *physics-system* nil)

;;; cursor position values
(defglobal *cursor-callback-p* nil) ;; cursor has been moved
(defglobal *first-mouse* t) ;; checks if first time cursor has been moved

;; current cursor position
(defglobal *cursor-x* (/ *width* 2.0))
(defglobal *cursor-y* (/ *height* 2.0))

;; previous cursor position
(defglobal *last-x* (/ *width* 2.0))
(defglobal *last-y* (/ *height* 2.0))

;; the scroll wheel has been used
(defglobal *scroll-callback-p* nil)

;; number of ticks of the scroll wheel
(defglobal *scroll-x* (/ *width* 2.0))
(defglobal *scroll-y* (/ *height* 2.0))

;;; entities

(defglobal *entities* (empty-map))
