(in-package :clglbo)

(defun build-var (var &optional initform)
  "Creates a list for slot VAR."
  (list var
        :type var
        ;; :accessor (intern (string var))
        :accessor var
        :initarg (intern (string var) :keyword)
        :initform initform))

(defun build-varlist (varlist)
  "Takes a list and creates a list of slot lists."
  (iter (for var in varlist)
    (collect (if (consp var)
                 (build-var (car var) (cadr var))
                 (build-var var)))))

(defmacro defclump (name direct-superclasses (&rest slots))
  `(defclass ,name ,direct-superclasses
     (,@(build-varlist slots))))
