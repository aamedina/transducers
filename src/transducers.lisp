(in-package :transducers)

(defgeneric seq (o)
  (declare (optimize speed (space 0))))

(defmethod seq ((o sequence))
  (declare (optimize speed (safety 0) (debug 0) (space 0)))
  o)

(define-compiler-macro seq (&whole form o)
  (if (constantp o)
      (funcall #'seq o)
      form))

