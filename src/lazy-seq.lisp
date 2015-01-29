(in-package :transducers)

(defmacro while (pred &body body)
  `(loop
     while ,pred
     do (progn ,@body)))

;; Lazy sequence type based on clojure.lang.LazySeq

(defstruct lazy-sequence
  sv
  (s nil :type sequence)
  (f nil :type (or null function)))

(defmacro lazy-seq (&body body)
  `(make-lazy-sequence :f (lambda ()
                            (declare (optimize speed (debug 0) (safety 0)))
                            (the (or null sequence) (progn ,@body)))))

(defun sval (o)
  (declare (lazy-sequence o) (optimize speed (debug 0) (safety 0)))
  (when-let (f (lazy-sequence-f o))
    (setf (lazy-sequence-sv o) (funcall f))
    (setf (lazy-sequence-f o) nil)
    (or (lazy-sequence-sv o) (lazy-sequence-s o))))

(defmethod seq ((o lazy-sequence))
  (declare (lazy-sequence o) (optimize speed (debug 0) (safety 0)))
  (sval o)
  (when (lazy-sequence-sv o)
    (let ((ls (lazy-sequence-sv o)))
      (setf (lazy-sequence-sv o) nil)
      (while (typep ls 'lazy-sequence)
        (setf ls (sval ls)))
      (setf (lazy-sequence-s o) (seq ls))))
  (lazy-sequence-s o))
