(in-package :transducers)

(defgeneric seq (o)
  (declare (optimize speed (space 0))))

(defmethod seq ((o sequence))
  (declare (optimize speed (safety 0) (debug 0) (space 0)))
  o)

(defmethod seq ((s simple-string))
  (declare (optimize speed (safety 0) (debug 0) (space 0)))
  (loop
    for ch across s collect ch))

(defmethod seq ((o hash-table))
  (declare (optimize speed (safety 0) (debug 0) (space 0)))
  (loop
    for k being the hash-keys in o
    for v being the hash-values in o
    collect (cons k v)))




