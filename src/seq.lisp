(in-package :transducers)

(defgeneric seq (o)
  (declare (optimize speed (space 0))))

(defmethod seq ((o sequence))
  (declare (optimize speed (safety 0) (debug 0) (space 0)))
  o)

(defmethod seq ((s simple-string))
  (declare (optimize speed (safety 0) (debug 0) (space 0)))
  (loop for ch across s collect ch))

(defmethod seq ((hash-table hash-table))
  (declare (optimize speed (safety 0) (debug 0) (space 0)))
  (when (plusp (hash-table-count hash-table))
    (loop
      for k being the hash-keys in hash-table
      for v being the hash-values in hash-table
      collect (cons k v))))
