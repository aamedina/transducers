(in-package :transducers)

(defgeneric seq (o)
  (declare (optimize speed (space 0)))
  (:method ((o sequence)) o))

(defgeneric -conj (coll value)
  (declare (optimize speed (space 0)))
  (:method ((coll sequence) value) (cons value (seq coll))))

(defgeneric -conj! (tcoll value)
  (declare (optimize speed (space 0))))

(defmethod seq ((s simple-string))
  (declare (optimize speed (safety 0) (debug 0) (space 0)))
  (loop for ch across s collect ch))

(defmethod seq ((a simple-vector))
  (declare (optimize speed (safety 0) (debug 0) (space 0)))
  (loop for x across a collect x))

(defmethod seq ((hash-table hash-table))
  (declare (optimize speed (safety 0) (debug 0) (space 0)))
  (when (plusp (hash-table-count hash-table))
    (loop
      for k being the hash-keys in hash-table
      for v being the hash-values in hash-table
      collect (cons k v))))

