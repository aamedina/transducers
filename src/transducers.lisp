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

(defmethod seq ((hash-table hash-table))
  (declare (optimize speed (safety 0) (debug 0) (space 0)))
  (when (plusp (hash-table-count hash-table))
    (loop
      for k being the hash-keys in hash-table
      for v being the hash-values in hash-table
      collect (cons k v))))

(defgeneric coll-reduce (coll f val)
  (declare (optimize speed (space 0))))

(defmethod coll-reduce ((coll null) f val)
  (declare (function f) (optimize speed (safety 0) (debug 0) (space 0)))
  (or val (funcall f)))

(defmethod coll-reduce ((coll sequence) f val)
  (declare (function f) (optimize speed (safety 0) (debug 0) (space 0)))
  (if val
      (cl:reduce f coll :initial-value val)
      (cl:reduce f coll)))

(define-compiler-macro reduce (f coll &optional (val nil providedp))
  (if providedp
      `(coll-reduce ,val ,f ,coll)
      `(coll-reduce ,coll ,f nil)))

(defun reduce (f coll-or-init &optional (init-or-coll nil providedp))
  (declare (function f) (optimize speed (safety 0) (debug 0) (space 0)))
  (if providedp
      (coll-reduce init-or-coll f coll-or-init)
      (coll-reduce coll-or-init f init-or-coll)))

(defun comp (&optional f g &rest more)
  (cond (more (reduce #'comp (comp f g) more))
        (g (lambda (&rest args) (funcall f (apply g args))))
        (f f)
        (t #'identity)))
