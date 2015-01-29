(in-package :transducers)

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

(defun map (f &optional coll &rest more)
  (declare (function f))
  (cond (more (lazy-seq
                (cons (apply f (cons (first coll) (map #'first more)))
                      (apply #'map f (cons (rest coll) (map #'rest more))))))
        (coll (lazy-seq
                (cons (funcall f (first coll)) (map f (rest coll)))))
        (f (lambda (rf) rf))
        (t ())))

(defun comp (&optional f g &rest more)
  (declare ((or function null) f g))
  (cond (more (reduce #'comp (comp f g) more))
        (g (lambda (&rest args)
             (funcall f (apply g args))))
        (f f)
        (t #'identity)))
