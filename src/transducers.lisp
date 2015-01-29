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

(defun seq-reduce (coll f val)
  (cond (coll (seq-reduce (next coll) f (funcall f val (first coll))))
        (val val)
        (t (funcall f))))

(defmethod coll-reduce (coll f val)
  (declare (function f) (optimize speed (safety 0) (debug 0) (space 0)))
  (when-let (s (seq coll))    
    (seq-reduce s f (if val (funcall f val) (funcall f)))))

(define-compiler-macro reduce (f coll &optional val)
  `(coll-reduce ,coll ,f ,val))

(defun reduce (f coll &optional val)
  (declare (function f) (optimize speed (safety 0) (debug 0) (space 0)))
  (coll-reduce coll f val))

(define-compiler-macro map (&whole e f &optional (coll nil collp) &rest more)
  (declare (ignore f coll))
  (cond (more e)
        (collp e)
        (t e)))

(defun map (f &optional (coll nil collp) &rest more)
  (declare (function f))
  (cond (more (lazy-seq
                (cons (apply f (cons (first coll) (map #'first more)))
                      (apply #'map f (cons (rest coll) (map #'rest more))))))
        (collp (when coll
                 (lazy-seq
                   (cons (funcall f (first coll)) (map f (rest coll))))))
        (f (lambda (rf) rf))
        (t ())))

(defun comp (&optional f g &rest more)
  (declare ((or function null) f g))
  (cond (more (reduce #'comp (comp f g) more))
        (g (lambda (&rest args)
             (funcall f (apply g args))))
        (f f)
        (t #'identity)))

(define-compiler-macro inc (n)
  `(1+ ,n))

(defun inc (n)
  (1+ n))

(define-compiler-macro dec (n)
  `(1- ,n))

(defun dec (n)
  (1- n))

(defun range (&optional start end step)
  (declare (type (or fixnum null) start end step))
  (cond (step (when (not (= start end))
                (lazy-seq
                  (cons start (range (+ start step) end step)))))
        (end (range start end 1))
        (start (range 0 start 1))
        (t (range 0 most-positive-fixnum 1))))

(defun range-vec (&optional start end step)
  (declare (type (or fixnum null) start end step))
  (cond (step (when (not (= start end))
                (lazy-seq
                  (cons start (range (+ start step) end step)))))
        (end (range start end 1))
        (start (range 0 start 1))
        (t (error "Unbounded range-vecs are disallowed"))))

(defun iterate (f x)
  (declare (function f))
  (cons x (lazy-seq (iterate f (funcall f x)))))

(defun take (n coll)
  (declare (fixnum n))
  (when (and (plusp n) coll)
    (lazy-seq
      (cons (first coll) (take (dec n) (rest coll)))) ))

(defun take-while (pred coll)
  (declare (function pred))
  (when-let (x (and (funcall pred (first coll)) (first coll)))
    (lazy-seq
      (cons x (take-while pred (rest coll))))))

(defun drop (n coll)
  (declare (fixnum n))
  (if (plusp n)
    (lazy-seq
      (when (seq coll)
        (drop (dec n) (rest coll))))
    coll))

(defun drop-while (pred coll)
  (declare (function pred))
  (when-let (x (and (funcall pred (first coll)) (first coll)))
    (lazy-seq
      (cons x (take-while pred (rest coll))))))

(defun repeat (x &optional (n most-positive-fixnum providedp))
  (declare (fixnum n))
  (when (plusp n)
    (if providedp
        (take n (repeat x))
        (lazy-seq
          (cons x (repeat x))))))
