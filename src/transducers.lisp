(in-package :transducers)

(defmacro reducing (rf &body body)
  (destructuring-bind ((&rest b0)
                       (a1 &rest b1)
                       (a2 &rest b2)) body
    (with-gensyms (result input resultp inputp)
      `(flet (,(when (typep rf '(or symbol list))
                     `(,rf (&rest args) (apply ,rf args))))
         (lambda (&optional (,result nil ,resultp) (,input nil ,inputp))
           (declare (optimize speed (debug 0) (safety 0)))
           (block nil
             (cond (,inputp (let ((,(car a2) ,result)
                                  (,(cadr a2) ,input))
                              ,@b2))
                   (,resultp (let ((,(car a1) ,result))
                               ,@b1))
                   (t ,@b0))))))))

(defmacro reducing-map (rf &body body)
  (destructuring-bind ((&rest b0)
                       (a1 &rest b1)
                       (a2 &rest b2)
                       (a3 &rest b3)) body
    (with-gensyms (result input resultp inputp inputs inputsp)
      `(flet (,(when (typep rf '(or symbol list))
                     `(,rf (&rest args) (apply ,rf args))))
         (lambda (&optional
                    (,result nil ,resultp)
                    (,input nil ,inputp)
                    &rest (,inputs nil ,inputsp))
           (declare (optimize speed (debug 0) (safety 0)))
           (block nil
             (cond (,inputsp (let ((,(car a3) ,result)
                                   (,(cadr a3) ,input)
                                   (,(last a3) ,inputs))
                               ,@b3))
                   (,inputp (let ((,(car a2) ,result)
                                  (,(cadr a2) ,input))
                              ,@b2))
                   (,resultp (let ((,(car a1) ,result))
                               ,@b1))
                   (t ,@b0))))))))

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

;; (defun interleave (&optional c1 c2 &rest more)
;;   (cond (more (let ((ss (map #'seq (list* c1 c2 more))))
;;                 (concat (map #'first ss)
;;                         (apply #'interleave (map #'next ss)))))
;;         (c2 (when-let ((s1 (seq c1))
;;                        (s2 (seq c2)))
;;               (cons (first s1)
;;                     (cons (first s2) (interleave (rest s1) (rest s2))))))
;;         (c1 c1)
;;         (t nil)))

(defun map (f)
  (declare (function f))
  (lambda (rf)
    (reducing-map rf
      (() (rf))
      ((result) (rf result))
      ((result input)
       (rf result (funcall f input)))
      ((result input inputs)
       (rf result (apply f input inputs))))))

(defun comp (&optional f g &rest more)
  (declare ((or function null) f g))
  (cond (more (reduce #'comp more (comp f g)))
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

(defun range (start &optional end step)
  (declare (type (or fixnum null) start end step))
  (cond (step (when (not (= start end))
                (loop
                  for i from start below end by step
                  collect i)))
        (end (range start end 1))
        (start (range 0 start 1))
        (t (range 0 most-positive-fixnum 1))))

(defun take (n)
  (declare (fixnum n))
  (lambda (rf)
    (let ((n n))
      (declare (fixnum n))
      (reducing rf
        (() (rf))
        ((result) (rf result))
        ((result input)
         (let* ((nn (setf n (dec n)))
                (result (if (plusp n)
                            (rf result input)
                            result)))
           (if (zerop nn)
               (return result)
               result)))))))

(defun take-while (pred)
  (declare (function pred))
  (lambda (rf)
    (reducing rf
      (() (rf))
      ((result) (rf result))
      ((result input)
       (if (funcall pred input)
           (rf result input)
           (return result))))))

(defun drop (n)
  (declare (fixnum n))
  (lambda (rf)
    (let ((n n))
      (declare (fixnum n))
      (reducing rf
        (() (rf))
        ((result) (rf result))
        ((result input)
         (let ((prev n))
           (setf n (dec n))
           (if (plusp prev)
               result
               (rf result input))))))))

(defun drop-while (pred)
  (declare (function pred))
  (lambda (rf)
    (let ((dropp t))
      (reducing rf
        (() (rf))
        ((result) (rf result))
        ((result input)
         (if (and dropp (funcall pred input))
             result
             (progn
               (setf dropp nil)
               (rf result input))))))))

(defun repeat (x n)
  (declare (fixnum n))
  (loop
    for i from 1 to n
    collect x))

(defun completing (f &optional (cf #'identity))
  (declare (function f cf))
  (reducing f
    (() (f))
    ((result) (funcall cf result))
    ((result input) (f result input))))

(defun concat (&optional x y &rest more)
  (cond (more (apply #'concat (concat x y) more))
        (y (if x
               (cons (car x) (concat (cdr x) y))
               y))
        (x x)
        (t nil)))

(defun cat (rf)
  (reducing rf
    (() (rf))
    ((result) (rf result))
    ((result input) (rf result input))))

(defun mapcat (f)
  (comp (map f) #'cat))

(defun partition-all (n)
  (declare (fixnum n))
  (lambda (rf)
    (let ((vec (make-array 0 :adjustable t)))
      (reducing rf
        (() (rf))
        ((result)
         (if (emptyp vec)
             (rf result)
             (rf (rf result vec))))
        ((result input)
         (push vec input)
         (if (= n (length vec))
             (rf result vec)
             result))))))

(defun filter (pred)
  (declare (function pred))
  (lambda (rf)
    (reducing rf
      (() (rf))
      ((result) (rf result))
      ((result input)
       (if (funcall pred input)
           (rf result input)
           result)))))

(defun transduce (xf f coll &optional (init (funcall f)))
  (let* ((f (funcall xf f))
         (ret (coll-reduce coll f init)))
    (funcall f ret)))
