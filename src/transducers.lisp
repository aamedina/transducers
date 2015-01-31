(in-package :transducers)

(defun map (f)
  (declare (function f))
  (transducing (rf)
    (reducing
      (() (rf))
      ((result) (rf result))
      ((result input) (rf result (funcall f input)))
      ((result input &rest inputs) (rf result (apply f input inputs))))))

(defun cat (rf)
  (declare (function rf))
  (reducing
    (() (funcall rf))
    ((result) (funcall rf result))
    ((result input) (reduce rf input :initial-value result))))

(defun mapcat (f)
  (declare (function f))
  (compose (map f) #'cat))

(defun filter (pred)
  (declare (function pred))
  (transducing (rf)
    (reducing
      (() (rf))
      ((result) (rf result))
      ((result input)
       (if (funcall pred input)
           (rf result input)
           result)))))

(defun remove (pred)
  (filter (complement pred)))

(defun take (n)
  (transducing (rf)
    (let ((n n))
      (declare (fixnum n))
      (reducing
        (() (rf))
        ((result) (rf result))
        ((result input)
         (let* ((old n)
                (new (setf n (dec n)))
                (result (if (plusp old)
                            (rf result input)
                            result)))
           (if (not (plusp new))
               (return result)
               result)))))))

(defun take-while (pred)
  (declare (function pred))
  (transducing (rf)
    (reducing
      (() (rf))
      ((result) (rf result))
      ((result input)
       (if (funcall pred input)
           (rf result input)
           (return result))))))

(defun drop (n)
  (transducing (rf)
    (let ((n n))
      (declare (fixnum n))
      (reducing
        (() (rf))
        ((result) (rf result))
        ((result input)
         (let* ((old n))
           (setf n (dec n))
           (if (plusp old)
               result
               (rf result input))))))))

(defun drop-while (pred)
  (declare (function pred))
  (transducing (rf)
    (let ((continue t))
      (reducing
        (() (rf))
        ((result) (rf result))
        ((result input)
         (if (and continue (funcall pred input))
             result
             (progn
               (setf continue nil)
               (rf result input))))))))

(defun take-nth (n)
  (declare (fixnum n))
  (transducing (rf)
    (let ((i -1))
      (declare (fixnum i))
      (reducing
        (() (rf))
        ((result) (rf result))
        ((result input)
         (setf i (inc i))
         (if (zerop (rem i n))
             (rf result input)
             result))))))

(defun keep (f)
  (declare (function f))
  (transducing (rf)
    (reducing
      (() (rf))
      ((result) (rf result))
      ((result input)
       (if-let ((v (funcall f input)))
         (rf result v)
         result)))))

(defun keep-indexed (f)
  (declare (function f))
  (transducing (rf)
    (let ((i -1))
      (declare (fixnum i))
      (reducing
        (() (rf))
        ((result) (rf result))
        ((result input)
         (setf i (inc i))
         (if-let ((v (funcall f i input)))
           (rf result v)
           result))))))

(defun dedupe (&optional (test #'equal))
  (declare (function test))
  (transducing (rf)
    (let ((prior (gensym)))
      (reducing
        (() (rf))
        ((result) (rf result))
        ((result input)
         (let ((prev-prior prior))
           (setf prior input)
           (if (funcall test prev-prior input)
               result
               (rf result input))))))))

(defun random-sample (prob)
  (declare (float prob))
  (filter (lambda (x)
            (declare (ignore x))
            (< (random 1.0) prob))))

(defun completing (f &optional (cf #'identity))
  (declare (function f cf))
  (reducing
    (() (funcall f))
    ((result) (funcall cf result))
    ((result input) (funcall f result input))))

(define-compiler-macro transduce (xf f coll &optional (init nil initp))
  (if initp
      `(let* ((rf (funcall ,xf ,f)))
         (declare (function rf))
         (funcall rf (reduce rf ,coll :initial-value ,init)))
      `(transduce ,xf ,f ,coll (funcall ,f))))

(defun transduce (xf f coll &optional (init nil initp))
  (declare (function xf f) (sequence coll) (optimize speed (debug 0)))
  (if initp
      (let* ((rf (funcall xf f)))
        (declare (function rf))
        (funcall rf (reduce rf coll :initial-value init)))
      (transduce xf f coll (funcall f))))

(defun sequence (xf coll &rest colls)
  (declare (function xf) (sequence coll))
  (transduce xf (completing (lambda (result input)
                              (vector-push-extend input result)
                              result))
             (if colls (cons coll colls) coll)
             (make-array (length coll) :adjustable t :fill-pointer 0)))
