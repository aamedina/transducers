(in-package :transducers)

(define-compiler-macro juxt (&rest more)
  (with-gensyms (args)
    `(lambda (&rest ,args)
       (declare (optimize speed (debug 0) (safety 0)))
       (vector ,@(mapcar (lambda (f)
                           `(apply ,f ,args))
                         more)))))

(defun juxt (&rest more)
  (declare (optimize speed (debug 0) (safety 0)))
  (lambda (&rest args)
    (declare (optimize speed (debug 0) (safety 0)))
    (let ((v (make-array (length more) :fill-pointer 0)))
      (dolist (f more)
        (declare (function f))
        (vector-push (apply f args) v))
      v)))

(define-compiler-macro partial (f &rest more)
  `(lambda (&rest args)
     (declare (optimize speed (debug 0) (safety 0)))
     (apply ,f ,@more args)))

(defun partial (f &rest more)
  (declare (function f))
  (lambda (&rest args)
    (declare (optimize speed (debug 0) (safety 0)))
    (apply f (concatenate 'list more args))))

(define-compiler-macro comp (&rest more)
  (destructuring-bind (&optional f g &rest more)
      more
    (cond (more `(comp (comp ,f ,g) ,@more))
          (g `(lambda (&rest args)
                (declare (function ,@(when (symbolp f) (list f))
                                   ,@(when (symbolp g) (list g)))
                         (optimize speed (debug 0) (safety 0)))
                (funcall ,f (apply ,g args))))
          (f f)
          (t #'identity))))

(defun comp (&rest more)
  (declare (optimize speed (debug 0) (safety 0)))
  (apply #'compose more))

(defun range (start &optional (end 0 endp) (step 1))
  (declare (optimize speed (debug 0) (safety 0))
           (fixnum start end step))
  (when-not (= start end)
    (let ((start (if endp start end))
          (end (if endp end start)))
      (loop for n from start below end by step collect (the fixnum n)))))

(defun concat (x y &rest more)
  (declare (optimize speed (debug 0) (safety 0)))
  (if more
      (concat (concatenate 'list x y) more)
      (concatenate 'list x y)))

(defun repeat (x n)
  (declare (fixnum n) (optimize speed (debug 0) (safety 0)))
  (loop for n from 0 below n collect x))
