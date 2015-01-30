(in-package :transducers)

(defun reducing-lambda-list (lambda-list)
  (destructuring-bind ((result input) optional rest &rest more)
      (-> lambda-list
          (parse-ordinary-lambda-list)
          (multiple-value-list))
    (declare (ignore optional more))
    (with-gensyms (resultp inputp)
      (values `(&optional (,result nil ,resultp)
                          (,input nil ,inputp)
                          ,@(when rest `(&rest ,rest)))
              resultp
              inputp
              rest))))

(defmacro reducing (&body cases)
  (destructuring-bind (lambda-list resultp inputp rest)
      (multiple-value-list (reducing-lambda-list (caar (last cases))))
    `(lambda ,lambda-list
       (declare (optimize speed (safety 0) (debug 0)))
       (block nil
         (cond ,@(when rest (list `(,rest ,@(cdr (nth 3 cases)))))
               (,inputp ,@(cdr (nth 2 cases)))
               (,resultp ,@(cdr (nth 1 cases)))
               (t ,@(cdr (nth 0 cases))))))))

(defmacro transducing (lambda-list &body body)
  (destructuring-bind ((rf &rest rfs) optional rest &rest more)
      (-> lambda-list
          (parse-ordinary-lambda-list)
          (multiple-value-list))
    (declare (ignore rest optional more))
    `(lambda ,lambda-list
       (declare (function ,@(cons rf rfs)))
       (flet (,@(mapcar (lambda (rf)
                          `(,rf (&rest args)
                                (declare (optimize speed (safety 0) (debug 0)))
                                (apply ,rf args)))
                        (cons rf rfs)))
         ,@body))))

(defun map (f)
  (declare (function f))
  (transducing (rf)
    (reducing
      (() (rf))
      ((result) (rf result))
      ((result input) (rf result (funcall f input)))
      ((result input &rest inputs) (rf result (apply f input inputs))))))

(defun cat (rf)
  (reducing
    (() (rf))
    ((result) (rf result))
    ((result input) (reduce rf input :initial-value result))))
