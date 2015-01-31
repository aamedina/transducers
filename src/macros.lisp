(in-package :transducers)

(define-compiler-macro inc (n)
  `(1+ ,n))

(defun inc (n)
  (declare (number n))
  (1+ n))

(define-compiler-macro dec (n)
  `(1- ,n))

(defun dec (n)
  (declare (number n))
  (1- n))

(defmacro when-not (test &body body)
  `(if ,test nil (progn ,@body)))

(defmacro if-not (test then &optional else)
  `(if ,test ,else ,then))

(defmacro -> (&environment env x &rest forms)
  (if forms
      (let* ((form (car forms))
             (threaded (if (listp form)
                           `(,(car form) ,x ,@(cdr form))
                           (list form x)))
             (mf (macro-function '-> env)))
        (declare (function mf))
        (funcall mf (list* '-> threaded (cdr forms)) env))
      x))

(defmacro ->> (&environment env x &rest forms)
  (if forms
      (let* ((form (car forms))
             (threaded (if (listp form)
                           `(,(car form) ,@(cdr form) ,x)
                           (list form x)))
             (mf (macro-function '->> env)))
        (declare (function mf))
        (funcall mf (list* '->> threaded (cdr forms)) env))
      x))

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

(defmacro delay (&body body)
  (with-gensyms (cached)
    `(let ((,cached nil))
       (lambda ()
         (declare (optimize speed (debug 0) (safety 0) (space 0))
                  ,@(when (and (listp (car body))
                               (eq (caar body) 'declare))
                          (cdar body)))
         (or ,cached (setf ,cached (progn
                                     ,@(if (and (listp (car body))
                                                (eq (caar body) 'declare))
                                           (cdr body)
                                           body))))))))

(define-compiler-macro force (o)
  `(funcall ,o))

(defun force (o)
  (funcall o))

(eval-when (:compile-toplevel)
  (set-macro-character #\@ (lambda (stream ch)
                             (declare (ignore ch))
                             (let ((form (read stream t nil t)))
                               `(force ,form)))))
