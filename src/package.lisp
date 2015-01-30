(defpackage :transducers
  (:use :common-lisp :alexandria)
  (:shadow :map :reduce))

(proclaim '(optimize speed))

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
