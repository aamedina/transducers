(defpackage :transducers
  (:use :common-lisp)
  (:shadow :map :reduce))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (proclaim '(optimize speed)))

