(defpackage :transducers
  (:use :common-lisp :alexandria)
  (:shadow :map :reduce :first :rest))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (proclaim '(optimize speed)))



