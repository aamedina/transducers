(defpackage :transducers
  (:use :common-lisp :alexandria :sb-ext :sb-thread)
  (:shadow :define-constant :map :remove :replace :first :rest))

(proclaim '(optimize speed))

(unlock-package :common-lisp)
