(defpackage :transducers
  (:use :common-lisp :alexandria :sb-ext :sb-thread)
  (:shadow :define-constant :map :remove :replace :first :rest
           :bit-not :bit-and :bit-xor))

(proclaim '(optimize speed))

(unlock-package :common-lisp)
