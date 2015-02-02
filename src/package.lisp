(defpackage :transducers
  (:use :common-lisp :alexandria :sb-ext :sb-thread)
  (:shadow :define-constant :map :remove :replace :first :rest :pop
           :bit-not :bit-and :bit-xor :vector))

(declaim (optimize speed (debug 0)))

(unlock-package :common-lisp)
