(defpackage :transducers
  (:use :common-lisp :alexandria :sb-ext :sb-thread)
  (:shadow :define-constant :map :remove :replace :first :rest :pop
           :bit-not :bit-and :bit-xor))

(declaim (optimize speed))

(unlock-package :common-lisp)
