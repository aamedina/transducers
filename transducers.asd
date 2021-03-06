(asdf:defsystem :transducers
  :description "Transducers for Common Lisp"
  :author "Adrian Medina <adrian.medina@mail.yu.edu>"
  :license "MIT"
  :depends-on (:alexandria)
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "bitwise")
               (:file "macros")
               (:file "seq")
               (:file "functional")
               (:file "vector")
               (:file "transducers")))

