(in-package :transducers)

(define-compiler-macro bit-not (n)
  `(lognot ,n))

(defun bit-not (n)
  (declare (number n))
  (lognot n))

(define-compiler-macro bit-and (x y &rest more)
  (if more
      `(logand (logand ,x ,y) ,@more)
      `(logand ,x ,y)))

(defun bit-and (x y &rest more)
  (declare (integer x y))
  (if more
      (reduce #'logand more :initial-value (logand x y))
      (logand x y)))

(define-compiler-macro bit-or (x y &rest more)
  (if more
      `(logior (logior ,x ,y) ,@more)
      `(logior ,x ,y)))

(defun bit-or (x y &rest more)
  (declare (integer x y))
  (if more
      (reduce #'logior more :initial-value (logior x y))
      (logior x y)))

(define-compiler-macro bit-xor (x y &rest more)
  (if more
      `(logxor (logxor ,x ,y) ,@more)
      `(logxor ,x ,y)))

(defun bit-xor (x y &rest more)
  (declare (integer x y))
  (if more
      (reduce #'logxor more :initial-value (logxor x y))
      (logxor x y)))

(define-compiler-macro bit-shift-left (x n)
  `(ash ,x ,n))

(defun bit-shift-left (x n)
  (declare (fixnum x n))
  (ash x n))

(define-compiler-macro bit-shift-right (x n)
  `(ash ,x (- ,n)))

(defun bit-shift-right (x n)
  (declare (fixnum x n))
  (ash x (- n)))
