(in-package :transducers)

(eval-when (:execute)
  (defconstant no-edit (gensym "no-edit"))
  (defconstant empty-node (cons no-edit (make-array 32))))

(defclass persistent-vector (standard-object sequence)
  ((count :type fixnum :initform 0 :initarg :count :accessor :count)
   (shift :type fixnum :initform 5 :initarg :shift :accessor :shift)
   (root :type cons :initform empty-node :initarg :root :accessor :root)
   (tail :type simple-array :initform (make-array 0) :initarg :tail
         :accessor :tail)))

(when (not (boundp 'empty-vector))
  (defconstant empty-vector (make-instance 'persistent-vector)))

(defmethod sequence:length ((o persistent-vector))
  (:count o))

(defmethod sequence:elt ((o persistent-vector) index)
  (declare (ignore o index)))

(defmethod (setf sequence:elt) (new-value (o persistent-vector) index)
  (error "Cannot mutate persistent data structures"))

(defmethod sequence:adjust-sequence ((o persistent-vector) length
                                     &key initial-element initial-contents)
  (declare (ignore o length initial-element initial-contents)))

(defmethod sequence:make-sequence-like ((o persistent-vector) length
                                        &key initial-element initial-contents)
  (declare (ignore o length initial-element initial-contents)))

(defmethod sequence:make-sequence-iterator ((o persistent-vector)
                                            &key from-end start end)
  (declare (ignore o from-end start end)))

(defclass transient-vector (standard-object sequence)
  ((count :type fixnum :initform 0 :initarg :count :accessor :count)
   (shift :type fixnum :initform 5 :initarg :shift :accessor :shift)
   (root :type cons :initform empty-node :initarg :root :accessor :root)
   (tail :type simple-array :initform (make-array 0) :initarg :tail
         :accessor :tail)))

(defmethod sequence:length ((o transient-vector))
  (:count o))

(defmethod sequence:elt ((o transient-vector) index)
  (declare (ignore o index)))

(defmethod (setf sequence:elt) (new-value (o transient-vector) index)
  (error "Cannot mutate transient data structures"))

(defmethod sequence:adjust-sequence ((o transient-vector) length
                                     &key initial-element initial-contents)
  (declare (ignore o length initial-element initial-contents)))

(defmethod sequence:make-sequence-like ((o transient-vector) length
                                        &key initial-element initial-contents)
  (declare (ignore o length initial-element initial-contents)))

(defmethod sequence:make-sequence-iterator ((o transient-vector)
                                            &key from-end start end)
  (declare (ignore o from-end start end)))

(defun print-vector (vec stream)
  (declare (ignore vec))
  (princ "[" stream)
  (princ "]" stream))

(defmethod print-object ((object persistent-vector) stream)
  (print-vector object stream))

(defmethod print-object ((object transient-vector) stream)
  (print-vector object stream))
