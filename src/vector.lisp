(in-package :transducers)

(define-constant empty-node (cons nil (make-array 32)))

(defclass persistent-vector (sb-mop:funcallable-standard-object sequence)
  ((count :type fixnum :initform 0 :initarg :count :accessor :count)
   (shift :type fixnum :initform 5 :initarg :shift :accessor :shift)
   (root :type cons :initform empty-node :initarg :root :accessor :root)
   (tail :type array :initform (make-array 0) :initarg :tail :accessor :tail))
  (:metaclass sb-mop:funcallable-standard-class))

(define-constant empty-vector (make-instance 'persistent-vector))

(declaim (inline tailoff))
(defun tailoff (vec)
  (declare ((or persistent-vector transient-vector) vec))
  (let ((cnt (length vec)))
    (declare (fixnum cnt))
    (the fixnum (if (< cnt 32)
                    0
                    (bit-shift-left (bit-shift-right (dec cnt) 5) 5)))))

(declaim (inline array-for))
(defun array-for (vec i)
  (declare ((or persistent-vector transient-vector) vec) ((integer 0 *) i))
  (when (and (>= i 0) (< i (:count vec)))
    (when (>= i (tailoff vec))
      (return-from array-for (:tail vec)))
    (let ((node (:root vec)))
      (loop
        for level from (:shift vec) downto 0 by 5
        do (->> (bit-and (bit-shift-right i level) #x01f)
                (aref (cdr node))
                (setf node)))
      (the array (cdr node)))))

(defmethod sequence:length ((o persistent-vector))
  (:count o))

(defmethod sequence:elt ((o persistent-vector) index)
  (when-let (node (array-for o index))
    (aref node (bit-and index #x01f))))

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

(defclass transient-vector (sb-mop:funcallable-standard-object sequence)
  ((count :type fixnum :initform 0 :initarg :count :accessor :count)
   (shift :type fixnum :initform 5 :initarg :shift :accessor :shift)
   (root :type cons :initform empty-node :initarg :root :accessor :root)
   (tail :type array :initform (make-array 0) :initarg :tail :accessor :tail))
  (:metaclass sb-mop:funcallable-standard-class))

(defmethod sequence:length ((o transient-vector))
  (:count o))

(defmethod sequence:elt ((o transient-vector) index)
  (when-let (node (array-for o index))
    (aref node (bit-and index #x01f))))

(defun ensure-editable (vec)
  (when (null (car (:root vec)))
    (error "Transient used after persistent! call")))

(defun assoc-in! (level node i val)
  (if (zerop level)
      (progn
        (setf (aref (cdr node) (bit-and i #x01f)) val)
        node)
      (let ((subidx (bit-and (bit-shift-right i level) #x01f)))
        (assoc-in! (- level 5) (aref (cdr node) subidx) i val))))

(declaim (inline vec-conj!))
(defun vec-conj! (tcoll val)
  (declare (transient-vector tcoll))
  (let ((i (:count tcoll)))
    (when (< (- i (tailoff tcoll)) 32)
      (setf (aref (:tail tcoll) (bit-and i #x01f)) val)
      (setf (:count tcoll) (1+ i))
      (return-from vec-conj! tcoll))))

(declaim (inline conj!))
(defun conj! (tcoll val)
  (typecase tcoll
    (transient-vector (vec-conj! tcoll val))
    (t (-conj! tcoll val))))

(defmethod (setf sequence:elt) (new-value (o transient-vector) index)
  (cond ((and (>= index 0) (< index (:count o)))
         (if (>= index (tailoff o))
             (progn
               (aref (:tail o) (bit-and index #x01f))
               o)
             (progn
               (setf (:root o) (assoc-in! (:shift o) (:root o) index new-value))
               o)))
        ((= index (:count o))
         (conj! o new-value))))

(defmethod sequence:adjust-sequence ((o transient-vector) length
                                     &key initial-element initial-contents)
  (declare (ignore o length initial-element initial-contents)))

(defmethod sequence:make-sequence-like ((o transient-vector) length
                                        &key initial-element initial-contents)
  (declare (ignore o length initial-element initial-contents)))

(defmethod sequence:make-sequence-iterator ((o transient-vector)
                                            &key from-end start end)
  (declare (ignore o from-end start end)))

(defun editable-root (node)
  (cons *current-thread* (copy-array (cdr node))))

(defun editable-tail (tail)
  (let ((arr (make-array 32)))
    (dotimes (i (length tail))
      (setf (aref arr i) (aref tail i)))
    arr))

(declaim (inline transient))
(defun transient (coll)
  (typecase coll
    (persistent-vector (make-instance 'transient-vector
                                      :count (:count coll)
                                      :shift (:shift coll)
                                      :root (editable-root (:root coll))
                                      :tail (editable-tail (:tail coll))))
    (t coll)))

(declaim (inline persistent!))
(defun persistent! (tcoll)
  (typecase tcoll
    (transient-vector (progn
                        (cas (car (:root tcoll)) (car (:root tcoll)) nil)
                        (make-instance 'persistent-vector
                                       :count (:count tcoll)
                                       :shift (:shift tcoll)
                                       :root (:root tcoll)
                                       :tail (->> (- (:count tcoll)
                                                     (tailoff tcoll))
                                                  (subseq (:tail tcoll))))))
    (t tcoll)))

(defun print-vector (vec stream)
  (declare (ignore vec))
  (princ "[" stream)
  (princ "]" stream))

(defmethod print-object ((object persistent-vector) stream)
  (print-vector object stream))

(defmethod print-object ((object transient-vector) stream)
  (print-vector object stream))

