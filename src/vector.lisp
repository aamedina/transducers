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
  (let ((cnt (:count vec)))
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

(defun ensure-editable (vec &optional (node nil nodep))
  (if nodep
      (if (eq (car node) (car (:root vec)))
          node
          (cons (car (:root vec)) (copy-array (cdr node))))
      (when (null (car (:root vec)))
        (error "Transient used after persistent! call"))))

(defun assoc-in! (level node i val)
  (if (zerop level)
      (progn
        (setf (aref (cdr node) (bit-and i #x01f)) val)
        node)
      (let ((subidx (bit-and (bit-shift-right i level) #x01f)))
        (assoc-in! (- level 5) (aref (cdr node) subidx) i val))))

(defun push-tail (vec level parent tailnode)
  (setf parent (ensure-editable parent))
  (let* ((subidx (-> (bit-shift-right (dec (:count vec)) level)
                     (bit-and #x01f)))
         (ret parent)
         (node-to-insert (if (= level 5)
                             tailnode
                             (if-let (child (aref (cdr parent) subidx))
                               (push-tail vec (- level 5) child tailnode)
                               (new-path (car (:root vec)) (- level 5)
                                         tailnode)))))
    (setf (aref (cdr ret) subidx) node-to-insert)
    ret))

(defun new-path (edit level node)
  (if (zerop level)
      node
      (let ((ret (cons edit (make-array 32))))
        (setf (aref (cdr ret) 0) (new-path edit (- level 5) node))
        ret)))

(declaim (inline vec-conj!))
(defun vec-conj! (tcoll val)
  (declare (transient-vector tcoll))
  (let ((i (:count tcoll)))
    (if (< (- i (tailoff tcoll)) 32)
        (progn
          (setf (aref (:tail tcoll) (bit-and i #x01f)) val)
          (setf (:count tcoll) (inc i)))
        (let ((newroot nil)
              (tailnode (cons (car (:root tcoll)) (:tail tcoll)))
              (tail (setf (:tail tcoll) (make-array 32)))
              (newshift (:shift tcoll)))
          (setf (aref tail 0) val)
          (if (> (bit-shift-right (:count tcoll) 5) (bit-shift-left 1 newshift))
              (progn
                (setf newroot (cons (car (:root tcoll)) (make-array 32)))
                (setf (aref (cdr newroot) 0) (:root tcoll))
                (setf (aref (cdr newroot) 1) (new-path (car (:root tcoll))
                                                       (:shift tcoll)
                                                       tailnode))
                (setf newshift (+ newshift 5)))
              (setf newroot (push-tail tcoll (:shift tcoll) (:root tcoll)
                                       tailnode)))
          (setf (:root tcoll) newroot)
          (setf (:shift tcoll) newshift)
          (setf (:count tcoll) (inc i))))
    (the transient-vector tcoll)))

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

(defun pop-tail (tcoll level node)
  (setf node (ensure-editable node))
  (let ((subidx (bit-and (bit-shift-right (- (:count tcoll) 2) level) #x01f)))
    (cond ((> level 5) (let ((newchild (pop-tail tcoll (- level 5)
                                                 (aref (cdr node) subidx))))
                         (if (and (null newchild) (zerop subidx))
                             nil
                             (progn
                               (setf (aref (cdr node) subidx) newchild)
                               node))))
          ((= subidx 0) nil)
          (t (setf (aref (cdr node) subidx) nil)
             node))))

(defun editable-array-for (tcoll i)
  (if (and (>= i 0) (< i (:count tcoll)))
      (if (>= i (tailoff tcoll))
          (:tail tcoll)
          (let ((node (:root tcoll)))
            (loop
              for level from (:shift tcoll) downto 0 by 5
              do (->> (bit-and (bit-shift-right i level) #x01f)
                      (aref (cdr node))
                      (ensure-editable tcoll)
                      (setf node)))
            (cdr node)))
      (error "Index out of bounds")))

(defun pop! (tcoll)
  (ensure-editable tcoll)
  (let ((cnt (:count tcoll)))
    (cond ((zerop cnt) (error "Can't pop empty vector"))
          
          ((= cnt 1) (setf (:count tcoll) 0) tcoll)

          ((plusp (bit-and (dec cnt) #x01f))
           (setf (:count tcoll) (dec cnt)) tcoll)

          (t (let* ((newtail (editable-array-for tcoll (- cnt 2)))
                    (newroot (pop-tail tcoll (:shift tcoll) (:root tcoll)))
                    (newshift (:shift tcoll)))
               (when (null newroot)
                 (setf newroot (cons (car (:root tcoll)) (make-array 32))))
               (when (and (> (:shift tcoll) 5) (null (aref (cdr newroot) 1)))
                 (setf newroot (ensure-editable tcoll (aref (cdr newroot) 0)))
                 (setf newshift (- newshift 5)))
               (setf (:root tcoll) newroot)
               (setf (:shift tcoll) newshift)
               (setf (:count tcoll) (dec cnt))
               (setf (:tail tcoll) newtail)
               tcoll)))))

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
  (princ "[" stream)
  (dotimes (i (dec (length vec)))
    (prin1 (elt vec i) stream)
    (princ #\space stream))
  (when (plusp (length vec))
    (prin1 (elt vec (dec (length vec))) stream))
  (princ "]" stream))

(defmethod print-object ((object persistent-vector) stream)
  (print-vector object stream))

(defmethod print-object ((object transient-vector) stream)
  (print-vector object stream))

