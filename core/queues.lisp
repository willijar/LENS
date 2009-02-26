;;;; Priority Queues as Binary Heaps in the CLR style
;;;; (Cormen, Leiserson and Rivest, "Introduction to Algorithms",
;;;; pp. 140--152, MIT Press).
;;;; Implementation derived from Priority Queues by
;;;; Marco Antoniotti at New York University which was Copyright (c) 1992
;;;; with the license given  below
;;;; Changes Copyright (C) 2003-2005
;;;;    John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;; The main changes are a repackaging to use a generic interface so that
;;;; different implementations can be substituted for each other
;;;; And to provide better error handling and restarts, sacrificing speed
;;;; for generality
;;;; $Id: queues.lisp,v 1.2 2005/11/29 09:03:17 willijar Exp $


(in-package :queues)

(defparameter +standard-heap-allocation-size+ 16
  "The standard allocation block size for the underlying priority
queue rep.")

(defparameter +standard-extend-size+ 16
  "The standard size by which the underlying priority queue rep. is
augmented.")

(define-condition empty-error (simple-error)
  ((pq :reader pq :initarg :pq))
  (:report (lambda(cnd strm)
	     (declare (stream strm))
	     (format strm "Queue Empty in ~A" (pq cnd))))
  (:documentation
   "Error signaled when an operation is tried on an empty priority queue"))

(define-condition overflow (storage-condition)
  ((pq :reader pq :initarg :pq))
  (:report (lambda (cnd strm)
	     (declare (stream strm))
	     (format strm "Queue overflow in ~A" (pq cnd))))
  (:documentation
   "Condition signaled by 'insert' when tried on a full and non adjustable
priority queue"))

;;; Public Interface

(defgeneric head(q)
  (:documentation "Returns the 'head' of the priority queue."))

(defgeneric extract-head(q)
  (:documentation "Removes the head element from a queue
and returns the element - and a flag empty-p"))

(defgeneric insert(elem q)
  (:documentation "Inserts an element into the queue"))

(defgeneric size(q)
  (:documentation "Return the size of the q"))

(defgeneric empty-p(q)
  (:documentation "Return true if empty")
  (:method(q) (= 0 (size q))))

(defgeneric lookup (item q &key test key)
  (:documentation "search for an element in the queue
that satisfies the test"))

(defgeneric extract(item q)
  (:documentation "Remove a specific item from the queue"))

(defclass queue ()
  ((front :type list :accessor front :initform nil)
   (back :type cons :accessor back :initform nil))
  (:documentation "A FIFO queue"))

(defmethod size((q queue)) (length (front q)))

(defmethod reset((q queue))
  (setf (front q) nil
        (back q) nil))

(defmethod print-object ((q queue) strm)
  "Print function for the internal priority queue data structure."
  (print-unreadable-object (q strm :type t :identity t)
    (format strm "length ~D" (length (front q)))))

(defmethod insert (elem (q queue))
  (let ((new-cons (cons elem nil)))
    (cond ((front q)
           (rplacd (back q) new-cons)
           (setf (back q) new-cons))
          (t (setf (front q) new-cons)
             (setf (back q) new-cons)))))

(defmethod head((q queue)) (car (front q)))

(defmethod empty-p((q queue)) (not (front q)))

(defmethod extract-head((q queue))
  (let ((front (front q)))
    (unless front (values nil t))
    (setf (front q) (rest front))
    (car front)))

(defmethod lookup (item (q queue) &key (test #'eql) key)
  (find item (front q) :test test :key key))

(defmethod extract(item (q queue))
  (let ((last (first (back q))))
    (setf (front q) (delete item (front q)))
    (when (eql item last)
      (setf (back q) (last (front q))))))

(defun make-queue ()
  "Creates a FIFO queue internal structure."
  (make-instance 'queue))

(defclass priority-queue ()
  ((heap :accessor heap :type vector
	 :documentation "The heap vector")
   (extend-size :initform +standard-extend-size+ :reader extend-size
		:type fixnum :initarg :extend-by
		:documentation "the resize value")
   (original-size :initform +standard-heap-allocation-size+ :initarg :size
		  :type fixnum :reader original-size
		  :documentation
		  "The original (and minimum) size for this queue")
   (key-fun :initform #'identity :reader key-fun :initarg :key
	    :documentation
	    "the function used for access the 'key' of an element")
   (comp-fun :initform #'> :reader comp-fun :initarg :test
	     :documentation "the comparison function")))

(defmethod initialize-instance :after ((pq priority-queue) &key
				       (element-type t)
				       (adjustable t)
				       &allow-other-keys)
  (setf (heap pq)
        (make-array (original-size pq)
                    :fill-pointer 0
                    :element-type element-type
                    :adjustable adjustable)))

(declaim (inline element-type allocation))
(defun element-type (pq)  (array-element-type (heap pq)))
(defun allocation (pq) (the fixnum (first (array-dimensions (heap pq)))))
(defmethod size ((pq priority-queue)) (fill-pointer (heap pq)))
(defmethod empty-p ((pq priority-queue)) (< (size pq) 1))
(defmethod reset((q priority-queue)) (setf (fill-pointer (heap q)) 0))

(defmethod head ((pq priority-queue))
  "Returns the 'head' of the priority queue."
  (if (empty-p pq)
      nil
      (aref (heap pq) 1)))

(defmethod print-object ((pq priority-queue) strm)
  "Print function for the internal priority queue data structure."
  (print-unreadable-object (pq strm :type t :identity t)
    (format
     strm
     "size ~D, element type ~S"
     (size  pq)
     (array-element-type (heap pq)))))

;;; Auxiliary functions for the array implementation.

(declaim (inline left right parent shrink-physical-heap))
(defun parent (i) (declare (fixnum i)) (floor i 2))
(defun right (i) (declare (fixnum i)) (the fixnum (1+ (* 2 i))))
(defun left (i) (declare (fixnum i)) (the fixnum (* 2 i)))
(defun shrink-physical-heap (new-heap-size phys-heap pq)
  "Physically shrink the size of the heap if allowed, but
 never below the originally specified size. returns the physical heap"
  (declare (fixnum new-heap-size))
  (declare (type (array t *) phys-heap))
  (when (and (adjustable-array-p phys-heap)
             (> new-heap-size (original-size pq))
             (<= new-heap-size
                 (floor (the fixnum (first (array-dimensions phys-heap))) 4)))
    (adjust-array phys-heap
                  (floor (the fixnum (first (array-dimensions phys-heap))) 2)))
  phys-heap)

;;; insert t pq => t
;;; For simplicity it is better not to use the first element of the array.
;;; See CLR.

(defmethod insert (elem (pq priority-queue))
  "Inserts an element elem in the priority queue pq"
  (let ((true-heap (heap pq))
        (keyfun (key-fun pq))
        (compfun (comp-fun pq)))
    (declare (function keyfun compfun))
    ;; Actually increment the size of the heap via 'incf'.
    (let ((fp (1+ (fill-pointer true-heap))))
      (declare (fixnum fp))
      ;; Extend the heap if allowed.
      (when (>= fp (first (array-dimensions true-heap)))
        (if (adjustable-array-p true-heap)
            (adjust-array true-heap (+ fp (extend-size pq)))
            (error 'overflow :pq pq)))
      (setf (fill-pointer true-heap) fp)
      (do ((i fp (parent i)))
          ((or (<= i 1)
               (funcall compfun
                        (funcall keyfun (aref true-heap (parent i)))
                        (funcall keyfun elem)))
           (setf (aref true-heap i) elem)
           elem)
        (declare (integer i))
        (setf (aref true-heap i)
              (aref true-heap (parent i)))))))

(defmethod extract-head ((pq priority-queue))
  "Removes the head element from the priority queue, returning it and
readjusts the contents."
  (if (empty-p pq)
      (values nil t)
      (prog1
          (aref (heap pq) 1)
        (pq-delete 1 pq))))


(defmethod extract(element (pq priority-queue))
  "Removes the specified element from a priority queue, returning it and
readjusting the contents. Returns nil if no item found"
  (do*((true-heap (heap pq))
       (end (length true-heap))
       (i 1 (1+ i))
       (item (aref true-heap i) (aref true-heap i)))
      ((> i end))
    (when (eql item element)
      (pq-delete i pq)
      (return item))))

(defmethod extract((test function) (pq priority-queue))
  "Extract one element meeting test from the priority queue, returns nil
if none found"
  (do*((true-heap (heap pq))
       (end (length true-heap))
       (i 1 (1+ i))
       (item (aref true-heap i) (aref true-heap i)))
      ((> i end))
    (when (funcall test item)
      (pq-delete i pq)
      (return item))))

(defun pq-delete (elem-heap-pos pq)
  "Deletes the element by accessing its position in the
priority queue. The priority queue is restructured and returned."
  (declare (fixnum elem-heap-pos))
  (let* ((true-heap (heap pq)))
    (declare (type (array t *) true-heap))
    (setf (aref true-heap elem-heap-pos)
          (aref true-heap (fill-pointer true-heap)))
    ;; Shrink the heap if allowed. The call to 'decf' is the
    ;; "logical" shrinking of the heap.
    (shrink-physical-heap (decf (fill-pointer true-heap)) true-heap pq)
    (heapify pq elem-heap-pos)
    pq))

(defun pq-find-by-key(pq key &key (equality #'=))
  (let* ((true-heap (heap pq))
         (keyfun (key-fun pq))
         (compfun (comp-fun pq))
         (fp (fill-pointer true-heap)))
    (declare (function keyfun compfun equality) (vector true-heap)
             (fixnum fp))
    (flet((check-element(index)
              (declare (fixnum index))
              (let* ((element (aref true-heap index))
                     (element-key (funcall keyfun element)))
                (when (funcall equality key element-key)
                  (return-from pq-find-by-key (values element index)))
                (funcall compfun element-key key))))
      (labels ((check-lr(index)
                 (declare (fixnum index))
                 (when (check-element index)
                   (check-lr (left index))
                   (check-lr (right index)))))
      (let ((parent
             (do ((i fp (parent i)))
                 ((or (< i 1) (check-element i)) i)
               (declare (fixnum i)))))
        (check-lr (left parent))
        (check-lr (right parent)))))))

(defun heapify (pq index)
  "Readjusts the heap property (see CLR)."
  (declare (fixnum index))
  (let ((true-heap (heap pq))
        (keyfun (key-fun pq))
        (compfun (comp-fun pq)))
    (declare (function keyfun compfun) (vector true-heap))
    ;; It might be more efficient. It is with CMULISP 16c.
    (labels
        ((do-heapify (index)
           (let ((l (left index))
                 (r (right index))
                 (largest 0))		; 0 is very convenient!
             (declare (fixnum l r largest))
             ;; Figure out which "son" is the largest -- 'largest'
             ;; will contain its index in the array/heap.
             (if (and (<= l (fill-pointer true-heap))
                      (funcall compfun
                               (funcall keyfun (aref true-heap l))
                               (funcall keyfun (aref true-heap index))))
                 (setq largest l)
                 (setq largest index))
             (when (and (<= r (fill-pointer true-heap))
                        (funcall compfun
                                 (funcall keyfun (aref true-heap r))
                                 (funcall keyfun (aref true-heap largest))
                                 ))
               (setq largest r))
             ;; Exchange elements if necessary -- 'rotatef' does this.
             (when (/= largest index)
               (rotatef (aref true-heap index) (aref true-heap largest))
               (do-heapify largest) )))	)
      (do-heapify index))))

(defmethod lookup (item (pq priority-queue)
                   &key (key (key-fun pq)) (test #'eql))
  (find item (heap pq)
        :key key
        :test test
        :start 1
        :end (length (heap pq))))

