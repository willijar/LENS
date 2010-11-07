;; Interface packet queue interface and implementations
;; Copyright (C) 2007 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;; This file is part of Lisp Educational Network Simulator (LENS)

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:

;;

;;; Code:

(in-package :physical)

(defclass packet-queue()
  ((enqueue-count :type counter :accessor enqueue-count :initform 0
              :documentation "Total enqueued packets")
   (drop-count :type counter :accessor drop-count :initform 0
               :documentation "Total dropped packets")
   (egress-filter :type (or boolean function list) :writer egress-filter
                :initform nil
                :documentation "If true or function of queue which
returns true, a packet loss should be enforced")
   (start-time :type time-type :accessor start-time :initform 0
               :documentation "Start time of this measurement")
   (last-update :type time-type :accessor last-update :initform 0
                :documentation "Time of last update")
   (total-byte-seconds
    :type time-type :accessor total-byte-seconds :initform 0
    :documentation "Area under time/occupancy curve")
   (limit-bytes :initarg :limit-bytes :initform 50000
                :type integer :accessor limit-bytes)
   (limit-packets :initarg :limit-packets  :initform nil
                  :type integer :accessor limit-packets)
   (length-bytes :type integer :initform 0 :accessor length-bytes)
   (length-packets :type integer :initform 0 :accessor length-packets))
  (:documentation "Base class for interface queues that enque packets before
they are sent down on the link"))

(defmethod print-object((q packet-queue) stream)
  (print-unreadable-object (q stream :type t :identity t)
    (format stream "~D~@[/~D~@] packets, ~D~@[/~D~] bytes"
            (length-packets q) (limit-packets q)
            (length-bytes q) (limit-bytes q))))

(defmethod reset((q packet-queue))
  (until (empty-p q) (dequeue q))
  (reset-average-queue-length q)
  (setf (enqueue-count q) 0
        (drop-count q) 0))

(defmethod enqueue :after (packet (queue packet-queue))
  (update-average-queue-length queue))

(defmethod enqueue :around (packet (queue packet-queue))
  (cond
    ((and (buffer-available-p packet queue)
          (not (drop-packet-p packet queue)))
     (incf (length-packets queue))
     (incf (length-bytes queue) (length-bytes packet))
     (call-next-method)
     (incf (enqueue-count queue)))
    (t
     (incf (drop-count queue))
     nil)))

(defmethod dequeue :after ((queue packet-queue))
  (update-average-queue-length queue))

(defmethod dequeue :around((queue packet-queue))
  (let ((packet (call-next-method)))
    (when packet
      (decf (length-packets queue))
      (decf (length-bytes queue) (length-bytes packet))
      packet)))

(defgeneric dequeue-if(predicate queue &key key)
  (:documentation "Remove the first packet on the queue for which
predicate is true and return it, nil if none."))

(defgeneric delete-packets-if(predicate queue &key key)
  (:documentation "Delete packets on queue for for which predicate is
true. Return count of number of packets removed"))

(defgeneric drop-packet-p(packet queue)
  (:documentation "Checks if a forced loss should be enforced.
Called by each subclass prior to enquing a packet.
The forced loss will be counted, and the entry
Return True if time for a forced loss. May also be used for egress filtering")
  (:method(packet (queue packet-queue))
    (let ((loss (slot-value queue 'egress-filter)))
      (etypecase loss
        (function (funcall loss queue packet))
        (list (some #'(lambda(f) (funcall f queue packet)) loss))
        (boolean loss)))))

(defgeneric buffer-available-p(size entity)
  (:documentation "Return true if queue has size space available")
  (:method((size integer) (queue packet-queue))
    (cond
      ((limit-packets queue)
       (< (length-packets queue) (limit-packets queue)))
      ((limit-bytes queue)
       (<= (+ (length-bytes queue) size)
           (limit-bytes queue)))
      (t)))
  (:method(packet (queue packet-queue))
    (buffer-available-p (length-bytes packet) queue)))

(defun average-queue-length(queue)
  (let ((interval (- (simulation-time) (start-time queue))))
    (if (zerop interval)
        0
        (/ (total-byte-seconds queue) interval))))

(defun reset-average-queue-length(queue)
  (let ((now (simulation-time)))
    (setf (start-time queue) now
          (last-update queue) now
          (total-byte-seconds queue) 0)))

(defun update-average-queue-length(queue)
  (let ((now (simulation-time)))
    (incf (total-byte-seconds queue)
          (* (length-bytes queue) (- now (last-update queue))))
    (setf (last-update queue) now)))

(defclass drop-tail(packet-queue)
  ((queue :initform (make-queue)))
  (:default-initargs :limit-bytes 50000)
  (:documentation "The simple droptail or FIFO queue"))

(defmethod enqueue(packet (q drop-tail))
  (enqueue packet (slot-value q 'queue)))

(defmethod dequeue((q drop-tail))
  (dequeue (slot-value q 'queue)))

(defmethod empty-p((q drop-tail))
  (empty-p (slot-value q 'queue)))

(defmethod delete-packets-if((predicate function) (queue drop-tail)
                             &key (key #'identity))
  (setf (alg::list-queue-head (slot-value queue 'queue))
        (mapcan
         #'(lambda(packet)
             (if (funcall predicate (funcall key packet))
                 (progn
                   (decf (length-packets queue))
                   (decf (length-bytes queue) (length-bytes packet))
                   nil)
                 (list packet)))
         (alg::list-queue-head (slot-value queue 'queue))))
  (setf (alg::list-queue-tail (slot-value queue 'queue)) (last (alg::list-queue-head (slot-value queue 'queue))))
  (update-average-queue-length queue))

(defmethod dequeue-if((predicate function) (queue drop-tail)
                    &key (key #'identity))
  (traverse
   #'(lambda(packet)
       (when (funcall predicate (funcall key packet))
         (alg::delete packet (slot-value queue 'queue))
         (decf (length-packets queue))
         (decf (length-bytes queue) (length-bytes packet))
         (return-from dequeue-if packet)))
   (slot-value queue 'queue))
  (update-average-queue-length queue))
