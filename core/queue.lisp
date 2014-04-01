;; Timestamped, packet and history queue definitions
;; Copyright (C) 2013-2014 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;;; Copying:

;; This file is part of Lisp Educational Network Simulator (LENS)

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; LENS is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; These queues allow for monitoring of average time on queue
;; The packet queue also provides signals and a byte-size
;; The history-buffer may be used to track if a packet is a duplicate.

;;; Code:

(in-package :lens)

(register-signal 'drop
 "Signal called with packet when it is dropped e.g. due to buffer overflow")

(register-signal 'buffer-length
 "Signal called with queue object when its length is changed")

(register-signal 'buffer-time
 "Sgnal called with a queueing time when an object is dequeued")

(defstruct (timestamped-queue (:include alg:vector-queue))
  "A timestamped queue records the simulation time of when items are
enqueued.  [[dequeue]] and [[peek]] from a [[timestamped-queue]]
returns this time as a second value. The timestamped-queue also keeps
track of the average queue duration which can be obtained using
[[average-queue-time]]."
  (average-timestamp 0.0d0 :type time-type))

(defmethod enqueue(x (q timestamped-queue))
  (let ((record (make-timestamped :value x))
        (size (size q)))
    (setf (timestamped-queue-average-timestamp q)
          (if (zerop size)
              (timestamped-time record)
              (/ (+ (timestamped-time record)
                    (* size (timestamped-queue-average-timestamp q)))
                 (1+ (size q)))))
    (call-next-method record q)))

(defmethod dequeue((q timestamped-queue))
  (let ((record (call-next-method)))
    (setf (timestamped-queue-average-timestamp q)
          (if (zerop (size q))
              0.0d0
          (/ (- (* (1+ (size q)) (timestamped-queue-average-timestamp q))
                (timestamped-time record))
             (size q))))
    (values (timestamped-value record) (timestamped-time record))))

(defgeneric average-queue-time(queue)
  (:documentation "* Arguments

- queue :: a [[timestamped-queue]]

* Description

Returns the average simulation time items have been on /queue/.")
  (:method((q timestamped-queue))
    (- (timestamped-queue-average-timestamp q) (simulation-time))))

(defmethod peek((q timestamped-queue))
  (let ((record (call-next-method)))
    (values (timestamped-value record) (timestamped-time record))))

(defclass packet-buffer(entity-with-signals parameter-object)
  ((queue :reader queue :type timestamped-queue)
   (buffer-size
    :initarg :buffer-size
    :type fixnum :parameter t :initform 32 :reader buffer-size
    :documentation "max buffer size in messages")
   (buffer-size-bytes
    :initarg :buffer-size-bytes
    :type fixnum :parameter t :initform nil :reader buffer-size-bytes
    :documentation "max size in bytes")
   (byte-length :initform 0 :type fixnum :reader byte-length))
  (:metaclass parameter-class)
  (:documentation "A packet buffer emplements the queue interface for
[[packet]]s using a timestamped queue. [[dequeue]] and [[peek]] from a
[[timestamped-queue]] returns this time as a second value. It also
keeps track of the average queue duration which can be obtained using
[[average-queue-time]].

It keeps track of the buffer size which may be set using the
=buffer-size= or =buffer-size-bytes= parameters. [[packet]]s are
dropped if either maximum buffer size is exceeded and the =drop=
signal will be generated with the dropped [[packet]] as the
argument. Every time the queue length is changed the =buffer-length=
and =buffer-time= events are generated with the buffer and the
duration the packet was in the buffer respectively.

 "))

(defmethod print-object((p packet-buffer) stream)
  (print-unreadable-object(p stream :type t :identity t)
    (format stream "~D packets" (size p))))

(defmethod initialize-instance :after((buffer packet-buffer)
                                      &key &allow-other-keys)
  (setf (slot-value buffer 'queue)
        (if (buffer-size buffer)
            (make-timestamped-queue
             :vector (make-array (1+ (buffer-size buffer)))
             :extend-size nil)
            (make-timestamped-queue))))

(defmethod size((buffer packet-buffer)) (size (queue buffer)))

(defmethod empty-p((buffer packet-buffer)) (empty-p (queue buffer)))

(defmethod enqueue(p (buffer packet-buffer))
  (when (or (and (buffer-size buffer)
                 (= (size buffer) (buffer-size buffer)))
            (and (buffer-size-bytes buffer)
                 (> (+ (byte-length p) (byte-length buffer))
                    (buffer-size-bytes buffer))))
    (emit buffer 'drop p)
    (return-from enqueue nil))
  (enqueue p (queue buffer))
  (incf (slot-value buffer 'byte-length) (byte-length p))
  (emit buffer 'buffer-length buffer)
  p)

(defmethod peek((buffer packet-buffer))
  (peek (queue buffer)))

(defmethod dequeue((buffer packet-buffer))
  (multiple-value-bind(p timestamp) (dequeue (queue buffer))
    (decf (slot-value buffer 'byte-length) (byte-length p))
    (emit buffer 'buffer-length buffer)
    (emit buffer 'buffer-time (- timestamp (simulation-time)))
    (values p timestamp)))

(defmethod average-queue-time((buffer packet-buffer))
  (average-queue-time (queue buffer)))

(defconstant +default-history-size+ 5 "This is the default number of
packets a [[history-buffer]] will retain to determine if a packet is a
duplicate.")

(defclass history-buffer()
  ((queue :type alg::vector-wrap-queue :reader queue)
   (test :initform #'equalp :initarg :test :reader buffer-test
         :documentation "The test function to compare entities")
   (key :initform #'identity :initarg :key :reader buffer-key
        :documentation "The jey function to use to compare entities."))
  (:documentation "A class for recording the history of seen objects.
The [[duplicate-p]] method is called with an object and will return true if the object is previously recorded in the history of this buffer.

* Additional Initialization Arguments

- :size :: an =integer= specifies the size of the history buffer
             (number of previous entities to remember)
- :element-type :: a /type specification/ for the elements
             to be stored in the histire buffer."))

(defmethod initialize-instance :after ((instance history-buffer)
                                      &key (size +default-history-size+)
                                      (element-type t)
                                      &allow-other-keys)
  (setf (slot-value instance 'queue)
        (alg::make-queue :element-type element-type
                         :initial-size size
                         :implementation :wrap)))

(defgeneric duplicate-p(entity buffer  &optional record)
  (:documentation "* Arguments

- entity :: an object
- buffer :: a [[history-buffer]]

* Optional Arguments

- record :: a =boolean= (default t)

* Description

Returns true if the /entity/ is recorded in the /buffer/. If /record/
is true then /entity will be recorded in the buffer.")
  (:method (x (buffer history-buffer) &optional (record t))
    (let ((key (funcall (buffer-key buffer) x)))
    (or
     (find
      key
      (alg::vector-wrap-queue-vector (queue buffer))
      :test #'(lambda(a b) (when b (funcall (buffer-test buffer) a b))))
     (progn
       (when record (enqueue key (queue buffer)))
       nil)))))
