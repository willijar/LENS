;;;; packet and headers
;;;; Copyright (C) 2011 John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;; This file is part of Lisp Educational Network Simulator (LENS)

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:


;;; Code:

(in-package :packet)

(defvar *packet-count* 0)

(defclass pdu()
  ()
  (:documentation "base class for all PDU entities"))

(defgeneric priority(pdu)
  (:documentation "Priority of a pdu for priority queueing")
  (:method(pdu) 0))

;; there are two useful external representations for pdus and packets
;; one is readable  - e.g. for tracing, the other is binary format for
;; interaction with real networks.

(defgeneric trace-format(pdu)
  (:documentation "Return the trace formatting for a pdu as a list -
  either slot names or a list of slot name and associated format string"))

(defun write-pdu-slots(pdu mask stream)
  "Helper to write the slots of a PDU to stream. mask is the detail
mask specifying which slots to write."
  (dolist(slot (trace-format pdu))
    (multiple-value-bind(slot format)
        (if (listp slot)
            (values (first slot) (second slot))
            (values slot " ~A"))
      (when (or  (eql mask :all) (member slot mask))
        (let ((value
               (cond
                 ((functionp slot) (funcall slot pdu))
                 ((slot-boundp pdu slot)
                  (slot-value pdu slot)))))
          (etypecase format
            (string (format stream format value))
            (function (funcall format stream value))))))))

(defmethod print-object((pdu pdu) stream)
  (print-unreadable-object (pdu stream :type t :identity nil)
    (write-pdu-slots pdu :all stream)))

(defgeneric pdu-trace(pdu detail stream &key packet text)
  (:documentation "All PDUs should define this method to trace their
output according to detail onto stream")
  (:method((pdu (eql :drop)) detail (stream stream) &key packet text)
    "Trace a packet drop"
    (declare (ignore detail))
    (format stream " D-~A ~D~%" text (if packet (uid packet) 0)))
  (:method((pdu null) detail (stream stream) &key packet text)
    (format stream " ~A ~D" text (if packet (uid packet) 0)))
  (:method((pdu pdu) detail (stream stream) &key packet text)
    (format stream " ~@[~A~]L~D~:[~*~;-~A~]"
            text (protocol:layer pdu)  (member 'type detail) (name pdu))
    (write-pdu-slots pdu detail stream)
    (when (member 'uid detail)
      (format stream " ~D" (if packet (uid packet) 0)))))

(defclass packet()
  ((uid :type integer :initform (incf *packet-count*) :reader uid
        :documentation "Unique id of the packet")
   (fid :type integer :initarg :fid :reader fid
        :documentation "Flow id - for tracing")
   (pdus :type vector :initform (make-array 5 :adjustable t :fill-pointer 0)
         :reader pdus)
   (created :type time-type :initform (simulation-time) :reader created
            :documentation "Time the packet was created")
   (routing :initarg :routing :initform nil :reader routing
            :documentation "Source routing e.g. nixvector data"))
  (:documentation  "A generic packet class composed of a vector of
protocol data units (PDUs)."))

(defmethod initialize-instance :after((packet packet)
                                      &key data &allow-other-keys)
  (push-pdu data packet))

(defmethod print-object((packet packet) stream)
  (print-unreadable-object(packet stream :type t :identity t)
    (format stream "~D ~:/print-eng/bytes" (uid packet) (length-bytes packet))))

(defmethod length-bytes((p packet))
  (reduce #'+ (pdus p) :key #'length-bytes))

(defun push-pdu(pdu packet)
  "Insert a protocol data unit (PDU) into the packet.
The PDU is added at the top of the PDU stack"
  (vector-push-extend pdu (pdus packet)))

(defun pop-pdu(packet)
  (when (> (length (pdus packet)) 0)
    (vector-pop (pdus packet))))

(defun peek-pdu(packet &optional (offset 0))
  (let* ((pdus (pdus packet))
         (length (length pdus)))
    (when (> length offset)
      (aref pdus (- length offset 1)))))

(defun skip-pdu(packet)
  "This method skips the current pdu that's at the top of the
   stack without deleting the PDU"
  (let* ((pdus (pdus packet))
         (length (length pdus)))
    (when (> length 0)
      (decf (fill-pointer pdus)))))

(defmethod priority((packet packet)) (priority (peek-pdu packet)))


