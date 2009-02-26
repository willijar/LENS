;;;; LENS packet header API
;;;; Copyright (C) 2003-2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;; $Id: packet.lisp,v 1.2 2005/11/29 09:00:39 willijar Exp $

(in-package :packet)

(defvar *packet-count* 0)

(defclass packet()
  ((uid :type counter
        :initform (incf *packet-count*)
        :reader uid
        :documentation "Unique id of the packet")
   (pdus :type vector :initform (make-array 5 :adjustable t :fill-pointer 0)
         :reader pdus)
   (created :type time-type :initform (simulation-time) :reader created
            :initarg :created :documentation "Time the packet was created")
   (expires :type time-type :initform 0 :reader expires :initarg :expires
            :documentation "Time the packet expires - used by aodv")
   (retx :type counter :initform 0 :accessor retx :initarg :retx
         :documentation "Retransmission Counter")
   (routing :initarg :routing :initform nil :reader routing
            :documentation "SOurce routing e.g. nixvector data")
   (notification
    :initarg :notification :initform nil :accessor notification
    :documentation "Object to inform when this packet is transmitted"))
  (:documentation  "A generic packet class composed of a vector of
protocol data units (PDUs)."))

(defmethod print-object((packet packet) stream)
  (print-unreadable-object(packet stream :type t :identity t)
    (format stream "~S ~:/print-eng/bytes" (uid packet) (size packet))))

(defmethod copy((packet packet))
  (let ((copy (copy-with-slots packet
                               '(uid created expires retx notification))))
    (setf (slot-value copy 'routing) (copy (slot-value packet 'routing)))
    (setf (slot-value copy 'pdus) (make-array 4 :adjustable t :fill-pointer 0))
    (map nil #'(lambda(pdu) (vector-push-extend (copy pdu) (pdus copy)))
         (pdus packet))
    copy))

(defgeneric size(pdu)
  (:documentation "The size in octets of an entity")
  (:method((packet packet))
    (reduce #'+  (pdus packet) :key #'size)))

(defun push-pdu(pdu packet)
  "Insert a protocol data unit (PDU) into the packet.
The PDU is added at the top of the PDU stack"
  (vector-push-extend pdu (pdus packet)))

(defun pop-pdu(packet)
  (when (> (length (pdus packet)) 0)
    (vector-pop (pdus packet))))

(defun push-pdu-bottom(pdu packet)
  "Insert a protocol data unit (PDU) into the packet at the bottom of
the stack"
  (let* ((pdus (pdus packet))
         (length (length pdus)))
    (vector-push-extend nil pdus packet)
    (loop :for i :from length :above 0
          :do  (setf (aref pdus i) (aref pdus (1- i))))
    (setf (aref pdus 0) pdu)))

(defun find-pdu(layer packet &optional protocol-number)
  (if protocol-number
      (find-if #'(lambda(pdu) (and (= (layer pdu) layer)
                                   (= (layer4:protocol-number pdu)
                                      protocol-number)))
               (pdus packet))
      (find layer (pdus packet) :key #'layer :test #'=)))

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

(defgeneric priority(pdu)
  (:documentation "Priority associated with a PDU or packet (0 if none)")
  (:method((packet packet))
    (let ((pdu
           (find-if #'(lambda(priority) (> priority 0))
                    (pdus packet) :from-end t :key #'priority)))
      (if pdu (priority pdu) 0))))

