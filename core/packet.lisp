;;;; LENS packet header API
;;;; Copyright (C) 2003-2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details

(in-package :packet)

(defvar *packet-count* 0)

(defclass pdu()
  ()
  (:documentation "base class for all PDU entities"))

(defgeneric length-bytes(pdu)
  (:documentation "Return length of pdu or packet in bytes"))

(defgeneric layer(pdu)
  (:documentation "Return the layer associated with a PDU"))

(defclass packet()
  ((uid :type counter
        :initform (incf *packet-count*)
        :reader uid
        :documentation "Unique id of the packet")
   (pdus :type vector :initform (make-array 4 :adjustable t :fill-pointer 0)
         :reader pdus)
   (created :type time-type :initform (simulation-time) :reader created
            :initarg :created :documentation "Time the packet was created")
   (routing :initarg :routing :initform nil :reader routing
            :documentation "Source routing e.g. nixvector data"))
  (:documentation  "A generic packet class composed of a vector of
protocol data units (PDUs)."))

(defmethod initialize-instance :after((packet packet) &key data &allow-other-keys)
  (typecase data
    (pdu
     (push-pdu data packet))
    (integer
     (push-pdu (make-instance 'layer5:data :length-bytes data) packet))
    ((vector octet *)
     (push-pdu (make-instance 'layer5:data :contents data) packet))))

(defmethod print-object((packet packet) stream)
  (print-unreadable-object(packet stream :type t :identity t)
    (format stream "~S ~:/print-eng/bytes" (uid packet) (length-bytes packet))))

(defmethod length-bytes((p packet))
  (reduce #'+ (pdus p) :key #'length-bytes))

(defmethod copy((packet packet))
  (let ((copy (copy-with-slots packet '(uid created routing))))
    (let ((pdus (make-array (length (pdus packet))
                            :adjustable t :fill-pointer 0)))
      (setf (slot-value copy 'pdus) pdus)
      (map nil #'(lambda(pdu) (vector-push-extend (copy pdu) pdus))
           (pdus packet)))
    copy))

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



