;;;; LENS packet header API
;;;; Copyright (C) 2003-2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details

(in-package :packet)

(defvar *packet-count* 0)

(defclass pdu()
  ()
  (:documentation "base class for all PDU entities"))


(defgeneric layer(pdu)
  (:documentation "Return the layer associated with a PDU"))

(defgeneric priority(pdu)
  (:documentation "Priority of a pdu for priority queueing")
  (:method(pdu) 0))

(defmethod copy((pdu pdu)) (make-instance (class-of pdu)))

(defclass packet()
  ((uid :type counter
        :initform (incf *packet-count*)
        :reader uid
        :documentation "Unique id of the packet")
   (fid :type counter :initarg :fid :documentation "Flow id useful for tracing"
        :reader fid)
   (pdus :type vector :initform (make-array 5 :adjustable t :fill-pointer 0)
         :reader pdus)
   (created :type time-type :initform (simulation-time) :reader created
            :initarg :created :documentation "Time the packet was created")
   (routing :initarg :routing :initform nil :reader routing
            :documentation "Source routing e.g. nixvector data"))
  (:documentation  "A generic packet class composed of a vector of
protocol data units (PDUs)."))

(defmethod initialize-instance :after((packet packet) &key data &allow-other-keys)
  (push-pdu data packet))

(defmethod print-object((packet packet) stream)
  (print-unreadable-object(packet stream :type t :identity t)
    (format stream "~S ~:/print-eng/bytes" (uid packet) (length-bytes packet))))

(defmethod length-bytes((p packet))
  (reduce #'+ (pdus p) :key #'length-bytes))

(defmethod copy((packet packet))
  ;; note this does not copy the pdu's for efficiency reasons.
  (let ((copy (copy-with-slots packet '(uid created routing))))
    (let ((pdus (make-array (length (pdus packet))
                            :adjustable t :fill-pointer 0)))
      (setf (slot-value copy 'pdus) pdus)
      (map nil #'(lambda(pdu) (vector-push-extend pdu pdus))
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

(defmethod priority((packet packet)) (priority (peek-pdu packet)))


