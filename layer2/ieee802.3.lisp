;; IEEE 802.3 MAC protocol
;; Copyright (C) 2010 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;; This file is part of Lisp Educational Network Simulator (LENS)

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:

;;

;;; Code:

(in-package :layer2)

(defclass ieee802.3-header(pdu)
  ((src-address :type macaddr :reader src-address :initarg :src-address)
   (dst-address :type macaddr :reader dst-address :initarg :dst-address)
   (msg-length :initform 0 :type word :reader msg-length :initarg :length))
  (:documentation "The 802.3 PDU"))

;; Note NS2 doesn't model layer 2 size so direct comparisons are difficult
(defmethod length-bytes((pdu ieee802.3-header)) 12)

(defmethod copy((h ieee802.3-header))
  (copy-with-slots h '(src-address dst-address length)))

(defmethod pdu-trace((pdu ieee802.3-header) detail stream &key packet text)
  (format stream " ~@[~A~] L2~@[-802.3~]" text (member 'type detail))
  (when (and packet (member 'length detail))
    (format stream " ~A" (length packet)))
  (write-pdu-slots pdu '(src-address dst-address) detail stream)
  (when (member 'uid detail)
    (format stream " ~D" (if packet (uid packet) 0))))

(defclass ieee802.3 (llcsnap)
  ()
  (:documentation "Defines the Medium Access Control portion of 802.3
layer 2 protocol."))

(defmethod reset((layer2 ieee802.3))) ;; do nothing

(defmethod default-trace-detail((entity ieee802.3))
  '(type src-address dst-address))

(defmethod send ((layer2 ieee802.3) packet layer3
                 &key address &allow-other-keys)
  (let ((interface (interface layer2)))
    (push-pdu
     (make-instance 'ieee802.3-header
                    :dst-address address
                    :src-address (hardware-address interface)
                    :length (length-bytes packet))
     packet)
    (send interface packet layer2)))

(defmethod receive((protocol ieee802.3) packet interface &key &allow-other-keys)
  (let ((pdu (pop-pdu packet)))
    (when (or (eql (dst-address pdu) (hardware-address interface))
              (broadcast-p (dst-address pdu)))
      (call-next-method))))





