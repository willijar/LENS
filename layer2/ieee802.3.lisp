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

(defclass ieee802.3-header(llcsnap-header)
  ((name :initform "802.3" :reader name :allocation :class)
   (trace-format :initform '((dsap " ~X")
                             (ssap " ~X")
                             (ctrl " ~X")
                             (oui " ~X")
                             (ethtype " ~X")
                             src-address
                             dst-address)
                 :reader trace-format
                 :allocation :class)
   (src-address :type macaddr :reader src-address :initarg :src-address)
   (dst-address :type macaddr :reader dst-address :initarg :dst-address)
   (msg-length :initform 0 :type word :reader msg-length :initarg :length))
  (:documentation "The 802.3 PDU"))

;; Note NS2 doesn't model layer 2 size so direct comparisons are difficult
(defmethod length-bytes((pdu ieee802.3-header)) 12)

(defmethod copy((h ieee802.3-header))
  (copy-with-slots h '(src-address dst-address msg-length) (call-next-method)))

(defclass ieee802.3 (llcsnap)
  ()
  (:documentation "Defines the Medium Access Control portion of 802.3
layer 2 protocol."))

(defmethod reset((layer2 ieee802.3))) ;; do nothing

(defmethod default-trace-detail((entity ieee802.3))
  '(type src-address dst-address ethtype))

(defmethod send ((layer2 ieee802.3) packet layer3
                 &key address &allow-other-keys)
  (let ((interface (interface layer2)))
    (push-pdu
     (make-instance 'ieee802.3-header
                    :type (protocol-number layer3)
                    :dst-address address
                    :src-address (hardware-address interface)
                    :length (length-bytes packet))
     packet)
    (send interface packet layer2)))

(defmethod receive((layer2 ieee802.3) packet interface &key &allow-other-keys)
  (let ((pdu (pop-pdu packet)))
    (when (or (eql (dst-address pdu) (hardware-address interface))
              (broadcast-p (dst-address pdu)))
      (receive (llcsnap-find-recipient pdu layer2) packet layer2))))





