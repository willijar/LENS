;; $Id$
;; IEEE 802.3 MAC protocol
;; Copyright (C) 2007 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;; This file is part of Lisp Educational Network Simulator (LENS)

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:

;;

;;; Code:

(defpackage :protocol.ieee802.3
  (:nicknames :ieee802.3)
  (:use :cl :common :address :protocol.layer2)
  (:import-from :protocol
                #:*common-protocol-graph* #:insert-protocol #:remove-protocol
                #:layer #:size)
  (:import-from :trace
                #:default-trace-detail #:pdu-trace #:write-trace
                #:write-pdu-slots)
  (:import-from :packet #:peek-pdu #:push-pdu #:pop-pdu #:packet)
  (:export #:ieee802.3))

(in-package :ieee802.3)

(defclass ieee802.3 (llcsnap)
  ()
  (:documentation "Defines the Medium Access Control portion of 802.3
layer 2 protocol."))

(defclass ieee802.3-header(pdu)
  ((size :type word :reader size :initform 12 :allocation :class
         :documentation "Specifies the size (in bytes) of the 802.3 header when transmitting on a link.  Normally, the default 12 is correct.  However, other
simulators (notably ns2) do not model this, so direct comparisons
to ns2 are difficult.  Changing this value to 0 (along with the
corresponding value in the LLCSnap header) will give a more direct
comparison to ns2.")
   (src-address :type macaddr :reader src-address :initarg :src-address)
   (dst-address :type macaddr :reader dst-address :initarg :dst-address)
   (length :type word :reader msg-length :initarg :length))
  (:documentation "The 802.3 PDU"))

(defmethod copy((h ieee802.3-header))
  (copy-with-slots h '(src-address dst-address length)))

(defmethod default-trace-detail((entity ieee802.3))
  '(type length src-address dst-address uid))

(defmethod pdu-trace((pdu ieee802.3-header) detail stream &key packet text)
  (format stream " ~@[~A~] L2~@[-802.3~]" text (member 'type detail))
  (when (and packet (member 'length detail))
    (format stream " ~A" (size packet)))
  (write-pdu-slots pdu '(src-address dst-address) detail stream)
  (when (member 'uid detail)
    (format stream " ~D" (if packet (uid packet) 0))))

(defmethod build-pdu((protocol ieee802.3) src-address dst-address packet
                     &optional (type #x0800))
  (declare (ignore type))
  (call-next-method)
  (push-pdu
   (make-instance
    'ieee802.3-header
    :src-address src-address
    :dst-address dst-address
    :length (size packet))
   packet))

(defmethod send((protocol ieee802.3) packet)
  (let* ((interface (interface protocol))
         (node (node interface)))
    (when (node:call-callbacks (layer protocol) 0
                               :tx packet node interface)
      (write-trace node protocol (peek-pdu packet)
                   nil :packet packet :text "-")
      (link:transmit (link interface) packet interface node))))

(defmethod receive((protocol ieee802.3) packet)
  (let* ((interface (interface protocol))
         (node (node interface)))
    (when (node:call-callbacks  (layer protocol) 0
           :rx packet node interface)
      (let ((h (peek-pdu packet)))
        (write-trace node protocol h nil :packet packet :text "+"))
      (pop-pdu packet)
      (call-next-method))))

(defmethod busy-p((protocol ieee802.3))
  (link:busy-p (link (interface protocol))))
