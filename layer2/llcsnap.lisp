;; $Id$
;; Implementation of 802.2 llc/snap frame
;; Copyright (C) 2007 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;; This file is part of Lisp Educational Network Simulator (LENS)

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:

;;  This is the file for the 802.2 llc/snap frame
;;  A low overhead (4 bytes less than ethernet encapsulation)
;;  We get to pass the 'ethertype' into the packet.
;;  For now SNAP is enough but maybe we need LLC later

;; This has
;;; Code:

(in-package :layer2)

(defclass llcsnap-header(pdu)
  ((dsap :type (unsigned-byte 8) :initform #xAA)
   (ssap  :type (unsigned-byte 8) :initform #xAA)
   (ctrl  :type (unsigned-byte 8) :initform #x3)
   (oui  :type (unsigned-byte 16) :initform 0)
   (ethtype :type (unsigned-byte 16) :accessor ethtype
            :initform #x0800 :initarg :type
            :documentation "Protocol Number for upper level")))

(defmethod length-bytes((h llcsnap-header)) 8)

(defmethod copy((h llcsnap-header))
  (copy-with-slots h '(dsap sap ctrl oui ethtype)))

(defclass llcsnap(protocol)
  ()
  (:documentation "Base class for protocols which use llcsnap sublayer"))

(defmethod send :before ((protocol llcsnap) packet layer3 &key &allow-other-keys)
  (push-pdu
   (make-instance 'llcsnap-header :type (protocol-number layer3))
   packet))

(defun find-recipient(h protocol)
  (let* ((interface (interface protocol))
         (arp (arp (interface protocol)))
         (type (ethtype h)))
    (cond
      ((and arp (= type (protocol-number arp)))
       arp)
      ((layer3:find-protocol type (node interface)))
      (t
       (error "Unable to find protocol ~X in protocol graph" type)))))

(defmethod send-complete((protocol llcsnap) packet interface &optional fail)
  (unless (empty-p (packet-queue interface))
    (send (link interface) (packet-queue interface) interface))
  (send-complete
   (find-recipient (find 'llcsnap-header (pdus packet) :key #'type-of) protocol)
   packet interface fail))

(defmethod receive ((protocol llcsnap) packet interface &key errors &allow-other-keys)
  (receive (find-recipient (packet:pop-pdu packet) protocol) packet protocol :errors errors))
