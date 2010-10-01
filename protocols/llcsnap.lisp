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

(in-package :protocol.layer2)

(defclass llcsnap-header(pdu)
  ((dsap :type (unsigned-byte 8) :accessor dsap
             :initform #xAA)
   (ssap  :type (unsigned-byte 8) :accessor ssap
             :initform #xAA)
   (ctrl  :type (unsigned-byte 8) :accessor ctrl
              :initform #x3)
   (oui  :type (unsigned-byte 16) :accessor oui
              :initform 0)
   (ethtype :type (unsigned-byte 16) :accessor ethtype
                 :initform #x0800 :initarg :type)))

(defmethod size((h llcsnap-header)) 8)

(defmethod copy((h llcsnap-header))
  (copy-with-slots h '(dsap sap ctrl oui ethtype)))

(defclass llcsnap(protocol)
  ()
  (:documentation "Base class for protocols which use llcsnap sublayer"))

(defmethod build-pdu((protocol llcsnap) src-address dst-address packet
                     &optional (type #x0800))
  (declare (ignore src-address dst-address))
  (packet:push-pdu (make-instance 'llcsnap-header :type type) packet))

(defmethod receive((protocol llcsnap) packet)
  (let* ((h (packet:pop-pdu packet))
         (interface (interface protocol))
         (layer3 (protocol:find-protocol 3 (ethtype h) (node interface))))
    (unless layer3
      (error "Unable to find protocol ~X in protocol graph" (ethtype h)))
    (layer3:receive layer3 interface packet)))