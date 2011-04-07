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

;;; Code:

(in-package :layer2)

(defclass llcsnap-header(pdu)
  (#+nil(dsap :type (unsigned-byte 8) :initform #xAA)
   #+nil(ssap  :type (unsigned-byte 8) :initform #xAA)
   #+nil(ctrl  :type (unsigned-byte 8) :initform #x3)
   #+nil(oui  :type (unsigned-byte 24) :initform 0)
   (ethtype :type (unsigned-byte 16) :accessor ethtype
            :initform #x0800 :initarg :type
            :documentation "Protocol Number for upper level")))

(defmethod default-trace-detail((pdu llcsnap-header))
  '(ethtype))

(defmethod pdu-trace((pdu llcsnap-header) detail stream &key packet text)
    (format stream " ~@[~A ~]LLC" text)
    (when (and packet (member 'length detail))
      (format stream " ~A" (length-bytes packet)))
    (write-pdu-slots pdu '((dsap " ~X")
                           (ssap " ~X")
                           (ctrl " ~X")
                           (oui " ~X")
                           (ethtype " ~X"))
                     detail stream)
    (when (member 'uid detail)
      (format stream " ~D" (if packet (uid packet) 0))))

(defmethod length-bytes((h llcsnap-header)) 8)

(defmethod copy((h llcsnap-header))
  (copy-with-slots h '(dsap sap ctrl oui ethtype)))

(defclass llcsnap(protocol)
  ()
  (:documentation "Base class for protocols which use llcsnap sublayer"))

(defmethod busy-p((layer2 llcsnap)) (busy-p (interface layer2)))

(defmethod send :before ((layer2 llcsnap) packet layer3 &key &allow-other-keys)
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

(defmethod receive ((protocol llcsnap) packet interface &key &allow-other-keys)
  (receive (find-recipient (packet:pop-pdu packet) protocol) packet protocol))
