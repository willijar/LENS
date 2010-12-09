;; $Id$
;; UDP implementation
;; Copyright (C) 2006 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;; This file is part of Lisp Educational Network Simulator (LENS)

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:

;;

;;; Code:

(in-package :protocol.layer3)

(defenumeration ICMP-msg-type
    ((echo-reply 0)
     (destination-unreachable 3)
     (source-quench 4)
     (redirect 5)
     (echo 8)
     (time-exceededb 11)
     (parameter-problem 12)
     (timestamp 13)
     (timestamp-reply 14)
     (information-request 15)
     (information-reply 16)))

(defenumeration destination-unreachable-code
    (net-unreachable
     host-unreachable
     protocol-unreachable
     port-unreachable
     cant-fragment
     source-route-failed))

(defenumeration time-exceeded-code
    (ttl-exceeded
     reassembly-exceeded))

(defclass icmp-header(pdu)
  ((icmp-type :accessor icmp-type :type ICMP-msg-type :initarg :type)
   (code :reader code :initarg :code :initform nil
         :type (or null destination-unreachable-code time-exceeded-code))
   (identifier :initform 0 :type seq
               :reader identifier :initarg :identifier)
   (seq :initform 0 :type seq :reader seq
             :initarg :seq)
   (originated :type time-type :initform (simulation-time)
              :reader originated)
   (received :type time-type :reader received)
   (transmitted :type time-type  :reader transmitted)
   (ipv4-header :initform nil :type ipv4-header :accessor ipv4-header)
   (layer4-header :initform nil :type layer4:pdu :accessor layer4-header)))

(defmethod initialize-instance :after
    ((h icmp-header) &key  ipv4-header layer4-header &allow-other-keys)
  (when ipv4-header
    (setf (slot-value h 'ipv4-header) (copy ipv4-header)))
  (when layer4-header
    (setf (slot-value h 'layer4-header) (copy layer4-header))))

(defmethod protocol-number((h icmp-header)) 1)

(defmethod length-bytes((h icmp-header))
  (+ (length-bytes (ipv4-header h))
     (length-bytes (layer4-header h))
     (ecase (icmp-type h)
       ((echo echo-reply) 8)
       ((destination-unreachable source-quench time-exceeded) (+ 8 8))
       ((timestamp timestamp-reply) (+ 8 12))
       ((redirect information-request information-reply parameter-problem)
        ;; not implemented
        0))))

(defmethod copy((h icmp-header))
  (let ((copy
         (copy-with-slots
          h
          '(icmp-type code identifier seq  originated received transmitted))))
    (setf (slot-value copy 'ipv4-header) (copy (ipv4-header h)))
    (setf (slot-value copy 'layer4-header) (copy (layer4-header h)))))

(defmethod pdu-trace((pdu icmp-header) detail os &key packet text)
  (declare (ignore packet))
  (format os " ~@[~A~] L4-ICMP" text)
  (write-pdu-slots pdu '(icmp-type code) detail os)
  (case (icmp-type pdu)
    ((echo echo-reply)
     (write-pdu-slots pdu '(identifier seq) detail os))
    ((timestamp timestamp-reply)
     (write-pdu-slots pdu '(originated received transmitted) detail os))))

(defclass icmp(protocol)
  ((callbacks :type hash-table :initform (make-hash-table) :accessor callbacks
              :documentation "callbacks"))
  (:documentation "A model of the Internet Control Message Protocol.
  ICMP is conceptually a Layer 3 protocol but uses the
  services of IPV4."))

(defmethod receive((protocol icmp) packet layer3 &key dst-address &allow-other-keys)
  (let ((ipv4 (peek-pdu packet -1))
        (icmp (pop-pdu packet)))
    (ecase (icmp-type icmp)
     (echo (echo-reply packet ipv4 icmp))
     (timestamp (timestamp-reply packet ipv4 icmp))
     (destination-unreachable
      (when (and (ipv4-header icmp) (layer4-header icmp))
        (kill-pending-connection (ipv4-header icmp) (layer4-header icmp) icmp)))
     ((timestamp echo-reply source-quench time-exceeded redirect
       information-request information-reply parameter-problem)
      ;; not implemented
      )))
    ;; call application callbacks with a copy of the packet
     (dolist(callback (callbacks protocol))
       (funcall callback (copy packet))))

;; ICMP message generation

(defgeneric destina

(defun destination-unreachable(node packet ipv4-header l4pdu code)
  (declare (ignore packet))
  (when (enabled-p)
    (let ((packet (make-instance 'packet))
          (icmp (make-instance 'icmp-header
                               :type :destination-unreachable
                               :code code)))
      (when ipv4-header (push-pdu (copy ipv4-header) packet))
      (when l4pdu (push-pdu (copy l4pdu) packet))
      (push-pdu icmp packet)
      (write-trace node (icmp) icmp nil :packet packet :text "-")
      (layer3:send (ipv4) node packet
                           :src (src-address ipv4-header)
                           :dst (src-address ipv4-header)
                           :proto (protocol-number (icmp))))))

(defun time-exceeded(node packet ipv4-header code)
  (declare (ignore packet))
  (when (enabled-p)
    (let ((packet (make-instance 'packet))
          (icmp (make-instance 'icmp-header
                               :type :time-exceeded
                               :code code)))
      (push-pdu icmp packet)
      (write-trace node (icmp) icmp nil :packet packet :text "-")
      (layer3:send (ipv4) node packet
                           :src (src-address ipv4-header)
                           :dst nil
                           :proto (protocol-number (icmp))))))

(defun echo-reply(node packet ipv4-header icmp)
  (declare (ignore packet))
  (when (enabled-p)
    (let ((packet (make-instance 'packet))
          (icmp (copy icmp)))
      (setf (icmp-type icmp) :echo-reply)
      (push-pdu icmp packet)
      (write-trace node (icmp) icmp nil :packet packet :text "-")
      (layer3:send (ipv4) node packet
                           :src (src-address ipv4-header)
                           :dst nil
                           :proto (protocol-number (icmp))))))

(defun echo(node icmp dst)
  (when (enabled-p)
    (let ((packet (make-instance 'packet)))
      (setf (icmp-type icmp) :echo)
      (push-pdu icmp packet)
      (write-trace node (icmp) icmp nil :packet packet :text "-")
      (layer3:send (ipv4) node packet
                           :src dst
                           :proto (protocol-number (icmp))))))

(defun timestamp(node icmp dst)
  (when (enabled-p)
    (let ((packet (make-instance 'packet)))
      (setf (icmp-type icmp) :timestamp)
      (push-pdu icmp packet)
      (write-trace node (icmp) icmp nil :packet packet :text "-")
      (layer3:send (ipv4) node packet
                           :src dst
                           :proto (protocol-number (icmp))))))

(defun kill-pending-connection(node ipv4-header layer4-header)
  (when (typep layer4-header 'tcp:tcp-header) ;; if tcp
    (let ((tcp (lookup-by-port
                node (protocol-number layer4-header)
                :local-port (src-port layer4-header)
                :local-address (src-address ipv4-header)
                :peer-port (dst-port layer4-header)
                :peer-address (dst-address ipv4-header))))
      (when tcp (tcp:abort tcp)))))
