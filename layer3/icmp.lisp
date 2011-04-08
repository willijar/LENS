;; ICMP implementation
;; Copyright (C) 2010 Dr. John A.R. Williams

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
     (time-exceeded 11)
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
  ((name :initform "ICMP" :reader name :allocation :class)
   (trace-format :initform '(icmp-type code
                             identifier seq
                             originated received transmitted)
                 :reader trace-format
                 :allocation :class)
   (icmp-type :accessor icmp-type :type ICMP-msg-type :initarg :type)
   (code :reader code :initarg :code :initform nil
         :type (or null destination-unreachable-code time-exceeded-code))
   (identifier :type seq
               :reader identifier :initarg :identifier)
   (seq :type seq :reader seq
        :initarg :seq)
   (originated :type time-type :reader originated)
   (received :type time-type :reader received)
   (transmitted :type time-type  :reader transmitted)))

(register-protocol 'icmp 1)

(defmethod initialize-instance :after((pdu icmp-header) &key &allow-other-keys)
  (case (icmp-type pdu)
    ((timestamp timestamp-reply)
     (setf (slot-value pdu 'originated)  (simulation-time)))))

(defmethod length-bytes((h icmp-header))
  (ecase (icmp-type h)
    ((echo echo-reply) 8)
    ((destination-unreachable source-quench time-exceeded) (+ 8 8))
    ((timestamp timestamp-reply) (+ 8 12))
    ((redirect information-request information-reply parameter-problem)
        ;; not implemented
     0)))

(defmethod copy((h icmp-header))
  (copy-with-slots
   h
   '(icmp-type code identifier seq  originated received transmitted)))

(defun icmp-receive(ipv4 packet ipv4hdr)
  (when (icmp-enabled-p ipv4)
    (let ((icmphdr (pop-pdu packet)))
      (ecase (icmp-type icmphdr)
        (echo (echo-reply ipv4 ipv4hdr icmphdr))
        (timestamp (timestamp-reply ipv4 ipv4hdr icmphdr))
        (destination-unreachable
         (kill-pending-connection ipv4 packet))
        ((echo-reply source-quench time-exceeded redirect
                     information-request information-reply parameter-problem)
      ;; not implemented
         )))))

(defun destination-unreachable(ipv4 ipv4-header layer4-header &key (code 'host-unreachable))
  (when(icmp-enabled-p ipv4)
    (let ((packet (make-instance 'packet:packet))
          (icmp-header (make-instance 'icmp-header
                                      :type 'destination-unreachable
                                      :code code)))
      (push-pdu (copy ipv4-header) packet)
      (when layer4-header (push-pdu (copy layer4-header) packet))
      (push-pdu icmp-header packet)
      (send ipv4 packet 'icmp
            :src-address (network-address (node ipv4))
            :dst-address (src-address ipv4-header)))))

(defun time-exceeded(ipv4 ipv4-header &key (code 'ttl-exceeded))
  (when(icmp-enabled-p ipv4)
    (let ((packet (make-instance 'packet:packet)))
      (push-pdu (make-instance 'icmp-header
                               :type 'destination-unreachable
                               :code code)
                packet)
      (send ipv4 packet 'icmp
            :src-address nil
            :dst-address (src-address ipv4-header)))))

(defun echo-reply(ipv4 ipv4-header icmp-header)
  (when (icmp-enabled-p  ipv4)
    (let ((packet (make-instance 'packet))
          (icmp-header (copy icmp-header)))
      (setf (icmp-type icmp-header) 'echo-reply)
      (push-pdu icmp-header packet)
      (send ipv4 packet 'icmp
                 :src-address nil
                 :dst-address (src-address ipv4-header)))))

(defun echo(ipv4 dst)
  (when(icmp-enabled-p ipv4)
    (let ((packet (make-instance 'packet)))
      (push-pdu (make-instance 'icmp-header :type 'echo) packet)
      (send ipv4 packet 'icmp
            :src-address nil
            :dst-address dst))))

(defun timestamp(ipv4 dst &key identifier seq)
  (let ((packet (make-instance 'packet))
        (icmp-header (make-instance 'icmp-header
                                    :type 'timestamp
                                    :originated (simulation-time)
                                    :seq seq
                                    :identifier identifier)))
      (push-pdu icmp-header packet)
      (send ipv4 packet 'icmp
                :src-address (network-address (node ipv4))
                :dst-address dst)))

(defun timestamp-reply(ipv4 ipv4-header icmp-header)
  (let ((packet (make-instance 'packet))
        (icmp-header (copy icmp-header)))
    (push-pdu icmp-header packet)
    (setf (icmp-type icmp-header) 'timestamp-reply)
    (setf (slot-value 'icmp-header 'received) (simulation-time))
    (setf (slot-value 'icmp-header 'transmitted) (simulation-time))
    (send ipv4 packet 'icmp
          :src-address (network-address (node ipv4))
          :dst-address (src-address ipv4-header))))

;; kill-pending-connection in tcp.lisp