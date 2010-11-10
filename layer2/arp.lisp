;; $Id$
;; IPv4 implementation
;; Copyright (C) 2006 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;; This file is part of Lisp Educational Network Simulator (LENS)

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:

;;

;;; Code:
(defpackage :protocol.arp
  (:nicknames :arp)
  (:use :cl :common :address :protocol.layer3)
  (:import-from :scheduler #:schedule #:simulation-time #:time-type)
  (:import-from :interface #:send)
  (:import-from :packet #:peek-pdu #:push-pdu #:pop-pdu #:size)
  (:import-from :trace #:default-trace-detail #:write-trace
                #:pdu-trace #:write-pdu-slots)
  (:import-from :protocol #:insert-protocol #:layer)
  (:export #:arp #:arp-header #:arp-request #:*max-retries*
           #:*default-rx-timeout* #:*default-arp-entry-timeout*))

(in-package :protocol.arp)

(defvar *max-retries* 5
  "Maximum number of retries for resolving a certain entry")
(defvar *default-rx-timeout 0.1
  "default timeout for arp request in seconds")
(defvar *default-arp-entry-timeout* 1200
  "Default timeout for a arp entries")

(defstruct arp-entry
  (state :free :type symbol) ;; current state, free, rsolved or pending
  (mac 0 :type macaddr)   ;; mac address of the target
  (lifetime 0 :type time-type) ;; lifetime of this entry
  (retries 0 :type counter) ;; number of arp attempts made for this entry
  (queue nil :type list));; Associated Queue


(defclass arp-header(pdu)
  ((hwmacaddrtype
    :initform 1 :initarg :hwmacaddrtype :type word :reader hwmacaddrtype
    :documentation "The hardware address type, default is ethernet mac")
   (protoaddrtype
    :initform #x800 :initarg :protoaddrtype :type word :accessor protoaddrtype
    :documentation "The protocol address type, default is IP")
   (hwmacaddrsize
    :initform 6 :initarg :hwmacaddrsize :type octet :accessor hwmacaddrsize
    :documentation
    "Size for the hardware address, default is 6 for ethernet mac")
   (protoaddrsize
      :initform 4 :initarg :protoaddrsize :type octet :accessor protoaddrsize
      :documentation "Size for the protocol address, default is 4 for IPV4")
   (op :initarg :op :type word :accessor op
       :documentation "Either Request or Reply")
   (srcmacaddr :initarg :srcmacaddr :type macaddr :reader srcmacaddr
               :documentation "The mac address of the source of the packet")
   (srcipaddr :initarg :srcipaddr :type ipaddr :reader srcipaddr
              :documentation "The IP address of the source of the packet")
   (dstmacaddr
    :initarg :dstmacaddr :type macaddr :reader dstmacaddr
    :documentation "The mac address of the destination of the packet")
   (dstipaddr
    :initarg :dstipaddr :type ipaddr :reader dstipaddr
    :documentation "The IP address of the destination of the packet")))

(defmethod size((pdu arp-header)) 28)
(defmethod protocol-number((pdu arp-header)) #x803)

(defmethod copy((h arp-header))
  (copy-with-slots
   h
   '(hwmacaddrtype protoaddrtype hwmacaddrsize protoaddrsize op srcmacaddr
     srcipaddr dstmacaddr dstipaddr)))

(defmethod pdu-trace((pdu arp-header) detail stream &key packet text)
  (format stream " ~@[~A~] L3-ARP" text)
  (write-pdu-slots pdu
                   '(hwmacaddrtype protoaddrtype hwmacaddrsize protoaddrsize)
                   detail stream)
  (write-string (ecase (op pdu) (:request " REQ") (:reply " RPL")) stream)
  (write-pdu-slots
   pdu '(srcmacaddr srcipaddr dstmacaddr dstipaddr) detail stream)
  (when (member 'uid detail)
    (format stream " ~D" (if packet (uid packet) 0))))

(defclass arp(protocol)
  ((timeout :initform *default-arp-entry-timeout*
            :initarg :timeout :type time-type :accessor timeout
            :documentation "Timeout for the arp entries")
   (rxtimeout :initform  *default-rx-timeout :initarg :rxtimeout
              :accessor rxtimeout
              :documentation "Timout for retransmitting arp request")
   (cache :initform (make-hash-table) :type hash-table :accessor cache
          :documentation "The arp cache that stores all the entries"))
  (:documentation "Address Resolution Protocol (ARP)  implementation"))

(defmethod initialize-instance :after ((arp arp) &key node &allow-other-keys)
  (when node (insert-protocol (layer arp) (protocol-number arp) arp node)))

(defmethod default-trace-detail((entity arp))
  '(op srcmacaddr srcipaddr dstmacaddr dstipaddr))

(defmethod protocol-number((pdu arp)) #x803)

(defun send-arp(packet dst interface arp &optional (reschedule t))
  "Actuall send arp packet, traace and schedule a retransmission"
  (when reschedule
    (schedule (rxtimeout arp)
              (list #'retransmit (copy packet) dst
                    interface arp)))
  (write-trace (node interface) arp (peek-pdu packet) nil
               :packet packet :text "-")
  (send interface packet (macaddr :broadcast) (protocol-number arp)))

(defmethod send(interface packet (arp arp) &optional type)
  (let* ((iphdr (peek-pdu packet))
         (entry (gethash (dst-address iphdr) (cache arp))))
    (if (and entry (> (arp-entry-lifetime entry) (simulation-time)))
        (case (arp-entry-state entry)  ;; found and not expired
          (:resolved
           (interface::send interface packet (arp-entry-mac entry) type))
          (:pending
           (push (cons packet type) (arp-entry-queue entry))))
        (let ((entry (make-arp-entry
                      ;; create new cache entry with this packet on queue
                      :state :pending
                      :lifetime (+ (simulation-time) (timeout arp))
                      :queue (list (cons packet type))))
              (packet (make-instance 'packet))
              (arphdr (make-instance 'arp-header
                                     :op :request
                                     :dstmacaddr nil
                                     :dstipaddr (dst-address iphdr)
                                     :srcmacaddr (macaddr interface)
                                     :srcipaddr (ipaddr interface))))
          (setf (gethash (dst-address iphdr) (cache arp)) entry)
          (push-pdu arphdr packet)
          (send-arp packet (dst-address iphdr) interface arp)))))

(defun retransmit(packet dst interface arp)
  (let ((entry (gethash dst (cache arp))))
    (when entry ;; unless found
      (when (eql (arp-entry-state entry) :pending)
        (if (or (> (incf (arp-entry-retries entry)) *max-retries*)
                (< (arp-entry-lifetime entry) (simulation-time)))
            (progn ;; finished trying
              ;; abort - delete all packets
              (dolist(qitem (nreverse (arp-entry-queue entry)))
                (let ((packet (first qitem)))
                  (write-trace (node interface) arp :drop nil
                               :packet packet :text "L3-NE")))
              (remhash dst (cache arp)))
            ;; else arp again
            (send-arp packet dst interface arp))))))

(defmethod receive((arp arp) interface packet)
  "arp request arrival"
  (let* ((arp (arp interface)) ;; ensure we are using arp on correct interface
         (arphdr (pop-pdu packet))
         (node (node interface)))
    (write-trace node arp arphdr nil :packet packet :text "+")
    (cond
      ((and arp ;; there is arp on this interface
            (address= (ipaddr interface) (dstipaddr arphdr))) ;; my IP address
       ;; check if we have source entry
       (let ((entry (gethash (srcipaddr arphdr) (cache arp))))
         (cond
           (entry ;; yes - update arp entry
            (setf (arp-entry-lifetime entry)
                  (+ (timeout arp) (simulation-time)))
            (setf (arp-entry-mac entry) (srcmacaddr arphdr))
            (when (eql (arp-entry-state entry) :pending)
              (setf (arp-entry-state entry) :resolved)
              ;; now we have resolved MAC address send all queued packets
              (dolist(qitem (nreverse (arp-entry-queue entry)))
                (interface::send interface (first qitem) (srcmacaddr arphdr)
                                 (second qitem)))
              (setf (arp-entry-queue entry) nil))
            (setf (arp-entry-retries entry) 0))
           (t ;; create fresh entry
            (setf (gethash (srcipaddr arphdr) (cache arp))
                  (make-arp-entry
                   :mac (srcmacaddr arphdr)
                   :state :resolved
                   :lifetime (+ (simulation-time) (timeout arp)))))))
       (when (eql (op arphdr) :request)
         ;; prepare arp reply packet
         (let ((packet (make-instance 'packet))
               (arphdr (make-instance 'arp-header
                                      :op :request
                                      :dstmacaddr (srcmacaddr arphdr)
                                      :dstipaddr (srcipaddr arphdr)
                                      :srcmacaddr (macaddr interface)
                                      :srcipaddr (ipaddr interface))))
           (push-pdu arphdr packet)
           (write-trace node arp (peek-pdu packet) nil
                        :packet packet :text "-")
           (send interface packet (dstmacaddr arphdr) (protocol-number arp)))))
      (t ;; else drop the packet as not addressed to host
       (write-trace node nil :drop nil :packet packet :text "L3-NA")))))
