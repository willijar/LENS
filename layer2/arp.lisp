;; ARP implementation
;; Copyright (C) 2006-2010 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;; This file is part of Lisp Educational Network Simulator (LENS)

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:

;;

;;; Code:

(in-package :layer2)

(defvar *arp-max-retries* 5
  "Maximum number of retries for resolving a certain entry")
(defvar *default-arp-rx-timeout* 0.1
  "default timeout for arp request in seconds")
(defvar *default-arp-entry-timeout* 1200
  "Default timeout for a arp entries")

(defstruct arp-entry
  (state :free :type symbol) ;; current state, free, resolved or pending
  (hwaddr 0 :type hardware-address)   ;; mac address of the target
  (lifetime 0 :type time-type) ;; lifetime of this entry
  (retries 0 :type counter) ;; number of arp attempts made for this entry
  (buffer nil :type packet));; One packet buffer

(defclass arp-header(pdu)
  ((protocol-number :type word :initform #x803 :allocation :class
                    :reader protocol-number)
   (op :initarg :op :type (member :request :reply) :accessor op
       :documentation "Either Request or Reply")
   (srchwaddr :initarg :srcmacaddr :type hardware-address :reader srchwaddr
              :reader src-address
               :documentation "The mac address of the source of the packet")
   (srcprotoaddr :initarg :srcprotoaddr :type network-address :reader srcprotoaddr
              :documentation "The IP address of the source of the packet")
   (dsthwaddr
    :initarg :dsthwcaddr :type hardware-address :reader dsthwaddr
    :reader dst-address
    :documentation "The mac address of the destination of the packet")
   (dstprotoaddr
    :initarg :dstprotoaddr :type network-address :reader dstprotoaddr
    :documentation "The IP address of the destination of the packet")))

(defun hwaddrtype(h) (protocol-number (srchwaddr h)))
(defun protoaddrtype(h) (protocol-number (srcprotoaddr h)))
(defun hwaddrsize(h) (length-bytes (srchwaddr h)))
(defun protoaddrsize(h) (length-bytes (srcprotoaddr h)))

(defmethod initialize-instance :after((h arp-header) &key &allow-other-keys)
  (assert (and (eql (type-of (srchwaddr h)) (type-of (dsthwaddr h)))
               (eql (type-of (srcprotoaddr h)) (type-of (dstprotoaddr h))))))

(defmethod length-bytes((pdu arp-header))
  (+ 8 (* 2 (+ (hwaddrsize pdu) (+ (protoaddrsize pdu))))))

(defmethod priority((pdu arp-header)) 2)

(defmethod copy((h arp-header))
  (copy-with-slots h '(srchwaddr srcprotoaddr dsthwaddr dstprotoaddr)
                   (call-next-method)))

(defclass arp(protocol)
  ((protocol-number :type word :initform #x803 :allocation :class
                    :reader protocol-number)
   (timeout :initform *default-arp-entry-timeout*
            :initarg :timeout :type time-type :accessor timeout
            :documentation "Timeout for the arp entries")
   (rxtimeout :initform  *default-arp-rx-timeout* :initarg :rxtimeout
              :accessor rxtimeout
              :documentation "Timout for retransmitting arp request")
   (cache :initform (make-hash-table) :type hash-table :accessor cache
          :documentation "The arp cache that stores all the entries"))
  (:documentation "Address Resolution Protocol (ARP)  implementation"))

(defmethod default-trace-detail((entity arp))
  '(type op srchwaddr srcprotoaddr dsthwaddr dstprotoaddr))

(defmethod send((arp arp) packet layer3 &key address &allow-other-keys)
  (let ((entry (or (gethash address (cache arp))
                   (setf (gethash address (cache arp))
                         (make-arp-entry :state :pending))))
        (link-protocol (layer2:protocol (interface arp))))
    (when (and (eql (arp-entry-state entry) :resolved)
               (> (arp-entry-lifetime entry) (simulation-time)))
      ;; we know hardware address - send packet to it
      (return-from send
        (send link-protocol packet layer3 :address
              (arp-entry-hwaddr entry))))
    (when-bind(packet (arp-entry-buffer entry))
              (send-complete link-protocol packet arp :fail :ne))
    (send-arp arp entry address)
    (setf (arp-entry-buffer entry) packet)))

(defun send-arp(arp entry address)
  (let*((interface (interface arp))
        (link-protocol (layer2:protocol interface)))
    (when (eql (arp-entry-state entry) :pending)
      (cond ((or (> (incf (arp-entry-retries entry)) *arp-max-retries*)
                 (< (arp-entry-lifetime entry) (simulation-time)))
             ;; give up trying so inform of not sending of buffer packet
             (when-bind(packet (arp-entry-buffer entry))
                  (send-complete link-protocol packet arp :fail "NE"))
             (remhash address (cache arp)))
            (t (send link-protocol
                     (make-instance
                      'packet
                      :data (make-instance
                             'arp-header
                             :op :request
                             :srchwaddr (hardware-address interface)
                             :srcprotoaddr (network-address interface)
                             :dstprotoaddr address))
                     arp
                     :address :broadcast)
               (schedule (rxtimeout arp) (list #'send-arp arp entry address)))))))

(defmethod receive((arp arp) packet layer2 &key &allow-other-keys)
  (let* ((h (pop-pdu packet))
         (interface (interface arp))
    ;; update source entry
        (entry (or (gethash (srcprotoaddr h) (cache arp))
                     (setf (gethash (srcprotoaddr h) (cache arp))
                           (make-arp-entry)))))
      (setf (arp-entry-lifetime entry) (+ (timeout arp) (simulation-time))
            (arp-entry-hwaddr entry) (srchwaddr h)
            (arp-entry-state entry) :resolved
            (arp-entry-retries entry) 0)
      (when-bind(packet (arp-entry-buffer entry))
                (send layer2 packet arp :address
                      (arp-entry-hwaddr entry)))
      (when (eql (op h) :request)
        (if (address= (network-address interface) (dstprotoaddr h))
            (send layer2
                  (make-instance
                   'packet
                   :data (make-instance
                          'arp-header
                          :op :reply
                          :dsthwaddr (srchwaddr h)
                          :dstprotoaddr (srcprotoaddr h)
                          :srchwaddr (hardware-address interface)
                          :srcprotoaddr (network-address interface)))
                  arp
                  :address (srchwaddr h))
            (drop (node arp) packet :text "L3-NA")))))


