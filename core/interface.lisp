;; Interface
;; Copyright (C) 2010 John A.R. Williams

;; Author: John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;; This file is part of Lisp Educational Network Simulator (LENS)

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:


;; Link layer will be sent packets either from ARP or from interface
;; and will send to link.


;; Interfaces are made up of a link, a layer 2 protocol, a packet
;; queue and possibley an arp unit.  layer 3 protocols send to and
;; receive from interfaces interfaces convert network address to
;; hardware address (using arp module if present) and then use the
;; layer 2 protocol to send packets over the link using the packet
;; queue The layer 2 protocol is responsible for managing the queue
;; and link and for notifications against the interface.

;; received packets go link -> interface -> layer 2 -> layer 3
;; transmitted packets go layer 3 -> interface -> layer 2 -> link

;;; Code:

(in-package :layer1)

;; this corresponsd to interface and interface-real in GTNetS
(defvar *default-arp* nil
  "Specify that all subsequently created interfaces should use the ARP
   protocol (or not). All interfaces on the same link must support ARP
   for it to work")

(defvar *default-delay* 1e-3 "Default link delay")
(defvar *default-ber* 0 "Default bit error rate on a link")

(defgeneric delay(start end)
 (:documentation "Return the propagation delay in bits/sec between
two things")
 (:method(start end)
   (/ (distance start end) +c+)))

(defgeneric bit-error-rate(sender receiver)
  (:documentation "Return the bit error rate for a simple link")
  (:method(sender receiver)
    (declare (ignore sender receiver))
    0))

(defclass interface(notifier)
  ((node
    :type node :reader node :initarg :node
    :documentation "Associated node")
   (network-address
    :type network-address :initarg :network-address
    :reader network-address
    :documentation "local network address for this interface")
   (network-mask
    :initarg :network-mask :reader network-mask :initform nil
    :documentation "Subnet mask for this interface")
   (hardware-address
    :initarg :hardware-address :initform (macaddr :next)
    :type hardware-address :reader hardware-address
    :documentation "layer2 address for this interface")
   (link
    :initarg :link :type link :reader link :documentation "Associated link")
   (layer2:protocol
    :initarg :protocol :type layer2:protocol :reader layer2:protocol
    :documentation "The Layer 2 protocol object")
   (packet-queue
    :initarg :packet-queue
    :initform (make-instance (if *default-arp* 'priority-queue 'drop-tail))
    :reader packet-queue :type packet-queue
    :documentation "Associated transmission packet queue (layer 2 packets)")
   (layer2:arp
    :initform nil :reader layer2:arp
    :documentation "The Arp protocol or nil if don't use arp")
   (up-p
    :initform t :type boolean :reader up-p
    :documentation "State of router - up or down")
   (rx-packet
    :type packet :initform nil :documentation "Packet being received")
   (tx-packet
    :type packet :initform nil :documentation "Packet being transmitted"))
   (:documentation "Base class for all interfaces"))

(defmethod print-object((interface interface) stream)
  (print-unreadable-object (interface stream :type t :identity t)
    (princ (type-of (layer2:protocol interface)) stream)
    (when (slot-boundp interface 'node)
      (format stream " N~D ~A"
              (uid (node interface))
              (hardware-address interface)))))

(defmethod buffer-available-p(size (interface interface))
  (buffer-available-p size (packet-queue interface)))

(defmethod bandwidth((interface interface))
  (bandwidth (link interface)))

(defmethod location((interface interface))
  (location (node interface)))

(defmethod delay((start interface) (end interface))
  (with-slots(link) start
    (if (slot-exists-p link 'delay)
        (slot-value (link start) 'delay)
        (/ (distance start end) (propagation-speed link)))))

(defmethod bit-error-rate((sender interface) (receiver interface))
  (with-slots(link) sender
    (if (slot-exists-p link 'bit-error-rate)
        (slot-value link 'bit-error-rate)
        (call-next-method))))

(defmethod initialize-instance :after ((interface interface)
                                       &key (protocol 'layer2:ieee802.3)
                                       node
                                       &allow-other-keys)
  (setf (slot-value interface 'layer2:protocol)
        (make-instance protocol :interface interface))
  (when *default-arp*
    (setf (slot-value interface 'layer2:arp)
          (make-instance *default-arp* :interface interface)))
  (node:add-interface interface node)
  (unless (slot-boundp interface 'network-address)
    (setf  (slot-value interface 'network-address)
           (network-address node))))

(defmethod send((interface interface) packet (protocol layer3:protocol) &key address &allow-other-keys)
  "Called by upper layers to send packets - may return null if
interface is busy. Requires address"
  (if (up-p interface)
      (etypecase address
        ((eql :broadcast)
         (send (layer2:protocol interface) packet protocol
               :address (macaddr :broadcast)))
        (hardware-address
         (send (layer2:protocol interface) packet protocol :address address))
        (network-address
         (if (layer2:arp interface)
             (send (layer2:arp interface) packet protocol :address address)
             (send (layer2:protocol interface) packet protocol
                 :address (network-to-hardware-address address interface)))))
      (drop interface packet :text "L2-ID")))

(defmethod send :before ((link link) packet (interface interface) &key &allow-other-keys)
  (when (busy-p link) (error "Unable to transmit over a busy link"))
  (with-slots(tx-packet) interface
    (when tx-packet (error "Interface cannot send two packets simultaneously"))
    (setf tx-packet packet)))

(defmethod send((interface interface) packet (protocol layer2:protocol) &key &allow-other-keys)
  (if (busy-p protocol)
      (enqueue packet (packet-queue interface))
      (send (link interface) packet interface)))

(defmethod send-complete((interface interface) packet (link link)
                         &key fail &allow-other-keys)
  (assert (eql packet (slot-value interface 'tx-packet)))
  (setf (slot-value interface 'tx-packet) nil)
  (when fail (drop interface packet :text fail))
  (unless (empty-p (packet-queue interface))
    (send (link interface) packet interface))
  (do-notifications interface))

(defmethod receive-start((interface interface) packet link)
  (when (up-p interface)
    (with-slots(rx-packet) interface
      (when rx-packet
        (drop interface rx-packet :text "L2-ID")
        (setf rx-packet packet)
        (receive-start (layer2:protocol interface) packet interface)))))

(defmethod receive((interface interface) packet link
                   &key errors &allow-other-keys)
  (with-slots(rx-packet) interface
    (assert (eql packet rx-packet))
    (setf rx-packet nil))
  (if (zerop errors)
      (receive (layer2:protocol interface) packet interface)
      (drop interface packet :text "L2-BER")))

(defmethod mkup((interface interface) &key (inform-routing t))
  (unless (up-p interface)
    (setf (slot-value interface 'up-p) t)
    (when inform-routing (layer3:topology-changed interface))))

(defmethod mkdown((interface interface) &key (inform-routing t))
  (when (up-p interface)
    (setf  (slot-value interface 'up-p) nil)
    (dolist(s '(rx-packet tx-packet)) ; empty tx and rx buffers
      (let ((p (slot-value interface s)))
        (when p
          (drop interface p :text "L2-ID")
          (setf (slot-value interface s) nil))))
    (until (empty-p (packet-queue interface))
      (drop interface (dequeue (packet-queue interface)) :text "L2-ID"))
        (when inform-routing (layer3:topology-changed interface))))

(defmethod reset((interface interface))
  (reset (packet-queue interface))
  (reset (layer2:protocol interface))
  (reset (layer2:arp interface))
  (setf (slot-value interface 'rx-packet) nil)
  (setf (slot-value interface 'tx-packet) nil)
  (reset (link interface)))

(defmethod busy-p((interface interface))
  (or
   (slot-value interface 'tx-packet)
   (slot-value interface 'rx-packet)
   (busy-p (link interface))))

(defgeneric network-to-hardware-address(network-address interface)
  (:documentation "map a network address to hardware  on a link")
  (:method ((addr network-address) (interface interface))
    (when-bind(peer-interface
               (find addr (peer-interfaces (link interface) interface)
                     :key #'network-address))
      (hardware-address peer-interface))))

(defmethod default-peer-interface((interface interface))
  (default-peer-interface (link interface)))

(defmethod peer-interfaces(link (interface interface))
  (remove interface (interfaces link)))

;; (defun call-notification(interface resched)
;;   "Notify next non-nil entry in notification list"
;;   (if (empty-p (notifications interface))
;;       (setf (pending-notification interface) nil)
;;       (progn
;;         (notify (extract-head (notifications interface)))
;;         (when resched
;;           (let ((notify-event (list #'call-notification interface t)))
;;             (schedule (queuing-delay (queue interface)) notify-event)
;;             (setf (pending-notification interface) notify-event))))))


