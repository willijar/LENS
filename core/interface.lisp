;; Interface
;; Copyright (C) 2010 John A.R. Williams

;; Author: John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;; This file is part of Lisp Educational Network Simulator (LENS)

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:

;;

;;; Code:

(in-package :layer1)

;; this corresponsd to interface and interface-real in GTNetS
(defvar *default-arp* nil
  "Specify that all subsequently created interfaces should use the ARP
   protocol (or not). All interfaces on the same link must support ARP
   for it to work")

(defclass interface(notifier)
  ((node
    :initarg :node :type node :reader node
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
   (queue
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
    :type packet :initform nil :documentation "Packet being received"))
   (:documentation "Base class for all interfaces"))

(defmethod print-object((interface interface) stream)
  (print-unreadable-object (interface stream :type t :identity t)
    (princ (type-of (layer2:protocol interface)) stream)
    (when (slot-boundp interface 'node)
      (format stream " N~D~@[ ~A~]~@[/~A~]"
              (uid (node interface))
              (hardware-address interface)
              (ipmask interface)))))

(defmethod initialize-instance :after ((interface interface)
                                       &key
                                       &allow-other-keys)
  (when *use-arp*
    (setf (slot-value interface 'arp)
          (make-instance *default-arp* :interface interface)))
  (setf (interface (layer2:protocol interface)) interface)
  (setf (interface (queue interface)) interface)
  (node:add-interface interface (node interface))
  (unless (slot-boundp interface 'network-address)
    (setf  (slot-value interface 'network-address)
           (network-address (node interface)))))

(defmethod send((interface interface) packet protocol &key address &allow-other-keys)
  "Called by upper layers to send packets - may return null if interface is busy. Requires address"
  (if (up-p interface)
      (etypecase address
        ((eql :broadcast)
         (send (layer2:protocol interface) packet protocol
               :address (macaddr :broadcast)))
        (hardware-address
         (send (layer2:protocol interface) packet protocol :address address))
        (network-address
         (if (arp interface)
             (send (arp interface) packet protocol :address address)
             (send (layer2:protocol interface) packet protocol
                 :address (network-to-hardware-address address interface)))))
      (drop interface packet :text "L2-ID")))

(defmethod send-complete((interface interface) packet link
                         &key fail &allow-other-keys)
  (assert (eql packet (pop-packet (queue interface))))
  (when fail (drop interface packet :text fail)))


(defmethod (setf up)(up (interface interface) &key (inform-routing t))
  ;; if fail during packet drop received packet
  (unless (eql up (up-p interface))
    (setf (slot-value interface 'rx-packet) nil)
    (setf (slot-value interface 'up-p) up)
    (when inform-routing (layer3:topology-changed interface)))
  (unless up
    (until (empty-p (queue interface))
      (drop interface (dequeue (queue interface)) :text "L2-ID"))))

(defmethod reset((interface interface))
  (reset (queue interface))
  (reset (layer2:protocol interface))
  (reset (arp interface))
  (setf (slot-value interface 'rx-packet) nil)
  (reset (link interface)))

(defmethod receive-start((interface interface) packet link)
  (when (up-p interface)
    (with-slots(rx-packet) interface
      (when rx-packet
        (drop interface packet :text "L2-ID")
        (setf rx-packet packet)))))

(defmethod receive((interface interface) packet link &key errors &allow-other-keys)
  (with-slots(rx-packet) interface
    (assert (eql packet rx-packet))
    (setf rx-packet nil))
  (if (zerop errors)
      (receive (layer2:protocol interface) packet interface)
      (drop interface packet :text "L2-BER")))

(defmethod busy-p((interface interface)) (busy-p (link interface)))

(defmethod network-to-hardware-address((addr network-address) (interface interface))
   (when-bind(peer-interface (find addr (peer-interfaces interface) :key #'network-address))
     (hardware-address peer-interface)))

(defmethod peer-interfaces((interface interface) &key only-if-up-p)
  (when (or (not only-if-up-p) (up-p interface))
    (remove interface (peer-interfaces (link interface))
            :only-if-up-p only-if-up-p)))

(defmethod default-peer-interface((interface interface))
   (default-peer-interface (link interface)))

;; (defmethod notify((interface interface))
;;   "Link has finished previous transmit"
;;   (unless (up-p interface)
;;     ;; interface has failed - drop all packets
;;     (loop
;;      (let ((p (deque (queue interface))))
;;        (unless p (return))
;;        (write-trace (node interface)  nil :drop nil :packet p
;;                     :text "L2-ID"))))
;;   (unless (layer2:busy-p (layer2:protocol interface))
;;     (let ((packet (deque (queue interface))))
;;       (when packet
;;         (layer2:send (layer2:protocol interface) packet)))
;;       ;; send queue space available notification
;;     (call-notification interface nil) ))

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

;; (defun add-notify(notification interface)
;;   (queues:insert notification (notifications interface))
;;   (unless (pending-notification interface)
;;      (let ((notify-event (list #'call-notification interface t)))
;;       (schedule (queuing-delay (queue interface)) notify-event)
;;       (setf (pending-notification interface) notify-event))))

;; (defun cancel-notify(notification interface)
;;   (queues:extract notification (notifications interface)))



;; (defgeneric neighbours(link)
;;   (:documentation "Return the routing neighbours to this interface")
;;   (:method ((interface interface))
;;     (let ((link (link interface)))
;;       (mapcan #'(lambda(peer-interface)
;;                   (unless (eql peer-interface interface)
;;                     (list (layer3:make-neighbour
;;                            :node (node peer-interface)
;;                            :interface interface
;;                            :weight (weight link)))))
;;               (peer-interfaces link)))))


