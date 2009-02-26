;; $Id$
;; Interfaces
;; Copyright (C) 2007 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;; This file is part of Lisp Educational Network Simulator (LENS)

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:

;;

;;; Code:

(in-package :interface)

;; this corresponsd to interface and interface-real in GTNetS

(defclass interface(timers-manager)
  ((node :initarg :node :type node :accessor node
         :documentation "Associated node")
   (link :initarg :link :type link :accessor link
         :documentation "Associated link")
   (up-p :initform t :type boolean :reader up-p
         :documentation "State of router - up or down")
   (ipaddr :initarg :ipaddr :initform nil :type ipaddr
           :documentation "IP address for this interface")
   (ipmask :initarg :ipmask :initform nil :type ipmask :reader ipmask
           :documentation "subnet mask associated with this interface")
   (macaddr :initarg :macaddr :initform (macaddr nil)
            :type macaddr :reader macaddr
            :documentation "layer2 address for this interface")
   (layer2:protocol :initarg :layer2-protocol
                    :initform (make-instance 'layer2:IEEE802.3)
                    :type layer2:protocol :reader layer2:protocol
                    :documentation "The Layer 2 protocol object")
   (queue :initarg :queue :initform (make-instance 'drop-tail)
          :reader queue :type packet-queue
          :documentation "Associated packet queue for an interface")
   (notifications  :initform (make-instance 'queue) :type queue
                  :reader notifications
                  :documentation "Queue of registered notifications")
   (pending-notification :initform nil :accessor pending-notification
                         :documentation "The pending notification ")
   (arp :initform nil
        :reader arp
        :documentation "The Arp protocol or nil if don't use arp"))
  (:documentation "Base class for all interfaces"))

(defmethod print-object((interface interface) stream)
  (print-unreadable-object (interface stream :type t :identity t)
    (princ (type-of (layer2:protocol interface)) stream)
    (when (slot-boundp interface 'node)
      (format stream " N~D~@[ ~A~]~@[/~A~]"
              (uid (node interface))
              (ipaddr interface)
              (ipmask interface)))))

(defmethod ipaddr((interface interface))
  (or (slot-value interface 'ipaddr)
      (ipaddr (node interface))))

(defvar *default-use-arp* nil
  "Specify that all subsequently created interfaces should
   use the ARP protocol (or not).")

(defmethod initialize-instance :after ((interface interface)
                                       &key
                                       &allow-other-keys)
  (when *default-use-arp*
    (setf (slot-value interface 'arp)
          (make-instance 'layer3:arp
                         :node (node interface))))
  (setf (interface (layer2:protocol interface)) interface)
  (when (slot-boundp interface 'node)
    (node:add-interface interface (node interface)))
  (setf (interface (queue interface)) interface))

(defgeneric send(interface packet address &optional llcsnaptype)
  (:documentation "Enque and transmit"))

(defgeneric receive(interface packet &optional lostp)
  (:documentation "Called when interface receives a packet from a link"))

(defmethod up((interface interface) &key (inform-routing t))
  (unless (slot-value interface 'up-p)
    (setf (slot-value interface 'up-p) t)
    (when inform-routing
      (routing:topology-changed interface))))

(defmethod down((interface interface) &key (inform-routing t))
  (when (slot-value interface 'up-p)
    (setf (slot-value interface 'up-p) nil)
    (when inform-routing
      (routing:topology-changed interface))))

(defmethod buffer-available-p(size (interface interface))
  (buffer-available-p size (queue interface)))

(defmethod send((interface interface) (packet packet) (dst ipaddr)
                &optional (type #x0800))
  (send interface packet (or (arp interface) (ip-to-mac dst interface)) type))

(defmethod send((interface interface) (packet packet) (dst (eql nil))
                &optional (type #x0800))
  (send interface packet (macaddr (default-peer-interface interface)) type))

(defmethod send((interface interface) (packet packet) (dst macaddr)
                &optional (type #x0800))
  (unless (up-p interface)
     ;; if down down't forward - just trace drop
    (write-trace (node interface)  nil :drop nil :packet packet :text "L2-ID")
    (return-from send nil))
  (let ((protocol (layer2:protocol interface)))
    (layer2:build-pdu protocol (macaddr interface) dst packet type)
    (cond
      ((not (layer2:busy-p protocol))
       (layer2:data-request protocol packet))
      (t (enque packet interface)))))

(defmethod enque(packet (interface interface))
  (or (enque packet (queue interface))
      (progn
        ;; not enqued so trace as dropped
        (write-trace (node interface)  nil
                     :drop nil :packet packet :text "L2-QD")
        (when (notification packet)
          ;; sender has requested notification so schedule one in 1ms
          (schedule 1e-3 (notification packet)))
        nil)))

(defmethod notify((interface interface))
  "Link has finished previous transmit"
  (unless (up-p interface)
    ;; interface has failed - drop all packets
    (loop
     (let ((p (deque (queue interface))))
       (unless p (return))
       (write-trace (node interface)  nil :drop nil :packet p
                    :text "L2-ID"))))
  (unless (layer2:busy-p (layer2:protocol interface))
    (let ((packet (deque (queue interface))))
      (when packet
        (layer2:data-request (layer2:protocol interface) packet)))
      ;; send queue space available notification
    (call-notification interface nil) ))

(defun call-notification(interface resched)
  "Notify next non-nil entry in notification list"
  (if (empty-p (notifications interface))
      (setf (pending-notification interface) nil)
      (progn
        (notify (extract-head (notifications interface)))
        (when resched
          (let ((notify-event (list #'call-notification interface t)))
            (schedule (queuing-delay (queue interface)) notify-event)
            (setf (pending-notification interface) notify-event))))))

(defun add-notify(notification interface)
  (queues:insert notification (notifications interface))
  (unless (pending-notification interface)
     (let ((notify-event (list #'call-notification interface t)))
      (schedule (queuing-delay (queue interface)) notify-event)
      (setf (pending-notification interface) notify-event))))

(defun cancel-notify(notification interface)
  (queues:extract notification (notifications interface)))

(defmethod peer-node-p((node node) (interface interface))
  (peer-node-p node (link interface)))

(defmethod ip-to-mac((addr ipaddr) (interface interface))
  (ip-to-mac addr (link interface)))

(defmethod peer-interfaces((interface interface))
  (remove interface (peer-interfaces (link interface))))

(defgeneric neighbours(link)
  (:documentation "Return the routing neighbours to this interface")
  (:method ((interface interface))
    (let ((link (link interface)))
      (mapcan #'(lambda(peer-interface)
                  (unless (eql peer-interface interface)
                    (list (routing:make-neighbour
                           :node (node peer-interface)
                           :interface interface
                           :weight (weight link)))))
              (peer-interfaces link)))))

(defmethod default-peer-interface((interface interface))
  (default-peer-interface (link interface)))

(defmethod peer-node-ipaddr ((node node) (interface interface))
  (peer-node-ipaddr node (link interface)))

(defmethod local-ipaddr-p((ipaddr ipaddr) (interface interface))
  (if (ipmask interface)
      (address= (subnet ipaddr (ipmask interface))
                (subnet (ipaddr interface) (ipmask interface)))
      (address= ipaddr (ipaddr interface))))

(defmethod receive((interface interface) packet &optional lostp)
  (if lostp
      (write-trace (node interface)  nil :drop nil :packet packet
                   :text "L2-BER")
      (layer2:data-indication (layer2:protocol interface) packet)))

(defmethod make-new-interface(link &key ipaddr ipmask)
  (make-instance 'interface :link link :ipaddr ipaddr :ipmask ipmask))

(defmethod reset((interface interface))
  (reset (link interface))
  (cancel-all-timers interface)
  (setf (pending-notification interface) nil))

(defmethod node:location((interface interface))
  (node:location (node interface)))