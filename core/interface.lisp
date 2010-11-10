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

(in-package :layer1)

;; this corresponsd to interface and interface-real in GTNetS
(defvar *default-arp* nil
  "Specify that all subsequently created interfaces should use the ARP
   protocol (or not). All interfaces on the same link must support ARP
   for it to work")

(defclass interface()
  ((node :initarg :node :type node :reader node
         :documentation "Associated node")
   (network-address :type network-address
                    :initarg :network-address
                    :reader network-address
                    :documentation "local network address for this interface")
   (hardware-address  ::initarg :hardware-address
                     initform (macaddr :next)
                     :type hardware-address
                     :reader hardware-address
                     :documentation "layer2 address for this interface")
   (link :initarg :link :type link :reader link
         :documentation "Associated link")
   (layer2:protocol :initarg :protocol
                    :type layer2:protocol :reader layer2:protocol
                    :documentation "The Layer 2 protocol object")
   (queue :initarg :packet-queue
          :initform (make-instance (if *default-arp* 'priority-queue 'drop-tail))
          :reader packet-queue :type packet-queue
          :documentation "Associated packet queue for an interface")
   (layer2:arp :initform nil :reader layer2:arp
        :documentation "The Arp protocol or nil if don't use arp")
   (up-p :initform t :type boolean :reader up-p
         :documentation "State of router - up or down")
   (receiving :type packet :documentation "Packet being received - if
   interface goes down reception is not completed."))
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
  "Send to local address attached to this interface using layer 2 protocols"
  (when (up-p interface)
    (cond
      ((or (eql address :broadcast) (typep address 'hardware-address))
       (send (layer2:protocol interface) packet protocol :address address))
      ((arp interface)
       (send (arp interface) packet protocol :address address))
      (t
       (send (layer2:protocol interface) packet protocol
             :address (network-to-hardware-address address interface))))))

    (if (busy-p (link interface))
        (enqueue packet (packet-queue interface))
        (send (link interface) packet interface)))

(defmethod (setf up-p)(value (interface interface))
  ;; if fail during packet drop received packet
  (unless (eql value (up-p interface))
    (setf (slot-value interface 'receiving) nil)
    (setf (slot-value interface 'up-p) value)))

(defmethod send-complete((interface interface) packet link &optional fail)
  (send-complete (layer2:protocol interface) packet interface fail))

;; (defmethod receive-start((interface interface) packet link)
;;   (if (up-p interface)
;;       (progn
;;         (setf (slot-value interface 'receiving) packet)
;;         (receive-start (layer2:protocol interface) packet interface))
;;       (setf (slot-value interface 'receiving) nil)))

;; (defmethod receive((interface interface) packet link &key errors &allow-other-keys)
;;   (when (eql packet (slot-value interface 'receiving))
;;     (receive (layer2:protocol interface) packet interface :errors errors))
;;   (setf (slot-value interface 'receiving) nil))


;; (defmethod up((interface interface) &key (inform-routing t))
;;   (unless (slot-value interface 'up-p)
;;     (setf (slot-value interface 'up-p) t)
;;     (when inform-routing
;;       (routing:topology-changed interface))))

;; (defmethod down((interface interface) &key (inform-routing t))
;;   (when (slot-value interface 'up-p)
;;     (setf (slot-value interface 'up-p) nil)
;;     (when inform-routing
;;       (routing:topology-changed interface))))

;; (defmethod buffer-available-p(size (interface interface))
;;   (buffer-available-p size (queue interface)))

;; (defmethod send((interface interface) (packet packet) (dst ipaddr)
;;                 &optional (type #x0800))
;;   (send interface packet (or (arp interface) (ip-to-mac dst interface)) type))

;; (defmethod send((interface interface) (packet packet) (dst (eql nil))
;;                 &optional (type #x0800))
;;   (send interface packet (macaddr (default-peer-interface interface)) type))

;; (defmethod send((interface interface) (packet packet) (dst macaddr)
;;                 &optional (type #x0800))
;;   (unless (up-p interface)
;;      ;; if down down't forward - just trace drop
;;     (write-trace (node interface)  nil :drop nil :packet packet :text "L2-ID")
;;     (return-from send nil))
;;   (let ((protocol (layer2:protocol interface)))
;;     (layer2:build-pdu protocol (macaddr interface) dst packet type)
;;     (cond
;;       ((not (layer2:busy-p protocol))
;;        (layer2:send protocol packet))
;;       (t (enque packet interface)))))

;; (defmethod enque(packet (interface interface))
;;   (or (enque packet (queue interface))
;;       (progn
;;         ;; not enqued so trace as dropped
;;         (write-trace (node interface)  nil
;;                      :drop nil :packet packet :text "L2-QD")
;;         (when (notification packet)
;;           ;; sender has requested notification so schedule one in 1ms
;;           (schedule 1e-3 (notification packet)))
;;         nil)))

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

;; (defmethod peer-node-p((node node) (interface interface))
;;   (peer-node-p node (link interface)))

;; (defmethod network-to-hardware-address((addr network-address) (interface interface))
;;   (when-bind(peer-interface (find addr (peer-interfaces interface) :key #'network-address))
;;             (hardware-address peer-interface)))

;; (defmethod peer-interfaces((interface interface))
;;   (remove interface (peer-interfaces (link interface))))

;; (defgeneric neighbours(link)
;;   (:documentation "Return the routing neighbours to this interface")
;;   (:method ((interface interface))
;;     (let ((link (link interface)))
;;       (mapcan #'(lambda(peer-interface)
;;                   (unless (eql peer-interface interface)
;;                     (list (routing:make-neighbour
;;                            :node (node peer-interface)
;;                            :interface interface
;;                            :weight (weight link)))))
;;               (peer-interfaces link)))))

;; (defmethod default-peer-interface((interface interface))
;;   (default-peer-interface (link interface)))

;; (defmethod peer-node-ipaddr ((node node) (interface interface))
;;   (peer-node-ipaddr node (link interface)))

;; (defmethod local-ipaddr-p((ipaddr ipaddr) (interface interface))
;;   (if (ipmask interface)
;;       (address= (subnet ipaddr (ipmask interface))
;;                 (subnet (ipaddr interface) (ipmask interface)))
;;       (address= ipaddr (ipaddr interface))))

;; (defmethod receive((interface interface) packet &optional lostp)
;;   (if lostp
;;       (write-trace (node interface)  nil :drop nil :packet packet
;;                    :text "L2-BER")
;;       (layer2:receive (layer2:protocol interface) packet)))

;; (defmethod make-new-interface(link &key ipaddr ipmask)
;;   (make-instance 'interface :link link :ipaddr ipaddr :ipmask ipmask))

;; (defmethod reset((interface interface))
;;   (reset (link interface))
;;   (cancel-all-timers interface)
;;   (setf (pending-notification interface) nil))

;; (defmethod node:location((interface interface))
;;   (node:location (node interface)))