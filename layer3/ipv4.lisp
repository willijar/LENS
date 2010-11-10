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

(in-package :protocol.ipv4)

(defclass ipv4-header-option(pdu)
  ((option-number :type octet :initarg :option-number :reader option-number))
  (:documentation "The IP options part of the IP Header."))

(defmethod size((pdu ipv4-header-option)) 0)

(defmethod copy((pdu ipv4-header-option))
  (make-instance 'ipv4-header-option :option-number (option-number pdu)))

(defclass ipv4-header(pdu)
  ((version :initarg :version :initform 4 :type octet :reader version)
   (service-type :initform 0 :type octet :reader priority
                 :initarg :service-type)
   (total-length :initform 0 :type word :accessor total-length)
   (identification :initform 0 :type word)
   (flags :initform 0 :type octet)
   (fragment-offset :initform 0 :type word)
   (ttl :initform 64 :type octet :initarg :ttl :accessor ttl
        :documentation "Default ttl")
   (layer4:protocol-number
    :initform #x0800 :type octet :initarg :protocol-number
    :reader layer4:protocol-number
    :documentation "Layer 4 protocol number")
   (header-checksum :initform 0 :type word)
   (src-address :type ipaddr :accessor src-address :initarg :src-address)
   (dst-address :type ipaddr :accessor dst-address :initarg :dst-address)
   (options :type list :accessor options
            :initform nil))
  (:documentation "The IP version 4 header"))

(defmethod size((pdu ipv4-header))
  (+ 20 (reduce #'+ (slot-value pdu 'options) :key #'size)))

(defmethod copy((h ipv4-header))
  (let ((copy
         (copy-with-slots
          h
          '(uid version service-type total-length identification
            flags fragment-offset ttl protocol header-checksum
            src-address dst-address))))
    (setf (slot-value copy 'options) (mapcar #'copy (options h)))
    copy))

(defmethod pdu-trace((pdu ipv4-header) detail stream &key packet text)
  (format stream " ~@[~A~] L3" text)
  (write-pdu-slots
   pdu
   '((version "-~A") header-length service-type total-length
     identification flags fragment-offset ttl protocol header-checksum
     src-address dst-address)
   detail
   stream)
  (when (member 'uid detail)
    (format stream " ~D" (if packet (uid packet) 0))))

(defclass ipv4(protocol)
  ((protocol-number :initform #x0800 :reader protocol-number
                    :allocation :class)
   (version :initform 4 :reader version :allocation :class)
   (route-locally :type boolean :initform nil :accessor route-locally
                  :documentation "Allows transport layer protocols on the same
node to communicate with each other."))
  (:documentation "IP v4 implementation"))

;; singleton instance accessed through function of same name
(let ((ipv4))
  (defun ipv4() (or ipv4 (setf ipv4 (make-instance 'ipv4)))))

(defmethod default-trace-detail((entity ipv4))
  '(ttl layer4:protocol-number src-address dst-address))

(defmethod send((ipv4 ipv4) node packet
                        &rest args
                        &key src-address dst-address (ttl 64)
                        protocol-number (tos 0)
                        &allow-other-keys)
  (cond
    ((not (node:call-callbacks (layer ipv4) (protocol-number ipv4)
                               :tx packet node))
     nil)
    ((broadcast-p dst-address) ;; broadcast address
     (apply #'broadcast (nconc (list ipv4 node packet) args)))
    (t
     (let ((iphdr (make-instance 'ipv4-header
                                 :src-address (ipaddr node)
                                 :dst-address dst-address
                                 :ttl ttl
                                 :protocol-number protocol-number
                                 :service-type tos)))
       (push-pdu iphdr packet)
       (setf (total-length iphdr) (size packet)) ;; size includes header
       (cond
         ((and (route-locally ipv4) (local-ipaddr-p dst-address node))
          ;; do local routing - send packet back up stack
          (write-trace node ipv4 iphdr nil :packet packet :text "-")
          (receive ipv4 (find-interface ipv4 node dst-address) packet))
         (t ;; send packet over interface
          (let ((routing-entry (find-route dst-address node packet)))
            (setf (src-address iphdr)
                  (or src-address ;; spoofing src
                      (ipaddr (interface routing-entry)) ;; src address
                      (ipaddr node))) ;; default src address from node
            (write-trace node ipv4 iphdr nil :packet packet :text "-")
            (interface:send (interface routing-entry)
                            packet
                            (next-hop routing-entry)
                            (protocol-number ipv4)))))))))

(defgeneric process-ip-option(option interface packet)
  (:documentation "Process an ip option")
  (:method(option interface packet)
    (error "Unknown IP option ~S" option)))

(defmethod receive((ipv4 ipv4) interface packet)
  "ipv4 data arrival"
  (let ((node (node interface)))
    (unless (node:call-callbacks
             (layer ipv4) (protocol-number ipv4)
             :rx packet node interface)
      (return-from receive))
    (let* ((iphdr (peek-pdu packet))
           (dst-address (dst-address iphdr))
           (route (find-route dst-address node packet)))
      (write-trace node ipv4 iphdr nil :packet packet :text "+")
      ;; process ip options
      (dolist(option (options iphdr))
        (process-ip-option option interface packet))
      (cond
        ((or (node:local-ipaddr-p dst-address node)
             (and (node:local-ipaddr-p (next-hop route) node)
                  (interface route))
             (broadcast-p dst-address))
         ;; destined for this node
         (packet:skip-pdu packet)
         (let ((l4demux (node:find-protocol
                         4 (layer4:protocol-number iphdr) node)))
           (unless l4demux
             (error "IPV4 indication with no l4demux object"))
           (layer4:receive
            l4demux node packet (dst-address iphdr) interface)))
        ((zerop (decf (ttl iphdr)))
         ;; TTL expired - drop, log and notify ICMP
         (write-trace node ipv4 :drop nil :packet packet :text "L3-TTL")
         (icmp:time-exceeded node packet iphdr :ttl-exceeded))
        ((not (interface route))
         ;; can't route so drop it
         (write-trace node ipv4 :drop nil :packet packet :text "L3-NR")
         (icmp:destination-unreachable
          node packet iphdr (peek-pdu packet 1)
          :host-unreachable))
        ((eql (interface route) interface)
         ;; routing loop - log dropped packet
         (write-trace node ipv4 :drop nil :packet packet :text "L3-RL"))
        (t
          ;; forward to next hop
          (write-trace node ipv4 iphdr nil :packet packet :text "-")
          (interface:send
           (interface route) packet (routing:next-hop route)
            (protocol-number ipv4)))))))

(defun broadcast(ipv4 node packet
                 &key src-address dst-address (ttl 64)
                 protocol-number (tos 0))
  "IPV4 broadcast.
First Common processing (before looping on all interfaces)
For broadcasts, the \"src\" argument is used to determine
which interfaces the packet is broadcast to.  If it is nil
or broadcast-p then it is sent on all interfaces.  If not,
it is sent only on the interface with an IP address matching
the specified src address."
     (let ((iphdr (make-instance
                   'ipv4-header
                   :src-address  (ipaddr node)
                   :dst-address dst-address
                   :ttl ttl
                   :protocol protocol-number
                   :service-type tos))
           (broadcast-p (or (not src-address) (broadcast-p src-address))))
       (push-pdu iphdr packet)
       (setf (total-length iphdr) (size packet))
       (loop :for iface :across (node:interfaces node)
             :when (and (interface:link iface)
                        (or broadcast-p (address= (ipaddr iface) src-address)))
             :do
             (setf (src-address iphdr)
                   (if broadcast-p
                       (or (ipaddr iface) (ipaddr node))
                       src-address))
             (let ((p (copy packet)))
               (write-trace node (ipv4) iphdr nil :packet p :text "-")
               (interface:send iface p (macaddr :broadcast)
                                    (protocol-number ipv4))))))

(defmethod find-interface((ipv4 ipv4) (node node) (addr ipaddr))
  (interface (find-route addr node nil)))

(defclass ipv4-demux(layer4:demux)
  ()
  (:documentation "Base class for TCP or UDP demux"))

(defmethod layer4:receive((demux ipv4-demux)
                                  node packet dst-address
                                  interface)
  (when (node:call-callbacks
           (layer demux) (layer4:protocol-number demux)
           :rx packet node interface)
    (let* ((pdu (peek-pdu packet))
           (layer4protocol
            (node:lookup-by-port (layer4:protocol-number pdu) node
                                 :local-port (dst-port pdu))))
      (cond
        (layer4protocol
         (layer4:receive
          layer4protocol node packet dst-address interface))
        (t ; no port - log and discard
         (write-trace node (ipv4)
                      :drop nil :packet packet :text "L3-NP")
         (icmp:destination-unreachable node packet
                                       (peek-pdu packet -1)
                                       pdu :port-unreachable))))))