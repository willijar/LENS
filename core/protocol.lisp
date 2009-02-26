;; $Id$
;; Protocol Stack Layer Implementation
;; Copyright (C) 2006 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;; This file is part of Lisp Educational Network Simulator (LENS)

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:

;;

;;; Code:

(in-package :protocol)

(defparameter +number-protocol-layers+ 7 "Number of ISO Protocol layers")

(defgeneric layer(protocol)
  (:documentation "Determine layer of a protocol"))

(defun make-protocol-graph()
  (make-array 2 :initial-element nil))

(defvar *common-protocol-graph* (make-protocol-graph)
  "The mapping of protocol numbers to global protocol instances")

(defgeneric protocol-graph(entity)
  (:documentation "Return the protocol graph associated with an entity")
  (:method (entity)
    (declare (ignore entity))
    *common-protocol-graph*))

(defgeneric insert-protocol(layer protocol-number protocol graph)
  (:documentation  "Insert a protocol into the protocol graph")
  (:method(layer protocol-number protocol (graph vector))
    (let* ((idx (- layer 3))
           (h (or (svref  graph idx)
                 (setf (svref graph idx)
                       (make-hash-table)))))
      (setf (gethash protocol-number h) protocol))))

(defgeneric remove-protocol(layer protocol-number graph)
  (:documentation  "Remove an entry from the protocol graph")
  (:method ((layer integer) (protocol-number integer) (graph vector))
    (let ((h (svref  graph (- layer 3))))
      (when h (remhash protocol-number h)))))

(defgeneric find-protocol(layer protocol-number graph)
  (:documentation "Lookup a protocol - if no proto eql :default
returns a default for that layer")
  (:method ((layer integer) protocol-number (graph vector))
    (let ((h (svref graph (- layer 3))))
      (when h
        (if (eql protocol-number :default)
            (when (= 1 (hash-table-count h))
              (maphash
               #'(lambda(k v)
                   (declare (ignore k))
                   (return-from find-protocol v))
               h))
            (gethash protocol-number h))))))

(defgeneric sublayer(pdu)
  (:documentation
   "Return the PDU sublayer number (if applicable e.g. LLCSNAP)"))

(defgeneric version(pdu)
  (:documentation "Return the PDU version number (used for tracing)")
  (:method(pdu) (declare (ignore pdu))))

(defmethod priority(pdu) (declare (ignore pdu)) 0)

(in-package :protocol.layer2)

(defclass protocol()
  ((layer :initform 2 :reader layer :allocation :class)
   (interface :initarg :iface :accessor interface
              :documentation "Interface for this protocol"))
  (:documentation "Layer 2 protocol base class"))

(defclass pdu()
  ()
  (:documentation "The base class for all layer two  protocol data units"))

(defmethod layer((p pdu)) 2)

(defgeneric busy-p(protocol)
  (:documentation "True if proto/link is busy"))

(defgeneric data-request(protocol packet)
  (:documentation "Packet received to be passed downwards to peer."))

(defgeneric data-indication(protocol packet)
  (:documentation "Packet received to be passed upwards"))

(defgeneric build-pdu(protocol src-address dst-address packet &optional type)
  (:documentation "Build and append a layer2 pdu to the specified packet."))

(in-package :protocol.layer3)

(defclass protocol()
  ((layer :initform 3 :reader layer :allocation :class))
  (:documentation "Layer 3 protocol base class. Layer 3 protocols are
global singleton instances"))

(defgeneric protocol-number(entity)
  (:documentation "Return the IANA PPP DLL number.
See http://www.iana.org/assignments/ppp-numbers"))

(defmethod initialize-instance :after ((protocol protocol)
                                       &key &allow-other-keys)
  (protocol:insert-protocol
   (layer protocol) (protocol-number protocol) protocol
   protocol:*common-protocol-graph*))

(defclass pdu()
  ()
  (:documentation "The base class for all layer two  protocol data units"))

(defmethod layer((p pdu)) 3)

(defgeneric data-request(protocol node packet &key &allow-other-keys)
  (:documentation "Packet received to be passed downwards"))

(defgeneric data-indication(protocol interface packet)
  (:documentation "packet received from lower layer"))

(defgeneric find-interface(protocol node addr)
  (:documentation "Find the forwarding interface"))

(in-package :protocol.layer4)

(defclass demux()
  ((layer :initform 4 :reader layer :allocation :class))
  (:documentation "Base class for Layer 4 demultiplexers. These are
global singleton instanciated classes responsible for finding specific
layer 4 protocol instances by port"))

(defmethod initialize-instance :after ((protocol demux)
                                       &key &allow-other-keys)
  (protocol:insert-protocol
   (layer protocol) (protocol-number protocol) protocol
   protocol:*common-protocol-graph*))

(defclass protocol()
  ((layer :initform 4 :reader layer :allocation :class)
   (node :initarg :node :initform nil :accessor node
         :documentation "Local Node")
   (peer-address :initarg :peer-address :initform nil :type ipaddr
                 :accessor peer-address
                 :documentation "IP address of peer")
   (peer-port  :initarg :peer-port :initform nil
               :type ipport :accessor peer-port
               :documentation "Port of peer")
   (local-port :initarg :local-port :initform nil
               :type ipport :accessor local-port
               :documentation "Local port number")
   (notification
    :initform nil :accessor notification
    :documentation "Entity to receive notification for all packets")
   (application :initarg :application :initform nil
                :type application :accessor application
                :documentation "Local application object")
   (layer3:protocol :initarg :layer3 :initform (layer3:ipv4)
                    :type layer3:protocol :reader layer3:protocol
                    :documentation "Layer 3 protocol to use")
   (ttl :accessor ttl :initform 64 :documentation "Layer 3 ttl")
   (fid :accessor fid :initform 0 :documentation "Flow id")
   (tos :accessor tos :initform 0
        :documentation "Layer 3 type of service")
   (interface :initform nil :documentation "Interface used for this flow"))
  (:documentation "Base class for all layer 4 protocols states"))

(defgeneric protocol-number(entity)
  (:documentation "Return the IANA protocol number.
See http://www.iana.org/assignments/protocol-numbers"))

(defgeneric data-indication(protocol node packet dst-address interface)
  (:documentation "Indication that packet has been received by layer 3
protocol addressed to this node with protocol number matching this
one"))

(defgeneric send(data protocol &key dst-address dst-port)
  (:documentation "Send data using this protocol to dst-address and port or
default dst-address and port. Return no bytes sent.")
  (:method((size integer) protocol &key
           (dst-address (peer-address protocol))
           (dst-port (peer-port protocol)))
    (send (make-instance 'layer5:data :size size) protocol
          :dst-address dst-address :dst-port dst-port))
  (:method((data vector) protocol &key
           (dst-address (peer-address protocol))
           (dst-port (peer-port protocol)))
    (send (make-instance 'layer5:data :contents data) protocol
          :dst-address dst-address :dst-port dst-port)))

(defgeneric local-address(protocol)
  (:documentation "Local IPaddress for this protocol")
  (:method ((p protocol)) (ipaddr (node p))))

(defgeneric connect(protocol peer-address peer-port)
  (:documentation "Connection to a remote host. Return true if
initiated OK - does not mean was successfully completed if TCP"))

(defgeneric close-connection(protocol)
  (:documentation "Close an open connection, return success"))

(defgeneric make-packet(protocol)
  (:documentation "Allocate a new packet")
  (:method ((protocol protocol))
    (make-instance 'packet:packet :notification (notification protocol))))

(defgeneric bind(protocol &key port)
  (:documentation "Bind to specific or available port and return successs")
  (:method((protocol protocol) &key port)
    (when (local-port protocol) (unbind protocol))
    (when (node protocol)
      (setf (local-port protocol)
            (node:bind protocol (node protocol)
                            :local-port port)))))

(defgeneric unbind(protocol &key port protocol-number)
  (:documentation "Unbind a protocol from a specific port")
  (:method((protocol protocol)
           &key (port (local-port protocol))
           (protocol-number (protocol-number protocol)))
    (when (node protocol)
      (when (= port (local-port protocol))
        (setf (local-port protocol) nil))
      (node:unbind protocol (node protocol)
                        :port port
                        :protocol-number protocol-number))))

(defmethod interface((protocol protocol))
  (or (slot-value protocol 'interface)
      (when (peer-address protocol)
        (setf (slot-value protocol 'interface)
              (layer3:find-interface (layer3:protocol protocol)
                                     (node protocol)
                                     (peer-address protocol))))))

(defmethod buffer-available-p(size (protocol protocol))
  (if (interface protocol)
      (buffer-available-p size (interface protocol))
      t))

(defun request-notification(protocol)
  "Rquest notification when buffer becomes available"
  (when (interface protocol)
    (interface:add-notify protocol (interface protocol))))

(defun cancel-notification(protocol)
  "Cancel a previous notification"
  (when (interface protocol)
    (interface:cancel-notify protocol (interface protocol))))

(defclass pdu()
  ()
  (:documentation "The base class for all layer four protocol data units"))

(defmethod layer((p pdu)) 4)

(in-package :protocol.layer5)

(defclass data()
  ((size :type integer :initarg :size
         :documentation "Number of data bytes if no contents")
   (contents :type (or null (vector octet *)) :initform nil :reader contents
             :initarg :contents
             :documentation "Vector of data if we are sending")
   (msg-size :type integer :initarg :msg-size :initform 0 :reader msg-size
             :documentation "Total size of message")
   (response-size :type integer :initarg :response-size :initform 0
                  :reader response-size
                  :documentation "Size of response requested")
   (checksum :type word :initform 0 :initarg :checksum
             :documentation "Checksum value"))
  (:documentation "Data PDU"))

(defmethod print-object((data data) stream)
  (print-unreadable-object (data stream :type t :identity t)
    (format stream "~:/print-eng/bytes" (size data))))

(defmethod size((pdu data))
  (if (contents pdu) (length (contents pdu)) (slot-value pdu 'size)))

(defmethod copy((data data))
  (let ((copy (copy-with-slots data '(msg-size response-size checksum))))
    (if (contents data)
        (setf (slot-value copy 'contents) (copy-seq (contents data)))
        (setf (slot-value copy 'size) (slot-value data 'size)
              (slot-value copy 'contents) nil))
    copy))

(defmethod layer((pdu data)) 5)

(defgeneric checksum(entity)
  (:documentation "Perform 16 bit xor checksum across entity data")
  (:method((data vector))
    "Perform 16 bit xor checksum across a vector of (unsigned-byte 8)"
    (check-type data (vector (unsigned-byte 8) *))
    (multiple-value-bind(s2 rem) (floor (length data) 2)
      (let ((sum 0))
        (declare (type word sum))
        ;; do 16 bit checksum across contents
        (loop :for i :from 0 :below s2
              :do (setf sum (logxor sum
                                    (ash (aref data i) 8)
                                    (aref data (1+ i)))))
        ;; odd size - add in last octet
      (when (= 1 rem)
        (setf sum
              (logxor sum
                      (ash (aref data (1- (length data))) 8))))
      sum)))
  (:method((pdu data))
    (let ((contents (contents pdu)))
      (if contents
          (checksum contents)
          (slot-value pdu 'checksum)))))

(declaim (inline offset-from-seq size-from-offset))

(defun offset-from-seq(f o)
  (when (< o f) (error "offset sequence < first sequence in data"))
  (- o f))

(defun size-from-offset(o data)
  (let ((size (size data)))
    (if (> o size) 0 (- size o))))

(defun size-from-seq(f o data)
  (size-from-offset (offset-from-seq f o) data))

(defun copy-from-offset(size offset data)
  (let ((size (min size (size-from-offset offset data))))
    (when (> size 0)
      (make-instance
       'data
       :contents (when (contents data)
                   (subseq (contents data) offset (+ offset size)))
       :msg-size (msg-size data)
       :size (unless (contents data) size)
       :response-size (response-size data)))))

(defun copy-from-seq(size f o data)
  (copy-from-offset size (offset-from-seq f o) data))

(defun remove-data(s data)
  "Remove s bytes from start of data"
  (with-slots(size contents) data
    (let ((r (min s size)))
      (setf size (- size r))
      (when contents
        (if (> size 0)
            (setf contents (subseq contents s))
            (setf contents nil))))))

(defun add-data(s data)
  "Adds s (either byte-count or vector of octets) onto end of data"
  (multiple-value-bind(s d)
      (etypecase s
        (integer (values s nil))
        ((vector octet *) (values (length s) s))
        (data (values (size s) (contents s))))
    (with-slots(size contents) data
      (if contents
          (setf contents
                (concatenate 'vector contents
                             (or d (make-array s
                                               :element-type 'octet
                                               :initial-element 0))))
          (when d (setf contents (copy-seq d))))
      (setf size (+ size s)))))
