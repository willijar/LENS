;; $Id$
;; Protocol Stack Layer Implementation
;; Copyright (C) 2006 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;; This file is part of Lisp Educational Network Simulator (LENS)

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:

;; Protocols should use a split-phase interface for sending
;; send on lower layer
;; starts the sending process - if it returns true, false if not
;; send-complete on upper layer is called with the packet and success arguments
;; when sending is completed
;;; Code:

(in-package :protocol)

(defclass protocol()
  ((node :initarg :node :reader node))
  (:documentation "Base class for all protocol entities"))

(defgeneric protocol-number(entity)
  (:documentation "Return the IANA protocol number.
See http://www.iana.org/assignments/protocol-numbers")
  (:method((protocol protocol)) (get (type-of protocol) 'protocol-number))
  (:method((protocol symbol)) (get protocol 'protocol-number)))

(defmethod layer((protocol protocol)) (get (type-of protocol) 'layer))
(defmethod layer((protocol symbol)) (get protocol 'layer))

;; split-phase transmission (going down protocol layers)
(defgeneric send(receiver packet sender &key &allow-other-keys)
  (:documentation "Called by sender to start sending packet - Returns
  true if packet accepted for transmission, false otherwise."))

(defgeneric send-complete(sender packet receiver &key fail &allow-other-keys)
  (:documentation "Called by receiver when sending completed to notify
  receiver - fail-reason indicates if packet was dropped - null if successful")
  (:method(sender packet receiver &key &allow-other-keys)
    (declare (ignore sender packet receiver fail))))

;; split phase reception going up protocol stack
(defgeneric receive-start(receiver packet sender)
  (:documentation "Called by sender to start receiver receiving a packet.")
  (:method(receiver packet sender)
    (declare (ignore receiver packet sender))))

(defgeneric receive(receiver packet sender &key &allow-other-keys)
  (:documentation "Called by packet when to pass received packet up to
  receiver (once reception is complete)"))

(in-package :protocol.layer2)

(defclass protocol(protocol:protocol)
  ((layer :initform 2 :reader layer :allocation :class)
   (interface :initarg :iface :accessor interface
              :documentation "Interface for this protocol"))
  (:documentation "Layer 2 protocol base class"))

(defclass pdu(protocol:pdu)
  ((layer :initform 2 :reader protocol:layer :allocation :class))
  (:documentation "The base class for all layer two  protocol data units"))

(defmethod send :around ((protocol protocol) packet layer3 &key address)
  (cond ((or (typep address 'hardware-address) (eql address :broadcast))
         (call-next-method))
        ((arp interface)
         (send (arp interface) packet layer3 :address address))
        (t
         (call-next-method protocol packet layer3
                           :address (network-to-hardware-address address interface)))))


(defgeneric build-pdu(protocol src-address dst-address packet &optional type)
  (:documentation "Build and append a layer2 pdu to the specified packet."))

(in-package :protocol.layer3)

(defclass protocol(protocol:protocol)
  ((layer :initform 3 :reader layer :allocation :class))
  (:documentation "Layer 3 protocol"))

(defclass pdu(packet:pdu)
  ((layer :initform 3 :reader protocol:layer :allocation :class))
  (:documentation "The base class for all layer three protocol data units"))

(defvar *standard-protocols* nil "List of standard registered layer 3
protocols")

(defgeneric register-protocol(protocol entity &optional replace)
  (:documentation "Register a layer 3 protocol. If replace is true this will
  replace the previously registered protocol with the same number."))

(defgeneric delete-protocol(protocol entity)
  (:documentation "Delete a layer 3 protocol from entity")
  (:method((protocol integer) (protocols sequence))
    (delete protocol protocols :key #'protocol-number :test #'=))
  (:method(protocol entity)
    (delete-protocol (protocol-number protocol) entity)))

(defgeneric find-protocol(protocol entity)
  (:documentation "Find a layer 3 protocol on entity")
  (:method((protocol integer) (protocols sequence))
    (find protocol protocols :key #'protocol-number :test #'=))
  (:method(protocol entity)
    (find-protocol (protocol-number protocol) entity)))

(defmethod register-protocol(protocol (protocols list) &optional replace)
  (cons protocol
        (if replace
            (delete-protocol protocol protocols)
            (when (find-protocol protocol protocols)
              (error "Unable to register a duplicate protocol ~A" protocol)))))

(defmethod register-protocol((protocol symbol) (protocol-number integer) &optional (replace t))
  "Register the protocol number for a protocol class name, if replace
is true add it to the list of standard protocols"
    (setf (get protocol 'protocol::layer) 3
          (get protocol 'protocol::protocol-number) protocol-number)
    (when replace
      (setf *standard-protocols* (register-protocol protocol *standard-protocols*))))

(defmethod delete-protocol((protocol integer) node)
  (setf (slot-value node 'protocols)
        (delete-protocol protocol (slot-value node 'protocols))))

(defmethod find-protocol((protocol-number integer) node)
  "Find an already existent protocol or add one"
  (or (find-protocol protocol-number (slot-value node 'protocols))
      (let ((protocol (find-protocol protocol-number *standard-protocols*)))
        (when protocol (make-instance protocol :node node)))))

(defmethod register-protocol((protocol protocol) node &optional (replace nil))
  (setf (slot-value node 'protocols)
        (register-protocol protocol (slot-value node 'protocols) replace)))

(defmethod initialize-instance :after ((protocol protocol)
                                       &key &allow-other-keys)
  (register-protocol protocol (node protocol)))

(defgeneric find-interface(protocol addr)
  (:documentation "Find the forwarding interface"))

(in-package :protocol.layer4)

(defvar *standard-protocols* nil "List of standard registered layer 4
protocols")

(defclass protocol(protocol:protocol)
  ((layer :initform 4 :reader layer :allocation :class)
   (node :initarg :node :reader node
         :documentation "Local Node"))
  (:documentation "Layer 3 protocol"))

(defclass pdu(packet:pdu)
  ((layer :initform 4 :reader protocol:layer :allocation :class))
  (:documentation "The base class for all layer three protocol data units"))

(defgeneric register-protocol(protocol entity &optional replace)
  (:documentation "Register a layer 4 protocol. If replace is true this will
  replace the previously registered protocol with the same number."))

(defgeneric delete-protocol(protocol entity)
  (:documentation "Delete a layer 4 protocol from entity")
  (:method((protocol integer) (protocols sequence))
    (delete protocol protocols :key #'protocol-number :test #'=))
  (:method(protocol entity)
    (delete-protocol (protocol-number protocol) entity)))

(defgeneric find-protocol(protocol entity)
  (:documentation "Find a layer 4 protocol on entity")
  (:method((protocol integer) (protocols sequence))
    (find protocol protocols :key #'protocol-number :test #'=))
  (:method(protocol entity)
    (find-protocol (protocol-number protocol) entity)))

(defmethod register-protocol(protocol (protocols list) &optional replace)
  (cons protocol
        (if replace
            (delete-protocol protocol protocols)
            (when (find-protocol protocol protocols)
              (error "Unable to register a duplicate protocol ~A" protocol)))))

(defmethod register-protocol((protocol symbol) (protocol-number integer) &optional (replace t))
    (setf (get protocol 'protocol::layer) 4
          (get protocol 'protocol::protocol-number) protocol-number)
    (when replace
      (setf *standard-protocols*
            (register-protocol protocol *standard-protocols*))))

(defmethod delete-protocol((protocol integer) node)
  (setf (slot-value node 'protocols)
        (delete-protocol protocol (slot-value node 'protocols))))

(defmethod find-protocol((protocol-number integer) node)
  (or (find-protocol protocol-number (slot-value node 'protocols))
      (let ((protocol (find-protocol protocol-number *standard-protocols*)))
        (when protocol (make-instance protocol :node node)))))

(defmethod register-protocol((protocol protocol) node &optional (replace nil))
  (setf (slot-value node 'protocols)
        (register-protocol protocol (slot-value node 'protocols) replace)))

(defmethod initialize-instance :after ((protocol protocol)
                                       &key &allow-other-keys)
  (register-protocol protocol (node protocol)))

;; (defclass flow()
;;   ((protocol :type protocol :initarg :protocol :reader protocol)
;;    (peer-address :initarg :peer-address :initform nil :type hardware-address
;;                  :accessor peer-address
;;                  :documentation "IP address of peer")
;;    (peer-port  :initarg :peer-port :initform nil
;;                :type ipport :accessor peer-port
;;                :documentation "Port of peer")
;;    (local-port :initarg :local-port :initform nil
;;                :type ipport :accessor local-port
;;                :documentation "Local port number")
;;    (notification
;;     :initform nil :accessor notification
;;     :documentation "Entity to receive notification for all packets")
;;    (application :initarg :application :initform nil
;;                 :type application :accessor application
;;                 :documentation "Local application object")
;;    (layer3:protocol :initarg :layer3 :initform (layer3:ipv4)
;;                     :type layer3:protocol :reader layer3:protocol
;;                     :documentation "Layer 3 protocol to use")
;;    (ttl :accessor ttl :initform 64 :documentation "Layer 3 ttl")
;;    (fid :accessor fid :initform 0 :documentation "Flow id")
;;    (tos :accessor tos :initform 0
;;         :documentation "Layer 3 type of service")
;;    (interface :initform nil :documentation "Interface used for this flow"))
;;   (:documentation "Base class for all layer 4 protocols states"))

;; (defmethod initialize-instance :after((protocol protocol) &key &allow-other-keys)
;;   (register-protocol protocol (node protocol)))

;; (defgeneric receive(protocol node packet dst-address interface)
;;   (:documentation "Indication that packet has been received by layer 3
;; protocol addressed to this node with protocol number matching this
;; one"))

;; (defgeneric send(data protocol &key dst-address dst-port)
;;   (:documentation "Send data using this protocol to dst-address and port or
;; default dst-address and port. Return no bytes sent.")
;;   (:method((size integer) protocol &key
;;            (dst-address (peer-address protocol))
;;            (dst-port (peer-port protocol)))
;;     (send (make-instance 'layer5:data :size size) protocol
;;           :dst-address dst-address :dst-port dst-port))
;;   (:method((data vector) protocol &key
;;            (dst-address (peer-address protocol))
;;            (dst-port (peer-port protocol)))
;;     (send (make-instance 'layer5:data :contents data) protocol
;;           :dst-address dst-address :dst-port dst-port)))

;; (defgeneric local-address(protocol)
;;   (:documentation "Local IPaddress for this protocol")
;;   (:method ((p protocol)) (ipaddr (node p))))

;; (defgeneric connect(protocol peer-address peer-port)
;;   (:documentation "Connection to a remote host. Return true if
;; initiated OK - does not mean was successfully completed if TCP"))

;; (defgeneric close-connection(protocol)
;;   (:documentation "Close an open connection, return success"))

;; (defgeneric make-packet(protocol)
;;   (:documentation "Allocate a new packet")
;;   (:method ((protocol protocol))
;;     (make-instance 'packet:packet :notification (notification protocol))))

;; (defgeneric bind(protocol &key port)
;;   (:documentation "Bind to specific or available port and return successs")
;;   (:method((protocol protocol) &key port)
;;     (when (local-port protocol) (unbind protocol))
;;     (when (node protocol)
;;       (setf (local-port protocol)
;;             (node:bind protocol (node protocol)
;;                             :local-port port)))))

;; (defgeneric unbind(protocol &key port protocol-number)
;;   (:documentation "Unbind a protocol from a specific port")
;;   (:method((protocol protocol)
;;            &key (port (local-port protocol))
;;            (protocol-number (protocol-number protocol)))
;;     (when (node protocol)
;;       (when (= port (local-port protocol))
;;         (setf (local-port protocol) nil))
;;       (node:unbind protocol (node protocol)
;;                         :port port
;;                         :protocol-number protocol-number))))

;; (defmethod interface((protocol protocol))
;;   (or (slot-value protocol 'interface)
;;       (when (peer-address protocol)
;;         (setf (slot-value protocol 'interface)
;;               (layer3:find-interface (layer3:protocol protocol)
;;                                      (node protocol)
;;                                      (peer-address protocol))))))

;; (defmethod buffer-available-p(size (protocol protocol))
;;   (if (interface protocol)
;;       (buffer-available-p size (interface protocol))
;;       t))

;; (defun request-notification(protocol)
;;   "Rquest notification when buffer becomes available"
;;   (when (interface protocol)
;;     (interface:add-notify protocol (interface protocol))))

;; (defun cancel-notification(protocol)
;;   "Cancel a previous notification"
;;   (when (interface protocol)
;;     (interface:cancel-notify protocol (interface protocol))))

(in-package :protocol.layer5)

(defclass data()
  ((layer :initform 5 :allocation :class :reader layer)
   (length-bytes :type integer :initarg :length-bytes
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
    (format stream "~:/print-eng/bytes" (length-bytes data))))

(defmethod length-bytes((pdu data))
  (if (contents pdu) (length (contents pdu)) (slot-value pdu 'length-bytes)))

(defmethod copy((data data))
  (let ((copy (copy-with-slots data '(msg-size response-size checksum))))
    (if (contents data)
        (setf (slot-value copy 'contents) (copy-seq (contents data)))
        (setf (slot-value copy 'length-bytes) (slot-value data 'length-bytes)
              (slot-value copy 'contents) nil))
    copy))

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
  (let ((size (length-bytes data)))
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
       :length-bytes (unless (contents data) size)
       :response-size (response-size data)))))

(defun copy-from-seq(size f o data)
  (copy-from-offset size (offset-from-seq f o) data))

(defun remove-data(s data)
  "Remove s bytes from start of data"
  (with-slots(length-bytes contents) data
    (let ((r (min s length-bytes)))
      (setf length-bytes (- length-bytes r))
      (when contents
        (if (> length-bytes 0)
            (setf contents (subseq contents s))
            (setf contents nil))))))

(defun add-data(s data)
  "Adds s (either byte-count or vector of octets) onto end of data"
  (multiple-value-bind(s d)
      (etypecase s
        (integer (values s nil))
        ((vector octet *) (values (length s) s))
        (data (values (length-bytes s) (contents s))))
    (with-slots(length-bytes contents) data
      (if contents
          (setf contents
                (concatenate 'vector contents
                             (or d (make-array s
                                               :element-type 'octet
                                               :initial-element 0))))
          (when d (setf contents (copy-seq d))))
      (setf length-bytes (+ length-bytes s)))))
