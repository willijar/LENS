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
  true if packet accepted for transmission, false otherwise.")
  (:method :before(receiver packet (sender protocol) &key &allow-other-keys)
           (write-trace sender (peek-pdu packet) :packet packet :text "-"))
  (:method :around (receiver packet (sender protocol) &key &allow-other-keys)
           (when (node:call-callbacks :tx sender packet) (call-next-method))))

(defgeneric receive(receiver packet sender &key &allow-other-keys)
  (:documentation "Called by packet when to pass received packet up to
  receiver (once reception is complete)")
  (:method :before((receiver protocol) packet sender &key &allow-other-keys)
           (write-trace receiver (peek-pdu packet) :packet packet :text "+"))
  (:method :around((receiver protocol) packet sender &key &allow-other-keys)
           (when (node:call-callbacks :rx receiver packet) (call-next-method))))

(defgeneric drop(entity packet &key text &allow-other-keys)
  (:documentation "Drop a packet")
  (:method :after (entity packet &key text &allow-other-keys)
    (write-trace entity :drop :packet packet :text text))
  (:method(entity packet &key &allow-other-keys)
    (declare (ignore entity packet))))

(in-package :protocol.layer2)

(defclass protocol(protocol:protocol)
  ((layer :initform 2 :reader layer :allocation :class)
   (interface :initarg :iface :initarg :interface :accessor interface
              :documentation "Interface for this protocol"))
  (:documentation "Layer 2 protocol base class"))

(defmethod node((protocol protocol)) (node (interface protocol)))

(defclass pdu(packet:pdu)
  ((layer :initform 2 :reader protocol:layer :allocation :class))
  (:documentation "The base class for all layer two  protocol data units"))

(defmethod protocol-number((addr macaddr)) 1)

(in-package :protocol.layer3)

;; standard layer 3 protocols may be registered so they can be
;; instantiated on nodes on demand.

(defvar *standard-protocols* nil
  "List of standard registered layer 3 protocols - these can be instantiated on nodes on demand")

(defgeneric find-protocol(protocol entity)
  (:documentation "Find a layer 3 protocol on entity")
  (:method((protocol-number integer) (seq sequence))
    (find protocol-number seq :key #'protocol-number :test #'=))
  (:method((protocol-type symbol) (seq sequence))
    (assert (= (layer protocol-type) 3))
    (find protocol-type seq :key #'type-of)))

(defgeneric register-protocol(protocol entity)
  (:documentation "Register a layer 3 protocol.")
  (:method((protocol-type symbol) (protocol-number integer))
    (setf (get protocol-type 'protocol::layer) 3
          (get protocol-type 'protocol::protocol-number) protocol-number)
    (unless (find-protocol protocol-number *standard-protocols*)
      (push protocol-type  *standard-protocols*))))

(defgeneric delete-protocol(protocol entity)
  (:documentation "Delete a layer 4 protocol from entity")
  (:method((protocol protocol) entity)
        (setf (protocols entity) (delete protocol (protocols entity))))
  (:method(protocol entity)
    (let ((protocol (find-protocol protocol entity)))
      (when protocol (delete-protocol protocol entity)))))

(defclass pdu(packet:pdu)
  ((layer :initform 3 :reader protocol:layer :allocation :class))
  (:documentation "The base class for all layer three protocol data units"))

(defclass protocol(protocol:protocol)
  ((layer :initform 3 :reader layer :allocation :class))
  (:documentation "Layer 3 protocol"))


(defmethod protocol-number((addr ipaddr)) #x0800)

(in-package :protocol.layer4)

(defclass pdu(packet:pdu)
  ((layer :initform 4 :reader layer :allocation :class))
  (:documentation "The base class for all layer 4 protocol data units"))

(defclass protocol(protocol:protocol)
  ((layer :initform 4 :reader layer :allocation :class))
  (:documentation "Layer 4 protocol base"))

;; as per layer 3 standard layer 4 protocols may be registered so they
;; can be instantiated on nodes on demand.

(defvar *standard-protocols* nil
  "List of standard registered layer 4 protocols")

(defgeneric find-protocol(protocol entity)
  (:documentation "Find a layer 4 protocol on entity")
  (:method((protocol-number integer) (seq sequence))
    (find protocol-number seq :key #'protocol-number :test #'=))
  (:method((protocol-type symbol) (seq sequence))
    (assert (= (layer protocol-type) 4))
    (find protocol-type seq :key #'type-of)))

(defgeneric register-protocol(protocol entity)
  (:documentation "Register a layer 4 protocol")
  (:method((protocol-type symbol) (protocol-number integer))
    (setf (get protocol-type 'protocol::layer) 4
          (get protocol-type 'protocol::protocol-number) protocol-number)
    (unless (find-protocol protocol-number *standard-protocols*)
      (push protocol-type  *standard-protocols*))))

(defgeneric delete-protocol(protocol entity)
  (:documentation "Delete a layer 4 protocol from entity")
  (:method((protocol protocol) entity)
    (if (bindings protocol)
        (error "Unable to remove bound protocol ~A from ~A" protocol entity)
        (setf (protocols entity) (delete protocol (protocols entity)))))
  (:method(protocol entity)
    (let ((protocol (find-protocol protocol entity)))
      (when protocol
        (delete-protocol protocol entity)))))


;; layer 4 protocols are typically implemented using two classes - a
;; demultiplexer which is registered with the protocol number and
;; demultiplexes packets to a specific protocol implementation
;; instance on the basis of a port address and the implementation
;; itself which deals with traffic to and from a specific port. This
;; enables the possibility that nodes may run different
;; implementations of the same protocol. It also means that all state
;; for a specific flow is in one instance.

(defclass protocol-dmux(protocol)
  ((bindings :type list :initform nil :accessor bindings
             :documentation "Sequence of bound connections")
   (min-ephemeral-port :type fixnum :initform 49152))
  (:documentation "Layer 4 protocol demultiplexer to bound protocol entities"))

(defmethod receive((dmux protocol-dmux) packet
                   (layer3protocol layer3:protocol) &rest args
                   &key &allow-other-keys)
  (let ((listener
         (find (dst-port (peek-pdu packet)) (bindings dmux)
               :test #'local-port)))
    (if listener
        (apply #'receive listener packet layer3protocol args)
        (drop dmux packet :text "L4-NP"))))

;; even connectionless protocols don't have a peer address or port
;; it is convenient to use the protocol-implementation to manage this for
;; the application in the simulation using open-connection and close-connection,
;; however connectionless protocols may take different

(defclass protocol-implementation(protocol)
  ((local-address :type network-address :reader local-address
                  :documentation "local address used by this socket")
   (local-port :type ipport :accessor local-port
               :documentation "Local bound service access port")
   (layer5:application :initarg :application :initform nil
                       :reader layer5:application
                       :documentation "Application entity for this flow")
   (peer-address :initform nil :type network-address
                 :accessor peer-address
                 :documentation "network address of peer")
   (peer-port  :type ipport :accessor peer-port
               :documentation "Service access port of peer")
   (fid :type counter :reader fid :documentation "Flow id for packets")
   (last-fid :type counter :initform 0 :documentation "last allocated flow id"
             :allocation :class)
   (ttl :accessor ttl :initarg :ttl :initform 64 :type word
        :documentation "Layer 3 ttl")
   (tos :accessor tos :initarg :tos :initform 0 :type octet
        :documentation "Layer 3 type of service"))
  (:documentation "Base class for IP layer 4 protocol implementations"))

(defun next-available-port(protocol-dmux)
  "Return next available ephemeral port for a protocol demultiplexer"
  (1+
   (reduce #'max (bindings protocol-dmux) :key #'local-port
           :initial-value  (slot-value protocol-dmux 'min-ephemeral-port))))

(defun protocol-dmux(protocol-implementation)
  "Return the protocol demultiplexer associated with a protocol implementation"
  (find-protocol (protocol-number protocol-implementation)
                 (node protocol-implementation)))

(defmethod initialize-instance :after
    ((protocol protocol-implementation) &key
     (local-address (network-address (node protocol)))
     (dmux (protocol-dmux protocol))
     (local-port (next-available-port dmux))
     peer-address peer-port
     &allow-other-keys)
  (when (find local-port (bindings dmux) :key #'local-port)
    (error "~A Port ~D already bound" protocol local-port))
  (setf (slot-value protocol 'local-adress) local-address
        (slot-value protocol 'local-port) local-port)
  (push protocol (bindings dmux))
  (when (and peer-address peer-port)
    (open-connection peer-address peer-port protocol)))

(defgeneric open-connection(peer-address peer-port protocol)
  (:documentation "Connect a protocol to a remote (peer) address")
  (:method(peer-address peer-port (protocol protocol-implementation))
    "Associate a peer address and port with a protocol implementation"
    (assert (not (connected-p protocol)))
    (setf (slot-value protocol 'peer-address) peer-address
          (slot-value protocol 'peer-port) peer-port
          (slot-value protocol 'fid) (incf (slot-value protocol 'last-fid)))))

(defgeneric connection-complete(application protocol &key failure)
  (:documentation "Called to signal completion of connection attempt -
  either success or if failure set failed")
  (:method(application protocol &key failure)
    (declare(ignore application protocol failure))))

(defgeneric connected-p(protocol)
  (:documentation "Return true if protocol is connected")
  (:method((protocol protocol-implementation))
    (and (slot-boundp protocol 'peer-port)
         (slot-boundp protocol 'peer-address))))

(defgeneric close-connection(protocol)
  (:documentation "Close an open connection")
  (:method((protocol protocol-implementation))
    "Disassociate a peer address and port with a protocol implementation"
    (slot-makunbound protocol 'peer-port)
    (slot-makunbound protocol 'peer-address)))

(defgeneric connection-closed(application protocol)
  (:documentation "Called by an associated layer 4 protocol when a connection
has completely closed")
  (:method(app protocol) (declare (ignore app protocol))))

(defgeneric close-request(application protocol)
  (:documentation "Called by an associated layer 4 protocol when a connection
close request has been received from a peer. Applications should
respond by calling the corresponding close-connection routine")
  (:method(app (protocol protocol-implementation))
    (declare (ignore app))
    (close-connection protocol)))

(defgeneric sent(application no-octets-sent protocol)
  (:documentation "Called by an associated layer 4 protocol when all some part
of the outstanding data has been sent.  For TCP protocols,
this occurs when the acknowledgement is received from the peer.")
  (:method(application no-octets-sent protocol)
    (declare (ignore application no-octets-sent protocol))))

(defgeneric connection-from-peer(application protocol &key peer-address peer-port)
  (:documentation "Called when a listening protocol receives a
  connection request. Return true if connection accepted.")
  (:method(application protocol &key &allow-other-keys)
    "Default - acceot and do nothing"
    (declare (ignore application protocol))
    t))



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

;; (defgeneric make-packet(protocol)
;;   (:documentation "Allocate a new packet")
;;   (:method ((protocol protocol))
;;     (make-instance 'packet:packet :notification (notification protocol))))

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
