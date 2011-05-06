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
  ((node :initarg :node :accessor node))
  (:documentation "Base class for all protocol entities"))

(defgeneric protocol-number(entity)
  (:documentation "Return the IANA protocol number.
See http://www.iana.org/assignments/protocol-numbers")
  (:method((protocol standard-object))
    (get (type-of protocol) 'protocol-number))
  (:method((protocol symbol)) (get protocol 'protocol-number)))

(defgeneric layer(protocol)
  (:documentation "Return the protocol layer for an entity")
  (:method((protocol protocol)) (get (type-of protocol) 'layer))
  (:method((protocol symbol)) (get protocol 'layer)))

(defun write-trace(protocol pdu &key (node (node protocol)) packet text
                   (stream trace:*lens-trace-output*))
  (dolist(stream (if (listp stream) stream (list stream)))
    (when (trace-enabled-p protocol stream)
      (setf (node stream) node)
      (setf (packet:packet stream) packet)
      (pdu-trace pdu
                 (if protocol (trace-detail protocol stream) nil)
                 stream
                 :packet packet
                 :text text))))

;; split-phase transmission (going down protocol layers)
(defgeneric send(receiver packet sender &key &allow-other-keys)
  (:documentation "Called by sender to start sending packet - Returns
  true if packet accepted for transmission, false otherwise. At
  transport layer return the number of bytes accepted for
  transmission")
  (:method :before((receiver protocol) (packet packet) (sender protocol)
                   &key &allow-other-keys)
      (write-trace sender (peek-pdu packet) :packet packet :text "-"))
  (:method :before((receiver protocol) (pdu pdu) (sender protocol)
                   &key &allow-other-keys)
      (write-trace sender pdu :text "-"))
  (:method :around (receiver packet (sender protocol) &key &allow-other-keys)
     (when (node:call-callbacks :tx sender packet) (call-next-method))))

(defgeneric receive(receiver packet sender &key &allow-other-keys)
  (:documentation "Called by packet when to pass received packet up to
  receiver (once reception is complete)")
  (:method :before((receiver protocol) (packet packet) (sender protocol)
                   &key &allow-other-keys)
     (write-trace receiver (peek-pdu packet) :packet packet :text "+"))
  (:method :before((receiver protocol) (pdu pdu) (sender protocol)
                   &key &allow-other-keys)
     (write-trace receiver pdu :text "+"))
  (:method :around((receiver protocol) packet sender &key &allow-other-keys)
    (when (node:call-callbacks :rx receiver packet) (call-next-method))))

(defgeneric drop(entity packet &key text &allow-other-keys)
  (:documentation "Drop a packet for entity")
  (:method :after (entity packet &key text &allow-other-keys)
    "Trace drops"
    (write-trace entity :drop :packet packet :text text))
  (:method(entity packet &key &allow-other-keys)
    (declare (ignore entity packet))))

(defgeneric control-message(receiver message sender &key &allow-other-keys)
  (:documentation "Called by sender protocol entity to signal a
  control message to the receiver protocol entity")
  (:method (receiver message sender &rest args &key &allow-other-keys)
    (format lens-user:*user-output* "~A received ~A signal from ~A ~@[~A~]"
            receiver message sender args)))

(in-package :protocol.layer2)

(defclass protocol(protocol:protocol)
  ((layer :initform 2 :reader layer :allocation :class)
   (interface :initarg :iface :initarg :interface :accessor layer1:interface
              :documentation "Interface for this protocol"))
  (:documentation "Layer 2 protocol base class"))

(defmethod node((protocol protocol)) (node (layer1:interface protocol)))

(defclass pdu(packet:pdu)
  ()
  (:documentation "The base class for all layer two  protocol data units"))

(defmethod layer((pdu pdu)) 2)

(defmethod protocol-number((addr macaddr)) 1)

(in-package :protocol.layer3)

(defclass pdu(packet:pdu)
  ()
  (:documentation "The base class for all layer three protocol data units"))

(defmethod layer((pdu pdu)) 3)

(defclass protocol(protocol:protocol)
  ((layer :initform 3 :reader layer :allocation :class))
  (:documentation "Layer 3 protocol"))

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
    ;; only first one registered as a standard protocol
    (unless (find-protocol protocol-number *standard-protocols*)
      (push protocol-type  *standard-protocols*))))

(defgeneric delete-protocol(protocol entity)
  (:documentation "Delete a layer 4 protocol from entity")
  (:method((protocol protocol) entity)
        (setf (protocols entity) (delete protocol (protocols entity))))
  (:method(protocol entity)
    (let ((protocol (find-protocol protocol entity)))
      (when protocol (delete-protocol protocol entity)))))

(defmethod protocol-number((addr ipaddr)) #x0800)

(in-package :protocol.layer4)

(defclass pdu(packet:pdu)
  ()
  (:documentation "The base class for all layer 4 protocol data units"))

(defmethod layer((pdu pdu)) 4)

(defgeneric src-port(pdu)
  (:documentation "Return the source port address"))

(defgeneric dst-port(pdu)
  (:documentation "Return the destination port address"))

;; even connectionless protocols don't have a peer address or port
;; it is convenient to use the protocol implementation to manage this for
;; the application in the simulation using open-connection and close-connection,
;; however connectionless protocols may take different

(defclass protocol(protocol:protocol)
  ((layer :initform 4 :reader layer :allocation :class)
   (layer5:application :initarg :application :initform nil
                       :accessor layer5:application
                       :documentation "Application entity for this flow")
   (local-address :initform nil :initarg :local-address
                  :type (or network-address null) :reader local-address
                  :documentation "local address used by this socket")
   (local-port :initform nil :initarg :local-port
               :type (or ipport null) :accessor local-port
               :documentation "Local bound service access port")
   (peer-address :initarg :peer-address :initform nil
                 :type (or network-address null)
                 :accessor peer-address
                 :documentation "network address of peer")
   (peer-port  :initarg :peer-port :type (or ipport null) :accessor peer-port
               :documentation "Service access port of peer")
   (fid :type integer :reader fid :documentation "Flow id for packets")
   (last-fid :type integer :initform 0 :documentation "last allocated flow id"
             :allocation :class)
   (ttl :accessor ttl :initarg :ttl :initform 64 :type integer
        :documentation "Layer 3 ttl")
   (tos :accessor tos :initarg :tos :initform 0
        :documentation "Layer 3 type of service"))
  (:documentation "Base class for IP layer 4 protocol implementations"))

;; as per layer 3 standard layer 4 protocols may be registered so they
;; can be instantiated on nodes on demand.

(defvar *standard-protocols* nil
  "List of standard registered layer 4 protocols")

(defmethod reset :before ((protocol protocol))
  (delete-notifications protocol (node protocol)))

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
    ;; only first one registered as a standard protocol
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

(defclass protocol-dmux()
  ((node :initarg :node :reader node)
   (layer :initform 4 :reader layer :allocation :class)
   (bindings :type list :initform nil :accessor bindings
             :documentation "Sequence of bound connections")
   (min-ephemeral-port :type (unsigned-byte 16) :initform 49152
                       :initarg :min-ephemeral-port))
  (:documentation "Layer 4 protocol demultiplexer to bound protocol entities"))

(defun protocol-dmux(protocol)
  "Return the protocol demultiplexer associated with a protocol implementation"
  (find-protocol (protocol-number protocol) (node protocol)))

(defun next-available-port(protocol-dmux)
  "Return next available ephemeral port for a protocol demultiplexer"
  (do((port (slot-value protocol-dmux 'min-ephemeral-port)
            (1+ port)))
     ((not (binding protocol-dmux port))
      (when (<= port #xFFFF) port))))

(defgeneric binding(entity local-port &key &allow-other-keys)
  (:documentation "Find best matching binding for given address details")
  (:method(node local-port
           &key local-address peer-port peer-address protocol-number)
    (binding (find-protocol protocol-number node)
             local-port
             :local-address local-address
             :peer-port peer-port
             :peer-address peer-address))
  (:method((dmux protocol-dmux) local-port
           &key local-address peer-port peer-address &allow-other-keys)
     (let ((binding nil)) ;; most specific found so far
      (map 'nil
           #'(lambda(p)
               (when (eql local-port (local-port p))
                 (if (local-address p)
                     (when (address= (local-address p) local-address)
                       (if (peer-address p)
                           (when (and
                                  (eql peer-port (peer-port p))
                                  (address= peer-address (peer-address p)))
                             (return-from binding p))
                           (setf binding p)))
                     (unless binding (setf binding p)))))
           (bindings dmux))
      binding)))

(defgeneric bind(protocol &key local-port peer-port
                          peer-address local-address node application)
  (:documentation "Bind a protocol - returning it if successful or nil if not")
  (:method((protocol protocol) &key
           (local-port (local-port protocol))
           (local-address (local-address protocol))
           (peer-port (peer-port protocol))
           (peer-address (peer-address protocol))
           (application (application protocol))
           (node (or (node protocol) (node application))))
    (let ((dmux (find-protocol (protocol-number protocol) node)))
      (unless local-port (setf local-port (next-available-port dmux)))
      (assert (or (not (or peer-port peer-address))
                  (and peer-port peer-address)))
      (assert (not (bound-p protocol)))
      (unless (binding dmux local-port :local-address local-address
                        :peer-port peer-port :peer-address peer-address)
         (setf (slot-value protocol 'local-port) local-port
               (slot-value protocol 'peer-port) peer-port
               (slot-value protocol 'local-address) local-address
               (slot-value protocol 'peer-address) peer-address
               (slot-value protocol 'node) node
               (slot-value protocol 'application) application)
         (push protocol (bindings dmux))))))

(defgeneric unbind(protocol)
  (:documentation "Remove protocol from dmux bindings")
  (:method((protocol protocol))
    (let ((dmux (protocol-dmux protocol)))
      (setf (bindings dmux) (delete protocol (bindings dmux))))))

(defgeneric bound-p(protocol)
  (:documentation "Return true if protocol is currently bound")
  (:method((protocol protocol))
    (and (node protocol)
         (find protocol
               (bindings
                (find-protocol (protocol-number protocol) (node protocol)))))))

;; dmux - around methods etc not
(defmethod receive ((dmux protocol-dmux) packet
                    (layer3protocol layer3:protocol) &rest args
                    &key src-address dst-address &allow-other-keys)
  (let* ((h (peek-pdu packet))
         (agent (binding dmux (dst-port h)
                         :local-address dst-address
                         :peer-address src-address
                         :peer-port (src-port h))))
    (if agent
        (apply #'receive agent packet layer3protocol args)
        (drop dmux packet :text "L4-NP"))))

(defmethod reset((dmux protocol-dmux))
  (map 'nil #'reset (bindings dmux)))

(defgeneric open-connection(peer-address peer-port protocol)
  (:documentation "Connect a protocol to a remote (peer) address")
  (:method(peer-address peer-port (protocol protocol))
    "Associate a peer address and port with a protocol implementation"
    (assert (not (connected-p protocol)))
    (setf (slot-value protocol 'peer-address) peer-address
          (slot-value protocol 'peer-port) peer-port
          (slot-value protocol 'fid) (incf (slot-value protocol 'last-fid)))))

(defgeneric connection-complete(application protocol)
  (:documentation "Called to signal successfull completion of
connection attempt")
  (:method(application protocol)
    (declare(ignore application protocol))))

(defgeneric connected-p(protocol)
  (:documentation "Return true if protocol is connected")
  (:method((protocol protocol))
    (slot-boundp protocol 'peer-port)))

(defgeneric close-connection(protocol)
  (:documentation "Close an open connection")
  (:method((protocol protocol))
    "Disassociate a peer address and port with a protocol implementation"
    (slot-makunbound protocol 'peer-port)
    (slot-makunbound protocol 'peer-address)))

(defgeneric connection-closed(application protocol)
  (:documentation "Called by an associated layer 4 protocol when a
connection has completely closed")
  (:method(app protocol) (declare (ignore app protocol)))
  (:method :after (app (protocol protocol))
      (declare (ignore app))
      (unbind protocol)
      (delete-notifications protocol (node protocol))))

(defgeneric close-request(application protocol)
  (:documentation "Called by an associated layer 4 protocol when a connection
close request has been received from a peer. Applications should
respond by calling the corresponding close-connection routine")
  (:method(app (protocol protocol))
    (declare (ignore app))
    (close-connection protocol)))

(defgeneric sent(application no-octets-sent protocol)
  (:documentation "Called by an associated layer 4 protocol when all some part
of the outstanding data has been sent.  For TCP protocols,
this occurs when the acknowledgement is received from the peer.")
  (:method(application no-octets-sent protocol)
    (declare (ignore application no-octets-sent protocol))))

(defgeneric connection-from-peer(application protocol)
  (:documentation "Called when a listening protocol receives a
  connection request. Return true if connection accepted. Protocol will be the connected socket (not the listener)")
  (:method(application protocol)
    "Default - accept and do nothing"
    (declare (ignore application protocol))
    t))

(declaim (inline seq+ seq- seq<))

(defun seq+(seq length-bytes)
  "32 bit modulus addition of a sequence number and a byte length"
  (declare ((unsigned-byte 32) seq length-bytes))
  (the (unsigned-byte 32) (mod (+ seq length-bytes) #x100000000)))

(defun seq-(seq-end seq-start)
  "Return the number of bytes between seq-start and seq-end"
  (declare ((unsigned-byte 32) seq-end seq-start))
  (the (unsigned-byte 32)
    (if (>= seq-end seq-start)
        (- seq-end seq-start)
        (+ seq-end (- #x100000000 seq-end)))))

(defun seq<(a b)
  (declare ((unsigned-byte 32) a b))
  (and (< a b)
       (< (- b a)  #x10000000)))

(defun seq-in-segment(sequence-number segment-start segment-no-bytes)
  "Return true if the sequence number corresponds to a byte in segment
with the given sequence start number and byte count."
  (declare ((unsigned-byte 32) sequence-number segment-start segment-no-bytes)
           (optimize speed (safety 0)))
  (let ((segment-end (seq+ segment-start segment-no-bytes)))
    (declare ((unsigned-byte 32) segment-end))
    ;; deal with wrap around edge case
    (if (>= segment-end segment-start)
         (and (>= sequence-number segment-start)
              (< sequence-number segment-end))
         (or (< sequence-number segment-end)
             (>= sequence-number segment-start)))))

(defun ack-after-segment(ack-number segment-start segment-no-bytes)
  "Return true if an acknowledgement number is after a sequence with a
given segment-start and no-bytes"
  (declare ((unsigned-byte 32) ack-number segment-start segment-no-bytes)
           (optimize speed (safety 0)))
  (let ((segment-end (seq+ segment-start segment-no-bytes))
        (wrap-around (seq+ segment-start #x7FFFFFFF)))
    (declare ((unsigned-byte 32) segment-end wrap-around))
    ;; deal with edge cases from modular arithmetic wrap around
    (if (> wrap-around segment-end)
        (and (>= ack-number segment-end)
             (< ack-number wrap-around))
        (or (>= ack-number segment-end)
            (< ack-number wrap-around)))))

(in-package :protocol.layer5)

(defclass pdu(packet:pdu)
  ()
  (:documentation "Application layer pdu"))

(defmethod layer((pdu pdu)) 5)

(defclass data(pdu)
  ((length-bytes :type integer :initarg :length-bytes :reader length-bytes
                 :documentation "Number of data bytes represented by this pdu"))
  (:documentation "Data representational PDU"))

(defmethod packet:trace-format((data data)) '(length-bytes))

;; can send vector of bytes as data too
(defmethod layer((pdu vector))
  (check-type pdu (vector (unsigned-byte 8) *))
  5)

(defmethod length-bytes((pdu vector))
  (check-type pdu (vector (unsigned-byte 8) *))
  (fill-pointer pdu))

(defmethod length-bytes((pdu null)) 0)

(defmethod send((layer4 layer4:protocol) (length-bytes integer) application &rest args)
  (apply #'send layer4
         (make-instance 'data :length-bytes length-bytes)
         application args))

(defmethod print-object((data data) stream)
  (print-unreadable-object (data stream :type t :identity t)
    (format stream "~:/print-eng/bytes" (length-bytes data))))

(defun checksum(data)
  (check-type data (vector (unsigned-byte 8) *))
  (multiple-value-bind(s2 rem) (floor (length data) 2)
    (let ((sum 0))
      (declare (type (unsigned-byte 16) sum))
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

(defgeneric data-concatenate(a b)
  (:documentation "Return a new data pdu made of the concatenation of a and b")
  (:method((a data) (b data))
    (make-instance (class-of a)
                   :length-bytes (+ (length-bytes a) (length-bytes b))))
  (:method((a vector) (b vector))
     (check-type a (vector (unsigned-byte 8) *))
     (check-type b (vector (unsigned-byte 8) *))
     (concatenate '(vector (unsigned-byte 8) *) a b)))

(defgeneric data-subseq(data start &optional length)
  (:documentation "Return a copy of a subset of data starting with
  element number start continuing to end of sequence or up to length
  bytes")
  (:method((data data) start &optional length-bytes)
    (let ((end (length-bytes data)))
      (assert (<= start end))
      (make-instance (class-of data) :length-bytes
                     (if (or (not length-bytes) (> (+ start length-bytes) end))
                         (- end start)
                         length-bytes))))
  (:method ((data vector) (start integer)  &optional length-bytes)
   (check-type data (vector (unsigned-byte 8) *))
   (let* ((end (length-bytes data))
          (length-bytes (if (or (not length-bytes)
                                (> (+ start length-bytes) end))
                            (- end start)
                            length-bytes)))
     (subseq data start (+ start length-bytes)))))
