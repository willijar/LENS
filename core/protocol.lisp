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

(defmethod copy((protocol protocol))
  (copy-with-slots protocol '(name) (call-next-method)))

;; base class for protocol conditions
(define-condition protocol-condition(simulation-condition)
  ((protocol :type protocol :initarg :protocol :reader protocol))
  (:report (lambda(c s)
               (format s "~A: ~A" (class-name (class-of c)) (protocol c)))))

(defgeneric protocol-number(entity)
  (:documentation "Return the IANA protocol number.
See http://www.iana.org/assignments/protocol-numbers")
  (:method((protocol standard-object))
    (get (type-of protocol) 'protocol-number))
  (:method((protocol symbol)) (get protocol 'protocol-number)))

(defmethod layer((protocol protocol)) (get (type-of protocol) 'layer))
(defmethod layer((protocol symbol)) (get protocol 'layer))

(defun write-trace(protocol pdu &key (node (node protocol)) packet text
                   (stream trace:*lens-trace-output*))
  (dolist(stream (if (listp stream) stream (list stream)))
    (when (trace-enabled-p protocol stream)
      (setf (slot-value stream 'node) node)
      (pdu-trace pdu
                 (if protocol (trace-detail protocol stream) nil)
                 stream
                 :packet packet
                 :text text))))

;; split-phase transmission (going down protocol layers)
(defgeneric send(receiver packet sender &key &allow-other-keys)
  (:documentation "Called by sender to start sending packet - Returns
  true if packet accepted for transmission, false otherwise.")
  (:method :before((receiver protocol) packet (sender protocol)
                   &key &allow-other-keys)
    (write-trace sender (peek-pdu packet) :packet packet :text "-"))
  (:method :around (receiver packet (sender protocol) &key &allow-other-keys)
    (when (node:call-callbacks :tx sender packet) (call-next-method))))

(defgeneric receive(receiver packet sender &key &allow-other-keys)
  (:documentation "Called by packet when to pass received packet up to
  receiver (once reception is complete)")
  (:method :before((receiver protocol) packet (sender protocol)
                   &key &allow-other-keys)
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
                       :reader layer5:application
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

(defmethod copy((protocol protocol))
  (copy-with-slots protocol
                   '(layer5:application local-address local-port
                     peer-address peer-port fid ttl tos)
                   (call-next-method)))

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
            (modulus+ 1 port 16)))
     ((or (zerop port) (not (binding protocol-dmux port))
          (the (unsigned-byte 16) port)))))

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
                          peer-address local-address node)
  (:documentation "Bind a protocol - returning it if successful or nil if not")
  (:method((protocol protocol) &key
           (local-port (local-port protocol))
           (local-address (local-address protocol))
           (peer-port (peer-port protocol))
           (peer-address (peer-address protocol))
           (node (node protocol)))
    (let ((dmux (find-protocol (protocol-number protocol) node)))
      (unless local-port (setf local-port (next-available-port dmux)))
      (assert (or (not (or peer-port peer-address))
                  (and peer-port peer-address)))
       (unless (binding dmux local-port :local-address local-address
                        :peer-port peer-port :peer-address peer-address)
         (setf (slot-value protocol 'local-port) local-port
               (slot-value protocol 'peer-port) peer-port
               (slot-value protocol 'local-address) local-address
               (slot-value protocol 'peer-address) peer-address
               (slot-value protocol 'node) node)
         (push protocol (bindings dmux))))))

(defgeneric unbind(protocol)
  (:documentation "Remove protocol from dmux bindings")
  (:method((protocol protocol))
    (let ((dmux (find-protocol (protocol-number protocol) (node protocol))))
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

(defgeneric connection-complete(application protocol &key failure)
  (:documentation "Called to signal completion of connection attempt -
  either success or if failure set failed")
  (:method(application protocol &key failure)
    (declare(ignore application protocol failure))))

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
  (:documentation "Called by an associated layer 4 protocol when a connection
has completely closed")
  (:method(app protocol) (declare (ignore app protocol)))
  (:method :after (app (protocol protocol))
      (declare (ignore app))
      (unbind protocol)))

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

(defgeneric connection-from-peer(application protocol &key peer-address peer-port)
  (:documentation "Called when a listening protocol receives a
  connection request. Return true if connection accepted.")
  (:method(application protocol &key &allow-other-keys)
    "Default - acceot and do nothing"
    (declare (ignore application protocol))
    t))

(defgeneric connection-error(application protocol error)
  (:documentation "Called by protocol to signal an error to the application")
  (:method(application protocol error)
    (declare (ignore application))
    (format *trace-output* "~A connection Error: ~A" protocol error)))

(in-package :protocol.layer5)

(defclass data(packet:pdu)
  ((layer :initform 5 :allocation :class :reader layer)
   (length-bytes :type integer :initarg :length-bytes
         :documentation "Number of data bytes if no contents")
   (contents :type (or null (vector (unsigned-byte 8) *)) :initform nil :reader contents
             :initarg :contents
             :documentation "Vector of data if we are sending")
   #+nil(response-size :type integer :initarg :response-size :initform 0
                  :reader response-size
                  :documentation "Size of response requested")
   #+nil(checksum :type (unsigned-byte 16) :initarg :checksum
                  :documentation "Checksum value"))
  (:documentation "Data PDU"))

(defmethod send((layer4 layer4:protocol) (length-bytes integer) application &rest args)
  (apply #'send layer4
         (make-instance 'data :length-bytes length-bytes)
         application args))

(defmethod print-object((data data) stream)
  (print-unreadable-object (data stream :type t :identity t)
    (format stream "~:/print-eng/bytes" (length-bytes data))))

(defmethod length-bytes((pdu data))
  (if (contents pdu) (length (contents pdu)) (slot-value pdu 'length-bytes)))

(defmethod copy((data data))
  (let ((copy (copy-with-slots data '(-size response-size checksum))))
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
       :length-bytes (unless (contents data) size)))))

       ;;:response-size (response-size data)

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
        ((vector (unsigned-byte 8) *) (values (length s) s))
        (data (values (length-bytes s) (contents s))))
    (with-slots(length-bytes contents) data
      (if contents
          (setf contents
                (concatenate 'vector contents
                             (or d (make-array s
                                               :element-type '(unsigned-byte 8)
                                               :initial-element 0))))
          (when d (setf contents (copy-seq d))))
      (setf length-bytes (+ length-bytes s)))))
