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

(defgeneric drop(entity packet &key node &allow-other-keys)
  (:documentation "Drop a packet")
  (:method :after (entity packet &key text &allow-other-keys)
    (write-trace entity :drop :packet packet :text text)))

(in-package :protocol.layer2)

(defclass protocol(protocol:protocol)
  ((layer :initform 2 :reader layer :allocation :class)
   (interface :initarg :iface :initarg :interface :accessor interface
              :documentation "Interface for this protocol"))
  (:documentation "Layer 2 protocol base class"))

(defmethod initialize-instance :after((protocol protocol)
                                      &key &allow-other-keys)
  (setf (slot-value protocol 'node) (node (slot-value protocol 'interface))))

(defclass pdu(packet:pdu)
  ((layer :initform 2 :reader protocol:layer :allocation :class))
  (:documentation "The base class for all layer two  protocol data units"))

(defmethod protocol-number((addr macaddr)) 1)

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
        (delete-protocol protocol (slot-value node 'layer3:protocols))))

(defmethod find-protocol((protocol-number integer) node)
  "Find an already existent protocol or add one"
  (or (find-protocol protocol-number (slot-value node 'layer3:protocols))
      (let ((protocol (find-protocol protocol-number *standard-protocols*)))
        (when protocol (make-instance protocol :node node)))))

(defmethod register-protocol((protocol protocol) node &optional (replace nil))
  (setf (slot-value node 'layer3:protocols)
        (register-protocol protocol (slot-value node 'layer3:protocols) replace)))

(defmethod initialize-instance :after ((protocol protocol)
                                       &key &allow-other-keys)
  (register-protocol protocol (node protocol)))

(defgeneric find-interface(protocol addr)
  (:documentation "Find the forwarding interface"))

(defmethod protocol-number((addr ipaddr)) #x0800)

(in-package :protocol.layer4)

;; layer 4 will receive packets from application and send to network layer
(defvar *standard-protocols* nil "List of standard registered layer 4
protocols")

(defvar +min-ephemeral-port+ 49152 "Start of ephemeral port range")

(defclass protocol(protocol:protocol)
  ((layer :initform 4 :reader layer :allocation :class)
   (node :initarg :node :reader node :documentation "Local Node")
   (bindings :type list :initform nil :reader bindings
             :documentation "Sequence of bound connections"))
  (:documentation "Layer 4 protocol"))

(defun next-available-port(protocol)
  (1+
   (reduce #'max (bindings protocol) :key #'local-port
           :initial-value  +min-ephemeral-port+)))

(defclass pdu(packet:pdu)
  ((layer :initform 4 :reader protocol:layer :allocation :class))
  (:documentation "The base class for all layer four protocol data units"))

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
  (setf (slot-value node 'layer4:protocols)
        (delete-protocol protocol (slot-value node 'layer4:protocols))))

(defmethod find-protocol((protocol-number integer) node)
  (or (find-protocol protocol-number (slot-value node 'protocols))
      (let ((protocol (find-protocol protocol-number *standard-protocols*)))
        (when protocol (make-instance protocol :node node)))))

(defmethod register-protocol((protocol protocol) node &optional (replace nil))
  (setf (slot-value node 'layer4:protocols)
        (register-protocol protocol (slot-value node 'layer4:protocols) replace)))

(defmethod initialize-instance :after ((protocol protocol)
                                       &key &allow-other-keys)
  (register-protocol protocol (node protocol)))

(defclass socket()
  ((local-address :type network-address :reader local-address
                  :documentation "local address used by this socket")
   (local-port :type ipport :accessor local-port
               :documentation "Local bound service access port")
   (layer5:application :initarg :application :initform nil
                       :reader layer5:application
                       :documentation "Local application object")
   (protocol :type protocol :initarg :protocol :reader protocol
             :documentation "layer 4 protocol used by this flow")
   (peer-address :initform nil :type network-address
                 :reader peer-address
                 :documentation "network address of peer")
   (peer-port  :type ipport :accessor peer-port
               :documentation "Service access port of peer")
   (fid :type counter :reader fid :documentation "Flow id")
   (last-fid :type counter :initform 0 :documentation "last allocated flow id")
   (ttl :accessor ttl :initarg :ttl :initform 64 :type word
        :documentation "Layer 3 ttl")
   (tos :accessor tos :initarg :tos :initform 0 :type octet
        :documentation "Layer 3 type of service"))
  (:documentation "Base class for all layer 4 protocols states"))

(defmethod initialize-instance :after
    ((socket socket) &key
     (local-address (network-address (node (protocol socket))))
     (local-port (next-available-port (protocol socket)))
     peer-address peer-port
     &allow-other-keys)
  (when local-address (setf (slot-value socket 'local-adress) local-address))
  (when local-port (bind local-port socket :local-address local-address))
  (when (and peer-address peer-port) (connect peer-address peer-port socket)))

(defun listener-p(socket)
  (not (slot-boundp socket 'peer-address)))

(defgeneric bind(local-port socket &key local-address )
  (:documentation "Bind a socket to a local address and port")
  (:method(local-port (socket socket) &key local-address)
    (when local-address
      (setf (slot-value socket 'local-address) local-address))
    (unless (slot-boundp socket 'local-address)
      (error "No local address defined to bind to"))
    (when (slot-boundp socket 'local-port)
      (lens-error "Socket already bound"))
    (setf (slot-value socket 'local-port) local-port)
    (if (listener-p socket)
        (when (find local-port (bindings (protocol socket)) :key #'local-port)
          (error "Port ~D already bound" local-port))
        (with-slots((lpa local-port) (paa peer-address) (ppa peer-port)) socket
          (when (find-if
                 #'(lambda(socket)
                     (with-slots((lpb local-port) (pab peer-address)
                                 (ppb peer-port)) socket
                     (and (eql lpa lpb) (eql paa pab) (eql ppa ppb))))
                 (bindings (protocol socket)))
            (error "Port ~D Socket alread bound to ~A:~A" lpa paa ppa))))
    (push socket (slot-value (protocol socket) 'bindings))))

(defgeneric unbind(socket)
  (:documentation "Unbind a bound socket")
  (:method((socket socket))
    (with-slots(bindings) (protocol socket)
      (setf bindings (delete socket bindings)))
    (slot-makunbound socket 'local-port)))

(defgeneric connect(peer-address peer-port socket)
  (:documentation "Connect a socket to a remote (peer) address")
  (:method(peer-address peer-port (socket socket))
    (setf (slot-value socket 'peer-address) peer-address
          (slot-value socket 'peer-port) peer-port
          (slot-value socket 'fid) (incf (slot-value socket 'last-fid)))))

(defgeneric connection-complete(socket application &key failure)
  (:documentation "Called to signal completion of connection attempt -
  either success or if failure set failed"))

(defgeneric connected-p(socket)
  (:documentation "Return true if socket is connected")
  (:method((socket socket)) (slot-boundp socket 'peer-port)))

(defgeneric close-connection(socket)
  (:documentation "Close an open connection")
  (:method((socket socket)) (slot-makunbound socket 'peer-port)))

(defgeneric closed(application protocol)
  (:documentation "Called by an associated layer 4 protocol when a connection
has completely closed")
  (:method(app protocol) (declare (ignore app protocol))))

(defgeneric close-request(application protocol)
  (:documentation "Called by an associated layer 4 protocol when a connection
close request has been received from a peer.  Applications should
respond by calling the corresponding close-connection routine")
  (:method(app protocol)
    (declare (ignore app))
    (close-connection protocol)))

(defmethod send(application data (socket socket)
                &key (peer-address (peer-address socket))
                (peer-port (peer-port socket)) &allow-other-keys)
  (send socket
        (make-instance 'packet :data data :fid (fid socket))
        (protocol socket) :peer-address peer-address :peer-port peer-port))

(defmethod receive((socket socket) packet (protocol protocol)
                   &key &allow-other-keys)
  (receive (layer5:application socket) (pop-pdu packet) socket))

(defgeneric sent(application no-octets-sent protocol)
  (:documentation "Called by an associated layer 4 protocol when all some part
of the outstanding data has been sent.  For TCP protocols,
this occurs when the acknowledgement is received from the peer.")
  (:method(application no-octets-sent protocol)
    (declare (ignore application no-octets-sent protocol))))

(defgeneric connection-from-peer(application protocol &key peer-address peer-port)
  (:documentation "Called when a listening socket receives a
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
