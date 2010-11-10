;;;; LENS packet header API
;;;; Copyright (C) 2003-2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;; $Id: packet.lisp,v 1.2 2005/11/29 09:00:39 willijar Exp $

(in-package :node)

(defstruct location
  (x 0.0 :type short-float :read-only t)
  (y 0.0 :type short-float :read-only t)
  (z 0.0 :type short-float :read-only t))

(defconstant +origin+ (if (boundp '+origin+) +origin+ (make-location)))

(defgeneric distance(a b)
  (:documentation "Return distance between a and b")
  (:method((a location) (b location))
    (let ((dx (- (location-x a) (location-x b)))
          (dy (- (location-y a) (location-y b)))
          (dz (- (location-z a) (location-z b))))
      (sqrt (+ (* dx dx) (* dy dy) (* dz dz)))))
  (:method(a b)
    (distance (location a) (location b))))

(defclass node()
  ((uid :type fixnum :reader uid
        :documentation "Unique id of this node - also index in nodes array")
   (location :initform +origin+ :type location :accessor location
             :documentation "Physical Location of this node")
   (network-address :type network-address :initform (ipaddr :next)
                    :initarg :ipaddr :reader ipaddr :reader network-address
                    :documentation "network address of this node")
   (interfaces :type (vector interface *)
               :initform (make-array 2 :adjustable t :fill-pointer 0)
               :reader interfaces
               :documentation "Vector of interfaces for this node")
   (layer3:protocols :type list :initform nil
                     :documentation "List of layer 3 protocol entities")
   (layer4:protocols :type list :initform nil
                     :documentation "List of layer 4 protocol entities")
   (up-p :type boolean :initform t :reader up-p
          :documentation "True if node up, false if failed")
   (routing :initarg :routing :accessor routing
            :documentation "Routing object for this node")
   (callbacks :type list :initform nil :accessor callbacks
              :documentation "List of callbacks on this node"))
  (:documentation "Class PDU serves as the base class for all the
protocol data units. Protocol headers and data chunks that
form the packets are derived from this class."))

(defmethod print-object((node node) stream)
  (print-unreadable-object (node stream :type t :identity t)
    (format stream "~D~@[ ~A~]" (uid node) (network-address node))))

(defvar *nodes*
  (make-array 1024 :initial-element nil :adjustable t :fill-pointer 0)
  "Global vector of all nodes")

(defun nodes() "Return the vector of all nodes" *nodes*)
(defun clear-nodes()
  (setf *nodes*
        (make-array 64 :initial-element nil :adjustable t :fill-pointer 0)))

(eval-when(:load-toplevel :execute)
  (pushnew *nodes* *reset-hooks*))

(defmethod initialize-instance :after ((node node) &key &allow-other-keys)
  (setf (slot-value node 'uid) (vector-push-extend node (nodes)))
  (setf (slot-value node 'routing)
        (apply #'make-instance (append routing:*default-routing*
                                       `(:node ,node)))))

(defmethod up((node node) &key (inform-routing t))
  (unless (slot-value node 'up-p)
    (setf (slot-value node 'up-p) t)
    (when inform-routing (routing:topology-changed node))))

(defmethod down((node node) &key (inform-routing t))
  (when (slot-value node 'up-p)
    (setf (slot-value node 'up-p) nil)
    (when inform-routing (routing:topology-changed node))))

(defgeneric node(entity)
  (:documentation "Return the node associated with an entity")
  (:method((entity integer)) (aref (nodes) entity))
  (:method ((ipaddr ipaddr))
    (find-if #'(lambda(node) (node:local-ipaddr-p ipaddr node)) (nodes))))

(defgeneric (setf node)(node entity)
  (:documentation "Set the node associated with an entity"))

(defgeneric add-interface(interface node)
  (:documentation "Add an interface to a node"))

(defgeneric find-interface(test node)
  (:documentation "Return the interface matching test"))

(defgeneric local-ipaddr-p(ipaddr node)
  (:documentation "Return true if ipaddr is local to node"))

(defgeneric receive-packet(packet interface node)
  (:documentation "Called when a packet is received on a swtich node"))

(defgeneric ipaddrs(node)
  (:documentation
   "Return a lists of ipaddr and ipmasks for all interfaces on this node"))

(defgeneric make-callback(function direction
                                   &key layer protocol-number interface)
  (:documentation "Make a callback functions. Callbacks allow
a function to examine every packet recieved by this node
at any protocol layer, and either packet receipt or
packet transmission

direction - either :tx or :rx for transmit or receive
layer     - callback only added to this layer (or all by default)
proto     - callback only added for this protocol number
interface - layer 2 callback may be interface specific

Callback functions must accept 5 arguments
the layer, direction, packet, node and (optionally - layer 2 only) interface
Should return false if deleted the packet
"))

(defgeneric call-callbacks(layer proto direction packet node
                                 &optional interface)
  (:documentation "Call the list of callbacks. Arguments as per make-callback.
First to return false causes packet to be dropped"))

(defclass packet-callback()
    ((layer :type integer :reader layer :initarg layer :initform nil
            :documentation "layer for this callback")
     (protocol-number :type integer :reader protocol-number
                      :initarg :protocol-number :initform nil
            :documentation "Protocol number for callback")
     (direction :type (member :tx :rx) :reader direction :initarg :direction
                :documentation "Callback direction")
     (interface :type interface :reader interface :initarg interface
                :documentation "Interface restriction")
     (callback :type function :reader callback :initarg :callback
               :documentation "Callback Function itself"))
  (:documentation "Defines the callback function.  Must have six
parameters that are passed to the callback by the GTNetS protocol
layer processors:

Layer is the protocol layer that is calling the callback.

Proto is the protocol number that is calling the callback.

Type is :TX (packet transmission) or :RX (Packet receipt).

Packet is the packet itself. The top of the PDU stack in the
packet will have the PDU from the calling layer.  In other words,
if the callback is from layer 2, the layer 2 pdu will be the
top of the stack.

Node is the node that is processing the packet when the callback

Interface is the interface that received or is transmitting the
packet (valid only for layer 2 callbacks).

IMPORTANT: The callback function must return true if the packet should
continue to be processed by the protocol stack, and false if the
callback function has deleted the packet.  For example, if the
callback is implementing a firewall function, the firewall may decide
that the packet should not be forwarded for further processing. In
that case, it should drop the packet and return false."))

(defmethod make-callback(function direction &key
                         layer protocol-number interface)
  (make-instance 'packet-callback
                 :layer layer
                 :protocol-number protocol-number
                 :direction direction
                 :interface interface
                 :callback function))

(defmethod call-callbacks(layer proto direction packet (node node)
                          &optional interface)
  (dolist(c (callbacks node))
    (when (and ;; check for match
           (eql direction (direction c))
           (or (not (layer c)) (= (layer c) layer))
           (or (not (interface c)) (eql (interface c) interface))
           (or (not (protocol-number c)) (eql (protocol-number c) proto)))
      (unless (funcall (callback c)
                       layer proto direction packet node interface)
        ;; if callback returns false we are done
        (return-from call-callbacks nil))))
  ;; all returned true or none found
  t)

(defgeneric neighbours(node &key no-leaf)
  (:documentation "Return the list of neigbour records for a node - if
no-leaf is true do not include leaf nodes in the list")
  (:method((node node) &key no-leaf)
    (reduce
     #'nconc
     (map
      'list
      #'(lambda(interface)
          (let ((neighbours (interface:neighbours interface)))
            (unless (and no-leaf ;; check for leaf neighbors with local route
                         (= (length neighbours) 1) ;; only 1 neighbour
                         (when-bind*
                             ((peer-interface
                               (first (interface:peer-interfaces interface)))
                              (node (node peer-interface))
                              (ip (when (<= (length (neighbours node)) 1)
                                    (ipaddr node))))
                           (local-ipaddr-p ip interface)))
              (copy-list neighbours))))
      (interfaces node)))))

;; port demultiplexing - maps layer 4 protocol instances to bindings


(defun lookup-by-port(protocol-number node
                      &key local-port local-addr peer-port peer-addr)
  (assert local-port (local-port))
  (let ((demux (port-demux node)))
    (or
     (gethash
      (list protocol-number local-port local-addr peer-port peer-addr)
      demux)
     (gethash (list protocol-number local-port local-addr) demux)
     (gethash (list protocol-number local-port) demux))))

(defvar *min-transient-port* 10000 "Minimum port number for transient ports")

(defun bind-key(proto lp la rp ra)
  `(,proto ,lp ,@(when la (list la)) ,@(when rp (list rp ra))))

(defun bind(protocol node
            &key (protocol-number (layer4:protocol-number protocol))
            local-port local-addr remote-port remote-addr)
  (let ((demux (port-demux node)))
    (when (or (not local-port) (zerop local-port))
      ;; bind to an available port
      (let ((port 0))
        (maphash
         #'(lambda(k v)
             (declare (ignore v))
             (when (= protocol-number (first k))
               (setf port (max port (second k)))))
         demux)
        (setf local-port (if (= port 0) *min-transient-port* (1+ port)))))
    (let ((k (bind-key protocol-number
                       local-port local-addr remote-port remote-addr)))
      (unless (gethash k demux)
        (setf (gethash k demux) protocol)))))

(defun unbind(protocol node
              &key (protocol-number (layer4:protocol-number protocol))
              local-port local-addr remote-port remote-addr)
  (let ((k (bind-key protocol-number
                     local-port local-addr remote-port remote-addr))
        (demux (port-demux node)))
    (let ((bound-protocol (gethash k demux)))
      (when (eql protocol bound-protocol)
        (remhash k demux)))))

(defun bound-protocols(node)
  "Return the list of bound protocols for this node"
  (let ((result nil))
    (maphash #'(lambda(k v) (declare (ignore k)) (push v result))
             (port-demux node))
    result))

(defun applications(node)
  (mapcan
   #'(lambda(protocol)
       (when-bind(app (layer4:application protocol))
         (list app)))
   (bound-protocols node)))

(defmethod reset((node node))
  (initialise-routes node)
  (map 'nil #'reset (interfaces node))
  (map 'nil #'reset (bound-protocols node))
  (map 'nil #'reset (applications node)))
