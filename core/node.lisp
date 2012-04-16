;;;; LENS base node definition
;;;; Copyright (C) 2003-2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;; $Id: packet.lisp,v 1.2 2005/11/29 09:00:39 willijar Exp $

(in-package :node)

(defgeneric interfaces(entity)
  (:documentation "Return a sequence of all the interfaces on entity"))

(defgeneric find-interface(attribute node)
  (:documentation "Find a local interface by attribute")
  (:method((test function) node)
    (find-if test (interfaces node))))

(defclass node()
  ((uid :type fixnum :reader uid
        :documentation "Unique id of this node - also index in nodes array")
   (location :initform +origin+ :type location :accessor location
             :documentation "Physical Location of this node")
   (network-address :type network-address :initform (ipaddr :next)
                    :initarg :ipaddr :initarg :network-address
                    :reader ipaddr :reader network-address
                    :documentation "network address of this node")
   (interfaces :type vector
               :initform (make-array 2 :adjustable t :fill-pointer 0)
               :reader interfaces
               :documentation "Vector of interfaces for this node")
   (layer3:protocols :type list :initform nil :accessor layer3:protocols
                     :documentation "List of layer 3 protocol entities")
   (layer4:protocols :type list :initform nil :accessor layer4:protocols
                     :documentation "List of layer 4 protocol entities")
   (applications :type list :initform nil :accessor applications
                 :documentation "List of applications")
   (up-p :type boolean :initform t :reader up-p
          :documentation "True if node up, false if failed")
   (layer3:routing :initarg :routing :accessor layer3:routing
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
        (make-array 1024 :initial-element nil :adjustable t :fill-pointer 0)))

;(eval-when(:load-toplevel :execute)
;  (pushnew *nodes* *reset-hooks*))

(defmethod initialize-instance :after ((node node) &key &allow-other-keys)
  (setf (slot-value node 'uid) (vector-push-extend node (nodes)))
  (setf (slot-value node 'layer3:routing)
        (apply #'make-instance `(,@layer3::*default-routing* :node ,node))))

(defmethod mkup((node node) &key (inform-routing t))
  (unless (slot-value node 'up-p)
    (setf (slot-value node 'up-p) t)
    (when inform-routing (layer3:topology-changed node))))

(defmethod mkdown((node node) &key (inform-routing t))
  (when (slot-value node 'up-p)
    (setf (slot-value node 'up-p) nil)
    (when inform-routing (layer3:topology-changed node))))

(defgeneric node(entity)
  (:documentation "Return a node associated with entity")
  (:method((entity integer)) (aref *nodes* entity))
  (:method((addr network-address))
    (find addr (nodes) :key #'network-address :test #'address=)))

(defgeneric (setf node)(node entity)
  (:documentation "Set the node associated with an entity"))

;; layer 3 and 4 protocol registration on nodes

(defmethod layer3:find-protocol((protocol-number integer) (node node))
  (or (layer3:find-protocol protocol-number (layer3:protocols node))
      (let ((type  (layer3:find-protocol protocol-number
                                         layer3::*standard-protocols*)))
        (when type (layer3:register-protocol type node )))))

(defmethod layer3:find-protocol((protocol-type symbol) (node node))
  (or (layer3:find-protocol protocol-type (layer3:protocols node))
      (layer3:register-protocol protocol-type node)))

(defmethod layer3:register-protocol((protocol symbol) (node node))
  (when (layer3:find-protocol (protocol:protocol-number protocol) (layer3:protocols node))
    (error "Unable to register a duplicate layer 3 protocol ~A" protocol))
  (let ((instance (make-instance protocol :node node)))
    (push instance (layer3:protocols node))
    instance))

(defmethod layer4:find-protocol((protocol-number integer) (node node))
  (or (layer4:find-protocol protocol-number (layer4:protocols node))
      (let ((type  (layer4:find-protocol protocol-number
                                         layer4::*standard-protocols*)))
        (when type (layer4:register-protocol type node )))))

(defmethod layer4:find-protocol((protocol-type symbol) (node node))
  (or (layer4:find-protocol protocol-type (layer4:protocols node))
      (layer4:register-protocol protocol-type node)))

(defmethod layer4:register-protocol((protocol symbol) (node node))
  (when (layer4:find-protocol (protocol:protocol-number protocol)
                              (layer3:protocols node) )
    (error "Unable to register a duplicate layer 4 protocol ~A" protocol))
  (let ((instance (make-instance protocol :node node)))
    (push instance (layer4:protocols node))
    instance))

(defstruct callback
  (direction :rx :type (member :tx :rx))
  (layer 0 :type (integer 5))
  (protocol-number 0 :type integer)
  (interface nil)
  (callback nil :type function)) ;; function of protocol entity and packet

(defun add-callback(callback node)
  (push callback (callbacks node)))

(defun call-callbacks(direction protocol packet)
  "The callback function must return true if the packet should
 continue to be processed by the protocol stack, and false if the
 callback function has deleted the packet. For example, if the
 callback is implementing a firewall function, the firewall may decide
 that the packet should not be forwarded for further processing. In
 that case, it should drop the packet and return false."
  (dolist(c (callbacks (node protocol)))
     (when (and ;; check for match
            (eql direction (callback-direction c))
            (or (zerop (callback-layer c))
                (= (callback-layer c) (protocol:layer protocol)))
            (and (= 2 (protocol:layer protocol))
                 (eql (callback-interface c) (interface protocol)))
            (or (zerop (callback-protocol-number c))
                (= (callback-protocol-number c)
                   (protocol:protocol-number protocol))))
       (unless (funcall (callback-callback c) protocol packet)
        ;; if callback returns false we are done
        (return-from call-callbacks nil))))
  ;; all returned true or none found
  t)

(defmethod reset((node node))
  (layer3:reinitialise-routes (layer3:routing node) nil)
  (dolist(slot '(layer3:protocols layer4:protocols interfaces applications))
    (reset (slot-value node slot))))

(defgeneric add-interface(interface node)
  (:documentation "Add an interface to a node")
  (:method(interface (node node))
    (unless (find interface (interfaces node))
      (vector-push-extend interface (interfaces node)))
    interface))

(defmethod layer3:topology-changed((node node))
  (layer3:reinitialise-routes (layer3:routing node) node))

(defmethod delete-notifications(entity (node node))
  (map 'nil #'(lambda(interface) (delete-notifications entity interface))
       (interfaces node)))

(defmethod reset((entity (eql :all)))
  (reset (node:nodes))
  (dolist(h *reset-hooks*)
    (typecase h
      (function (funcall h))
      (t (reset h)))))