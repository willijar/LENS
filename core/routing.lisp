;;;; LENS routing interface
;;;; Copyright (C) 2010 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;; $Id: packet.lisp,v 1.2 2005/11/29 09:00:39 willijar Exp $

(in-package :layer3)

;; vertex for topology and routing
;; we store interface entities for efficiency although ipaddresses are used by
;; protocols
(defstruct vertex start end)
(defun vertex=(a b)
  (and (eql (vertex-start a) (vertex-start b))
       (eql (vertex-end a) (vertex-end b))))

(defclass routing()
  ((node :type node :initarg :node :reader node
         :documentation "Node to which routing object is attached")
   (default-route :type vertex :initform nil :accessor default-route
                  :documentation "The default (gateway) route"))
  (:documentation "Base class for all the routing protocols that may
be needed for a simulation."))

(defgeneric getroute(network-address routing &key packet &allow-other-keys)
  (:documentation "Lookup vertex routing entry from node to ipaddr (possibly
using source routing in packet")
  (:method(any routing &key &allow-other-keys)
    (declare (ignore any))
    (default-route routing)))

(defgeneric (setf getroute)(vertex network-address routing &key &allow-other-keys)
  (:documentation "Add a routing entry to dest ip address using subnet mask and
interface and next hop IP address"))

(defgeneric remroute(network-address routing &key &allow-other-keys)
  (:documentation "Remove a route to dest using subnet"))

(defun leaf-node-p(node)
  (with-slots(interfaces) node
    (and (= 1 (length interfaces))
         (= 1 (length (peer-interfaces (link (aref interfaces 0))))))))

(defgeneric reinitialise-routes(routing changed-entity)
  (:documentation "Reinitialise routing table due to topology change -
changed-entity is the object in the topology who's state has changed. If no changed entity specified initialise entire state"))

(defgeneric topology-changed(changed-entity)
  (:documentation "Inform routing that topology has changed")
  (:method(entity)
    (reinitialise-routes (routing (node entity)) entity)))

(defvar *default-routing* nil "make-instance args for default routing")



(defun routing-neighbours(node &key no-leaf)
  "Return list of routing neighours for a node - if no-leaf is true do
 not include leaf nodes in the list. Only uses up interfaces and
 nodes."
  (when (up-p node)
    (mapcan
     #'(lambda(interface)
         (let ((peers (filter #'up-p (peer-interfaces (link interface) interface))))
           (unless (and no-leaf (<= (length peers) 1))
             (mapcar
              #'(lambda(peer) (make-vertex :start interface :end peer))
              peers))))
     (filter #'up-p (interfaces node)))))

(defmethod find-route((addr network-address) routing &optional packet)
  (let ((node (find addr (node:nodes) :key #'network-address)))
    (unless node (error "Node with network address ~A not found" addr))
    (find-route node routing packet)))