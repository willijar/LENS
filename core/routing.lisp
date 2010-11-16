;;;; LENS routing interface
;;;; Copyright (C) 2010 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;; $Id: packet.lisp,v 1.2 2005/11/29 09:00:39 willijar Exp $

(in-package :layer3)

(defstruct next-hop
  interface address)

(defclass routing()
  ((node :type node :initarg :node :reader node
         :documentation "Nope to which routing object is attached")
   (default-route :type routing-entry :initform nil :accessor default-route
                  :documentation "The default (gateway) route"))
  (:documentation "Base class for all the routing protocols that may
be needed for a simulation."))

(defgeneric add-route(dest mask interface nexthop routing)
  (:documentation "Add a routing entry to dest ip address using subnet mask and
interface and next hop IP address"))

(defgeneric rem-route(dest mask routing)
  (:documentation "Remove a route to dest using subnet"))

(defgeneric find-route(ipaddr routing &optional packet)
  (:documentation "Lookup routing-entry from node to ipaddr (possibly
using source routing in packet"))

(defgeneric initialise-routes(routing)
  (:documentation "Initialise routing table"))

(defgeneric reinitialise-routes(routing changed-entity)
  (:documentation "Reinitialise routing table due to topology change -
changed-entity is the object in the topology who's state has changed"))

(defgeneric topology-changed(changed-entity)
  (:documentation "Inform routing that topology has changed")
  (:method(entity)
    (reinitialise-routes changed-entity (routing (node entity)))))

(defvar *default-routing* nil "make-instance args for default routing")