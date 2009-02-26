;;;; LENS packet header API
;;;; Copyright (C) 2003-2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;; $Id: packet.lisp,v 1.2 2005/11/29 09:00:39 willijar Exp $

(in-package :routing)

(defclass routing-entry()
  ((interface :type interface :reader interface :initarg :interface
              :documentation "Interface for route")
   (next-hop :type ipaddr :reader next-hop :initarg :next-hop
            :documentation "Next hop IP Address"))
  (:documentation "This class defines each entry in the routing table."))

(defmethod print-object((re routing-entry) stream)
  (print-unreadable-object (re stream :type t :identity t)
    (format stream "~A -> ~A"
            (or (ipaddr (interface re)) (ipaddr (node (interface re))))
            (or (next-hop re)
                (let ((peer (interface:default-peer-interface (interface re))))
                  (or (ipaddr peer)
                      (ipaddr (node peer))))))))

(defclass routing()
  ((node :type node :initarg :node :reader node
         :documentation "Nopde to which routing object is attached")
   (default-route :type routing-entry :initform nil :accessor default-route
                  :documentation "The default (gateway) route"))
  (:documentation "Base class for all the routing protocols that may
be needed for a simulation."))

(defgeneric add-route(dest mask interface nexthop routing)
  (:documentation "Add a routing entry to dest ip address using subnet mask and
interface and next hop IP address"))

(defgeneric rem-route(dest mask routing)
  (:documentation "Remove a route to dest using subnet"))

(defgeneric find-route(ipaddr routing packet)
  (:documentation "Lookup routing-entry from node to ipaddr (possibly
using source routing in packet"))

(defgeneric initialise-routes(routing)
  (:documentation "Initialise routing table"))

(defgeneric reinitialise-routes(routing changed-entity)
  (:documentation "Reinitialise routing table due to topology change -
changed-entity is the object in the topology who's state has changed"))

(defun topology-changed(&optional changed-entity)
  (loop :for node :across (nodes)
        :do (reinitialise-routes node changed-entity)))

(defvar *default-routing* nil "make-instance args for default routing")

(defstruct neighbour
  (node nil :type node) ;; end node of a network edge
  (interface nil)  ;; the (outgoing) corresponding interface
  (weight 1 :type number)) ;; weighting/cost
