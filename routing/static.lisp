;; $Id$
;; Implementation of global static routing
;; Copyright (C) 2007 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;; This file is part of Lisp Educational Network Simulator (LENS)

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:

;; Static routing is simple and easy to use by the user, but is memory
;; intensive.  It is recommended for simulation topologies on the
;; order of a few hundred Larger topologies should use either manual
;; or nix-vector routing

;;; Code:

(in-package :routing)

(defclass routing-static(fib routing)
  ((nodes :initarg :nodes :initform (node:nodes) :accessor routing-nodes
          :documentation
          "Sequence of nodes to be included in this static routing."))
  (:documentation "The static routing class. This routing object
calculates all the routes needed at a single instance. Static routing
is simple and easy to use by the user, but is memory intensive.  It is
recommended for simulation topologies on the order of a few hundred
Larger topologies should use either manual or nix-vector routing."))

;; if no default set set static as it
(eval-when(:compile-toplevel :load-toplevel)
  (unless *default-routing* (setf *default-routing* '(routing-static))))

(defmethod initialize-instance :after
    ((routing routing-static) &key &allow-other-keys)
  (unless (find (node routing) (routing-nodes routing))
    (error "Attempt to add static routing module to a node not
included in the routing set of nodes")))

(defmethod reinitialise-routes((routing routing-static) changed)
  ;;Naively clear route map and call initialise-routes
  (declare (ignore changed))
  (loop :for h :across (fib-map routing)
        :when h :do (clrhash h))
  (initialise-routes routing))

(defun up-nodes(routing)
  "Return a list of nodes which are up"
  (loop :for node :across (routing-nodes routing)
        :when (up-p node) :collect node))

(defun up-neighbours(node)
  "Return the neighbours which have up interfaces for node"
  (filter-if #'up-p (neighbours node) :key #'neighbour-interface))

(defmethod initialise-routes((routing routing-static))
  (let* ((node (node routing))
         (neighbours (up-neighbours node)))
    (cond
      ((> (length neighbours) 1)
       (let ((nodes (up-nodes routing)))
         (multiple-value-bind(previous cost edges)
             (dijkstra node nodes #'up-neighbours
                       :edge-cost #'neighbour-weight
                       :edge-end-vertice #'neighbour-node)
           (declare (ignore cost))
           ;; extra first hop data from dijkstra
           (let ((first-hops (dijkstra-first-hops previous)))
             ;; determine the most common first hop, make it the default
             ;; and remove from those to be entered in fib
             (let ((max-hop nil) (biggest 0))
               (maphash #'(lambda(hop nodes)
                            (let ((l (length nodes)))
                              (when (> l biggest)
                                (setf max-hop hop biggest l))))
                        first-hops)
               (when max-hop
                 (let ((iface (neighbour-interface (gethash max-hop edges))))
                   (setf (default-route routing)
                         (make-instance
                          'routing-entry
                          :interface iface
                          :next-hop (peer-node-ipaddr max-hop iface))))
                 (remhash max-hop first-hops)))
             (maphash
              #'(lambda(next-hop nodes)
                  ;; get list of all ipaddresss and masks for all nodes
                  (let ((next-hop-if (neighbour-interface
                                      (gethash next-hop edges))))
                    (multiple-value-bind(ipaddrs masks) (ipaddrs nodes)
                      (add-route ipaddrs masks
                                 next-hop-if
                                 (peer-node-ipaddr next-hop next-hop-if)
                                 routing))))
              first-hops)))))
      ((= (length neighbours) 1)
       ;; if only one neighbour all traffic must go through it
       (let ((interface (neighbour-interface (first neighbours))))
         (setf (default-route routing)
               (make-instance
                'routing-entry
                :interface interface
                :next-hop (peer-node-ipaddr
                           (neighbour-node (first neighbours))
                           interface))))))))
