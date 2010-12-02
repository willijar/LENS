;; $Id$
;; Implementation of base node methods
;; Copyright (C) 2007 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;; This file is part of Lisp Educational Network Simulator (LENS)

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:

;;

;;; Code:

(in-package :node)

(defmethod ipaddrs((node node))
  (let ((local-found-p nil)
        (local-ipaddr (ipaddr node))
        (addrs nil)
        (masks nil))
    (loop :for interface :across (interfaces node)
          :for ipaddr = (ipaddr interface)
          :for ipmask = (ipmask interface)
          :when ipaddr
          :do (progn
                (push ipaddr addrs)
                (push ipmask masks)
                (when (address= ipaddr local-ipaddr)
                  (setf local-found-p t))))
    (when (and local-ipaddr (not local-found-p))
      (push local-ipaddr addrs)
      (push (ipmask 32) masks))
    (values addrs masks)))

(defmethod ipaddrs((nodes list))
  "Accumulate ipaddrs and masks for a list of nodes"
  (let ((addrs nil)
        (masks nil))
    (dolist(node nodes)
      (multiple-value-bind(a m) (ipaddrs node)
        (setf addrs (append a addrs))
        (setf masks (append m masks))))
    (values addrs masks)))

(defmethod add-interface((interface interface) (node node))
  (setf (node interface) node)
  (unless (find interface (interfaces node))
    (vector-push-extend interface (interfaces node)))
  ;; if only 1 interface node has same ipaddr
  (when (= 1 (length (interfaces node)))
    (cond ((not (ipaddr node))
           (setf (slot-value node 'ipaddr) (ipaddr interface)))
          ((not (ipaddr interface))
           (setf (slot-value interface 'ipaddr) (ipaddr node)))))
  interface)

(defmethod find-interface((test function) (node node))
  (find-if test (interfaces node)))

(defmethod find-interface((link link) (node node))
  "Return interface on node connected to link"
  (find link (interfaces node) :key #'link))

(defmethod find-interface((peer node) (node node))
  "Return interface connecting node to peer"
  (find peer (interfaces node) :test #'peer-node-p))

(defmethod find-interface((local-ipaddr ipaddr) (node node))
  "Return interface with given ipaddress"
  (find local-ipaddr (interfaces node)
        :test #'(lambda(a b) (and a b (address= a b)))
        :key #'ipaddr))

(defmethod insert-protocol(layer protocol-number protocol (node node))
  (insert-protocol layer protocol-number protocol
                   (or (protocol-graph node)
                       (setf (protocol-graph node) (make-protocol-graph)))))

(defmethod find-protocol(layer protocol-number (node node))
  (or
   (and (protocol-graph node)
        (find-protocol layer protocol-number (protocol-graph node)))
   (find-protocol layer protocol-number *common-protocol-graph*)))

(defmethod local-ipaddr-p((ipaddr ipaddr) (node node))
  (and (ipaddr node)
       (or
        (address= ipaddr (ipaddr node))
        (find-interface ipaddr node))))

(defmethod add-route (dest mask interface nexthop (node node))
  (add-route dest mask interface nexthop (routing node)))

(defmethod rem-route(dest mask (node node))
  (rem-route dest mask (routing node)))

(defmethod find-route(ipaddr (node node) packet)
  (find-route ipaddr (routing node) packet))

(defmethod initialise-routes((node node))
  (initialise-routes (routing node)))

(defmethod reinitialise-routes((node node) changed-entity)
  (reinitialise-routes (routing node) changed-entity))