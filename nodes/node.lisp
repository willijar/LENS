;; Implementation of base node methods
;; Copyright (C) 2010 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;; This file is part of Lisp Educational Network Simulator (LENS)

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:

;;

;;; Code:

(in-package :node)

(defun network-addresses(node)
  "List of all network addresses and masks for a node"
  (let ((result
         (delete nil
                 (map 'list #'(lambda(interface)
                                (list (network-address interface)
                                      (network-mask interface)))
                      (interfaces node))
                 :key #'car)))
    (if (find (network-address node) result :key #'car :test #'address=)
        result
        (cons (list (network-address node)) result))))

(defmethod find-interface((link link) (node node))
  "Return interface on node connected to link"
  (find link (interfaces node) :key #'link))

(defmethod find-interface((peer node) (node node))
  "Return interface connecting node to peer"
  (some
   #'(lambda(interface)
       (find peer (layer1:peer-interfaces (link interface) interface) :key #'node))
   (interfaces node)))

(defmethod find-interface((address network-address) (node node))
  "Return interface for given  network address"
  (or
   (find local-ipaddr (interfaces node)
         :test #'address=
         :key #'network-address)
   (let ((r (layer3::getroute peer-address node)))
     (when r (interface r)))))

(defun local-ipaddr-p(ipaddr node)
  (or
   (address= ipaddr (network-address node))
   (find local-ipaddr (interfaces node)
         :test #'address=
         :key #'network-address)))

(defmethod layer3:getroute(address (node node) &key packet &allow-other-keys)
  (layer3:getroute address (layer3:routing node) :packet packet))

(defmethod (setf layer3:getroute)(vertex address (node node) &key
                                  &allow-other-keys)
  (setf (layer3:getroute address (layer3:routing node)) vertex))

(defmethod layer3:remroute(address (node node) &key &allow-other-keys)
  (layer3:remroute address (layer3:routing node)))

(defmethod layer3:reinitialise-routes((node node) changed-entity)
  (layer3:reinitialise-routes (layer3:routing node) changed-entity))
