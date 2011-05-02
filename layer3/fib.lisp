;; Fowarding Information Base implementation
;; Copyright (C) 2011 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;; This file is part of Lisp Educational Network Simulator (LENS)

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:

;;

;;; Code:

(in-package :layer3)

(defclass fib(routing)
  ((fib-map :type list
            :reader fib-map
            :documentation "fib tables per mask type")
   (address-length :initform 32 :allocation :class))
  (:documentation "Abstract class for routing using a Forwarding
Information Base"))

(defmethod initialize-instance :after ((fib fib) &key &allow-other-keys)
  (reinitialise-routes fib nil))

(defmethod reinitialise-routes((fib fib) (changed-entity null))
  (with-slots(fib-map address-length) fib
    (setf fib-map
          (list (cons (network-mask address-length)
                      (make-hash-table)))))
  (call-next-method))

(defmethod getroute((address network-address) (fib fib)
                      &key &allow-other-keys)
  (map 'nil
       #'(lambda(entry)
           (let ((match (gethash (subnet address (car entry)) (cdr entry))))
             (when match (return-from getroute match))))
       (fib-map fib)))

(defmethod (setf getroute)(vertex (address network-address) (fib fib)
                           &key (mask (caar (slot-value fib 'fib-map)))
                           &allow-other-keys)
  (unless (vertex= vertex (default-route fib))
    (with-slots(fib-map) fib
      (let ((entry
             (or (find mask fib-map :key #'car)
                 (let ((new-entry (cons mask (make-hash-table))))
                   (setf fib-map (sort (cons new-entry fib-map) #'address<))
                   new-entry))))
        (setf (gethash (subnet address mask) (cdr entry)) vertex)))))

(defmethod remroute((dest network-address) (fib fib)
                    &key (mask (caar (slot-value fib 'fib-map)))
                     &allow-other-keys)
  (let ((entry  (find mask (fib-map fib) :key #'car)))
    (when entry (remhash (subnet dest mask) (cdr entry)))))

(defmethod reinitialise-routes((fib fib) (interface interface))
  (unless (up-p interface)
    (map 'nil
         #'(lambda(entry)
             (let ((h (cdr entry)))
               (maphash #'(lambda(k v)
                            (when (eql (vertex-start v) interface)
                              (remhash k h)))
                        h)))
         (fib-map fib))))

(defmethod reinitialise-routes((fib fib) (changed-entity node))
  (unless (up-p changed-entity) (reinitialise-routes fib nil)))



;; (defclass routing-manual(fib routing)
;;   ()
;;   (:documentation "Manual routing. The routing protocol doesn't manage
;; routing - the user must add the entries explicitly."))

;; (defmethod initialise-routes((routing routing-manual)))

;; (defmethod reinitialise-routes((routing routing-manual) changed)
;;   (declare (ignore changed)))

;; (defmethod initialise-routes((routing routing-static))
;;   (let* ((node (node routing))
;;          (neighbours (up-neighbours node)))
;;     (cond
;;       ((> (length neighbours) 1)
;;        (let ((nodes (up-nodes routing)))
;;          (multiple-value-bind(previous cost edges)
;;              (dijkstra node nodes #'up-neighbours
;;                        :edge-cost #'neighbour-weight
;;                        :edge-end-vertice #'neighbour-node)
;;            (declare (ignore cost))
;;            ;; extra first hop data from dijkstra
;;            (let ((first-hops (dijkstra-first-hops previous)))
;;              ;; determine the most common first hop, make it the default
;;              ;; and remove from those to be entered in fib
;;              (let ((max-hop nil) (biggest 0))
;;                (maphash #'(lambda(hop nodes)
;;                             (let ((l (length nodes)))
;;                               (when (> l biggest)
;;                                 (setf max-hop hop biggest l))))
;;                         first-hops)
;;                (when max-hop
;;                  (let ((iface (neighbour-interface (gethash max-hop edges))))
;;                    (setf (default-route routing)
;;                          (make-instance
;;                           'routing-entry
;;                           :interface iface
;;                           :next-hop (peer-node-ipaddr max-hop iface))))
;;                  (remhash max-hop first-hops)))
;;              (maphash
;;               #'(lambda(next-hop nodes)
;;                   ;; get list of all ipaddresss and masks for all nodes
;;                   (let ((next-hop-if (neighbour-interface
;;                                       (gethash next-hop edges))))
;;                     (multiple-value-bind(ipaddrs masks) (ipaddrs nodes)
;;                       (add-route ipaddrs masks
;;                                  next-hop-if
;;                                  (peer-node-ipaddr next-hop next-hop-if)
;;                                  routing))))
;;               first-hops)))))
;;       ((= (length neighbours) 1)
;;        ;; if only one neighbour all traffic must go through it
;;        (let ((interface (neighbour-interface (first neighbours))))
;;          (setf (default-route routing)
;;                (make-instance
;;                 'routing-entry
;;                 :interface interface
;;                 :next-hop (peer-node-ipaddr
;;                            (neighbour-node (first neighbours))
;;                            interface))))))))

