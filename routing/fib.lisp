;;;; Copyright (C) 2010 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;; $Id: packet.lisp,v 1.2 2005/11/29 09:00:39 willijar Exp $

(in-package :layer3)

(defclass fib(routing)
  ((fib-map :type hash-table
            :initform (make-hash-table :size 4)
            :reader fib-map
            :documentation "fib tables per mask type")
   (address-length :initform 32 :allocation :class))
  (:documentation "Abstract class for routing using a Forwarding
Information Base"))

(defmethod initialize-instance :after ((fib fib) &key &allow-other-keys)
  (reinitialise-routes fib nil))

(defmethod reinitialise-routes((fib fib) (changed-entity null))
  (with-slots(fib-map address-length) fib
    (setf fib-map (make-array (/ address-length 8)))
    (dotimes(i (length fib-map))
      (setf (aref fib-map i)
            (cons (make-address-mask (* (- (length fib-map) i) 8)
                                     address-length)
                  (make-hash-table))))))

(declaim (inline get-fib-hash))
(defun get-fib-hash(mask fib)
  (with-slots(fib-map) fib
    (aref fib-map (if mask (- (length fib-map) (/ (logcount mask) 8)) 0))))

(defmethod getroute((address network-address) (fib fib)
                      &key &allow-other-keys)
  (let ((bytes (address-bytes address)))
    (with-slots(fib-map) fib
      (dotimes(i (length fib-map))
        (let* ((v (aref fib-map i))
               (mask (car v))
               (hash (cdr v)))
          (let ((match (gethash (logand mask bytes) hash)))
            (when match (return-from getroute match))))))))

(defmethod (setf getroute)(vertex (address network-address) (fib fib) &key mask &allow-other-keys)
  (unless (vertex= vertex (default-route fib))
    (with-slots(fib-map) fib
    (let ((bytes (address-bytes address)))
      (if mask
        (dotimes(i (length fib-map))
          (let* ((v (aref fib-map i))
                 (tmask (car v))
                 (hash (cdr v))
                 (k (logand mask bytes)))
            (if (= mask tmask)
                (return-from getroute
                  (setf (gethash k hash) vertex))
                (remhash k hash))))
        (setf (gethash bytes (aref fib-map 0)) vertex))))))

(defmethod remroute((dest network-address) (fib fib)
                     &key mask &allow-other-keys)
  (remhash (logand mask (address-bytes dest)) (get-fib-hash mask fib)))

(defmethod reinitialise-routes((fib fib) (interface interface))
  (unless (up-p interface)
    (loop
       :for h :across (fib-map fib)
       :do
       (maphash #'(lambda(k v)
                    (when (eql (vertex-start v) interface)
                      (remhash k h)))
                h))))

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

