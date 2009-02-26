;;;; LENS packet header API
;;;; Copyright (C) 2003-2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;; $Id: packet.lisp,v 1.2 2005/11/29 09:00:39 willijar Exp $

(in-package :routing)

(defclass fib()
  ((fib-map :type vector
            :initform (make-array 1 :initial-element nil
                                  :adjustable t :fill-pointer 0)
            :reader fib-map
            :documentation "fib tables per mask type"))
  (:documentation "Abstract class for routing using a Forwarding
Information Base"))

(declaim (inline fib-key))

(defun fib-key(mask address)
  (if mask
    (logand (address::ip address) (address::mask mask))
    (address::ip address)))

(defun get-fib-map(mask fib &optional create)
  (let* ((c (- 32 (bitcount mask)))
         (v (fib-map fib))
         (big-enough (< c (length v)))
         (map (when big-enough (aref v c))))
    (or map
        (when create
          (unless big-enough
            (adjust-array v (1+ c) :initial-element nil)
            (setf (fill-pointer v) (1+ c)))
          (setf (aref v c) (make-hash-table))))))

(defmethod add-route((dests list) (masks list)
                     interface nexthop (fib fib))
  "Add a routing entry for a list of ip addresses and masks"
  (let ((entry (make-instance 'routing-entry
                              :interface interface :next-hop nexthop)))
    (map 'nil
         #'(lambda(dest mask)
             (setf (gethash (fib-key mask dest) (get-fib-map mask fib t))
                   entry))
         dests masks)))

(defmethod add-route((dest ipaddr) (mask ipmask)
                     interface nexthop (fib fib))
  (add-route (list dest) (list mask) interface nexthop fib))

(defmethod rem-route((dest ipaddr) (subnet ipmask) (fib fib))
  (let ((h (get-fib-map subnet fib)))
    (when h (remhash (fib-key subnet dest) h))))

(defmethod find-route((dest ipaddr) (fib fib) packet)
  (declare (ignore packet))
  (let ((map (fib-map fib)))
    (loop :for i :from 0 :below (length map)
          :for h = (aref map i)
          :when h
          :do (let ((match (gethash (fib-key (ipmask (- 32 i)) dest) h)))
                (when match (return-from find-route match)))))
  (default-route fib))

(defclass routing-manual(fib routing)
  ()
  (:documentation "Manual routing. The routing protocol doesn't manage
routing - the user must add the entries explicitly."))

(defmethod initialise-routes((routing routing-manual)))

(defmethod reinitialise-routes((routing routing-manual) changed)
  (declare (ignore changed)))