;; Node definition and implementation for WSN
;; Copyright (C) 2014 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;;; Copying:

;; This file is part of Lisp Educational Network Simulator (LENS)

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; LENS is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; WSN nodes bring together sensors, applications, communications,
;; mobility and resource handling. They correspond to a single mote
;; If they run out of energy or memory they shutdown

;;; Code:

(in-package :lens.wsn)

(defclass node(compound-module)
  ((owner :reader network)
   (index :reader nodeid)
   (network-address :parameter t :reader network-address)
   (startup-offset
    :parameter t :type time-type :initform 0.0d0 :reader startup-offset
    :documentation "Node offset startup delay in seconds")
   (startup-randomization
    :parameter t :type time-type :initform 0.05d0
    :reader startup-randomization
    :documentation "node startup randomisation, in seconds"))
  (:gates
   (receive :input))
  (:submodules
   (sensor #'(lambda(node) (num-physical-processes (network node))) sensor)
   (application application)
   (communications communications)
   (mobility mobility)
   (resources resources))
  (:connections
   (<=> (application network) (communications application))
   (<= (communications receive) receive))
  (:metaclass compound-module-class))

(defmethod print-object((node node) os)
  (print-unreadable-object(node os :type t :identity nil)
    (when (slot-boundp node 'index)
      (format os "~D~:[(~A)~;~]"
              (nodeid node)
              (eql (nodeid node) (network-address node))
              (network-address node) ))))

(defmethod build-connections((node node))
  (call-next-method)
  (let ((application (submodule node 'application)))
    (map nil
         #'(lambda(sensor)
             (connect (gate sensor 'application :direction :output)
                      (gate application 'sensor :direction :input :index '++))
             (connect (gate application 'sensor :direction :output :index '++)
                      (gate sensor 'application :direction :input)))
         (submodule node 'sensor))))

(defmethod initialize-instance :after ((node node) &key &allow-other-keys)
  (unless (slot-boundp node 'network-address)
    (setf (slot-value node 'network-address) (nodeid node)))
  (subscribe node 'out-of-memory node)
  (subscribe node 'out-of-energy node))

(defmethod initialize list ((node node) &optional (stage 0))
  (case stage
    (0
     (let ((delay (+ (startup-offset node)
                     (* (uniform 0.0 (startup-randomization node))))))
       (tracelog "Delaying startup to ~:/dfv:eng/s" delay)
       (schedule-at
        node (make-instance 'message :name 'node-startup)
        :delay delay))))
  t)

(defmethod handle-message((node node) message)
  (case (name message)
    (node-startup (emit node 'node-startup))
    (node-shutdown (emit node 'node-shutdown))
    (t (call-next-method))))

(defmethod receive-signal((node node) signal source value)
  (case signal
    ((out-of-energy out-of-memory)
     ;; if out of energy or memory stop everything
     (emit node 'node-shutdown value))))

(defmethod location((node node)) (location (submodule node 'mobility)))