;; SImple Aggregation application for WSN
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

;; This is an example of sensor reading aggregation.

;; The aggregated value is maximum of its sensor reading and received
;; sensor reading from nodes further away (at higher level) in ring routing.

;;; Code:
(in-package :lens.wsn)

(defclass simple-aggregation (application)
  ((header-overhead :initform 5)
   (payload-overhead :initform 100)
   (priority :parameter t :initform 1 :reader priority :initarg :priority)
   (sink-network-address :parameter t :type fixnum :reader sink-network-address)
   (sample-interval :parameter t :type time-type :initform 1)
   ;; timers
   (request-sample
    :type timer-message :initform (make-instance 'timer-message))
   (send-aggregated-value :type timer-message :initform (make-instance 'timer-message))
   ;; implementatio values
   (waiting-time-for-lower-level-data :type time-type :initform 0d0)
   (last-sensed-value :type real :initform 0.0)
   (aggregated-value :type real :initform 0.0))
  (:metaclass module-class)
  (:documentation "Application providing an example of sensor reading
 aggregation with [[multipath-rings]] routing. The aggregated value is
 maximum of its sensor reading and received sensor reading from nodes
 further away (at higher level) in [[multipath-rings]] routing."))

(defmethod startup((instance simple-aggregation))
  (call-next-method)
  (set-timer instance 'request-sample 0))

(defmethod handle-timer
    ((instance simple-aggregation) (timer (eql 'request-sample)))
  (sensor-request instance)
  (set-timer instance 'request-sample (slot-value instance 'sample-interval)))

(defmethod handle-timer
    ((instance simple-aggregation) (timer (eql 'send-aggregated-value)))
  (to-network instance
              (make-instance 'application-packet
                       :payload (slot-value instance 'aggregated-value))
              'application))

(defmethod handle-sensor-reading((instance simple-aggregation) (value real))
  (with-slots(last-sensed-value aggregated-value) instance
    (setf last-sensed-value value
          aggregated-value (max aggregated-value value))))

(defmethod handle-message ((instance simple-aggregation)
                           (pkt network-control-message))
  (let ((argument (argument pkt))
        (command (command pkt)))
    (when (member command '(lens.wsn.routing.multipath-rings::tree-level-updated
                            lens.wsn.routing.multipath-rings::connected-to-tree))
      (with-slots(routing-level waiting-time-for-lower-level-data
                  sample-interval) instance
        (setf routing-level
              (lens.wsn.routing.multipath-rings::mprings-sink-level argument))
        (setf  waiting-time-for-lower-level-data
               (* sample-interval (expt 2 routing-level)))
        (tracelog "Routing level ~D" routing-level)
        (set-timer instance
                   'send-aggregated-value  waiting-time-for-lower-level-data)))))
