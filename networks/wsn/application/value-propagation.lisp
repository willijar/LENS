;; Value Propoagation application for WSN
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

;; THis application will transmit maximum of its sensor reading or received
;; sensor reading once over network

;;; Code:
(in-package :lens.wsn)

(register-signal
 'got-value
 "Emitted when application got value")

(defclass value-propagation(application)
  ((header-overhead :initform 8)
   (payload-overhead :initform 2)
   (temp-threshold :initform 15 :parameter t :initarg :temp-threshold)
   (total-packets :initform 0 :accessor total-packets :type fixnum)
   (current-max-received-value
    :initform -1e3 :accessor current-max-received-value :type float)
   (current-max-sensed-value
    :initform -1e3 :accessor current-max-sensed-value :type float)
   (sent-once :type boolean :initform nil)
   (the-value :type float :initform 0 :accessor the-value))
  (:properties
   :statistic (got-value :title "got value" :default (last-value)))
  (:metaclass module-class)
  (:documentation "Application class which will continually sample sensors
and  will transmit maximum of its sensor reading or received
sensor reading once over the network."))

(defmethod startup((application value-propagation))
  (call-next-method)
  (set-timer application 'request-sample 0d0))

(defmethod handle-timer((application value-propagation)
                        (timer (eql 'request-sample)))
  (sensor-request application))

(defmethod handle-message((application value-propagation)
                          (pkt application-packet))
  (emit application 'packet-receive pkt)
  (let ((received-value (payload pkt)))
    (with-slots(current-max-received-value temp-threshold the-value sent-once)
        application
      (when (> received-value current-max-received-value)
        (setf current-max-received-value received-value))
      (when (and (not sent-once) (> received-value temp-threshold))
        (setf the-value received-value
              sent-once t)
        (emit application 'got-value received-value)
        (to-network application
                    (duplicate pkt) broadcast-network-address)
        (tracelog "Got the value: ~/dfv:eng/" received-value)))))

(defmethod handle-sensor-reading((application value-propagation)
                                 (sensed-value real))
  (with-slots(current-max-sensed-value temp-threshold the-value sent-once)
        application
    (when (> sensed-value current-max-sensed-value)
      (setf current-max-sensed-value sensed-value))
    (when (and (not sent-once) (> sensed-value temp-threshold))
      (setf the-value sensed-value
            sent-once t)
      (emit application 'got-value sensed-value)
      (to-network application sensed-value broadcast-network-address))))
