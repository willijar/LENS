;; Application simulation interfaces
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

;;

;;; Code:
(in-package :lens.wsn)

(register-signal
 'packet-receive
 "Emitted when an application receives a packet.")

(register-signal
 'packet-send
 "Emitted when an application entity sends a packet.")

(defclass app-net-control-info()
  ((RSSI :type float :initarg :RSSI :reader RSSI :initform nil
         :documentation "Received signal strength indicator (RSSI) of the received packet")
   (LQI :type float :initarg :LQI :reader LQI :initform nil
        :documentation "Link Quality Indicator (LQI) of the received packet")
   (source :initarg :source :reader source
           :documentation "Routing layer source address of the received packet")
   (destination
    :initarg :destination :reader destination
    :documentation "Routing layer destination address of the packet to be sent"))
  (:documentation "Information passed between application and
 communication layer which is external to the packet i.e. not carried
 by a real packet (the source and destination addresses and quality
 measures of received signal for the packet)"))

(defclass application-packet(packet)
  ((name
    :initarg :applicationid :reader applicationid
    :documentation "Used to filter packet delivery to specific applications.")
   (lens::encapsulated-packet :reader payload) ;; add in payload reader
   (sequence-number
    :initarg :seqnum :initarg :sequence-number
    :reader sequence-number :reader sequence-number
    :documentation "A field to distinguish between packets")
   (byte-length :type fixnum :initarg :byte-length :reader byte-length
                :initform 20))
  (:documentation "A generic application packet. If defining your own
  packet you have to extend from this packet. You do not have to use
  the fields already defined, and you can always define your own
  size."))

(defmethod print-object((pkt application-packet) stream)
  (print-unreadable-object(pkt stream :type t :identity nil)
    (format stream "#~D (~D bytes)" (sequence-number pkt) (byte-length pkt))))

(defmethod bit-length((pkt application-packet))
  (* 8 (byte-length pkt)))

(defmethod duplicate((pkt application-packet)
                     &optional (duplicate (make-instance 'application-packet)))
  (call-next-method)
  (copy-slots '(sequence-number byte-length) pkt duplicate))

(defclass application(wsn-module)
  ((owner :reader node)
   (applicationid
    :parameter t :type symbol :initform nil
    :initarg :id :reader applicationid
    :documentation "Used to filter packet delivery to specific applications.")
   (priority :parameter t :type integer :initarg :priority :initform 1
             :reader priority
             :documentation "What priority to give the application packets")
   (header-overhead
    :parameter t  :initform 8
    :type integer :initarg :header-overhead :reader header-overhead
    :documentation "Size of application packet header in bytes")
   (payload-overhead
    :parameter t :initform 12
    :type integer :initarg :payload-overhead :reader payload-overhead
    :documentation "Size of application packet payload in bytes")
   (last-sequence-number
    :initform -1 :type integer
    :documentation "Sequence number of last packet sent"))
  (:gates
   (network :inout)
   (sensor :inout 0))
  (:properties
   :statistic (latency
               :source (latency packet-receive)
               :title "Application Latency"
               :default ((histogram :min 0  :unit "s" :format "~3@/dfv:eng/")))
   :statistic (packet-receive :title "Application Packets Received"
                              :default (count))
   :statistic (packet-receive-per-node
               :title "Packets received per source node"
               :source (source (control-info packet-receive))
               :default (indexed-count))
   :statistic (packet-send :title "Application Packets Sent"
                           :default (count)))
  (:metaclass module-class)
  (:documentation "The Application core module class connects to node
  sensors for measurements and to the node communication module for
  sending and receiving data."))

(defmethod initialize-instance :after ((application application)
                                       &key &allow-other-keys)
  (unless (slot-boundp application 'applicationid)
    (setf (slot-value application 'applicationid)
          (class-name (class-of application)))))

(defgeneric next-sequence-number(instance)
  (:documentation "* Arguments

- instance :: a [[module]]

* Description

Returns the next packet sequence number to be used by a source module.")
  (:method (instance)
    (incf (slot-value instance 'last-sequence-number))))

(defun sensor-request(application &optional (sensor-index 0))
  "* Arguments

- application :: a [[application]]

* Optional Arguments

- sensor-index :: an =integer= (default 0)

* Description

Sends a request for a reading from /application/ to the sensor
indicated by /sensor-index/. There may be a delay in the reading. The
/application/ must implement [[handle-sensor-reading]] to receive the
returned measured value."
  (send application
        (make-instance 'sensor-message :name 'sensor-request)
        (gate application 'sensor :index sensor-index :direction :output)))

(defgeneric handle-sensor-reading(application measurement)
  (:documentation "* Arguments

- application :: a [[application]]

- measurement :: a [[measurement]] or a =real=

* Description

Called to pass a measured value from a [[sensor]] to an [[application]] module.
It must be specialised for all applications which

Must be implemented by applications to handle
  sensor readings")
  (:method(application (measurement measurement))
    (handle-sensor-reading application (measurement-value measurement)))
  (:method(application measurement)
    (declare (ignore application measurement))))

(defgeneric to-network(application entity &optional destination)
  (:documentation "* Arguments

- application :: an [[application]] or [[mac]]
- entity      :: a [[application-packet]] or [[communications-control-command]]
                 from an application or  [[routing-packet]] from a mac

* Optional Arguments

- destination :: a network routing destination address

* Description

Send message, packet or data to a [[routing]] module. This may be an
[[application-packet]] or [[communications-control-command]] from an
[[application]] module.
")
  (:method((application application) (command communications-control-command)
           &optional destination)
    (assert (not destination))
    (send application command 'network))
  (:method((application application) (packet application-packet)
           &optional destination)
    (if destination
        (setf (control-info packet)
              (make-instance
               'app-net-control-info
               :destination destination
               :source (network-address (node application))))
        (assert (destination (control-info packet))
                ()
                "Destination not specified for packet send"))
    (emit application 'packet-send packet)
    (send application packet 'network)
    (tracelog "Sending ~A to communication layer" packet))
  (:method((application application) (message message) &optional destination)
    (declare (ignore destination))
    (error "Application ~A attempting to send ~A to network"
           application message))
  (:method((application application) data &optional destination)
    (let ((packet (encapsulate application data)))
      (to-network application packet destination))))

(defmethod encapsulate((application application) data)
  (make-instance
   'application-packet
   :applicationid (applicationid application)
   :timestamp (simulation-time)
   :payload data
   :sequence-number (next-sequence-number application)
   :byte-length (packet-size application data)))

(defgeneric packet-size(application data)
  (:documentation "* Arguments

- application :: an [[application]] module
- data :: application data

* Description

Returns the size in bytes to be used as the [[byte-length]] of application packets sent by /application/ when sending /data/. Default implementation returns the sum of the =header-overhead= and =payload-overhead= parameters of the /application/.")
  (:method((application application) data)
    (declare (ignore data))
    (+ (header-overhead application) (payload-overhead application))))

(defmethod handle-message ((application application)
                           (message application-packet))
  (emit application 'packet-receive message)
  (tracelog "Received ~A" message))

(defmethod handle-message((application application) (message sensor-message))
  (handle-sensor-reading application (measurement message)))

(defgeneric sink-network-address(module)
  (:documentation "* Attributes

- module :: an [[application]] or other [[wsn-module]]

* Description

Return the address of sink node for reporting applications. Default
is =sink= however some applications take this as a parameter.")
  (:method((entity application)) 'sink)
  (:method((node node))
    (sink-network-address (submodule node 'application)))
  (:method((instance wsn-module))
    (sink-network-address (node instance))))

(defgeneric parent-network-address(entity)
    (:documentation "* Attributes

- entity :: an [[application]] or other [[wsn-module]]

* Description

Return the address of parent node for aggregation applications. Default
is =parent= however some applications take this as a parameter.")
    (:method((entity application)) 'parent))

(defgeneric sink-p(entity)
  (:documentation "* Attributes

- entity :: an [[application]] or other [[wsn-module]]

* Description

Return true if an application (or router) is on a sink node")
  (:method((application application))
    (eql (sink-network-address application)
         (network-address (node application))))
  (:method((node node))
    (eql (network-address node)
         (sink-network-address  (submodule node 'application))))
  (:method((instance wsn-module))
    (sink-p (node instance))))

(defmethod handle-message((instance application)
                          (message communications-control-message))
  ;; by default applications can ignore control messages
)
