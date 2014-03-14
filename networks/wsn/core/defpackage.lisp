;; Core Package Definitions for Wireless Sensor Network Simulations
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

;;; Commentary:

;; All the public protocol interfaces should appear here.

;;; Code:

(in-package :cl-user)

(defpackage :lens.wsn
  (:documentation "LENS Wireless Sensor Networks")
  (:use :cl :cl-user :lens)
  (:shadowing-import-from :lens #:duration)
  (:export
   ;; signals
   #:node-move #:out-of-energy #:node-startup #:node-shutdown #:out-of-memory
   #:application-receive #:application-send #:power-change #:energy-consumed
   ;; common
   #:startup #:shutdown #:node #:nodeid #:wsn-module #:disabled-p
   #:get-simulation-time #:get-clock
   #:set-timer #:timer-message #:timer #:cancel-timer #:handle-timer
   #:buffer #:buffer-size #:packet-history #:last-sequence-number
   ;; application
   #:app-net-control-info #:application-packet
   #:application #:rssi #:lqi #:source #:destination
   #:applicationid
   #:sequence-number #:header-overhead #:payload-overhead
   #:next-sequence-number #:sensor-request #:handle-sensor-reading
   #:to-network #:packet-size #:payload #:priority
   ;;communications
   #:communications :network #:mac #:radio
   #:network-control-message #:mac-control-message #:radio-control-message
   #:network-control-command #:mac-control-command #:radio-control-command
   #:wsn-packet #:command #:argument #:comms-module
   #:packet-history #:state #:set-state #:handle-control-command
   #:print-state-transitions
   ;; network layer
   #:net-mac-control-info #:next-hop #:last-hop #:routing-packet
   #:routing #:max-net-frame-size #:to-mac #:resolve-network-address
   #:sink-p #:broadcast-network-address
   #:sink-network-address #:parent-network-address
   ;; MAC layer
   #:mac-radio-control-info #:mac-packet #:mac #:mac-packet-breakdown
   #:to-radio #:to-network #:broadcast-mac-address
   #:max-mac-frame-size #:mac-address #:phy-delay-for-valid-cs
   #:attempt-tx
   ;; RADIO layer
   #:sleep #:rx #:tx #:tx-time #:radio-control-command-name
   #:collision-model #:additive-interference-model
   #:no-interference-no-collisions #:simple-collision-model
   #:set-state #:set-mode #:set-tx-output #:set-sleep-level
   #:set-carrier-frequency
   #:set-cca-threshold #:set-cs-interrupt
   #:channel-clear-status
   #:set-encoding #:read-rssi
   #:data-rate #:bits-per-symbol #:symbol-length #:transition-delay
   #:cs-not-valid #:cs-not-valid-yet #:busy #:clear
   #:carrier-sense-interrupt
   ;; mobility
   #:mobility #:location #:cell #:theta #:phi
   ;; top level network
   #:SensorNetwork #:field #:num-nodes #:deployment #:physical-processes
   #:nodes #:wireless-channel
   ;; node
   #:num-sensors #:network-address #:sensors
   ;; physical process
   #:physical-process #:description #:measure
   ;; resources
   #:resources #:draw-power #:ram-store #:ram-size #:flash-size #:clock-drift
   #:get-simulation-time #:get-clock
   ;; protocol specific -- add in public protocol interfaces here
   ;; so available in ini files without package suffix
   #:bypass-mac
   #:tuneable-mac #:sleep-interval #:constant #:multiplying #:exponential
   #:tmac
   #:mac802.15.4
   #:bypass-routing
   #:multipath-rings-routing
   #:leach-routing
))

(pushnew :castelia-compatability *features*)