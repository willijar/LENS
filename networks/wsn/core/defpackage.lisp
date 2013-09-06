;; Core Package Definitions
;; Copyright (C) 2007 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;; This file is part of Lisp Educational Network Simulator (LENS)

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

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
   #:to-network #:packet-size #:payload
   ;;communications
   #:communications :network #:mac #:radio
   #:network-control-message #:mac-control-message #:radio-control-message
   #:network-control-command #:mac-control-command #:radio-control-command
   #:wsn-packet #:command #:argument #:comms-module
   #:packet-history #:state #:set-state
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
   #:set-state #:set-mode #:set-tx-output #:set-sleep-level #:set-carrier-freq
   #:set-cca-threshold #:set-cs-interrupt-on #:set-cs-interrupt-off
   #:channel-clear-status
   #:set-encoding #:read-rssi
   #:data-rate #:bits-per-symbol #:symbol-length #:transition-delay
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
    #:get-simulation-time #:get-clock))

(pushnew :castelia-compatability *features*)