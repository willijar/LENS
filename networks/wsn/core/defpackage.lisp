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
   #:application-receive #:application-send
   ;; common
   #:startup #:shutdown #:node #:wsn-module #:disabled-p
   #:get-simulation-time #:set-timer
   ;; application
   #:app-net-control-info #:application-packet
   #:application #:RSSI #:LQI #:source #:destination #:applicationid
   #:sequence-number #:header-overhead #:payload-overhead
   #:next-sequence-number #:sensor-request #:handle-sensor-reading
   #:toNetwork #:packet-size
   ;;communications
   #:communications :network #:mac #:radio
   #:network-control-message #:mac-control-message #:radio-control-message
   #:network-control-command #:mac-control-command #:radio-control-command
   #:command #:argument
   ;; network layer
   #:net-mac-control-info #:next-hop #:last-hop #:routing-packet
   #:routing #:toMacLayer
   ;; MAC layer
   #:mac-radio-control-info #:mac-packet #:mac #:toRadioLayer
   ;; RADIO layer

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
    #:resources #:draw-power #:ram-store
   #:ram-size #:flash-size #:clock-drift))

