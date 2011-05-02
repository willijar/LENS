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

(defpackage :common
  (:documentation "Some common declarations and interfaces for LENS")
  (:use :cl)
  (:import-from :closer-mop #:class-slots #:slot-definition-name
                #:slot-definition-allocation #:class-direct-slots)
  (:export #:start #:stop #:reset #:busy-p #:*reset-hooks*
           #:while #:until #:filter #:defenumeration
           #:when-bind #:when-bind* #:simulation-condition
           #:uid #:name #:length-bytes
           #:interface #:link #:node #:application
           #:copy #:copy-with-slots #:immutable
           #:trace-accessor #:untrace-accessor
           #:notifier #:add-notification #:delete-notifications
           #:do-notifications
           #:up-p #:mkup #:mkdown
           #:lens-error
           #:make-location #:location #:distance #:+c+ #:+origin+))

(defpackage :scheduler
  (:documentation "LENS Discrete Event Scheduler")
  (:use :cl :common)
  (:import-from :alg
                #:enqueue #:dequeue #:make-binary-heap #:empty-p #:size)
  (:import-from :closer-mop
                #:slot-definition-name #:slot-definition-type #:class-slots)
  (:export #:scheduler #:schedule #:simulation-time #:time-type
           #:status #:with-timers #:timer #:timers #:timeout #:with-delay
           #:event #:handle #:timer-delay #:event-time #:cancel))

(defpackage :math
  (:documentation "Various mathematical functions and classes")
  (:use :cl :common)
  (:import-from :scheduler #:simulation-time #:time-type)
  (:export
   #:record #:average-min-max #:histogram #:inter-arrival-histogram
   #:random-variable #:random-value #:constant #:uniform #:normal
   #:exponential #:pareto #:paretoII #:lognormal))

(defpackage :packet
  (:documentation "Packet implementation")
  (:use :cl :common)
  (:import-from :scheduler #:simulation-time #:time-type)
  (:export #:pdu #:pdus #:layer #:packet #:created
           #:push-pdu #:skip-pdu #:peek-pdu #:pop-pdu #:priority
           #:trace-format #:pdu-trace  #:fid))

(defpackage :trace
   (:documentation "Packet Trace handling")
   (:use :cl :trivial-gray-streams :common)
;   (:import-from :address #:print-ip-format)
   (:import-from :scheduler #:simulation-time #:time-type)
   (:import-from :packet #:packet)
   (:export #:trace-status #:trace-detail #:trace-stream #:time-format
            #:*lens-trace-output* #:trace #:default-trace-detail
            #:trace-enabled-p))

(defpackage :protocol
  (:documentation "Protocol stack layer implementations")
  (:use :cl #:common #:trace)
  (:import-from :packet #:pdu #:layer #:peek-pdu #:pdu-trace)
  (:export #:protocol-number #:protocol #:protocol-condition #:layer #:pdu
           #:send #:receive #:drop #:control-message #:default-trace-detail))

(defpackage :address
  (:documentation "network and hardware addressing")
  (:use :cl :common)
  (:import-from :split-sequence #:split-sequence)
  (:import-from :protocol #:protocol-number)
  (:export  #:hardware-address #:network-address #:network-mask
            #:address= #:address< #:subnet #:address-bytes
            #:broadcast-p #:macaddr #:ipaddr #:ipport
            #:*print-ip-format* #:print-ip-format
            #:src-address #:dst-address
            #:ipaddr-allocator))

(defpackage :node
  (:documentation "Node implementations")
  (:use :cl :common :address)
  (:export #:node #:nodes #:clear-nodes #:interfaces #:applications
           #:interfaces #:ipaddrs #:local-ipaddr-p
           #:add-interface #:find-interface  #:neighbours
           #:receive-packet
           #:lookup-by-port #:bound-protocols #:applications
           #:callbacks #:make-callback #:add-callback #:call-callbacks))

(defpackage :layer1
  (:documentation "Physical layer implementation")
  (:nicknames :physical)
  (:use :cl :common :address)
  (:import-from :node #:node #:interfaces)
  (:import-from :alg  #:make-queue
                #:enqueue #:dequeue #:list-queue #:traverse #:empty-p)
  (:import-from :packet #:packet #:priority #:peek-pdu
                #:trace-format)
  (:import-from :scheduler #:time-type #:simulation-time #:schedule)
  (:import-from :protocol #:send #:receive #:drop)
  (:export #:interface #:interfaces :link #:packet-queue
           #:send-complete
           #:enqueu #:dequeue #:dequeue-if #:empty-p
           #:delete-packets-if #:drop-packet-p #:buffer-available-p
           #:average-queue-length #:reset-average-queue-length
           #:enqueue-count #:drop-count #:egress-filter
           #:length-packets #:limit-packets #:limit-bytes
           #:interface #:peer-interfaces #:default-peer-interface
           #:receive-own-broadcast-p #:utilisation #:network-to-hardware-address
           #:*default-bandwidth* #:*default-delay* #:*default-ber*
           #:delay #:bandwidth #:bit-error-rate #:propagation-speed
           #:drop-tail #:priority-queue
           #:point-to-point #:simplex-p #:unidirectional-p))

 (defpackage :protocol.layer2
   (:documentation "Link layer protocol interface")
   (:nicknames :layer2 :layer.link)
   (:use :cl :address :common :protocol)
   (:shadow #:protocol #:pdu)
   (:import-from :packet #:packet #:pop-pdu #:push-pdu
                 #:trace-format)
   (:import-from :scheduler #:schedule #:simulation-time #:time-type)
   (:import-from :node #:node)
   (:import-from :layer1 #:packet-queue #:enqueue #:dequeue #:empty-p
                 #:send-complete)
   (:export #:protocol #:pdu
            #:IEEE802.3 #:llcsnap #:snap-ethtype #:ieee802.11 #:arp))

 (defpackage :protocol.layer3
   (:documentation "Network Layer protocol interface")
   (:nicknames :layer3 :layer.network)
   (:use :cl :common :address :protocol)
   (:shadow #:protocol #:pdu)
   (:import-from :packet #:push-pdu #:pop-pdu #:peek-pdu
                 #:trace-format)
   (:import-from :alg #:+infinity+ #:dijkstra #:extract-route
                 #:extract-first-hops)
   (:import-from :node #:node #:nodes #:ipaddrs #:neighbours #:interfaces
                 #:find-interface #:local-ipaddr-p)
   (:import-from :layer1 #:interface)
   (:import-from :scheduler  #:simulation-time #:time-type)
   (:export #:protocol #:pdu
            #:register-protocol #:protocols #:find-protocol #:delete-protocol
            #:routing #:getroute #:remroute
            #:initialise-routes #:reinitialise-routes #:default-route
            #:*default-routing* #:topology-changed
            #:routing-manual #:routing-static
            ;; some specific default layer 3 protocols
            #:ipv4))

(defpackage :protocol.layer4
  (:documentation "Transport Layer protocol interface")
  (:nicknames :layer4 :layer.transport)
  (:use :cl :common :address :protocol)
  (:shadow #:protocol #:pdu)
  (:import-from :packet #:packet #:pop-pdu #:push-pdu #:peek-pdu
                #:trace-format #:fid)
  (:import-from :alg #:enqueue #:dequeue #:make-queue #:empty-p)
  (:import-from :node #:node #:interfaces #:find-interface)
  (:import-from :scheduler #:simulation-time #:time-type #:timer #:schedule)
  (:export #:protocol #:pdu #:protocol-dmux
           #:register-protocol #:protocols #:find-protocol #:delete-protocol
           #:bind #:unbind #:binding #:bound-p
           #:peer-address #:peer-port #:local-port #:local-address
           #:src-port #:dst-port
           #:open-connection #:connection-complete
           #:close-connection #:connection-closed #:close-request
           #:connection-from-peer #:connected-p
           #:send #:receive #:sent
           #:seq+ #:seq- #:seq-in-segment #:ack-after-segment
           ;; conditions
           ;; specific default layer 4 protocols
           #:udp-dmux #:tcp-dmux #:udp-header #:tcp-header
           #:udp #:tcp #:icmp #:tcp-tahoe #:tcp-reno #:tcp-newreno))

(defpackage :protocol.layer5
  (:documentation "Application Layer protocol interface")
  (:nicknames :layer5 :data :layer.application)
  (:use :cl :common :address :protocol.layer4)
  (:shadow #:protocol #:pdu)
  (:export #:data #:checksum
           #:data-concatenate #:data-subseq #:protocol #:pdu
           #:application
           #:abr-source #:udp-sink))

(defpackage :lens-user
   (:documentation "LENS User interface")
   (:use :cl :cl-user :address :common :math)
   (:import-from :alg #:enqueue #:dequeue #:empty-p)
   (:import-from :scheduler
                 #:scheduler #:simulation-time #:schedule)
   (:import-from :trace
                 #:*lens-trace-output* #:trace-status #:trace-detail
                 #:time-format #:trace-stream)
   (:import-from :packet #:created #:pdus)
   (:import-from #:protocol #:protocol-number #:layer)
   (:import-from :node
                 #:node #:nodes #:clear-nodes
                 #:interfaces #:applications #:add-interface
                 #:make-callback #:add-callback)
   (:import-from :layer1
                 #:*default-bandwidth* #:*default-delay* #:*default-ber*
                 #:delay #:bandwidth #:bit-error-rate #:peer-interfaces
                 #:default-peer-interface #:receive-own-broadcast-p
                 #:utilisation #:network-to-hardware-address
                 #:point-to-point #:simplex-p #:unidirectional-p
                 #:packet-queue #:drop-tail #:average-queue-length
                 #:enqueue-count #:drop-count #:limit-bytes #:limit-packets
                 #:length-packets #:priority-queue)
   (:import-from :protocol.layer2 #:llcsnap #:IEEE802.3 #:arp #:IEEE802.11)
   (:import-from :protocol.layer3 #:ipv4
                 #:*default-routing* #:default-route #:initialise-routes
                  #:routing-static #:getroute #:remroute
                 #:topology-changed)
   (:import-from :protocol.layer4
                 #:udp #:tcp #:icmp  #:tcp-tahoe #:tcp-reno #:tcp-newreno)
   (:import-from :protocol.layer5 #:abr-source #:udp-sink)
   (:export #:*user-output*))

