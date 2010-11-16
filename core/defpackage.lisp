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
  (:export #:+c+ #:start #:stop #:reset #:busy-p #:*reset-hooks*
           #:while #:until #:filter #:defenumeration
           #:when-bind #:when-bind*
           #:uid #:counter #:name #:node
           #:octet #:word #:counter #:seq #:fid
           #:interface #:link #:node #:application
           #:copy #:copy-with-slots
           #:notifier #:add-notify #:do-notifications))

(defpackage :scheduler
  (:documentation "LENS Discrete Event Scheduler")
  (:use :cl :common)
  (:import-from :alg #:enqueue #:dequeue #:make-binary-heap
                #:empty-p #:size)
  (:import-from :closer-mop #:slot-definition-name #:slot-definition-type
                #:class-direct-slots)
  (:export #:scheduler #:schedule #:simulation-time #:time-type
           #:status #:with-timers #:timer #:timeout #:with-delay))

(defpackage :math
  (:documentation "Various mathematical functions and classes")
  (:use :cl)
  (:export
   #:random-variable #:random-value #:constant #:uniform #:normal
   #:exponential #:pareto #:paretoII #:lognormal))

(defpackage :packet
  (:documentation "Packet implementation")
  (:use :cl :common)
  (:import-from :scheduler #:simulation-time #:time-type)
  (:export #:pdu #:pdus #:length-bytes #:layer #:packet #:created #:routing
           #:push-pdu #:skip-pdu #:peek-pdu #:pop-pdu #:priority))

(defpackage :trace
   (:documentation "Packet Trace handling")
   (:use :cl :trivial-gray-streams :common)
;   (:import-from :address #:print-ip-format)
   (:import-from :scheduler #:simulation-time #:time-type)
   (:export #:trace-status #:trace-detail #:trace-stream #:time-format
            #:*lens-trace-output* #:trace #:write-pdu-slots #:pdu-trace))

(defpackage :protocol
  (:documentation "Protocol stack layer implementations")
  (:use :cl #:common #:trace)
  (:import-from :packet #:pdu #:layer #:length-bytes)
  (:export #:protocol-number #:protocol #:layer #:pdu #:length-bytes
           #:send #:receive #:drop))

(defpackage :address
  (:documentation "network and hardware addressing")
  (:use :cl)
  (:import-from :split-sequence #:split-sequence)
  (:import-from :protocol #:length-bytes #:protocol-number)
  (:export  #:address #:hardware-address #:network-address #:network-mask
            #:address= #:broadcast-p
            #:macaddr #:ipaddr #:ipport #:ipmask
            #:*print-ip-format* #:print-ip-form
            #:ipaddr #:ipport #:ipmask #:macaddr
            #:src-address #:dest-address
            #:address-condition #:address-out-of-range
            #:subnet #:ipaddr-allocator #:local-network-address-p))

 (defpackage :node
  (:documentation "Node implementations")
  (:use :cl :common :address)
  (:export #:node #:nodes #:clear-nodes
           #:interfaces #:ipaddrs #:bind #:unbind #:local-ipaddr-p
           #:add-interface #:find-interface
           #:neighbours
           #:receive-packet #:callbacks #:call-callbacks #:make-callback
           #:lookup-by-port #:bound-protocols #:applications
           #:make-location #:location #:distance))

(defpackage :layer1
  (:documentation "Physical layer implementation")
  (:nicknames :physical)
  (:use :cl :common :address)
  (:import-from :node #:node)
  (:import-from :alg  #:make-queue
                #:enqueue #:dequeue #:list-queue #:traverse #:empty-p)
  (:import-from :packet #:length-bytes #:priority)
  (:import-from :scheduler #:time-type #:simulation-time #:schedule)
  (:import-from :protocol #:send #:receive #:drop)
  (:export #:interface #:packet-queue
           #:enqueu #:dequeue #:dequeue-if #:empty-p
           #:delete-packets-if #:drop-packet-p #:buffer-available-p
           #:average-queue-length #:reset-average-queue-length
           #:enqueue-count #:drop-count #:egress-filter
           #:length-packets #:length-bytes #:limit-packets #:limit-bytes
           #:drop-tail #:interface #:peer-interfaces))

 (defpackage :protocol.layer2
   (:documentation "Link layer protocol interface")
   (:nicknames :layer2 :layer.link)
   (:use :cl :address :common :protocol)
   (:shadow #:protocol #:pdu)
   (:import-from :node #:node)
   (:export #:protocol #:pdu #:ieee802.3 #:llcsnap #:snap-ethtype #:ieee802.11 #:arp))

 (defpackage :protocol.layer3
   (:documentation "Network Layer protocol interface")
   (:nicknames :layer3 :layer.network)
   (:use :cl :common :address :protocol)
   (:shadow #:protocol #:pdu)
   (:import-from :node #:node #:nodes #:ipaddrs #:neighbours #:find-interface)
   (:import-from :layer1 #:interface)
   (:export #:register-protocol #:protocols #:find-protocol #:delete-protocol
            #:routing #:lookup-route
            #:find-route #:add-route #:rem-route
            #:initialise-routes #:reinitialise-routes #:default-route
            #:*default-routing* #:topology-changed
            #:routing-manual #:routing-static
            #:protocol #:pdu
            ;; some specific default layer 3 protocols
            #:ipv4))

(defpackage :protocol.layer4
  (:documentation "Transport Layer protocol interface")
  (:nicknames :layer4 :layer.transport)
  (:use :cl :common :address :protocol)
  (:shadow #:protocol #:pdu)
  (:import-from :alg
                #:enqueue #:dequeue #:make-queue
                #:empty-p)
  (:export
            #:register-protocol #:protocols #:find-protocol #:delete-protocol
            #:peer-address #:peer-port #:local-port #:local-address
            #:protocol #:pdu
           ;; #:receive
           ;; #:notification #:request-notification #:cancel-notification
           ;; #:ttl #:fid #:tos #:interface
           ;; #:send #:connect #:close-connection #:bind #:unbind #:make-packet
           ;; specific default layer 4 protocols
           #:udp #:tcp #:icmp #:demux #:tcp-tahoe #:tcp-reno #:tcp-newreno))

(defpackage :protocol.layer5
  (:documentation "Application Layer protocol interface")
  (:nicknames :layer5 :data :layer.application)
  (:use :cl :common :address :protocol)
  (:shadow #:protocol #:pdu)
  (:export #:data #:contents #:msg-size #:response-size #:checksum
           #:copy-from-offset #:size-from-seq #:copy-from-seq
           #:add-data #:remove-data #:protocol #:pdu))

;; #:link #:local-interface #:peer-interfaces #:peer-node-p
;;            #:default-peer-interface #:ip-to-mac #:find-interface #:busy-p
;;            #:bandwidth #:delay #:bit-error-rate  #:jitter #:weight
;;            #:notifications #:*default-link* #:*default-bandwidth*
;;            #:*default-delay* #:*default-jitter*
;;            #:transmit #:transmit-complete #:rx-own-broadcast
;;            #:transmit-helper
;;            #:point-to-point #:busy-p #:peer-node-ipaddr
;;            #:make-new-interface))



;; ;; each protocol layer has its own pdu and protocol class
;; ;; as it should be made explicit in other packages which
;; ;; layer is being refered to. They all have different APIs


;; (defpackage :application
;;    (:documentation "Application Implementations")
;;    (:use :cl :common :address :protocol.layer4 :lens.math)
;;    (:import-from :protocol #:size #:layer)
;;    (:import-from :node #:node)
;;    (:import-from :packet #:push-pdu #:pop-pdu #:peek-pdu)
;;    (:import-from :scheduler
;;                  #:start #:stop #:cancel #:schedule #:handle
;;                  #:simulation-time #:time-type)
;;    (:export #:application #:start #:stop #:receive #:sent #:protocol
;;             #:close-request #:closed #:connection-complete
;;             #:connection-failed #:server-connection-complete
;;             #:connection-from-peer #:data #:checksum
;;             #:cbr-source #:udp-sink))

;; (defpackage :link
;;   (:documentation "Node link implementation")
;;   (:use :cl :common :address :lens.math)
;;   (:import-from :node #:node)
;;   (:import-from :queues #:queue #:insert #:extract-head #:empty-p)
;;   (:import-from :protocol.layer2 #:busy-p)
;;   (:import-from :scheduler
;;                 #:time-type #:simulation-time #:schedule #:schedule-timer)
;;   (:import-from :packet #:notification #:size)
;;   (:export #:link #:local-interface #:peer-interfaces #:peer-node-p
;;            #:default-peer-interface #:ip-to-mac #:find-interface #:busy-p
;;            #:bandwidth #:delay #:bit-error-rate  #:jitter #:weight
;;            #:notifications #:*default-link* #:*default-bandwidth*
;;            #:*default-delay* #:*default-jitter*
;;            #:transmit #:transmit-complete #:rx-own-broadcast
;;            #:transmit-helper
;;            #:point-to-point #:busy-p #:peer-node-ipaddr
;;            #:make-new-interface))

;; (defpackage :interface
;;   (:documentation "Node interface implementation")
;;   (:use :cl :address :common)
;;   (:import-from :queues #:queue #:insert #:extract-head #:empty-p)
;;   (:import-from :node #:node #:local-ipaddr-p)
;;   (:import-from :link
;;                 #:link #:bandwidth #:ip-to-mac #:default-peer-interface
;;                 #:peer-node-p #:make-new-interface #:weight
;;                 #:peer-interfaces #:peer-node-ipaddr)
;;   (:import-from :scheduler
;;                 #:schedule #:simulation-time #:cancel-all-timers
;;                 #:schedule-timer #:time-type #:timers-manager)
;;   (:import-from :packet #:packet #:size #:notification #:push-pdu)
;;   (:import-from :trace #:write-trace)
;;   (:import-from :protocol.layer3 #:arp)
;;   (:export #:interface #:receive #:neighbours
;;            #:peer-interfaces #:peer-node-p #:queue
;;            #:packet-queue #:enque #:deque #:peek-deque #:deque-if
;;            #:delete-packets-if #:length-packets #:length-bytes
;;            #:buffer-available-p #:egress-filter
;;            #:limit-packets #:limit-bytes #:average-queue-length
;;            #:reset-average-queue-length #:queueing-delay
;;            #:drop-tail #:add-notify #:cancel-notify
;;            #:peers #:peer-p #:default-peer-interface #:ip-to-mac
;;            #:peer-node-ipaddr #:local-ipaddr-p #:node #:send #:link))


;; ;; backward symbol dependencies
;; (in-package :protocol)
;; (import '(packet:packet node:node))
;; (in-package :protocol.layer4)
;; (import '(interface:buffer-available-p packet:packet))
;; (in-package :trace)
;; (import '(protocol:layer))
;; (in-package :node)
;; (import '(interface:interface interface:peer-node-p
;;           routing:add-route routing:rem-route routing:find-route
;;           routing:initialise-routes routing:reinitialise-routes))

;; ;; the user interface package
;; (in-package :cl-user)

;; (defpackage :lens-user
;;    (:documentation "LENS User interface")
;;    (:use :cl :cl-user :address :common :lens.math)
;;    (:import-from :scheduler
;;                  #:scheduler #:simulation-time #:schedule #:cancel
;;                  #:start #:stop #:reset)
;;    (:import-from :trace
;;                  #:*lens-trace-output* #:trace-status #:trace-detail
;;                  #:time-format)
;;    (:import-from #:protocol
;;                  #:layer #:src-address #:dst-address
;;                  #:*common-protocol-graph* #:size #:insert-protocol)
;;    (:import-from :protocol.layer2 #:IEEE802.3)
;;    (:import-from :protocol.layer3 #:arp #:ipv4)
;;    (:import-from :protocol.layer4
;;                  #:udp #:icmp  #:tcp-tahoe #:tcp-reno #:tcp-newreno)
;;    (:import-from :application #:cbr-source #:udp-sink)
;;    (:import-from :lens.math
;;                  #:time-value #:average-min-max
;;                  #:histogram #:inter-arrival-histogram)
;;    (:import-from :node
;;                  #:callbacks #:add-interface
;;                  #:make-callback #:node #:nodes #:clear-nodes
;;                  #:interfaces #:bind #:unbind)
;;    (:import-from :link
;;                  #:*default-link* #:*default-bandwidth* #:*default-delay*
;;                  #:*default-jitter* #:delay #:bandwidth #:jitter
;;                  #:ip-to-mac #:bit-error-rate #:local-interface
;;                  #:point-to-point)
;;    (:import-from :packet #:size)
;;    (:import-from :interface
;;                  #:packet-queue #:interface #:length-packets #:length-bytes
;;                  #:limit-packets #:limit-bytes #:egress-filter
;;                  #:reset-average-queue-length #:average-queue-length
;;                  #:queueing-delay #:drop-tail #:make-new-interface)
;;    (:import-from :routing
;;                  #:default-route #:*default-routing* #:routing-manual
;;                  #:routing-static #:find-route #:add-route #:rem-route
;;                  #:topology-changed))

