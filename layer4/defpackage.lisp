;; Interdependant Protocol package definitions
;; Copyright 2007 Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>

;; This file is part of Lisp Educational Network Simulator (LENS)

;; This is free software released under the GNU General Public License (GPL)
;; See the LICENSE file provided or <http://www.gnu.org/licenses>

;;; Commentary:

;;

;;; Code:

(defpackage :protocol.ipv4
  (:nicknames :ipv4)
  (:use :cl :common :address :protocol.layer3)
  (:import-from :node #:node #:local-ipaddr-p)
  (:import-from :routing #:find-route #:next-hop)
  (:import-from :packet #:peek-pdu #:push-pdu #:size)
  (:import-from :trace #:default-trace-detail)
  (:import-from :protocol
                #:*common-protocol-graph* #:insert-protocol #:remove-protocol
                #:write-trace #:write-pdu-slots #:layer #:pdu-trace)
  (:export #:ipv4 #:ipv4-demux
           #:ipv4-header #:version #:service-type #:total-length
           #:identification #:flags #:fragment-offset
           #:ttl #:src-address #:dst-address #:src-port #:dst-port))

(defpackage :protocol.icmp
  (:nicknames :icmp)
  (:use :cl :common :address :protocol.layer4)
  (:import-from :protocol
                #:*common-protocol-graph*
                #:insert-protocol #:remove-protocol #:find-protocol
                #:write-trace #:write-pdu-slots #:layer #:pdu-trace)
  (:import-from :ipv4 #:ipv4 #:ipv4-header #:ipv4-demux #:src-port #:dst-port
                #:src-address #:dst-address)
  (:import-from :packet #:peek-pdu #:push-pdu #:packet #:size)
  (:import-from :node #:node #:lookup-by-port)
  (:import-from :scheduler #:simulation-time #:time-type)
  (:import-from :trace
                #:eol #:pdu-trace #:write-trace #:write-pdu-slots
                #:default-trace-detail)
  (:export #:icmp #:time-exceeded #:destination-unreachable #:add-callback
           #:echo #:timestamp))
