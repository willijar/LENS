;; Core Package Definitions
;; Copyright (C) 2013-2014 Dr. John A.R. Williams

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

;; All the public protocol interfaces should be exported appear here so
;; they can easily be imported into the packages for specific networks.

;;; Code:

(in-package :cl-user)

(defpackage :lens
  (:documentation "LENS Simulator Base")
  (:use :closer-common-lisp :closer-common-lisp-user :data-format-validation)
  (:import-from :alg
                 #:enqueue #:dequeue #:make-binary-heap #:empty-p #:size
                 #:peek)
  (:shadow duration)
  (:export
   ;; common
   #:coord #:make-coord #:coord-x #:coord-y #:coord-z
   #:coord+ #:coord- #:coord* #:coord-op #:distance
   #:while #:until #:filter #:for #:copy-slots #:set-slots #:reinitialise-slots
   #:range-getf #:range-list-p
   #:queue #:make-queue #:enqueue #:dequeue #:peek #:average-queue-time #:size
   #:timestamped-queue #:packet-buffer
   #:buffer-size #:buffer-size-bytes #:empty-p
   #:history-buffer #:duplicate-p
   ;; common object
   #:+c+ #:*context* #:simulation-condition #:name #:owner
   #:named-object #:owned-object #:index #:parent-module #:full-name #:full-path
   #:for-each-child #:info #:detailed-info #:duplicate #:serialise #:find-object
   #:property-union #:initialize #:initialized-p #:finish
   ;; parameters and configuration
   #:read-configuration #:format-from-type #:read-parameter
   #:parameter-class #:parameter-object #:configure #:number-or-expression
   ;; simulation kernel
   #:*simulation* #:*simulation-init-hooks* #:*time-format* #:time-type
   #:configuration #:simulation #:network #:sim-time-limit #:cpu-time-limit
   #:simulation-time #:schedule #:scheduled-p #:cancel #:stop
   #:run-simulations
   #:sent-time #:arrival-time
   #:root-event #:*simulation-trace-stream* #:tracelog
   #:timestamped #:timestamped-time #:timestamped-value #:latency
   ;; signals and listeners
   #:register-signal #:signal-id #:receive-signal #:entity-with-signals
   #:listeners #:may-have-listeners #:has-listeners #:emit
   #:subscribe #:unsubscribe  #:subscribed-p #:repair-signal-flags
   ;; common signals
   #:pre-model-change #:post-model-change #:message-sent #:message-discarded
   #:drop #:buffer-length #:buffer-time
  ;; components: modules,gates and channels
   #:gate #:gate-direction #:gate-slot #:input #:output #:gate-extend
   #:gate-size #:size #:end-module
   #:gate-type #:path-start-gate #:path-end-gate #:connect #:disconnect
   #:deliver #:connected-p #:connected-outside-p #:connect-inside-p
   #:deliver-on-reception-start-p
   #:channel #:transmission-channel #:ideal-channel #:channel-result
   #:process-message #:nominal-datarate #:calculate-duration
   #:transmission-finish-time #:delay-channel #:delay #:disabled-p
   #:message-sent-signal-value #:message-sent-signal-value-timestamp
   #:message-sent-signal-value-message #:message-sent-signal-value-result
   #:component #:module-class #:module #:compound-module-class #:compound-module
   #:build-gates #:build-submodules #:build-connections #:build-inside
   #:arrived #:for-each-gate #:for-each-submodule #:for-each-channel
   #:submodule #:schedule-at #:send #:send-direct #:<= #:=> #:<=>
   ;; RNG
   #:uint #:rand #:urandom #:mt-random-state #:seed #:rng-map
   #:uniform #:exponential #:normal #:lognormal #:truncnormal #:gamma-d #:beta
   #:erlang-k
   #:chi-square #:student-t #:cauchy #:triang #:weibull #:pareto-shifted
   #:intuniform #:bernoulli #:binomial #:geometric #:negbinomial #:poisson
   #:do-histogram
   ;; model change notifications
   #:pre-module-add-notification #:post-module-add-notification
   #:pre-module-delete-notification #:post-module-delete-notification
   #:pre-module-reparent-notification #:post-module-reparent-notification
   #:pre-gate-add-notification #:post-gate-add-notification
   #:pre-gate-delete-notification #:post-gate-delete-notification
   #:pre-gate-vector-resize-notification #:post-gate-vector-resize-notification
   #:pre-gate-connect-notification #:post-gate-connect-notification
   #:pre-gate-disconnect-notification #:post-gate-disconnect-notification
   #:pre-path-create-notification #:post-path-create-notification
   #:pre-path-cut-notification #:post-path-cut-notification
   #:pre-parameter-change-notification #:post-parameter-change-notification
   ;; messages and packets
   #:message #:creation-time #:to-gate #:from-gate #:to-module #:from-module
   #:timestamp #: #:handle-message #:send-message #:self-message-p
   #:packet #:bit-length #:byte-length
   #:duration #:control-info #:bit-error-p #:header-specification
   #:unknown-message #:encapsulate #:decapsulate
   ;; statistics layer
   #:define-statistic-filter #:define-result-recorder #:record #:report
   #:title
   #:sum #:mean #:constant0 #:constant1 #:last-value #:stddev #:timeavg
   #:time-average-recorder #:stddev-recorder #:histogram
   #:indexed-count-recorder #:indexed-count #:accumulated-time
   #:probability-density-function #:cumulative-density-function
))


