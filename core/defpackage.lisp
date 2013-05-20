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

(defpackage :lens
  (:documentation "LENS Simulator Base")
  (:use :closer-common-lisp :closer-common-lisp-user :data-format-validation)
  (:import-from :alg
                 #:enqueue #:dequeue #:make-binary-heap #:empty-p #:size)
  (:shadow duration)
  (:export
   ;; common object
   +c+ *context* simulation-condition while until filter for copy-slots
   named-object owned-object index parent-module full-name full-path
   for-each-child info detailed-info duplicate serialise find-object
   property-union initialized-p finish
   ;; parameters and configuration
   read-configuration format-from-type read-parameter
   finalize-parameters parameter-class parameter-object configure
   ;; simulation kernel
   *simulation* *simulation-init-hooks* *time-format* time-type
   configuration simulation network sim-time-limit cpu-time-limit
   simulation-time schedule scheduled-p cancel stop run-simulation timestamped
   sent-time arrival-time root-event
   ;; signals and listeners
   register-signal signal-id receive-signal entity-with-signals
   listeners may-have-listeners has-listeners emit subscribe unsubscribe
   subscribed-p repair-signal-flags
   ;; components: modules,gates and channels
   gate gate-direction gate-slot input output gate-extend gate-type
   path-start-gate path-end-gate connect disconnect deliver
   connected-p connected-outside-p connect-inside-p  deliver-on-reception-start
   channel transmission-channel ideal-channel channel-result
   process-message nominal-datarate calculate-duration transmission-finish-time
   delay-channel delay disabled-p
   message-sent message-discarded
   message-sent-signal-value  message-sent-signal-value-timestamp
   message-sent-signal-value-message  message-sent-signal-value-result
   component
   module-class module compound-module-class compound-module
   build-gates build-submodules build-connections build-inside
   arrived for-each-gate for-each-submodule for-each-channel
   submodule
   ;; RNG
   uint rand urandom mt-random-state seed rng-map
   uniform exponential normal lognormal truncnormal gamma-d beta erlang-k
   chi-square student-t cauchy triang weibull pareto-shifted
   do-histogram
   ;; model change notifications
   pre-module-add-notification post-module-add-notification
   pre-module-delete-notification post-module-delete-notification
   pre-module-reparent-notification post-module-reparent-notification
   pre-gate-add-notification post-gate-add-notification
   pre-gate-delete-notification post-gate-delete-notification
   pre-gate-vector-resize-notification post-gate-vector-resize-notification
   pre-gate-connect-notification post-gate-connect-notification
   pre-gate-disconnect-notification post-gate-disconnect-notification
   pre-path-create-notification post-path-create-notification
   pre-path-cut-notification post-path-cut-notification
   pre-parameter-change-notification post-parameter-change-notification
   ;; messages and packets
   message creation-time to-gate from-gate to-module from-module time-stamp
   handle-message send-message self-message-p
   packet bit-length byte-length
   duration control-info bit-error-p header-specification
   ;; statistics layer
   *vector-output* *scalar-output*
   result-filter delegates accept report-result
   result-recorder scalar-recorder vector-recorder count-recorder
   last-value-recorder sum-recorder mean-recorder min-recorder max-recorder
   time-average-recorder stddev-recorder

))


