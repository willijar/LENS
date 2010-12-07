;; $Id$
;; udp sink application
;; Copyright (C) 2006 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;; This file is part of Lisp Educational Network Simulator (LENS)

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:

;;

;;; Code:

(in-package :protocol.layer5)

(defclass udp-sink(application)
  ((node :initarg :node :type node :reader node
         :documentation "Attached node.")
   (local-port :initarg :local-port :type ipport :reader local-port
               :documentation "Port number to bind to")
   (protocol :type udp :accessor protocol
             :documentation "The udp protocol instance")
   (loss-statistics :initarg :loss-statistics :initform nil
                    :accessor loss-statistics
                    :documentation "Loss Statistics collection object")
   (bandwidth-statistics :initarg :bandwidth-statistics :initform nil
                          :accessor bandwidth-statistics
                          :documentation "Delay statistics object")
   (delay-statistics
    :initarg :delay-statistics :initform nil :accessor delay-statistics
    :documentation "Statistics collection object for delays")
   (stats-update-interval
    :type type-type :initarg :stats-update-interval
    :initform 1.0 :accessor stats-update-interval
    :documentation "interval over which statistics are recorded")
   (first-packet-rx
    :type time-type :initform 0
    :documentation "Time first packet is received. Used to calculate
overall bytes/sec data rx.")
   (last-packet-rx
    :initform 0 :type time-type
    :documentation "Time last packet is received. Used to calculate jitter.")
   (bytes-rx  :initform 0 :type counter :reader bytes-rx
              :documentation "total number of bytes received")
   (packets-rx :initform 0 :type counter :reader packets-rx
               :documentation "total number of packets received")
   (packets-lost :initform 0 :type counter :reader packets-lost
                 :documentation "total number of packets lost")
   (next-sequence-rx :initform 0 :type seq
                     :documentation "Sequence number of next expected packet")
   (last-log-time :initform 0 :type time-type)
   (last-packets-rx :initform 0 :type counter)
   (last-bytes-rx :initform 0 :type counter)
   (last-packets-lost :initform 0 :type counter)
   (iat-statistics :initarg :iat-statistics :initform nil
                   :accessor iat-statistics
                   :documentation "Inter-arrival time distribution statistics")
   (last-transit :initform 0 :type time-type)
   (jitter :initform 0 :type real))
  (:documentation "Listen and record statistics of incomming packets
on a specific port"))

(defmethod reset((app udp-sink))
  (dolist(s '(loss-statistics bandwidth-statistics
              delay-statistics iat-statistics))
    (when (slot-value app s) (reset (slot-value app s))))
  (shared-initialize
   app
   '(first-packet-rx last-packet-rx bytes-rx packets-rx packets-lost
     next-sequence-rx last-log-time last-packets-rx last-packets-lost
     last-transit jitter)))

(defmethod receive((application udp-sink) packet protocol
                   &optional sequence-number)
  (let ((now (simulation-time)))
    (with-slots(first-packet-rx last-packet-rx iat-statistics
                last-transit jitter bytes-rx packets-rx packets-lost
                next-sequence-rx) application
      (when (zerop first-packet-rx) (setf first-packet-rx now))
      ;; do iat history
      (when iat-statistics
        (record (- now last-packet-rx) iat-statistics))
      (setf last-packet-rx now)
      ;; calculate jitter as per RFC189
      (let ((this-transit (- now (packet:created packet))))
        (unless (zerop last-transit)
          (let ((diff (abs (- this-transit last-transit))))
            (incf jitter (/ (- diff jitter) 16.0))))
        (setf last-transit this-transit)
        ;; record delay if requested
        (when (delay-statistics application)
          (record this-transit (delay-statistics application))))
      ;; do counts
      (when-bind(d (pop-pdu packet))
                (incf bytes-rx (size d)))
      (when (>= sequence-number next-sequence-rx)
        (incf packets-rx)
        (unless (zerop next-sequence-rx)
          (incf packets-lost (- sequence-number next-sequence-rx)))
        (setf next-sequence-rx (1+ sequence-number))))))

(defmethod start((app udp-sink))
  (unless (slot-boundp app 'protocol)
    ;; need to allocate udp and bind endpoint
    (let ((udp (make-instance 'udp :node (node app) :application app)))
      (setf (protocol app) udp)
      (bind udp :port (local-port app))))
  (when (or (loss-statistics app) (bandwidth-statistics app))
    ;; need to set up logging event
    (schedule (stats-update-interval app) app)))

(defmethod stop((app udp-sink) &key abort)
  (declare (ignore abort))
  (cancel app)
  (unbind (protocol app)))

(defmethod handle((app udp-sink))
  (with-slots(bytes-rx packets-rx packets-lost loss-statistics
             bandwidth-statistics last-bytes-rx last-packets-rx
             last-packets-lost last-log-time) app
    (when loss-statistics
      (let*((this-rx (- packets-rx last-packets-rx))
            (this-lost (- packets-lost last-packets-lost))
            (loss-rate (let ((tot (+ this-rx this-lost)))
                         (if (zerop tot ) 0 (/ this-lost tot)))))
        (record loss-rate loss-statistics)))
    (when bandwidth-statistics
      (let ((this-bytes-rx (- bytes-rx last-bytes-rx)))
        (record (/ (* 8 this-bytes-rx) (- (simulation-time) last-log-time))
                bandwidth-statistics)))
    (setf last-packets-rx packets-rx
          last-bytes-rx bytes-rx
          last-packets-lost packets-lost
          last-log-time (simulation-time)))
  (schedule (stats-update-interval app) app))






