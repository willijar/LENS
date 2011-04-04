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
  ((protocol :type udp :accessor protocol
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
    :type scheduler:time-type :initarg :stats-update-interval
    :initform 1.0 :accessor stats-update-interval
    :documentation "interval over which statistics are recorded")
   (first-packet-rx
    :type scheduler:time-type :initform 0
    :documentation "Time first packet is received. Used to calculate
overall bytes/sec data rx.")
   (last-packet-rx
    :initform 0 :type  scheduler:time-type
    :documentation "Time last packet is received. Used to calculate jitter.")
   (bytes-rx  :initform 0 :type counter :reader bytes-rx
              :documentation "total number of bytes received")
   (packets-rx :initform 0 :type counter :reader packets-rx
               :documentation "total number of packets received")
   (packets-lost :initform 0 :type counter :reader packets-lost
                 :documentation "total number of packets lost")
   (next-sequence-rx :initform 0 :type seq
                     :documentation "Sequence number of next expected packet")
   (last-log-time :initform 0 :type  scheduler:time-type)
   (last-packets-rx :initform 0 :type counter)
   (last-bytes-rx :initform 0 :type counter)
   (last-packets-lost :initform 0 :type counter)
   (iat-statistics :initarg :iat-statistics :initform nil
                   :accessor iat-statistics
                   :documentation "Inter-arrival time distribution statistics")
   (last-transit :initform 0 :type  scheduler:time-type)
   (jitter :initform 0 :type real))
  (:documentation "Listen and record statistics of incomming packets
on a specific port"))

(defmethod initialize-instance
    ((app udp-sink)
     &key local-port
     (protocol-type 'layer4:udp)
     &allow-other-keys)
  (setf (slot-value app 'protocol)
        (make-instance protocol-type
                       :local-port local-port
                       :node (node app) :application app)))

(defmethod reset((app udp-sink))
  (dolist(s '(loss-statistics bandwidth-statistics
              delay-statistics iat-statistics))
    (when (slot-value app s) (reset (slot-value app s))))
  (dolist (slot
            '(first-packet-rx last-packet-rx bytes-rx packets-rx packets-lost
              next-sequence-rx last-log-time last-packets-rx last-packets-lost
              last-transit jitter))
    (setf (slot-value app slot) 0)))

(defmethod receive((application udp-sink) data protocol
                   &key sequence-number packet-created &allow-other-keys)
  (let ((now (scheduler:simulation-time)))
    (with-slots(first-packet-rx last-packet-rx iat-statistics
                last-transit jitter bytes-rx packets-rx packets-lost
                next-sequence-rx) application
      (when (zerop first-packet-rx) (setf first-packet-rx now))
      ;; do iat history
      (when iat-statistics
        (math:record (- now last-packet-rx) iat-statistics :time now))
      (setf last-packet-rx now)
      ;; calculate jitter as per RFC189
      (let ((this-transit (- now packet-created)))
        (unless (zerop last-transit)
          (let ((diff (abs (- this-transit last-transit))))
            (incf jitter (/ (- diff jitter) 16.0))))
        (setf last-transit this-transit)
        ;; record delay if requested
        (when (delay-statistics application)
          (math:record this-transit (delay-statistics application) :time now)))
      ;; do counts
      (incf bytes-rx (length-bytes data))
      (when (>= sequence-number next-sequence-rx)
        (incf packets-rx)
        (unless (zerop next-sequence-rx)
          (incf packets-lost (- sequence-number next-sequence-rx)))
        (setf next-sequence-rx (1+ sequence-number))))))

(defmethod start((app udp-sink))
  (when (or (loss-statistics app) (bandwidth-statistics app))
    ;; need to set up logging event
    (scheduler:schedule (stats-update-interval app) app)))

(defmethod scheduler:handle((app udp-sink))
  (with-slots(bytes-rx packets-rx packets-lost loss-statistics
             bandwidth-statistics last-bytes-rx last-packets-rx
             last-packets-lost last-log-time) app
    (when loss-statistics
      (let*((this-rx (- packets-rx last-packets-rx))
            (this-lost (- packets-lost last-packets-lost))
            (loss-rate (let ((tot (+ this-rx this-lost)))
                         (if (zerop tot ) 0 (/ this-lost tot)))))
        (math:record loss-rate loss-statistics)))
    (when bandwidth-statistics
      (let ((this-bytes-rx (- bytes-rx last-bytes-rx)))
        (math:record (/ (* 8 this-bytes-rx) (- (scheduler:simulation-time)
                                          last-log-time))
                bandwidth-statistics)))
    (setf last-packets-rx packets-rx
          last-bytes-rx bytes-rx
          last-packets-lost packets-lost
          last-log-time (scheduler:simulation-time)))
  (scheduler:schedule (stats-update-interval app) app))






