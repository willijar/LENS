;; Constant or average bit rate source generator application
;; Copyright (C) 2010 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;; This file is part of Lisp Educational Network Simulator (LENS)

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:

;;

;;; Code:

(in-package :protocol.layer5)

(defclass abr-source(client-application event)
  ((rate  :initarg :rate :initform 500000 :accessor rate
          :documentation "Rate at which data is generated - may be a
random variable or an integer or zero for as fast as possible")
   (pkt-size  :initarg :pkt-size :initform 512 :type integer :accessor pkt-size
              :documentation "Size of packets"))
  (:default-initargs :protocol (make-instance 'udp))
  (:documentation "A constant or average bit rate source transmitting
either at rate (which may be a random variable or a constant) or as
fast as possible if rate is 0"))

(defmethod sent((app abr-source) n socket)
  "Send a packet on every notification if rate is 0"
  (call-next-method)
  (when (zerop (rate app))
    (send socket (make-instance 'data :length-bytes (pkt-size app)) app)))

(defmethod handle((app abr-source))
  "Send a packet and reschedule"
  (let ((n (pkt-size app)))
    (send (protocol app) (make-instance 'data :length-bytes n) app)
    (schedule (/ (* n 8) (math:random-value (rate app))) app)))

(defmethod start((app abr-source))
  (open-connection (peer-address app)  (peer-port app) (protocol app)))

(defmethod connection-complete((app abr-source) layer4)
  (if (zerop (rate app))
      (send layer4 (make-instance 'data :length-bytes (pkt-size app)) app)
      (handle app)))
