;; Constant or average bit rate source application
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

(defclass abr-source(application scheduler:event)
  ((protocol-type :type symbol :reader protocol-type :initform 'layer4:udp
                  :documentation "Name of protocol to use")
   (peer-address :type network-address
                 :initarg :peer-address :reader peer-address
                 :documentation "Destination address for data")
   (peer-port :type ipport :initarg :peer-port :reader peer-port
              :documentation "Destination port for packets")
   (protocol :type layer4:protocol :reader protocol)
   (rate  :initarg :rate :initform 500000 :accessor rate
          :documentation "Rate at which data is generated - may be a
random variable or an integer or zero for as fast as possible")
   (pkt-size  :initarg :pkt-size :initform 512 :type integer :accessor pkt-size
              :documentation "Size of packets"))
  (:documentation "A constant or average bit rate source transmitting
either at rate (which may be a random variable or a constant) or as
fast as possible if rate is 0"))

(defmethod sent((app abr-source) n socket)
  "Send a packet on every notification if rate is 0"
  (when (equal (rate app) 0) (send socket (pkt-size app) app)))

(defmethod scheduler:handle((app abr-source))
  "Send a packet and reschedule"
  (with-slots(rate protocol pkt-size) app
    (send protocol (make-instance 'data :length-bytes pkt-size) app)
    (scheduler:schedule (/ (* pkt-size 8) (math:random-value rate)) app)))

(defmethod start((app abr-source))
  (unless (slot-boundp app 'protocol)
    (make-instance (protocol-type app)
                         :peer-address (peer-address app)
                         :peer-port (peer-port app)
                         :node (node app)
                         :application app)))

(defmethod connection-complete((app abr-source) layer4 &key failure)
  (unless failure
    (with-slots(protocol rate pkt-size) app
      (setf protocol layer4)
      (if (equal rate 0)
          (send protocol (make-instance 'data :length-bytes pkt-size) app)
          (scheduler:handle app)))))

(defmethod stop((app abr-source) &key &allow-other-keys)
  (when (slot-boundp app 'protocol)
    (close-connection (protocol app))
    (slot-makunbound app 'protocol)))

(defmethod reset((app abr-source))
  (stop app))





