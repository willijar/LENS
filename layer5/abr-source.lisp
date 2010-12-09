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

(defclass abr-source(application event)
  ((peer-address :type network-address :initarg :peer-address
                 :accessor peer-address)
   (peer-port :type ipport :initarg :peer-port
              :accessor peer-port)
   (layer4:protocol :type layer4:protocol)
   (layer4:socket :type layer4:socket
           :documentaton "Connected socket managing transmissions")
   (rate  :initarg :rate :initform 500000 :accessor rate
          :documentation "Rate at which data is generated - may be a
random variable or an integer")
   (pkt-size  :initarg :pkt-size :initform 512 :type integer :accessor pkt-size
              :documentation "Size of packets"))
  (:documentation "A constant or average bit rate source transmitting
either at rate (which may be a random variable or a constant) or as
fast as possible if rate is 0"))

(defmethod initialize-instance :after
    ((app abr-source) &key (protocol 'layer4:udp) node
     &allow-other-keys)
  (let ((protocol (layer4:find-protocol protocol- node)))
    (unless protocol
      (error "Unable to find protocol ~A for ~A" protocol-type app))
    (setf (slot-value app 'layer4:protocol) protocol)))

(defmethod sent((app abr-source) n socket)
  "Send a packet on every notification if rate is 0"
  (when (and (numberp (rate app)) (zerop (rate app)))
    (send socket (pkt-size app) app)))

(defmethod handle((app abr-source))
  "Send a packet and reschedule"
  (with-slots(rate socket pkt-size) app
    (send socket pkt-size app)
    (schedule (/ (* pkt-size 8) (random-value rate)) app)))

(defmethod start((app abr-source))
  (unless (slot-boundp app 'socket)
    (setf (slot-value app 'socket)
          (make-instance
           'socket
           :protocol (slot-value app 'layer4:protocol)
           :application app
           :peer-address (peer-address app)
           :peer-port (peer-port app)))))

(defmethod connection-complete((app abr-source) socket &key failure)
  (unless failure
    (with-slots(rate pkt-size) app
      (if (and (numberp rate) (zerop rate))
          (send socket pkt-size app)
          (handle app)))))

(defmethod stop((app abr-source) &key &allow-other-keys)
  (close-connection (slot-value app 'layer4:socket))
  (call-next-method))





