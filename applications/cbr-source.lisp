;; $Id$
;; Constant bit rate source application
;; Copyright (C) 2006 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;; This file is part of Lisp Educational Network Simulator (LENS)

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:

;;

;;; Code:

(in-package :application)

(defclass cbr-source(application)
  ((protocol :type protocol :reader protocol
             :documentation "layer 4 protocol used by this application")
   (peer-address :initarg :peer-address :initform nil :type ipaddr
                 :accessor peer-address
                 :documentation "IP address of peer to send to")
   (peer-port  :initarg :peer-port :initform nil
               :type ipport :accessor peer-port
               :documentation "Port of peer to send to")
   (rate  :initarg :rate :initform 500000 :accessor rate
          :documentation "Rate at which data is generated - may be a
random variable or an integer")
   (pkt-size  :initarg :pkt-size :initform 512 :type integer :accessor pkt-size
              :documentation "Size of packets"))
  (:documentation "A constant or average bit rate source transmitting
either at rate (which may be a random variable or a constant) or as
fast as possible if rate is 0"))

(defmethod initialize-instance :after
    ((app cbr-source) &key (protocol 'udp) node &allow-other-keys)
  (let ((args `(:application ,app :node ,node)))
    (setf (slot-value app 'protocol)
          (apply #'make-instance
                 (if (listp protocol)
                     (append protocol args) (cons protocol args))))))

(defmethod notify((app cbr-source))
  "Send a packet on every notification if rate is 0"
  (send (pkt-size app) (protocol app)))

(defmethod handle((app cbr-source))
  "Send a packet and reschedule"
  (with-slots(rate protocol pkt-size) app
    (send pkt-size protocol)
    (schedule (/ (* pkt-size 8) (random-value rate)) app)))

(defmethod start((app cbr-source))
  (stop app)
  (connect (protocol app) (peer-address app) (peer-port app)))

(defmethod connection-complete((app cbr-source) protocol)
  (cond
    ((and (numberp (rate app)) (zerop (rate app)))
     (setf (notification protocol) app)
     (send (pkt-size app) protocol))
    (t
     (handle app))))

(defmethod stop((app cbr-source) &key abort)
  (declare (ignore abort))
  (cancel app)
  (when (eql app (notification (protocol app)))
    (setf (notification (protocol app)) nil))
  (close-connection (protocol app)))





