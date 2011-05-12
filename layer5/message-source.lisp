;; A message source application
;; Copyright (C) 2007 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;; This file is part of Lisp Educational Network Simulator (LENS)

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:

;;

;;; Code:

(in-package :protocol.layer5)

(defclass message-source(client-application event)
  ((sleep-time :initarg :sleep-time :accessor sleep-time :initform 5
               :documentation "Random time to sleep between transmissions")
   (data-size :initarg :data-size :initform 512  :accessor data-size
              :documentation "Random size of message to send")
   (response-size :initarg :response-size :initform nil :accessor response-size
                  :documentation "Random size of response to request")
   (loop-count :initarg :loop-count :initform 1 :accessor loop-count)
   (repeat-count :initform 0 :accessor repeat-count))
  (:default-initargs :protocol (make-instance 'tcp-reno))
  (:documentation "An application that sends a random amount of data
to a TCP server.  The application will optionally sleep for a random
amount of time and send some more data, up to a user specified limit
on the number of sending iterations."))

(defmethod start((app message-source)) (handle app))

(defmethod reset((app message-source))
  (call-next-method)
  (setf (repeat-count app) 0))

(defmethod connection-complete((app message-source) layer4)
  (let* ((data-size (random-value (data-size app)))
         (response-size  (random-value (response-size app)))
         (data (make-message-data data-size :response-size response-size)))
    (send (protocol app) data app)
    (close-connection (protocol app))
    (schedule (random-value (sleep-time app)) app)))

(defmethod handle((app message-source))
  (when (<= (incf (repeat-count app)) (loop-count app))
    (open-connection (peer-address app)  (peer-port app) (protocol app))))
