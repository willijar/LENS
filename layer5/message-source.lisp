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

(defclass message-source(application scheduler:event)
  ((peer-address :initarg :peer-address :initform nil :type ipaddr
                 :reader peer-address
                 :documentation "IP address of peer to send to")
   (peer-port  :initarg :peer-port :initform nil
               :type ipport :reader peer-port
               :documentation "Port of peer to send to")
   (protocol :type tcp :reader protocol :initarg :protocol
             :initform (make-instance 'tcp-reno)
             :documentation "The tcp (layer 4) protocol instance")
   (sleep-time :initarg :sleep-time :accessor sleep-time :initform 5
               :documentation "Random time to sleep between transmissions")
   (data-size :initarg :data-size :initform 512  :accessor data-size
              :documentation "Random size of message to send")
   (response-size :initarg :response-size :initform nil :accessor response-size
                  :documentation "Random size of response to request")
   (loop-count :initarg :loop-count :initform 1 :accessor loop-count)
   (repeat-count :initform 0 :accessor repeat-count)
   (bytes-sent :initform 0 :accessor bytes-sent
               :documentation "Bytes sent this loop")
   (bytes-ack :initform 0 :accessor bytes-ack
              :documentation "Bytes acknowledged this loop")
   (bytes-requested :initform 0 :accessor bytes-requested
                    :documentation "Bytes requested this loop")
   (bytes-received :initform 0 :accessor bytes-received
                   :documentation "Bytes received this loop"))
  (:documentation "An application that sends a random amount of data
to a TCP server.  The application will optionally sleep for a random
amount of time and send some more data, up to a user specified limit
on the number of sending iterations."))

(defmethod initialize-instance :after ((app message-source)
                               &key
                                &allow-other-keys)
  (with-slots(protocol) app
    (setf (node protocol) (node app)
          (application protocol) app)))

(defmethod start((app message-source))
  (setf (repeat-count app) 0
        (bytes-sent app) 0
        (bytes-ack app) 0
        (bytes-requested app) 0
        (bytes-received app) 0)
  (open-connection (peer-address app)  (peer-port app) (protocol app)))

(defmethod stop((app message-source) &key &allow-other-keys)
  (call-next-method)
  (break "Stopping ~A" app)
  (close-connection (protocol app))
  (unbind (protocol app)))

(defmethod reset((app message-source)) (stop app))

(defmethod connection-complete((app message-source) layer4)
  (handle app))

(defmethod handle((app message-source))
  (if (<= (incf (repeat-count app)) (loop-count app))
      (let* ((data-size (random-value (data-size app)))
             (response-size  (random-value (response-size app)))
             (data (make-message-data data-size :response-size response-size)))
        (incf (bytes-requested app) response-size)
        (incf (bytes-sent app) data-size)
        (send (protocol app) data app)
        (schedule (random-value (sleep-time app)) app))
      (stop app)))

(defmethod sent((app message-source) n protocol)
  (declare (ignore protocol))
  (incf (bytes-ack app) n))

(defmethod receive((app message-source) data protocol &key &allow-other-keys)
  (declare (ignore protocol))
  (incf (bytes-received app) (length-bytes data)))

(defmethod control-message ((app message-source) msg protocol
                            &key &allow-other-keys)
  (stop app))
