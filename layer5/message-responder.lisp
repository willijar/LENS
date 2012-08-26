;; tcp application base
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

(defclass message-responder(application)
  ((listener :type layer4:protocol :accessor listener
             :initform (make-instance 'tcp-reno)
             :documentation "The listener (layer 4) protocol instance")
   (local-port :initarg :local-port
               :type ipport :reader local-port
               :documentation "Port number to bind to")
   (responders :initform nil :type list :accessor responders
               :documentation "List of responders (connections)")
   (bytes-sent :initform 0 :accessor bytes-sent
               :documentation "Bytes sent")
   (bytes-ack :initform 0 :accessor bytes-ack
              :documentation "Bytes acknowledged")
   (bytes-received :initform 0 :accessor bytes-received
                   :documentation "Bytes received")
   (response-statistics :initform nil
                        :initarg :response-statistics
                        :reader response-statistics
                        :documentation "object for logging response time"))
  (:documentation "A simple model of a request/response server.  The
  server binds to a specified port, and listens for connection
  requests. The messages received from the peers specifies how much data
  to send or receive."))

(defmethod initialize-instance :after((app message-responder)
                               &key
                               &allow-other-keys)
  (with-slots(listener) app
    (setf (node listener) (node app)
          (application listener) app)))

(defmethod start((app message-responder))
  (bind (listener app) :local-port (local-port app)))

(defmethod stop((app message-responder) &key &allow-other-keys)
  (call-next-method)
  (map 'nil #'close-connection (responders app))
  (close-connection (listener app))
  (unbind (listener app)))

(defmethod connection-from-peer((app message-responder) layer4)
  (unless (eql layer4 (listener app))
    (push layer4 (responders app))))

(defmethod control-message ((app message-responder) msg protocol
                            &key &allow-other-keys)
  (if (eql protocol (listener app))
      (stop app)
      (close-request app protocol)))

(defmethod close-request((app message-responder) protocol)
  (setf (responders app) (delete protocol (responders app)))
  (call-next-method))

(defmethod receive((app message-responder) (data data) protocol
                   &key &allow-other-keys)
  (incf (bytes-received app) (length-bytes data)))

(defmethod receive((app message-responder) (data message-data) protocol
                   &key &allow-other-keys)
  (call-next-method)
  (dolist(msg (messages data))
    (when (response-statistics app)
      (record (- (simulation-time) (message-created msg))
              (response-statistics app)))
    (let ((sz (message-response-size msg)))
      (when sz
        (incf (bytes-sent app) sz)
        (send protocol sz app)))))

(defmethod sent((app message-responder) c protocol)
  (incf (bytes-ack app) c))