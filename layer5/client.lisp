;; Client application
;; Copyright (C) 2011 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;; This file is part of Lisp Educations Network Simulator (LENS)

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:

;;

;;; Code:

(in-package :layer5)

(defclass client-application(application)
  ((peer-address :initarg :peer-address :type network-address
                 :reader peer-address
                 :documentation "IP address of peer to send to")
   (peer-port  :initarg :peer-port
               :type ipport :reader peer-port
               :documentation "Port of peer to send to")
   (protocol :type layer4:protocol :reader protocol :initarg :protocol
             :documentation "The layer 4 protocol instance")
   (bytes-sent :initform 0 :accessor bytes-sent
               :documentation "Bytes sent")
   (bytes-received :initform 0 :accessor bytes-received
                   :documentation "Bytes received")
   (bytes-ack :initform 0 :accessor bytes-ack
              :documentation "Bytes acknowledged")
   (bytes-requested :initform 0 :accessor bytes-requested
                    :documentation "Bytes requested"))
  (:documentation "Basis for single transport agent client applications"))

(defmethod initialize-instance :after ((app client-application)
                               &key
                                       &allow-other-keys)
  (with-slots(protocol) app
    (setf (node protocol) (node app)
          (application protocol) app)))

(defmethod stop((app client-application) &key &allow-other-keys)
  (call-next-method)
  (close-connection (protocol app)))

(defmethod reset((app client-application))
  (call-next-method)
  (setf (bytes-sent app) 0
        (bytes-received app) 0
        (bytes-ack app) 0
        (bytes-requested app) 0)
  (reset (protocol app)))

(defmethod receive((app client-application) (data data) protocol &key &allow-other-keys)
  (incf (bytes-received app) (length-bytes data)))

(defmethod send :before(layer4 data (app client-application) &key &allow-other-keys)
  (incf (bytes-sent app) (length-bytes data)))

(defmethod send :before(layer4 (data message-data) (app client-application)
                               &key &allow-other-keys)
  (incf (bytes-requested app)
        (reduce #'+ (mapcar #'message-response-size (messages data)))))

(defmethod sent((app client-application) n protocol)
  (declare (ignore protocol))
  (incf (bytes-ack app) n))

(defmethod send((transport layer4:protocol) (length-bytes integer) app
                &key &allow-other-keys)
  (send transport (make-instance 'data :length-bytes length-bytes) app))