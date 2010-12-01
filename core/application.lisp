;; $Id$
;; Application Interface
;; Copyright (C) 2006 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;; This file is part of Lisp Educations Network Simulator (LENS)

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:

;;

;;; Code:

(in-package :layer5)

(defclass application()
  ((name :type string :initarg :name
         :documentation "Name of this application"))
  (:documentation "Class Application is the base class for all applications.
It defines the interface between the application class and
the associated layer 4 protocols.  Applications can have one
or more layer 4 protocols assigned, to allow (for example)
a web browser model with multiple simultaneous connections."))

(defmethod name((app application))
  (if (slot-boundp app 'name)
      (slot-value app 'name)
      (string-downcase (class-name (class-of app)))))

(defmethod receive((application application) pdu flow &key &allow-other-keys)
  "Called by layer 4 protocol object when data is received. Default - do nothing"
    (declare (ignore application pdu flow)))

(defgeneric send-complete(application packet receiver &key fail &allow-other-keys)
  (:documentation "Called by receiver when sending completed to notify
  receiver - fail-reason indicates if packet was dropped - null if successful")
  (:method(sender packet receiver &key &allow-other-keys)
    (declare (ignore sender packet receiver))))

(defgeneric sent(application no-octets-sent protocol)
  (:documentation "Called by an associated layer 4 protocol when all some part
of the outstanding data has been sent.  For TCP protocols,
this occurs when the acknowledgement is received from the peer.")
  (:method(application no-octets-sent protocol)
    (declare (ignore application no-octets-sent protocol))))

(defgeneric close-request(application protocol)
  (:documentation "Called by an associated layer 4 protocol when a connection
close request has been received from a peer.  Applications should
respond by calling the corresponding close-connection routine")
  (:method(app protocol)
    (declare (ignore app))
    (close-connection protocol)))

(defgeneric closed(application protocol)
  (:documentation "Called by an associated layer 4 protocol when a connection
has completely closed")
  (:method(app protocol) (declare (ignore app protocol))))

(defgeneric connection-complete(application protocol)
  (:documentation "Called by layer 4 protocol when a previous
connection request is completed")
  (:method(app protocol) (declare (ignore app protocol))))

(defgeneric server-connection-complete(application protocol)
  (:documentation "Called by an associated layer 4 protocol when a connection
 that the server agent accepted has successfully completed.")
  (:method(app protocol) (declare (ignore app protocol))))

(defgeneric connection-failed(application protocol)
  (:documentation "Called by layer 4 protocol when a previous
connection request failed")
  (:method(app protocol) (declare (ignore app protocol))))

(defgeneric connection-from-peer(application protocol src-address)
  (:documentation "Called when a listening TCP protocol receives a
conntection request. Return true if connection accepted.")
  (:method(application protocol src-address)
    (declare (ignore application protocol src-address))
    t))

