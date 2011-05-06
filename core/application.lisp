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

(defclass application(protocol:protocol)
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
      (call-next-method)))

(defmethod trace:default-trace-detail((application application))
  '(length-bytes))

(defmethod initialize-instance :after ((app application) &key node &allow-other-keys)
  (push app (node:applications node)))

(defmethod receive((application application) data layer4 &key &allow-other-keys)
  "Called by layer 4 protocol object when data is received. Default -
do nothing"
    (declare (ignore application data layer4)))