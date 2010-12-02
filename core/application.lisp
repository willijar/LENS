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
  ((name :type string :initarg :name :reader name
         :documentation "Name of this application"))
  (:documentation "Class Application is the base class for all applications.
It defines the interface between the application class and
the associated layer 4 protocols.  Applications can have one
or more layer 4 protocols assigned, to allow (for example)
a web browser model with multiple simultaneous connections."))

(defmethod initialize-instance :after ((app application) &key &allow-other-keys)
  (unless (slot-boundp app 'name)
    (setf (slot-value app 'name)
          (string-downcase (class-name (class-of app))))))

(defmethod receive((application application) pdu socket &key &allow-other-keys)
  "Called by layer 4 protocol object when data is received. Default - do nothing"
    (declare (ignore application pdu flow)))