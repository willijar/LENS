;; Application Interface
;; Copyright (C) 2006-2012 Dr. John A.R. Williams

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
  ((node :type node :initarg :node :reader node)
   (name :type string :initarg :name
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

(defmethod start((app application)))
(defmethod stop((app application) &key &allow-other-keys))
(defmethod reset((app application)))

(defmethod trace:default-trace-detail((application application))
  '(length-bytes))

(defmethod initialize-instance :after ((app application) &key &allow-other-keys)
  (push app (node:applications (node app))))

(defmethod receive :before ((receiver application) data
                            (sender protocol:protocol) &key &allow-other-keys)
  (protocol::write-trace receiver data :text "+"))

(defmethod send :before ((receiver protocol:protocol) data (sender application)
                         &key &allow-other-keys)
  (protocol::write-trace sender data :text "-"))


