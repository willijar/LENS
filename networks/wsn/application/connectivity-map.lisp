;; Application to send packets at regular intervals.
;; Copyright (C) 2014 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;;; Copying:

;; This file is part of Lisp Educational Network Simulator (LENS)

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; LENS is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; The packets received satatistic will give connectivity map

;;; Code:
(in-package :lens.wsn)

(defclass connectivity-map (application)
  ((header-overhead :initform 8)
   (payload-overhead :initform 32)
   (priority :parameter t :initform 1 :reader priority :initarg :priority)
   (packet-spacing :parameter t :type time-type :initform 1d-1
                   :reader packet-spacing)
   (packets-per-node :parameter t :type integer :initform 100
                     :reader packets-per-node)
   (packets-sent :initform 0 :type integer :accessor packets-sent)
   (send-packet
    :type timer-message :initform (make-instance 'timer-message)))
  (:metaclass module-class)
  (:documentation "Application module that will generate
  [[packets-per-node]] packets at intervals of [[packets-per-spacing]]
  - useful to determine connectivity statistics of a network."))

(defmethod startup((application connectivity-map))
  (call-next-method)
  (set-timer application 'send-packet
             (* (packets-per-node application)
                (packet-spacing application)
                (nodeid (node application)))))

(defmethod handle-timer((application connectivity-map)
                        (timer (eql 'send-packet)))
  (unless (>= (packets-sent application) (packets-per-node application))
    (to-network application
                (encapsulate application (incf (packets-sent application)))
                broadcast-network-address)
    (set-timer application 'send-packet (packet-spacing application))))
