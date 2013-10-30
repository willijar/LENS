;; Message and packet definitions for LENS
;; Copyright (C) 2011 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;;; Copying:

;; This file is part of LENS

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; FOOBAR is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(in-package :lens)

(defclass message(event owned-object)
  ((creation-time
    :type time-type :initform (simulation-time) :reader creation-time
    :documentation "The creation time of the message. With cloned
messages (see dup() later), the creation time of the original message
is returned and not the time of the cloning operation. This is
particularly useful when modeling communication protocols, because
many protocols clone the transmitted packages to be able to do
retransmissions and/or segmentation/reassembly.")
   (from :accessor from
         :documentation "Module or gate Sending message")
   (to :accessor to
       :documentation  "Module or Gate to receive message after delay")
   (sent-time :type time-type :accessor sent-time)
   (timestamp :type time-type :initarg :timestamp :initform 0.0d0
               :accessor timestamp
               :documentation "Utility time stamp field fror programmer"))
  (:documentation "Messages objects represent events, packets,
commands, jobs, customers or other kinds of entities, depending on the
model domain."))

(defmethod print-object((instance message) stream)
  (print-unreadable-object(instance stream :type t)
    (when (slot-boundp instance 'name)
      (format stream "~A" (name instance)))
    (if (>= (slot-value instance 'rank) 0)
      (format stream " (~D)" (uid instance)))
    (when (slot-boundp instance 'to)
      (format stream " to ~A" (slot-value instance 'to)))))

(defmethod latency((message message))
  (- (arrival-time message) (timestamp message)))

(defmethod duplicate((message message)
                      &optional (duplicate (make-instance (class-of message))))
  (call-next-method message duplicate)
  (copy-slots '(creation-time to from sent-time timestamp)
              message duplicate))

(define-condition unknown-message(warning)
  ((module :reader module :initarg :module)
   (message :reader message :initarg :message))
  (:report (lambda(c os)
             (format os "Unknown message ~A arrived at module ~A" (message c)
                     (module c)))))

(defgeneric handle-message(entity message)
  (:documentation "Must be implemented for protocols to receive messages")
  (:method(entity message)
    (warn 'unknown-message :module entity :message message)))

(defmethod handle((message message))
  (let ((*context* (to-module message)))
    (handle-message *context* message)))

(defgeneric byte-length(entity)
  (:documentation "Return the length in whole octets (8 bit bytes) of entity"))

(defmethod byte-length((v bit-vector)) (* 8 (ceiling  (length v) 8)))

(defgeneric bit-length(entity)
  (:method(entity) (* 8 (byte-length entity)))
  (:method((v bit-vector)) (length v)))

(defclass packet(message)
  ((encapsulated-packet
    :type packet :initarg :encapsulated-packet
    :documentation "Higher level encapsulated protocol packet.")
   (duration :accessor duration :type time-type :initform 0.0d0
             :documentation "Duration of last transmission")
   (control-info
    :accessor control-info :initarg :control-info
    :documentation "Additional data to be passed with packet between
    protocol layers")
   (reception-start-p :accessor reception-start-p
    :type boolean :initform nil :initarg :deliver-on-reception-start
    :documentation "Identify whether this packet represents the start
or the end of the reception after the packet travelled through a
channel with a data rate. This flag is controlled by the
deliver-on-reception-start flag of the receiving gate.")
   (bit-error-p
    :initform nil :reader bit-error-p
    :documentation "The result of error modelling after the packet is
sent through a channel that has a nonzero packet error rate (PER) or
bit error rate (BER). It is up to the receiver to examine this flag
after having received the packet, and to act upon it."))
   (:documentation "Representation of network packets"))

;; TODO add in reference counting so last reference of encapsulated
;; packet doesn't need to be duplicated.

(defgeneric encapsulate(packet packet-to-be-encapsulated)
  (:documentation "Called to encapsulate one packet within another")
  (:method((packet packet) other)
    (assert (not (slot-boundp packet 'encapsulated-packet)))
    (setf (slot-value packet 'encapsulated-packet) other)
    packet))

(defgeneric decapsulate(packet)
  (:documentation "Users must use this to get encapsulated packet for
  subsequent packet may be shared shared across network.")
  (:method((packet packet))
    (duplicate (slot-value packet 'encapsulated-packet))))

(defmethod duplicate((packet packet) &optional duplicate)
  (call-next-method)
  (copy-slots '(encapsulated-packet bit-error-p control-info duration)
              packet duplicate))
