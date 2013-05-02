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

(defclass message(named-object event)
  ((creation-time
    :type time-type :initform (simulation-time) :reader creation-time
    :documentation "The creation time of the message. With cloned
messages (see dup() later), the creation time of the original message
is returned and not the time of the cloning operation. This is
particularly useful when modeling communication protocols, because
many protocols clone the transmitted packages to be able to do
retransmissions and/or segmentation/reassembly.")
   (source :reader source :initarg :source
           :documentation "Object Sending message")
   (destination :reader destination :initarg :destination
                :documentation  "Object to receive message after delay")
   (time-stamp :type time-type :initarg :time-stamp :initform 0
               :accessor time-stamp
               :documentation "Utility time stamp field fror programmer"))
  (:documentation "Messages objects represent events, packets,
commands, jobs, customers or other kinds of entities, depending on the
model domain."))

(defmethod duplicate((message message)
                      &optional (duplicate (make-instance 'message)))
  (call-next-method)
  (copy-slots '(creation-time source destination time-stamp) message duplicate))

(defgeneric handle-message(entity message)
  (:documentation "Must be implemented for protocols to receive messages"))

(defgeneric send-message(message source destination &optional delay)
  (:documentation "Main interface for sending messages")
  (:method :before(message source destination &optional delay)
      (declare (ignore delay))
      (assert (not (scheduled-p message)))))

(defun self-message-p(message) (eql (source message) (destination message)))

(defmethod handle((message message))
  (handle-message (destination message) message))

(defgeneric bit-length(entity)
  (:documentation "Return the length in bits of an entity"))

(defgeneric byte-length(entity)
  (:documentation "Return the length in whole octets (8 bit bytes) of entity")
  (:method(entity) (* 8 (ceiling (bit-length entity) 8))))

(defmethod bit-length((v bit-vector)) (length v))

(defclass packet(message)
  ((encapsulated-packet
    :type packet
    :documentation "Higher level encapsulated protocol packet.")
   (duration :accessor duration :type number :initform 0
             :documentation "Duration of last transmission")
   (control-info
    :accessor control-info :initarg :control-info
    :documentation "Additional data to be passed with packet between
    protocol layers")
   (reception-start-p
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
   (:documentation "Representation of netwolympus 75-300ork packets"))

(defmethod duplicate((packet packet)
                     &optional (duplicate (make-instance 'packet)))
  (call-next-method)
  (copy-slots '(encapsulated-packet bit-error-p control-info) packet duplicate)
  (copy-slots (mapcar #'car (header-specification packet)) packet duplicate))

(defgeneric header-specification(packet)
  (:documentation "return an a-list mapping header field slot names to
  length in bits")
  (:method((packet packet)) nil))

(defmethod bit-length((packet packet))
  (+ (reduce #'+ (header-specification packet) :key #'cdr)
     (if (slot-boundp packet 'encapsulated-packet)
         (bit-length  (slot-value packet 'encapsulated-packet))
         0)))
