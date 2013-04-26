;; <description>
;; Copyright (C) 2011 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;;; Copying:

;; This file is part of

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

(defclass gate(named-object owned-object)
  ((previous-gate
    :type gate :reader previous-gate :initform nil
    :documentation "The previous gate in the series of connections (the path)")
   (next-gate
    :type gate :reader next-gate :initform nil
    :documentation "The next gate in the series of connections (the path)")
   (channel
    :type channel :reader channel :initform nil
    :documentation "Channel object (if exists) to next gate")
   (deliver-on-reception-start
    :initform nil :accessor deliver-on-reception-start
    :documentation " Messages with nonzero length then have a nonzero
    transmission duration (and thus, reception duration on the other
    side of the connection). By default, the delivery of the message
    to the module marks the end of the reception. Setting this bit
    will cause the channel to deliver the message to the module at the
    start of the reception. The duration that the reception will take
    can be extracted from the message object, by its duration()
    method."))
  (:documentation "Represents a module gate. Created and managed by modules;
the user typically does not want to directly create or destroy cGate
objects. However, they are important if a simple module algorithm
needs to know about its surroundings."))

(defclass input-gate(gate)
  ()
  (:documentation "An input gate type"))

(defclass output-gate(gate)
  ()
  (:documentation "An output gate type"))

(defclass inout-gate(input-gate output-gate)
  ()
  (:documention "An input/output gate"))

(defun gate-type-name(gate)
  (etypecase gate
    (inout-gate "inout")
    (input "input")
    (output "output")))

(defun path-start-gate(gate)
  (do((gate gate (prev-gate gate)))
     ((not (prev-gate gate)) gate)))

(defun path-end-gate(gate)
  (do((gate gate (next-gate gate)))
     ((not (next-gate gate)) gate)))

(defun check-channels(gate)
  (do* ((gate (path-start-gate gate) (next-gate gate))
       (end-gate (path-end-gate gate))
        (n 0))
      (end-gate)
    (when (transmission-channel-p (channel gate))
      (incf n))
    (assert (<= n 1) () "More More than one channel with data rate
    found in the connection path between gates ~A and
    ~A" (path-start-gate gate) end-gate)))

(defgeneric connect(from-gate to-gate &key channel leave-uninitialized)
  (:documentation "Connects the gate to another gate, using the given
channel object (if one is specified). This method can be used to
manually create connections for dynamically created modules.

This method invokes callInitialize() on the channel object, unless the
compound module containing this connection is not yet
initialized (then it assumes that this channel will be initialized as
part of the compound module initialization process.) To leave the
channel uninitialized, specify true for the leave-uninitialized
parameter.

If the gate is already connected, an error will occur. The gate
argument cannot be NULL, that is, you cannot use this function
to disconnect a gate; use disconnect() for that.")
  (:method((from gate) (to gate)  &key channel leave-uninitialized)
    (assert (and (not (next-gate from)) (not (prev-gate to))))
    (when (has-listeners (parent from) 'PRE-MODEL-CHANGE)
      (emit (owner from)
            'PRE-MODEL-CHANGE
            (make-pre-gate-connect-notification
             :gate from :target-gate to :channel channel)))
    (let* ((start-gate (path-start-gate from))
           (end-gate (path-end-gate to))
           (start-module (owner start-gate))
           (end-module (owner end-gate)))
      (when (or (has-listeners start-module 'PRE-MODEL-CHANGE)
                (has-listeners end-module 'PRE-MODEL-CHANGE))
        (let ((notification (make-pre-path-create-notification
                             :start-gate start-gate
                             :end-gate end-gate
                             :changed-gate from)))
          (emit start-module 'pre-model-change notification)
          (emit end-module 'pre-model-change notification))))
    (setf (slot-value from-gate 'next-gate) to-gate
          (slot-value to-gate 'prev-gate) from-gate
          (channel from-gate) channel)
    (check-channels from-gate)
    (when channel
      (configure channel (config *simulation*))
      (when (or (not leave-uninitialized)



))


(defgeneric disconnect(gate)
  (:documentation "Disconnects the gate, and also deletes the associated channel object if one has been set. disconnect() must be invoked on the source gate
(from side) of the connection.
The method has no effect if the gate is not connected."))

(defgeneric (setf channel)(gate channel &key leave-uninitialized)
  (:documentation "Disconnects the gate, then connects it again to the
  same gate, with the given channel object (if not NULL). The gate
  must be connected."))

(defgeneric transmission-channel(gate &optional incoming-p)
  (:documentation "Typically invoked on an output gate, this method
  returns the channel in the connection path that supports
  datarate (as determined; it is guaranteed that there can be at most
  one such channel per path). If there is no such channel, nil is
  returned. If incoming-p is true looks for an incoming channel - else
  returns the outgoing channel."))

(defmethod for-each-child((gate gate) operator)
  (when (channel gate) (funcall operator channel)))

(defgeneric deliver(message gate time)
  (:documentation "This function is called internally by the send()
  functions and channel classes' deliver() to deliver the message to
  its destination. A false return value means that the message object
  should be deleted by the caller. (This is used e.g. with parallel
  simulation, for messages leaving the partition.)"))


(defmethod info((gate gate))
