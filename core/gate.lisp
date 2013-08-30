;; Gate Implementation
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

(deftype gate-direction() '(member :input :output :inout))

(defclass gate(owned-object)
  ((previous-gate
    :type link :reader previous-gate :initform nil
    :documentation "The previous gate in the series of connections (the path)")
   (next-gate
    :type link :reader next-gate :initform nil
    :documentation "The next gate in the series of connections (the path)")
   (channel
    :type channel :reader channel :initform nil
    :documentation "Channel object (if exists) to next link")
   (deliver-on-reception-start-p
    :initform nil :accessor deliver-on-reception-start-p
    :documentation "Messages with nonzero length then have a nonzero
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

(defclass gate-slot(owned-object)
  ((input :reader input :initform nil :reader input-gate-p
          :documentation "Slot for input gate or gates")
   (output :reader output :initform nil :reader output-gate-p
           :documentation "Slot for output gate or gates"))
  (:documentation "Named storage slot for gate or gates - direction
  initarg must be specified as :input, :output or :inout. If an
  initial size is given then it will be an array of gates and access
  must by index"))

(defmethod for-each-child((gate gate) (operator function))
  (when (channel gate) (funcall operator (channel gate))))

(defmethod parent-module((link gate))
  (owner (owner link)))

(defmethod parent-module((g gate-slot))
  (owner g))

(defmethod name((link gate))
  (name (owner link)))

(defmethod full-name((gate gate))
  `(,(name gate)
     ,(gate-direction gate)
     ,@(when (slot-boundp gate 'index) (list (index gate)))))

(defmethod print-object((gate gate) stream)
  (print-unreadable-object (gate stream :type t :identity t)
    (format stream "~A.~A[~A]~@[[~A]~]"
            (name (parent-module gate))
            (first (full-name gate))
            (second (full-name gate))
            (third (full-name gate)))))

(defun gate-direction(gate)
  (let ((input (input (owner gate))))
    (cond  ((eql gate input) :input)
           ((eql gate (output (owner gate))) :output)
           ((find gate input) :input)
           (:output))))

(defmethod for-each-child((slot gate-slot) (operator function))
  (dolist(d (list (input slot) (output slot)))
    (when d
      (if (vectorp d) (map nil operator d) (funcall operator d)))))

(defgeneric gate(entity address &key index &allow-other-keys)
  (:method((entity gate-slot) direction &key index &allow-other-keys)
    (let ((s (ecase direction
               (:input (input entity))
               (:output (output entity)))))
      (if index
          (if (eql index '++)
              (let ((new-gate
                     (make-instance 'gate :index (length s) :owner entity)))
                (vector-push-extend new-gate s)
                new-gate)
              (aref s index))
          s))))

(defun gate-extend(gate-slot)
  "Adds new gate to an gate array returning new gate (or gates if an
inout gate-slot)"
  (values-list
   (mapcan
    #'(lambda(array)
        (when array
          (assert (arrayp array))
          (let ((new-gate (make-instance 'gate :owner gate-slot)))
            (vector-push-extend new-gate array)
            (list new-gate))))
    (list (input gate-slot) (output gate-slot)))))

(defmethod initialize-instance :after ((gate-slot gate-slot)
                                       &key direction initial-size)
  (assert (typep  direction 'gate-direction))
  (flet((make-gates()
          (if initial-size
              (progn
                (let ((a (make-array initial-size
                                     :element-type 'gate
                                     :adjustable t
                                     :fill-pointer 0)))
                  (dotimes(x initial-size)
                    (vector-push (make-instance 'gate
                                                :name (name gate-slot)
                                                :owner gate-slot) a))
                  a))
              (make-instance 'gate :name (name gate-slot) :owner gate-slot))))
  (when (member direction '(:input :inout))
    (setf (slot-value gate-slot 'input) (make-gates)))
  (when (member direction '(:output :inout))
    (setf (slot-value gate-slot 'output) (make-gates)))))

(defun gate-type(gate-slot)
  (if (input-gate-p gate-slot)
      (if (output-gate-p gate-slot)
          :inout
          :input)
      :output))

(defmethod info((gate gate))
  (multiple-value-bind(arrow g chan)
      (ecase (gate-direction gate)
        (:input
         (let ((pg (previous-gate gate)))
           (values "<-" pg (when pg (channel pg)))))
        (:output (values "->" (next-gate gate) (channel gate))))
    (if g
        (format nil "~A ~A.~A ~:[~;,~A ~A~]"
                arrow
                (if (eq (owner g) (parent-module (owner gate)))
                    "<parent>"
                    (full-name (owner g)))
                (full-name g)
                chan
                (class-name (class-of chan))
                (info chan))
        "not connected")))

(defun path-start-gate(gate)
  (do((gate gate (previous-gate gate)))
     ((not (previous-gate gate)) gate)))

(defun path-end-gate(gate)
  (do((gate gate (next-gate gate)))
     ((not (next-gate gate)) gate)))

(defun check-channels(gate)
  (do* ((gate (path-start-gate gate) (next-gate gate))
        (end-gate (path-end-gate gate))
        (n 0))
      (end-gate)
    (when (typep (channel gate) 'transmission-channel)
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
argument cannot be nil, that is, you cannot use this function
to disconnect a gate; use disconnect() for that.")
  (:method((from gate) (to gate)  &key channel leave-uninitialized)
    (assert (and (not (next-gate from)) (not (previous-gate to))))
    (let* ((module (parent-module from))
           (start-gate (path-start-gate from))
           (end-gate (path-end-gate to))
           (start-module (parent-module start-gate))
           (end-module (parent-module end-gate)))
      (when (has-listeners module 'pre-model-change)
        (emit module 'pre-model-change
            (make-instance 'pre-gate-connect-notification
                           :gate from :target-gate to :channel channel)))
      (when (or (has-listeners start-module 'pre-model-change)
                (has-listeners end-module 'pre-model-change))
        (let ((notification
               (make-instance 'pre-path-create-notification
                 :start-gate start-gate :end-gate end-gate
                 :changed-gate from)))
          (emit start-module 'pre-model-change notification)
          (emit end-module 'pre-model-change notification)))
    (setf (slot-value from 'next-gate) to
          (slot-value to 'previous-gate) from)
    (when channel
      (setf (channel from) channel)
      (configure channel)
      (when (and (not leave-uninitialized) (parent-module channel)
                 (not (initialized-p (parent-module channel))))
        (initialize channel)))
    (when (has-listeners module 'post-model-change)
      (emit module 'post-model-change
            (make-instance 'post-gate-connect-notification :gate from)))
    (when (or (has-listeners start-module 'post-model-change)
              (has-listeners end-module 'post-model-change))
        (let ((notification
               (make-instance 'post-path-create-notification
                 :start-gate start-gate :end-gate end-gate
                 :changed-gate from)))
          (emit start-module 'post-model-change notification)
          (emit end-module 'post-model-change notification))))
    channel))

(defgeneric disconnect(gate)
  (:documentation "Disconnects the gate, and also deletes the
  associated channel object if one has been set. disconnect() must be
  invoked on the source gate (from side) of the connection.
The method has no effect i(load-system :lens.wsn)f the gate is not connected.")
  (:method((gate gate))
    (unless (next-gate gate) (return-from disconnect))
    (let* ((module (parent-module gate))
           (start-gate (path-start-gate gate))
           (end-gate (path-end-gate gate))
           (start-module (parent-module start-gate))
           (end-module (parent-module end-gate)))
      (when (has-listeners module 'pre-model-change)
        (emit module 'pre-model-change
              (make-instance 'pre-gate-disconnect-notification
                             :gate gate)))
      (when (or (has-listeners start-module 'pre-model-change)
                (has-listeners end-module 'pre-model-change))
        (let ((notification
               (make-instance 'pre-path-cut-notification
                 :start-gate start-gate :end-gate end-gate
                 :changed-gate gate)))
          (emit start-module 'pre-model-change notification)
          (emit end-module 'pre-model-change notification)))
      (let ((next-gate (next-gate gate))
            (channel (channel gate)))
        (setf (slot-value gate 'channel) nil
              (slot-value gate 'next-gate) nil
              (slot-value next-gate 'previous-gate) nil)
        (when (has-listeners module 'post-model-change)
          (emit module 'post-model-change
              (make-instance 'post-gate-disconnect-notification
                             :gate gate
                             :target-gate next-gate
                             :channel channel)))
      (when (or (has-listeners start-module 'post-model-change)
                (has-listeners end-module 'post-model-change))
        (let ((notification
               (make-instance 'post-path-cut-notification
                 :start-gate start-gate :end-gate end-gate
                 :changed-gate gate)))
          (emit start-module 'post-model-change notification)
          (emit end-module 'post-model-change notification)))))))

(defgeneric (setf channel)(channel source-gate)
  (:method(channel (gate gate))
    (assert (null (channel gate)))
    (setf (slot-value channel 'source-gate) gate
          (slot-value gate 'channel) channel)
    (check-channels gate)
    (repair-signal-flags channel)
    channel))

(defmethod (setf deliver-on-reception-start-p) :before (value (gate gate))
  (assert (eql (gate-direction gate) :input)))

(defgeneric transmission-channel(gate &optional incoming-p)
  (:documentation "Typically invoked on an output gate, this method
  returns the channel in the connection path that supports
  datarate (as determined; it is guaranteed that there can be at most
  one such channel per path). If there is no such channel, nil is
  returned. If incoming-p is true looks for an incoming channel - else
  returns the outgoing channel.")
  (:method((gate gate) &optional incoming-p)
    (let ((f (if incoming-p #'previous-gate #'next-gate)))
      (do((g gate (funcall f gate)))
         ((typep (channel g) 'transmission-channel) (channel g))))))

(defgeneric deliver(message gate time)
  (:documentation "This function is called internally by the send()
  functions and channel classes' deliver() to deliver the message to
  its destination. A false return value means that the message object
  should be deleted by the caller. (This is used e.g. with parallel
  simulation, for messages leaving the partition.)")
  (:method(message (gate gate) time)
    (cond
      ((not (next-gate gate))
       (arrived (parent-module gate) message gate time)
       t)
      ((not (channel gate))
       (deliver message (next-gate gate) time))
      (t
       (let* ((channel (channel gate))
             (transmission-p (typep channel 'transmission-channel)))
         (assert (initialized-p channel))
         (when transmission-p
           (assert (> time (transmission-finish-time channel))
                   ()
                   "Error sending message ~A on gate ~A: channel is currently busy with an ongoing transmission -- please rewrite the sender simple module to only send when the previous transmission has  already finished, using cGate::getTransmissionFinishTime(), scheduleAt(), and possibly a cQueue for storing messages waiting to be transmitted" message gate)
           (assert (or (not (typep message 'packet))
                       (zerop (duration message)))
                    ()
                    "Packet ~A already has a duration set; there may be more than one channel with data rate in the connection path, or it was sent with a sendDirect() call that specified duration as well" message))
         (let ((result (process-message channel message time)))
           (unless (channel-result-discard result)
             (when (and transmission-p (typep message 'packet))
               (setf (duration message) (channel-result-duration result)))
             (deliver message (next-gate gate)
                      (+ time (channel-result-delay result))))))))))

(defun connected-outside-p(gate)
  (if (eql (gate-direction gate) :input)
      (previous-gate gate)
      (next-gate gate)))

(defun connected-inside-p(gate)
  (if (eql (gate-direction gate) :input)
      (next-gate gate)
      (previous-gate gate)))

(defun end-module(gate)
  (parent-module
   (if (eql (gate-direction gate) :input)
       (path-start-gate gate)
       (path-end-gate gate))))