;; Channel interfaces and basic implementations
;; Copyright (C) 2013-2014 Dr. John A.R. Williams

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

;;

;;; Code:
(in-package :lens)

(defclass channel(component)
  ((source-gate :initarg :source-gate :accessor source-gate
                :documentation "The gate which sends messages over the channel."))
  (:metaclass parameter-class)
  (:documentation "Base class for all channels."))

(defclass ideal-channel(channel)
  ()
  (:metaclass parameter-class)
  (:documentation "Channel with zero propagation delay, zero
  transmission delay (infinite datarate), and always enabled."))

(defclass transmission-channel(channel)
  ()
  (:metaclass parameter-class)
  (:documentation "Base classe for all transmission channels"))

(defstruct channel-result
  "* Slots
- delay :: The propagation delay of the channel
- duration :: The transmition duration of the packet.
- discard :: If true packet packet will be discarded (was lost in transmission)

* Description
Structure containg result of process-message from a channel"
  (delay 0.0d0 :type time-type)
  (duration 0.0d0 :type time-type)
  (discard nil :type boolean))

(defgeneric process-message(channel message time)
  (:documentation "* Arguments
- channel :: an instance of [[class channel]]
- message :: the [[class message]] to be processed
- time :: [[time-type]] the time message is to be processed

* Returns
- channel-result :: a [[structure channel-result]]

* Description
This method encapsulates the channel's functionality. The method
should model the transmission of the given message starting at the
given time, and return the propagation delay, transmission
duration, and discard flag in the channel-result object.

Transmission duration and bit error modeling only applies to packets
i.e. to instances of [[class packet]], it should be skipped for
non-packet messages. The method does not need to set the [[duration]]
of the packet; this is done by the simulation kernel. However, the
method should call [[function (setf bit-error)]] on the packet if
error modeling results in bit errors.

If the method sets the discard flag in the result object, it means
that the message object should be deleted by the simulation kernel;
this facility can be used to model that the message gets lost in the
channel.

The method does not need to throw errors on overlapping transmissions,
or if the packet's duration field is already set; these checks are
done before [[process-message]] is called.

* Simulation Events

- message-discarded :: [[structure timestamped]] with message
  as value should be emited if the [[class message]] is being discarded

- message-sent :: [[structure message-sent-signal-value]] should
  be emitted if message is successfully delivered with both the
  [[class message]] and [[structure channel-result]] objects.

* See also
[[structure channel-result]]

")
  (:method(channel message time) nil))

(defgeneric nominal-datarate(channel)
  (:documentation "* Arguments
- channel :: a [[class transmission-channel]]

* Returns
- nominal-datarate :: a =number=, bits per second

* Description

Returns the nominal data rate of the channel in bits per
second (bps). The number returned from this method should be treated
as informative; there is no strict requirement that the channel
calculates packet duration by dividing the packet length by the
nominal data rate. For example, specialized channels may add the
length of a lead-in signal to the duration.")
  (:method(channel) 0))

(defgeneric calculate-duration(channel message)
  (:documentation "* Arguments
- channel :: a [[transmission-channel]]
- message :: a [[message]]

* Returns
- duration :: a [[time-type]]

* Description

Calculates the transmission duration of the message with the current
transmission channel configuration (datarate, etc); it does not check
or modify channel state. For non-transmission channels this method
returns zero.

This method is useful for transmitter modules that need to determine
the transmission time of a packet without actually sending the packet.

Caveats: this method is best-effort -- there is no guarantee that
transmission time when the packet is actually sent will be the same as
the value returned by this method. The difference may be caused by
changed channel parameters (i.e. datarate being overwritten), or by a
non-time-invariant transmission algorithm.

Note that there is no requirement that [[method process-message]]
relies on this method to calculated the packet duration. That is, to
change the duration computation algorithm via subclassing you need to
redefine *both* [[process-message]] and [[calculate-duration]].")
  (:method(channel message) (declare (ignore message)) 0.0d0))

(defgeneric transmission-finish-time(channel)
  (:documentation "* Arguments
- channel :: a [[transmission-channel]]

* Returns
- duration :: a [[time-type]]

* Description

Returns the simulation time the sender gate will finish transmitting
over a transmission channel. If the gate is not currently
transmitting, the result is unspecified but less or equal the current
simulation time.")
  (:method(channel) (simulation-time)))

(defgeneric busy-p(channel)
  (:documentation "* Arguments
- channel :: a [[transmission-channel]]

* Returns
- busy :: a =boolean=

* Description

For transmission channels: returns whether the sender gate
is currently transmitting, ie. whether [[transmission-finish-time]]
is greater than the current simulation time.")
   (:method((channel ideal-channel)) nil)
   (:method(channel) (< (simulation-time) (transmission-finish-time channel))))

(defgeneric (setf transmission-finish-time)(channel time)
  (:documentation "* Arguments
- channel :: a [[transmission-channel]]
- time :: a [[time-type]]

* Returns
- time :: a [[time-type]]

* Description

For transmission channels: Forcibly overwrites the
finish time of the current transmission in the channel (see
[[transmission-finish-time]]).

This method is a crude device that allows for implementing aborting
transmissions; it is not needed for normal packet transmissions.
Calling this method with the current simulation time will allow
you to immediately send another packet on the channel without the
channel reporting error due to its being busy.

Note that this call does NOT affect the delivery of the packet being
transmitted: the packet object is delivered to the target module at
the time it would without the call to this method. The sender needs to
inform the target module in some other way that the transmission was
aborted and the packet should be treated accordingly `i.e. discarded
as incomplete; for example by sending an out-of-band cMessage that the
receiver has to understand."))

(defmethod info((channel channel))
  (format nil "~{~A=~A ~}"
          (mapcan
           #'(lambda(slot)
               (when (typep slot 'parameter-slot)
                 (let ((name (slot-definition-name slot)))
                   (when (slot-boundp channel name)
                     (list name (slot-value channel name))))))
           (class-slots (class-of channel)))))

;; this is actually wrong (parent-module is needed before connection) and in
;; addition owner is parent module for  all cases so default is fine.
;; (defmethod parent-module((channel channel))
;;   "For channels - if source is an output then parent is module
;; producing output - else we are connected an iput gate to something and
;; our parent is the gate parent."
  ;; (let ((gate (source-gate channel)))
  ;;   (when gate
  ;;     (if (eql (gate-direction gate) :input)
  ;;         (owner gate) ;; parent is source
  ;;         (owner (owner gate))))))

(defclass delay-channel(channel)
  ((delay :type time-type :initform 0.0d0 :accessor delay :parameter t
          :initarg :delay :documentation "The propagation delay in seconds")
   (disabled-p :type bool :initform nil :accessor disabled-p :parameter t
             :initarg :disabled :documention "If true packets are discarded"))
  (:metaclass parameter-class)
  (:documentation "A [[channel]] with propagation delay."))

(register-signal 'message-sent)
(register-signal 'message-discarded)

(defstruct message-sent-signal-value
  "* Slots
- timestamp :: a [[time-type]] - the time message was sent
- message :: a [[message]] being sent.
- result :: a [[channel-result]]

* Description

Structure used to to pass information on =message-sent= signal
conataining the time it was sent, the message and the channel result."
  (timestamp (simulation-time) :type time-type)
  message
  (result (make-channel-result) :type channel-result))

(defmethod process-message((channel delay-channel) message time)
  "For a delay channel we discard messages if channel is disable,
otherwise successfully send with specified delay."
  (cond
    ((disabled-p channel)
     (emit channel 'message-discarded (make-timestamped :value message))
     (make-channel-result :discard t))
    (t
     (let ((result (make-channel-result :delay (delay channel))))
       (when (may-have-listeners channel (signal-id 'message-sent))
         (emit channel 'message-sent
               (make-message-sent-signal-value
                :message message :result result)))
       result))))