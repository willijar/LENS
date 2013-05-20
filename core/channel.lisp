(in-package :lens)

(defclass channel(component)
  ((source-gate :initarg :source-gate :accessor source-gate))
  (:metaclass parameter-class)
  (:documentation "Base class for channels"))

(defclass ideal-channel(channel)
  ()
  (:metaclass parameter-class)
  (:documentation "Channel with zero propagation delay, zero
  transmission delay (infinite datarate), and always enabled."))

(defclass transmission-channel(channel)
  ()
  (:metaclass parameter-class)
  (:documentation "base classe for all transmission channels"))

(defstruct channel-result
  (delay 0.0 :type time-type)
  (duration 0.0 :type time-type)
  (discard nil :type boolean))

(defgeneric process-message(channel message time)
  (:documentation "This method encapsulates the channel's
  functionality. The method should model the transmission of the given
  message starting at the given t time, and return the propagation
  delay, transmission duration, and discard flag in the channel-result
  object.

Transmission duration and bit error modeling only applies to packets
i.e. to instances of cPacket, where cMessage's isPacket() returns true,
it should be skipped for non-packet messages. The method does not need
to call the setDuration method on the packet; this is
done by the simulation kernel. However, the method should call
setBitError(true) on the packet if error modeling results
 in bit errors.

If the method sets the discard flag in the result object, it means
that the message object should be deleted by the simulation kernel;
this facility can be used to model that the message gets lost in the
channel.

The method does not need to throw error on overlapping transmissions,
or if the packet's duration field is already set; these checks are
done by the simulation kernel before processMessage() is called.")
  (:method(channel message time) nil))

(defgeneric nominal-datarate(channel)
  (:documentation "For transmission channels: Returns the nominal data
  rate of the channel. The number returned from this method should be
  treated as informative; there is no strict requirement that the
  channel calculates packet duration by dividing the packet length by
  the nominal data rate. For example, specialized channels may add the
  length of a lead-in signal to the duration.")
  (:method(channel) 0))

(defgeneric calculate-duration(channel message)
  (:documentation "For transmission channels: Calculates the
transmission duration of the message with the current channel
configuration (datarate, etc); it does not check or modify channel
state. For non-transmission channels this method returns zero.

This method is useful for transmitter modules that need to determine
the transmission time of a packet without actually sending the packet.

Caveats: this method is best-effort -- there is no guarantee that
transmission time when the packet is actually sent will be the same as
the value returned by this method. The difference may be caused by
changed channel parameters (i.e. datarate being overwritten), or by a
non-time-invariant transmission algorithm.

Note that there is no requirement that processMessage() relies on this
method to calculated the packet duration. That is, to change the
duration computation algorithm via subclassing you need to redefine
BOTH the processMessage() and calculateDuration() methods.")
  (:method(channel message) (declare (ignore message)) 0))

(defgeneric transmission-finish-time(channel)
  (:documentation "For transmission channels: Returns the simulation time
the sender gate will finish transmitting. If the gate is not
currently transmitting, the result is unspecified but less or equal
the current simulation time.")
  (:method(channel) (simulation-time)))

(defgeneric busy-p(channel)
  (:documentation "For transmission channels: Returns whether the sender gate
is currently transmitting, ie. whether transmission-finish-time
is greater than the current simulation time.")
   (:method((channel ideal-channel)) nil)
   (:method(channel) (< (simulation-time) (transmission-finish-time channel))))

(defgeneric (setf transmission-finish-time)(channel time)
  (:documentation "For transmission channels: Forcibly overwrites the
finish time of the current transmission in the channel (see
transmission-finish-time).

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
          :initarg :delay :documentation "Delay in seconds")
   (disabled-p :type bool :initform nil :accessor disabled-p :parameter t
             :initarg :disabled :documention "If true packets are discarded"))
  (:metaclass parameter-class)
  (:documentation "Channel with propagation delay."))

(register-signal 'message-sent)
(register-signal 'message-discarded)

(defstruct message-sent-signal-value
  (timestamp (simulation-time) :type time-type)
  message
  (result (make-channel-result) :type channel-result))

(defmethod process-message((channel delay-channel) message time)
  (cond
    ((disabled-p channel)
     (emit channel 'message-discarded (make-timestamped :value message))
     (make-channel-result :discard t))
    (t
     (let ((result (make-channel-result :delay (delay channel))))
       (when (may-have-listeners channel (signal-id 'message-sent))
         (emit channel 'message-sent
               (make-message-sent-signal-value
                :message  message :result result)))
       result))))