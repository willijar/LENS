(in-package :lens.wsn)

(deftype radio-control-command-name()
  '(member set-state set-mode set-tx-output set-sleep-level set-carrier-freq
          set-cca-threshold set-cs-interrupt-on set-cs-interrupt-off
          set-encoding))

(deftype radio-state () '(member rx tx sleep))

;; signals for statistic collection
(register-signal 'tx
                 "Transmissions")
(register-signal 'rx-succeed-no-interference
                 "Successfully Received packets")
(register-signal 'rx-succeed-interference
                 "Successfully Received packets despite interference")
(register-signal 'rx-fail-no-interference
                 "packets failed even without interference")
(register-signal 'rx-fail-interference
                 "packets failed with interference")
(register-signal 'rx-fail-sensitivity
                 "packets failed, below sensitivity")
(register-signal 'rx-fail-modulation
                 "packets failed, wrong modulation")
(register-signal 'rxfail-no-rx-state
                 "packets failed, radio not in RX")

(deftype modulation-type()
  '(member ideal fsk psk dbpsk dqpsk))

(deftype collision-model-type()
  '(member
    no-interference-no-collisions
    simple-collision-model
    additive-interference-model
    complex-interference-model))

(deftype encoding-type() '(member nrz code-4b5b manchester secdec))

(defstruct rx-mode
  (name nil :type symbol)
  (data-rate 0.0 :type double-float)
  (modulation 'ideal :type (or modulation-type array))
  (bits-per-symbol 1 :type fixnum)
  (bandwidth 0.0 :type double-float)
  (noise-bandwidth 0.0 :type double-float)
  (noise-floor 0.0 :type double-float)
  (sensitivity 0.0 :type double-float)
  (power 0.0 :type double-float))

(defstruct received-signal
  (id 0 :type fixnum)
  (power-dbm 0.0 :type double-float)
  (modulation 'ideal :type modulation-type)
  (encoding 'nrz :type encoding-type)
  (current-interference 0.0 :type double-float) ;; in dbm
  (max-interference 0.0 :type double-float) ;; in dbm
  (bit-errors 0 :type (or fixnum t)))

(defstruct total-power-received
  (power-dbm 0.0 :type double-float)
  (start-time 0.0 :type time-type))

(defstruct transition-element
  (delay 0.0 :type time-type)
  (power 0.0 :type double-float)) ;; in mW

(defstruct sleep-level
  (name nil :type symbol)
  (power 0.0 :type double-float)
  (up (make-transition-element) :type (or transition-element nil))
  (down (make-transition-element) :type (or transmition-element nil)))

(defstruct tx-level
  (output-power 0.0 :type double-float) ;; in dbm
  (power-consumed 0.0 :type double-float)) ;; in W

(defstruct custom-modulation ;; element for storing custom snrtober.
  (snr 0.0 :type double-float)
  (ber 0.0 :type double-float))

(deftype cca-result() '(member clear busy cs-not-valid cs-not-valid-yet))

(defclass radio(comms-module)
  ((address :parameter t :type integer :reader mac-address
            :documentation "MAC address - will default to node index.")
   (radio-parameters-file
    :parameter t :type string :reader radio-parameters-file
    :documentation "the file that contains most radio parameters")
   (mode :parameter t :initform nil :reader mode
         :documentation "we can choose a mode to begin with. Modes are
         defined in the RadioParametersFile. Empty string means use
         the first mode defined)")
   (state :type symbol :parameter t :initform 'rx :accessor state
          :documentation "we can choose a radio state to begin
          with. RX and TX are always there. according to the radio
          defined we can choose from a different set of sleep states")
   (tx-output-power
    :type symbol :parameter t :initform nil :accessor tx-output-power
    :documentation "we can choose a Txpower to begin with. Possible tx
    power values are defined in the RadioParametersFile. Empty string
    means use the first tx power defined (which is also the highest)")
   (sleep-level
    :type symbol :parameter t :initform nil :accessor sleep-level
    :documentation "we can choose a sleep level which will be used
    when a transition to SLEEP state is requested. Empty string means
    use first level defined (will usually be the fastest and most
    energy consuming sleep state)")
   (carrier-frequency
    :type double :parameter t :initform 2.4E9 :accessor carrier-frequency
    :documentation "the carrier frequency (in Hz) to begin with.")
   (encoding :type encoding-type :reader encoding)
   (collision-model
    :type symbol :parameter t :initform 'additive-interference-model
    :reader collision-model
    :documentation "none, simple, additive or advance interference")
   (cca-threshold
    :type real :parameter t :initform -95 :reader cca-threshold
    :documentation "the threshold of the RSSI register (in dBm) were
    above it channel is NOT clear")
   (symbols-for-rssi
    :type integer :parameter t :initform 8 :reader symbols-for-rssi)
   (carrier-sense-interrupt-enabled
    :type boolean :parameter t :initform nil
    :reader carrier-sense-interrupt-enabled)
   (max-phy-frame-size
    :initform 1024 :type integer :parameter t :reader max-phy-frame-size
    :properties (:units "B")
    :documentation "in bytes")
   (header-overhead :initform 6 :documentation "in bytes - 802.15.4=6bytes")
   (avg-busy-frame
    :type real :initform 1 :parameter t :reader avg-busy-frame
    :documentation "integration time for measuring avg busy time")
   (avg-busy :type real :initform 0 :accessor avg-busy)
   (buffer-size :initform 16)
   (wireless-channel :type gate :reader wireless-channel
                     :documentation "Gate to directly send wireless messages
               to. Messages from the wireless layer will be send
               direct to fromWireless input gate in radio module")
   ;; these are derived from radio-parameters file or ini file
   (tx-levels :type list :reader tx-levels)
   (rx-modes :type list :reader rx-modes)
   (sleep-levels :initform nil :type list :reader sleep-levels)
   (transitions
    :type list :reader transitions
    :documentation "Power and delay for transitions between sleep states")
   (tx-level :type tx-level :accessor tx-level)
   (rx-mode :type rx-mode :accessor rx-mode)
   (sleep-level :type sleep-level :accessor sleep-level)
   (state :type radio-state :initform 'rx :accessor state)
   (last-transition-time
    :type time-type :initform 0.0 :reader last-transition-time)
   (changing-to-state
    :type radio-state :initform nil :accessor changin-to-state
    :documentation "indicates that the Radio is in the middle of changing to
							a new state")
   (received-signals
    :initform nil :type list :accessor received-signals
    :documentation " a list of signals curently being received")
   (time-of-last-signal-change
    :initform 0.0 :type time-type :accessor time-of-last-signal-change
    :documentation "last time the above list changed")
   (total-power-received
    :initform nil :type list :accessor total-power-received
    :documentation " a history of recent changes in total received power to help calculate RSSI")
   (rssi-integration-time
    :initform 1.0 :type time-type :reader rssi-integration-time
    :documentation "span of time the total received power is integrated to calculate RSSI")
   (cs-interrupt-message
    :type radio-control-message :reader cs-interrupt-message
    :initform (make-instance 'radio-control-message
                             :command 'carrier-sense-interrupt)
    :documentation " message that carries a future carrier sense interrupt")
   (latest-cs-interrupt-time
    :initform 0.0 :type time-type :accessor latest-cs-interrupt-time)
   (state-transition-message
    :type message :reader state-transition-message
    :initform (make-instance 'message :name 'state-transition)
    :documentation "Self message to complete state transmisition")
   (state-after-tx :initform 'rx :type radio-state :reader state-after-tx))
  (:gates
   (mac :inout)
   (fromWireless :input))
  (:properties
   :statistic (tx
               :title "Transmissions" :default (count))
   :statistic (rx-succeed-no-interference
               :title "Successfully Received packets"  :default (count))
   :statistic (rx-succeed-interference
               :title "Successfully Received packets despite interference"
               :default (count))
   :statistic (rx-fail-no-interference
               :title "packets failed even without interference"
               :default (count))
   :statistic (rx-fail-interference
               :title "packets failed with interference" :default (count))
   :statistic (rx-fail-sensitivity
               :title "packets failed, below sensitivity" :default (count))
   :statistic (rx-fail-modulation
               :title "packets failed, wrong modulation" :default (count))
   :statistic (rxfail-no-rx-state
               :title "packets failed, radio not in RX" :default (count)))
  (:metaclass module-class))

(defmethod initialize-instance :after ((radio radio) &key &allow-other-keys)
  (unless (slot-boundp instance 'address)
    (setf (slot-value instance 'address) (index (node instance))))
  (parse-radio-parameter-file radio))

(defmethod initialize((radio radio) &optional (stage 0))
  (case stage
    (0
     (setf (last-transition-time radio) 0.0)
     (complete-state-transition radio) ;; complete initialisation according to starting state
     ))
  (call-next-method))


(defun parse-radio-parameter-file(radio)
  (let ((parameters
         (with-open-file(is (merge-pathnames (radio-parameters-file radio)) :direction :input :if-does-not-exist :error)
           (read is))))
    (dolist(record parameters)
      (let((args (rest record)))
        (assert (not (slot-boundp radio (car record)))
                ()
                "Duplicate ~A specification in radio parameters file" (car record))
        (ecase (car record)
          (rx-modes
           (assert (every #'rx-mode-p args)
                   ()
                   "Invalid rx modes specification in radio parameters file")
           (setf (slot-value radio 'rx-modes) args))
          (tx-levels
           (assert (every #'tx-level-p args)
                   ()
                   "Invalid tx levels specification in radio parameters file")
           (setf (slot-value radio 'tx-levels) args))
          (sleep-levels
           (assert (every #'sleep-level-p args)
                   ()
                   "Invalid sleep levels specification in radio parameters file")
           (setf (slot-value radio 'sleep-levels) args))
          (transitions
           (let ((sec '(rx tx sleep)))
             (loop :for to :in sec
                (loop :for from in sec
                   :unless (eql from to)
                   :for e = (getf args (getf args to) from)
                   :do (assert (transition-element-p e))
                           ()
                   "Invalid state transition ~A in radio parameters file" e)))
           (setf (slot-value radio 'transitions) args)))))))

(defmethod startup((radio radio))
  (setf (time-of-last-signal-change radio) (simulation-time))
  (push (make-total-power-received
         :start-time (simulation-time)
         :power-dbm (rx-mode-noise-floor (rx-mode radio)))
        (total-power-received radio)))

(defmethod handle-message((radio radio) (message wireless-signal-start))
  ;; if carrier doesn't match ignore messge
  (unless (= (frequency message) (carrier-frequency message))
    (return-from handle-message))
  ;; if we are not in RX state or we are changing state, then process
  ;; the signal minimally. We still need to keep a list of signals
  ;; because when we go back in RX we might have some signals active
  ;; from before, acting as interference to the new (fully received
  ;; signals)
  (when (or (changing-to-state radio) (not (eql (state radio) 'rx)))
    (push
     (make-received-signal
      :id (nodeid (fromModule message))
      :power-dbm (power-dbm message)
      :bit-errors t)
     (received-signals radio))
    (emit radio 'tx-fail-no-rx-state)
    (return-from handle-message))
  ;; If we are in RX state, go throught the list of received signals
  ;; and update bitErrors and currentInterference
  (dolist(signal (received-signals radio))
    (let ((bit-errors (received-signal-bit-errors message)))
      ;; no need to update bitErrors for an element which will not be received
    (unless (and (numberp bit-errors)
                 (<= bit-errors
                     (max-errors-allowed radio
                                         (received-signal-encoding message))))
      (let ((num-of-bits
             (ceiling (* (rx-mode-data-rate (rx-mode radio))
                         (- (simulation-time)
                            (time-of-last-signal-change radio)))))
            (ber (snr2ber radio
                          (- (received-signal-power-dbm signal)
                             (current-interference signal)))))
        (incf (received-signal-bit-errors signal)
              (bit-errors
               ber num-of-bits
               (max-errors-allowed radio (received-signal-encoding message))))
        (update-interference signal message)))))
  (let* ((rx-mode (rx-mode radio))
         (new-signal
          (make-received-signal
           :id (nodeid (fromModule message))
          :power-dbm (power-dbm message)
          :modulation (modulation message)
          :encoding (encoding message)
          :current-interference
          (ecase (collision-model radio)
            (no-interference-no-collisions
             (rx-mode-noise-floor rx-mode))
            (additive-interference-model
             (total-power-received-power-dbm
              (first (total-power-received radio))))
            (simple-collision-model
             (if (> (total-power-received-power-dbm
                     (first (total-power-received radio)))
                    (rx-mode-noise-floor rx-mode))
                 0.0
                 (rx-mode-noise-floor rx-mode))))))
            #+nil(complex-interference-model
                  (rx-mode-noise-floor (rx-mode radio))))
    (setf (received-signal-max-interference new-signal)
          (received-signal-current-interference new-signal))
    (when (not (eql (rx-mode-modulation rx-mode)
                    (received-signal-modulation new-signal)))
      (setf (received-signal-bit-errors new-signal) t)
      (emit radio 'rx-fail-sensitivity))
    (when (< (received-signal-power-dbm new-signal)
             (rx-mode-sensitivity rx-mode))
      (setf (received-signal-bit-errors new-signal) t)
      (emit radio 'rx-fail-modulation))
    (push new-signal (received-signals radio))
    (update-total-power-received radio (received-signal-power-dbm new-signal))
    (when (and (carrier-sense-interrupt-enabled radio)
               (> (received-signal-power-dbm new-signal) (cca-threshold radio)))
      (update-possible-cs-interrupt radio))
    (setf (time-of-last-signal-change radio) (simulation-time))))

(defmethod handle-message((radio radio) (message wireless-signal-end))
  (let ((ending-signal (find (node (node message)) (received-signals radio)
                             :key #'node)))
    (unless ending-signal
      (event-log "End signal ignored - mo matxching start signal")
      (return-from handle-message))
    ;; if not in RX state or are changin state just delete signal
    (when (or (changing-to-state radio) (not (eql (state radio) 'rx)))
      (when (numberp (received-signal-bit-errors ending-signal))
        (emit radio 'rx-fail-no-rx-state))
      (setf (received-signals radio)
            (delete ending-signal (received-signals radio)))
      (return-from handle-message))
    ;; if in rx state update received signals
    (dolist(signal (received-signals radio))
      (let ((bit-errors (received-signal-bit-errors signal))
            (max-errors-allowed
             (max-errors-allowed
              radio (received-signal-encoding message))))
        ;; no need to update bitErrors for an element which will not be received
        (when (and (numberp bit-errors) (<= bit-errors max-errors-allowed))
          (let ((num-of-bits
                 (ceiling (* (rx-mode-data-rate (rx-mode radio))
                             (- (simulation-time)
                                (time-of-last-signal-change radio)))))
                (ber (snr2ber (rx-mode radio)
                              (- (received-signal-power-dbm signal)
                                 (current-interference signal)))))
            (incf (received-signal-bit-errors signal)
                  (bit-errors ber num-of-bits max-errors-allowed))
            (unless (eql signal ending-signal)
              (update-interference signal message))))))
    (update-total-power-received radio ending-signal)
    (setf (time-of-last-signal-change radio) (simulation-time))
    (when (numberp (received-signal-bit-errors ending-signal))
      (cond
        ((<= (received-signal-bit-errors end-signal)
             (max-errors-allowed radio (received-signal-encoding signal)))
         (let* ((mac-pkt (decapsulate message))
                (info (control-info mac-pkt)))
           (setf (mac-radio-control-info-rssi info) (read-rssi radio)
                 (mac-radio-control-info-lqi info)
                 (- (received-signal-power-dbm ending-signal)
                    (received-signal-power-max-interference signal)))
           (send radio mac-pky 'mac (processing-delay radio)))
         (if (= (received-signal-max-interference ending-signal)
                (rx-mode-noise-floor (rx-mode radio)))
             (emit radio 'rx-succeed-no-interference)
             (emit radio 'rx-succeed-interference)))
        ((= (received-signal-max-interference ending-signal)
            (rx-mode-noise-floor (rx-mode radio)))
         (emit radio 'rx-fail-no-interference))
        (t (emit radio 'rx-fail-interference))))
    (setf (received-signals radio)
          (delete ending-signal (received-signals radio)))))

(defmethod handle-message((radio radio) (message message))
  (cond
    ((eql message (state-transition-message radio))
     (complete-state-transition radio))
    (t (call-next-method))))

(defmethod handle-message((radio radio) (message radio-control-command))
  (ecase (command message)
    (set-state
     (with-slots(state changing-to-state) radio
     ;; If we are asked to change to the current (stable) state,
     ;; or the state we are changing to anyway, do nothing
     (when  (eql (argument message)
                 (or (changing-to-state radio) state))
       (return-from handle-message))
     ;; If we are asked to change from TX, and this is an external
     ;; command, and we are not changing already, then just record the
     ;; intended target state. Otherwise proceed with the change.
     (when (and (eql state 'tx) (not (self-message-p message))
                (not changing-to-state))
       (setf (state-after-tx radio) (argument message))
       (return-from handle-message))
     (setf changing-to-state (argument message))
     (let* ((transition (getf (getf (transitions radio) changing-to-state)
                             state))
            (transition-delay (transisiton-element-delay transition))
            (transition-power (transition-element-power transition)))
       ;; With sleep levels it gets a little more complicated. We
			 ;; can add the trans delay from going to one sleep level to the
			 ;; other to get the total transDelay, but the power is not as
			 ;; easy. Ideally we would schedule delayed powerDrawn messages
			 ;; as we get from one sleep level to the other, but what
			 ;; happens if we receive another state change message? We would
			 ;; have to cancel these messages. Instead we are calculating
			 ;; the average power and sending one powerDrawn message. It
			 ;; might be a little less accurate in rare situations (very
			 ;; fast state changes), but cleaner in implementation.
       (flet((accumulate-level(level direction)
               (let ((transition (funcall direction level))
                     (level-delay (transisiton-element-delay transition))
                     (level-power (transition-element-power transition)))
                 (set transiton-power
                      (/ (+ (* transition-power transition-delay)
                            (* level-power level-delay))
                         (+ transition-delay level-delay))
                      transition-delay (+ transition-delay level-delay)))))
         (cond
           ((eql state 'sleep)
            (loop :for a :on (reverse (sleep-levels radio))
               :until (eql (car a) (sleep-level radio))
               :finally
               (loop :for b :on a
                  :do (accumulate-level (car b) #'sleep-level-up))))
           ((eql changing-to-state 'sleep)
            (loop :for a :on (sleep-levels radio)
               :until (eql (car a) (sleep-level radio))
               :do (accumulate-level (car a) #'sleep-level-down)))))
       (eventlog "Set state to ~A, delay=~A, power=~A"
                 changing-to-state transition-delay transition-power)
       (emit radio 'power-drawn transition-power)
       (delay-state-transition radio transition-delay))))
    (set-mode
     (with-slots(rx-mode rx-modes rssi-integration-time symbols-for-rssi state)
         radio
       (setf rx-mode (find (argument message) rx-modes :key rx-mode-name))
       (assert rx-mode()
               "No radio mode named ~A found" (argument message))
       (eventlog "Changed rx mode to ~A" rx-mode)
       (setf rssi-integration-mode
             (* symbols-for-rssi
                  (/ (rx-mode-bits-per-symbol rx-mode)
                     (rx-mode-data-rate rx-mode))))
       ;; if in rx mode change drawn power
       (when (eql state 'rx)
         (emit radio 'power-drawn (rx-mode-power rx-mode)))))
    (set-tx-output
     (with-slots(tx-level tx-levels) radio
       (setf tx-level (find (argument message) tx-levels :key #'tx-level-output-power))
       (assert tx-level ()
               "Invalid tx output power level ~A" (argument message))
       (eventlog "Changed TX power output to ~A dBm consuming ~A W"
                 (tx-level-output-power tx-level)
                 (tx-level-power-consumed tx-level))))
    (set-sleep-level
     (with-slots(sleep-level sleep-levels) radio
       (setf sleep-level (find (argument message) sleep-levels
                               :key #'sleep-level-name))
       (assert sleep-level ()
               "Invalid sleep level ~A" (argument-message))
       (eventlog "Changed default sleep level to ~A"
                 (sleep-level-name sleep-level))))
    (set-carrier-freq
     (with-slots(carrier-frequency) radio
       (setf carrier-frequency (argument message))
       (eventlog "Changed carrier frequency to ~A Hz" carrier-frequency)
       ;; clear received signals as they are no longer valid and don't create
       ;; interference with the new incoming signals
       (setf (received-signals radio) nil)))
    (set-cca-threshold
     (with-slots(cca-threshold) radio
       (setf cca-threshold (argument message))
       (eventlog "Changed CCA thresold to ~A dBm" cca-threshold)))
    (set-cs-interrupt-on
     (setf (slot-value radio 'carrier-sense-interrupt-enabled) t)
     (eventlog "CS interrupt turned ON"))
    (set-cs-interrupt-off
     (setf (slot-value radio 'carrier-sense-interrupt-enabled) nil)
     (eventlog "CS interrupt turned OFF"))
    (set-encoding
     (with-slots(encoding) radio
       (setf encoding (argument message))
       (assert (typep encoding encoding-type)
               ()
               "Invalid encoding type ~A" encoding)))))

(defun delay-state-transition(radio delay)
  (let ((msg (state-transition-message radio)))
    (when (scheduled-p msg)
      (eventlog "WARNING: command to change to a new state was received before previous state transition was completed")
      (cancel msg))
    (schedule-at radio msg :delay delay)))

(defun complete-state-transition(radio)
  (with-slots(state changing-to-state last-transition-time
                    avg-busy avg-busy-frame) radio
    (when (and (or (eql changing-to-state 'sleep) (eql state 'sleep))
               (not (eql state changing-to-state)))
      (let* ((now (simulation-time))
             (ratio (min 1 (/ (- now last-transition-time) avg-busy-frame))))
        (setf avg-busy (* avg-busy (1- ratio)))
        (when (not (eql state 'sleep))
          (incf avg-busy ratio))
        (setf last-transition-time now)))
    (setf state changing-to-state)
    (eventlog "completing transition to ~A" state)
    (setf changing-to-state nil)
    (ecase state
      (tx
       (cond
         ((empty-p (buffer radio))
          ;; just changed to TX but buffer empty so send command to change to rx
          (schedule-at radio
                       (make-instance 'radio-command-message
                                      :command 'set-state
                                      :argument 'rx))
          (eventlog "WARNING: just changed to TX but buffer is empty - changing to RX"))
         (t
          (let ((time-to-tx-packet (dubuffer-and-send radio)))
            (emit radio 'power-drawn (tx-level-power-consumed (tx-level radio)))
            ;; flush received power history
            (setf (total-power-received radio) nil)
            (schedule-at module
                         (make-instance 'message
                                        :name radio-continue-tx)
                         :delay time-to-tx-packet)))))
      (tx
       (emit radio 'power-drawn (rx-mode-power (rx-mode radio)))
       (update-total-power-received radio nil))
      (sleep
       (emit radio 'power-drawn (sleep-level-power (sleep-level radio)))
       (setf (total-power-received radio) nil)))))

(defun debuffer-and-send(radio)
  (let* ((mac-pkt (dequeue (buffer radio)))
         (begin
          (make-instance 'wireless-signal-start
                         :node (node radio)
                         :power-dbm (tx-level-output-power (tx-level radio))
                         :carrier-frequency (carrier-frequency radio)
                         :bandwidth (rx-mode-bandwidth (rx-mode radio))
                         :modulation-type (rx-mode-modulation-type (rx-mode radio))
                         :encoding-type (encoding radio)))
         (end (make-instance 'wireless-signal-end
                             :node (node radio)
                             :byte-length (header-overhead radio))))
    (encapsulate end mac-pkt)
    (let ((tx-time (/ (bit-length end) (rx-mode-data-rate (rx-mode radio)))))
      (send radio begin (wireless-channel radio))
      (send radio end (wireless-channel radio) :delay tx-time)
      (emit radio 'tx)
      (eventlog "Sending packet, transmission will last ~A secs" tx-time)
      tx-time)))

(defgeneric update-total-power-received(radio value)
  (:documentation "Update the history of total power received."))

(defmethod update-total-power-received(radio (new-power number))
  (push
   (make-total-power-received
    :start-time (simulation-time)
    :power-dbm (add-power-dbm
                (total-power-received-power-dbm
                 (car (total-power-received radio)))
                new-power))
   (total-power-received radio)))

(defmethod update-total-power-received(radio (ending-signal received-signal))
  (let ((p (rx-mode-noise-floor (rx-mode radio))))
    (dolist(received-signal (received-signals radio))
      (setf p (add-power-dbm p (received-signal-power-dbm received-signal))))
    (push
     (make-total-power-received
      :start-time (simulation-time)
      :power-dbm p)
     (total-power-received radio))))

(defmethod update-total-power-received(radio (dummy (eql nil)))
  (let((p (rx-mode-noise-floor (rx-mode radio))))
    (dolist(received-signal (received-signals radio))
      (setf p (add-power-dbm p (received-signal-power-dbm received-signal)))
      (when (numberp (received-signal-bit-errors received-signal))
        (setf (received-signal-bit-errors received-signal) t)
        (emit radio 'rx-fail-no-rx-state)
        (eventlog "Just entered RX, existing signal from ~A cannot be received." (id received-signal))))
        (push
         (make-total-power-received
          :start-time (simulation-time)
          :power-dbm p)
         (total-power-received radio))))

(defgeneric update-interference(radio received-signal arg)
  (:documentation " Update interference of one element in the receivedSignals list."))

(defmethod update-interference(radio
                               (received-signal received-signal)
                               (msg wireless-channel-signal-begin))
  (ecase (collision-model radio)
    (no-interference-no-collisions );; do nothing
    (simple-collision-model
     ;; an arbritrary rule: if the signal is more than 6dB less than
     ;; sensitivity, intereference is considered catastrophic.
     (when (> (power-dbm msg) (- (rx-mode-sensitivity (rx-mode radio)) 6.0))
       ;; corrupt signal and set interference to a large value
       (setf (received-signal-bit-errors received-signal)
             (1+ (max-errors-allowed
                  radio
                  (received-signal-encoding received-signal)))
             (received-signal-max-interference received-signal) 0.0)))
    (additive-interference-model
     (setf (received-signal-current-interference received-signal)
           (add-power-dbm
            (received-signal-current-interference received-signal)
            (power-dbm msg)))
     (setf (received-signal-max-interference received-signal)
           (max (received-signal-current-interference received-signal)
                (received-signal-max-interference received-signal))))
    #+nil(complex-interference-model )))

(defmethod update-interference(radio
                               (remaining-signal received-signal)
                               (ending-signal received-signal))
  (ecase (collision-model radio)
    (no-interference-no-collisions );; do nothing
    (simple-collision-model
     ;; do nothing - this signal corrupted/destroyed other signals already
     )
    (additive-interference-model
     (setf (received-signal-current-interference remaining-signal)
           (rx-mode-noise-floor (rx-mode radio)))
     (dolist(it (received-signals radio))
       (unless (or (eql it remaining-signal) (eql it enging-signal))
         (setf (received-signal-current-interference remaining-signal)
               (add-power-dbm
                (received-signal-current-interference remaining-signal)
                (received-signal-current-interference it))))))
    #+nil(complex-interference-model
                  )))

(defun read-rssi(radio)
  (unless (eql (state radio) 'rx) (return-from read-rssi 'cs-not-valid))
  (let* ((rssi -200.0)
         (current-time (simulation-time))
         (rssi-integration-time (rssi-integration-time radio))
         (limit-time (- current-time rssi-integration-time))
         (remaining (total-power-received-radio)))
    (loop
         (when (or (<= current-time limit-time) (not remaining)) (return))
       (let* ((it (car remaining))
              (fraction-time (- current-time
                                (/ (max (total-power-received-start-time it)
                                        limit-time)
                                   rssi-integration-time))))
         (setf rssi (add-power-dbm
                     (+ (total-power-received-power-dbm it)
                        (radio-to-db fraction-time))
                     rssi))
         (setf current-time  (total-power-received-start-time it)))
       (setf remaining (cdr remaining)))
    ;; if not rx long enough return error code
    (when (> current-time limit-time) (return-from read-rssi 'cs-not-valid-yet))
    (when (<= rssi-integration-time 0)
       ;; special case for naive model - current total signal power returned
       (setf rssi (total-power-received-power-dbm (car remaining)))
       (setf remaining (cdr remaining)))
    ;; erase rest of elements that are out of date
    (when remaining (setf (cdr remaining) nil))
    rssi))

(defun update-possible-cs-interrupt(radio)
 ;; A method to calculate a possible carrier sense in the future and
 ;; schedule a message to notify layers above. Since the received
 ;; power history is expressed in dBm, exact computations are
 ;; complex. Instead we assume that all previous received power is
 ;; negligibly small and given the current power, CCAthreshold and
 ;; averaging/integration time for RSSI.
  (when (> (read-rssi radio) (cca-threshold radio))
    ;; if we are above the threshold, no point in scheduling an interrupt
    (return-from update-possible-cs-interrupt))
  ;;if we are going to schedule an interrupt, cancel any future CS interrupt
  (cancel (cs-interrupt-message radio))
  	;; We calculate the fraction of the RSSI averaging time that it
  	;; will take for the current power to surpass the CCA
  	;; threshold. This is based on how many times larger is the
  	;; current time from the CCAthreshold. E.g., if it is 2 times
  	;; larger the fraction is 1/2, if it is 8, the fraction is 1/8
  (let ((fraction-time
         (/ 1.0
            (db-to-ratio
             (- (total-power-received-power-dbm
                 (car (total-power-received radio)))
                (cca-thresold radio))))))
	;;  we might adjust the fraction based on symbolsForRSSI. E.g. if
	;; symbolsForRSSI is 4 and we get 1/8 then we might adjust it to
	;; 1/4. We do not have enough  details for the RSSI model
	;; calculation though.

	;; // schedule message
    ;; TODO - replace latest-cs-interrupt-time with message arrival time?
    (setf (latest-cs-interrupt-time radio)
          (+ (simulation-time) (processing-delay radio)
             (* fraction-time (rssi-integration-time radio))))
    (schedule-at radio (cs-interrupt-message radio)
                 :time (latest-cs-interrupt-time radio))))

(defmethod snr2ber(rx-mode snr-db &optonal bpnb)
  (declare (ignore bpnb))
  (snr2ber (modulation rx-mode) snr-db
           (/ (rx-mode-data-rate rx-mode)
              (rx-mode-noise-bandwidth rx-mode))))

(defun is-channel-clear(radio)
  (let ((value (read-rssi radio)))
    (if (symbolp value)
        value
        (if (< value cca-threshold) t nil))))

(defun bit-errors(ber num-of-bits max-bit-errors-allowed)
  (do((bit-errors 0.0 (1+ bit-errors))
      (prob
       (probability-of-exactly-n-errors ber bit-errors num-of-bits)
       (probability-of-exactly-n-errors ber bit-errors num-of-bits))
      (c 0.0 (+ c prob))) ;cumulativeProbabilityOfUnrealizedEvents
     ((or (> bit-errors max-bit-errors-allowed)
          (<= (lens::%gendblrand 0) (/ (- 1.0 c))))
      (if (= bit-errors max-bit-errors-allowed) (1+ bit-errors) bit-errors))))

(defun max-tx-power-consumed(radio)
  (reduce #'max (mapcar #'tx-level-power-consumed (tx-levels radio))))







