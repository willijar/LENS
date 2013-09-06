(in-package :lens.wsn)

(defpackage :lens.wsn.mac.tuneable
  (:use :cl :cl-user :lens :lens.wsn)
  (:export #:tuneable-mac
           #:sleep-interval #:constant #:multiplying #:exponential
           #:mac-packet-breakdown))

(in-package :lens.wsn.mac.tuneable)

(deftype tuneable-mac-state() '(member default tx contending rx))

(deftype backoff-type()
  '(member sleep-interval constant multiplying exponential))

(defclass tuneable-mac(mac)
  ;; The main tuneable parameters
  ((duty-cycle
    :parameter t :type float :initform 1.0 :reader duty-cycle
    :documentation  "listening / (sleeping+listening)")
   (listen-interval
    :parameter t :type time-type :initform 10e-3 :reader listen-interval
    :documentation "how long do we leave the radio in listen mode, in ms")
   (beacon-interval-fraction
    :parameter t :type float :initform 1.0 :accessor beacon-interval-fraction
    :documentation "fraction of the sleeping interval that we send beacons")
   (probability-tx
    :parameter t :type float :initform 1.0 :reader probability-tx
    :documentation "the probability of a single try of Transmission to happen")
   (num-tx
    :parameter t :type integer :initform 1 :accessor num-tx
    :documentation "when we have something to Tx, how many times we try")
   (random-tx-offset
    :parameter t :type time-type :initform 0d0 :accessor random-tx-offset
    :documentation "Tx after time chosen randomly from interval [0..randomTxOffset]")
   (retx-interval
    :parameter t :type time-type :initform 0d0 :accessor retx-interval
    :documentation "Interval between retransmissions in ms, (numTx-1) retransmissions")
   (backoff-type
    :parameter t :type symbol
    :initform 'constant :accessor backoff-type
    :documentation "sleep-interval, constant, multiplying (e.g. 1*a, 2*a, 3*a, 4*a ...), exponential (e.g. 2, 4, 8, 16, 32...)")
   (backoff-base-value
    :parameter t :type time-type :initform 16d-3 :accessor backoff-base-value)
   (csma-persistence
    :parameter t :type float :initform 0 :reader csma-persistence
    :properties (:format (number :min 0 :max 1 :coerce-to float))
    :documentation "value in [0..1], is CSMA non-persistent, p-persistent, or 1-persistent?")
   (tx-all-packets-in-free-channel
    :parameter t :type boolean :initform t :reader tx-all-packets-in-free-channel
    :documentation " if you find the channel free, tx all packets in buffer?")
   (sleep-during-backoff
    :parameter t :type boolean :initform nil :reader sleep-during-backoff
    :documentation " for no dutyCycle case: sleep when backing off")

   ;;other parameters
   (header-overhead :initform 9)
   (beacon-frame-size
    :parameter t :type integer :initform 125 :reader beacon-frame-size
    :documentation "have a big beacon, to avoid much processing
    overhead, but fit at least 2 in the listening interval")
   (buffer-size :initform 32)
   (max-mac-frame-size :initform 0)

   ;; implementation values
   (state :initform 'default :type tuneable-mac-state :accessor state)
   (num-tx-tries :initform 0 :type fixnum :accessor num-tx-tries)
   (backoff-times :initform 0 :type fixnum :accessor backoff-times
                  :documentation "number of consequtive backoff times")
   (remaining-beacons-to-tx
    :initform 0 :type fixnum :accessor remaining-beacons-to-tx)
   (sleep-interval :type time-type :reader sleep-interval)

   ;; timers (may be able to use same message with different names)
   (sleep-listen-timer
    :type message :reader sleep-listen-timer
    :initform (make-instance 'timer-message))
   (start-cs-timer
    :type message :reader start-cs-timer
    :initform (make-instance 'timer-message :name 'start-cs-timer))
   (attempt-tx-timer
    :type message :reader attempt-tx-timer
    :initform (make-instance 'timer-message :name 'attempt-tx-timer))
   (send-beacons-or-data-timer
    :type message :reader send-beacons-or-data-timer
    :initform (make-instance 'timer-message :name 'send-beacons-or-data-timer)))
  (:properties
   :statistic (mac-packet-breakdown
               :default (indexed-count)))
  (:documentation "Default parameter values will result in
  non-persistent CSMA-CA behaviour")
  (:metaclass module-class))

(defmethod initialize-instance :after ((instance tuneable-mac)
                                       &key &allow-other-keys)
  (with-slots(duty-cycle backoff-type) instance
    (assert (or (not (eql backoff-type 'sleep-interval))
                (< 0.0 duty-cycle 1.0))
            (duty-cycle backoff-type)
            "Backoff timer is set to sleeping interval but sleeping
            interval is not defined because duty cycle is 0 or 1.")))

(defgeneric beacon-tx-time(mac)
  (:method((instance tuneable-mac))
    (tx-time instance (beacon-frame-size instance))))

(defmethod startup((instance tuneable-mac))
  ;; start listening and schedule a time to sleeep if needed
  (to-radio instance '(set-state . rx))
  (let ((duty-cycle (duty-cycle instance))
        (listen-interval (listen-interval instance)))
    (if (< 0.0 duty-cycle 1.0)
        (progn
          (setf (slot-value instance 'sleep-interval)
                (* listen-interval (/ (- 1.0 duty-cycle) duty-cycle)))
          (set-timer instance (sleep-listen-timer instance) listen-interval
                     'sleep))
        (setf (slot-value instance 'sleep-interval) -1d0))
    (setf (state instance) 'default
          (num-tx-tries instance) 0
          (remaining-beacons-to-tx instance) 0
          (backoff-times instance) 0)))

(defmethod handle-message((instance tuneable-mac) (message message))
  (cond
    ((eql message (sleep-listen-timer instance))
     (ecase (name message)
       (sleep
        (to-radio instance '(set-state . sleep))
        (set-timer instance message (sleep-interval instance) 'listen))
       (listen
        (to-radio instance '(set-state . rx))
        (set-timer instance message (listen-interval instance) 'sleep))))
    ((eql message (start-cs-timer instance))
     (to-radio instance '(set-state . rx))
     (handle-cs-result instance
                       (channel-clear-status
                        (submodule (node instance) '(communications radio)))))
     ((eql message (attempt-tx-timer instance))
      (attempt-tx instance))
     ((eql message  (send-beacons-or-data-timer instance))
      (send-beacons-or-data instance))
     (t (call-next-method))))

(defun handle-cs-result(instance result)
  (ecase result
    (clear
     (cond
       ((< 0 (csma-persistence instance) (uniform 0.0 1.0))
         ;; For non-persistent and 1-persistent CSMA we just start the
         ;; transmission. But for p-persistent CSMA we have to draw a
         ;; random number and if this is bigger than p
         ;; (=CSMApersistence) then we do not transmit but instead we
         ;; carrier sense yet again.
         (set-timer instance (start-cs-timer instance)
                    (phy-delay-for-valid-cs instance)))
       (t
        ;; We transmit: Reset the backoff counter, then proceed to
        ;; calculate the number of beacons required to be sent
        ;; based on sleep interval and beacon interval fraction
        (setf (backoff-times instance) 0)
        (setf (remaining-beacons-to-tx instance)
              (if (< 0.0 (duty-cycle instance) 1.0)
                  (ceiling (* (sleep-interval instance)
                              (beacon-interval-fraction instance))
                           (beacon-tx-time instance))
                  0))
        (setf (state instance) 'tx)
        (tracelog "Channel clear, TX state, sending ~D beacons followed by data"
                  (remaining-beacons-to-tx instance))
        (send-beacons-or-data instance))))
    (busy
     (cond
       ((> (csma-persistence instance) 0)
        ;; For p-persistent and 1-persistent CSMA we carrier sense at a
        ;; very fast rate until we find the channel free. Ideally we
        ;; would get notified by the radio when the channel becomes
        ;; free. Since most radios do not provide this capability, we
        ;; have to poll.  Because of polling and random start times for
        ;; each node, 1-persistent does not necessarily cause
        ;; collisions when 2 or more nodes are waiting to TX. Its
        ;; performance can thus be better than p-persistent even with
        ;; high traffic and and high number of contending nodes.
        (set-timer instance (start-cs-timer instance)
                   (phy-delay-for-valid-cs instance)))
       (t
        (with-slots(backoff-times sleep-interval backoff-base-value)
            instance
          (incf backoff-times)
          (let* ((backoff-limit
                  (ecase (backoff-type instance)
                    (sleep-interval
                     (if (< sleep-interval 0)
                         backoff-base-value
                         sleep-interval))
                    (constant
                     backoff-base-value)
                    (multiplying
                     (* backoff-times backoff-base-value))
                    (exponential
                     (expt 2.0 (if (zerop backoff-times) 0 (1- backoff-times))))))
                 (backoff-time (uniform 0.0 backoff-limit 1)))
            (set-timer instance (start-cs-timer instance) backoff-time)
            (tracelog "Channel busy, backing off ~D (0-~:/dfv::eng/s) for ~:/dfv::eng/s" backoff-times backoff-limit backoff-time))
        (when (or (< 0.0 (duty-cycle instance) 1.0)
                  (sleep-during-backoff instance))
          (to-radio instance '(set-state . sleep)))))))
    ((cs-not-valid cs-not-valid-yet)
     (set-timer instance (start-cs-timer instance)
                (phy-delay-for-valid-cs instance))
     (tracelog "CS not valid yet, trying again."))))

(defmethod handle-message((instance tuneable-mac) (packet routing-packet))
  ;; from routing layer
  (let ((mac-packet (encapsulate instance packet)))
    (setf (destination mac-packet) (next-hop (control-info packet)))
    (setf (name mac-packet) 'data)
    (emit instance 'mac-packet-breakdown "Received from App")
    ;; always try and buffer packet first
    (if (enqueue mac-packet instance)
        ;; If the new packet is the only packet and we are in default
        ;; state then we need to initiate transmission process for
        ;; this packet.  If there are more packets in the buffer, then
        ;; transmission is already being handled
        (when (and (eql (state instance) 'default)
                   (= (size (buffer instance)) 1))
          ;; First action to initiate new transmission is to deal with
          ;; all scheduled timers. In particular we need to suspend
          ;; duty cycle timers (if present) and attempt to transmit
          ;; the new packet with a maximum number of retries left.
          (when (< 0.0 (duty-cycle instance) 1.0)
            (cancel (sleep-listen-timer instance)))
          (setf (num-tx-tries instance) (num-tx instance))
          (attempt-tx instance))
        (progn
          ;; bufferPacket() failed, buffer is full FULL_BUFFER control
          ;; msg sent by virtualMAC code
          (emit instance 'mac-packet-breakdown "Overflown")
          (tracelog "WARNING: Tuneable MAC buffer overflow")))))

(defmethod attempt-tx((instance tuneable-mac) &optional description)
  (tracelog "attempt-tx, buffer size ~A, num-tx-tries ~A~:[ ~A~]"
            (size (buffer instance)) (num-tx-tries instance) description)
  (cond
    ((<= (num-tx-tries instance) 0)
     ;; if we have attempted num-tx times on front message so remove it
     (unless (empty-p (buffer instance))
       (dequeue (buffer instance)))
     (cond
       ((empty-p (buffer instance))
        ;; if no more packets go to default sleep/listen pattern
        (setf (state instance) 'default)
        (tracelog "DEFAULT state, no more packets to TX")
        (when (< 0.0 (duty-cycle instance) 1.0)
          (set-timer instance (sleep-listen-timer instance) 0 'sleep)))
       (t
        ;; check buffer again and see if new packet needs to be transmitted
        (setf (num-tx-tries instance) (num-tx instance))
        (attempt-tx instance))))
    (t
     (setf (state instance) 'contending)
     (cond
       ((< (uniform 0.0 1.0 0) (probability-tx instance))
        (set-timer instance (start-cs-timer instance)
                   (uniform 0.0 (random-tx-offset instance) 1))
        (tracelog "CONTENDING state, attempt ~D/~D contending"
                  (1+ (- (num-tx instance) (num-tx-tries instance)))
                  (num-tx instance)))
       (t
        (set-timer instance
                   (attempt-tx-timer instance) (retx-interval instance))
        (tracelog "CONTENDING state, attempt ~D/~D skipped"
                  (1+ (- (num-tx instance) (num-tx-tries instance)))
                  (num-tx instance))
        (decf (num-tx-tries instance)))))))

(defun send-beacons-or-data(instance)
  (cond
    ((> (remaining-beacons-to-tx instance) 0)
     (decf (remaining-beacons-to-tx instance))
     (to-radio instance
               (make-instance 'mac-packet
                              :name 'beacon
                              :header-overhead (beacon-frame-size instance)
                              :source (mac-address instance)
                              :destination broadcast-mac-address))
     (to-radio instance '(set-state . tx))
     (tracelog "Sending Beacon.")
     (emit instance 'mac-packet-breakdown "sent beacons")
     ;; Set timer to send next beacon (or data packet). Schedule the
		 ;; timer a little faster than it actually takes to TX a a beacon,
		 ;; because we want to TX beacons back to back and we have to
		 ;; account for clock drift.
     (set-timer instance (send-beacons-or-data-timer instance)
                (* 0.98 (beacon-tx-time instance))))
    ((empty-p (buffer instance))
     (setf (num-tx-tries instance) 0) ; safeguarding
     (attempt-tx instance))
    (t
     (to-radio instance (duplicate (peek (buffer instance))))
     (to-radio instance '(set-state . tx))
     ;; record whether sending or resending packet
     (cond
       ((= (num-tx-tries instance) (num-tx instance))
        (tracelog "Sending data packet")
        (emit instance 'mac-packet-breakdown "sent data packets"))
       (t
        (tracelog "Sending copy of data packet")
        (emit instance 'mac-packet-breakdown "copies of sent data packets")))
     (let ((packet-tx-time (tx-time instance (peek (buffer instance)))))
       (decf (num-tx-tries instance))
       (cond
         ((tx-all-packets-in-free-channel instance)
          ;; we will try to continur sending packets - a little faster than tx
          ;; to keep radio buffer non-empty so they are transmitted back to back
          (set-timer instance
                     (send-beacons-or-data-timer instance)
                     (* 0.95 packet-tx-time))
          (when (<= (num-tx-tries instance) 0)
            (dequeue (buffer instance))
            (setf (num-tx-tries instance) (num-tx instance))))
         ((> (num-tx-tries instance) 0)
          (set-timer instance
                     (attempt-tx-timer instance)
                     (+ packet-tx-time (retx-interval instance))))
         (t
          (set-timer instance (attempt-tx-timer instance) packet-tx-time)))))))

(defmethod handle-message((instance tuneable-mac) (packet mac-packet))
  ;; from radio layer
  (unless (or (eql (destination packet) (mac-address instance))
              (eql (destination packet) broadcast-mac-address))
    (emit instance 'mac-packet-breakdown "filtered other destination")
    (return-from handle-message))
  (ecase (name packet)
    (beacon
     (emit instance 'mac-packet-breakdown "received beacons")
     (case (state instance)
       (default
        (when (< 0.0 (duty-cycle instance) 1.0)
          (cancel (sleep-listen-timer instance))))
       (contending
        (cancel (start-cs-timer instance))
        (cancel (attempt-tx-timer instance)))
       (tx ;; ignore beacon as we are sending our own data
        (tracelog "ignoring beacon, we are in TX state")
        (emit instance 'mac-packet-breakdown
              "received beacons ignored")
        (return-from handle-message)))
     (setf (state instance) 'rx)
     (tracelog "state RX, received beacon")
     (if (< 0.0 (duty-cycle instance) 1.0)
         (set-timer
          instance (attempt-tx-timer instance) (sleep-interval instance))
         (progn
           ;; only happens if one node has duty cycle and other has not
           ;; so just wait 0,5 secs
           (tracelog "WARNING: received a beacod packet without a duty cycle in place.")
           (set-timer instance (attempt-tx-timer instance) 0.5))))
    (data
     (let ((routing-packet (decapsulate packet)))
        (setf (control-info routing-packet)
              (make-instance 'net-mac-control-info
                             :rssi (rssi (control-info packet))
                             :lqi (lqi (control-info packet))))
        (emit instance 'mac-packet-breakdown "received data packets")
        (to-network instance routing-packet))
     (when (eql (state instance) 'rx)
       (cancel (attempt-tx-timer instance))
       (attempt-tx instance)))))

(defmethod handle-control-command
    ((mac tuneable-mac) (command (eql 'set-duty-cycle)) (new-duty-cycle real))
  (with-slots(duty-cycle listen-interval sleep-interval) mac
    (let ((had-duty-cycle (< 0.0 duty-cycle 1.0)))
      (setf duty-cycle new-duty-cycle)
      ;; if a duty cycle defined set sleep-interval etc
      (cond
        ((< 0.0 duty-cycle 1.0)
         (setf sleep-interval (* listen-interval (/ (- 1.0 duty-cycle) duty-cycle)))
         (unless had-duty-cycle
           (set-timer mac (sleep-listen-timer mac)
                      listen-interval 'sleep)))
        (t
         ;; else if no duty cycle
         ;; cancel timer and since radio can sleep only in default state it
         ;;can only wake in default state too
         (setf sleep-interval -1.0)
         (when had-duty-cycle
           (cancel (sleep-listen-timer mac)))
         (when (eql (state mac) 'default)
           (to-radio mac '(set-state . rx)))))))
  t)

(defmethod handle-control-command
    ((mac tuneable-mac) (command (eql 'set-listen-interval))
     (new-listen-interval real))
  (assert (>= new-listen-interval 0.0)
          (new-listen-interval)
          "Invalid listen interval")
   (with-slots(duty-cycle listen-interval sleep-interval) mac
     (setf listen-interval new-listen-interval)
     (setf sleep-interval
           (if (< 0.0 duty-cycle 1.0)
               (* listen-interval (/ (- 1.0 duty-cycle) duty-cycle))
               -1.0)))
   t)

(defmethod handle-control-command
    ((mac tuneable-mac) (command (eql 'set-beacon-interval-fraction))
     (beacon-interval-fraction real))
  (assert (< 0.0 beacon-interval-fraction 1.0)
          (beacon-interval-fraction))
  (setf (beacon-interval-fraction mac) beacon-interval-fraction))

(defmethod handle-control-command
    ((mac tuneable-mac) (command (eql 'set-prob-tx))
     (num-tx integer))
  (assert (>= num-tx 0) (num-tx))
  (setf (num-tx mac) num-tx))

(defmethod handle-control-command
    ((mac tuneable-mac) (command (eql 'set-random-tx-offset))
     (random-tx-offset real))
  (assert (> random-tx-offset 0) (random-tx-offset))
  (setf (random-tx-offset mac) (coerce random-tx-offset 'double-float)))

(defmethod handle-control-command
    ((mac tuneable-mac) (command (eql 'set-retx-interval)) (retx-interval real))
   (assert (> retx-interval 0) (retx-interval))
   (setf (retx-interval mac) (coerce retx-interval 'double-float)))

(defmethod handle-control-command
    ((mac tuneable-mac) (command (eql 'set-backoff-type)) (backoff-type symbol))
  (check-type backoff-type backoff-type)
  (when (eql backoff-type 'sleep-interval)
    (assert (< 0 (duty-cycle mac) 1)
            (backoff-type)
             "Sleep-interval backoff invalid as duty cycle does not allow a sleep interval"))
  (setf (backoff-type mac) backoff-type))

(defmethod handle-control-command
    ((mac tuneable-mac) (command (eql 'set-backoff-base-value))
     (backoff-base-value real))
  (assert (> backoff-base-value 0) (backoff-base-value))
  (setf (backoff-base-value mac) (coerce backoff-base-value 'double-float)))
