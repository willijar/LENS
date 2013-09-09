(in-package :lens.wsn)

(defpackage :lens.wsn.mac.tmac
  (:use :cl :cl-user :lens :lens.wsn)
  (:export #:tmac))

(in-package :lens.wsn.mac.tmac)

(defclass tmac-packet(mac-packet)
  ())

;; differe tmac packet types
;; mac type (1 byte)  + source + destination requires 9 bytes

(defclass tmac-sync-packet(tmac-packet) ;; sequence number but no source/dest
  ((sync :type integer :initarg :sync) ;; 4 bytes
   (sync-id :type integer :initarg :sync-id))) ;; 4 bytes

(defmethod duplicate((pkt tmac-sync-packet) &optional duplicate)
  (call-next-method)
  (copy-slots '(sync sync-id) pkt duplicate))

(defclass tmac-rts-packet(tmac-packet)
  ((nav :type time-type :initarg :nav))) ;; 4 bytes

(defmethod duplicate((pkt tmac-rts-packet) &optional duplicate)
  (call-next-method)
  (copy-slots '(nav) pkt duplicate))

(defclass tmac-cts-packet(tmac-packet)
  ((nav :type time-type :initarg :nav)))

(defmethod duplicate((pkt tmac-cts-packet) &optional duplicate)
  (call-next-method)
  (copy-slots '(nav) pkt duplicate))

(defclass tmac-ds-packet(tmac-packet) ())
(defclass tmac-frts-packet(tmac-packet) ())
;; ack and data require sequence number (4 bytes)
(defclass tmac-data-packet(tmac-packet) ())
(defclass tmac-ack-packet(tmac-packet) ())

(defstruct tmac-schedule
  offset
  id
  sn)

(deftype tmac-state ()
  '(member
    setup
    sleep
    active
    active-silent
    in-tx
    carrier-sense-for-tx-rts
    carrier-sense-for-tx-data
    carrier-sense-for-tx-sync
    carrier-sense-before-sleep
    wait-for-data
    wait-for-cts
    wait-for-ack))

(deftype tmac-collision-resolution ()
  '(member
    immediate-retry ;; (low collision avoidance)
    overhearing ;; (default)
    retry-next-frame)) ;;(aggressive collision avoidance)"

(defclass tmac(mac)
  ((print-state-transitions
    :parameter t :type boolean :initform nil
    :documentation "Debugging parameter")

   ;; mac layer packet sizes
   (ack-packet-size :parameter t :type integer :initform 11)
   (sync-packet-size :parameter t :type integer :initform 11)
   (rts-packet-size :parameter t :type integer :initform 13)
   (cts-packet-size :parameter t :type integer :initform 13)

   ;; common mac layer parameters
   (max-mac-frame-size :parameter t :initform 0)
   (header-overhead :parameter t :initform 11) ;; data-packet-size
   (buffer-size :parameter t :initform 32)

   ;; tmac protocol parameters
   (max-tx-retries :parameter t :initform 2 :type integer)
   (allow-sink-sync
    :parameter t :initform t :type boolean
    :documentation "allows sink node to start synchronisation immediately")
   (use-frts
    :parameter t :initform nil :type boolean
    :documentation "enable/disable FRTS (Future Request To Send), true
    value not supported")
   (use-rts-cts
    :parameter t :initform t :type boolean
    :documentation "enable/disable RTS/CTS handshake")
   (disable-TA-extension
    :parameter t :initform nil :type boolean
    :documentation "disabling TA extension effectively creates an SMAC
    protocol")
   (conservative-TA
     :parameter t :initform t :type boolean
     :documentation "conservative activation timeout - will always
     stay awake for at least 15 ms after any activity on the radio")
   (resync-time
    :parameter t :type time-type :initform 6d0
    :documentation "time for re-sending SYNC msg, in seconds")
   (contention-period
    :parameter t :type time-type :initform 10e-3)
   (listen-timeout
     :parameter t :type time-type :initform 15e-3
     :documentation "15 ms, is the timeout TA (Activation event)")
   (wait-timeout
    :parameter t :type time-type :initform 5e-3
    :documentation "timeout for expecting a reply to DATA or RTS packet")
   (frame-time
    :parameter t :type time-type :initform 610e-3
    :documentation "frame time (standard = 610ms)")
   (collision-resolution
    :parameter t :type symbol :initform 'overhearing
    :documentation
    "collision resolution mechanism, choose from
     'immediate-retry (low collision avoidance)
     'overhearing (default)
		 'retry-next-frame (aggressive collision avoidance)")

   ;; implementation values
   (state :type tmac-state)
   (tx-addr :type integer
            :documentation "current communication peer (can be BROADCAST)")
   (tx-retries
    :type fixnum
    :documentation "number of transmission attempts to txAddr (when reaches 0 - packet is dropped)")
   (tx-sequence-num
    :type integer
    :documentation "sequence number for current transmission")
   (primary-wakeup
    :type boolean
    :documentation "to distinguish between primary and secondary schedules")
   (need-resync
    :type boolean
    :documentation "	//set to 1 when a SYNC packet has to be sent")
   (current-frame-start
    :type time-type
    :documentation "recorded start time of the current frame")
   (activation-timeout
    :type time-type
    :documentation "time untill MAC_CHECK_TA message arrival")
   (schedule-table
    :type vector
    :initform (make-array 8 :element-type 'tmac-schedule :adjustable t :fill-pointer 0)
    :documentation "TMAC Schedule table (list of effective schedules)")

   ;; packet instances (sometimes packet is created not immediately before sending)
   (sync-packet :initform nil :type tmac-sync-packet)
   (rts-packet :initform nil :type tmac-rts-packet)
   (cts-packet :initform nil :type tmac-cts-packet)
   (ack-packet :initform nil :type tmac-ack-packet)

   ;; cached timers
   (transmission-timeout :initform (make-instance 'timer-message)))
  (:documentation "TMAC")
  (:metaclass module-class))

(defmethod startup((instance tmac))
  ;; start listening and schedule a time to sleeep if needed
  (setf (slot-value instance 'state) 'setup
        (fill-pointer (slot-value instance 'schedule-table)) 0
        (slot-value instance 'primary-wakeup) t
        (slot-value instance 'need-resync) nil
        (slot-value instance 'current-frame-start) -1
        (slot-value instance 'activation-timeout) 0)
  (with-slots(allow-sink-sync frame-time) instance
    (if (and allow-sink-sync (sink-p instance))
        (set-timer instance 'sync-create 0.1)
        (set-timer instance 'sync-setup
                   (if allow-sink-sync (* 3 frame-time) 0.1))))
  (to-radio instance '(set-cs-interrupt . t)))

(defmethod handle-timer((instance tmac) (timer (eql 'sync-setup)))
  (with-slots(state frame-time) instance
    (when (eql state 'setup)
      (set-timer instance 'sync-create (* frame-time (uniform 0 1 1))))))

(defmethod handle-timer((instance tmac) (timer (eql 'sync-create)))
  (when (eql (slot-value instance 'state) 'setup)
    (create-primary-schedule instance)))

(defmethod handle-timer((instance tmac) (timer (eql 'sync-renew)))
  (tracelog "Initiated RESYNC procedure")
  (incf (tmac-schedule-sn (aref (slot-value instance 'schedule-table) 0)))
  (setf (slot-value instance 'need-resync) t)
  (set-timer instance 'sync-renew (slot-value instance 'resync-time)))

(defmethod handle-timer((instance tmac) (timer (eql 'frame-start)))
  (with-slots(primary-wakeup current-frame-start active-timeout frame-time
              schedule-table need-resync state) instance
  ;; primary wakeup is always the one at beginning of frame
  (setf primary-wakeup t)
  ;; record current time and extend activation timeout
  (let ((clk (get-clock instance)))
    (setf current-frame-start clk
          (slot-value instance 'activation-timeout) clk))
  (extend-active-period instance)
  ;; schedule message to start next frame
  (set-timer instance 'frame-start frame-time)
  ;; Also check for frame offsets if we received a RESYNC packet,
	;;frame start time could had been shifted due to clock drift - in
	;;this case it is necessary to rebroadcast this resync further)
  (let ((offset  (tmac-schedule-offset (aref schedule-table 0))))
    (unless (zerop offset)
      (tracelog "New frame started, shifted by ~A" offset)
      (setf  (tmac-schedule-offset (aref schedule-table 0)) 0d0)
      (setf need-resync t)))
  ;; schedule wakeup messages for secondary schedules within the current frame
  (do((i 1 (1+ i)))
     ((= i (length schedule-table)))
    (let ((schedule (aref schedule-table i)))
      (when (< (tmac-schedule-offset schedule) 0)
        (incf (tmac-schedule-offset schedule) frame-time))
      (set-timer instance i (tmac-schedule-offset schedule))))
  ;; if sleeping wake up radio and reset internal MAC state to start
  ;; contending for transmissions
  (when (eql state 'sleep)
    (to-radio instance '(set-state . rx)))
  (when (member state '(sleep 'active 'active-silent))
    (reset-default-state instance "new frame started"))))

(defmethod handle-timer((instance tmac) (timer (eql 'check-ta)))
  ;; Activation timeout fired, however we may need to extend the timeout
	;; here based on the current MAC state, or if there is no reason to
	;; extend it, then we need to go to sleep.
  (cond
    ((slot-value instance 'disable-ta-extension)
     (setf (slot-value instance 'primary-wakeup) nil)
     (to-radio instance '(set-state . sleep))
     (set-state instance 'sleep "active period expired (SMAC)"))
    ((slot-value instance 'conservative-ta)
     (if (member (slot-value instance 'state) '(active sleep active-silent))
         (perform-carrier-sense instance 'carrier-sense-before-sleep)
         (extend-active-period instance)))
    (t
     (setf (slot-value instance 'primary-wakeup) nil)
     (to-radio instance '(set-state . sleep))
     (set-state instance 'sleep "active period expired (TMAC)"))))

(defmethod handle-timer((instance tmac) (timer (eql 'carrier-sense)))
  ;; First it is important to check for valid MAC state If we heard
	;;  something on the radio while waiting to start carrier sense,
	;;  then MAC was set to MAC_STATE_ACTIVE_SILENT. In this case we can
	;;  not transmit and there is no point to perform carrier sense
  (let ((state (slot-value instance 'state)))
    (when (member state '(sleep active-silent))
      (return-from handle-timer))
    (when (member state '(carrier-sense-for-tx-rts
                          carrier-sense-for-tx-sync
                          carrier-sense-for-tx-data
                          carrier-sense-before-sleep))
      (tracelog "Warning: bad MAC state for carrier sense")
      (return-from handle-timer))
    (ecase (channel-clear-status
            (submodule (node instance) '(communications radio)))
      (clear (carrier-is-clear instance))
      (busy (carrier-is-busy instance))
      (cs-not-valid-yet
       (set-timer instance 'carrier-sense
                  (slot-value instance 'phy-delay-for-valid-cs)))
      (cs-not-valid
       (unless (eql state 'sleep)
         (to-radio instance '(set-state . rx))
         (set-timer instance 'carrier-sense
                    (slot-value instance 'phy-delay-for-valid-cs)))))))

(defmethod handle-timer((instance tmac) (timer (eql 'transmission-timeout)))
  (reset-default-state instance "timeout"))

(defmethod handle-timer((instance tmac) (timer (eql 'wakeup-silent)))
 ;;This is the wakeup timer for secondary schedules. here we only wake
 ;;up the radio and extend activation timeout for listening. NOTE that
 ;;default state for secondary schedules is MAC_STATE_ACTIVE_SILENT
  (with-slots(activation-timeout state) instance
    (setf activation-timeout (get-clock instance))
    (extend-active-period instance)
    (when (eql state 'sleep)
      (to-radio instance '(set-state . rx))
      (reset-default-state instance "secondary schedule starts"))))

(defmethod handle-timer((instance tmac) (timer integer))
  ;; timer is wakeup inhdexed into schedule table
  (if (< 0 timer (length (slot-value instance 'schedule-table)))
      (handle-timer instance 'wakeup-silent)
      (tracelog "Unknown timer ~A" timer)))


(defmethod handle-message((instance tmac)
                          (message radio-control-message))
  (when (eql (command message) 'carrier-sense-interrupt)
    (carrier-is-busy instance))
  (call-next-method))

(defmethod handle-message((instance tmac) (packet routing-packet))
  ;; from routing layer
  (when
      (enqueue
       (encapsulate
        (make-instance
         'tmac-data-packet
         :header-overhead (header-overhead instance)
         :source (mac-address instance)
         :destination (next-hop (control-info packet))
         :sequence-number (next-sequence-number instance))
        packet)
       instance)
    (check-tx-buffer instance)))

(defun reset-default-state(instance description)
  (when description
    (tracelog "Resetting MAC state to default, reason: ~A" description))
  (with-slots(activation-timeout disable-ta-extension primary-wakeup
              contention-period need-resync sync-packet schedule-table
              sync-packet-size frame-time current-frame-start)
      instance
    (cond
      ((<= activation-timeout (get-clock instance))
       (if disable-ta-extension
           (progn
             (to-radio instance '(set-state sleep))
             (set-state instance 'sleep "active period expierd (SMAC)"))
           (perform-carrier-sense instance 'carrier-sense-before-sleep)))
      (primary-wakeup
       (let ((random-contention-interval (uniform 0.0 contention-period 1)))
         (when need-resync
           (when sync-packet (cancel sync-packet))
           (setf sync-packet
                 (make-instance
                  'tmac-sync-packet
                  :destination broadcast-mac-address
                  :sync-id (tmac-schedule-id (aref schedule-table 0))
                  :sync-value  (- (+ current-frame-start frame-time)
                                  (+ (get-clock instance)
                                     (tx-time instance sync-packet-size)
                                     random-contention-interval))
                  :sequence-number (tmac-schedule-sn (aref schedule-table 0))
                  :byte-length sync-packet-size))
           (perform-carrier-sense instance 'carrier-sense-for-tx-sync
                                  random-contention-interval)
           (return-from reset-default-state))
         (with-slots(tx-retries use-rts-cts tx-addr) instance
           (while (not (empty-p (buffer instance)))
             (if (<= tx-retries 0)
                 (progn
                   (tracelog "Transmission failed to ~A" tx-addr)
                   (dequeue  instance))
                 (progn
                   (if (and use-rts-cts
                            (not (eql tx-addr broadcast-mac-address)))
                       (perform-carrier-sense
                        instance
                        'carrier-sense-for-tx-rts
                        random-contention-interval)
                       (perform-carrier-sense
                        instance
                        'carrier-sense-for-tx-data
                        random-contention-interval))
                   (return-from reset-default-state))))))
       (set-state instance 'active "nothing to transmit"))
      (t
       (set-state instance 'active-silent
                      "node is awake not in primary schedule")))))

(defun create-primary-schedule(instance)
  (update-schedule-table instance
                         (slot-value instance 'frame-time)
                         (mac-address instance) 0)
  (set-timer instance 'sync-renew (slot-value instance 'resync-time)))

(defmethod set-state :before((instance tmac) new-state &optional description)
  (declare (ignore description))
  (check-type new-state tmac-state))

(defun update-schedule-table(instance wakeup id sn)
  (with-slots(frame-time current-frame-start schedule-table
              activation-timeout) instance
    (let ((clock (get-clock instance)))
      (dotimes(i (length schedule-table))
        (let ((schedule (aref schedule-table i)))
          (when (eql (tmac-schedule-id schedule) id)
            (cond
              ((< (tmac-schedule-sn schedule) sn)
               (let ((new-offset (- (+ clock wakeup)
                                  (+ current-frame-start frame-time))))
                 (tracelog "Resync successful for ID: ~A old offset: ~A new offset: ~A"
                           id (tmac-schedule-offset schedule) new-offset)
                 (setf (tmac-schedule-offset schedule) new-offset
                       (tmac-schedule-sn schedule) sn)
                 (cond
                   ((= i 0)
                    ;;If the update came for primary schedule, then the
                    ;;next frame message has to be rescheduled
                    (set-timer instance 'frame-start wakeup)
                    (incf current-frame-start new-offset))
                   (t
                    ;;This is not primary schedule, check that offset
                    ;;value falls within the interval: 0 < offset < frameTime
                    (cond
                      ((< (tmac-schedule-offset schedule) 0)
                       (incf (tmac-schedule-offset schedule) frame-time))
                      ((> (tmac-schedule-offset schedule) frame-time)
                       (decf (tmac-schedule-offset schedule) frame-time)))))))
              ((> (tmac-schedule-sn schedule) sn)
               ;; TMAC received a sync with lower SN than what
               ;; currently stored in the schedule table. With current
               ;; TMAC implementation, this is not possible, however
               ;; in future it may be neccesary to implement a unicast
               ;; sync packet here to notify the source of this packet
               ;; with the updated schedule
               ))
          (return-from update-schedule-table))))
    ;; schedule not found so add to current table
      (let ((schedule
             (make-tmac-schedule
              :id id
              :sn sn
              :offset (if (= current-frame-start -1)
                          0
                          (- (+ clock wakeup)
                             (+ current-frame-start frame-time))))))
        (tracelog "Creading schedule ~A, wakeup:~A" schedule wakeup)
        (vector-push-extend schedule schedule-table))
    ;; if primary more needs to be done
    (when  (= current-frame-start -1)
      (setf current-frame-start (- (+ clock wakeup) frame-time)
            activation-timeout clock)
      (extend-active-period instance)
      (set-timer instance 'frame-start wakeup)
      (setf (slot-value instance 'need-resync) t
            (slot-value instance 'primary-wakeup) t)
      (reset-default-state instance "new primary schedule found")))))

(defmethod handle-message :around ((instance tmac) (pkt tmac-packet))
  ;; from radio layer
  ;; first check if packet is for this node
  (if (or (eql (destination pkt) (mac-address instance))
          (eql (destination pkt) broadcast-mac-address))
      (call-next-method)
      (with-slots(state use-frts collision-resolution) instance
        (when (and (eql state 'carrier-sense-for-tx-rts)  use-frts)
        ;; FRTS would be implemented here
        )
        (let ((nav (if (slot-exists-p pkt 'nav) (slot-value pkt 'nav) 0)))
          (when (and (> nav 0) (eql collision-resolution 'retry-next-frame))
            (set-timer instance 'transmission-timeout nav))
          (extend-active-period instance nav)
          (set-state instance 'active-silent "overheard a packet")))))

(defmethod handle-message((instance tmac) (rts-packet tmac-rts-packet))
  ;; reply with a CTS packet
  (with-slots(cts-packet cts-packet-size wait-timeout) instance
    (when cts-packet (cancel cts-packet))
    (setf cts-packet
          (make-instance
           'tmac-cts-packet
           :source (mac-address instance)
           :destination (source rts-packet)
           :nav (- (slot-value rts-packet 'nav)
                   (tx-time instance cts-packet-size))
           :byte-length cts-packet-size))
    (to-radio instance cts-packet)
    (to-radio instance '(set-state . tx))
    (emit instance 'mac-packet-breakdown "CTS Sent")
    (setf cts-packet nil)
    (set-state instance 'wait-for-data "sent CTS packet")
    (set-timer instance 'transmission-timeout
               (+ (tx-time instance cts-packet-size) wait-timeout))))

(defmethod handle-message((instance tmac) (cts-packet tmac-cts-packet))
  (with-slots(state tx-addr) instance
    (when (eql state 'wait-for-cts)
      (cond
        ((empty-p (buffer instance))
         (tracelog "WARNING: invalid WAIT_FOR_CTS while buffer is empty")
         (reset-default-state instance "invalid state while buffer is empty"))
        ((eql (source cts-packet) tx-addr)
         (cancel-timer instance 'transmission-timeout)
         (send-data-packet instance))
        (t
         (tracelog "WARNING: recieved unexpected CTS from ~A" (source cts-packet))
         (reset-default-state instance "unexpected CTS"))))))

(defmethod handle-message((instance tmac) (packet tmac-data-packet))
  ;; forward frame to upper layer
  (unless (duplicate-p packet (packet-history instance))
    (let ((routing-packet (decapsulate packet)))
      (setf (control-info routing-packet)
            (make-instance 'net-mac-control-info
                           :rssi (rssi (control-info packet))
                           :lqi (lqi (control-info packet))))
      (emit instance 'mac-packet-breakdown "received data packets")
      (to-network instance routing-packet)))
  (when (eql (destination packet) broadcast-mac-address)
    (return-from handle-message))
  (when (eql (slot-value instance 'state) 'wait-for-data)
    (cancel-timer instance 'transmission-timeout))
  (with-slots(ack-packet ack-packet-size) instance
    (when ack-packet (cancel ack-packet))
    (setf ack-packet
          (make-instance
           'tmac-ack-packet
           :source (mac-address instance)
           :destination (source packet)
           :byte-length ack-packet-size
           :sequence-number (sequence-number packet)))
    (to-radio instance ack-packet)
    (to-radio instance '(set-state . tx))
    (emit instance 'mac-packet-breakdown "ACK sent")
    (setf ack-packet nil)
    (set-state instance 'in-tx "transmitting ACK packet")
    (set-timer instance 'transmission-timeout
               (tx-time instance ack-packet-size))
    (extend-active-period instance (tx-time instance ack-packet-size))))

(defmethod handle-message((instance tmac) (packet tmac-ack-packet))
  (when (and (eql (slot-value instance 'state) 'wait-for-ack)
             (eql (source packet) (slot-value instance 'tx-addr)))
    (tracelog "Transmisson Successful to ~A" (slot-value instance 'tx-addr))
    (emit instance 'mac-packet-breakdown "successful transmissions")
    (cancel-timer instance 'transmission-timeout)
    (dequeue instance)
    (reset-default-state instance "transmission successful (ACK received)")))

(defmethod handle-message((instance tmac) (packet tmac-sync-packet))
  (update-schedule-table instance
                         (slot-value packet 'sync)
                         (slot-value packet 'sync-id)
                         (sequence-number packet)))

(defun carrier-is-busy(instance)
  " Since we are hearing some communication on the radio we need to do two things:
	 1 - extend our active period
	 2 - set MAC state to MAC_STATE_ACTIVE_SILENT unless we are actually expecting to receive something (or sleeping)"
  (with-slots(state collision-resolution disable-ta-extension) instance
    (when (member state '(setup sleep)) (return-from carrier-is-busy))
    (unless disable-ta-extension (extend-active-period instance))
    (if (eql collision-resolution 'immediate-retry)
        (when (member state
                      '(carrier-sense-for-tx-rts carrier-sense-for-tx-data
                        carrier-sense-for-tx-sync carrier-sense-before-sleep))
          (reset-default-state instance "sensed carrier"))
        (when (member state '(wait-for-ack wait-for-data wait-for-cts))
          (set-state instance 'active-silent "sensed carrier")))))

(defun carrier-is-clear(instance)
  (ecase (slot-value instance 'state)
    (carrier-sense-for-tx-rts
     ;; MAC requested carrier sense to transmit an RTS packet
     (when (empty-p (buffer instance))
       (tracelog "WARNING: buffer is empty in CARRIER_SENSE_FOR_TX_RTS, will reset state")
       (reset-default-state instance "empty transmission buffer")
       (return-from carrier-is-clear))
     ;; create and send RTS frame
     (with-slots(rts-packet rts-packet-size cts-packet-size ack-packet-size
                 tx-addr tx-retries use-rts-cts wait-timeout) instance
       (when rts-packet (cancel rts-packet))
       (setf rts-packet
          (make-instance
           'tmac-rts-packet
           :source (mac-address instance)
           :destination tx-addr
           :nav (+ (tx-time instance rts-packet-size)
                   (tx-time instance cts-packet-size)
                   (tx-time instance ack-packet-size)
                   (tx-time instance (byte-length (peek (buffer instance)))))
           :byte-length rts-packet-size))
       (to-radio instance rts-packet)
       (to-radio instance '(set-state . tx))
       (when use-rts-cts (decf tx-retries))
       (emit instance 'mac-packet-breakdown "RTS sent")
       (setf rts-packet nil)
       (set-state instance 'wait-for-cts "sent RTS packet")
       (set-timer instance 'transmission-timeout
                  (+ (tx-time instance rts-packet-size) wait-timeout))))
    (carrier-sense-for-tx-sync
     ;;MAC requested carrier sense to transmit a SYNC packet
     (with-slots(sync-packet need-resync sync-packet-size) instance
       (unless sync-packet
         (tracelog "WARNING: Invalid CARRIER_SENSE_FOR_TX_SYNC while sync-packet undefined")
         (reset-default-state instance"invalid state, no SYNC packet found")
         (return-from carrier-is-clear))
       ;; Send SYNC packet to radio
       (to-radio instance sync-packet)
       (to-radio instance '(set-state . tx))
       (emit instance 'mac-packet-breakdown "SYNC sent")
       (setf sync-packet nil
             need-resync nil)
       (set-state instance 'in-tx  "transmitting SYNC packet")
       (set-timer instance 'transmission-timeout
                  (tx-time instance sync-packet-size))))
    (carrier-sense-for-tx-data
     (send-data-packet instance))
    (carrier-sense-before-sleep
     (setf (slot-value instance 'primary-wakeup) nil)
     (to-radio instance '(set-state . sleep))
     (set-state instance 'sleep "no activity on the channel"))))

(defun send-data-packet(instance)
  (when (empty-p (buffer instance))
    (tracelog "WARNING: Invalid CARRIER_SENSE_FOR_TX_DATA while TX buffer is empty")
		(reset-default-state instance "empty transmission buffer")
    (return-from send-data-packet))
  (to-radio instance (duplicate (peek (buffer instance))))
  (emit instance 'mac-packet-breakdown "DATA sent")
  ;; update MAC state based on transmission time and destination address
  (let ((tx-time (tx-time instance (peek (buffer instance)))))
    (cond
      ((eql (slot-value instance 'tx-addr) broadcast-mac-address)
       ;; This packet is broadcast, so no reply will be received
       ;; The packet can be cleared from transmission buffer
       ;; and MAC timeout is only to allow RADIO to finish the transmission
       (dequeue instance)
       (set-state instance 'in-tx "sent DATA packet to BROADCAST address")
       (set-timer instance 'transmission-timeout tx-time))
      (t
       ;; This packet is unicast, so MAC will be expecting an ACK
       ;; packet in reply, so the timeout is longer If we are not
       ;; using RTS/CTS exchange, then this attempt also decreases the
       ;; txRetries counter (NOTE: with RTS/CTS exchange sending RTS
       ;; packet decrements counter)
       (when (slot-value instance 'use-rts-cts)
         (decf (slot-value instance 'tx-retries)))
       (set-state instance 'wait-for-ack
                      "sent DATA packet to UNICAST address")
       (set-timer instance 'transmission-timeout
                  (+ tx-time (slot-value instance 'wait-timeout)))))
    (extend-active-period instance tx-time)
    (to-radio instance '(set-state . tx))))

(defun perform-carrier-sense(instance new-state &optional (delay 0))
 ;; This function will set a timer to perform carrier sense.  MAC
 ;; state is important when performing CS, so setMacState is always
 ;; called here.  delay allows to perform a carrier sense after a
 ;; choosen delay (useful for randomisation of transmissions)
  (set-state instance new-state)
  (set-timer instance 'carrier-sense delay))

(defun extend-active-period(instance &optional (extra 0))
 ;; This function will extend active period for MAC, ensuring that the
 ;; remaining active time it is not less than listenTimeout
 ;; value. Also a check TA message is scheduled here to allow the node
 ;; to go to sleep if activation timeout expires
  (let ((cur-time (get-clock instance)))
    (with-slots(conservative-ta activation-timeout listen-timeout) instance
      (cond
        (conservative-ta
         (incf cur-time extra)
         (while (< activation-timeout cur-time)
           (incf activation-timeout listen-timeout))
         (when (< (+ cur-time listen-timeout) activation-timeout)
           (return-from extend-active-period))
         (incf activation-timeout listen-timeout))
        ((< activation-timeout (+ cur-time listen-timeout extra))
         (setf activation-timeout (+ cur-time listen-timeout extra))))
      (set-timer instance 'check-ta (- activation-timeout cur-time)))))

(defmethod dequeue((instance tmac))
  (dequeue (buffer instance))
  (check-tx-buffer instance))

(defun check-tx-buffer(instance)
  (unless (empty-p (buffer instance))
    (let ((packet (peek (buffer instance))))
      (with-slots(tx-addr tx-retries max-tx-retries last-sequence-number)
          instance
        (setf tx-addr (destination packet)
              tx-retries max-tx-retries
              last-sequence-number -1)))))
