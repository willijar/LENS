;; TMAC/SMAC implementations
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

;;

;;; Code:
(in-package :lens.wsn)

(defpackage :lens.wsn.mac.tmac
  (:use :cl :cl-user :lens :lens.wsn)
  (:export #:tmac))

(in-package :lens.wsn.mac.tmac)

(defclass tmac-packet(mac-packet)
  ()
  (:default-initargs :name nil))

;; differe tmac packet types
;; mac type (1 byte)  + source + destination requires 9 bytes

(defclass tmac-sync-packet(tmac-packet) ;; sequence number but no source/dest
  ((sync :type integer :initarg :sync) ;; 4 bytes
   (sync-id :type integer :initarg :sync-id))  ;; 4 bytes
  (:default-initargs :byte-length 11 :name 'sync))

(defmethod duplicate((pkt tmac-sync-packet) &optional duplicate)
  (call-next-method)
  (copy-slots '(sync sync-id) pkt duplicate))

(defclass tmac-rts-packet(tmac-packet)
  ((nav :type time-type :initarg :nav))
  (:default-initargs :byte-length 13 :name 'rts)) ;; 4 bytes

(defmethod duplicate((pkt tmac-rts-packet) &optional duplicate)
  (call-next-method)
  (copy-slots '(nav) pkt duplicate))

(defclass tmac-cts-packet(tmac-packet)
  ((nav :type time-type :initarg :nav))
   (:default-initargs :byte-length 13 :name 'cts))

(defmethod duplicate((pkt tmac-cts-packet) &optional duplicate)
  (call-next-method)
  (copy-slots '(nav) pkt duplicate))

(defclass tmac-ds-packet(tmac-packet) ())
(defclass tmac-frts-packet(tmac-packet) ())

;; ack and data require sequence number (4 bytes)
(defclass tmac-data-packet(tmac-packet)
  ()
  (:default-initargs :header-overhead 11))

(defclass tmac-ack-packet(tmac-packet)
  ()
  (:default-initargs :byte-length 11 :name 'ack))

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
   (max-tx-retries
    :parameter t :initform 2 :type integer
    :documentation
    "This number of transmission attempts of a single unicast packet
that TMAC will perform. A transmission is considered successful only
if acknowledgment packet is received from the destination
node. Sending an RTS packet is also considered as a transmission
attempt. Note that this parameter does not apply to broadcast
packets.")
   (allow-sink-sync
    :parameter t :initform t :type boolean
    :documentation "If true allows sink node to start synchronisation
    immediately to avoid contention interval when creating a
    synchronisation schedule for the network, thus allowing for faster
    synchronisation, and consequently, better throughput (especially
    if packets need to be sent early in the simulation)")
   (use-frts
    :parameter t :initform nil :type boolean
    :documentation "enable/disable FRTS (Future Request To
    Send). Although in the original TMAC protocol it is not supported
    here.")
   (use-rts-cts
    :parameter t :initform t :type boolean
    :documentation "Enable/disable RTS/CTS handshake. If disabled (not
    in orginal TMAC) limits any transmission to a simple DATA - ACK
    exchange between nodes without the overhead of a reservation. More
    efficient for small packets.")
   (disable-TA-extension
    :parameter t :initform nil :type boolean
    :documentation "Disabling TA extension effectively creates an SMAC
    protocol if we also define an appropriate listen interval (10% of
    the whole frame).")
   (conservative-TA
     :parameter t :initform t :type boolean
     :documentation "Conservative activation timeout - will always
     stay awake for at least 15 ms after any activity on the radio")
   (resync-time
    :parameter t :type time-type :initform 6d0
    :documentation "The interval between broadcasting synchronization
    packets (in seconds). The value of this parameter is directly
    related to the clock drift of nodes in the simulation network. 40
    seconds is an adequate value to use with current clock drift
    model.")
   (contention-period
    :parameter t :type time-type :initform 10e-3
    :documentation "The duration of contention interval (i.e. interval
    where transmissions of randomized), in milliseconds, for any
    transmission attempt. The major effect of this parameter is to
    avoid transmission interference from neighbouring nodes.")
   (listen-timeout
     :parameter t :type time-type :initform 15e-3
     :documentation "The duration of listen timeout(can also be called
     activation timeout). This parameter defines the amount of time
     which has to pass without any activity on the wireless channel in
     order for a node to go to sleep in the current frame.")
   (wait-timeout
    :parameter t :type time-type :initform 5e-3
    :documentation "The duration of timeout for expecting a reply from
    another node. This reply may be a CTS packet or an ACK packet. If
    no reply is received after this time interval, then transmission
    attempt is considered failed and transmission attempt counter is
    decremented.")
   (frame-time
    :parameter t :type time-type :initform 610e-3
    :documentation "The length of each frame period for all nodes (in
    milliseconds). Nodes try to synchronise the start and end of each
    frame with a global schedule (with the possibility of more than
    one schedules). Note that this refers to the duration of the whole
    frame; the active and inactive portions of each frame are
    determined dynamically and individually for each node.")
   (collision-resolution
    :parameter t :type symbol :initform 'immediate-retry
    :documentation
    "Collision resolution mechanism, choose from
     - immediate-retry :: Low collision avoidance - retry immediately after losing channel)
     - overhearing  :: (default). Retry only when hear a CTS or RTS/
		 - retry-next-frame :: Aggressive collision avoidance - retry only in next frame.")

   ;; implementation values
   (state :type tmac-state :reader state :initform nil)
   (tx-addr :type integer
            :documentation "current communication peer (can be BROADCAST)")
   (tx-retries
    :type fixnum
    :documentation "number of transmission attempts to txAddr (when reaches 0 - packet is dropped)")
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
   (transmission-timeout :initform (make-instance 'timer-message))
   (check-ta  :initform (make-instance 'timer-message))
   (carrier-sense :initform (make-instance 'timer-message)))
  (:documentation "TMAC employs many techniques to keep the energy
  consumption low (using aggressive duty cycling and synchronization)
  while trying to keep performance (e.g. packet delivery) high by
  adapting its duty cycle according to the traffic needs. S-MAC is a
  predecessor of TMAC as it initiated many of the techniques but uses
  a more rigid duty cycle. This implementation is from CASTELIA - see::

 Yuri Tselishchev, Athanassios Boulis, Lavy Libman, “Experiences and
 Lessons from Implementing a Wireless Sensor Network MAC Protocol in
 the Castalia Simulator,” submitted to IEEE Wireless Communications &
 Networking Conference 2010 (WCNC 2010), Sydney, Australia.

The parameters for SMAC are defined in the SMAC.ini configuration
file.
")
  (:metaclass module-class))

(defmethod startup((instance tmac))
  ;; start listening and schedule a time to sleeep if needed
  (set-slots
   instance
   '((state setup)
     (primary-wakeup t)
     (need-resync nil)
     (current-frame-start -1)
     (activation-timeout 0)
     sync-packet
     rts-packet
     cts-packet
     ack-packet))
  (with-slots(allow-sink-sync frame-time) instance
    (if (and allow-sink-sync (sink-p instance))
        (set-timer instance 'sync-create 0.1)
        (set-timer instance 'sync-setup
                   (if allow-sink-sync (* 3 frame-time) 0.1))))
  (to-radio instance '(set-cs-interrupt . t)))

(defmethod  handle-timer :before ((instance tmac) timer)
  (tracelog "Timer:~A (state ~A)" timer (slot-value instance 'state)))

(defmethod handle-timer((instance tmac) (timer (eql 'sync-setup)))
  (with-slots(frame-time) instance
    (when (eql (state instance) 'setup)
      (set-timer instance 'sync-create (* frame-time (uniform 0 1 1))))))

(defmethod handle-timer((instance tmac) (timer (eql 'sync-create)))
  (when (eql (state instance) 'setup)
    (create-primary-schedule instance)))

(defmethod handle-timer((instance tmac) (timer (eql 'sync-renew)))
  (tracelog "Initiated RESYNC procedure")
  (incf (tmac-schedule-sn (aref (slot-value instance 'schedule-table) 0)))
  (setf (slot-value instance 'need-resync) t)
  (set-timer instance 'sync-renew (slot-value instance 'resync-time)))

(defmethod handle-timer((instance tmac) (timer (eql 'frame-start)))
  (with-slots(primary-wakeup current-frame-start active-timeout frame-time
              schedule-table need-resync) instance
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
  (when (eql (state instance) 'sleep)
    (to-radio instance '(set-state . rx)))
  (when (member (state instance) '(sleep active active-silent))
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
     (if (member (state instance) '(active sleep active-silent))
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
  (let ((state (state instance)))
    (when (member state '(sleep active-silent))
      (return-from handle-timer))
    (unless (member state '(carrier-sense-for-tx-rts
                            carrier-sense-for-tx-sync
                            carrier-sense-for-tx-data
                            carrier-sense-before-sleep))
      (tracelog "Warning: bad MAC state for carrier sense")
      (return-from handle-timer))
    (ecase (channel-clear-status (submodule (owner instance) 'radio))
      (clear (carrier-is-clear instance))
      (busy (carrier-is-busy instance))
      (cs-not-valid-yet
       (set-timer instance 'carrier-sense
                  (slot-value instance 'phy-delay-for-valid-cs))
       (tracelog "Set carrier sense timer ~:/dfv:eng/s" (slot-value instance 'phy-delay-for-valid-cs)))
      (cs-not-valid
       (unless (eql state 'sleep)
         (to-radio instance '(set-state . rx))
         (set-timer instance 'carrier-sense
                    (slot-value instance 'phy-delay-for-valid-cs))
         (tracelog "Set carrier sense timer ~:/dfv:eng/s" (slot-value instance 'phy-delay-for-valid-cs)))))))

(defmethod handle-timer((instance tmac) (timer (eql 'transmission-timeout)))
  (reset-default-state instance "timeout"))

(defmethod handle-timer((instance tmac) (timer (eql 'wakeup-silent)))
 ;;This is the wakeup timer for secondary schedules. here we only wake
 ;;up the radio and extend activation timeout for listening. NOTE that
 ;;default state for secondary schedules is MAC_STATE_ACTIVE_SILENT
  (with-slots(activation-timeout) instance
    (setf activation-timeout (get-clock instance))
    (extend-active-period instance)
    (when (eql (state instance) 'sleep)
      (to-radio instance '(set-state . rx))
      (reset-default-state instance "secondary schedule starts"))))

(defmethod handle-timer((instance tmac) (timer integer))
  ;; timer is wakeup inhdexed into schedule table
  (tracelog "Timer: ~D" timer)
  (if (< 0 timer (length (slot-value instance 'schedule-table)))
      (handle-timer instance 'wakeup-silent)
      (tracelog "Unknown timer ~A" timer)))

(defmethod handle-message((instance tmac)
                          (message radio-control-message))
  (when (eql (command message) 'lens.wsn:carrier-sense-interrupt)
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
         :destination
         #+CASTELIA-COMPATABILITY broadcast-mac-address
         ;; note that in castelia the virtualmac encapsulate overwrites specific
         ;; mac packet address with broadcast address (probably an error)
         #-CASTELIA-COMPATABILITY (next-hop (control-info packet))
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
             (set-state instance 'sleep "active period expired (SMAC)"))
           (perform-carrier-sense instance 'carrier-sense-before-sleep)))
      (primary-wakeup
       (let ((random-contention-interval (uniform 0.0 contention-period 1)))
         (tracelog "Random contention interval=~:/dfv:eng/s" random-contention-interval)
         (when need-resync
           (when sync-packet (cancel sync-packet))
           (setf sync-packet
                 (make-instance
                  'tmac-sync-packet
                  :destination broadcast-mac-address
                  :source (mac-address instance)
                  :sync-id (tmac-schedule-id (aref schedule-table 0))
                  :sync (- (+ current-frame-start frame-time)
                           (+ (get-clock instance)
                              (tx-time instance sync-packet-size)
                              random-contention-interval))
                  :sequence-number (tmac-schedule-sn (aref schedule-table 0))
                  :header-overhead sync-packet-size))
           (perform-carrier-sense instance 'carrier-sense-for-tx-sync
                                  random-contention-interval)
           (return-from reset-default-state))
         (with-slots(tx-retries use-rts-cts tx-addr) instance
           (while (not (empty-p (buffer instance)))
             (cond
               ((<= tx-retries 0)
                (tracelog "Transmission failed to ~A" tx-addr)
                (dequeue instance))
               (t
                (perform-carrier-sense
                   instance
                   (if (and use-rts-cts
                            (not (eql tx-addr broadcast-mac-address)))
                       'carrier-sense-for-tx-rts
                       'carrier-sense-for-tx-data)
                   random-contention-interval)
                (return-from reset-default-state)))))
         (set-state instance 'active "nothing to transmit")))
      (;; primary-wakup is false
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
        (tracelog "Creating schedule ID:~D SN:~D Wakeup:~:/dfv:eng/s"
                  (tmac-schedule-id schedule) (tmac-schedule-sn schedule)
                  wakeup)
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
      (with-slots(use-frts collision-resolution) instance
        (when (and (eql (state instance) 'carrier-sense-for-tx-rts)  use-frts)
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
           :header-overhead cts-packet-size))
    (to-radio instance cts-packet)
    (to-radio instance '(set-state . tx))
    (emit instance 'mac-packet-breakdown "CTS Sent")
    (setf cts-packet nil)
    (set-state instance 'wait-for-data "sent CTS packet")
    (set-timer instance 'transmission-timeout
               (+ (tx-time instance cts-packet-size) wait-timeout))))

(defmethod handle-message((instance tmac) (cts-packet tmac-cts-packet))
  (with-slots(tx-addr) instance
    (when (eql (state instance) 'wait-for-cts)
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
  (when (eql (state instance) 'wait-for-data)
    (cancel-timer instance 'transmission-timeout))
  (with-slots(ack-packet ack-packet-size) instance
    (when ack-packet (cancel ack-packet))
    (setf ack-packet
          (make-instance
           'tmac-ack-packet
           :source (mac-address instance)
           :destination (source packet)
           :header-overhead ack-packet-size
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
  (when (and (eql (state instance) 'wait-for-ack)
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
  "Since we are hearing some communication on the radio we need to do two things:
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
  (ecase (state instance)
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
           :header-overhead rts-packet-size))
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
  (set-timer instance 'carrier-sense delay)
  (tracelog "Set carrier sense timer ~:/dfv:eng/s" delay))

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
      (tracelog "Extend Active Period activation timeout=~:/dfv:eng/s"
                activation-timeout)
      (set-timer instance 'check-ta (- activation-timeout cur-time)))))

(defun check-tx-buffer(instance)
  ;; if buffer not empty update communication parameters
  (unless (empty-p (buffer instance))
    (let ((packet (peek (buffer instance))))
      (with-slots(tx-addr tx-retries max-tx-retries last-sequence-number)
          instance
        (setf tx-addr (destination packet)
              tx-retries max-tx-retries)))))

(defmethod dequeue((instance tmac))
  (let ((p (dequeue (buffer instance))))
    (cancel p)
    (check-tx-buffer instance)))
