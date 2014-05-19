;; Partial Implementation of IEEE802.15.6 (Baseline BAN) MAC Layer
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

(defpackage :lens.wsn.mac.802.15.6
  (:use :cl :cl-user :lens :lens.wsn)
  (:export #:mac802.15.6))

(in-package :lens.wsn.mac.802.15.6)

(register-signal 'desync "Signal emitted when PAN connection lost")

(deftype security-level()
  '(member not-secured authenticated-not-encrypted authenticated-and-encrypted))

(deftype acknowledgement-policy()
  '(member n-ack i-ack l-ack b-ack))

(deftype frame-type() '(member management control data))

(deftype frame-subtype()
  '(member reserved beacon association disassociation ptk gtk
    connection connection-assignment disconnection i-ack b-ack
    i-ack-poll b-ack-poll poll t-poll))

(deftype status-code()
  '(member accepted rej-no-bandwidth rej-no-nid rej-no-resources
    rej-no-high-security rej-no-low-security rej-no-reason modified))

(defconstant BROADCAST-NID 255)
(defconstant UNCONNECTED-BROADCAST-NID 0)
(defconstant UNCONNECTED -1)

(defmethod header-overhead((type (eql 'mac802.15.6-packet))) 7)

(defclass mac802.15.6-packet(mac-packet)
  ((header-overhead :initform (header-overhead 'mac802.15.6-packet)
                    :allocation :class :reader header-overhead)
   ;;MAC Header fields
   (hid :type integer :accessor hid :initarg :hid :initform unconnected
        :documentation "2 last bytes of full MAC address")
   (nid :type integer :accessor nid :initarg :hid :initform unconnected
        :documentation "1 bytes short MAC address")
   ;;MAC header control fields
   ;;int protocolVersion;							//2 bits
   ;; int securityLevel enum(SecurityLevel_type);		//2 bits
   ;;int TKindex;									//1 bit
   ;;int retry;										//1 bit
   (ack-policy :type acknowledgement-policy :initarg :ack-policy
               :accessor ack-policy)
   (frame-type
    :type frame-type :initarg :frame-type :accessor frame-type)
   (frame-subtype
    :type frame-subtype :initarg :frame-subtype :accessor frame-subtype)
   (more-data :type integer :initform 0 :initarg more-data
              :accessor more-data)
   ;;int firstFrame;									//1 bit		total: 2 bytes
   (fragment-number :initarg :fragment-number :initform 0
                    :reader fragment-number))
  (:documentation "Base for all baseline MAC packets types"))

(defmethod duplicate((pkt mac802.15.6-packet) &optional duplicate)
  (call-next-method)
  (copy-slots '(hid nid ack-policy frame-type frame-subtype fragment-number)
              pkt duplicate))

;; different packet types

(defclass beacon-packet(mac802.15.6-packet)
  ((header-overhead :initform (+ (header-overhead 'mac802.15.6-packet) 17)
                    :allocation :class :reader header-overhead)
   ;;int beaconShiftingSequenceIndex; 			//1 byte
   ;; int beaconShiftingSequencePhase;
   (beacon-period-length
    :type fixnum :accessor beacon-period-length :initarg :beacon-period-length)
   (allocation-slot-length
    :type fixnum :accessor allocation-slot-length
    :initarg :allocation-slot-length)
   (rap1-length :type fixnum :initarg :rap1-length :accessor rap1-length)
   (rap2-length :type fixnum :initarg :rap2-length :accessor rap2-length))
  (:default-initargs :frame-type 'management :frame-subtype 'beacon
                     :ack-policy 'n-ack))

(defmethod duplicate((pkt beacon-packet) &optional duplicate)
  (call-next-method)
  (copy-slots '(beacon-period-length allocation-slot-length rap1-length
                rap2-length)
              pkt duplicate))

(defclass connection-request-packet(mac802.15.6-packet)
  ((header-overhead :initform (+ (header-overhead 'mac802.15.6-packet) 28)
                    :allocation :class :reader header-overhead)
   (source :accessor sender-address)
   (destination :accessor recipient-address)
   (former-hub-address
    :initarg :former-hub-address :accessor former-hub-address)
   ;;int changeIndication;						//1 byte
   (next-wakeup :type integer :initarg :next-wakeup :accessor next-wakeup)
   (wakeup-interval
    :type integer :initarg wakeup-interval :accessor wakeup-interval)
   ;; simplified request - number of slots requested
   (uplink-request
    :type integer :initarg :uplink-request :accessor uplink-request
    :initform 0)
   (downlink-request
    :type integer :initarg :downlink-request :accessor downlink-request
    :initform 0)
   (bilink-request
    :type integer :initarg :bilink-request :accessor bilink-request
    :initform 0)))

(defmethod duplicate((pkt connection-request-packet) &optional duplicate)
  (call-next-method)
  (copy-slots '(former-hub-address next-wakeup  wakeup-interval uplink-request
                downlink-request bilink-request)
              pkt duplicate))

(defclass connection-assignment-packet(mac802.15.6-packet)
  ((header-overhead :initform (+ (header-overhead 'mac802.15.6-packet) 29)
                    :allocation :class :reader header-overhead )
   (source :accessor sender-address)
   (destination :accessor recipient-address)
   ;;int channelDwellLength;						//1 byte
   ;;int channelDwellPhase;						//1 byte
    ;;int minRAPlength;							//1 byte
   (status-code :type status-code :initarg :status-code
                :accessor status-code)
   (assigned-nid :type integer :initarg :assigned-nid :accessor :assigned-nid)
   ;;int changeIndication;                       //1 byte
   ;;:type integer :initarg :next-wakeup :accessor next-wakeup)
   (wakeup-interval
    :type integer :initarg wakeup-interval :accessor wakeup-interval)
   ;; simplified request - number of slots requested
   (uplink-request-start
    :type integer :initarg :uplink-request-start :accessor uplink-request-start
    :initform 0)
   (uplink-request-end
    :type integer :initarg :uplink-request-end :accessor uplink-request-end
    :initform 0)
   (downlink-request-start
    :type integer :initarg :downlink-request-start
    :accessor downlink-request-start :initform 0)
   (downlink-request-end
    :type integer :initarg :downlink-request-end :accessor downlink-request-end
    :initform 0)
   (bilink-request-start
    :type integer :initarg :bilink-request-start :accessor bilink-request-start
    :initform 0)
   (bilink-request-end
    :type integer :initarg :bilink-request-end :accessor bilink-request-end
    :initform 0)))

(defmethod duplicate((pkt connection-request-packet) &optional duplicate)
  (copy-slots
   '(status-code assigned-nid next-wakeup  wakeup-interval
     uplink-request-start downlink-request-start bilink-request-start
     uplink-request-end downlink-request-end bilink-request-end)
   pkt duplicate)
  (call-next-method))

(deftype mac802.15.6-state ()
  '(member setup rap free-tx-access free-rx-access beacon-wait sleep))

(defvar CW-min #(16 16 8 8 4 4 2 1))
(defvar CW-max #(64 32 32 16 16 8 8 4))

(defstruct timer-info
  (nid 0 :type fixnum)
  (slots-given 0 :type fixnum)
  (end-slot 0 :type fixnum))

(defstruct slot-assign
  (nid 0 :type fixnum)
  (start-slot 0 :type fixnum)
  (end-slot 0 :type fixnum))

(defstruct access-slot
  (scheduled 0 :type fixnum)
  (polled 0 :type fixnum))

(defclass mac802.15.6(mac)
  ((print-state-transitions
    :parameter t :type boolean :initform nil
    :documentation "Debugging parameter")

   ;; common mac layer parameters
   (max-mac-frame-size :parameter t :initform 1000)
   (header-overhead :parameter t :initform 7)
   (buffer-size :parameter t :initform 32)
   (is-hub
    :parameter t :type boolean :initform nil :reader is-hub
    :documentation "Is the node a hub (coordinator device)?")
   (allocation-slot-length
    :parameter t :type time-type :initform 10d-3 :reader allocation-slot-length
    :documentation "Defines the length of the basic allocation slot.")
   (beacon-period-length
    :parameter t :type integer :initform 32 :reader beacon-period-length
    :documentation "Defines the beacon period length in allocation
    slots (for Hub)")
   (RAP1-length
    :parameter t :type integer :initform 8 :reader RAP1-length
    :documentation "Defines the Random Access Period length in
    slots (for Hub)")
   (scheduled-access-length
    :parameter t :type integer :initform 0 :reader scheduled-access-length
    :documentation "The slots asked by a sensor node from the hub to be assigned for scheduled access.")
   (scheduled-access-period
    :parameter t :type integer :initform 1 :reader scheduled-access-period
    :documentation "If asking allocation slots for scheduled access,
    how often (in beacon periods) is the scheduled access requested
    for.")

   (max-packet-retries
    :parameter t :type integer :initform 2 :reader max-packet-retries
    :documentation "The maximum number of tries a packet will be attempted for successful reception.")
   (contention-slot-length
    :parameter t :type time-type :initform 0.36e-3
    :reader contention-slot-length
    :documentation "The length of the mini-slots used in contention")
   (enhance-guard-time
    :parameter t :type boolean :initform nil :reader enhance-guard-time
    :documentation "The draft proposal suggests guard times to account
    for worse cases of de-synchronization among the nodes. The default
    used here is that that while ending a TX we guard for 2GT instead
    of GT. If true this parameter allows the more conservative guard time to be
    used.")
   (enhance-more-data
    :parameter t :type boolean :initform nil :reader enhance-more-data
    :documentation "If this parameter is true then the more-data flag
    of the protocol carries information on how many more packets are
    waiting in the buffer to be transmitted, not just if there are
    more packets to transmit.")
   (polling-enabled
    :parameter t :type boolean :initform nil :reader polling-enabled
    :documentation "If true polling will be attempted.")
   (naive-polling-scheme
    :parameter t :type boolean :initform nil :reader naive-polling-scheme
    :documentation "A variable controlling how polling will be performed")

   (tifs
    :parameter t :type time-type :initform 0.03d-3 :reader tifs
    :documentation "Time to start TXing a frame after you received one")
   (clock-accuracy
    :parameter t :type time-type :initform 0.0001 :reader clock-accuracy
    :documentation "The clock drift of the node's clock in parts per
    unit of time. Default is equal to 100ppm (parts per million")

   ;; state variables
   (connected-hid :type integer :accessor connected-hid)
   (connected-nid :type integer :accessor connected-nid :initform BROADCAST-NID)
   (unconnected-nid :type integer :accessor unconnected-nid)

   (scheduled-tx-access-start
    :type integer :accessor scheduled-tx-access-start :initform unconnected)
   (scheduled-tx-access-end
    :type integer :accessor scheduled-tx-access-end :initform unconnected)
   (scheduled-rx-access-start
    :type integer :accessor scheduled-rx-access-start :initform unconnected)
   (scheduled-rx-access-end
    :type integer :accessor scheduled-rx-access-end :initform unconnected)

   (polled-access-end :type integer :accessor polled-access-end)
   (posted-access-end :type integer :accessor posted-access-end)

   (current-packet-transmissions
    :type integer :accessor current-packet-transmissions :initform 0)
   (current-packet-cs-fails
    :type integer :accessor current-packet-cs-fails :initform 0)

   (cw :type integer :accessor cw)
   (cw-double :type boolean :accessor cw-double :initform nil)
   (backoff-counter :type integer :initform 0 :accessor backoff-counter)

   (state :type mac802.15.6-state :reader state :initform 'setup)
   (past-sync-interval-nominal
    :type boolean :accessor past-sync-interval-nominal :initform nil)
   (sync-interval-additional-start
    :type time-type :accessor sync-interval-additional-start)

   (packet-to-be-sent
    :type mac802.15.6-packet :initform nil :accessor packet-to-be-sent)
   (end-time
    :type time-type :accessor end-time
    :documentation "the time when our right to TX ends. Covers RAP,
    scheduled and polled access")
   (frame-start-time
    :type time-type :accessor frame-start-time
    :documentation "time the frame starts (when we receive the beacon
    - beacon TX time)")

   (si-nominal :type time-type :accessor si-nominal :initform -1)

   (waiting-for-ack :type boolean :accessor waiting-for-ack :initform nil)
   (future-attempt-to-tx :type boolean :accessor future-attempt-to-tx
                         :initform nil)
   (attempting-to-tx :type boolean :accessor attempting-to-tx :initform nil)
   (is-poll-period :type boolean :accessor is-poll-period :initform nil)

   (mgmt-queue
    :accessor mgmt-queue :type packet-buffer
    :initform (clrs:make-queue :initial-size 16 :implementation :wrap)
    :documentation "a buffer to store Management packets that require
    ack and possible reTX these packets are treated like data packets,
    but with higher priority")

   ;; hub-specific state
   (current-slot :type integer :accessor current-slot :initform -1)
   (send-i-ack-poll :type boolean :accessor send-i-ack-poll :initform nil)
   (current-first-free-slot :type integer :accessor current-first-free-slot)
   (current-free-connected-nid
    :type integer :accessor current-free-connected-nid :initform 16
    :documentation "start assigning connected NID from ID 16")
   (next-future-poll-slot :type integer :accessor next-future-poll-slot
                          :initform -1)
   (last-tx-access-slot
    :type array :initform (make-array 216)
    :accessor last-tx-access-slot)
   (req-to-send-more-data
    :type array :initform (make-array 216 :element-type 'integer)
    :accessor req-to-send-more-data)
   (hub-poll-timers :initform (make-queue :initial-size 16) :reader hub-poll-timers)
   (slot-assignment-map :reader slot-assignment-map :initform (make-hash-table)) )
  (:properties
   :statistic (desync
               :title "Fraction of time without PAN connection"
               :default ((accumulated-time :fractional t :initial-state t
                                           :format "~,8f"))))
  (:documentation "IEEE802.15.6 Body area network MAC
  implementation (see draft proposal MAC and Security Baseline
  Proposal,
  [https://mentor.ieee.org/802.15/dcn/10/15-10-0196-02-0006-mac-and-security-baseline-proposal-c-normative-text-doc.doc]).

")
  (:metaclass module-class))

(defmethod more-data((mac mac802.15.6))
  "Return more-data field for DATA frames"
  (if (is-hub mac)
      0
      (let((n (+ (size (buffer mac)) (size (mgmt-queue mac)))))
        (if (or (zerop n) (enhance-more-data mac)) n 1))))

(defun guard-time(mac)
  (+ (/ (allocation-slot-length mac) 10d0)
     (if (past-sync-interval-nominal mac)
         (extra-guard-time mac)
         0.0)))

(defun guard-tx-time(mac)
  (if (past-sync-interval-nominal mac)
         (extra-guard-time mac)
         0d0))

(defun extra-guard-time(mac)
  (* (- (simulation-time mac) (sync-interval-additional-start mac))
     (clock-accuracy mac)))

(defun guard-factor(mac) (if (enhance-guard-time mac) 2d0 1d0))

(defun sleep-to-tx(mac)
  (lens.wsn::transition-delay (radio mac) 'sleep 'tx))

(defun is-radio-sleeping(mac)
  (eql (state (radio mac)) 'sleep))

(defmethod priority((mac mac802.15.6))
  (priority (submodule (owner (owner mac)) 'application)))

(defmethod startup((instance mac802.15.6))
  (cond
    ((is-hub instance)
     (setf (connected-hid instance)
           (logand (mac-address instance) #xFFFFFF));; 16LSbits as short address
     (setf (current-first-free-slot instance)
           (1+ (rap1-length instance)))
     (set-timer instance 'send-beacon 0)
     (dotimes(i (length (last-tx-access-slot instance)))
       (setf (aref (last-tx-access-slot instance) i)
             (make-access-slot)))
     (reinitialise-slots
      '(connected-nid current-free-connected-nid
        req-to-send-more-data
        hub-poll-timers slot-assignment-map)
      instance))
     (t
      (setf (connected-hid instance) unconnected
            (connected-nid instance) unconnected
            (unconnected-nid instance) (intuniform 1 15))
      (tracelog "Selected random unconnected NID ~A" (unconnected-nid instance))
       (reinitialise-slots
        '(past-sync-interval-nominal si-nominal) instance)
       (set-state instance 'setup) ))
  (reinitialise-slots
   '(send-i-ack-poll current-slot next-future-poll-slot cw-double
     backoff-counter packet-to-be-sent current-packet-transmissions
     current-packet-cs-fails waiting-for-ack future-attempt-to-tx
     attempting-to-tx is-poll-period
     scheduled-tx-access-start scheduled-tx-access-end
     scheduled-rx-access-start scheduled-rx-access-end)
   instance)
  (setf (cw instance) (aref cw-min (priority instance))))

(defmethod handle-timer((instance mac802.15.6) (timer (eql 'carrier-sensing)))
  (cond
    ((can-fit-tx instance)
     (if (eql (channel-clear-status (radio instance)) 'clear)
         (if (> (decf (backoff-counter instance)) 0)
             (set-timer instance 'carrier-sensing
                        (contention-slot-length instance))
             (send-packet instance))
         (set-timer instance 'carrier-sensing
                    (* 3.0  (contention-slot-length instance)))))
    (t
     (setf (attempting-to-tx instance) nil)
     (incf (current-packet-cs-fails instance)))))

(defmethod handle-timer((instance mac802.15.6) (timer (eql 'start-attempt-tx)))
  (setf (future-attempt-to-tx instance) nil)
  (attempt-tx instance))

(defmethod handle-timer((instance mac802.15.6) (timer (eql 'ack-timeout)))
  (tracelog "ACK timeout fired")
  (setf (waiting-for-ack instance) nil)
  ; double the Contention Window, after every second fail.
  (when (setf (cw-double instance) (not (cw-double instance)))
    (setf (cw instance) (max (* 2 (cw instance))
                             (aref cw-max (priority instance)))))
  ;; check if we reached the max number and if so delete the packet
  (when (>= (+ (current-packet-transmissions instance)
               (current-packet-cs-fails instance))
            (max-packet-retries instance))
    (with-slots(packet-to-be-sent) instance
      (emit instance 'mac-packet-breakdown
          (if (eql (frame-type packet-to-be-sent) 'data)
              "Data Failed, No Ack"
              "Mgmt & Ctrl Failed, No Ack"))
      (cancel packet-to-be-sent)
      (setf packet-to-be-sent nil
            (current-packet-transmissions instance) 0
            (current-packet-cs-fails instance) 0)))
  (attempt-tx instance))

(defmethod handle-timer((instance mac802.15.6) (timer (eql 'start-sleeping)))
  (set-state instance 'sleep)
  (to-radio  instance '(set-state . sleep))
  (setf (is-poll-period instance) nil))

(defmethod handle-timer((instance mac802.15.6)
                        (timer (eql 'start-scheduled-tx-access)))
  (set-state instance 'free-tx-access)
  (with-slots(scheduled-tx-access-end scheduled-tx-access-start) instance
    (let ((dt (* (+ scheduled-tx-access-end scheduled-tx-access-start)
                 (allocation-slot-length instance))))
      (setf (end-time instance)  (+ (get-clock instance) dt))
      (when (> (beacon-period-length instance) scheduled-tx-access-end)
        (set-timer instance 'start-sleeping dt))
      (attempt-tx instance))))

(defmethod handle-timer((instance mac802.15.6)
                        (timer (eql 'start-scheduled-rx-access)))
  (set-state instance 'free-rx-access)
  (to-radio  instance '(set-state . rx))
  (with-slots(scheduled-rx-access-end scheduled-rx-access-start) instance
    (let ((dt (* (+ scheduled-rx-access-end scheduled-rx-access-start)
                 (allocation-slot-length instance))))
      (when (> (beacon-period-length instance) scheduled-rx-access-end)
        (set-timer instance 'start-sleeping dt)))))

(defmethod handle-timer((instance mac802.15.6)
                        (timer (eql 'start-posted-access)))
  (set-state instance 'free-rx-access)
  (to-radio  instance '(set-state . rx))
  ;; reset the timer for sleeping as needed
  (with-slots((pae posted-access-end)) instance
    (if (and (/= (1- pae) (beacon-period-length instance))
             (/= pae (scheduled-tx-access-start instance))
             (/= pae (scheduled-rx-access-start instance)))
    (set-timer instance 'start-sleeping (allocation-slot-length instance))
    (cancel-timer instance 'start-sleeping))))

(defmethod handle-timer((instance mac802.15.6)
                        (timer (eql 'wakeup-for-beacon)))
  (set-state instance 'beacon-wait)
  (to-radio  instance '(set-state . rx))
  (setf (is-poll-period instance) nil))

(defmethod handle-timer((instance mac802.15.6)
                        (timer (eql 'sync-interval-timeout)))
  (setf (past-sync-interval-nominal instance) t
        (sync-interval-additional-start instance) (get-clock instance)))

(defmethod handle-timer((instance mac802.15.6) (timer (eql 'start-setup)))
  (set-state instance 'setup))

(defmethod handle-timer((instance mac802.15.6) (timer (eql 'send-beacon)))
  (let ((beacon-period-length (beacon-period-length instance))
        (allocation-slot-length (allocation-slot-length instance)))
    (tracelog "BEACON SEND, next beacon in ~:/dfv:eng/s"
              (* beacon-period-length allocation-slot-length))
    (set-state instance 'rap)
    (set-timer instance 'send-beacon
               (* beacon-period-length allocation-slot-length))
    (set-timer instance 'hub-scheduled-access
               (* (rap1-length instance)  allocation-slot-length))
    (setf (end-time instance)
          (+ (get-clock instance)
             (* (rap1-length instance)  allocation-slot-length)))
    (let ((pkt
           (make-instance
            'beacon-packet
            :source (mac-address instance)
            :ack-policy 'n-ack
            :frame-type 'management
            :frame-subtype 'beacon
            :nid broadcast-nid
            :allocation-slot-length
            (* allocation-slot-length
               (slot-value instance 'max-mac-frame-size))
            :beacon-period-length beacon-period-length
            :rap1-length (rap1-length instance))))
    (to-radio instance pkt)
    (to-radio instance '(set-state . tx))
    (set-timer instance 'start-attempt-tx
               (+ (tx-time instance pkt) (* 2 (tifs instance))))
    (setf (future-attempt-to-tx instance) t)
    (emit instance 'mac-packet-breakdown "beacons sent")
    (setf (frame-start-time instance) (get-clock instance)
          (current-slot instance) 1)
    (set-timer instance 'increment-slot allocation-slot-length)
    (setf (next-future-poll-slot instance) (current-first-free-slot instance))
    (when (and (polling-enabled instance)
               (naive-polling-scheme instance)
               (<= (next-future-poll-slot instance) beacon-period-length))
      (set-timer instance 'send-future-polls
                 (* (1- (next-future-poll-slot instance))
                        allocation-slot-length))))))

(defmethod handle-timer((instance mac802.15.6) (timer (eql 'send-future-polls)))
  (set-state instance 'free-tx-access)
  (setf (end-time instance)
        (+ (get-clock instance) (allocation-slot-length instance)))
  (let ((available-slots (1- (- (beacon-period-length instance)
                                (1- (current-slot instance))))))
    ;; The current slot is used to TX the future polls, so we have 1 less slot available
    (when (<= available-slots 0) (return-from handle-timer))
    (let ((next-poll-start (1+ (current-slot instance)))
          (total-requests (reduce #'+ (req-to-send-more-data instance))))
      ;;Our (immediate) polls should start one slot after the current one.
      (when (zerop total-requests) (return-from handle-timer))
      (dotimes(nid (length (req-to-send-more-data instance)))
        (let ((rtsmd (aref  (req-to-send-more-data instance) nid)))
          (when (> rtsmd 0)
            ;; simple assignment scheme can leave sever slots unused
            (let ((slots-given
                   (floor (* rtsmd available-slots) total-requests)))
              (unless (zerop slots-given)
                (enqueue (make-timer-info
                          :nid nid
                          :slots-given slots-given
                          :end-slot (+ next-poll-start (1- slots-given)))
                         (hub-poll-timers instance))
                (setf (aref (req-to-send-more-data instance) nid) 0);;reset request
                (let ((pkt
                       (make-instance
                        'mac802.15.6-packet
                        :name "Future Poll"
                        :ack-policy 'n-ack
                        :frame-type 'management
                        :frame-subtype 'poll
                        :source (mac-address instance)
                        :nid nid
                        :sequence-number next-poll-start
                        :fragment-number 0
                        :more-data 1)))
                  (tracelog "Created Future POLL for NID: ~D, for slot ~D"
                            nid next-poll-start)
                  (incf next-poll-start slots-given)
                  (enqueue pkt (mgmt-queue instance))))))))))
  (unless (empty-p (hub-poll-timers instance))
    (set-timer instance 'send-poll (allocation-slot-length instance)))
  (attempt-tx instance))

(defmethod handle-timer((instance mac802.15.6) (timer (eql 'send-poll)))
  (assert (not (empty-p (hub-poll-timers instance))))
  (set-state instance 'free-rx-access)
  (let ((ti (dequeue (hub-poll-timers instance))))
    (to-radio
     instance
     (make-instance
      'mac802.15.6-packet
      :name "Immediate Poll"
      :ack-policy 'n-ack
      :frame-type 'management
      :frame-subtype 'poll
      :source (mac-address instance)
      :nid (timer-info-nid ti)
      :sequence-number (timer-info-end-slot ti)
      :fragment-number 0
      :more-data nil))
    (let ((slots-given (timer-info-slots-given ti)))
      (emit instance 'poll-slots-given slots-given)
      (tracelog "POLL ~A" ti)
      (unless (empty-p  (hub-poll-timers instance))
        (set-timer instance 'send-poll
                   (* slots-given (allocation-slot-length instance)))))))

(defmethod handle-timer((instance mac802.15.6) (timer (eql 'increment-slot)))
  (when (< (incf (current-slot instance)) (beacon-period-length instance))
    (set-timer instance 'increment-slot (allocation-slot-length instance))))

(defmethod handle-timer((instance mac802.15.6)
                        (timer (eql 'hub-scheduled-access)))
  (set-state instance 'free-rx-access))

(defmethod handle-message((instance mac802.15.6) (packet routing-packet))
  ;; from routing layer
  (if (enqueue
       (encapsulate
        (make-instance
         'mac802.15.6-packet
         :source (mac-address instance)
         :destination (next-hop (control-info packet)))
        packet)
       instance)
      (attempt-tx instance)
      (emit instance 'mac-packet-breakdown "Fail, buffer overflow")))

(defmethod encapsulate((instance mac802.15.6) packet)
  ;; if this is a data packet but NOT from the Hub then set its moreData flag
	;; Hub needs to handle its moreData flag (signaling posts) separately
    (setf (more-data packet) (more-data instance))
    (call-next-method))

(defun is-packet-for-me(instance pkt)
  (with-slots(connected-hid connected-nid unconnected-nid) instance
    (let((pkt-hid (hid pkt))
         (pkt-nid (nid pkt)))
      (cond
        ((= connected-hid pkt-hid)
         (or (is-hub instance)
             (= pkt-nid connected-nid)
             (= pkt-nid broadcast-nid)))
        ((and (= connected-hid unconnected)
              (or (= pkt-nid unconnected-nid)
                  (= pkt-nid unconnected-broadcast-nid)
                  (= pkt-nid broadcast-nid)))
          ;; We need to check all cases of packets types. It is tricky
          ;; because when unconnected two or more nodes can have the
          ;; same NID. Some packets have a Recipient Address to
          ;; distinguish the real destination. Others can be filtered
          ;; just by type. Some like I-ACK we have to do more tests
          ;; and still cannot be sure
         (case (frame-subtype pkt)
           (connection-assignment
            (cond
              ((eql (destination pkt) (mac-address instance))
               (setf unconnected-nid (intuniform 1 15))
               (when (packet-to-be-sent instance)
                 (setf (unconnected-nid (packet-to-be-sent instance))
                       unconnected-nid ))
               (tracelog "Choosing NEW unconnected-nid ~D"unconnected-nid)
               nil)
              (t)))
           (connection-request nil)
           ((i-ack b-ack i-ack-poll b-ack-poll)
            (cond
              ((or (not (packet-to-be-sent instance))
                   (zerop (current-packet-transmissions instance)))
               (tracelog "While unconnected: ACK packet received with no packet to acknowledgement - renewing NID")
               (setf unconnected-nid (intuniform 1 15))
               (when (packet-to-be-sent instance)
                 (setf (unconnected-nid (packet-to-be-sent instance))
                       unconnected-nid ))
               (tracelog "Choosing NEW unconnected-nid ~D"unconnected-nid)
               nil)
              (t)))))))))

(defgeneric handle-control-packet(mac subtype packet))

(defmethod handle-message((instance mac802.15.6) (packet mac802.15.6-packet))
  ;; from radio
  (unless (is-packet-for-me instance packet) (return-from handle-message))
  (when (eql (frame-type packet) 'data)
    (to-network instance (decapsulate packet))
    (when (eql (ack-policy packet) 'b-ack)
      (warn "Block ACK not implemented"))
    (when (> (more-data packet) 0)
      (handle-post instance packet)))
  (when (eql (ack-policy packet) 'i-ack)
    (let ((ack-packet
           (make-instance
            'mac802.15.6-packet
            :name "ACK PACKET"
            :ack-policy 'n-ack
            :frame-type 'control
            :frame-subtype  (if (send-i-ack-poll instance) 'i-ack-poll 'i-ack)
            :nid (nid packet))))
      (when (eql (connected-hid instance) unconnected)
        (setf (hid ack-packet) (hid packet)))
      (when (send-i-ack-poll instance)
        (setf (more-data ack-packet) 1)
        (setf (send-i-ack-poll instance) nil)
        (unless (naive-polling-scheme instance)
          ;; If this node was not given a future poll already, update
          ;; the hubPollTimers and nextFuturePollSlot. Also if the
          ;; hubPollTimers is empty, schedule the timer to send this
          ;; first POLL [the one that the (future)I_ACK_POLL points
          ;; to]
          (with-slots(hub-poll-timers frame-start-time next-future-poll-slot
                                      allocation-slot-length) instance
            (when (or (empty-p hub-poll-timers)
                      (/= (nid packet)
                          (nid (alg:back (hub-poll-timers instance)))))
              (let ((poll-time
                     (+  frame-start-time
                         (* (1- next-future-poll-slot) allocation-slot-length)
                         (- (get-clock instance))))
                    (timer (make-timer-info
                            :nid (nid packet)
                            :slots-given 1
                            :end-slot next-future-poll-slot)))
                (tracelog
                 "TEST: frame-start-time=~A poll-from-start=~A timer=~A"
                 frame-start-time
                 (* (1- next-future-poll-slot) allocation-slot-length)
                 poll-time)
                (when (empty-p hub-poll-timers)
                  (set-timer instance 'send-poll poll-time))
                (enqueue timer hub-poll-timers)
                (incf next-future-poll-slot)
                (tracelog "TEST: next future poll slot = ~A" next-future-poll-slot)
                (setf (access-slot-polled
                       (aref (last-tx-access-slot instance) (nid packet)))
                      (timer-info-end-slot timer))))))
        (let ((future-poll-slot
               (if (naive-polling-scheme instance)
                   (next-future-poll-slot instance)
                   (timer-info-end-slot (back (hub-poll-timers instance))))))
          (tracelog "Future POLL at slot ~A inserted in ACK packet"
                    future-poll-slot)
          (setf (slot-value ack-packet 'sequence-number) future-poll-slot)))
      (tracelog "Transmitted ACK to/from NID ~D" (nid packet))
      (to-radio instance ack-packet)
      (to-radio instance '(set-state . tx))
      ;; Any future attempts to TX should be done AFTER we are finished TXing
      ;; the I-ACK. To ensure this we set the appropriate timer and variable.
		  ;; BASELINEBAN_HEADER_SIZE is the size of the ack. 2*pTIFS is explained at sendPacket()
      (set-timer instance 'start-tx
                 (+ (tx-time instance ack-packet) (* 2 (tifs instance))))
      (setf (future-attempt-to-tx instance) t))
    (unless (eql (frame-type packet) 'data)
      (handle-control-packet instance  (frame-subtype packet) packet))))

(defmethod handle-control-packet((instance  mac802.15.6)
                                 (subtype (eql 'beacon))
                                 packet)
  (let ((beacon-tx-time (+ (tx-time instance packet) (tifs instance))))
    (setf (frame-start-time instance) (- (get-clock instance) beacon-tx-time)
          (allocation-slot-length instance)
          (/ (allocation-slot-length packet) (max-mac-frame-size instance))
          (si-nominal instance)
          (/ (- (/ (allocation-slot-length instance) 10.0) (tifs instance))
             (* 2 (clock-accuracy instance))))
    (set-timer instance 'sync-interval-timeout (si-nominal instance))
    (setf (beacon-period-length instance)  (beacon-period-length packet)
          (rap1-length instance) (rap1-length packet))
    (when (> (rap1-length instance) 0)
      (set-state instance 'rap)
      (setf (end-time instance)
            (+ (get-clock instance)
               (* (rap1-length instance) (allocation-slot-length instance))
               (- beacon-tx-time))))
    (emit instance 'mac-stats "beacons received")
    (tracelog "Beacon rx: reseting sync clock to ~As~%~Tslot=~A, beacon-period=~A slots~%~TRAP1=~D slots, RAP ends at time ~A"
              (si-nominal instance)
              (allocation-slot-length instance)
              (beacon-period-length instance)
              (rap1-length instance)
              (end-time instance))
    ;; Flush the Management packets buffer. Delete packetToBeSent
    ;; if it is a management packet This is a design choice. It
    ;; simplifies the flowcontrol and prevents rare cases where
    ;; management packets are piled up. More complicated schemes
    ;; chould be applied w.r.t. connection requests and
    ;; connection assignments.
    (with-slots((p packet-to-be-sent) (b mgmt-queue)) instance
      (when (and p (not (eql (frame-type p) 'data)))
        (cancel p)
        (setf p nil))
      (while(not (empty-p b))
        (cancel (dequeue b))))
    (cond
      ((eql (connected-hid instance) 'unconnected)
       (set-timer instance 'start-setup
                  (- (* (rap1-length instance) (allocation-slot-length instance))
                     beacon-tx-time))
       (tracelog "~t(unconnected): go back to setup mode when RAP ends")
       (when (>= (scheduled-access-length instance) 0)
         ;; We will try to connect to this BAN if our scheduled
         ;; access length is NOT set to unconnected (-1). If it is
         ;; set to 0, it means we are establishing a sleeping
         ;; pattern and waking up only to hear beacons and are only
         ;; able to transmit in RAP periods.
         (enqueue
          (make-instance
           'connection-request-packet
           :ack-policy 'i-ack
           :frame-type 'management
           :frame-subtype 'connection-request
           :hid (hid packet)
           :destination (source packet)
           :source (mac-address instance)
           :next-wakeup (1+ (sequence-number packet))
           :wakeup-interval (scheduled-access-period instance)
           :upling-request (scheduled-access-length instance))
          (mgmt-queue instance))
         (tracelog "~T(unconnected): created connection request")))
      (t
       ;; schedule a timer to wake up for the next beacon (it
       ;; might be m periods away
       (set-timer instance 'wakeup-for-beacon
                  (- (* (beacon-period-length instance)
                        (scheduled-access-period instance)
                        (allocation-slot-length instance))
                     beacon-tx-time
                     (guard-time instance)))
       ;; if we have a schedule that does not start after RAP, or
       ;; our schedule is not assigned yet, then go to sleep
       ;; after RAP.
       (when (or (and (eql (scheduled-tx-access-start instance) unconnected)
                      (< (rap1-length instance) (beacon-period-length instance)))
                 (> (1- (scheduled-tx-access-start instance))
                    (rap1-length instance)))
         (set-timer instance 'start-sleeping
                    (- (* (rap1-length instance)
                          (allocation-slot-length instance))
                       beacon-tx-time))
         (tracelog "~T--- start sleeping in ~A sec"
                   (- (* (rap1-length instance)
                         (allocation-slot-length instance))
                      beacon-tx-time)))
       (when (> (scheduled-tx-access-end instance)
                (scheduled-tx-access-start instance) )
         (set-timer instance 'scheduled-tx-access
                    (- (* (1- (scheduled-tx-access-end instance))
                          (allocation-slot-length instance))
                       beacon-tx-time
                       (guard-tx-time instance)))
         (tracelog "~T--- start scheduled TX access in ~A sec"
                   (- (* (1- (scheduled-tx-access-end instance))
                         (allocation-slot-length instance))
                      beacon-tx-time
                      (guard-tx-time instance))))))
    (attempt-tx instance)))

(defmethod handle-control-packet((instance  mac802.15.6)
                                 (subtype (eql 'i-ack-poll))
                                 packet)
  (handle-poll instance packet))

(defmethod handle-control-packet((instance  mac802.15.6)
                                 (subtype (eql 'i-ack))
                                 packet)
  (setf (waiting-for-ack instance) nil)
  (cancel-timer instance 'ack-timeout)
  (let ((packet (packet-to-be-sent instance)))
    (when (or (not packet)
              (zerop (current-packet-transmissions instance)))
    (tracelog  "WARNING: Received I-ACK with packetToBeSent being NULL, or not TXed!")
    (return-from handle-control-packet))
  (emit instance 'mac-packet-breakdown
        (format nil "~:[Mgmt & Ctrl~;Data~] pkt breakdown. Success ~:[2 or more tries~;1st try~]"
                (eql (frame-type packet) 'data)
                (= (current-packet-transmissions instance) 1)))
  (cancel packet)
  (setf (packet-to-be-sent instance) nil
        (current-packet-transmissions instance) 0
        (current-packet-cs-fails instance) 0
        (cw instance) (aref cw-min (priority instance))))
  (attempt-tx instance))

(defmethod handle-control-packet((instance  mac802.15.6)
                                 (subtype (eql 'b-ack-poll))
                                 packet)
  (handle-poll instance packet))

(defmethod handle-control-packet((instance  mac802.15.6)
                                 (subtype (eql 'b-ack))
                                 packet)
  (setf (waiting-for-ack instance) nil)
  (cancel-timer instance 'ack-timeout)
  (cancel (packet-to-be-sent instance))
  (setf (packet-to-be-sent instance) nil
        (current-packet-transmissions instance) 0
        (current-packet-cs-fails instance) 0
        (cw instance) (aref cw-min (priority instance)))
  (attempt-tx instance))

(defmethod handle-control-packet((instance  mac802.15.6)
                                 (subtype (eql 'connection-assignment))
                                 (packet connection-assignment-packet))
  (case (status-code packet)
    ((accepted modified)
     (setf (connected-hid instance) (hid packet)
           (connected-nid instance) (nid packet))
     (let ((packet-to-be-sent (packet-to-be-sent instance)))
       (when packet-to-be-sent
         (setf (hid packet-to-be-sent) (hid instance)
               (nid packet-to-be-sent) (nid instance))))
     (setf (scheduled-tx-access-start instance)
           (uplink-request-start packet)
           (scheduled-tx-access-end instance)
           (uplink-request-end packet))
     (tracelog "connected as nid ~A -- start tx access from slot ~D to slot ~D"
               (nid instance)
               (scheduled-tx-access-start instance)
               (scheduled-tx-access-end instance)))
    (t
     (tracelog "Connection requested REJECTED, status code ~A"
                (status-code packet)))))

(defmethod handle-control-packet((instance  mac802.15.6)
                                 (subtype (eql 'disconnection))
                                 packet)
  (setf (connected-hid instance) unconnected
        (connected-nid instance) unconnected))

(defmethod handle-control-packet((instance  mac802.15.6)
                                 (subtype (eql 'connection-request))
                                 (request connection-request-packet))
  ;; The ACK for the connection req packet is handled by the general
  ;; code. Here we need to create the connection assignment packet and
  ;; decide when to send it. We treat management packets that need
  ;; ack, similar to data packets, but with higher priority. They have
  ;; their own buffer.
  (let((assignment
        (make-instance
         'connection-assignment
         :ack-policy 'i-ack
         :frame-type 'management
         :subtype 'connection-assignment
         :nid (nid request)
         :destination (source request)))
       (slot-assign (gethash (source request) (slot-assignment-map instance))))
    (cond
      (slot-assign
       (setf (status-code assignment) 'accepted
             (nid assignment) (slot-assign-nid slot-assign)
             (uplink-request-start assignment) (slot-assign-start-slot slot-assign)
             (uplink-request-end assignment) (slot-assign-end-slot slot-assign))
       (tracelog
        "Connection request from NID ~D (~A) seen before. Assigning connected NID ~D"
        (nid request) (source request) (slot-assign-nid slot-assign)))
      (t
       ;; request  not processed before
       (cond
         ((> (uplink-request 'request)
             (- (beacon-period-length instance)
                (1- (current-first-free-slot instance))))
          (setf (status-code assignment) 'no-resources))
         ((> (current-free-connected-nid instance) 239)
          (setf (status-code assignment) 'no-nid))
         (t
          (let ((assigned
                 (make-slot-assign
                  :nid (current-free-connected-nid instance)
                  :start-slot (current-first-free-slot instance)
                  :end-slot (+ (current-first-free-slot instance)
                               (uplink-request request)))))
            (setf (gethash (source request) (slot-assignment-map instance))
                  assigned)
            (setf (status-code assignment) 'accepted
                  (nid assignment) (slot-assign-nid assigned)
                  (uplink-request-start assignment) (slot-assign-start-slot assigned)
                  (uplink-request-end assignment) (slot-assign-end-slot assigned))

            (tracelog
             "Connection request from NID ~D (~A) . Assigning connected NID ~D"
             (nid request) (source request) (slot-assign-nid assigned))
            (setf (aref (last-tx-access-slot instance)
                        (current-free-connected-nid instance))
                  (1- (slot-assign-end-slot assigned)))
            (incf (current-first-free-slot instance)
                  (uplink-request request))
            (incf (current-free-connected-nid instance))))))
      (enqueue assignment (mgmt-queue instance))
      (tracelog "Conn assignment created, wait for I-ACK before sending"))))

(defmethod handle-control-packet((instance  mac802.15.6)
                                 (subtype (eql 't-poll))
                                 packet)
  (handle-poll instance packet))

(defmethod handle-control-packet((instance  mac802.15.6)
                                 (subtype (eql 'poll))
                                 packet)
  (handle-poll instance packet))

(defmethod handle-control-packet((instance mac802.15.6) subtype packet)
  (tracelog "Warning: unimplemented packet subtype ~A in ~A" subtype packet))

(defun attempt-tx-in-rap(instance)
  (when (zerop (backoff-counter instance))
    (setf (backoff-counter instance) (1+ (intuniform 0 (cw instance)))))
  (setf (attempting-to-tx instance) t)
  (set-timer instance 'carrier-sensing 0))

(defmethod attempt-tx((instance mac802.15.6) &optional description)
  (declare (ignore description))
  (when (or (member (state instance) '(rap free-tx-access))
            (waiting-for-ack instance)
            (attempting-to-tx instance)
            (future-attempt-to-tx instance))
    ;; not in appropriate state or attemtping to TX now or in future or
    ;; awaiting ACK
    (return-from attempt-tx))
  (with-accessors((packet packet-to-be-sent)
                  (transmissions current-packet-transmissions)
                  (cs-fails current-packet-cs-fails)
                  (max-retries max-packet-retries)) instance
    (when (and packet (< (+ transmissions cs-fails) max-retries))
      (case (state instance)
        (rap (attempt-tx-in-rap instance))
        (free-tx-access
         (when (can-fit-tx instance) (send-packet instance))))
      (return-from attempt-tx))
    ;; if still packet in buffer after max retries delete it etc
    (when packet
      (tracelog "Max TX attempts reached. Last attempt was CS fail.")
      (emit
       instance
       'mac-packet-breakdown
       (format nil "~:[Mgmt & Ctrl~;Data~] pkt breakdown. Failed, ~:[No Ack~;Channel Busy~]"
                (eql (frame-type packet) 'data)
                (= cs-fails max-retries)))
      (cancel packet)
      (setf packet nil transmissions 0 cs-fails 0))

    ;; try and draw new packet from mgmt or data buffers
    (cond
      ((not (empty-p (mgmt-queue instance)))
       (setf packet (dequeue (mgmt-queue instance))))
      ((not (empty-p (buffer instance)))
       (setf packet (dequeue (buffer instance)))
       (setf (ack-policy packet) 'i-ack
             (frame-type packet) 'data
             (frame-subtype packet) 'reserved
             (more-data packet) (more-data instance))))
    (when packet
      (case (state instance)
        (rap (attempt-tx-in-rap instance))
        (free-tx-access
         (when (can-fit-tx instance) (send-packet instance)))))))

(defun can-fit-tx(instance)
  "This function lets us know if a transmission fits in the time we have (scheduled or RAP) It takes into account guard times too. A small issue exists with scheduled access:

 * Sleeping is handled at the timer code, which does not take into account the guard times.

 * In fact if we TX once then we'll stay awake for the whole duration of the scheduled slot."
  (and (packet-to-be-sent instance)
       (> (- (end-time instance)
             (get-clock instance)
             (* (guard-factor instance) (guard-time instance))
             (tx-time instance (packet-to-be-sent instance))
             (tifs instance))
          0)))

(defun send-packet(instance)
  (with-accessors((packet packet-to-be-sent)) instance
    (setf (attempting-to-tx instance) nil)
    ;; collect stats
    (when (eql (frame-type packet) 'data)
      (emit instance 'mac-packet-breakdown
            (if (eql (state instance) 'rap)
                "TX - RAP"
                (if (is-poll-period instance)
                    "TX - POLL"
                    "Scheduled"))))
    (cond
      ((member (ack-policy packet) '(i-ack b-ack))
       ;; Need to wait for ACK. Here we explicitly take into account
       ;; the clock drift, since the spec does not mention a rule for
       ;; ack timeout. In other timers, GUARD time takes care of the
       ;; drift, or the timers are just scheduled on the face value. We
       ;; also take into account sleep->TX delay, which the BaselineBAN
       ;; spec does not mention but it is important.
       (let ((ack-timeout
              (* (+ (tx-time instance packet)
                    (sleep-to-tx instance)
                    (* 2 (tifs instance))
                    (tx-time instance (header-overhead 'mac802.15.6-packet)))
                 (1+ (clock-accuracy instance)))))
         (tracelog "Txing ~A, ACK-TIMEOUT in ~A" packet ack-timeout)
         (set-timer instance 'ack-timeout ack-timeout)
         (setf (waiting-for-ack instance) t)
         (incf (current-packet-transmissions instance))
         (to-radio instance (duplicate packet))))
      (t
       ;; no need to wait for ack.
       ;; Start the process of attempting to
       ;; TX again. The spec does not provide details on this. Our
       ;; choice is more fair for contention-based periods, since we
       ;; contend for every pkt. If we are in a scheduled TX access
       ;; state the following implication arises: The spec would
       ;; expect us to wait the TX time + pTIFS, before attempting to
       ;; TX again. Because the pTIFS value is conservative (larger
       ;; than what the radio actually needs to TX), the radio would
       ;; TX and then try to go back in RX. With the default values,
       ;; our new SET_STATE_TX message would find the radio, in the
       ;; midst of changing back to RX. This is not a serious problem,
       ;; in our radio implementation we would go back to TX and just
       ;; print a warning trace message. But some real radios may need
       ;; more time (e.g., wait to finish the first TX->RX transision)
       ;; For this reason we are waiting for 2*pTIFS just to be on the
       ;; safe side. We do not account for the clock drift, since this
       ;; should be really small for just a packet transmission.
       (tracelog "TXing ~A, no ACK required." packet)
       (set-timer instance 'start-attempt-tx
                  (+ (tx-time instance packet) (sleep-to-tx instance)
                     (* 2 (tifs instance))));
       (setf (future-attempt-to-tx instance) t)
       (to-radio instance  packet)
       (setf packet nil (current-packet-transmissions instance) 0))))
    (to-radio instance '(set-state . tx)))

(defun handle-poll(instance pkt)
  (cond
    ((zerop (more-data pkt))
     (set-state instance 'free-tx-access)
     (setf (is-poll-period instance) t)
     (let ((end-polled-access-slot (sequence-number pkt)))
       ;; The end of the polled access time is given as the end of an
       ;; allocation slot. We have to know the start of the whole
       ;; frame to calculate it. NOTICE the difference in semantics
       ;; with other end slots such as scheduled access
       ;; scheduledTxAccessEnd where the end is the beginning of
       ;; scheduledTxAccessEnd equals with the end of
       ;; scheduledTxAccessEnd-1 slot.
       (setf (end-time instance)
             (+ (frame-start-time instance)
                (* end-polled-access-slot (allocation-slot-length instance))))
       (if (and (/= end-polled-access-slot (beacon-period-length instance))
                (/= (1+ end-polled-access-slot)
                    (scheduled-tx-access-start instance))
                (/= (1+ end-polled-access-slot)
                    (scheduled-rx-access-start instance)))
           (set-timer instance 'start-sleeping
                      (- (end-time instance) (get-clock instance)))
           (cancel-timer instance 'start-sleeping)))
     (let ((current-slot-estimate
            (1+ (round (- (get-clock instance) (frame-start-time instance))
                       (allocation-slot-length instance)))))
       (when (> (1- current-slot-estimate) (beacon-period-length instance))
         (tracelog "Warning: current slot estimate=~D" current-slot-estimate))
       (emit instance 'poll-slots-taken (- (1+ (polled-access-end instance))
                                           current-slot-estimate))
       (attempt-tx instance)))
    (t
     (let* ((post-access-start (sequence-number pkt))
            (post-time
             (+ (frame-start-time instance)
                (* (allocation-slot-length instance)
                   (+ (1- post-access-start)
                      (* (fragment-number pkt)
                         (beacon-period-length instance))))))
            (guard-time (guard-time instance))
            (now (get-clock instance)))
       (setf (posted-access-end instance) (1+ post-access-start))
       (tracelog "Future Poll received, postSlot=~D, waiking up in ~A"
                 post-access-start (- post-time guard-time now))
       (set-timer instance 'start-posted-access
                  (if (<= post-time (- now guard-time))
                      0
                      (- post-time guard-time now)))))))

(defun handle-post(instance pkt)
  (cond
    ((is-hub instance)
     (if (polling-enabled instance) (handle-more-data-at-hub instance pkt)))
    (t
     (let ((posted-access-start
            (1+ (round (- (get-clock instance) (frame-start-time instance))
                       (allocation-slot-length instance)))))
       (setf (posted-access-end instance) (1+ posted-access-start))
       (set-timer instance 'start-posted-access 0)))))

(defun handle-more-data-at-hub(instance pkt)
  	;; If the packet we received is in the node's last TX access slot
  	;; (scheduled or polled) then send a POLL. This means that we
  	;; might send multiple polls (as we may receive multiple packets
  	;; in the last slot but this is fine since all will point to the
  	;; same time. Note that a node can only support one future poll
  	;; (one timer for START_POSTED_ACCESS). Sending multiple polls
  	;; (especially with I_ACK+POLL which do not cost anything extra
  	;; compared to I_ACK) is beneficial bacause it increases the
  	;; probability of the poll's reception. Also note that
  	;; reqToSendMoreData[NID] will have the latest info (the info
  	;; carried by the last packet with moreData received. Finally the
  	;; lastTxAccessSlot[NID].polled does not need to be reset for a
  	;; new beacon period. If we send a new poll, this variable will be
  	;; updated, if we don't then we will not receive packets from that
  	;; NID in the old slot so no harm done.
  (let* ((nid (nid pkt))
         (last-access (aref (last-tx-access-slot instance) nid))
         (current-slot (current-slot instance)))
    (when (or (= current-slot (access-slot-scheduled last-access))
              (= current-slot (access-slot-polled last-access)))
      (when (<= (next-future-poll-slot instance)
                (beacon-period-length instance))
        (tracelog "HUB handles more data ~D from NID: ~D, current slot: ~D"
                  (more-data pkt) nid current-slot)
        (setf (aref (req-to-send-more-data instance) nid) (more-data pkt))
        (if (eql (ack-policy pkt) 'i-ack)
            (setf (send-i-ack-poll instance) t))
        ;; alternatives not implemented - all packets require i-ack
        ))))

(defun time-to-next-beacon(instance interval index phase)
  ;; not implemented but may be used if implement beacon shift sequences
  (declare (ignore instance index phase))
  interval)
