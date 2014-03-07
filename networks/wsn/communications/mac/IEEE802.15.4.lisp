(in-package :lens.wsn)

(defpackage :lens.wsn.mac.802.15.4
  (:use :cl :cl-user :lens :lens.wsn)
  (:export #:mac802.15.4))

(in-package :lens.wsn.mac.802.15.4)

(register-signal 'desync "Signal emitted when PAN connection lost")

(defclass mac802.15.4-packet(mac-packet)
  ((pan-id :type integer :accessor pan-id :initarg :pan-id)))

(defmethod duplicate((pkt mac802.15.4-packet) &optional duplicate)
  (call-next-method)
  (copy-slots '(pan-id) pkt duplicate))

;; different packet types

(defclass mac802.15.4-data-packet(mac802.15.4-packet)
  ((header-overhead :initform 14 :allocation :class)))

(defclass mac802.15.4-protocol-packet(mac802.15.4-packet)
  ((gts-length :type integer :accessor gts-length :initarg :gts-length)))

(defmethod duplicate((pkt mac802.15.4-protocol-packet) &optional duplicate)
  (call-next-method)
  (copy-slots '(gts-length) pkt duplicate))

(defclass mac802.15.4-associate-packet(mac802.15.4-protocol-packet)
  ((byte-length :initform 6 :type fixnum :allocation :class
                :reader byte-length)))

(defclass mac802.15.4-ack-packet(mac802.15.4-protocol-packet)
  ((byte-length :initform 8 :type fixnum :allocation :class
                :reader byte-length)))

(defclass mac802.15.4-gts-request-packet(mac802.15.4-protocol-packet)
  ((byte-length :initform 8 :type fixnum :allocation :class
                :reader byte-length)))

(defstruct gts-spec
  (owner 0 :type fixnum)
  (start 0 :type fixnum)
  (length 0 :type fixnum))

(defclass mac802.15.4-beacon-packet(mac802.15.4-protocol-packet)
  ((header-overhead :initform 12 :allocation :class)
   (beacon-order :type integer :initarg :beacon-order :accessor beacon-order)
   (frame-order :type :nteger :initarg :frame-order :accessor frame-order)
   (bsn :type integer :initarg :bsn :accessor bsn)
   (cap-length :type integer :initarg :cap-length :accessor cap-length)
   (gts-spec :type list :initarg :gts-spec :initform nil
             :accessor gts-spec)))

(defmethod byte-length((pkt mac802.15.4-beacon-packet))
  (+ (header-overhead pkt) (* (length (gts-spec pkt)) 3)))
(defmethod byte-length((pkt mac802.15.4-protocol-packet))
  (byte-length (class-name (class-of pkt))))
(defmethod byte-length((pkt (eql 'mac802.15.4-associate-packet))) 6)
(defmethod byte-length((pkt (eql 'mac802.15.4-ack-packet))) 8)
(defmethod byte-length((pkt (eql 'mac802.15.4-gts-request-packet))) 8)

(defmethod tx-time(instance (packet-type symbol))
  (tx-time instance (byte-length packet-type)))

(defmethod duplicate((pkt mac802.15.4-beacon-packet) &optional duplicate)
  (call-next-method)
  (copy-slots '(beacon-order frame-order bsn cap-length gts-spec)
              pkt duplicate))

(deftype mac802.15.4-state ()
  '(member
    setup
    sleep
    idle
    csma-ca
    cca
    in-tx
    wait-for-associate-response
    wait-for-data-ack
    wait-for-beacon
    wait-for-gts
    processing))

(defclass mac802.15.4(mac)
  ((print-state-transitions
    :parameter t :type boolean :initform nil
    :documentation "Debugging parameter")

   ;; common mac layer parameters
   (max-mac-frame-size :parameter t :initform 0)
   (header-overhead :parameter t :initform 14) ;; data-packet-size
   (buffer-size :parameter t :initform 32)

   ;; mac specific parameters
   (enable-slotted-csma :parameter t :type boolean :initform t)
   (enable-cap :parameter t :type boolean :initform t)
   (is-ffd :parameter t :type boolean :initform nil)
   (is-pan-coordinator :parameter t :type boolean :initform nil)
   (battery-life-extension :parameter t :type boolean :initform nil)

   (frame-order :parameter t :type fixnum :initform 4)
   (beacon-order :parameter t :type fixnum :initform 6)
   (unit-backoff-period :parameter t :type fixnum :initform 20) ;;in symbols
   (base-slot-duration :parameter t :type fixnum :initform 60)
   (base-superframe-duration :type fixnum)

   (num-superframe-slots :parameter t :type fixnum :initform 16)
   (min-be :parameter t :type fixnum :initform 5)
   (max-be :parameter t :type fixnum :initform 7)
   (max-csma-backoffs :parameter t :type fixnum :initform 4)
   (max-frame-retries :parameter t :type fixnum :initform 2)
   (max-lost-beacons :parameter t :type fixnum :initform 4)
   (min-cap-length :parameter t :type fixnum :initform 440)
   (request-gts :parameter t :type fixnum :initform 0)

   ;; reception guard time
   (guard-time :parameter t :type time-type :initform 1e-3)

   ;; general variables
   (next-packet-try  :type integer)
   (next-packet-time :type time-type)
   (locked-gts :type boolean :initform nil)

   ;; state variables
   (associated-pan
    :type fixnum :initform -1
    :documentation "ID of current PAN (-1 if not associated)")
   (state :type mac802.15.4-state :initform 'setup :reader state)
   (next-state
    :type mac802.15.4-state
    :documentation "will be switched after csma-ca algorithm")
   (cap-length
    :type fixnum
    :documentation "duration of CAP interval (in number of superframe slots)")
   (mac-bsn
    :type integer :initform (random 255)
    :documentation "beacon sequence number (unused)")
   (next-packet-retries
    :type fixnum :initform 0
    :documentation "number of retries left for next packet to be sent")
   (lost-beacons
    :type fixnum :initform 0
    :documentation "number of consequitive lost beacon packets")
   (frame-interval
    :type fixnum
    :documentation "duration of active part of the frame (in symbols)")
   (beacon-interval
    :type integer
    :documentation "duration of the whole frame (in symbols)")
   (csma-number-backoffs :type fixnum)
   (csma-contention-window-length :type fixnum)
   (csma-backoff-exponent :type fixnum)

   (cap-end
    :type time-type :initform 0d0
    :documentation "Absolute time of end of CAP period for current frame")
   (current-frame-start
    :type time-type :initform 0d0
    :documentation "Absolute recorded start time of the current frame")
   (gts-start  :type time-type :initform 0d0)
   (gts-end :type time-type :initform 0d0)
   (gts-length :type time-type :initform 0d0)
   (gts-list :type list :initform nil)

   (next-packet-response
    :type time-type :initform 0d0
    :documentation "Duration of timeout for receiving a reply after sending a packet")
   (next-packet-state :type list :initform nil)
   (associated-devices
    :type list :initform nil
    :documentation "List of associated devices for PAN coordinator")

   ;; packet references  (sometimes packet is created not immediately before sending)
   (beacon-packet :type mac802.15.4-packet :initform nil)
   (associate-request-packet :type mac802.15.4-packet :initform nil)
   (next-packet :type mac802.15.4-data-packet :initform nil)

   (gts-spec :type vector :initform (make-array 0 :adjustable t)
             :documentation "list of GTS specifications (for PAN coordinator)")
   ;; cached timers
   (attempt-tx :initform (make-instance 'timer-message)))
  (:properties
   :statistic (desync
               :title "Fraction of time without PAN connection"
               :default ((accumulated-time :fractional t :initial-state t
                                           :format "~,8f"))))
  (:documentation "IEEE802.15.4 MAC implementation")
  (:metaclass module-class))

(defun ack-wait-duration(instance)
  (+ (* (symbol-length (radio instance))
        (slot-value instance 'unit-backoff-period))
     (* 2 (transition-delay (radio instance) 'rx 'tx))
     (tx-time instance 'mac802.15.4-ack-packet)))

(defmethod startup((instance mac802.15.4))
  ;; start listening and schedule a time to sleeep if needed
  (assert (or (not (slot-value instance 'is-pan-coordinator))
              (slot-value instance 'is-ffd))
          ()
          "Only full-function devices (isFFD=true) can be PAN coordinators")
  (with-slots(base-superframe-duration base-slot-duration num-superframe-slots
              beacon-interval frame-interval frame-order beacon-order
              cap-length) instance
    (assert (<= 0 frame-order beacon-order 14)
            ()
            "Invalid combination of frame-order and beacon-order parameters.")
    (setf base-superframe-duration
          (* base-slot-duration num-superframe-slots))
    (setf beacon-interval (*  base-superframe-duration (expt 2 beacon-order))
          frame-interval (*  base-superframe-duration (expt 2 frame-order))
          cap-length num-superframe-slots)
    (assert (and (> beacon-interval 0) (> frame-interval 0))
            ()
            "Invalid parameter combination of baseSlotDuration and numSuperframeSlots"))
  (reinitialise-slots
   '(beacon-packet associate-request-packet next-packet next-packet-response
     next-packet-retries state mac-bsn associated-pan current-frame-start
     gts-start gts-end cap-end lost-beacons next-packet-state)
   instance)
  (with-slots(is-ffd is-pan-coordinator associated-pan) instance
    (when (and is-ffd is-pan-coordinator)
      (setf associated-pan (mac-address instance))
      (set-timer instance 'frame-start 0d0)))) ;; frame start is now

(defmethod handle-timer((instance mac802.15.4) (timer (eql 'frame-start)))
  (cond
    ((slot-value instance 'is-pan-coordinator)
     (with-slots(mac-bsn beacon-order frame-order cap-length cap-end
                 base-slot-duration current-frame-start guard-time
                 num-superframe-slots gts-list beacon-interval) instance
       (setf cap-length num-superframe-slots)
       (dolist(gts gts-list)
         (cond
           ((> cap-length (gts-spec-length gts))
            (decf cap-length (gts-spec-length gts))
            (setf (gts-spec-start gts) (1+ cap-length)))
           (t
            (setf (gts-spec-length gts) 0)
            (tracelog  "Internal ERROR: GTS list corrupted"))))
       (setf cap-end (* cap-length base-slot-duration (expt 2 frame-order)
                        (symbol-length (radio instance))))
       (let ((beacon-packet
              (make-instance
               'mac802.15.4-beacon-packet
               :source (mac-address instance)
               :destination broadcast-mac-address
               :pan-id (mac-address instance)
               :beacon-order beacon-order
               :frame-order frame-order
               :bsn (setf mac-bsn (mod (1+ mac-bsn) 255))
               :gts-spec (mapcar #'copy-gts-spec gts-list)
               :cap-length cap-length)))
         (emit instance 'mac-packet-breakdown "sent beacons")
         (set-state instance 'in-tx)
         (to-radio instance beacon-packet)
         (to-radio instance '(set-state . tx))
         (set-timer instance 'attempt-tx
                    (tx-time instance beacon-packet))
         (set-timer instance 'frame-start
                    (* beacon-interval (symbol-length (radio instance))))
         (setf current-frame-start
               (+ (get-clock instance)
                  (transition-delay (radio instance) 'rx 'tx))))))
    (t ;; not PAN coordinator so wait for beacon
     (to-radio instance '(set-state . tx))
     (set-state instance 'wait-for-beacon)
     (set-timer instance 'beacon-timeout
                (* 3 (slot-value instance 'guard-time))))))


(defmethod handle-timer((instance mac802.15.4) (timer (eql 'beacon-timeout)))
  (with-slots(lost-beacons max-lost-beacons associated-pan locked-gts
              beacon-interval guard-time) instance
    (emit instance 'mac-packet-breakdown "lost beacons")
    (cond
      ((>= (incf lost-beacons) max-lost-beacons)
       (tracelog "Lost synchronisation with PAN ~A" associated-pan)
       (set-state instance 'setup)
       (setf associated-pan -1
             locked-gts nil)
       (emit instance 'desync t))
      ((/= associated-pan -1)
       (let ((delay (- (* beacon-interval (symbol-length (radio instance)))
                       (* 3 guard-time))))
         (tracelog "Missed beacon from PAN ~A, will wake up to receive next beacon in ~:/dfv:eng/s" associated-pan delay)
         (set-state instance 'sleep)
         (to-radio  instance '(set-state . sleep))
         (set-timer instance 'frame-start delay))))))

(defmethod handle-timer((instance mac802.15.4) (timer (eql 'gts-start)))
  (unless (member (state instance)
                  '(wait-for-data-ack wait-for-asociate-response 'processing))
    (to-radio instance '(set-state . rx))
		;; we delay transmission attempt by the time requred by radio to wake up
		;; note that GTS_START timer was scheduled exactly phyDelaySleep2Tx seconds
		;; earlier than the actual start time of GTS slot
    (set-state instance 'processing)
    (let ((sleep2tx (transition-delay (radio instance) 'sleep 'tx)))
    (set-timer instance 'attempt_tx  sleep2tx)
    ;; set a timer to go to sleep after our GTS slot ends
    (set-Timer instance 'start_sleeping
               (+ sleep2tx (slot-value instance 'gts-length))))))

(defmethod handle-timer((instance mac802.15.4) (timer (eql 'attempt-tx)))
  (unless (member (state instance)
                  '(in-tx wait-for-data-ack wait-for-associate-response
                    wait-for-gts processing))
    (tracelog "WARNING ATTEMPT_TX timer was not cancelled, macState is ~A"
              (state instance))
    (return-from handle-timer))
  (when (eql (state instance) 'wait-for-data-ack)
    (collect-packet-state instance :noack))
  (attempt-tx instance "Attempt-tx timer"))

(defmethod handle-timer((instance mac802.15.4) (timer (eql 'perform-cca)))
  (unless (eql (state instance) 'csma-ca)
    (return-from handle-timer))
  (with-slots(csma-number-backoffs csma-contention-window-length
              csma-backoff-exponent next-packet unit-backoff-period
              max-be max-csma-backoffs next-packet-retries)
      instance
    (ecase (channel-clear-status (radio instance))
      (clear
       (decf csma-contention-window-length)
       (cond
         ((/= csma-contention-window-length 0)
          (set-timer instance 'perform-cca
                     (* unit-backoff-period (symbol-length instance))))
         (next-packet
          ;;CSMA-CA successful (CW == 0), can transmit the queued packet
          (transmit-next-packet instance))
         (t
          (tracelog "ERROR: CSMA_CA algorithm executed without any data to transmit")
          (attempt-tx instance "ERROR in CSMA_CA"))))
      (busy
       (setf csma-contention-window-length
             (if (slot-value instance 'enable-slotted-csma) 2 1))
       (incf csma-number-backoffs)
       (setf csma-backoff-exponent (min (1+ csma-backoff-exponent) max-be))
       (cond
         ((> csma-number-backoffs max-csma-backoffs)
          (collect-packet-state instance :csfail)
          (decf next-packet-retries)
          (attempt-tx instance "NB exeeded maxCSMAbackoffs"))
         (t
          (set-state instance 'csma-ca)
          (continue-csma-ca instance))))
      (not-valid-yet
       (set-timer instance 'perform-cca
                  (slot-value instance 'phy-delay-for-valid-cs))))))

(defmethod handle-timer((instance mac802.15.4) (timer (eql 'start-sleeping)))
  (set-state instance 'sleep)
  (to-radio instance '(set-state . sleep)))

(defmethod handle-message((instance mac802.15.4) (packet routing-packet))
  ;; from routing layer
  (when
      (enqueue
       (encapsulate
        (make-instance
         'mac802.15.4-data-packet
         :source (mac-address instance)
         :destination (next-hop (control-info packet)))
        packet)
       instance)
    (when (eql (state instance)'idle)
      (attempt-tx instance  "New packet from network layer"))))

(defmethod set-state :before ((instance mac802.15.4) new-state
                                 &optional description)
  (declare (ignore description))
  (check-type new-state mac802.15.4-state))

(defmethod handle-message((instance mac802.15.4)
                          (pkt mac802.15.4-beacon-packet))
  (let ((pan-addr (pan-id pkt))
        (symbol-length (symbol-length (radio instance)))
        (get-clock (get-clock instance)))
    (with-slots(associated-pan next-packet next-packet-state) instance
      (emit instance 'mac-packet-breakdown "recv beacons")
      (cond
        ((< associated-pan 0)
       ;;if not associated - create an association request packet
         (when next-packet
           (emit-packet-state instance :nopan))
         (setf next-packet
               (make-instance  'mac802.15.4-associate-packet
                               :destination pan-addr
                               :pan-id pan-addr
                               :source (mac-address instance)))
         (initiate-csma-ca
          instance 9999 'wait-for-associate-response
          (+ (ack-wait-duration instance) (tx-time instance next-packet))))
        ((not (eql associated-pan pan-addr))
         (return-from handle-message))))
    (with-slots(current-frame-start lost-beacons frame-order beacon-order
                beacon-interval mac-bsn cap-length cap-end
                base-slot-duration base-superframe-duration
                gts-start gts-end gts-length locked-gts) instance
      (setf current-frame-start (- get-clock (tx-time instance pkt))
            lost-beacons 0
            frame-order (frame-order pkt)
            beacon-order (beacon-order pkt)
            beacon-interval (* base-superframe-duration (expt 2 beacon-order))
            mac-bsn (bsn pkt)
            cap-length (cap-length pkt)
            cap-end (* cap-length base-slot-duration (expt 2 frame-order)
                       symbol-length)
            gts-start 0d0
            gts-end 0d0
            gts-length 0d0)
      (dolist(gts (gts-spec pkt))
        (when (and locked-gts (eql (gts-spec-owner gts) (mac-address instance)))
          (let ((s (* base-slot-duration
                      (expt 2 frame-order) symbol-length)))
          (setf gts-start (* (1- (gts-spec-start gts)) s))
          (setf gts-end (+ gts-start (* (gts-spec-length gts) s)))
          (setf gts-length (- gts-end gts-start))
          (tracelog "GTS slot from ~A to ~A"
                    (+ get-clock gts-start)
                    (+ get-clock gts-end))))))
    (cancel-timer instance 'beacon-timeout)
    (with-slots(request-gts locked-gts gts-start associated-pan
                enable-cap cap-end gts-length) instance
      (unless (zerop request-gts)
        (cond
          (locked-gts
           (when (zerop gts-start)
             (tracelog  "invalid state, GTS descriptor not found in beacon frame")
             (setf locked-gts nil)))
          ((eql associated-pan pan-addr)
           (issue-gts-request instance))))
      (when (eql associated-pan pan-addr)
        (cond
          (enable-cap
           (attempt-tx instance "CAP started")
           ;;set timer to start sleeping
           (set-timer instance 'start-sleeping
                      (+ cap-end (if (eql gts-start cap-end) gts-length 0))))
          (t
           (set-state instance 'sleep)
           (to-radio instance '(set-state . sleep))))
        (when (and (not (zerop gts-start))
                   (not (and enable-cap (eql gts-start cap-end))))
          ;; if GTS slot exists and does not start after CAP (or CAP
          ;; is disabled) then we set GTS timer phyDelaySleep2Tx
          ;; seconds earlier as radio will be sleeping
          (set-timer instance 'gts-start
                     (- gts-start
                        (transition-delay (radio instance) 'sleep 'tx)))))
      (with-slots(base-superframe-duration beacon-order guard-time)
          instance
        (set-timer instance 'frame-start
                   (- (* base-superframe-duration (expt 2 beacon-order)
                         symbol-length)
                      guard-time
                      (tx-time instance pkt)))))))

(defmethod handle-message((instance mac802.15.4)
                          (pkt mac802.15.4-associate-packet))
  ;; only PAN coordinators can accept association requests
	;;if multihop communication is to be allowed - then this has to be changed
	;; in particular, any FFD can become a coordinator and accept requests
  (unless (and (slot-value instance 'is-pan-coordinator)
               (eql (pan-id pkt) (mac-address instance)))
    (return-from handle-message))
  ;; update associatedDevices and reply with an ACK (i.e. association
  ;; is always allowed)
  (tracelog "Received association request from ~A" (source pkt))
  (pushnew (source pkt) (slot-value instance 'associated-devices))
  (let ((ack-packet
         (make-instance
          'mac802.15.4-ack-packet
          :pan-id (mac-address instance)
          :destination (source pkt))))
    (to-radio instance ack-packet)
    (to-radio instance '(set-state . tx))
    (set-state instance 'in-tx)
    (set-timer instance 'attempt-tx (tx-time instance ack-packet))))

(defmethod handle-message((instance mac802.15.4)
                          (pkt mac802.15.4-gts-request-packet))
    (unless (and (slot-value instance 'is-pan-coordinator)
               (eql (pan-id pkt) (mac-address instance)))
    (return-from handle-message))
    (tracelog "Received GTS request from ~A" (source pkt))
    (let ((ack-packet
           (make-instance
            'mac802.15.4-ack-packet
            :pan-id (mac-address instance)
            :destination (source pkt)
            :gts-length 0))
          (index -1))
      (with-slots(gts-list cap-length base-slot-duration frame-order
                  min-cap-length) instance
        (dotimes(i (length gts-list))
          (let((gts (aref gts-list i)))
            (when (eql (gts-spec-owner gts) (source pkt))
              (if (= (gts-spec-length gts) (length (gts-spec pkt)))
                  (setf (gts-length ack-packet) (gts-spec-length gts))
                  (progn
                  (incf cap-length (gts-spec-length gts))
                  (setf (gts-spec-length gts) 0)
                  (setf index i))))))
        (when (zerop (gts-length ack-packet))
          (cond
            ((< (* (- cap-length (gts-length pkt)) base-slot-duration
                   (expt 2 frame-order))
                min-cap-length)
             (tracelog "GTS request from ~A cannot be accomodated."
                       (source pkt)))
            ((>= index 0)
             (setf (gts-spec-length (aref gts-list index)) (gts-length pkt))
             (setf (gts-length ack-packet) (gts-length pkt)))
            (t
             (setf gts-list
                   (nconc gts-list
                          (list
                           (make-gts-spec :owner (source pkt)
                                          :length (gts-length pkt))))))))
        (to-radio instance ack-packet)
        (to-radio instance '(set-state . tx))
        (set-state instance 'in-tx)
        (set-timer instance 'attempt-tx (tx-time instance ack-packet)))))

(defmethod handle-message((instance mac802.15.4)
                          (pkt mac802.15.4-data-packet))
  (let ((dst-addr (destination pkt)))
    (unless (or (eql dst-addr broadcast-mac-address)
                (eql dst-addr (mac-address instance)))
      (return-from handle-message))
    (to-network instance (decapsulate pkt))
    (unless (eql dst-addr (mac-address instance))
      (return-from handle-message)))
  (let ((ack-packet
          (make-instance
           'mac802.15.4-ack-packet
           :pan-id (mac-address instance)
           :source (mac-address instance)
           :destination (source pkt))))
    (to-radio instance ack-packet)
    (to-radio instance '(set-state . tx))
    (set-state instance 'in-tx)
    (set-timer instance 'attempt-tx (tx-time instance ack-packet))))

(defmethod handle-message((instance mac802.15.4)
                          (pkt mac802.15.4-ack-packet))
  (unless (eql (destination pkt) (mac-address instance))
    (return-from handle-message))
  (case (state instance)
    (wait-for-associate-response
     (with-slots(associated-pan next-packet request-gts) instance
       (setf associated-pan (pan-id pkt))
       (emit instance 'desync nil)
       (tracelog "associated with PAN ~A" associated-pan)
       (cancel next-packet)
       (setf next-packet nil)
       (if (zerop request-gts)
           (attempt-tx instance "Associated with PAN")
           (issue-gts-request instance) )))
    (wait-for-data-ack
     (with-slots(is-pan-coordinator associated-pan next-packet-retries) instance
       (when (or is-pan-coordinator (eql associated-pan (source pkt)))
         (collect-packet-state instance :success)
         (tracelog "Packet successfully transmitted to ~A" (source pkt))
         (setf next-packet-retries 0)
         (attempt-tx instance "ACK received"))))
    (wait-for-gts
     (with-slots(locked-gts next-packet enable-cap) instance
       (setf locked-gts t)
       (cancel next-packet)
       (setf next-packet nil)
       (if enable-cap
           (attempt-tx instance "GTS request granted")
           (progn
             (set-state instance 'sleep)
             (to-radio instance '(set-state . sleep))))))
    (t (tracelog "Warning: received unexpected ACK in state ~A"
                 (state instance)))))

(defmethod attempt-tx((instance mac802.15.4) &optional description)
  "This function will initiate a transmission (or retransmission)
attempt after a given delay"
  (cancel-timer instance 'attempt-tx)
  (tracelog "Attempt transmission: ~A" description)
  (let((get-clock (get-clock instance)))
    (with-slots(enable-cap cap-end current-frame-start gts-start gts-end
                           request-gts) instance
      (cond
        ((> (+ current-frame-start cap-end) get-clock)
                                        ; still in CAP period of the frame
         (unless enable-cap
           (set-state instance 'idle)
           (return-from attempt-tx)))
        ((or (zerop request-gts) (zerop gts-start))
         ;; not in CAP period and not in GTS - no transmission possible
         (set-state instance 'idle)
         (return-from attempt-tx))
        ((or (> (+ current-frame-start gts-start) get-clock)
             (< (+ current-frame-start gts-end) get-clock))
         ;; outside GTS - no transmissions possible
         (set-state instance 'idle)
         (return-from attempt-tx)))))
  (with-slots(next-packet next-packet-state associated-pan
              next-packet-retries tx-addr max-frame-retries) instance
    ;; if packet already queued check available retries
    (when next-packet
      (cond
        ((<= next-packet-retries 0)
         (when (typep next-packet 'mac802.15.4-data-packet)
           (if (eql (destination next-packet) broadcast-mac-address)
               (progn
                 (emit instance 'mac-packet-breakdown "Broadcast")
                 (cancel next-packet)
                 (setf next-packet nil))
               (emit-packet-state instance nil))))
        (t
         (tracelog "Continuing transmission of ~A, ~D retries left"
                   next-packet next-packet-retries)
         (initiate-csma-ca instance)
         (return-from attempt-tx))))
    (when (eql associated-pan -1)
      ;; no pan cannot initate transmissions other than association requests
      (return-from attempt-tx))
    (unless (empty-p (buffer instance))
      (setf next-packet (dequeue (buffer instance)))
      (setf next-packet-state nil)
      (if (eql (destination next-packet) broadcast-mac-address)
          (initiate-csma-ca instance 0 'in-tx (tx-time instance next-packet))
          (initiate-csma-ca
           instance
           max-frame-retries
           'wait-for-data-ack
           (+ (ack-wait-duration instance) (tx-time instance next-packet))))
      (return-from attempt-tx))
    (set-state instance 'idle)))

(defun initiate-csma-ca(instance &optional retries next-state response)
  (with-slots(next-packet next-packet-retries next-packet-state
              next-packet-response) instance
    (when retries
      (tracelog "Initiating new transmission of ~A, ~D retries left"
                next-packet retries)
      (setf next-packet-retries retries
            (slot-value instance 'next-state) next-state
            next-packet-response response)))
  (with-slots(request-gts locked-gts current-frame-start gts-start) instance
    (let ((get-clock (get-clock instance)))
      (cond
        ((and (not (zerop request-gts))
              locked-gts
              (< (+ current-frame-start gts-start) get-clock))
         ;; in GTS so transmit immediately - no need for csma-ca
         (tracelog "Transmitting packet in GTS")
         (transmit-next-packet instance))
        ((eql (state instance) 'csma-ca)
         (tracelog  "WARNING: cannot initiate CSMA-CA algorithm while in CSMA-CA state"))
        (t
         (set-state instance 'csma-ca)
         (with-slots(csma-number-backoffs csma-contention-window-length
                     csma-backoff-exponent enable-slotted-csma
                     battery-life-extension min-be) instance
           (setf csma-number-backoffs 0
                 csma-contention-window-length (if  enable-slotted-csma 2 1)
                 csma-backoff-exponent (if battery-life-extension
                                           (min min-be 2)
                                           min-be))
           (continue-csma-ca instance)))))))

(defmethod symbol-length((instance mac802.15.4))
  (symbol-length (radio instance)))

(defun continue-csma-ca(instance)
  (unless (eql (state instance) 'csma-ca)
    (tracelog "WARNING: continue-csma-ca called when not in CSMA-CA state")
    (return-from continue-csma-ca))
  (with-slots(csma-backoff-exponent unit-backoff-period enable-slotted-csma
              current-frame-start) instance
    (let* ((base-period (*  unit-backoff-period (symbol-length instance)))
           (rnd (intuniform 1 (expt 2 csma-backoff-exponent) 1))
           (cca-time (* rnd base-period)))
      (when enable-slotted-csma
        (multiple-value-bind(quotient backoff-boundary)
            (ceiling (- (get-clock instance) current-frame-start)  base-period)
          (declare (ignore quotient))
          (incf cca-time (- backoff-boundary))))
      (tracelog "Random backoff value: ~A, in ~:/dfv:eng/s"
                rnd cca-time)
      (set-timer instance 'perform-cca cca-time))))

(defun transmit-next-packet(instance)
  (with-slots(next-packet current-frame-start cap-end request-gts gts-start
             gts-end enable-cap next-packet-retries next-packet-response
             next-state)
      instance
    (let ((tx-time (tx-time instance next-packet))
          (get-clock (get-clock instance)))
      (unless
          (cond
            ((> (+ current-frame-start cap-end) (+ get-clock tx-time))
             ;; still in CAP period of frame
             (not (and (not enable-cap) (typep next-packet 'mac802.15.4-data-packet))))
            ((or (zerop request-gts) (zerop gts-start))
             ;; not in CAP period and not in GTS - no transmissions possible
             nil)
            ((or (> (+ current-frame-start gts-start) get-clock)
                 (< (+ current-frame-start gts-end) (+ get-clock tx-time)))
             ;; outside GTS - no transmission possible
             nil))
        (set-state instance 'idle)
        (return-from transmit-next-packet))
      (decf next-packet-retries)
      (multiple-value-bind(next-state delay)
          (if (> next-packet-response 0)
              (values next-state next-packet-response)
              (values 'in-tx tx-time))
        (set-state instance next-state)
        (set-timer instance 'attempt-tx delay)))
    (to-radio instance (duplicate next-packet))
    (to-radio instance '(set-state . tx))))

(defun issue-gts-request(instance)
  (with-slots(next-packet next-packet-state request-gts associated-pan)
      instance
    (when next-packet
      (emit-packet-state instance  :nopan))
    (setf next-packet
          (make-instance
           'mac802.15.4-gts-request-packet
           :pan-id associated-pan
           :destination associated-pan
           :source (mac-address instance)
           :gts-length request-gts))
    (initiate-csma-ca instance 9999 'wait-for-gts
                      (+ (ack-wait-duration instance)
                         (tx-time instance next-packet)))))

(defun collect-packet-state(instance msg)
  (with-slots(next-packet next-packet-state) instance
    (assert  next-packet()
            "MAC 802.15.4 internal error: collect-packet-state called while next-packet pointer is NULL")
    (when (and (typep next-packet 'mac802.15.4-data-packet)
               (not (eql (destination next-packet) broadcast-mac-address)))
      (setf next-packet-state (nconc next-packet-state (list msg)))
      t)))

(defun emit-packet-state(instance msg)
  (with-slots(next-packet (state next-packet-state)) instance
    (when (collect-packet-state instance msg)
      (cond
        ((eql (first state) :success)
         (emit instance 'mac-packet-breakdown "Success, first try"))
        ((eql (first state) :broadcast)
         (emit instance 'mac-packet-breakdown "Broadcast"))
        ((find :success state)
         (emit instance 'mac-packet-breakdown "Success, not first try"))
        ((find :noack state)
         (emit instance 'mac-packet-breakdown "Failed, no ack"))
        ((find :csfail state)
         (emit instance 'mac-packet-breakdown "Failed, busy channel"))
        ((find :nopan state)
         (emit instance 'mac-packet-breakdown "Failed, no PAN"))
        (t
         (tracelog "Unknown breakdown category: ~A" state)))
      (cancel next-packet)
      (setf state nil next-packet nil))))
