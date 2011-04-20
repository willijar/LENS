;; TCP base implementation
;; Copyright (C) 2011 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;; This file is part of Lisp Educational Network Simulator (LENS)

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:

;; This provides the basic TCP protocol interface and common implementation
;; specific varieties need to implement newack dupack retx-timer-event
;; The same class is used for listener and responder - the listener copies
;; itself on a new connection filling in the peer information etc

;; See Tanenbaum "Computer Networks" (TCN) section 6.5.
;;; Code:

(defpackage :protocol.layer4.tcp
  (:documentation "package for TCP class implementations")
  (:nicknames :tcp)
  (:use :cl :common :protocol.layer4 :address :protocol.layer4.rtt)
  (:import-from :packet
                #:packet #:pop-pdu #:push-pdu #:peek-pdu #:trace-format)
  (:import-from :alg
                #:enqueue #:dequeue #:make-queue #:empty-p)
  (:import-from :node
                #:node #:interfaces #:find-interface)
  (:import-from :scheduler
                #:time-type #:cancel #:timer #:schedule #:timers
                #:simulation-time #:timeout)
  (:export #:tcp-header #:tcp-dmux #:tcp #:tcp-tahoe #:tcp-reno #:tcp-newreno))

(in-package :protocol.layer4.tcp)

(defclass tcp-dmux(protocol-dmux)
  ()
  (:documentation "Protocol demultiplexer for received TCP packets"))

(register-protocol 'tcp-dmux 6)

(defenumeration (tcp-flags (unsigned-byte 8))
    ((fin #x01)
     (syn #x02)
     (rst #x04)
     (psh #x08)
     (ack #x10)
     (urg #x20)
     (max-flag #x40)))

(declaim (inline flag-set-p))
(defun flag-set-p(flag datum)
  (not (zerop (logand flag datum))))

(defvar *tcp-flags-format* :text "Format for logging TCP flags")

(defun cl-user::tcp-flags(os flags &optional colon-p at-p
                 d padchar exponentchar)
  "Formatter for TCP flags"
  (declare (ignore colon-p at-p d padchar exponentchar))
  (cond
    ((stringp  *tcp-flags-format*)
     (format os *tcp-flags-format* flags))
    ((eql *tcp-flags-format* :text)
     (let ((first t))
       (dolist(flag tcp-flags)
         (when (flag-set-p (symbol-value flag) flags)
           (write-string (string flag) os))
         (if first (setf first nil) (write-char #\| os)))
       (when first (write 0 :stream os))))
    (t (format os "~2,'0X" flags))))

(defclass tcp-header(pdu)
  ((src-port :type (unsigned-byte 16) :initarg :src-port :reader src-port)
   (dst-port :type (unsigned-byte 16) :initarg :dst-port :reader dst-port)
   (sequence-number :type (unsigned-byte 32)
                    :initarg :sequence-number :reader sequence-number)
   (ack-number :type (unsigned-byte 32)
               :initarg :ack-number :initform 0 :reader ack-number)
   (header-length :type (unsigned-byte 8) :initarg :header-length :initform 0
                  :reader header-length)
   (flags  :type (unsigned-byte 8) :initarg :flags :initform 0 :reader flags)
   (window :type (unsigned-byte 16) :initarg :window :initform 0 :reader window)
   (checksum  :type (unsigned-byte 16) :initarg :checksum :accessor checksum)
   (urgent-pointer  :type (unsigned-byte 16) :initarg :urgent-pointer
                    :initform 0 :reader urgent-pointer)
   (data-length :initarg :data-length :initform 0 :type integer
                :reader data-length
                :documentation "Also not part of TCP but used for tracing")
   (cwnd :initarg :cwnd :initform 0 :type integer :reader cwnd
         :documentation
         "Not part of TCP header but used for simulation analysis"))
  (:documentation "TCP PDU class"))

(defmethod name ((h tcp-header)) "TCP")

(defmethod length-bytes((h tcp-header)) 20)

(defmethod trace-format((h tcp-header))
  '(src-port dst-port sequence-number ack-number header-length
    (flags "~/tcp-flags/") window (checksum "~4,'0X")
    urgent-pointer))

(defmethod header-fields((h tcp-header))
  '(src-port dst-port sequence-number ack-number header-length
    flags window checksum urgent fid data-length cwnd))

;; Debug history log
#+debug(defenumeration db_events (NoEvent Ind App Tout Pkt))
#+debug(defstruct db-hist (event time state action seq ack dLth))

;; logging detail types
(defenumeration tcp-time-seq-selector
    (log-seq-tx log-ack-tx log-seq-rx log-ack-rx log-cwin
                log-ssthresh log-dupack log-last))

;; define TCP state machine

;; states are integers indexing first index in the state table
;; events are integers indexing second index in state table
;; state table returns a new state and an action - a function
;; which takes the event, the tcp protocol entity,
;; and optionally a received packet

(defenumeration (tcp-states (unsigned-byte 4))
    (closed ;; No connection is active or pending
     listen ;; The server is waiting for an incoming call
     syn-sent ;; Application has started to open a connection
     syn-rcvd ;; A connection request has arrived, wait for ACK
     established ;; normal data transfer state
     close-wait ;; The other side has initiated a release
     last-ack ;; Wait for all packets to die off
     fin-wait-1 ;; Application has said it is finished
     fin-wait-2 ;; Other side has agreed to repease
     closing ;; both sides have tried to close simultaneously
     timed-wait ;; wait for all packets to die off
     last-state))

(defenumeration (tcp-events (unsigned-byte 4))
    (app-listen app-connect app-send seq-recv app-close timeout ack-rx
     syn-rx syn-ack-rx fin-rx fin-ack-rx rst-rx bad-flags last-event))

(defenumeration (tcp-actions (unsigned-byte 5))
    (no-act ack-tx ack-tx-1 rst-tx syn-tx syn-ack-tx fin-tx fin-ack-tx
     new-ack new-seq-rx retx tx-data peer-close app-closed cancel-tm
     app-notify serv-notify last-action))

;; fill in tcp state table
(eval-when(:compile-toplevel :load-toplevel :execute)
(defparameter *state-table*
  (make-array (list last-state last-event) :element-type 'cons)
  "The TCP state table")
  (map
     'nil
     #'(lambda(state-row)
         (let ((state (symbol-value (first state-row))))
           (map
            nil
            #'(lambda(event-row)
                (let ((event (symbol-value (first event-row)))
                      (new-state (symbol-value (second event-row)))
                      (action  (symbol-value (third event-row))))
                  (setf (aref *state-table* state event)
                        (list new-state action))))
            (rest state-row))))
 ;; a list - car is current state, rest is event list - each row
 ;; is the event, the new state and the action
 '((closed   ;; closed state
    (app-listen listen no-act)
    (app-connect syn-sent syn-tx)
    (app-send closed rst-tx)
    (seq-recv closed no-act)
    (app-close closed no-act)
    (timeout closed rst-tx)
    (ack-rx closed rst-tx)
    (syn-rx closed rst-tx)
    (syn-ack-rx closed rst-tx)
    (fin-rx closed rst-tx)
    (fin-ack-rx closed rst-tx)
    (rst-rx closed cancel-tm)
    (bad-flags closed rst-tx))
   (listen
    ;; listen state
    ;; for the listen state anything other than connect or send
    ;; is simply ignored....this likely indicates the child tcp
    ;; has finished and issued unbind call but the remote end
    ;; has not yet closed.
    (app-listen listen no-act)
    (app-connect syn-sent syn-tx)
    (app-send syn-sent syn-tx)
    (seq-recv listen no-act)
    (app-close closed no-act)
    (timeout listen no-act)
    (ack-rx listen no-act)
    (syn-rx listen syn-ack-tx)
    (syn-ack-rx listen no-act)
    (fin-rx listen no-act)
    (fin-ack-rx listen no-act)
    (rst-rx listen no-act)
    (bad-flags listen no-act))

   (syn-sent ;; syn sent state
    (app-listen closed rst-tx)
    (app-connect syn-sent syn-tx)
    (app-send syn-sent no-act)
    (seq-recv established new-seq-rx)
    (app-close closed rst-tx)
    (timeout closed no-act)
    (ack-rx syn-sent no-act)
    (syn-rx syn-rcvd syn-ack-tx)
    (syn-ack-rx established ack-tx-1)
    (fin-rx closed rst-tx)
    (fin-ack-rx closed rst-tx)
    (rst-rx closed app-notify)
    (bad-flags closed rst-tx))

   (syn-rcvd ;; syn recvd state
    (app-listen closed rst-tx)
    (app-connect closed rst-tx)
    (app-send closed rst-tx)
    (seq-recv established new-seq-rx)
    (app-close fin-wait-1 fin-tx)
    (timeout closed rst-tx)
    (ack-rx established serv-notify)
    (syn-rx syn-rcvd syn-ack-tx)
    (syn-ack-rx closed rst-tx)
    (fin-rx closed rst-tx)
    (fin-ack-rx close-wait peer-close)
    (rst-rx closed cancel-tm)
    (bad-flags closed rst-tx))

   (established ;; established state
    (app-listen closed rst-tx)
    (app-connect closed rst-tx)
    (app-send established tx-data)
    (seq-recv established new-seq-rx)
    (app-close fin-wait-1 fin-tx)
    (timeout established retx)
    (ack-rx established new-ack)
    (syn-rx syn-rcvd syn-ack-tx)
    (syn-ack-rx established no-act)
    (fin-rx close-wait peer-close)
    (fin-ack-rx close-wait peer-close)
    (rst-rx closed cancel-tm)
    (bad-flags closed rst-tx))

   (close-wait ;; close wait state
    (app-listen closed rst-tx)
    (app-connect syn-sent syn-tx)
    (app-send close-wait tx-data)
    (seq-recv close-wait new-seq-rx)
    (app-close last-ack fin-ack-tx)
    (timeout close-wait no-act)
    (ack-rx close-wait no-act)
    (syn-rx closed rst-tx)
    (syn-ack-rx closed rst-tx)
    (fin-rx close-wait ack-tx)
    (fin-ack-rx close-wait ack-tx)
    (rst-rx closed cancel-tm)
    (bad-flags closed rst-tx))

   (last-ack ;; close last ack state
    (app-listen closed rst-tx)
    (app-connect syn-sent syn-tx)
    (app-send closed rst-tx)
    (seq-recv last-ack new-seq-rx)
    (app-close closed no-act)
    (timeout closed no-act)
    (ack-rx closed app-closed)
    (syn-rx closed rst-tx)
    (syn-ack-rx closed rst-tx)
    (fin-rx last-ack fin-ack-tx)
    (fin-ack-rx closed no-act)
    (rst-rx closed cancel-tm)
    (bad-flags closed rst-tx))

   (fin-wait-1 ;; fin-wait-1 state
    (app-listen closed rst-tx)
    (app-connect closed rst-tx)
    (app-send closed rst-tx)
    (seq-recv fin-wait-1 new-seq-rx)
    (app-close fin-wait-1 no-act)
    (timeout fin-wait-1 no-act)
    (ack-rx fin-wait-2 new-ack)
    (syn-rx closed rst-tx)
    (syn-ack-rx closed rst-tx)
    (fin-rx closing ack-tx)
    (fin-ack-rx timed-wait ack-tx)
    (rst-rx closed cancel-tm)
    (bad-flags closed rst-tx))

   (fin-wait-2 ;; fin-wait-2 state
    (app-listen closed rst-tx)
    (app-connect closed rst-tx)
    (app-send closed rst-tx)
    (seq-recv fin-wait-2 no-act)
    (app-close fin-wait-2 no-act)
    (timeout fin-wait-2 no-act)
    (ack-rx fin-wait-2 new-ack)
    (syn-rx closed rst-tx)
    (syn-ack-rx closed rst-tx)
    (fin-rx timed-wait ack-tx)
    (fin-ack-rx timed-wait ack-tx)
    (rst-rx closed cancel-tm)
    (bad-flags closed rst-tx))

   (closing ;; closing state
    (app-listen closed rst-tx)
    (app-connect closed rst-tx)
    (app-send closed rst-tx)
    (seq-recv closed rst-tx)
    (app-close closed rst-tx)
    (timeout closing no-act)
    (ack-rx timed-wait no-act)
    (syn-rx closed rst-tx)
    (syn-ack-rx closed rst-tx)
    (fin-rx closed ack-tx)
    (fin-ack-rx closed ack-tx)
    (rst-rx closed cancel-tm)
    (bad-flags closed rst-tx))

   (timed-wait ;; timed-wait state
    (app-listen timed-wait no-act)
    (app-connect timed-wait no-act)
    (app-send timed-wait no-act)
    (seq-recv timed-wait no-act)
    (app-close timed-wait no-act)
    (timeout timed-wait no-act)
    (ack-rx timed-wait no-act)
    (syn-rx timed-wait no-act)
    (syn-ack-rx timed-wait no-act)
    (fin-rx timed-wait no-act)
    (fin-ack-rx timed-wait no-act)
    (rst-rx timed-wait no-act)
    (bad-flags timed-wait no-act)))))

(defgeneric process-action(action protocol &optional packet header dst-address)
  (:documentation "Process action in response to events"))


;; fill in flag events table
(eval-when(:compile-toplevel :load-toplevel :execute)
  (defparameter *flag-events*
  (make-array max-flag :initial-element bad-flags
              :element-type '(unsigned-byte 8))
  "Mapping between flags and events")
(map
 'nil
 #'(lambda(entry) (setf (aref *flag-events* (first entry))
                        (symbol-value (second entry))))
 '((#x00 seq-recv)  ;; no flags
   (#x01 fin-rx)    ;; fin
   (#x02 syn-rx)    ;; syn
   (#x04 rst-rx)    ;; rst
   (#x08 seq-recv)  ;; psh flag is not used
   (#x09 fin-rx)    ;; fin
   (#x0a syn-rx)    ;; syn
   (#x0c rst-rx)    ;; rst
   (#x10 ack-rx)    ;; ack
   (#x11 fin-ack-rx);; fin/ack
   (#x12 syn-ack-rx);; syn/ack
   (#x14 rst-rx)    ;; rst
   (#x18 ack-rx)    ;; ack
   (#x19 fin-ack-rx);; fin/ack
   (#x1a syn-ack-rx);; syn/ack
   (#x1c rst-rx)    ;; rst
   (#x20 seq-recv)  ;; no flags (urgent not presently used)
   (#x21 fin-rx)    ;; fin
   (#x22 syn-rx)    ;; syn
   (#x24 rst-rx)    ;; rst
   (#x28 seq-recv)  ;; psh flag is not used
   (#x29 fin-rx)    ;; fin
   (#x2a syn-rx)    ;; syn
   (#x2c rst-rx)    ;; rst
   (#x30 ack-rx)    ;; ack (urgent not used)
   (#x31 fin-ack-rx);; fin/ack
   (#x32 syn-ack-rx);; syn/ack
   (#x34 rst-rx)    ;; rst
   (#x38 ack-rx)    ;; ack
   (#x39 fin-ack-rx);; fin/ack
   (#x3a syn-ack-rx);; syn/ack
   (#x3c rst-rx))))    ;; rst

(declaim (inline lookup-state flags-event))
(defun lookup-state(state event)
  "Given a current state and event return the new state and an action"
  (values-list (aref *state-table* state event)))
(defun flags-event(flags) (aref *flag-events* flags))

;; default tcp parameters
;; (defvar *default-seg-size* 512 "Segment Size")
;; (defvar *default-adv-win* #xFFFF "Advertised window")
;; (defvar *default-ss-thresh* #xFFFF "Slow-start threshold")
;; (defvar *default-tx-buffer* most-positive-fixnum)
;; (defvar *default-rx-buffer* most-positive-fixnum)
;; (defvar *default-tw-timer* 5 "Default timeout in seconds")
;; (defvar *default-conn-timer* 6 "Default timeout in seconds")
;; (defvar *default-del-ack-timer* 0 "Default to not used")
;; (defvar *default-initial-cwnd* 1 "Default initial cwnd in segments")

;; note - use accessors where possible to enable tracing
(defclass tcp(protocol timers-manager copyable-object)
  ((pending-data :initform nil :accessor pending-data
                 :documentation "Data sent by application but not acknowledged")
   (buffered-data :initform nil :type list :accessor buffered-data
                  :documentation "data Received but out of sequence")
   (rtt :initform (make-instance 'rtt-mdev) :initarg :rtt :reader rtt
        :documentation "RTT estimator")

   ;; connection state
   (max-connection-retry-count
    :type integer :initform 3 :accessor max-connection-retry-count
    :initarg :max-connection-retry-count)
   (connection-retry-count :type integer :accessor connection-retry-count)

   ;; timers
   (connection-timer :type timer :initform (make-instance 'timer :delay 6))
   (delayed-ack-timer :type timer :initform (make-instance 'timer :delay 0))
   (timed-wait-timer :type timer :initform (make-instance 'timer :delay 5))
   (last-ack-timer :type timer :initform (make-instance 'timer))
   (retransmit-packet-timer :type timer :initform (make-instance 'timer))

   ;; sequence numbers
   (first-pending-seq
    :type (unsigned-byte 32) :accessor first-pending-seq
    :documentation "Sequence number for start of pending data")
   (next-tx-seq :initform (random #xFFFFFFFF) :type (unsigned-byte 32)
                :documentation "Next sequence to send" :accessor next-tx-seq)
   (high-tx-mark :initform 0 :type (unsigned-byte 32) :accessor high-tx-mark
                 :documentation "TX High water mark, for counting retx")
   (highest-rx-ack :initform 0 :type (unsigned-byte 32) :accessor highest-rx-ack
                   :documentation "largest ack received")
   (last-rx-ack :initform 0 :type (unsigned-byte 32) :accessor last-rx-ack
                :documentation "For dupack testing")
   (fast-recovery-mark :initform 0 :type (unsigned-byte 32)
                       :accessor fast-recovery-mark
                       :documentation "Mark for fast recovery")
   (next-rx-seq :initform 0 :type (unsigned-byte 32)
                :documentation"Next expected sequence number")
   (next-ack-seq :type (unsigned-byte 32)
                 :documentation "Set non-zero when using delayed acks")

   ;; recorded times
   (last-rx-time :accessor last-rx-time :type time-type
                 :documentation "TIme last packet received")
   (syn-time :initform 0.0 :accessor syn-time :type time-type
             :documentation "Time SYN sent")
   (last-ack-time :initform 0.0 :type time-type
                  :documentation  "Time last Ack received")

   ;; counters
   (pkt-received-count :initform 0 :type integer :accessor pkt-received-count
                       :documentation "Number of packets received")
   (pkt-sent-count :initform 0  :type integer :accessor pkt-sent-count
                   :documentation "Count of packets sent")
   (retransmit-count :initform 0 :accessor retransmit-count :type integer
                     :documentation "Count of retransmits")
   (timeout-count :initform 0 :accessor timeout-count :type integer
                  :documentation "Count of timeouts")
   (dup-ack-count :initform 0 :type integer :accessor dup-ack-count
                  :documentation "Number of dup acks in a row")
   (bytes-sent :initform 0 :accessor bytes-sent :type integer
               :documentation "Total bytes sent")
   (bytes-received :initform 0 :accessor bytes-received :type integer
                   :documentation "Total bytes received")


   ;; (fast-recovery-p :initform nil :type boolean :accessor fast-recovery-p
   ;;                  :documentation "True if fast recovery in progress")
   ;;
   ;; (no-timer-p  :initform nil :type boolean :accessor no-timer-p
   ;;              :documentation "True if skip resched of re-tx timer")

   ;; ;; Window Management
   (seg-size :initarg :seq-size :initform 512 :type (unsigned-byte 16)
             :accessor seg-size
             :documentation "SegmentSize")
   ;; (rx-win :initform *default-adv-win* :type integer :accessor rx-win
   ;;         :documentation "Window as received from peer")
   ;; (adv-win :initform *default-adv-win* :type integer :accessor adv-win
   ;;          :documentation "Window to advertise to peer")
   ;; (cwnd :initform (* *default-initial-cwnd* *default-seg-size*)
   ;;       :type integer :accessor cwnd
   ;;       :documentation "Congestion window")
   (slow-start-threshold :initform #xFFFF :type integer)
   ;; (initial-cwnd :initform *default-initial-cwnd*
   ;;               :type integer :accessor initial-cwnd
   ;;               :documentation "Initial (and reset) value for cWnd")

   ;; TCP state
   (state :initform closed :accessor state :type tcp-states
          :documentation "Current state")
   (close-on-empty :initform nil :type boolean :accessor close-on-empty
                   :documentation "True if send FIN pkt on empty data")
   ;; (pending-close :initform nil :accessor pending-close :type boolean
   ;;                :documentation "True if close pending")
   (close-notified :initform nil :accessor close-notified :type boolean
                   :documentation "True if close has been notified")
   (need-ack :initform nil :type boolean :accessor need-ack
             :documentation "True if need ACK bit transmit")
   ;; (close-req-notified
   ;;  :initform nil :accessor close-req-notified :type boolean
   ;;  :documentation "True if close request has been notified")
   ;; ;;Buffer limits
   ;; (tx-buffer :initform *default-tx-buffer* :accessor tx-buffer :type integer
   ;;            :documentation "Size of tx buffer")
   ;; (rx-buffer :initform *default-rx-buffer* :accessor rx-buffer :type integer
   ;;            :documentation "Size of rx buffer")
   ;; Statistics


   ;; (total-ack :initform 0 :accessor total-ack :type integer
   ;;            :documentation "Total bytes acked")
   ;; (open-time :initform 0.0 :accessor open-time :type time-type
   ;;            :documentation "Time connection was opened")

   ;;
   ;;                :documentation "Count of packets received")
   ;; (time-seq-stats :initform nil :accessor time-seq-stats :type vector
   ;;                 :documentation "Pointer to Array of time/seq stats")
   ;; (parent :initform nil :accessor parent :type tcp
   ;;         :documentation "Parent (server) tcp (if exists)")
   ;; (number-children :initform 0 :accessor number-children :type integer
   ;;                   :documentation "Number of child TCP's (if server)")
   ;; (child-limit :initform most-positive-fixnum
   ;;              :accessor child-limit :type integer
   ;;              :documentation "Maximum number of allowed children")
   ;; (number-fin :initform 0 :accessor number-fin :type integer
   ;;             :documentation "Number fin packets rx, just testing")
   ;; (next-fid :initform 0 :documentation "Next unique tcp flow id" :allocation :class)
   ;; (last-timer-delay
   ;;  :initform 0.0 :accessor last-timer-delay :type time-type
   ;;  :documentation "Timeout delay of last scheduled retx timer")
   #+debug(dbvec :initform (alg:make-queue :initial-size 50 :element-type db-hist :implementation :wrap))
   (initargs :type list
             :documentation "Arguments passed during initialisation - used during reset")
)
  (:documentation "A model of the Transmission Control Protocol."))

(defmethod initialize-instance :after ((tcp tcp) &rest initargs
                                       &key &allow-other-keys)
  (setf (slot-value tcp 'initargs) initargs))&ALLOW-OTHER-KEYS

(defmethod reset((tcp tcp))
  (let ((initargs (slot-value tcp 'initargs)))
    (dolist(slot (closer-mop:class-slots tcp))
      (when (eql :instance (closer-mop:slot-definition-allocation slot))
        (slot-makunbound tcp (closer-mop:slot-definition-name slot))))
    (apply #'shared-initialize tcp t initargs)
    (setf (slot-value tcp 'initargs) initargs)))

(defmethod protocol-number((tcp tcp)) 6)

(defmethod default-trace-detail((protocol tcp))
  `(type src-port dst-port sequence-number ack-number flags fid))

(defmethod schedule((timer (eql 'last-ack-timer)) (tcp tcp))
  (schedule (rtt-retransmit-timer (rtt tcp))
            (timer 'last-ack-timer tcp)))

(defmethod schedule((timeout (eql 'retransmit-packet-timeout)) (tcp tcp))
  (schedule (rtt-retransmit-timer (rtt tcp))
            (timer 'retransmit-packet-timer tcp)))

(defmethod receive((tcp tcp) (packet packet) (layer4 layer3:ipv4)
                   &key dst-address &allow-other-keys)
  (setf (last-rx-time tcp) (simulation-time))
  (incf (pkt-received-count tcp))
  (let ((header (peek-pdu packet))
        (old-state (state tcp)))
    ;; Check if ACK bit set, and if so notify rtt estimator
    (when (flag-set-p (flags header) ack)
      (rtt-ack-seq (ack-number header) (rtt tcp))
      ;; Also insure that connection timer is canceled,
      ;; as this may be the final part of 3-way handshake
      (cancel 'connection-timer tcp))

    ;; Check syn/ack and update rtt if so
    (when (and (= old-state syn-sent) (= (flags header) (logior syn ack)))
      (math:record (- (simulation-time) (syn-time tcp)) (rtt tcp)))

    ;; determine event type and process
    (let ((event (flags-event (flags header)))
          (action (process-event event tcp))
          (state (state tcp)))
      ;; sanity checks
      (assert (not (and (= state close-wait) (= event seq-recv)))
              ()
              "TCP ~A New seq in close-wait" tcp)
      (assert (not (and (= state closed) (= event syn-rx)))
              ()
              "TCP ~A syn in closed l=~A:~A r=~A:~A"
                tcp (ipaddr (node tcp)) (local-port tcp)
                dst-address (src-port header))
      (assert (not (and (= old-state listen) (/= state listen)))
              ()
               "TCP transition from ~A to ~A" old-state state)

      (cond
        ((and (/= state old-state) (= state timed-wait))
         (connection-closed (application tcp) tcp))
        ((and (= state closed) (/= old-state closed))
         (cancel :all tcp))
        ((= state timed-wait)
         (cancel :all tcp)
         (schedule 'timed-wait-timer tcp)))
      (process-action action tcp packet header dst-address)
      (when (and (= state last-ack)
                 (not (busy-p  (timer 'last-ack-timer tcp))))
        (schedule 'last-ack-timer tcp)))))

(defmethod send((tcp tcp) data application
                &key &allow-other-keys)
  (assert (member (state tcp) #.(list established syn-sent close-wait))
          ()
          "TCP ~A send wrong state ~A" tcp (state tcp))
  (let ((pending (pending-data tcp)))
    (if pending
        (layer5:add-data pending data)
        (setf (pending-data tcp) (copy data))))
  (process-action (process-event app-send tcp) tcp)
  (length-bytes data))

(defmethod open-connection(peer-address peer-port (tcp tcp))
  (call-next-method)
  (unless (bound-p protocol) (bind protocol))
  (setf (connection-retry-count tcp) 0)
  (let ((state (state protocol))
        (action (process-event app-connect protocol)))
    (assert (= action syn-tx)
            (action)
            "Bad connect action ~A from ~A state" action state)
    (process-action action protocol)))

(defmethod close-connection((tcp tcp))
  (with-accessors(state) tcp
    (unless (= state closed)
      (let ((action (process-event app-close protocol))
            (pending-data (pending-data tcp)))
        (when (and pending-data (/= 0 (length-bytes pending-data)))
           (when (= action fin-tx)
             (setf (close-on-empty tcp) t)
             (return-from close-connection t)))
        (process-action action protocol)
        (when (and (= state last-ack)
                   (not (busy-p (timer 'last-ack-timer tcp))))
          (schedule 'last-ack-timer tcp))))))

(defmethod connection-complete :around (application (tcp tcp) &key failure)
  (setf (close-notified tcp) nil)
  (call-next-method))

(defmethod connection-closed :around (application (tcp tcp))
  (unless (close-notified tcp)
    (call-next-method)
    (setf (close-notified tcp) t)))

(defmethod timeout((timer (eql 'connection-timer)) (tcp tcp))
  (with-accessors(connection-retry-count max-connection-retry-count
                                         application) tcp
    (if (= (state tcp)syn-sent)
        ;; if active side of connection attempt
        (cond
          ((>= (incf connection-retry-count) max-connection-retry-count)
           ;; if exceeded max number connections so timeout report failure
           (process-action (process-event timeout tcp) tcp)
           (incf (timeout-count tcp))
           (connection-error application tcp :timeout))
          (t ;; else try again
           (process-action (process-event app-connect tcp) tcp))))
    (t
     ;; passive side of connection (listener) -
     ;; notify application of missing ACK on 3-way handshake
     (connection-error application tcp :timeout))))

;; must be implemented in concrete classes
(defmethod timeout :before ((timer (eql 'retransmit-packet-timer)) (tcp tcp))
  (incf (timeout-count tcp)))

(defmethod timeout((timer (eql 'delayed-ack-timer)) (tcp tcp))
  (send-ack tcp (next-ack-seq tcp) t)
  (slot-makunbound tcp 'next-ack-seq))

(defmethod timeout((timer (eql 'timed-wait-timer)) (tcp tcp))
  ;; finished so close down tcp endpoint
  (cancel :all tcp)
  (connection-closed (application tcp) tcp))

(defmethod timeout((timer (eql 'last-ack-timer)) (tcp tcp))
  ;; close connection assuming peer has gone
  (when (= (state tcp) last-ack)
      (process-action (process-event timeout tcp) tcp))
  (connection-closed (application tcp) tcp))

(defmethod timeout :after (timer (tcp tcp))
  (when (and (= (state tcp) last-ack) (not (busy-p 'last-ack-timer tcp)))
    (schedule 'last-ack-timer tcp)))

;; tcp specific functions

(defmethod bind :after((tcp tcp) &key &allow-other-keys)
  ;; if no peer defined this is a listener
  (unless (connected-p tcp)
    (process-action (process-event app-listen tcp) tcp)))

(defmethod connection-error((tcp tcp) (layer3 layer3:protocol) error)
  ;; Called by ICMP when  unreachable response is received
  (when (= state syn-sent)
    (process-action (process-event timeout tcp) tcp)
    (connection-error (application tcp) tcp :timeout)))

(defun respond(tcp peer-address peer-port)
  ;; called by copy of listeneing TCP. SYN packet received - bind to peer
  (bind tcp :peer-address peer-address :peer-port peer-port)
  (process-action syn-ack-tx))

(defun un-ack-data-count(tcp) (- (next-tx-seq tcp) (highest-rx-ack tcp)))
(defun bytes-in-flight(tcp) (- (high-tx-mark tcp) (highest-rx-ack tcp)))
(defun window(tcp) (min (rx-win tcp) (c-wind tcp)))
(defun available-window(tcp)
  (let ((unack (un-ack-data-count tcp))
        (win (window tcp)))
    (if (< win unack) 0 (- win unack))))

(defun process-event(event tcp)
  (let ((old-state (state tcp)))
    (multiple-value-bind(new-state action) (lookup-state old-state event)
      (setf (state tcp) new-state)
      (cond
        ((and (= new-state timed-wait) (/= old-state timed-wait))
         (schedule 'timedWaitTimeout tcp))
        ((and (= new-state closed)
              (/= old-state closed)
              (/= event timeout)
              (bound-p tcp))
         ;; finally closed so notify application
         (connection-closed (application tcp) tcp)))
      action)))

(defmethod process-action((action (eql no-act)) tcp
                          &optional packet header dst-address)
    (declare (ignore tcp packet header dst-address)))

(defmethod process-action((action (eql ack-tx)) tcp
                          &optional packet header dst-address)
  (send-packet tcp ack (next-tx-seq tcp) (next-rx-seq tcp))
  ;; save receiver advertised window
  (when header (setf (rx-win tcp) (window header))))

(defmethod process-action((action (eql ack-tx-1)) tcp
                          &optional packet header dst-address)
    ;; save receiver advertised window
  (when header (setf (rx-win tcp) (window header)))
  (cancel 'connection-timer tcp)
  ;; notify application
  (connection-complete (application tcp) tcp)
  ;; if no data need to send ack
  (unless (pending-data tcp)
    (send-packet tcp ack (next-tx-seq tcp) (next-rx-seq tcp))))

(defmethod process-action((action (eql tst-tx)) tcp
                          &optional packet header dst-address)

(defun send-ack(tcp ack-number &optional forced)
  (let ((delayed-ack-timer (timer 'delayed-ack-timer tcp))
        (send-it t))
    (unless (or forced (zerop (timer-delay delayed-ack-timer)))
        ;; need to delay this ack - see if pending
        (if (busy-p delayed-ack-timer)
            (cancel delayed-ack-timer tcp)
            (progn
              ;; delay this ack
              (schedule 'delayed-ack-timer tcp)
              (setf next-ack-seq ack-number)
              (setf send-it nil)))))
    (or
     (send-pending-data tcp t)
     (if send-it (send-packet tcp ack (next-tx-seq tcp) ack-number))))

(defun send-packet(tcp flags sequence-number ack-number &key
                   data
                   (dst-address (peer-address tcp))
                   (dst-port (peer-port tcp)))
  (let ((packet (make-instance 'packet :data data :fid (fid tcp))))
    (assert (<= (length-bytes data) (mtu tcp)))
    (when data ;; if data
      (unless (busy-p (timer 'retransmit-packet-timer tcp))
        (schedule 'retransmit-packet-timer tcp)
        (rtt-sent-seq seq (length-bytes packet) (rtt tcp))
        (when (< seq (high-tx-mark tcp))
          (incf (retransmit-count tcp)))))
    (when (and (or (= state fin-wait-1) (= state fin-wait 2))
               (not (penging-data tcp)))
      (setf flags (logior flags fin)))
    (when (need-ack tcp) (setf flags (logior flags ack)))
    (push-pdu
     (make-instance 'tcp-header
                    :src-port (local-port tcp)
                    :dst-port peer-port
                    :seq sequence-number
                    :ack ack-number
                    :flags flags
                    :window (adv-win tcp)
                    :data-length (length-bytes packet)
                    :cwnd (cwnd tcp))
     packet)
    (send  (layer3:find-protocol 'layer3:ipv4 (node udp)) packet tcp
           :src-address (or (local-address tcp) (network-address (node tcp)))
           :dst-address peer-address
           :ttl (ttl tcp)
           :tos (tos tcp))
    (incf (pkts-sent-count tcp))))


(defun send-pending-data(tcp &optional with-ack)
  (let ((n-sent 0)
        (pending-data (pending-data tcp))
        (interface (find-interface (peer-address tcp) (node udp))))
    (unless pending-data (return-from send-pending-data nil))
    (loop
       ;; if no data finished
       (when (zerop (length-bytes pending-data))
         (setf (pending-data tcp) nil)
         (return))
       (let ((w (available-window tcp))
             (seg-size (seg-size tcp)))
         ;; don't send if window less than segment size and more data
         (when (and  (< w seg-size) (> (length-bytes data) w))
           (return))
         ;; if no buffer available stop and request
         (unless (layer1:buffer-available-p
                  (+ 100 (length-bytes data)) interface)
           (add-notification udp #'send-pending-data interface)
           (return))
         (let ((s (min w seg-size))
               (data (remove-data s pending-data))
               (flags 0))
           ;; see if we need fin flag
           (when (and (close-on-empty tcp)
                      (zerop (length-bytes pending-data)))
             (setf flags (logior flags fin))
             (setf (state tcp) fin-wait-1))
           (send-packet tcp flags (next-tx-seq tcp) (next-rx-seq tcp) :data data)
           (incf n-sent)
           (setf (next-tx-seq tcp) (seq+ (next-tx-seq tcp) (length-bytes data)))
           (setf (high-tx-mark tcp) (max (next-tx-seq tcp) (high-tx-mark tcp)))
           (incf (bytes-sent tcp) (length-bytes data)))))
    (when (> n-sent 0)
      (schedule 'retransmit-packet tcp)
      t)))




(defclass tcp-reno(tcp)
  ()
  (:documentation "Tahoe tcp implementation"))

(defmethod timeout((timer (eql 'retransmit-packet-timer)) (tcp tcp))
  )


(defmethod process-action(action protocol &optional packet header dst-address)
  (:documentation "Process action in response to events")
  (:method((action (eql no-act)) tcp &optional packet header dst-address)
    (declare (ignore tcp packet header dst-address))))


;; ;; timer events

;; (defun conn-timer-event(protocol)
;;   (let ((application (application protocol)))
;;     (if (= (state protocol) syn-sent) ;; this is active side of connection
;;         (if (> (decf (conn-count protocol)) 0)
;;             (process-action (process-event app-connect protocol) protocol)
;;             (progn
;;               (process-action (process-event timeout protocol) protocol)
;;               (when application (connection-failed application protocol))))
;;         ;; is passive listener
;;         (when application (connection-failed application protocol)))))

;; (defun delayed-ack-timer-event(protocol)
;;   (when (= 0 (next-ack-seq protocol))
;;     (error "DelAck timeout with no pending ack"))
;;   (send-ack  (next-ack-seq protocol) protocol t)
;;   (setf  (next-ack-seq protocol) 0))

;; (defun tw-timer-event(protocol)
;;   (cancel-all-timers protocol)
;;   (cancel-notification protocol)
;;   (unbind protocol))

;; (defun last-ack-timer-event(protocol)
;;   (when (= (state protocol) last-ack)
;;     (process-action (process-event timeout protocol) protocol))
;;   (unless (close-notified protocol)
;;     (setf (close-notified protocol) t)
;;     (when-bind(a (application protocol)) (closed a protocol))))


;; the following 3 generic functions ar<e implemented for different variations
;; of TCP

;; (defgeneric newack(seq tcp)
;;   (:documentation "Called by tcp for new (non duplicate acks)"))

;; (defgeneric dupack(tcp-header count tcp)
;;   (:documentation "Duplicate Ack received"))

;; (defgeneric retx-timer-event(tcp)
;;   (:documentation "Retransmit timeout"))

;; (defun next-fid(tcp) (incf (slot-value tcp 'next-fid)))

;; (defmethod reset((tcp tcp))
;;   (call-next-method)
;;   (setf (buffered-data tcp) nil)
;;   (reset (pending-data tcp))
;;   ;; remove all timeouts from scheduler
;;   (cancel-all-timers tcp)
;;   ;; reset rtt calculations
;;   (reset (rtt tcp))
;;   ;; cancel notifications
;;   (map 'nil #'(lambda(interface) (delete-notifications udp interface))
;;        (interfaces (node udp)))
;;   ;; initialize specified slots according to initforms
;;   (shared-initialize
;;    tcp
;;    '(next-tx-seq
;;      high-tx-mark
;;      highest-rx-ack
;;      dup-ack-count
;;      fast-recovery-p
;;      need-ack
;;      no-timer-p
;;      next-rx-seq
;;      seg-size
;;      rx-win
;;      adv-win
;;      cwnd
;;      ss-thresh
;;      initial-cwnd
;;      tw-timer
;;      conn-timer
;;      del-ack-timer
;;      retry-count
;;      conn-count
;;      state
;;      close-one-empty
;;      delete-on-complete
;;      delete-on-timed-wait
;;      pending-close
;;      close-notified
;;      close-req-notified
;;      tx-buffer
;;      rx-buffer
;;      total-ack
;;      open-time
;;      last-rx-time
;;      last-ack-time
;;      syn-time
;;      retransmit-count
;;      timeout-count
;;      pkts-sent
;;      pkts-received
;;      bytes-sent
;;      bytes-received
;;      time-seq-stats
;;      parent
;;      number-children
;;      child-limit
;;      number-fin
;;      last-timer-delay
;;      last-measured-rtt))
;;   #+debug(setf (fill-pointer (slot-value tcp 'db-vec)) 0))

;; (defmethod copy((original tcp))
;;     (call-nest-method)
;;   (let ((copy (make-instance (class-of original))))
;;     ;; copy rtt
;;     (map 'nil
;;          #'(lambda(slot)
;;              (setf (slot-value copy slot) (slot-value original slot)))
;;          '(seg-size rx-win adv-win cwnd ss-thresh initial-cwnd
;;            tw-timer conn-timer del-ack-timer
;;            retry-count conn-count state close-on-empty
;;            pending-close close-notified close-req-notified
;;            tx-buffer rx-buffer
;;            retransmit-count timeout-count
;;            child-limit
;;            last-timer-delay last-measured-rtt))
;;     (setf (rtt copy)
;;           (make-instance (class-of (rtt original))
;;                          :initial-estimate (estimate (rtt original))))
;;     (when-bind(tss (time-seq-stats original))
;;               (loop :for i :from 0 :below (length tss)
;;                     :when (aref tss i) :do (enable-time-seq i copy)))
;;     copy))


;; (defstruct time-seq
;;   "Time/sequence logs"
;;  (time 0.0 :type time-type)
;;  (seq 0 :type (unsigned-byte 32)))

;; (defun note-time-seq(selector seq protocol)
;;   (when-bind(tss (time-seq-stats protocol))
;;      (when-bind(tsv (aref tss selector))
;;        (vector-push-extend
;;         (make-time-seq :time (simulation-time) :seq seq) tsv))))


;; (defmethod notify((tcp tcp)) (tcp-send-pending tcp))





;; (defun listen(protocol)
;;   ;; set endpoint to accept connection requests
;;   (process-action (process-event app-listen protocol) protocol))

;; (defun child-complete(protocol)
;;   (if (= 0 (number-children protocol))
;;       (error "TCP child complete with no children")
;;       (decf (number-children protocol))))

;; (defun abort(protocol)
;;   "Called by ICMP when unreachable response is received"
;;   (when (= (state protocol) syn-sent)
;;     (process-action (process-event timeout protocol) protocol)
;;     (when-bind(a (application protocol)) (connection-failed a protocol))))

;; (defun respond(peer-address peer-port protocol)
;;   "This tcp is a copy of a listening tcp. The SYN packet was received
;;   by the listener, and it created a copy of itself.  This copy
;;   will bind to the local port/ip, remote port/ip and respond to all
;;   future packets."
;;   (setf (state protocol) syn-rcvd
;;         (peer-address protocol) peer-address
;;         (peer-port protocol) peer-port)
;;   (node:bind protocol (node protocol)
;;              :local-port (local-port protocol)
;;              :remote-port peer-port
;;              :remote-address peer-address)
;;   (process-action syn-ack-tx protocol))

;; (defun reject(peer-address peer-port tcp)
;;   "Application no accepting more connections"
;;   (send-packet rst 0 0 tcp
;;                :dst-address peer-address :dst-port peer-port))

;; (defun unack-data-count(protocol)
;;   "Return count of unacknowledged data bytes"
;;   (- (next-tx-seq protocol) (highest-rx-ack protocol)))

;; (defun bytes-in-flight(protocol)
;;   "Return count of unacknowledged data bytes"
;;   (- (high-tx-mark protocol) (highest-rx-ack protocol)))

;; (defmethod window((protocol tcp))
;;   "Return window size"
;;   (min (rx-win protocol) (cwnd protocol)))

;; (defun available-window(protocol)
;;   "Return unfilled portion of window"
;;   (let ((unack (unack-data-count protocol))
;;         (win (window protocol)))
;;     (if (< win unack) 0 (- win unack))))

;; (defun new-cwnd(ack-bytes tcp)
;;   "Adjust congestion window in response to new ack's received"
;;   (declare (ignore ack-bytes))
;;   (let ((seg-size (seg-size tcp))
;;         (cwnd (cwnd tcp)))
;;     (if (< cwnd (ssthresh tcp))
;;         (incf (cwnd tcp) seg-size) ;; slow start mode inc by one seg-size
;;         (let ((adder (/ (*  seg-size seg-size) cwnd)))
;;           ;; as per RFC 2581
;;           (incf (cwnd tcp) (if (zerop adder) 1 adder))))
;;     (note-time-seq log-cwin cwnd tcp)))

;; (defun enable-time-seq(selector tcp)
;;   (let ((tss (or (time-seq-stats tcp)
;;                  (setf (time-seq-stats tcp) (make-array log-last)))))
;;     (setf (aref tss selector)
;;           (make-array 10
;;                       :element-type 'time-seq :fill-pointer 0 :adjustable t))))

;; (defun disable-time-seq(selector tcp)
;;   (when-bind(tss (time-seq-stats tcp))
;;      (setf (aref tss selector) nil)))

;; (defun reset-time-seq(selector tcp)
;;   (when-bind(tss (time-seq-stats tcp))
;;      (setf (fill-pointer (aref tss selector)) 0)))

;; (defun log-time-seq(selector protocol &key
;;                     (os *standard-output*) div mod (sep " "))
;;     (when-bind(tss (time-seq-stats protocol))
;;      (when-bind(tsv (aref tss selector))
;;        (loop :for ts :across tsv
;;              :do (format os "~@?~A~D~%" *time-format* (time-seq-time ts) sep
;;                          (let ((seq (time-seq-seq ts)))
;;                            (when div (setf seq (/ seq div)))
;;                            (when mod (setf seq (mod seq mod)))
;;                            seq))))))




;; (defmethod process-action((action (eql 'ack-tx)) tcp &optional
;;                           packet header dst-address)
;;   (declare (ignore packet dst-address))
;;   (send-packet ack (next-tx-seq tcp) (next-rx-seq tcp) tcp)
;;   (when header  ;;Save the receiver advertised window
;;     (setf (rx-win tcp) (window header))))

;; (defmethod process-action((action (eql 'ack-tx-1)) tcp &optional
;;                           packet header dst-address)
;;     (declare (ignore packet dst-address))
;;     ;; Ack in response to syn/ack"
;;      (when header  ;;Save the receiver advertised window
;;        (setf (rx-win tcp) (window header)))
;;      (cancel-timer 'conn-timer-event tcp)
;;      (when-bind(a (application tcp)) ;;  Notify application
;;         (connection-complete a tcp))
;;      (unless (pending-data tcp)
;;        ;; Application did not send anything, need to send ack
;;        (send-packet ack (next-tx-seq tcp) (next-rx-seq tcp) tcp)))

;; (defmethod process-action((action (eql 'rst-tx)) tcp &optional
;;                           packet header dst-address)
;;     (declare (ignore packet))
;;     (send-packet rst 0 0 tcp
;;                  :dst-address dst-address
;;                  :dst-port (if header (src-port header) (peer-port tcp))))

;; (defmethod process-action((action (eql 'syn-tx)) tcp &optional
;;                           packet header dst-address)
;;     (declare (ignore packet header dst-address))
;;     ;; Schedule a Timeout for "no reply" processing
;;     (schedule-timer (conn-timer tcp) 'conn-timer-event tcp)
;;     ;; log time sent to get an initial rtt estimate
;;     (setf (syn-time tcp) (simulation-time))
;;     (send-packet syn 0 0 tcp))

;; (defmethod process-action((action (eql 'syn-ack-tx)) tcp &optional
;;                           packet header dst-address)
;;   ;; Need a little special-casing here.  If tcp header passed in,
;;   ;; then make a copy and ask the copy to respond, because this is
;;   ;; the listening TCP.  Otherwise, it is the copied tcp from above,
;;   ;; then just send the Syn/Ack
;;   (declare (ignore packet dst-address))
;;   (let ((application (application tcp)))
;;     (cond
;;       ((and header (= (state tcp) listen))
;;        ;; this is the listener - create a copy for response
;;        (let* ((responder (copy tcp))
;;               (accept
;;                (if application
;;                    (progn
;;                      (setf (application responder) application)
;;                      (connection-from-peer application responder
;;                                            (when header (src-port header)))
;;                      t))))
;;          (setf (parent responder) tcp)
;;          (setf (delete-on-timed-wait responder) t)
;;          (when (and (/= 0 (fid header)) (= 0 (fid responder)))
;;            (setf (fid responder) (fid header))) ;; assign fid to responder
;;          (if accept
;;              (progn
;;                (respond (src-address header) (src-port header) responder)
;;                (incf (number-children tcp)))
;;              (reject (src-address header) (src-port header) responder))))
;;       (t
;;        ;; this is the responder so just respond
;;        (send-packet (logior syn ack) 0 0 tcp)
;;        (schedule-timer (retransmit-timer (rtt tcp))
;;                        'conn-timer-event tcp)))))

;; (defmethod process-action((action (eql 'fin-tx)) tcp &optional
;;                           packet header dst-address)
;;   (declare (ignore packet header dst-address))
;;   (send-packet fin (next-tx-seq tcp) (next-rx-seq tcp) tcp))

;; (defmethod process-action((action (eql 'fin-ack-tx)) tcp &optional
;;                           packet header dst-address)
;;   (declare (ignore packet header dst-address))
;;   (send-packet (logior fin ack)
;;                (next-tx-seq tcp) (next-rx-seq tcp) tcp)
;;   (when (= (state tcp) last-ack)
;;     (schedule-timer (retransmit-timer (rtt tcp)) 'last-ack tcp)))

;; (defmethod process-action((action (eql 'new-ack)) tcp &optional
;;                           packet header dst-address)
;;   (declare (ignore dst-address))
;;   (let ((ack-number (ack-number header)))
;;     (cond
;;       ((< ack-number (highest-rx-ack tcp)) t) ;; old ack no action)
;;       ((and (= ack-number (highest-rx-ack tcp))
;;             (< ack-number (next-tx-seq tcp)))
;;        ;; dupack received
;;        (dupack header (incf (dup-ack-count tcp)) tcp)
;;        t)
;;       (t
;;        (when (> ack-number (highest-rx-ack tcp))
;;          (setf (dup-ack-count tcp) 0))
;;        (newack ack-number tcp)
;;        (new-rx packet header tcp))))) ;; in case associated data

;; (defmethod process-action((action (eql 'new-seq-rx)) tcp &optional
;;                           packet header dst-address)
;;   (declare (ignore dst-address))
;;   (new-rx packet header tcp)
;;   t)

;; (defmethod process-action((action (eql 'retx)) tcp &optional
;;                           packet header dst-address)
;;   (declare (ignore tcp packet header dst-address)))

;; (defmethod process-action((action (eql 'tx-data)) tcp &optional
;;                           packet header dst-address)
;;   (declare (ignore packet header dst-address))
;;   (tcp-send-pending tcp))

;; (defmethod process-action((action (eql 'peer-close)) tcp &optional
;;                           packet header dst-address)
;;   (declare (ignore dst-address))
;;   ;; First we have to be sure the FIN packet was not received
;;   ;; out of sequence.  If so, note pending close and process
;;   ;; new sequence rx
;;   (when (and header (/= (sequence-number header) (next-rx-seq tcp)))
;;     ;; process close later
;;     (setf (pending-close tcp) t)
;;     (new-rx packet header tcp)
;;     t)
;;   (let ((data (peek-pdu packet 1)))
;;     (when (and data (> (size data) 0))
;;       (new-rx packet header tcp)))
;;   (let ((saved-state (state tcp))
;;         (application (application tcp)))
;;     (if application ;; if application notify it to close
;;         (if (close-req-notified tcp)
;;             (close-request application tcp)
;;             (setf (close-req-notified tcp) t))
;;         (close-connection tcp)) ;; else just close here
;;     (when (= saved-state (state tcp))
;;       ;; need to ack app will close later
;;       (send-packet ack (next-tx-seq tcp) (next-rx-seq tcp) tcp)))
;;   (when (= (state tcp) last-ack)
;;     (schedule (retransmit-timer (rtt tcp)) 'last-ack-timer-event)))

;; (defmethod process-action((action (eql 'app-closed)) tcp &optional
;;                           packet header dst-address)
;;   (declare (ignore packet header dst-address))
;;   (when (and (= (state tcp) syn-sent) (not (close-notified tcp)))
;;     (setf (close-notified tcp) t)
;;     (when-bind(a (application tcp))
;;       (closed a tcp))))

;; (defun send-packet(flags seq sack tcp &key
;;                    (packet (make-instance 'packet))
;;                    (dst-address (peer-address tcp))
;;                    (dst-port (peer-port tcp)))
;;   (unless (node tcp)
;;     (error "TCP ~S sending packet with no local node" tcp))
;;   (let* ((data (peek-pdu packet))
;;          (data-length (size data))
;;          (state (state tcp)))
;;     (when data
;;       (let ((rto (retransmit-timer (rtt tcp))))
;;         (unless (or (no-timer-p tcp)
;;                     (find-timer 'retx-timer-event tcp))
;;           (schedule rto 'retx-timer-event))
;;         (setf (last-timer-delay tcp) rto)
;;         (sent-seq seq data-length (rtt tcp)) ;; notify rtt estimator
;;         (when (< seq (high-tx-mark tcp)) ;; count retransmits
;;           (incf (retransmit-count tcp)))))
;;     (let ((h (make-instance
;;               'tcp-header
;;               :src-port (local-port tcp)
;;               :src-address (ipaddr (node tcp))
;;               :dst-port dst-port
;;               :dst-address dst-address
;;               :sequence-number seq
;;               :ack-number sack
;;               :flags (logior flags
;;                             (if (and (or (= state fin-wait-1)
;;                                          (= state fin-wait-2))
;;                                      (not (pending-data tcp)))
;;                                 fin
;;                                 0)
;;                             (if (need-ack tcp) ack 0))
;;               :window (adv-win tcp)
;;               :fid (fid tcp)
;;               :data-length data-length
;;               :cwnd (cwnd tcp))))
;;       (push-pdu h packet)
;;       (setf (need-ack tcp) nil)
;;       (when (flag-set-p (flags h) ack)
;;         (setf (last-ack-time tcp) (simulation-time))))
;;     (layer3:send (layer3:protocol tcp) (node tcp) packet
;;                          :dst-address dst-address
;;                          :ttl (ttl tcp)
;;                          :protocol-number (protocol-number tcp)
;;                          :tos (tos tcp))))


;; (defun common-newack(sack tcp &optional skip-timer)
;;   "common-newack is called only for new (non-duplicate) acks
;;  and MUST be called by any subclass, from the newack function
;;  Always cancel any pending re-tx timer on new acknowledgement"
;;   (unless skip-timer (cancel-timer 'retx-timer-event tcp))
;;   (let ((numberack (- sack (highest-rx-ack tcp))))
;;     (incf (total-ack tcp) numberack)
;;     (setf (highest-rx-ack tcp) sack) ;;Note the highest recieved Ack
;;     (when (> ack (next-tx-seq tcp)) (setf (next-tx-seq tcp) sack)) ;; advanced
;;     (with-slots(pending-data first-pending-seq highest-rx-ack) tcp
;;       (when (and pending-data
;;                  (zerop (size-from-seq
;;                          first-pending-seq highest-rx-ack pending-data)))
;;         ;; All pending acked, can be deleted
;;         (setf pending-data nil)
;;         (cancel-timer 'retx-timer-event tcp)))
;;     ;; Notify application of data sent
;;     (when-bind(a (application tcp))
;;         (when (> numberack 0) (sent a numberack tcp))))
;;   ;; Try to send more data
;;   (tcp-send-pending tcp)
;;   ;; See if we need to post a re-tx timer
;;   (when (and (not skip-timer)
;;              (not (find-timer 'retx-timer-event tcp))
;;              (or (> (unack-data-count tcp) 0)
;;                  (= (state tcp) fin-wait-1)
;;                  (= (state tcp) fin-wait-2)))
;;     (schedule-timer (retransmit-timer (rtt tcp)) 'retx-timer-event tcp)))

;; (defun send-ack(sack tcp &optional forced)
;;   (let ((sendit
;;          (or forced
;;              (unless (zerop (del-ack-timer tcp))
;;               ;; Using delayed ack, see if already pending
;;               (cond
;;                 ((find-timer 'del-ack-timer-event tcp)
;;                  ;; if already pending cancel and send packet
;;                  (cancel-timer 'del-ack-timer-event tcp)
;;                  t)
;;                 (t ;; else delay this ack
;;                  (schedule-timer
;;                   (del-ack-timer tcp) 'del-ack-timer-event tcp)
;;                  (setf (next-ack-seq tcp) ack)
;;                  nil))))))
;;     (unless (tcp-send-pending tcp)
;;       ;; nothing sent send empty
;;       (when (= (last-ack-time tcp) (simulation-time))
;;         (error "TCP ~A resending ack ~A" tcp sack))
;;       (when sendit (send-packet ack (next-tx-seq tcp) sack tcp)))))

;; (defun new-rx(packet header tcp)
;;   (let* ((original-state (state tcp))
;;          (data (peek-pdu packet 1))
;;          (s (size data))
;;          (application (application tcp)))
;;     (when (> s 0)
;;       ;; Log sequence received if enabled
;;       (note-time-seq log-seq-rx (sequence-number header) tcp)
;;       ;;Note we need the ACK bit on next transmitted pkt
;;       (setf (need-ack tcp) t)
;;       (cond
;;         ((= (sequence-number header) (next-rx-seq tcp))
;;          ;;  Received seq is expected, deliver this and any buffered data
;;          (incf (next-rx-seq tcp) s)
;;          (incf (bytes-received tcp) s)
;;          (when application
;;            (pop-pdu packet)
;;            (receive application packet tcp (sequence-number header))
;;            (when (close-notified tcp)
;;              (error "TCP ~A got data after close-notified" tcp)))
;;          ;; look for buffered data
;;          (flet ((seqnum(p) (sequence-number (peek-pdu p)))) ;; packet seq num
;;            (setf (buffered-data tcp)
;;                  (sort (buffered-data tcp) #'< :key #'seqnum))
;;            (do ((packet (pop (buffered-data tcp)) (pop (buffered-data tcp))))
;;                ((not packet))
;;              (when  (> (seqnum packet) (next-rx-seq tcp))
;;                (push packet (buffered-data tcp)) ;; not next expected
;;                (return))
;;              (let* ((h (pop-pdu packet))
;;                     (data (peek-pdu packet))
;;                     (sz (size data))
;;                     (seq (sequence-number h))
;;                     (next-rx-seq (next-rx-seq tcp)))
;;                (cond
;;                  ((< (+ seq sz) next-rx-seq)
;;                   ;; nothing to deliver
;;                   (setf packet nil))
;;                  ((< seq next-rx-seq)
;;                   ;; remove already delivered data
;;                   (remove-data (- next-rx-seq seq) data))
;;                  ((/= seq next-rx-seq)
;;                   (error "Next rx failure, first ~D, next rx seq ~D"
;;                          seq  next-rx-seq)))
;;                (incf (bytes-received tcp) sz)
;;                (when (and application packet)
;;                  (receive application packet tcp seq))
;;                (incf (next-rx-seq tcp) sz)
;;                (when (or (pending-close tcp) (> original-state established))
;;                  ;; se if can close now
;;                  (unless (buffered-data tcp)
;;                    (process-action peer-close tcp)))))))
;;         ((>= (sequence-number header) (next-rx-seq tcp))
;;          ;; Received seq is > expected, just re-ack previous and buffer data
;;          (push packet (buffered-data tcp))))
;;       (send-ack (next-rx-seq tcp) tcp))))

;; (defun retransmit(tcp)
;;   "Retransmit the oldest pending packet"
;;   (let ((pending-data (pending-data tcp)))
;;     (if pending-data
;;         (let* ((packet (make-instance 'packet))
;;                (data (copy-from-seq (seg-size tcp)
;;                                     (first-pending-seq tcp)
;;                                     (highest-rx-ack tcp)
;;                                     pending-data))
;;                (sz (size data))
;;                (flags 0))
;;           (unless (> sz 0) (error "Retx without pending data"))
;;           (let ((remaining-data
;;                  (size-from-seq (first-pending-seq tcp)
;;                                 (+ (next-tx-seq tcp) sz)
;;                                 pending-data)))
;;             (when (and (close-on-empty tcp) (= 0 remaining-data))
;;               (setf flags (logand flags fin)))
;;             (push-pdu data packet)
;;             (note-time-seq log-seq-tx (highest-rx-ack tcp) tcp)
;;             (send-packet flags (highest-rx-ack tcp) (next-rx-seq tcp) tcp
;;                          :packet packet)))
;;         (send-packet
;;          fin (next-tx-seq tcp) (next-rx-seq tcp) tcp))))  ;; resend lost FIN


;; ;;;; ------------------------------------------------


;; ;;; fill in state machine




;; (defvar *default-tcp-variant* 'tcp-tahoe "Class for default TCP variant")

;; (defmethod make-instance((tcp (eql 'tcp)) &rest rest)
;;   (apply #'make-instance (cons *default-tcp-variant* rest)))


;; (defun protocol.layer3:kill-pending-connection(icmp ipv4-header layer4-header)
;;   (when (typep layer4-header 'tcp:tcp-header) ;; if tcp
;;     (let ((tcp (lookup-by-port
;;                 node (protocol-number layer4-header)
;;                 :local-port (src-port layer4-header)
;;                 :local-address (src-address ipv4-header)
;;                 :peer-port (dst-port layer4-header)
;;                 :peer-address (dst-address ipv4-header))))
;;       (when tcp (tcp:abort tcp)))))


