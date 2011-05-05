;; TCP base implementation
;; Copyright (C) 2011 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;; This file is part of Lisp Educational Network Simulator (LENS)

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:

;; This provides the basic TCP protocol interface and common implementation
;; specific varieties need to implement newack dupack retx-timerevent
;; The same class is used for listener and responder - the listener copies
;; itself on a new connection filling in the peer information etc

;; See Tanenbaum "Computer Networks" (TCN) section 6.5.
;;; Code:

(defpackage :protocol.layer4.tcp
  (:documentation "package for TCP class implementations")
  (:nicknames :tcp)
  (:use :cl :common :protocol.layer4 :address :protocol.layer4.rtt)
  (:import-from :protocol
                #:protocol-number #:send #:receive #:drop #:control-message)
  (:import-from :packet
                #:packet #:pop-pdu #:push-pdu #:peek-pdu #:trace-format #:fid)
  (:import-from :alg
                #:enqueue #:dequeue #:make-queue #:empty-p
                #:make-binary-heap #:binary-heap #:peek)
  (:import-from :node
                #:node #:interfaces #:find-interface)
  (:import-from :scheduler
                #:time-type #:cancel #:timer #:schedule #:timers
                #:simulation-time #:timeout #:with-timers)
  (:import-from :protocol.layer5
                #:data #:data-concatenate #:data-subseq)
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
         (when (logtest (symbol-value flag) flags)
           (write-string (string flag) os))
         (if first (setf first nil) (write-char #\| os)))
       (when first (write 0 :stream os))))
    (t (format os "~2,'0X" flags))))

(defclass tcp-header(pdu)
  ((src-port
    :type (unsigned-byte 16) :initarg :src-port :reader src-port)
   (dst-port
    :type (unsigned-byte 16) :initarg :dst-port :reader dst-port)
   (sequence-number
    :type (unsigned-byte 32) :initarg :sequence-number :reader sequence-number)
   (ack-number
    :type (unsigned-byte 32) :initarg :ack-number :initform 0
    :reader ack-number)
   (header-length
    :type (unsigned-byte 8) :initarg :header-length :initform 0
    :reader header-length)
   (flags
    :type (unsigned-byte 8) :initarg :flags :initform 0 :reader flags)
   (window
    :type (unsigned-byte 16) :initarg :window :initform 0 :reader window)
   (checksum
    :type (unsigned-byte 16) :initarg :checksum :accessor checksum)
   (urgent-pointer
    :type (unsigned-byte 16) :initarg :urgent-pointer :initform 0
    :reader urgent-pointer)
   (data-length
    :initarg :data-length :initform 0 :type integer :reader data-length
    :documentation "not part of TCP but used for tracing")
   (cwnd
    :initarg :cwnd :initform 0 :type integer :reader cwnd
    :documentation "Not part of TCP header but used for simulation analysis"))
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


;; define TCP state machine

;; states are integers indexing first index in the state table
;; events are integers indexing second index in state table
;; state table returns a new state and an action - a function
;; which takes the event, the tcp protocol entity,
;; and optionally a received packet, header and destination address as keywords

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
                      (action  (third event-row)))
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

(defun process-event(event tcp)
  (let ((old-state (state tcp)))
    (multiple-value-bind(new-state action) (lookup-state old-state event)
      (setf (state tcp) new-state)
      (cond
        ((and (= new-state timed-wait) (/= old-state timed-wait))
         (schedule 'timed-wait-timer tcp))
        ((and (= new-state closed)
              (/= old-state closed)
              (/= event timeout)
              (bound-p tcp))
         ;; finally closed so notify application
         (connection-closed (application tcp) tcp)))
      action)))

(defclass tcp(protocol timers-manager copyable-object)
  ((pending-data :initform nil :accessor pending-data
                 :documentation "Data sent by application but not acknowledged")
   (buffered-data
    :initform (make-binary-heap
               :key-fn  #'(lambda(packet) (sequence-number (peek-pdu packet)))
               :comp-fn #'seq<
               :element-type packet)
    :type binary-heap  :accessor buffered-data
    :documentation "data received but out of sequence")
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
   (next-tx-seq :initform (random #xFFFFFFFF) :type (unsigned-byte 32)
                :documentation "Next sequence to send" :accessor next-tx-seq)
   (high-tx-mark :initform 0 :type (unsigned-byte 32) :accessor high-tx-mark
                 :documentation "TX High water mark, for counting retx")
   (highest-rx-ack :initform 0 :type (unsigned-byte 32)
                   :accessor highest-rx-ack
                   :documentation "largest ack received")
   (last-rx-ack :initform 0 :type (unsigned-byte 32) :accessor last-rx-ack
                :documentation "For dupack testing")
   (fast-recovery-mark :initform 0 :type (unsigned-byte 32)
                       :accessor fast-recovery-mark
                       :documentation "Mark for fast recovery")
   (next-rx-seq  :type (unsigned-byte 32) :accessor next-rx-seq
                :documentation"Next expected sequence number")
   (next-ack-seq :type (unsigned-byte 32) :accessor next-ack-seq
                 :documentation "Set when using delayed acks")

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
   (maximum-segment-size
    :type (unsigned-byte 16)
    :initform 536 ;; this is usual size for IP networks (576-40)
    :initarg :mss :initarg :maximum-segment-size
    :reader maximum-segment-size
    :documentation "Maximum Segment Size to use")
   (rx-window :initform #xFFFF :type (unsigned-byte 16) :accessor rx-window
            :documentation "Window as received from peer")
   (advertised-window
    :initform #xFFFF :type (unsigned-byte 16) :accessor advertised-window
    :documentation "Window to advertise to peer")
   (congestion-window :initform 536
                      :type (unsigned-byte 16) :accessor congestion-window
                      :documentation "Congestion window")
   (slow-start-threshold :initform #xFFFF :type integer
                         :accessor slow-start-threshold)

   ;; TCP state
   (state :initform closed :accessor state :type tcp-states
          :documentation "Current state")
   (close-on-empty :initform nil :type boolean :accessor close-on-empty
                   :documentation "True if send FIN pkt on empty data")
   (pending-close :initform nil :accessor pending-close :type boolean
                   :documentation "True if close pending")
   (close-notified :initform nil :accessor close-notified :type boolean
                   :documentation "True if close has been notified")
   (need-ack :initform nil :type boolean :accessor need-ack
             :documentation "True if need ACK bit on next transmit")

   ;; ;;Buffer limits
   ;; (tx-buffer :initform *default-tx-buffer* :accessor tx-buffer :type integer
   ;;            :documentation "Size of tx buffer")
   ;; (rx-buffer :initform *default-rx-buffer* :accessor rx-buffer :type integer
   ;;            :documentation "Size of rx buffer")
   ;; Statistics


   (total-ack :initform 0 :accessor total-ack :type integer
              :documentation "Total bytes acked")

   (initargs :type list
             :documentation "Arguments passed during initialisation - used during reset")
)
  (:documentation "A model of the Transmission Control Protocol."))

(defmethod initialize-instance :after ((tcp tcp) &rest initargs
                                       &key &allow-other-keys)
  (setf (slot-value tcp 'initargs) initargs))

(defmethod reset((tcp tcp))
  (call-next-method)
  (let ((initargs (slot-value tcp 'initargs)))
    (dolist(slot (closer-mop:class-slots tcp))
      (when (eql :instance (closer-mop:slot-definition-allocation slot))
        (slot-makunbound tcp (closer-mop:slot-definition-name slot))))
    (apply #'shared-initialize tcp t initargs)
    (setf (slot-value tcp 'initargs) initargs)))

(defmethod protocol-number((tcp tcp)) 6)

(defmethod default-trace-detail((protocol tcp))
  `(type src-port dst-port sequence-number ack-number flags))

(defmethod schedule((timer (eql 'last-ack-timer)) (tcp tcp))
  (schedule (retransmit-timeout (rtt tcp))
            (timer 'last-ack-timer tcp)))

(defmethod schedule((timer (eql 'retransmit-packet-timeout)) (tcp tcp))
  (schedule (retransmit-timeout (rtt tcp))
            (timer 'retransmit-packet-timer tcp)))

(defmethod receive((tcp tcp) (packet packet) (layer4 layer3:ipv4)
                   &key dst-address &allow-other-keys)
  (setf (last-rx-time tcp) (simulation-time))
  (incf (pkt-received-count tcp))
  (let ((header (peek-pdu packet))
        (old-state (state tcp)))
    ;; Check if ACK bit set, and if so notify rtt estimator
    (when (logtest  ack (flags header))
      (rtt-ack-seq (ack-number header) (rtt tcp))
      ;; Also insure that connection timer is canceled,
      ;; as this may be the final part of 3-way handshake
      (cancel 'connection-timer tcp))

    ;; Check syn/ack and update rtt if so
    (when (and (= old-state syn-sent) (= (flags header) (logior syn ack)))
      (math:record (- (simulation-time) (syn-time tcp)) (rtt tcp)))

    ;; determine event type and process
    (let* ((event (flags-event (flags header)))
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
      (funcall action tcp
               :packet packet :header header :dst-address dst-address)
      (when (and (= state last-ack)
                 (not (busy-p  (timer 'last-ack-timer tcp))))
        (schedule 'last-ack-timer tcp)))))

(defmethod send((tcp tcp) (data data) application
                &key &allow-other-keys)
  (assert (member (state tcp) '#.(list established syn-sent close-wait)))
  (with-accessors((pending-data pending-data)) tcp
    (setf pending-data
          (if (zerop (length-bytes pending-data))
              data
              (data-concatenate pending-data data))))
  (funcall (process-event app-send tcp) tcp)
  (length-bytes data))

(defmethod open-connection(peer-address peer-port (tcp tcp))
  (call-next-method)
  (unless (bound-p tcp) (bind tcp))
  (setf (connection-retry-count tcp) 0)
  (let ((state (state tcp))
        (action (process-event app-connect tcp)))
    (assert (= action syn-tx)
            (action)
            "Bad connect action ~A from ~A state" action state)
    (syn-tx tcp)))

(defmethod close-connection((tcp tcp))
  (with-accessors((state state)) tcp
    (unless (= state closed)
      (let ((action (process-event app-close tcp))
            (pending-data (pending-data tcp)))
        (when (and pending-data (/= 0 (length-bytes pending-data)))
           (when (= action fin-tx)
             (setf (close-on-empty tcp) t)
             (return-from close-connection t)))
        (funcall action tcp)
        (when (and (= state last-ack)
                   (not (busy-p (timer 'last-ack-timer tcp))))
          (schedule 'last-ack-timer tcp))))))

(defmethod connection-complete :around (application (tcp tcp) &key failure)
  (declare (ignore failure))
  (setf (close-notified tcp) nil)
  (call-next-method))

(defmethod connection-closed :around (application (tcp tcp))
  (unless (close-notified tcp)
    (call-next-method)
    (setf (close-notified tcp) t)))

(defmethod connection-closed :after (application (protocol with-timers))
  (cancel :all protocol))

(defmethod timeout((timer (eql 'connection-timer)) (tcp tcp))
  (with-accessors((connection-retry-count connection-retry-count)
                  (max-connection-retry-count max-connection-retry-count)
                  (application application)) tcp
    (if (= (state tcp) syn-sent)
        ;; if active side of connection attempt
        (cond
          ((>= (incf connection-retry-count) max-connection-retry-count)
           ;; if exceeded max number connections so timeout report failure
           (funcall (process-event timeout tcp) tcp)
           (incf (timeout-count tcp))
           (control-message application :timeout tcp))
          (t ;; else try again
           (funcall (process-event app-connect tcp) tcp)))
        ;; passive side of connection (listener) -
        ;; notify application of missing ACK on 3-way handshake
        (control-message application :timeout tcp))))

;; main method must be implemented in concrete classes
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
      (funcall (process-event timeout tcp) tcp))
  (connection-closed (application tcp) tcp))

(defmethod timeout :after (timer (tcp tcp))
  (when (and (= (state tcp) last-ack)
             (not (busy-p (timer 'last-ack-timer tcp))))
    (schedule 'last-ack-timer tcp)))

;; tcp specific functions

(defmethod bind :after((tcp tcp) &key &allow-other-keys)
  ;; if no peer defined this is a listener
  (unless (connected-p tcp)
    (funcall (process-event app-listen tcp) tcp)))

(defun un-ack-data-count(tcp) (- (next-tx-seq tcp) (highest-rx-ack tcp)))

(defun bytes-in-flight(tcp) (- (high-tx-mark tcp) (highest-rx-ack tcp)))

(defun window(tcp) (min (rx-win tcp) (congestion-window tcp)))

(defun available-window(tcp)
  (let ((unack (un-ack-data-count tcp))
        (win (window tcp)))
    (if (< win unack) 0 (- win unack))))

(defun new-congestion-window(tcp ackBytes)
  (declare (ignore ackBytes))
  ;; Adjust congestion window in response to new ack's received
  (with-accessors((congestion-window congestion-window)
                  (slow-start-threshold slow-start-threshold)
                  (maximum-segment-size maximum-segment-size)) tcp
      (incf congestion-window
            (if (< congestion-window  slow-start-threshold)
                ;; Slow start mode, add one segSize to cWnd
                maximum-segment-size
                ;; Congestion avoidance mode,
                ;; adjust by (ackBytes*segSize) / cWnd as per  RFC2581
                ;; changed to match RFC2581
                (max 1 (/ (* maximum-segment-size maximum-segment-size)
                          congestion-window))))))

;; TCP actions
(defun no-act(tcp &key &allow-other-keys)
  (declare (ignore tcp)))

(defun ack-tx(tcp &key header &allow-other-keys)
  (send-packet tcp ack (next-tx-seq tcp) (next-rx-seq tcp))
  ;; save receiver advertised window
  (when header (setf (rx-window tcp) (window header))))

(defun ack-tx-1(tcp &key header &allow-other-keys)
  ;; save receiver advertised window
  (when header (setf (rx-window tcp) (window header)))
  (cancel 'connection-timer tcp)
  ;; notify application
  (connection-complete (application tcp) tcp)
  ;; if no data need to send ack
  (unless (pending-data tcp)
    (send-packet tcp ack (next-tx-seq tcp) (next-rx-seq tcp))))

(defun rst-tx(tcp &key header dst-address &allow-other-keys)
  (send-packet tcp rst 0 0
               :dst-address dst-address
               :dst-port  (if header (src-port header) (peer-port tcp))))

(defun syn-tx(tcp &key &allow-other-keys)
  (schedule 'connection-timer tcp)
  (setf (syn-time tcp) (simulation-time)) ;; for initial RTT estimate
  (send-packet tcp SYN 0 0))

(defun respond(tcp peer-address peer-port)
  "This tcp is a copy of a listening tcp. The SYN packet was received
   by the listener, and it created a copy of itself.  This copy
   will bind to the local port/ip, remote port/ip and respond to all
  future packets."
  (setf (state tcp) syn-rcvd)
  (bind tcp :peer-address peer-address :peer-port peer-port)
  (syn-ack-tx tcp))

(defun reject(tcp peer-address peer-port)
  "Application not accepting more connections"
  (send-packet tcp RST 0 0 :dst-address peer-address :dst-port peer-port))

(defun syn-ack-tx(tcp &key header dst-address &allow-other-keys)
  (cond
    ((and header (= (state tcp) listen))
    ;; create clone of listener for connection
     (let ((copy (copy tcp)))
       (if (connection-from-peer (application copy) copy)
           (respond copy dst-address (src-port header))
           (reject  copy dst-address (src-port header)))))
    (t
     ;; this is copy - just respond - but timeout in case of syn-flood attacker
     (send-packet tcp (logior syn ack) 0 0)
     (schedule 'connection-timeout tcp))))

(defun fin-tx(tcp &key &allow-other-keys)
  (send-packet tcp fin (next-tx-seq tcp) (next-rx-seq tcp)))

(defun fin-ack-tx(tcp &key &allow-other-keys)
  (send-packet tcp (logior fin ack) (next-tx-seq tcp) (next-rx-seq tcp))
  (when (= (state tcp) last-ack)
    (schedule 'last-ack-timer tcp)))

(defun new-seq-rx(tcp &key packet &allow-other-keys)
  ;; data received
  (new-rx tcp  packet))

(defun retx(tcp &key &allow-other-keys)
  (declare (ignore tcp)))

(defun tx-data(tcp  &key &allow-other-keys)
  (send-pending-data tcp))

(defun peer-close(tcp &key packet header &allow-other-keys)
  ;; make sure FIN packet not out of sequence.
  (when (and header (/= (sequence-number header) (next-rx-seq tcp)))
    ;; process close later
     (setf (pending-close tcp) t)
     (new-rx tcp  packet)
     (return-from peer-close t))
   (when (let ((d (peek-pdu packet 1)))
           (and d (> (length-bytes d) 0)))
     ;; data came wqith fin so new rx
     (new-rx tcp packet))
   (with-accessors((state state)) tcp
     (let ((saved-state state))
       (close-request (application tcp) tcp)
       (when (= state saved-state)
       ;; need to ack - application will close later
         (send-packet tcp ack (next-tx-seq tcp) (next-rx-seq tcp))))
     (when (= state last-ack)
       (schedule 'last-ack-timer tcp))))

(defun app-closed(tcp &key &allow-other-keys)
  (when (and (= (state tcp) syn-sent) (not (close-notified tcp)))
    (setf (close-notified tcp) t)
    (connection-closed (application tcp) tcp)))

(defun app-notify(tcp &key &allow-other-keys)
  (control-message (application tcp) :connection-failed tcp))

(defun cancel-tm(tcp &key &allow-other-keys)
  (cancel :all tcp))

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
            (setf (next-ack-seq tcp) ack-number)
            (setf send-it nil))))
    (or
     (send-pending-data tcp t)
     (if send-it (send-packet tcp ack (next-tx-seq tcp) ack-number)))))

(defun send-packet(tcp flags sequence-number ack-number &key
                   data
                   (dst-address (peer-address tcp))
                   (dst-port (peer-port tcp))
                   (ipv4 (layer3:find-protocol 'layer3:ipv4 (node tcp))))
  "Send a TCP packet"
  (let ((packet (make-instance 'packet :data data :fid (fid tcp))))
    (assert (<= (length-bytes data) (maximum-segment-size tcp)))
    (when data ;; if data
      (unless (busy-p (timer 'retransmit-packet-timer tcp))
        (schedule 'retransmit-packet-timer tcp)
        (rtt-sent-seq sequence-number (length-bytes packet) (rtt tcp))
        (when (< sequence-number (high-tx-mark tcp))
          (incf (retransmit-count tcp)))))
    (push-pdu
     (make-instance
      'tcp-header
      :src-port (local-port tcp)
      :dst-port dst-port
      :seq sequence-number
      :ack ack-number
      :flags (logior flags
                     (if  (need-ack tcp) ack 0)
                     (if (and (or (= state fin-wait-1)
                                  (= state fin-wait-2))
                              (zerop (length-bytes (pending-data tcp))))
                         fin
                         0))
      :window (advertised-window tcp)
      :data-length (length-bytes packet)
      :cwnd (congestion-window tcp))
     packet)
    (setf (need-ack tcp) nil)
    (send ipv4 packet tcp
           :src-address (or (local-address tcp) (network-address (node tcp)))
           :dst-address dst-address
           :ttl (ttl tcp)
           :tos (tos tcp))
    (incf (pkt-sent-count tcp))))

(defun send-pending-data(tcp &optional with-ack)
  "Send as much pending data as necessary/possible"
  (let ((n-sent 0)
        (pending-data (pending-data tcp))
        (ipv4 (layer3:find-protocol 'layer3:ipv4 (node tcp)))
        (interface (find-interface (peer-address tcp) (node udp))))
    (loop
       ;; if no data finished
       (when (zerop (length-bytes pending-data))
         (setf (pending-data tcp) nil)
         (return))
       (let ((w (available-window tcp))
             (mss (maximum-segment-size tcp))
             (data
                 (data-subseq pending-data
                              (seq- (next-tx-seq tcp) (highest-rx-ack tcp))
                              (min w mss))))
         ;; don't send small segment unnecessarily
         (when (and  (< w mss) (> (length-bytes data) w))
           (return))
         ;; if no buffer available stop and request
         ;;(100 extra bytes is to allow for headers)
         (unless (layer1:buffer-available-p
                  (+ 100 (length-bytes data)) interface)
           (add-notification udp #'send-pending-data interface)
           (return))
         (let ((flags (if with-ack ack 0)))
           ;; see if we need fin flag
           (when (and (close-on-empty tcp)
                      (zerop (length-bytes pending-data)))
             (setf flags (logior flags fin))
             (setf (state tcp) fin-wait-1))
           (send-packet tcp flags (next-tx-seq tcp) (next-rx-seq tcp)
                        :data data :ipv4 ipv4)
           (incf n-sent)
           (setf (next-tx-seq tcp) (seq+ (next-tx-seq tcp) (length-bytes data)))
           (setf (high-tx-mark tcp) (max (next-tx-seq tcp) (high-tx-mark tcp)))
           (incf (bytes-sent tcp) (length-bytes data)))))
    (when (> n-sent 0)
      (schedule 'retransmit-packet tcp)
      t)))

(defmethod control-message((tcp tcp) (msg (eql :destination-unreachable))
                            ipv4
                            &key packet &allow-other-keys)
  "Called by ICMP when unreachable response is received"
  (when (= (state tcp) syn-sent)
    (funcall (process-event timeout tcp) tcp :packet packet))
  (control-message (application tcp) msg tcp))

(defgeneric rx-ack(tcp ack-number &optional skip-timer)
  (:documentation "Ack management")
  (:method(tcp ack-number  &optional skip-timer)
    (unless skip-timer (cancel 'retransmit-packet-timer tcp))
    (with-accessors((highest-rx-ack highest-rx-ack)
                    (next-tx-seq next-tx-seq)
                    (pending-data pending-data)
                    (total-ack total-ack)
                    (first-pending-seq first-pending-seq)
                    (state state)) tcp
      (let ((number-ack (- ack-number highest-rx-ack)))
        (incf total-ack number-ack)
        (setf highest-rx-ack ack-number)
        (when (> ack-number next-tx-seq)
          (setf next-tx-seq ack-number))
        ;; delete ack'd pending data
        (setf pending-data
              (data-subseq pending-data number-ack))
        ;; All pending acked, can be deleted
        (cancel 'retransmit-packet-timer tcp)
    ;; Notify application of data sent
        (when (> number-ack 0) (send (application tcp) number-ack tcp)))
    ;; Try to send more data
    (send-pending-data tcp)
    ;; See if we need to post a re-tx timer
    (when (and (not skip-timer)
               (not (busy-p (timer 'retransmit-packet-timer tcp)))
               (or (> (unack-data-count tcp) 0)
                   (= state fin-wait-1)
                   (= state fin-wait-2)))
      (schedule 'retransmit-packet-timer tcp)))))

(defgeneric dup-ack(tcp count)
  (:documentation "Implement for duplicate acks"))

(defun new-ack(tcp &key header packet &allow-other-keys)
  (let ((highest-rx-ack (highest-rx-ack tcp))
        (ack-number (ack-number header)))
    (cond
      ((< highest-rx-ack ack-number)
       ;; old ack, no action
       )
      ((and (=  highest-rx-ack ack-number) (< ack-number (next-tx-seq tcp)))
       ;; duplicate ack received
       (dup-ack tcp (incf (dup-ack-count tcp))))
      (t
       (when (> ack-number (highest-rx-ack tcp))
         (setf (dup-ack-count tcp) 0))
       (rx-ack tcp ack-number)
       (new-rx tcp packet))))) ;; in case any data

(defmethod receive (application (packet packet) (tcp tcp) &key &allow-other-keys)
  (let ((header (pop-pdu packet))
        (data (pop-pdu packet)))
    (with-slots(next-rx-seq) tcp
      (unless (= next-rx-seq (sequence-number header))
        (setf data
              (data-subseq data (seq- (sequence-number header) next-rx-seq))))
      (let ((s (length-bytes data)))
        (setf next-rx-seq (seq+ next-rx-seq s))
        (incf (bytes-received tcp) s)
        (receive application data tcp)))))

(defun new-rx(tcp packet)
  "New received data - packet should have tcp header in case we buffer it"
  (let* ((original-state (state tcp))
         (header (peek-pdu packet))
         (s (length-bytes (peek-pdu packet 1))))
    (when (> s 0)
      ;;Note we need the ACK bit on next transmitted pkt
      (setf (need-ack tcp) t)
      (with-accessors((next-rx-seq next-rx-seq)
                      (buffered-data buffered-data)) tcp
        (cond
          ((= (sequence-number header) next-rx-seq)
           ;;  Received seq is expected, deliver this and any buffered data
           (receive (application tcp) packet tcp)
           (when (close-notified tcp)
             (error "TCP ~A got data after close-notified" tcp))
           ;; look for buffered data
           (loop
              :while (not (empty-p buffered-data))
              :until (> (sequence-number (peek-pdu (peek buffered-data)))
                        next-rx-seq)
              :do (receive (application tcp) (dequeue buffered-data) tcp))
           ;; see if can close now
           (when (and (or (pending-close tcp) (> original-state established))
                      (empty-p buffered-data))
             (peer-close tcp)))
          ((> (sequence-number header) next-rx-seq)
           ;; buffer this packet
           (enqueue packet buffered-data)))
        (send-ack tcp next-rx-seq)))))

(defun retransmit(tcp)
  "Retransmit the oldest pending packet"
  (with-accessors ((pending-data pending-data)
                   (state state)
                   (next-tx-seq next-tx-seq)
                   (highest-rx-ack highest-rx-ack)
                   (next-rx-seq next-rx-seq)) tcp
    (cond
      ((zerop (length-bytes pending-data))
       (assert (or (= state fin-wait-1 fin-wait-2)))
       ;; must have lost fin - retransmit
       (send-packet tcp fin next-tx-seq next-rx-seq))
      (t
       (let* ((data (data-subseq pending-data 0 (maximum-segment-size tcp)))
              (remaining-bytes (- (length-bytes pending-data)
                                  (length-bytes data))))
          (send-packet tcp
                       (if (and (close-on-empty tcp) (zerop remaining-bytes))
                           fin
                           0)
                       highest-rx-ack
                       next-rx-seq))))))

(defclass tcp-reno(tcp)
  ()
  (:documentation "Tahoe tcp implementation"))
