;; TCP base implementation
;; Copyright (C) 2007 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;; This file is part of Lisp Educational Network Simulator (LENS)

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:

;; This provides the basic TCP protocol interface and common implementation
;; specific varieties need to implement newack dupack retx-timeout-event
;; The same class is used for listener and responder - the listener copies
;; itself on a new connection filling in the peer information etc

;;; Code:

(defpackage :protocol.tcp
  (:nicknames :tcp)
  (:use :cl :common :address :protocol.layer4)
  (:shadowing-import-from :packet #:size)
  (:shadow #:listen #:abort)
  (:import-from :scheduler
                #:simulation-time #:schedule #:cancel
                #:time-type #:*time-format* #:timers-manager #:schedule-timer
                #:cancel-timer #:find-timer #:cancel-all-timers)
  (:import-from :packet #:packet #:pop-pdu #:push-pdu #:peek-pdu)
  (:import-from :ipv4 #:ipv4 #:ipv4-demux #:src-port #:dst-port)
  (:import-from :layer5
                #:data #:copy-from-seq #:size-from-seq
                #:remove-data #:add-data)
  (:import-from :layer4 #:buffer-available-p)
  (:import-from :application
                #:receive #:sent #:close-request #:closed #:connection-complete
                #:connection-failed #:connection-from-peer)
  (:import-from :protocol #:layer )
  (:import-from :lens.math
                #:ack-seq #:sent-seq #:retransmit-timeout #:rtt-mdev
                #:estimate #:record #:rtt-estimator)
  (:import-from :trace
                #:eol #:pdu-trace #:write-trace #:write-pdu-slots
                #:default-trace-detail)
  (:export #:tcp #:tcp-header #:abort #:listen #:*default-tcp-variant*
           #:close-on-empty))


(in-package :protocol.tcp)

;;; define TCP flags
(defconstant fin #x01)
(defconstant syn #x02)
(defconstant rst #x04)
(defconstant psh #x08)
(defconstant ack #x10)
(defconstant urg #x20)
(defconstant max-flag #x40)

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

(defenumeration tcp-states
    (closed listen syn-sent syn-rcvd established close-wait
     last-ack fin-wait-1 fin-wait-2 closing timed-wait last-state ))

(defenumeration tcp-events
    (app-listen app-connect app-send seq-recv app-close timeout ack-rx
     syn-rx syn-ack-rx fin-rx fin-ack-rx rst-rx bad-flags last-event ))

(defenumeration tcp-actions
    (no-act ack-tx ack-tx-1 rst-tx syn-tx syn-ack-tx fin-tx fin-ack-tx
     new-ack new-seq-rx retx tx-data peer-close app-closed cancel-tm
     app-notify serv-notify last-action))

(defparameter *state-table*
  (make-array (list last-state last-event) :element-type 'cons)
  "The TCP state table")
(defparameter *flag-events*
  (make-array max-flag :initial-element bad-flags
              :element-type '(unsigned-byte 8))
  "Mapping between flags and events")

(declaim (inline lookup-state flags-event))
(defun lookup-state(state event)
  "Given a current state and event return the new state and an action"
  (values-list (aref *state-table* state event)))
(defun flags-event(flags) (aref *flag-events* flags))

(declaim (inline flag-p))
(defun flag-set-p(flag datum)
  (not (zerop (logand flag datum))))

;; default tcp parameters
(defvar *default-seg-size* 512 "Segment Size")
(defvar *default-adv-win* #xFFFF "Advertised window")
(defvar *default-ss-thresh* #xFFFF "Slow-start threshold")
(defvar *default-tx-buffer* most-positive-fixnum)
(defvar *default-rx-buffer* most-positive-fixnum)
(defvar *default-tw-timeout* 5 "Default timeout in seconds")
(defvar *default-conn-timeout* 6 "Default timeout in seconds")
(defvar *default-del-ack-timeout* 0 "Default to not used")
(defvar *default-initial-cwnd* 1 "Default initial cwnd in segments")
(defvar *tcp-flags-format* :text "Format for logging TCP flags")
(defvar *default-conn-count* 3 "Number of connection retries")
(defvar *default-tcp* 'tcp "Default tcp variant")

(defclass tcp-header(pdu)
  ((src-port  :initarg :src-port :initform 0 :type ipport :reader src-port)
   (dst-port  :initarg :dst-port :initform 0 :type ipport :reader dst-port)
   (sequence-number :initarg :sequence-number :initform 0 :type seq
                    :reader sequence-number)
   (ack-number :initarg :ack-number :initform 0 :type seq :reader ack-number)
   (header-length :initarg :header-length :initform 0 :type octet
                  :reader header-length)
   (flags :initarg :flags :initform 0 :type octet :reader flags)
   (window :initarg :window :initform 0 :type counter :reader window)
   (checksum  :initarg :checksum :initform 0 :type word :accessor checksum)
   (urgent-pointer :initarg :urgent-pointer :initform 0 :type word
                   :reader urgent-pointer)
   (fid  :initarg :fid :initform 0 :type fid :accessor fid
        :documentation "Flow id's are not part of TCP, but useful for tracing")
   (data-length :initarg :data-length :initform 0 :type counter
                :reader data-length
                :documentation "Also not part of TCP but used for tracing")
   (cwnd :initarg :cwnd :initform 0 :type counter :reader cwnd
         :documentation
         "Not part of TCP header but used for simulation analysis"))
  (:documentation "TCP PDU class"))

(defmethod protocol-number((h tcp-header)) 6)

(defmethod size((h tcp-header)) 20)

(defmethod copy((h tcp-header))
  (copy-with-slots h '(src-port dst-port sequence-number ack-number
                       header-length flags window checksum urgent-pointer
                       fid data-length cwnd)))

(defmethod pdu-trace((pdu tcp-header) detail os &key packet text)
  (declare (ignore packet))
  (format os " ~@[~A~] L4-TCP" text)
  (write-pdu-slots
   pdu '(src-port dst-port sequence-number ack-number header-length)
   detail os)
  (when (member 'flags detail)
    (let ((flags (flags pdu)))
      (write-char #\space os)
      (cond
        ((numberp *tcp-flags-format*)
         (write flags :stream os :base *tcp-flags-format*))
        ((eql *tcp-flags-format* :text)
         (let ((first t))
           (dolist(flag '(fin syn rst psh ack urg))
             (when (flag-set-p (symbol-value flag) flags)
               (write-string (string flag) os))
             (if first (setf first nil) (write-char #\| os)))
           (when first (write 0 :stream os))))
        (t (write flags :stream os :base 16)))))
  (write-pdu-slots
   pdu '(window (checksum "~4,'0X") urgent fid data-length cwnd)
   detail os))

(defclass tcp(protocol timers-manager)
  ((protocol-number :initform 6 :reader protocol-number :allocation :class)
   ;; history data
   (buffered-data :initform nil :type list :accessor buffered-data
                  :documentation "Received but out of sequence")
   (pending-data :initform nil :type data
                 :accessor pending-data
                 :documentation "Data sent by application but not sent")
   (first-pending-seq :initform 0 :type seq :accessor first-pending-seq
                      :documentation "First sequence number in pending data")
   ;; sequence information - sender side
   (next-tx-seq :initform 0 :type seq :accessor next-tx-seq
                :documentation "Next sequence to send")
   (high-tx-mark :initform 0 :type seq :accessor high-tx-mark
                 :documentation "TX High water mark, for counting retx")
   (highest-rx-ack :initform 0 :type seq :accessor highest-rx-ack
                   :documentation "largest ack received")
   (last-rx-ack :initform 0 :type seq :accessor last-rx-ack
                :documentation "For dupack testing")
   (fast-recovery-mark :initform 0 :type seq :accessor fast-recovery-mark
                       :documentation "Mark for fast recovery")
   (dup-ack-count :initform 0 :type counter :accessor dup-ack-count
                  :documentation "Number of dup acks in a row")
   (fast-recovery-p :initform nil :type boolean :accessor fast-recovery-p
                    :documentation "True if fast recovery in progress")
   (need-ack :initform nil :type boolean :accessor need-ack
               :documentation "True if need ACK bit transmit")
   (no-timer-p  :initform nil :type boolean :accessor no-timer-p
                :documentation "True if skip resched of re-tx timer")
   ;; Sequence Information, receiver side
   (next-rx-seq :initform 0 :type seq :accessor next-rx-seq
                :documentation"Next expected sequence")
   (next-ack-seq :initform 0 :type seq :accessor next-ack-seq
                 :documentation "Set non-zero when using delayed acks")
   ;; Window Management
   (seg-size :initform *default-seg-size* :type counter :accessor seg-size
             :documentation "SegmentSize")
   (rx-win :initform *default-adv-win* :type counter :accessor rx-win
           :documentation "Window as received from peer")
   (adv-win :initform *default-adv-win* :type counter :accessor adv-win
            :documentation "Window to advertise to peer")
   (cwnd :initform (* *default-initial-cwnd* *default-seg-size*)
         :type counter :accessor cwnd
         :documentation "Congestion window")
   (ssthresh :initform *default-ss-thresh* :type counter :accessor ssthresh
              :documentation "Slow Start Threshold")
   (initial-cwnd :initform *default-initial-cwnd*
                 :type counter :accessor initial-cwnd
                 :documentation "Initial (and reset) value for cWnd")
   (rtt :initform (make-instance 'rtt-mdev)
        :initarg :rtt :type rtt-estimator :accessor rtt
        :documentation "An RTT estimator")
   (tw-timeout :initform *default-tw-timeout*
               :accessor tw-timeout :type time-type
               :documentation "Timeout period for timed-wait state")
   (conn-timeout :initform *default-conn-timeout*
                 :accessor conn-timeout :type time-type
                 :documentation "Timeout period for connection retry")
   (del-ack-timeout :initform *default-del-ack-timeout*
                    :accessor del-ack-timeout :type time-type
                    :documentation "Timeout period for delayed acks")
   ;; put timers in??
   (retry-count :initform 0 :accessor retry-count :type counter
                :documentation "Limit the re-tx retries")
   (conn-count :initform *default-conn-count*
               :accessor conn-count :type counter
               :documentation "Connection retry count")
   (state :initform closed :accessor state :type (unsigned-byte 8)
          :documentation "Current state")
   ;; state
   (close-on-empty :initform nil :accessor close-on-empty :type boolean
                    :documentation "True if send FIN pkt on empty data")
   (delete-on-complete :initform nil :accessor delete-on-complete :type boolean
                       :documentation "True if should delete object on close")
   (delete-on-timed-wait :initform nil :accessor delete-on-timed-wait
                         :type boolean
                         :documentation "True if delete after timed wait")
   (pending-close :initform nil :accessor pending-close :type boolean
                  :documentation "True if close pending")
   (close-notified :initform nil :accessor close-notified :type boolean
                   :documentation "True if close has been notified")
   (close-req-notified
    :initform nil :accessor close-req-notified :type boolean
    :documentation "True if close request has been notified")
   ;;Buffer limits
   (tx-buffer :initform *default-tx-buffer* :accessor tx-buffer :type counter
              :documentation "Size of tx buffer")
   (rx-buffer :initform *default-rx-buffer* :accessor rx-buffer :type counter
              :documentation "Size of rx buffer")
   ;; Statistics
   (total-ack :initform 0 :accessor total-ack :type counter
              :documentation "Total bytes acked")
   (open-time :initform 0.0 :accessor open-time :type time-type
              :documentation "Time connection was opened")
   (last-rx-time :initform 0.0 :accessor last-rx-time :type time-type
                 :documentation "Time of last pkt receipt")
   (last-ack-time :initform 0.0 :accessor last-ack-time :type time-type
                  :documentation "Time last ack sent")
   (syn-time :initform 0.0 :accessor syn-time :type time-type
             :documentation "Time SYN sent")
   (retransmit-count :initform 0 :accessor retransmit-count :type counter
                     :documentation "Count of retransmits")
   (timeout-count :initform 0 :accessor timeout-count :type counter
                  :documentation "Count of timeouts")
   (pkts-sent :initform 0 :accessor pkts-sent :type counter
              :documentation "Count of packets sent")
   (pkts-received :initform 0 :accessor pkts-received :type counter
                  :documentation "Count of packets received")
   (bytes-sent :initform 0 :accessor bytes-sent :type counter
               :documentation "Total bytes sent")
   (bytes-received :initform 0 :accessor bytes-received :type counter
                   :documentation "Total bytes received")
   (time-seq-stats :initform nil :accessor time-seq-stats :type vector
                   :documentation "Pointer to Array of time/seq stats")
   (parent :initform nil :accessor parent :type tcp
           :documentation "Parent (server) tcp (if exists)")
   (number-children :initform 0 :accessor number-children :type counter
                     :documentation "Number of child TCP's (if server)")
   (child-limit :initform most-positive-fixnum
                :accessor child-limit :type counter
                :documentation "Maximum number of allowed children")
   (number-fin :initform 0 :accessor number-fin :type counter
               :documentation "Number fin packets rx, just testing")
   (next-fid :initform 0 :documentation "Next unique tcp flow id")
   (last-timeout-delay
    :initform 0.0 :accessor last-timeout-delay :type time-type
    :documentation "Timeout delay of last scheduled retx timer")
   (last-measured-rtt :initform 0.0 :accessor last-measured-rtt :type time-type
                      :documentation "Time of last rtt measurement"))
  (:documentation "A  model of the Transmission Control Protocol."))

;; the following 3 generic functions are implemented for different variations
;; of TCP

(defgeneric newack(seq tcp)
  (:documentation "Called by tcp for new (non duplicate acks)"))

(defgeneric dupack(tcp-header count tcp)
  (:documentation "Duplicate Ack received"))

(defgeneric retx-timeout-event(tcp)
  (:documentation "Retransmit timeout"))

(defun next-fid(tcp) (incf (slot-value tcp 'next-flow-id)))


(defmethod reset((tcp tcp))
  (setf (buffered-data tcp) nil)
  (reset (pending-data tcp))
  ;; remove all timeouts from scheduler
  (cancel-all-timers tcp)
  ;; reset rtt calculations
  (reset (rtt tcp))
  ;; initialize specified slots according to initforms
  (shared-initialize
   tcp
   '(next-tx-seq
     high-tx-mark
     highest-rx-ack
     dup-ack-count
     fast-recovery-p
     need-ack
     no-timer-p
     next-rx-seq
     seg-size
     rx-win
     adv-win
     cwnd
     ss-thresh
     initial-cwnd
     tw-timeout
     conn-timeout
     del-ack-timeout
     retry-count
     conn-count
     state
     close-one-empty
     delete-on-complete
     delete-on-timed-wait
     pending-close
     close-notified
     close-req-notified
     tx-buffer
     rx-buffer
     total-ack
     open-time
     last-rx-time
     last-ack-time
     syn-time
     retransmit-count
     timeout-count
     pkts-sent
     pkts-received
     bytes-sent
     bytes-received
     time-seq-stats
     parent
     number-children
     child-limit
     number-fin
     last-timeout-delay
     last-measured-rtt)))

(defmethod copy((original tcp))
  (let ((copy (make-instance (class-of original))))
    ;; copy rtt
    (map 'nil
         #'(lambda(slot)
             (setf (slot-value copy slot) (slot-value original slot)))
         '(seg-size rx-win adv-win cwnd ss-thresh initial-cwnd
           tw-timeout conn-timeout del-ack-timeout
           retry-count conn-count state close-on-empty
           pending-close close-notified close-req-notified
           tx-buffer rx-buffer
           retransmit-count timeout-count
           child-limit
           last-timeout-delay last-measured-rtt))
    (setf (rtt copy)
          (make-instance (class-of (rtt original))
                         :initial-estimate (estimate (rtt original))))
    (when-bind(tss (time-seq-stats original))
              (loop :for i :from 0 :below (length tss)
                    :when (aref tss i) :do (enable-time-seq i copy)))
    copy))

(defmethod default-trace-detail((protocol tcp))
  `(src-port dst-port sequence-number ack-number flags fid))

(defstruct time-seq (time 0.0 :type time-type) (seq 0 :type seq))

(defun note-time-seq(selector seq protocol)
  (when-bind(tss (time-seq-stats protocol))
     (when-bind(tsv (aref tss selector))
       (vector-push-extend
        (make-time-seq :time (simulation-time) :seq seq) tsv))))

(defmethod receive((tcp tcp) node packet dst-address interface)
  (let ((header (peek-pdu packet))
        (state (state tcp)))
    (setf (last-rx-time tcp) (simulation-time))
    (write-trace node tcp header nil :packet packet :text "+")
    ;; Check if ACK bit set, and if so notify rtt estimator
    (when (flag-set-p (flags header) ack)
      (let ((m (ack-seq (ack-number header) (rtt tcp)))) ;; send-seq
        (unless (zerop m) (setf (last-measured-rtt tcp) m))
        (note-time-seq log-ack-rx (ack-number header) tcp)
        ;; Also insure that connection timer is canceled,
        ;; as this may be the final part of 3-way handshake
        (cancel-timer 'conn-timeout-event tcp)))
    ;; Check syn/ack and update rtt if so
    (when (and (= state syn-sent) (= (flags header) (logior syn ack)))
      (record (- (simulation-time) (syn-time tcp)) (rtt tcp)))
    ;; determine event type and process
    (let* ((event (flags-event (flags header)))
           (old-state state)
           (action (process-event event tcp)))
      (cond
        ((and (= state close-wait) (= event seq-recv))
         (error "TCP ~A New seq in close-wait" tcp))
        ((and (= state closed) (= event syn-rx))
         (error "TCP ~A syn in closed l=~A:~A r=~A:~A"
                tcp (ipaddr (node tcp)) (local-port tcp)
                dst-address (src-port header)))
        ((and (= old-state listen) (/= state listen))
         (error "TCP transition from ~A to ~A" old-state state))
        ((and (/= state old-state) (= state timed-wait))
         (unless (close-notified tcp)
           (setf (close-notified tcp) t)
           (when-bind(a (application tcp))
                     (closed a tcp))))
        ((and (= state closed) (/= old-state closed))
         (cancel-all-timers tcp))
        ((= state timed-wait)
         (cancel-all-timers tcp)
         (schedule-timer (tw-timeout tcp) 'tw-timeout-event tcp)))
      (process-action action tcp packet header dst-address)
    (when (and (= state last-ack) (not (zerop (last-ack-time tcp))))
      (schedule-timer (retransmit-timeout (rtt tcp))
                      'last-ack-timeout-event tcp)))))

(defmethod notify((tcp tcp)) (tcp-send-pending tcp))

(defmethod send((tcp tcp) (data data)
                &key dst-address dst-port)
  (declare (ignore dst-address dst-port))
  (unless (member (state tcp) '#.(list established syn-sent close-wait))
    (error "TCP ~A send wrong state ~A" tcp (state tcp)))
  (if (pending-data tcp)
      (data:add-data (or (data:contents data) (size data))  (pending-data tcp))
      (progn
        (setf (first-pending-seq tcp) (next-tx-seq tcp))
        (setf (pending-data tcp) (copy data))))
  (process-action (process-event app-send tcp) tcp)
  (size data))

(defmethod connect((protocol tcp) (peer-address ipaddr) (peer-port integer))
  (unless (local-port protocol) (bind protocol))
  (setf (conn-count protocol) *default-conn-count*)
  (let ((state (state protocol))
        (action (process-event app-connect protocol)))
    (setf (peer-address protocol) peer-address
          (peer-port protocol) peer-port
          (open-time protocol) (simulation-time))
    (if (= action syn-tx)
        (process-action action protocol)
        (error "Bad connect action ~A from ~A state" action state))))

(defmethod close-connection((protocol tcp))
  (unless (= (state protocol) closed)
    (let ((action (process-event app-close protocol)))
      (unless (pending-data protocol)
        (when (= action fin-tx)
          (setf (close-on-empty protocol) t)
          (return-from close-connection t)))
      (prog1
          (process-action action protocol)
        (when (and (= (state protocol) last-ack)
                   (not (last-ack-timeout-event protocol)))
          (schedule-timer (retransmit-timeout (rtt protocol))
                          'last-ack-timeout-event protocol))))))

;; timer events

(defun conn-timeout-event(protocol)
  (let ((application (application protocol)))
    (if (= (state protocol) syn-sent) ;; this is active side of connection
        (if (> (decf (conn-count protocol)) 0)
            (process-action (process-event app-connect protocol) protocol)
            (progn
              (process-action (process-event timeout protocol) protocol)
              (when application (connection-failed application protocol))))
        ;; is passive listener
        (when application (connection-failed application protocol)))))

(defun delayed-ack-timeout-event(protocol)
  (when (= 0 (next-ack-seq protocol))
    (error "DelAck timeout with no pending ack"))
  (send-ack  (next-ack-seq protocol) protocol t)
  (setf  (next-ack-seq protocol) 0))

(defun tw-timeout-event(protocol)
  (cancel-all-timers protocol)
  (cancel-notification protocol)
  (unbind protocol))

(defun last-ack-timeout-event(protocol)
  (when (= (state protocol) last-ack)
    (process-action (process-event timeout protocol) protocol))
  (unless (close-notified protocol)
    (setf (close-notified protocol) t)
    (when-bind(a (application protocol)) (closed a protocol))))

(defun listen(protocol)
  ;; set endpoint to accept connection requests
  (process-action (process-event app-listen protocol) protocol))

(defun child-complete(protocol)
  (if (= 0 (number-children protocol))
      (error "TCP child complete with no children")
      (decf (number-children protocol))))

(defun abort(protocol)
  "Called by ICMP when unreachable response is received"
  (when (= (state protocol) syn-sent)
    (process-action (process-event timeout protocol) protocol)
    (when-bind(a (application protocol)) (connection-failed a protocol))))

(defun respond(peer-address peer-port protocol)
  "This tcp is a copy of a listening tcp. The SYN packet was received
  by the listener, and it created a copy of itself.  This copy
  will bind to the local port/ip, remote port/ip and respond to all
  future packets."
  (setf (state protocol) syn-rcvd
        (peer-address protocol) peer-address
        (peer-port protocol) peer-port)
  (node:bind protocol (node protocol)
             :local-port (local-port protocol)
             :remote-port peer-port
             :remote-address peer-address)
  (process-action syn-ack-tx protocol))

(defun reject(peer-address peer-port tcp)
  "Application no accepting more connections"
  (send-packet rst 0 0 tcp
               :dst-address peer-address :dst-port peer-port))

(defun unack-data-count(protocol)
  "Return count of unacknowledged data bytes"
  (- (next-tx-seq protocol) (highest-rx-ack protocol)))

(defun bytes-in-flight(protocol)
  "Return count of unacknowledged data bytes"
  (- (high-tx-mark protocol) (highest-rx-ack protocol)))

(defmethod window((protocol tcp))
  "Return window size"
  (min (rx-win protocol) (cwnd protocol)))

(defun available-window(protocol)
  "Return unfilled portion of window"
  (let ((unack (unack-data-count protocol))
        (win (window protocol)))
    (if (< win unack) 0 (- win unack))))

(defun new-cwnd(ack-bytes tcp)
  "Adjust congestion window in response to new ack's received"
  (declare (ignore ack-bytes))
  (let ((seg-size (seg-size tcp))
        (cwnd (cwnd tcp)))
    (if (< cwnd (ssthresh tcp))
        (incf (cwnd tcp) seg-size) ;; slow start mode inc by one seg-size
        (let ((adder (/ (*  seg-size seg-size) cwnd)))
          ;; as per RFC 2581
          (incf (cwnd tcp) (if (zerop adder) 1 adder))))
    (note-time-seq log-cwin cwnd tcp)))

(defun enable-time-seq(selector tcp)
  (let ((tss (or (time-seq-stats tcp)
                 (setf (time-seq-stats tcp) (make-array log-last)))))
    (setf (aref tss selector)
          (make-array 10
                      :element-type 'time-seq :fill-pointer 0 :adjustable t))))

(defun disable-time-seq(selector tcp)
  (when-bind(tss (time-seq-stats tcp))
     (setf (aref tss selector) nil)))

(defun reset-time-seq(selector tcp)
  (when-bind(tss (time-seq-stats tcp))
     (setf (fill-pointer (aref tss selector)) 0)))

(defun log-time-seq(selector protocol &key
                    (os *standard-output*) div mod (sep " "))
    (when-bind(tss (time-seq-stats protocol))
     (when-bind(tsv (aref tss selector))
       (loop :for ts :across tsv
             :do (format os "~@?~A~D~%" *time-format* (time-seq-time ts) sep
                         (let ((seq (time-seq-seq ts)))
                           (when div (setf seq (/ seq div)))
                           (when mod (setf seq (mod seq mod)))
                           seq))))))

(defun process-event(event tcp)
  (let ((old-state (state tcp)))
    (multiple-value-bind(new-state action) (lookup-state (state tcp) event)
      (setf (state tcp) new-state)
      (when (and (= new-state timed-wait) (/= old-state timed-wait))
        (schedule-timer (tw-timeout tcp) 'timed-wait-timeout-event tcp))
      (let ((need-close-notify
             (and (= new-state closed)
                  (/= old-state closed)
                  (/= event timeout))))
        (when (and need-close-notify (not (close-notified tcp)))
          (node:unbind tcp (node tcp)
                       :local-port (local-port tcp)
                       :peer-port (peer-port tcp)
                       :peer-address (peer-address tcp))
          (setf (close-notified tcp) t)
          (when-bind(a (application tcp))
                    (closed a tcp))))
      action)))

(defgeneric process-action(action protocol &optional packet header dst-address)
  (:documentation "Process action in response to events")
  (:method((action (eql 'no-act)) tcp &optional packet header dst-address)
    (declare (ignore tcp packet header dst-address))))

(defmethod process-action((action (eql 'ack-tx)) tcp &optional
                          packet header dst-address)
  (declare (ignore packet dst-address))
  (send-packet ack (next-tx-seq tcp) (next-rx-seq tcp) tcp)
  (when header  ;;Save the receiver advertised window
    (setf (rx-win tcp) (window header))))

(defmethod process-action((action (eql 'ack-tx-1)) tcp &optional
                          packet header dst-address)
    (declare (ignore packet dst-address))
    ;; Ack in response to syn/ack"
     (when header  ;;Save the receiver advertised window
       (setf (rx-win tcp) (window header)))
     (cancel-timer 'conn-timeout-event tcp)
     (when-bind(a (application tcp)) ;;  Notify application
        (connection-complete a tcp))
     (unless (pending-data tcp)
       ;; Application did not send anything, need to send ack
       (send-packet ack (next-tx-seq tcp) (next-rx-seq tcp) tcp)))

(defmethod process-action((action (eql 'rst-tx)) tcp &optional
                          packet header dst-address)
    (declare (ignore packet))
    (send-packet rst 0 0 tcp
                 :dst-address dst-address
                 :dst-port (if header (src-port header) (peer-port tcp))))

(defmethod process-action((action (eql 'syn-tx)) tcp &optional
                          packet header dst-address)
    (declare (ignore packet header dst-address))
    ;; Schedule a Timeout for "no reply" processing
    (schedule-timer (conn-timeout tcp) 'conn-timeout-event tcp)
    ;; log time sent to get an initial rtt estimate
    (setf (syn-time tcp) (simulation-time))
    (send-packet syn 0 0 tcp))

(defmethod process-action((action (eql 'syn-ack-tx)) tcp &optional
                          packet header dst-address)
  ;; Need a little special-casing here.  If tcp header passed in,
  ;; then make a copy and ask the copy to respond, because this is
  ;; the listening TCP.  Otherwise, it is the copied tcp from above,
  ;; then just send the Syn/Ack
  (declare (ignore packet dst-address))
  (let ((application (application tcp)))
    (cond
      ((and header (= (state tcp) listen))
       ;; this is the listener - create a copy for response
       (let* ((responder (copy tcp))
              (accept
               (if application
                   (progn
                     (setf (application responder) application)
                     (connection-from-peer application responder
                                           (when header (src-port header)))
                     t))))
         (setf (parent responder) tcp)
         (setf (delete-on-timed-wait responder) t)
         (when (and (/= 0 (fid header)) (= 0 (fid responder)))
           (setf (fid responder) (fid header))) ;; assign fid to responder
         (if accept
             (progn
               (respond (src-address header) (src-port header) responder)
               (incf (number-children tcp)))
             (reject (src-address header) (src-port header) responder))))
      (t
       ;; this is the responder so just respond
       (send-packet (logior syn ack) 0 0 tcp)
       (schedule-timer (retransmit-timeout (rtt tcp))
                       'conn-timeout-event tcp)))))

(defmethod process-action((action (eql 'fin-tx)) tcp &optional
                          packet header dst-address)
  (declare (ignore packet header dst-address))
  (send-packet fin (next-tx-seq tcp) (next-rx-seq tcp) tcp))

(defmethod process-action((action (eql 'fin-ack-tx)) tcp &optional
                          packet header dst-address)
  (declare (ignore packet header dst-address))
  (send-packet (logior fin ack)
               (next-tx-seq tcp) (next-rx-seq tcp) tcp)
  (when (= (state tcp) last-ack)
    (schedule-timer (retransmit-timeout (rtt tcp)) 'last-ack tcp)))

(defmethod process-action((action (eql 'new-ack)) tcp &optional
                          packet header dst-address)
  (declare (ignore dst-address))
  (let ((ack-number (ack-number header)))
    (cond
      ((< ack-number (highest-rx-ack tcp)) t) ;; old ack no action)
      ((and (= ack-number (highest-rx-ack tcp))
            (< ack-number (next-tx-seq tcp)))
       ;; dupack received
       (dupack header (incf (dup-ack-count tcp)) tcp)
       t)
      (t
       (when (> ack-number (highest-rx-ack tcp))
         (setf (dup-ack-count tcp) 0))
       (newack ack-number tcp)
       (new-rx packet header tcp))))) ;; in case associated data

(defmethod process-action((action (eql 'new-seq-rx)) tcp &optional
                          packet header dst-address)
  (declare (ignore dst-address))
  (new-rx packet header tcp)
  t)

(defmethod process-action((action (eql 'retx)) tcp &optional
                          packet header dst-address)
  (declare (ignore tcp packet header dst-address)))

(defmethod process-action((action (eql 'tx-data)) tcp &optional
                          packet header dst-address)
  (declare (ignore packet header dst-address))
  (tcp-send-pending tcp))

(defmethod process-action((action (eql 'peer-close)) tcp &optional
                          packet header dst-address)
  (declare (ignore dst-address))
  ;; First we have to be sure the FIN packet was not received
  ;; out of sequence.  If so, note pending close and process
  ;; new sequence rx
  (when (and header (/= (sequence-number header) (next-rx-seq tcp)))
    ;; process close later
    (setf (pending-close tcp) t)
    (new-rx packet header tcp)
    t)
  (let ((data (peek-pdu packet 1)))
    (when (and data (> (size data) 0))
      (new-rx packet header tcp)))
  (let ((saved-state (state tcp))
        (application (application tcp)))
    (if application ;; if application notify it to close
        (if (close-req-notified tcp)
            (close-request application tcp)
            (setf (close-req-notified tcp) t))
        (close-connection tcp)) ;; else just close here
    (when (= saved-state (state tcp))
      ;; need to ack app will close later
      (send-packet ack (next-tx-seq tcp) (next-rx-seq tcp) tcp)))
  (when (= (state tcp) last-ack)
    (schedule (retransmit-timeout (rtt tcp)) 'last-ack-timeout-event)))

(defmethod process-action((action (eql 'app-closed)) tcp &optional
                          packet header dst-address)
  (declare (ignore packet header dst-address))
  (when (and (= (state tcp) syn-sent) (not (close-notified tcp)))
    (setf (close-notified tcp) t)
    (when-bind(a (application tcp))
      (closed a tcp))))

(defun send-packet(flags seq sack tcp &key
                   (packet (make-instance 'packet))
                   (dst-address (peer-address tcp))
                   (dst-port (peer-port tcp)))
  (unless (node tcp)
    (error "TCP ~S sending packet with no local node" tcp))
  (let* ((data (peek-pdu packet))
         (data-length (size data))
         (state (state tcp)))
    (when data
      (let ((rto (retransmit-timeout (rtt tcp))))
        (unless (or (no-timer-p tcp)
                    (find-timer 'retx-timeout-event tcp))
          (schedule rto 'retx-timeout-event))
        (setf (last-timeout-delay tcp) rto)
        (sent-seq seq data-length (rtt tcp)) ;; notify rtt estimator
        (when (< seq (high-tx-mark tcp)) ;; count retransmits
          (incf (retransmit-count tcp)))))
    (let ((h (make-instance
              'tcp-header
              :src-port (local-port tcp)
              :src-address (ipaddr (node tcp))
              :dst-port dst-port
              :dst-address dst-address
              :sequence-number seq
              :ack-number sack
              :flags (logior flags
                            (if (and (or (= state fin-wait-1)
                                         (= state fin-wait-2))
                                     (not (pending-data tcp)))
                                fin
                                0)
                            (if (need-ack tcp) ack 0))
              :window (adv-win tcp)
              :fid (fid tcp)
              :data-length data-length
              :cwnd (cwnd tcp))))
      (push-pdu h packet)
      (setf (need-ack tcp) nil)
      (when (flag-set-p (flags h) ack)
        (setf (last-ack-time tcp) (simulation-time))))
    (layer3:send (layer3:protocol tcp) (node tcp) packet
                         :dst-address dst-address
                         :ttl (ttl tcp)
                         :protocol-number (protocol-number tcp)
                         :tos (tos tcp))))

(defun tcp-send-pending(tcp &optional with-ack)
  (declare (ignore with-ack))
  (let ((pending-data (pending-data tcp))
        (n-sent 0)) ;; count number packets sent
    (when pending-data
      (while (> (size-from-seq (first-pending-seq tcp) (next-tx-seq tcp)
                                pending-data)
                0)
        (let ((w (available-window tcp)))
          (when  (and (< w (seg-size tcp)) (> (size (pending-data tcp)) w))
            (return))
          (unless (buffer-available-p (+ (seg-size tcp) 100) tcp)
            (request-notification tcp)
            (return))
          (let* ((s (min w (seg-size tcp)))
                 (data (copy-from-seq s (first-pending-seq tcp)
                                      (next-tx-seq tcp)))
                 (sz (size data))
                 (flags 0))
            (let ((remaining-data (size-from-seq
                                   (first-pending-seq tcp)
                                   (+ (next-tx-seq tcp) sz)
                                   pending-data)))
              ;; Compute amount still remaining to see if we need FIN flag
              (when (and (close-on-empty tcp)
                         (= 0 remaining-data)
                         (/= 0 (bytes-sent tcp)))
                (setf flags (logand flags fin)
                      (state tcp) fin-wait-1)))
            (note-time-seq log-seq-tx (next-tx-seq tcp) tcp)
            (let((packet (make-instance 'packet)))
              (push-pdu data packet)
              (send-packet flags (next-tx-seq tcp) (next-rx-seq tcp) tcp
                           :packet packet)
              (incf n-sent)
              (incf (next-tx-seq tcp) sz)
              (incf (bytes-sent tcp) sz)
              (setf (high-tx-mark sz)
                    (max (next-tx-seq tcp) (high-tx-mark sz)))))))
      (when (> n-sent 0)
        ;; At least one packet sent, be sure we have pending re-tx timer
        (unless (find-timer 'retx-timeout-event tcp)
          (let ((rto (retransmit-timeout (rtt tcp))))
            (schedule rto 'retx-timeout-event)
            (setf (last-timeout-delay tcp) rto)))))))

(defun common-newack(sack tcp &optional skip-timer)
  "common-newack is called only for new (non-duplicate) acks
 and MUST be called by any subclass, from the newack function
 Always cancel any pending re-tx timer on new acknowledgement"
  (unless skip-timer (cancel-timer 'retx-timeout-event tcp))
  (let ((numberack (- sack (highest-rx-ack tcp))))
    (incf (total-ack tcp) numberack)
    (setf (highest-rx-ack tcp) sack) ;;Note the highest recieved Ack
    (when (> ack (next-tx-seq tcp)) (setf (next-tx-seq tcp) sack)) ;; advanced
    (with-slots(pending-data first-pending-seq highest-rx-ack) tcp
      (when (and pending-data
                 (zerop (size-from-seq
                         first-pending-seq highest-rx-ack pending-data)))
        ;; All pending acked, can be deleted
        (setf pending-data nil)
        (cancel-timer 'retx-timeout-event tcp)))
    ;; Notify application of data sent
    (when-bind(a (application tcp))
        (when (> numberack 0) (sent a numberack tcp))))
  ;; Try to send more data
  (tcp-send-pending tcp)
  ;; See if we need to post a re-tx timer
  (when (and (not skip-timer)
             (not (find-timer 'retx-timeout-event tcp))
             (or (> (unack-data-count tcp) 0)
                 (= (state tcp) fin-wait-1)
                 (= (state tcp) fin-wait-2)))
    (schedule-timer (retransmit-timeout (rtt tcp)) 'retx-timeout-event tcp)))

(defun send-ack(sack tcp &optional forced)
  (let ((sendit
         (or forced
             (unless (zerop (del-ack-timeout tcp))
              ;; Using delayed ack, see if already pending
              (cond
                ((find-timer 'del-ack-timeout-event tcp)
                 ;; if already pending cancel and send packet
                 (cancel-timer 'del-ack-timeout-event tcp)
                 t)
                (t ;; else delay this ack
                 (schedule-timer
                  (del-ack-timeout tcp) 'del-ack-timeout-event tcp)
                 (setf (next-ack-seq tcp) ack)
                 nil))))))
    (unless (tcp-send-pending tcp)
      ;; nothing sent send empty
      (when (= (last-ack-time tcp) (simulation-time))
        (error "TCP ~A resending ack ~A" tcp sack))
      (when sendit (send-packet ack (next-tx-seq tcp) sack tcp)))))

(defun new-rx(packet header tcp)
  (let* ((original-state (state tcp))
         (data (peek-pdu packet 1))
         (s (size data))
         (application (application tcp)))
    (when (> s 0)
      ;; Log sequence received if enabled
      (note-time-seq log-seq-rx (sequence-number header) tcp)
      ;;Note we need the ACK bit on next transmitted pkt
      (setf (need-ack tcp) t)
      (cond
        ((= (sequence-number header) (next-rx-seq tcp))
         ;;  Received seq is expected, deliver this and any buffered data
         (incf (next-rx-seq tcp) s)
         (incf (bytes-received tcp) s)
         (when application
           (pop-pdu packet)
           (receive application packet tcp (sequence-number header))
           (when (close-notified tcp)
             (error "TCP ~A got data after close-notified" tcp)))
         ;; look for buffered data
         (flet ((seqnum(p) (sequence-number (peek-pdu p)))) ;; packet seq num
           (setf (buffered-data tcp)
                 (sort (buffered-data tcp) #'< :key #'seqnum))
           (do ((packet (pop (buffered-data tcp)) (pop (buffered-data tcp))))
               ((not packet))
             (when  (> (seqnum packet) (next-rx-seq tcp))
               (push packet (buffered-data tcp)) ;; not next expected
               (return))
             (let* ((h (pop-pdu packet))
                    (data (peek-pdu packet))
                    (sz (size data))
                    (seq (sequence-number h))
                    (next-rx-seq (next-rx-seq tcp)))
               (cond
                 ((< (+ seq sz) next-rx-seq)
                  ;; nothing to deliver
                  (setf packet nil))
                 ((< seq next-rx-seq)
                  ;; remove already delivered data
                  (remove-data (- next-rx-seq seq) data))
                 ((/= seq next-rx-seq)
                  (error "Next rx failure, first ~D, next rx seq ~D"
                         seq  next-rx-seq)))
               (incf (bytes-received tcp) sz)
               (when (and application packet)
                 (receive application packet tcp seq))
               (incf (next-rx-seq tcp) sz)
               (when (or (pending-close tcp) (> original-state established))
                 ;; se if can close now
                 (unless (buffered-data tcp)
                   (process-action peer-close tcp)))))))
        ((>= (sequence-number header) (next-rx-seq tcp))
         ;; Received seq is > expected, just re-ack previous and buffer data
         (push packet (buffered-data tcp))))
      (send-ack (next-rx-seq tcp) tcp))))

(defun retransmit(tcp)
  "Retransmit the oldest pending packet"
  (let ((pending-data (pending-data tcp)))
    (if pending-data
        (let* ((packet (make-instance 'packet))
               (data (copy-from-seq (seg-size tcp)
                                    (first-pending-seq tcp)
                                    (highest-rx-ack tcp)
                                    pending-data))
               (sz (size data))
               (flags 0))
          (unless (> sz 0) (error "Retx without pending data"))
          (let ((remaining-data
                 (size-from-seq (first-pending-seq tcp)
                                (+ (next-tx-seq tcp) sz)
                                pending-data)))
            (when (and (close-on-empty tcp) (= 0 remaining-data))
              (setf flags (logand flags fin)))
            (push-pdu data packet)
            (note-time-seq log-seq-tx (highest-rx-ack tcp) tcp)
            (send-packet flags (highest-rx-ack tcp) (next-rx-seq tcp) tcp
                         :packet packet)))
        (send-packet
         fin (next-tx-seq tcp) (next-rx-seq tcp) tcp))))  ;; resend lost FIN


;;;; ------------------------------------------------

(def-singleton-class tcp-demux(ipv4:ipv4-demux)
  ((layer4:protocol-number :initform 6 :reader layer4:protocol-number
                           :allocation :class))
  (:documentation "Demultiplexer for TCP protocol"))

(tcp-demux) ;; Ensure the instance of tcp-demux is created

;;; fill in state machine

(map
 nil
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
    ;; has not yet  closed.
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
    (bad-flags timed-wait no-act))))

(map
 nil
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
   (#x3c rst-rx)))    ;; rst

(defvar *default-tcp-variant* 'tcp-tahoe "Class for default TCP variant")

(defmethod make-instance((tcp (eql 'tcp)) &rest rest)
  (apply #'make-instance (cons *default-tcp-variant* rest)))