(in-package :lens.samples)

(defparameter *tictoc* #p"/home/willijar/dev/lisp/src/lens/samples/tictoc.ini")

(defclass Txc1(module)
  ()
  (:gates
   (in :input)
   (out :output))
  (:metaclass module-class))

(defmethod initialize((module Txc1) &optional (stage 0))
  (when (and (zerop stage) (eql (name module) 'tic))
    (send module (make-instance 'message :name 'TicTocMsg) 'out))
  t)

(defmethod handle-message((module Txc1) msg)
  (send module msg 'out))

(defclass TicToc1(network)
  ()
  (:submodules
   (tic Txc1)
   (toc Txc1))
  (:connections
   (=> (delay-channel :delay 0.1) (tic out) (toc in))
   (=> (delay-channel :delay 0.1) (toc out) (tic in)))
  (:metaclass compound-module-class))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Txc2(Txc1)
  ()
  (:properties :display (:icon "block/routing"))
  (:metaclass module-class))

(defmethod initialize((module Txc2) &optional (stage 0))
  (when (and (zerop stage) (eql (name module) 'tic))
    (write-line "Sending initial message" *trace-output*)
    (send module (make-instance 'message :name 'TicTocMsg) 'out))
  t)

(defmethod handle-message((module Txc2) msg)
  (format *trace-output* "Received message ~A, sending it out again.~%"
          msg)
  (call-next-method))

(defclass TicToc2(network)
  ()
  (:submodules
   (tic Txc2 :properties (:display (:color "cyan")))
   (toc Txc2 :properties (:display (:color "gold"))))
  (:connections
   (=> (delay-channel :delay 0.1) (tic out) (toc in))
   (=> (delay-channel :delay 0.1) (toc out) (tic in)))
  (:metaclass compound-module-class))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Txc3(Txc2)
  ((counter :initform 10 :accessor counter))
  (:metaclass module-class))

(defmethod handle-message((module Txc3) msg)
  (decf (counter module))
  (if (zerop (counter module))
      (format *trace-output*
              "~A's counter reached zero, not resending message.~%"
              module)
      (progn
        (format *trace-output*
                "~A's counter is ~D, sending back message.~%"
                module (counter module))
        (call-next-method))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Txc4(Txc3)
  ((send-msg-on-init
    :parameter t :initarg :send-msg-on-init :initform nil :type boolean
    :reader send-msg-on-init
    :documentation "Whether module should send message on initialization"))
  (:metaclass module-class))

(defmethod initialize((instance Txc4) &optional (stage 0))
  (let ((limit (read-parameter instance "limit" 'integer)))
    (when limit (setf (counter instance) limit)))
  (when (and (zerop stage) (send-msg-on-init instance))
    (write-line "Sending initial message" *trace-output*)
    (send instance (make-instance 'message :name 'TicTocMsg) 'out))
  t)

(defclass TicToc4(network)
  ()
  (:submodules
   (tic Txc4 :send-msg-on-init t :properties (:display (:color "cyan")))
   (toc Txc4 :send-msg-on-init nil :properties (:display (:color "gold"))))
  (:connections
   (=> (delay-channel :delay 0.1) (tic out) (toc in))
   (=> (delay-channel :delay 0.1) (toc out) (tic in)))
  (:metaclass compound-module-class))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Txc5(Txc4)
  ()
  (:metaclass module-class))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Txc6(Txc1)
  ((event :type message :initform nil
          :documentation "Event object used for timing")
   (tictocmsg :initform nil
    :type message :documentation "To remeber message until we send it again"))
  (:metaclass module-class))

(defmethod initialize((instance Txc6)  &optional stage)
  (when (zerop stage)
    (with-slots(event tictocmsg) instance
      (setf event (make-instance 'message :name 'event))
      (when (eql (name instance) 'tic)
        (eventlog "Scheduling first send to t=5.0s")
        (setf tictocmsg (make-instance 'message :name 'TicTocMsg))
        (schedule-at instance event :time 5.0))))
  t)

(defmethod handle-message((instance Txc6) msg)
  (with-slots(event tictocmsg) instance
    (cond
      ((eql msg event)
        ;; self message arrived so sent out tictocmsg and null it out
        ;; so no confusion later
       (eventlog "Wait period is over, sending back message")
       (send instance tictocmsg 'out)
       (setf tictocmsg nil))
      (t
       ;; Not self message so must be tictoc message from partner.
       ;;  save it and schedule our self message to send it back later"
       (eventlog "Message arrived, starting to wait 1 sec")
       (setf tictocmsg msg)
       (schedule-at instance event :time (+ (simulation-time) 1.0))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Txc7(Txc6)
  ((delay-time :parameter t :volatile t :type time-type :reader delay-time))
  (:metaclass module-class))

(defmethod handle-message((instance Txc7) msg)
  (with-slots(event tictocmsg) instance
    (cond
      ((eql msg event)
        ;; self message arrived so sent out tictocmsg and null it out
        ;; so no confusion later
       (eventlog "Wait period is over, sending back message")
       (send instance tictocmsg 'out)
       (setf tictocmsg nil))
      ((< (uniform 0 1) 0.1) ;; lose message with 0.1 probability
       (eventlog "Losing Message"))
      (t ;;The "delayTime" module parameter can be set to values like
            ;; "exponential(5)" and then here
            ;; we'll get a different delay every time.
       (let ((delay (delay-time instance)))
         (eventlog "Message arrived, starting to wait ~A seconds" delay)
         (setf tictocmsg msg)
         (schedule-at instance event :time (+ (simulation-time) delay)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Tic8(module)
  ((timeout :type time-type :initform 1.0 :reader timeout)
   (timeout-event :type message :reader timeout-event
                  :initform (make-instance 'message :name 'timeout-event)))
  (:gates
   (in :input)
   (out :output))
  (:properties :display (:icon "block/routing"))
  (:metaclass module-class))

(defclass Toc8(module)
  ()
  (:gates
   (in :input)
   (out :output))
  (:properties :display (:icon "block/process"))
  (:metaclass module-class))

(defclass TicToc8(network)
  ()
  (:submodules
   (tic Tic8 :properties (:display (:color "cyan")))
   (toc Toc8 :properties (:display (:color "gold"))))
  (:connections
   (=> (delay-channel :delay 0.1) (tic out) (toc in))
   (=> (delay-channel :delay 0.1) (toc out) (tic in)))
  (:metaclass compound-module-class))

(defmethod initialize((instance Tic8) &optional stage)
  (when (zerop stage)
    (eventlog "Sending initial message")
    (send instance (make-instance 'message :name 'tictoc) 'out)
    (schedule-at instance (timeout-event instance)
                 :time (+ (simulation-time) (timeout instance))))
  t)

(defmethod handle-message((instance Tic8) message)
  (cond
    ((eql message (timeout-event instance))
     ;; if receive timeout event packet didn't arrive so resent
     (eventlog "Timeout expired, resending message and restarting time")
     (send instance (make-instance 'message :name 'tictoc) 'out)
     (schedule-at instance (timeout-event instance)
                  :time (+ (simulation-time) (timeout instance))))
    (t ;; message arrived - cancel timer and send new one
     (eventlog "Timer Cancelled")
     (cancel (timeout-event instance))
     (send instance (make-instance 'message :name 'tictoc) 'out)
     (schedule-at instance (timeout-event instance)
                  :time (+ (simulation-time) (timeout instance))))))

(defmethod handle-message((instance Toc8) message)
  (cond
    ((< (uniform 0 1) 0.1)
     (eventlog "Losing message"))
    (t
     (eventlog "Sending back same message as acknowledgement.")
     (send instance message 'out))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Tic9(Tic8)
  ((message :type message
            :documentation "Message that has to be resent on timeout"))
  (:metaclass module-class))

(defclass Toc9(Toc8)
  ()
  (:metaclass module-class))

(defun generate-new-message()
  (make-instance 'message :name (gensym "tic")))

(defun send-message-copy(instance)
  (send instance (duplicate (slot-value instance 'message)) 'out))

(defmethod initialize((instance Tic9) &optional stage)
  (when (zerop stage)
    (eventlog "Sending initial message")
    (with-slots(message) instance
      (setf message (generate-new-message))
      (send-message-copy instance)
      (schedule-at instance (timeout-event instance)
                   :time (+ (simulation-time) (timeout instance)))))
  t)

(defmethod handle-message((instance Tic9) message)
  (cond
    ((eql message (timeout-event instance))
     ;; if receive timeout event packet didn't arrive so resent
     (eventlog "Timeout expired, resending message and restarting time")
     (send-message-copy instance)
     (schedule-at instance (timeout-event instance)
                  :time (+ (simulation-time) (timeout instance))))
    (t ;; message arrived - cancel timer and send new one
     (eventlog "Ack received: ~A" message)
     (eventlog "Timer Cancelled")
     (cancel (timeout-event instance))
     (setf (slot-value instance 'message) (generate-new-message))
     (send-message-copy instance)
     (schedule-at instance (timeout-event instance)
                  :time (+ (simulation-time) (timeout instance))))))

(defmethod handle-message((instance Toc9) message)
  (cond
    ((< (uniform 0 1) 0.1)
     (eventlog "Losing message"))
    (t
     (eventlog "~A received, sending back acknowledgement." message)
     (send instance (make-instance 'message :name 'ack) 'out))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Tic10(module)
  ()
  (:gates ;; declare in and out to be vector gates
   (in :input 0)
   (out :output 0))
  (:properties :display (:icon "block/routing"))
  (:metaclass module-class))

(defclass Tictoc10(network)
  ()
  (:types
   (C delay-channel :delay 0.1))
  (:submodules
   (tic 6 Tic10))
  (:connections
   (=> C (tic 0 out ++) (tic 1 in ++))
   (<= C (tic 0 in ++) (tic 1 out ++))

   (=> C (tic 1 out ++) (tic 2 in ++))
   (<= C (tic 1 in ++) (tic 2 out ++))

   (=> C (tic 1 out ++) (tic 4 in ++))
   (<= C (tic 1 in ++) (tic 4 out ++))

   (=> C (tic 3 out ++) (tic 4 in ++))
   (<= C (tic 3 in ++) (tic 4 out ++))

   (=> C (tic 5 out ++) (tic 4 in ++))
   (<= C (tic 5 in ++) (tic 4 out ++)))
  (:metaclass compound-module-class))

(defmethod initialize((instance Tic10) &optional stage)
  (when (and (zerop stage) (zerop (index instance)))
    ;; boot process with initial self message
    (schedule-at instance
                 (make-instance 'message
                                :name (format nil "tic-~D" (index instance)))
                 :time 0.0d0))
  t)

(defmethod handle-message((instance Tic10) message)
  ;; pick a random gate to sent it on unless we are node 3
  (if (= (index instance) 3)
      (eventlog "Message ~A arrived" message)
      (let* ((n (gate-size instance 'out))
             (k (intuniform 0 (1- n))))
        (eventlog "~A Forwarding message ~A on port out[~D]"
                  instance message k)
        (send instance message `(out ,k)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Tic12(module)
  ()
  (:gates ;; declare in and out to be vector gates
   (gate :inout 0))
  (:properties :display (:icon "block/routing"))
  (:metaclass module-class))

(defclass Tictoc12(network)
  ()
  (:types
   (C delay-channel :delay 0.1))
  (:submodules
   (tic 6 Tic12))
  (:connections
   (<=> C (tic 0 gate ++) (tic 1 gate ++))
   (<=> C (tic 1 gate ++) (tic 2 gate ++))
   (<=> C (tic 1 gate ++) (tic 4 gate ++))
   (<=> C (tic 3 gate ++) (tic 4 gate ++))
   (<=> C (tic 4 gate ++) (tic 5 gate ++)))
  (:metaclass compound-module-class))

(defmethod initialize((instance Tic12) &optional stage)
  (when (and (zerop stage) (zerop (index instance)))
    ;; boot process with initial self message
    (schedule-at instance
                 (make-instance 'message
                                :name (format nil "tic-~D" (index instance)))
                 :time 0.0d0))
  t)

(defmethod handle-message((instance Tic12) message)
  ;; pick a random gate to sent it on unless we are node 3
  (if (= (index instance) 3)
      (eventlog "Message ~A arrived" message)
      (let* ((n (gate-size instance 'gate))
             (k (intuniform 0 (1- n))))
        (eventlog "~A Forwarding message ~A on port out[~D]"
                  instance message k)
        (send instance message `(gate ,k)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass TicTocMsg(message)
  ((source :reader source :initarg :source)
   (destination :reader destination :initarg :destination)
   (hop-count :initform 0 :accessor hop-count)))

(defclass Tic13(Tic12)
  ()
  (:metaclass module-class))

(defmethod initialize((instance Tic13) &optional stage)
  (when (and (zerop stage) (zerop (index instance)))
    ;; boot process with initial self message
    (schedule-at instance
                 (generate-message instance)
                 :time 0.0d0))
  t)

(defmethod handle-message((instance Tic13) message)
  (cond
    ((= (index instance) (destination message))
     (eventlog "Message ~A arrived after ~A hops" message (hop-count message))
     (forward-message instance (generate-message instance)))
    (t
     (forward-message instance message))))

(defgeneric generate-message(instance)
  (:method ((instance Tic13))
    (let* ((src (index instance))
           (n (size instance)) ;; module vector size
           (dest (intuniform 0 (- n 2))))
      (when (>= dest src) (incf dest))
      (make-instance 'TicTocMsg
                     :name (format nil "tic-~D-to-~D" src dest)
                     :source src
                     :destination dest))))

(defgeneric forward-message(instance message)
  (:method((instance Tic13) message)
    (incf (hop-count message))
    (let* ((n (gate-size instance 'gate))
           (k (intuniform 0 (1- n))))
        (eventlog "~A Forwarding message ~A on gate[~D]"
                  instance message k)
        (send instance message `(gate ,k)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Tic14(Tic13)
  ((num-sent :initform 0 :accessor num-sent)
   (num-received :initform 0 :accessor num-received))
  (:metaclass module-class))

;; note WATCH is unnecessary in common lisp as all data is inspectable

(defmethod handle-message((instance Tic14) message)
  (cond
    ((= (index instance) (destination message))
     (eventlog "Message ~A arrived after ~A hops" message (hop-count message))
     (incf (num-received instance))
     (forward-message instance (generate-message instance))
     (incf (num-sent instance)))
    (t
     (forward-message instance message))))

(defmethod finish((instance Tic14))
  (eventlog "~A rcvd:~A send:~A" instance (num-received instance) (num-sent instance)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(register-signal 'arrivalSignal)

(defclass Txc16(Tic13)
  ()
  (:properties
   :statistic (hopCount
               :source arrivalSignal
               :title "hop count"
               :default (vector stddev)
               :optional (timeavg (histogram :mode integer))
               :interpolation-mode nil))
  (:metaclass module-class))

(defmethod handle-message((instance Txc16) message)
   (cond
    ((= (index instance) (destination message))
     (emit instance 'arrivalSignal (hop-count message))
     (eventlog "Message ~A arrived after ~A hops" message (hop-count message))
     (forward-message instance (generate-message instance)))
    (t
     (forward-message instance message))))

