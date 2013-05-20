(in-package :lens)

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

(defclass TicToc3(network)
  ()
  (:submodules
   (tic Txc3)
   (toc Txc3))
  (:connections
   (=> (delay-channel :delay 0.1) (tic out) (toc in))
   (=> (delay-channel :delay 0.1) (toc out) (tic in)))
  (:metaclass compound-module-class))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Txc4(Txc3)
  ((send-msg-on-init
    :parameter t :initarg :send-msg-on-init :initform nil :type boolean
    :reader send-msg-on-init
    :documentation "Whether module should send message on initialization"))
  (:metaclass module-class))

(defmethod finalize-parameters((instance Txc4))
  (let ((limit (read-parameter instance "limit" 'integer)))
    (when limit (setf (counter instance) limit))))

(defmethod initialize((module Txc4) &optional (stage 0))
  (when (and (zerop stage) (send-msg-on-init module))
    (write-line "Sending initial message" *trace-output*)
    (send module (make-instance 'message :name 'TicTocMsg) 'out))
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
  (declare (ignore stage))
  (with-slots(event tictocmsg) instance
    (setf event (make-instance 'message :name 'event))
    (when (eql (name instance) 'tic)
      (simtrace "Scheduling first send to t=5.0s")
      (setf tictocmsg (make-instance 'message :name 'TicTocMsg))
      (schedule-at instance event :time 5.0)))
  t)

(defmethod handle-message((instance Txc6) msg)
  (with-slots(event tictocmsg) instance
    (cond
      ((eql msg event)
        ;; self message arrived so sent out tictocmsg and null it out
        ;; so no confusion later
       (simtrace "Wait period is over, sending back message")
       (send instance tictocmsg 'out)
       (setf event nil))
      (event
       ;; Not self message so must be tictoc message from partner.
       ;;  save it and schedule our self message to send it back later"
       (simtrace "Message arrived, starting to wait 1 sec")
       (setf tictocmsg msg)
       (schedule-at instance event :time (+ (simulation-time) 1.0))))))


