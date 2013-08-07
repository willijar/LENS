(in-package :lens.wsn)

(register-signal
 'packets-received
 "Emitted with source address as value when packet received by application")

(defclass throughput-test (application)
  ((header-overhead :initform 5)
   (payload-overhead :initform 100)
   (priority :parameter t :initform 1 :reader priority :initarg :priority)
   (next-recipient
    :parameter t :reader next-recipient :initform 0
    :properties (:format read)
    :documentation "Destination address for packets produced in this
    node. This parameter can be used to create an application-level
    static routing. This way we can have a multi-hop throughput
    test.")
   (packet-rate :parameter t :initform 0 :reader packets-rate :type real
                :documentation "Packets per second")
   (startup-delay
    :parameter t :initform 0 :reader startup-delay :type time-type
    :properties (:units "s")
    :documentation "Delay in seconds before application starts
    producing packets")
   (packet-spacing :type real :reader packet-spacing)
   (timer-message :type message :reader timer-message
                  :initform (make-instance 'message :name 'send-packet-timer)))
  (:properties
   :statistic (packets-received
               :title "packets received per node"
               :default (indexed-count)))
  (:metaclass module-class)
  (:documentation "Application that will generate packets at specified rate"))

(defmethod startup((application throughput-test))
  (call-next-method)
  (with-slots(packet-spacing packet-rate startup-delay) application
    (setf packet-spacing (if (> packet-rate 0) (/ 1.0d0 packet-rate) 0))
    (if (and (> packet-spacing 0)
             (not (eql (network-address (node application))
                       (next-recipient application))))
        (set-timer application (timer-message application)
                   (+ startup-delay packet-spacing))
        (tracelog "Not sending Packets"))))

(defmethod shutdown((application throughput-test))
  (call-next-method)
  (cancel (timer-message application)))

(defmethod handle-message((application throughput-test) message)
  (cond
    ((eql message (timer-message application))
     (to-network application nil  (next-recipient application))
     (set-timer
      application (timer-message application) (packet-spacing application)))
    (t (call-next-method))))

(defmethod handle-message ((application throughput-test)
                           (message radio-control-message))
  (case (command message)
    (carrier-sense-interrupt
     (tracelog "CS Interrupt received. Current RSSI value is ~/dfv:eng/dBm"
               (read-rssi
                (submodule (node application) '(communications radio)))))))

(defmethod handle-message ((application throughput-test)
                           (rcvPacket application-packet))
  (if (eql (network-address (node application))
           (next-recipient application))
      (progn
        (tracelog "Received packet #~A from node ~A"
                  (sequence-number rcvPacket) (source (control-info rcvPacket)))
        (emit application 'packets-received (source (control-info rcvPacket)))
        (call-next-method)) ;; log receipt
      (to-network application
                  (duplicate rcvPacket)
                  (next-recipient application))))







