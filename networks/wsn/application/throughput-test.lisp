(in-package :lens.wsn)

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
   (send-packet
    :type timer-message :initform (make-instance 'timer-message)))
  (:properties
   :statistic (packets-received-by-sender
               :source (source (control-info packet-receive)))
               :title "packets received per sender node"
               :default (indexed-count))
  (:metaclass module-class)
  (:documentation "Application that will generate packets at specified rate"))

(defmethod startup((application throughput-test))
  (call-next-method)
  (with-slots(packet-spacing packet-rate startup-delay) application
    (setf packet-spacing (if (> packet-rate 0) (/ 1.0d0 packet-rate) 0))
    (if (and (> packet-spacing 0)
             (not (eql (network-address (node application))
                       (next-recipient application))))
        (set-timer application 'send-packet
                   (+ startup-delay packet-spacing))
        (tracelog "Not sending Packets"))))

(defmethod handle-timer((application throughput-test)
                        (timer (eql 'send-packet)))
  (to-network application nil  (next-recipient application))
  (set-timer application 'send-packet (packet-spacing application)))

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
        (emit application 'packet-receive rcvPacket)) ;; log receipt
      (to-network application
                  (duplicate rcvPacket)
                  (next-recipient application)))) ;; else send on







