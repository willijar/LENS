(in-package :lens.wsn)

(register-signal
 'packet-receive
 "Emitted when application receives a packet.")

(register-signal
 'packet-send
 "Emitted when application sends a packet.")

(defclass app-net-control-info()
  ((RSSI :type float :initarg :RSSI :reader RSSI :initform nil
         :documentation "the RSSI of the received packet")
   (LQI :type float :initarg :LQI :reader LQI :initform nil
        :documentation "the LQI of the received packet")
   (source :initarg :source :reader source
           :documentation "the routing layer source of the received packet")
   (destination
    :initarg :destination :reader destination
    :documentation "the routing layer dest of the packet to be sent"))
  (:documentation "We need to pass information between app and communication
 layer which is external to the packet i.e. not carried by a real
 packet (e.g., which is the destination, or what was the RSSI for the
 packet received) but this information is related to the specific
 packet."))

(defclass application-packet(packet)
  ((name
    :initarg :applicationid :reader applicationid
    :documentation "virtual app uses application ID to filter packet delivery.")
   (lens::encapsulated-packet
    :type t :initarg :payload :reader payload
    :documentation "Higher level encapsulated protocol packet.")
   (sequence-number :initarg :seqnum :initarg :sequence-number
                    :reader sequence-number :reader sequence-number
                    :documentation "a field to distinguish between packets")
   (byte-length :type fixnum :initarg :byte-length :reader byte-length
                :initform 20))
  (:documentation "A generic application packet. If defining your own
  packet you have to extend from this packet. You do not have to use
  the fields already defined, and you can always define your own
  size."))

(defmethod print-object((pkt application-packet) stream)
  (print-unreadable-object(pkt stream :type t :identity nil)
    (format stream "#~D (~D bytes)" (sequence-number pkt) (byte-length pkt))))

(defmethod bit-length((pkt application-packet))
  (* 8 (byte-length pkt)))

(defmethod duplicate((pkt application-packet)
                     &optional (duplicate (make-instance 'application-packet)))
  (call-next-method)
  (copy-slots '(sequence-number byte-length) pkt duplicate))

(defclass application(wsn-module)
  ((owner :reader node)
   (applicationid :parameter t :type symbol :initform nil
                  :initarg :id :reader applicationid)
   (priority :parameter t :type integer :initarg :priority :initform 1)
   (header-overhead
    :parameter t  :initform 8
    :type integer :initarg :header-overhead :reader header-overhead
    :documentation "in bytes")
   (payload-overhead
    :parameter t :initform 12
    :type integer :initarg :payload-overhead :reader payload-overhead
    :documentation "in bytes")
   (last-sequence-number :initform -1 :type integer))
  (:gates
   (network :inout)
   (sensor :inout 0))
  (:properties
   :statistic (latency
               :source (latency packet-receive)
               :title "application latency"
               :default ((histogram :min 0)))
   :statistic (packet-receive :title "application packets received"
                              :default (count))
   :statistic (packet-receive-per-node
               :title "packets received per node"
               :source (source (control-info packet-receive))
               :default (indexed-count))
   :statistic (packet-send :title "application packets sent"
                           :default (count)))
  (:metaclass module-class)
  (:documentation "Application core module connects to sensors for measurements
  and to communication module for sending/receiving data."))

(defmethod initialize-instance :after ((application application)
                                       &key &allow-other-keys)
  (unless (slot-boundp application 'applicationid)
    (setf (slot-value application 'applicationid)
          (class-name (class-of application)))))

(defgeneric next-sequence-number(instance)
  (:method (instance)
    (incf (slot-value instance 'last-sequence-number))))

(defun sensor-request(application &optional (sensor-index 0))
  "To be used by applications to request a sensor reading"
  (send application
        (make-instance 'sensor-message :name 'sensor-request)
        (gate application 'sensor :index sensor-index :direction :output)))

(defgeneric handle-sensor-reading(application measurement)
  (:documentation "Must be implemented by applications to handle
  sensor readings")
  (:method(application (measurement measurement))
    (handle-sensor-reading application (measurement-value measurement)))
  (:method(application measurement)
    (declare (ignore application measurement))))

(defgeneric to-network(application entity &optional destination)
  (:documentation "Send message, packet or data to network")
  (:method((application application) (command communications-control-command)
           &optional destination)
    (assert (not destination))
    (send application command 'network))
  (:method((application application) (packet application-packet)
           &optional destination)
    (if destination
        (setf (control-info packet)
              (make-instance
               'app-net-control-info
               :destination destination
               :source (network-address (node application))))
        (assert (destination (control-info packet))
                ()
                "Destination not specified for packet send"))
    (emit application 'packet-send packet)
    (send application packet 'network)
    (tracelog "Sending ~A to communication layer" packet))
  (:method((application application) (message message) &optional destination)
    (declare (ignore destination))
    (error "Application ~A attempting to send ~A to network"
           application message))
  (:method((application application) data &optional destination)
    (let ((packet (encapsulate application data)))
      (to-network application packet destination))))

(defmethod encapsulate((application application) data)
  (make-instance
   'application-packet
   :applicationid (applicationid application)
   :timestamp (simulation-time)
   :payload data
   :sequence-number (next-sequence-number application)
   :byte-length (packet-size application data)))

(defgeneric packet-size(application data)
  (:method((application application) data)
    (declare (ignore data))
    (+ (header-overhead application) (payload-overhead application))))

(defmethod handle-message ((application application)
                           (message application-packet))
  (emit application 'packet-receive message)
  (tracelog "Received ~A" message))

(defmethod handle-message((application application) (message sensor-message))
  (handle-sensor-reading application (measurement message)))

(defgeneric sink-network-address(entity)
  (:documentation "Address of sink node for reporting applications")
  (:method((entity application)) 'sink)
  (:method((node node))
    (sink-network-address (submodule node 'application)))
  (:method((instance wsn-module))
    (sink-network-address (node instance))))

(defgeneric parent-network-address(entity)
    (:documentation "Address of parent node for aggregation applications")
    (:method((entity application)) 'parent))

(defgeneric sink-p(entity)
  (:documentation "Return true if an application (or router) is a sink")
  (:method((application application))
    (eql (sink-network-address application)
         (network-address (node application))))
  (:method((node node))
    (eql (network-address node)
         (sink-network-address  (submodule node 'application))))
  (:method((instance wsn-module))
    (sink-p (node instance))))

(defmethod handle-message((instance application)
                          (message communications-control-message))
  ;; by default applications can ignore control messages
)
