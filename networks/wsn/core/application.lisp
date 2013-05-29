(in-package :lens.wsn)

(defclass app-control-info()
  ((RSSI :type double-float :initarg :RSSI :reader RSSI
         :documentation "the RSSI of the received packet")
   (LQI :type double-float :initarg :LQI :reader LQI
        :documentation "the LQI of the received packet")
   (source :initarg :source :reader source
           :documentation "the routing layer source of the received packet")
   (destination :initarg :destination :reader destination
                :documentation "the routing layer dest of the packet to be sent"))
  (:documentation "We need to pass information between app and routing
 layer which is external to the packet i.e. not carried by a real
 packet (e.g., which is the destination, or what was the RSSI for the
 packet received) but this information is related to the specific
 packet. Since information is passed between modules with
 messages/packets, we decided to encode this kind of external info as
 a separate structure in the packet. The fields there are handled by
 the virtualApp and virtualRouting code, setting a framework of
 interaction."))

(defclass application-packet(packet)
  ((applicationid
    :initarg :applicationid :reader applicationid
    :documentation "virtual app uses application ID to filter packet delivery.")
   (sequence-number :initarg :seqnum :initarg :sequence-number
                    :reader sequence-number :reader seqnum
                    :documentation "a field to distinguish between packets")
   (payload :initarg :payload :initarg :data
            :reader payload :reader data
            :documentation "Application data")
   (packet-header-overhead
    :type fixnum :initarg :packet-header-overhead :reader packet-header-overhead
    :documentation "in bytes")
   (payload-overhead
    :type fixnum :initarg :payload-overhead :reader payload-overhead
    :documentation "in bytes"))
  (:documentation "A generic application packet. If defining your own
  packet you have to extend from this packewt. You do not have to use
  the fields already defined, and you can always define your own
  size."))

(defmethod byte-length((pkt application-packet))
  (+ (packet-header-overhead pkt) (payload-overhead pkt)))

(defmethod bit-length((pkt application-packet))
  (* 8 (byte-length pkt)))

(defmethod duplicate((pkt application-packet)
                     &optional (duplicate (make-instance 'application-packet)))
  (call-next-method)
  (copy-slots '(applicationid sequence-number payload packet-header-overhead
                payload-overhead)
   pkt duplicate))

(defclass application(module)
  ((applicationid :initarg :applicationid :reader applicationid)
   (priority :parameter t :type integer :initarg :priority :initform 0)
   (packet-header-overhead
    :parameter t
    :type fixnum :initarg :packet-header-overhead :reader packet-header-overhead
    :documentation "in bytes")
   (payload-overhead
    :parameter t
    :type fixnum :initarg :payload-overhead :reader payload-overhead
    :documentation "in bytes"))
  (:gates
   (to-communication-module :output)
   (to-sensor-device-manager :output)
   (from-communication-module :input)
   (from-sensor-device-manager :input)
   (from-resource-manager :input))
  (:metaclass module-class)
  (:documentation  "The sensor node module. Connects to the wireless channel in order to communicate with other nodes. Connects to psysical processes so it can sample them."))


