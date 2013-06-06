(in-package :lens.wsn)

(defclass net-mac-control-info()
  ((RSSI :type double-float :initarg :RSSI :accessor RSSI
         :documentation "the RSSI of the received packet")
   (LQI :type double-float :initarg :LQI :accessor LQI
        :documentation "the LQI of the received packet")
   (next-hop :type integer :initarg :next-hop :accessor next-hop)
   (last-hop :type integer :initarg :last-hop :accessor last-hop))
  (:documentation "We need to pass information between routing and MAC
  which is external to the packet i.e. not carried by a real
  packet (e.g., what is the next hop, or what was the RSSI for the
  packet received) but this information is related to the specific
  packet."))

(defclass routing-packet(wsn-packet)
  ()
  (:documentation "A generic routing packet header. An app packet will
  be encapsulated in it. If definining your own routing protocol and
  need a more specialized packet you have to create one the extends
  this generic packet."))

;; Network_GenericFrame has the following real-world
;; (non-simulation-specific) fields:
;;    unsigned short int frameType; --> 2bytes
;;    string source;  ----------------> 2bytes
;;    string destinationCtrl; --------> 2bytes
;;    string lastHop; ------------> 2bytes
;;    string nextHop; ------------> 2bytes
;;    unsigned short int ttl; ----> 2bytes
;;    string applicationID; ------> 2bytes
;; Total bytes = 7*2 = 14 (|*|)
;; From these 14bytes, BypassRoutingModule doesn't use everything.
;; It doesn't use the ttl and applicationID fields.
;; Concluding the calculations, the Network_GenericFrame for
;; BypassRoutingModule has a total overhead of:
;; 14-(2+2) = 10 bytes

(defclass routing(network-layer)
  ((max-net-frame-size
    :initform 0 :type integer :parameter t :reader max-net-frame-size
    :properties (:units "B")
    :documentation "in bytes (0 for no limit)"))
  (:gates
   (application :inout)
   (mac :inout))
  (:metaclass module-class))

;; default - pass through unwanted control commands and messages -
;; warn if for this layer but unhandled
;; implementations must handle application packets and routing packets
;; by specialising handle-message main method

(defmethod handle-message((instance routing)
                          (message network-control-command))
  (warn 'unknown-message :module instance :message message))

(defmethod handle-message((instance routing)
                          (message communications-control-command))
  (send instance message 'mac))

(defmethod handle-message((instance routing)
                          (message network-control-message))
  (warn 'unknown-message :module instance :message message))

(defmethod handle-message((instance routing)
                          (message communications-control-message))
  (send instance message 'application))

(defmethod node((instance comms-module)) (owner (owner instance)))

(defmethod handle-message :around ((module routing) (packet application-packet))
  (with-slots(max-net-frame-size header-overhead) module
    (if (and (> max-net-frame-size 0)
             (> (+ (byte-length packet) header-overhead) max-net-frame-size))
        (eventlog "Oversized packet ~A dropped. Size ~A, network layer overhead ~A, max network packet size ~A" (byte-length packet) header-overhead max-net-frame-size)
        (call-next-method))))

(defgeneric toMacLayer(routing entity &optional destination)
  (:documentation "Send packet to mac layer")
  (:method((module routing) (command communications-control-command)
           &optional destination)
    (assert (and (not destination)
                 (not (typep command 'network-control-command))))
    (send module command 'mac))
  (:method((module routing) (packet routing-packet) &optional destination)
    (if destination
        (setf (control-info packet)
              (make-instance
               'net-mac-control-info
               :next-hop destination))
        (assert (next-hop (control-info packet))))
    (send module packet 'mac))
  (:method((module routing) (message message) &optional destination)
    (declare (ignore destination))
    (error "Network module ~A attempting to send ~A to mac"
           module message)))

(defmethod encapsulate((module routing) (packet application-packet))
  (encapsulate
   (make-instance 'routing-packet
                  :header-overhead (header-overhead module)
                  :source (network-address (node module))
                  :sequence-number (next-sequence-number module))
   packet))
