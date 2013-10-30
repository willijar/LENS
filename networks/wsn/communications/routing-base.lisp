(in-package :lens.wsn)

(defclass net-mac-control-info()
  ((RSSI :type double-float :initarg :RSSI :accessor RSSI :initform nil
         :documentation "the RSSI of the received packet")
   (LQI :type double-float :initarg :LQI :accessor LQI :initform nil
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

(defclass routing(comms-module)
  ((max-net-frame-size
    :initform 0 :type integer :parameter t :reader max-net-frame-size
    :properties (:units "B")
    :documentation "in bytes (0 for no limit)"))
  (:gates
   (application :inout)
   (mac :inout))
  (:metaclass module-class))

(defmethod network-address((instance routing))
  (network-address (node instance)))

;; default - pass through unwanted control commands and messages -
;; warn if for this layer but unhandled
;; implementations must handle application packets and routing packets
;; by specialising handle-message main method

(defmethod handle-message((instance routing)
                          (message communications-control-command))
  (send instance message 'mac))

(defmethod handle-message((instance routing)
                          (message network-control-message))
  (warn 'unknown-message :module instance :message message))

(defmethod handle-message((instance routing)
                          (message communications-control-message))
  (send instance message 'application))

(defmethod handle-message((instance routing) (msg network-control-command))
  (handle-control-command instance (command msg) (argument msg)))

(defmethod node((instance comms-module)) (owner (owner instance)))

(defmethod handle-message :around ((module routing) (packet application-packet))
  (with-slots(max-net-frame-size header-overhead) module
    (if (and (> max-net-frame-size 0)
             (> (+ (byte-length packet) header-overhead) max-net-frame-size))
        (tracelog "Oversized packet ~A dropped. Size ~A, network layer overhead ~A, max network packet size ~A"
                  (byte-length packet) header-overhead max-net-frame-size)
        (call-next-method))))

(defmethod  handle-message :before ((instance routing) (packet routing-packet))
  (tracelog "Received ~A from mac layer." packet))

(defmethod handle-message ((instance routing) (packet routing-packet))
  ;; from mac layer
  (when (or (eql (destination packet) (network-address (node instance)))
            (eql (destination packet) broadcast-network-address))
    (send instance (decapsulate packet) 'application)))

(defgeneric to-mac(routing entity &optional next-hop-mac-address)
  (:documentation "Send packet to mac layer")
  (:method((module routing) (command communications-control-command)
           &optional destination)
    (assert (and (not destination)
                 (not (typep command 'network-control-command))))
    (send module command 'mac))
  (:method((module routing) (packet routing-packet) &optional next-hop)
    (if next-hop
        (setf (control-info packet)
              (make-instance
               'net-mac-control-info
               :next-hop next-hop))
        (assert (next-hop (control-info packet))))
    (send module packet 'mac))
  (:method((module routing) (message message) &optional destination)
    (declare (ignore destination))
    (error "Network module ~A attempting to send ~A to mac"
           module message)))

(defmethod decapsulate((packet routing-packet))
  (let ((application-packet (call-next-method)))
    (setf (control-info application-packet)
          (make-instance 'app-net-control-info
                         :rssi (rssi (control-info packet))
                         :lqi (lqi (control-info packet))
                         :source (source packet)
                         :destination (destination packet)))
    application-packet))

(defgeneric resolve-network-address(routing network-address)
  (:documentation "Return  resolved mac address from given network address")
  (:method(routing network-address)
    (declare (ignore routing))
    ;; by default mac address and network address have same values in WSN
    network-address))

(defmethod enqueue(packet (instance routing))
  (cond
    ((enqueue packet (buffer instance))
     ;; success
     (tracelog "Packet buffered from application layer, buffer state : ~D/~D"
               (size (buffer instance)) (buffer-size (buffer instance)))
     t)
    (t ;; failure
     (send instance
           (make-instance 'net-control-message :command 'net-buffer-full)
           'application)
     nil)))
