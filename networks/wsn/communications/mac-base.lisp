(in-package :lens.wsn)

(defclass mac-radio-control-info()
  ((rssi :type double-float :initarg :rssi :accessor rssi
         :documentation "the RSSI of the received packet")
   (lqi :type double-float :initarg :lqi :accessor lqi ;
        :documentation "the LQI of the received packet")))

(defclass mac-packet(wsn-packet)
  ())

(register-signal 'mac-packet-breakdown "Statistics on mac packets")

(defclass mac(comms-module)
  ((max-mac-frame-size
    :initform 0 :type integer :parameter t :reader max-mac-frame-size
    :initarg :max-mac-frame-size :properties (:units "B")
    :documentation "in bytes")
   (address :parameter t :type integer :reader mac-address
            :documentation "MAC address - will default to nodeid.")
   ;; physical layer parameters
   (phy-delay-for-valid-cs :parameter t :initform 0.128e-3
                           :type time-type :reader phy-delay-for-valid-cs)
   (radio :reader radio))
  (:properties
   :statistic (mac-packet-breakdown :default (indexed-count)))
  (:default-initargs :num-rngs 2)
  (:gates
   (routing :inout)
   (radio :inout))
  (:metaclass module-class))

(defmethod initialize list ((instance mac) &optional stage)
  (case stage
    (0
     (unless (slot-boundp instance 'address)
       (setf (slot-value instance 'address) (nodeid (node instance))))
     (setf (slot-value instance 'radio)
           (end-module (gate instance 'radio :direction :output)))))
  t)

(defgeneric tx-time(entity no-octets)
  (:documentation "Return the transmission time for no-octets on entity")
  (:method ((mac mac) (no-octets integer))
    (tx-time (radio mac) no-octets))
  (:method ((mac mac) (pkt mac-packet))
    (tx-time mac (byte-length pkt))))

(defmethod mac-address((node node))
  ;; currently one mac and radio per node so this is OK - change if multiple
  ;; interfaces per node
  (mac-address (submodule node '(communications mac))))

(defmethod handle-message((instance mac)
                          (message radio-control-command))
  (send instance message 'radio))

(defmethod handle-message((instance mac)
                          (message communications-control-message))
  (warn 'unknown-message :module instance :message message))

(defmethod handle-message((instance mac)
                          (message radio-control-message))
  (to-network instance message))

(defmethod handle-message((instance mac) (message mac-control-message))
  (handle-control-command instance (command message) (argument message)))

(defmethod handle-message :around ((module mac) (packet mac-packet))
  (with-slots(max-mac-frame-size header-overhead) module
    (if (and (> max-mac-frame-size 0)
             (> (+ (byte-length packet) header-overhead) max-mac-frame-size))
        (tracelog "Oversized packet ~A dropped. Size ~A, mac layer overhead ~A, max mac packet size ~A" (byte-length packet) header-overhead max-mac-frame-size)
        (call-next-method))))

(defgeneric to-radio(mac entity)
  (:documentation "Send packet to radio layer")
  (:method((module mac) (command communications-control-command))
    (assert (typep command 'radio-control-command))
    (send module command 'radio))
  (:method((module mac) (packet mac-packet))
    (send module packet 'radio))
  (:method((module mac) (message message))
    (error "Mac module ~A attempting to send ~A to RADIO"
           module message))
  (:method((module mac) (command cons))
    (to-radio module
              (make-instance 'radio-control-command
                             :command (first command)
                             :argument (rest command))))
  (:method((module routing) (command cons))
    (send module (make-instance 'radio-control-command
                             :command (first command)
                             :argument (rest command))
          'mac)))

(defmethod to-network((mac mac) packet &optional dummy)
  (declare (ignore dummy))
  (tracelog "Delivering ~A to network layer" packet)
  (send mac packet 'routing))

(defmethod encapsulate((module mac) packet)
  (encapsulate
   (make-instance 'mac-packet
                  :name (class-name (class-of module))
                  :header-overhead (header-overhead module)
                  :source (mac-address module)
                  :destination broadcast-mac-address
                  :sequence-number (next-sequence-number module))
   packet))

(defmethod enqueue(packet (instance mac))
  (cond
    ((enqueue packet (buffer instance))
     ;; success
     (tracelog "Packet buffered from network layer, buffer state : ~D/~D"
               (size (buffer instance)) (buffer-size (buffer instance)))
     t)
    (t ;; failure
     (send instance
           (make-instance 'mac-control-message :command 'mac-buffer-full)
           'routing)
     nil)))

(defgeneric attempt-tx(module &optional description)
  (:documentation "Attempt to transmit next packet. Description added to tracelog if present."))
