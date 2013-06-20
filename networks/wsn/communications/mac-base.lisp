(in-package :lens.wsn)

(defclass mac-radio-control-info()
  ((rssi :type double-float :initarg :rssi :accessor rssi
         :documentation "the RSSI of the received packet")
   (lqi :type double-float :initarg :lqi :accessor lqi ;
        :documentation "the LQI of the received packet")))

(defclass mac-packet(wsn-packet)
  ())

(defclass mac(comms-module)
  ((max-mac-frame-size
    :initform 0 :type integer :parameter t :reader max-mac-frame-size
    :initarg :max-mac-frame-size :properties (:units "B")
    :documentation "in bytes")
   (address :parameter t :type integer :reader mac-address
            :documentation "MAC address - will default to nodeid."))
  (:default-initargs :num-rngs 2)
  (:gates
   (routing :inout)
   (radio :inout))
  (:metaclass module-class))

(defmethod initialize and ((instance mac) &optional stage)
  (case stage
    (0
     (unless (slot-boundp instance 'address)
       (setf (slot-value instance 'address) (nodeid (node instance))))))
  t)

(defmethod mac-address((node node))
  ;; currently one mac and radio per npode so this is OK - change if multiple
  ;; interfaces per node
  (mac-address (submodule node '(communications mac))))

(defmethod handle-message((instance mac)
                          (message communications-control-command))
  (warn 'unknown-message :module instance :message message))

(defmethod handle-message((instance mac)
                          (message radio-control-command))
  (send instance message 'radio))

(defmethod handle-message((instance mac)
                          (message communications-control-message))
  (warn 'unknown-message :module instance :message message))

(defmethod handle-message((instance mac)
                          (message radio-control-message))
  (send instance message 'routing))

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
           module message)))

(defmethod encapsulate((module mac) packet)
  (encapsulate
   (make-instance 'mac-packet
                  :name (class-name (class-of module))
                  :header-overhead (header-overhead module)
                  :source (mac-address module)
                  :sequence-number (next-sequence-number module))
   packet))
