(in-package :lens.wsn)

(defclass mac-radio-control-info()
  ((RSSI :type double-float :initarg :RSSI :accessor RSSI
         :documentation "the RSSI of the received packet")
   (LQI :type double-float :initarg :LQI :accessor LQI
        :documentation "the LQI of the received packet")))

(defclass mac-packet(wsn-packet)
  ())

(defclass mac(comms-module)
  ((max-mac-frame-size
    :initform 0 :type integer :parameter t :reader max-net-frame-size
    :properties (:units "B")
    :documentation "in bytes")
   (address :parameter t :type integer :reader mac-address
            :documentation "MAC address - will default to node index."))
  (:gates
   (network :inout)
   (radio :inout))
  (:metaclass module-class))

(defmethod initialize-instance :after ((instance mac) &key &allow-other-keys)
  (unless (slot-boundp instance 'address)
    (setf (slot-value instance 'address) (index (node instance)))))

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
  (send instance message 'network))

(defmethod handle-message :around ((module routing) (packet routing-packet))
  (with-slots(max-mac-frame-size header-overhead) module
    (if (and (> max-mac-frame-size 0)
             (> (+ (byte-length packet) header-overhead) max-mac-frame-size))
        (eventlog "Oversized packet ~A dropped. Size ~A, mac layer overhead ~A, max mac packet size ~A" (byte-length packet) header-overhead max-mac-frame-size)
        (call-next-method))))

(defgeneric toRadioLayer(mac entity &optional destination)
  (:documentation "Send packet to mac layer")
  (:method((module mac) (command communications-control-command)
           &optional destination)
    (assert (and (not destination)
                 (not (typep module 'mac-control-command))))
    (send module command 'radio))
  (:method((module routing) (packet mac-packet) &optional destination)
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
   (make-instance 'mac-packet
                  :header-overhead (header-overhead module)
                  :source (mac-address module)
                  :sequence-number (next-sequence-number module))
   packet))
