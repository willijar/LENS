(in-package :lens.wsn)

(defclass bypass-routing(routing)
  ((header-overhead :initform 10)
   (buffer-size :initform 32)
   (max-net-frame-size :initform 0))
  (:metaclass module-class))

;; default - pass through unwanted control commands and messages -
;; warn if for this layer but unhandled
;; implementations must handle application packets and routing packets
;; by specialising handle-message main method

(defmethod handle-message((instance bypass-routing) (packet application-packet))
  ;; from application layer
  (let ((routing-packet (encapsulate instance packet))
        (destination (destination (control-info packet))))
    (setf (destination routing-packet) destination)
    (to-mac instance routing-packet
            (resolve-network-address instance destination))))

(defmethod handle-message ((instance bypass-routing) (packet routing-packet))
  ;; from mac layer
  (when (or (eql (destination packet) (network-address (node instance)))
            (eql (destination packet) broadcast-network-address))
    (let ((application-packet (decapsulate packet)))
      (setf (control-info application-packet)
            (make-instance 'app-net-control-info
                           :rssi (rssi (control-info packet))
                           :lqi (lqi (control-info packet))
                           :source (source packet)
                           :destination (destination packet)))
      (send instance application-packet 'application))))