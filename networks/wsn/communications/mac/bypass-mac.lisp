(in-package :lens.wsn)

(defclass bypass-mac(mac)
  ((header-overhead :initform 8)
   (buffer-size :initform 0)
   (max-mac-frame-size :initform 0))
  (:metaclass module-class))

(defmethod handle-message((instance bypass-mac) (packet routing-packet))
  ;; from routing layer
  (let ((mac-packet (encapsulate instance packet)))
    (setf (destination mac-packet) (next-hop (control-info packet)))
    (to-radio instance mac-packet)
    (to-radio instance
              (make-instance 'radio-control-command
                             :command 'set-state
                             :argument 'tx))))

(defmethod handle-message((instance bypass-mac) (packet mac-packet))
  ;; from radio layer
    (when (or (eql (destination packet) (mac-address instance))
              (eql (destination packet) broadcast-mac-address))
      (let ((routing-packet (decapsulate packet)))
        (setf (control-info routing-packet)
              (make-instance 'net-mac-control-info
                             :rssi (rssi (control-info packet))
                             :lqi (lqi (control-info packet))))
        (send instance routing-packet 'routing))))