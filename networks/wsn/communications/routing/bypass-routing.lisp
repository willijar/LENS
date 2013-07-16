(in-package :lens.wsn)

(defclass bypass-routing(routing)
  ((header-overhead :initform 10)
   (buffer-size :initform 32)
   (max-net-frame-size :initform 0))
  (:metaclass module-class))

(defmethod handle-message((instance bypass-routing) (packet application-packet))
  ;; from application layer
  (let* ((destination (destination (control-info packet)))
         (routing-packet
          (encapsulate
           (make-instance 'routing-packet
                          :name (class-name (class-of module))
                          :header-overhead (header-overhead module)
                          :source (network-address (node module))
                          :destination destination
                          :sequence-number (next-sequence-number module))
           packet)))
    (to-mac instance routing-packet
            (resolve-network-address instance destination))))
