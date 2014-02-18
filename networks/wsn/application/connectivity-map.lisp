(in-package :lens.wsn)

(defclass connectivity-map (application)
  ((header-overhead :initform 8)
   (payload-overhead :initform 32)
   (priority :parameter t :initform 1 :reader priority :initarg :priority)
   (packet-spacing :parameter t :type time-type :initform 1d-1
                   :reader packet-spacing)
   (packets-per-node :parameter t :type integer :initform 100
                     :reader packets-per-node)
   (packets-sent :initform 0 :type integer :accessor packets-sent)
   (send-packet
    :type timer-message :initform (make-instance 'timer-message)))
  (:metaclass module-class)
  (:documentation "Application that will generate packets at specified rate"))

(defmethod startup((application connectivity-map))
  (call-next-method)
  (set-timer application 'send-packet
             (* (packets-per-node application)
                (packet-spacing application)
                (nodeid (node application)))))

(defmethod handle-timer((application connectivity-map)
                        (timer (eql 'send-packet)))
  (unless (>= (packets-sent application) (packets-per-node application))
    (to-network application
                (encapsulate application (incf (packets-sent application)))
                broadcast-network-address)
    (set-timer application 'send-packet (packet-spacing application))))
