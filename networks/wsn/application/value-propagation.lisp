(in-package :lens.wsn)

(register-signal
 'got-value
 "Emitted when application got value")

(defclass value-propagation(application)
  ((header-overhead :initform 8)
   (payload-overhead :initform 2)
   (temp-threshold :initform 15 :parameter t :initarg :temp-threshold)
   (total-packets :initform 0 :accessor total-packets :type fixnum)
   (current-max-received-value
    :initform -1e3 :accessor current-max-received-value :type float)
   (current-max-sensed-value
    :initform -1e3 :accessor current-max-sensed-value :type float)
   (sent-once :type boolean :initform nil)
   (the-value :type float :initform 0 :accessor the-value)
   (timer-message :type message :reader timer-message
                  :initform (make-instance 'message :name 'request-sample)))
  (:properties
   :statistic (got-value :title "got value" :default (last-value))
   :statistic (packets-received :title "app packets received"
                                :default (count)))
  (:metaclass module-class)
  (:documentation "Document class which will continually sample sensors
and send data to 'SINK over network"))

(defmethod startup((application value-propagation))
  (call-next-method)
  (set-timer application (timer-message application) 0d0))

(defmethod shutdown((application value-propagation))
  (call-next-method)
  (cancel (timer-message application)))

(defmethod handle-message((application value-propagation) message)
  (cond
    ((eql message (timer-message application))
     ;; timer fired so request sensor measurements and reset timer
     (sensor-request application))
    (t (call-next-method))))

(defmethod handle-message((application value-propagation)
                          (pkt application-packet))
  (emit application 'packets-received (source (control-info pkt)))
  (let ((received-value (payload pkt)))
    (with-slots(current-max-received-value temp-threshold the-value sent-once)
        application
      (when (> received-value current-max-received-value)
        (setf current-max-received-value received-value))
      (when (and (not sent-once) (> received-value temp-threshold))
        (setf the-value received-value
              sent-once t)
        (emit application 'got-value t)
        (to-network application
                    (duplicate pkt) broadcast-network-address)
        (tracelog "Got the value: ~/dfv:eng/" received-value)))))

(defmethod handle-sensor-reading((application value-propagation)
                                 (sensed-value real))
  (with-slots(current-max-sensed-value temp-threshold the-value sent-once)
        application
    (when (> sensed-value current-max-sensed-value)
      (setf current-max-sensed-value sensed-value))
    (when (and (not sent-once) (> sensed-value temp-threshold))
      (setf the-value sensed-value
            sent-once t)
      (emit application 'got-value t)
      (to-network application sensed-value broadcast-network-address))))