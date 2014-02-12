(in-package :lens.wsn)

(defclass simple-aggregation (application)
  ((header-overhead :initform 5)
   (payload-overhead :initform 100)
   (priority :parameter t :initform 1 :reader priority :initarg :priority)
   (sink-network-address :parameter t :type fixnum :reader sink-network-address)
   (sample-interval :parameter t :type time-type :initform 1)
   ;; timers
   (request-sample
    :type timer-message :initform (make-instance 'timer-message))
   (send-aggregated-value :type timer-message :initform (make-instance 'timer-message))
   ;; implementatio values
   (waiting-time-for-lower-level-data :type time-type :initform 0d0)
   (last-sensed-value :type real :initform 0.0)
   (aggregated-value :type real :initform 0.0))
  (:metaclass module-class)
  (:documentation "Application that will generate packets at specified rate"))

(defmethod startup((instance simple-aggregation))
  (call-next-method)
  (set-timer instance 'request-sample 0))

(defmethod handle-timer
    ((instance simple-aggregation) (timer (eql 'request-sample)))
  (sensor-request instance)
  (set-timer instance 'request-sample (slot-value instance 'sample-interval)))

(defmethod handle-timer
    ((instance simple-aggregation) (timer (eql 'send-aggregated-value)))
  (to-network instance
              (make-instance 'application-packet
                       :payload (slot-value instance 'aggregated-value))
              'application))

(defmethod handle-sensor-reading((instance simple-aggregation) (value real))
  (with-slots(last-sensed-value aggregated-value) instance
    (setf last-sensed-value value
          aggregated-value (max aggregated-value value))))

(defmethod handle-message ((instance simple-aggregation)
                           (pkt network-control-message))
  (let ((argument (argument pkt))
        (command (command pkt)))
    (when (member command '(lens.wsn.routing.multipath-rings::tree-level-updated
                            lens.wsn.routing.multipath-rings::connected-to-tree))
      (with-slots(routing-level waiting-time-for-lower-level-data
                  sample-interval) instance
        (setf routing-level
              (lens.wsn.routing.multipath-rings::mprings-sink-level argument))
        (setf  waiting-time-for-lower-level-data
               (* sample-interval (expt 2 routing-level)))
        (tracelog "Routing level ~D" routing-level)
        (set-timer instance
                   'send-aggregated-value  waiting-time-for-lower-level-data)))))
