(in-package :lens.wsn)

(defstruct value-report
  nodeid
  location
  value)

(defmethod print-object((v value-report) stream)
  (if *print-readably*
      (call-next-method)
      (print-unreadable-object(v stream :type t :identity t)
        (format stream "~A from ~A"
              (value-report-value v)
              (value-report-nodeid v)))))

(defclass value-reporting(application)
  ((sink-network-address :parameter t :type fixnum :reader sink-network-address)
   (header-overhead :initform 8)
   (payload-overhead :initform 12)
   (max-sample-interval :parameter t :type time-type :initform 60e0)
   (min-sample-interval :parameter t :type time-type :initform 1e0)
   (routing-level :type fixnum)
   (last-sensed-value :type real)
   (sent-once :type boolean :initform nil)
   (random-back-off-interval-fraction :type real)
   (request-sample :type timer-message
                   :initform (make-instance 'timer-message)))
   (:properties
    :statistic (got-value :title "got value" :default (last-value)))
  (:metaclass module-class)
  (:documentation "Document class which will continually sample sensors
and send data to 'SINK over network"))

(defmethod startup((application value-reporting))
  (call-next-method)
  (with-slots(random-back-off-interval-fraction max-sample-interval)
          application
    (setf random-back-off-interval-fraction (uniform 0 1.0))
    (set-timer application 'request-sample
               (* random-back-off-interval-fraction max-sample-interval))))

(defmethod handle-timer((application value-reporting)
                        (timer (eql 'request-sample)))
  (sensor-request application)
  (set-timer application 'request-sample
             (slot-value application 'max-sample-interval)))

(defmethod handle-message ((application value-reporting)
                           (pkt application-packet))
  (when (sink-p application)
    (emit application 'packet-receive pkt)
    (emit application 'got-value (value-report-value (payload pkt)))
    (tracelog "Sink received ~A" (payload pkt))))

(defmethod handle-sensor-reading((application value-reporting)
                                 (sensed-value real))
  (tracelog "Sensed = ~A" sensed-value)
  (emit application 'got-value sensed-value)
  (to-network application
              (make-value-report
               :nodeid (nodeid (node application))
               :location (location (node application))
               :value sensed-value)
              (sink-network-address application)))
