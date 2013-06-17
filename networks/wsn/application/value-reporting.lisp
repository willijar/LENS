(in-package :lens.wsn)

(defclass value-reporting(application)
  ((sink-address :parameter t :initform 'SINK :reader sink-address
                 :properties (:format 'read))
   (header-overhead :initform 8)
   (payload-overhead :initform 12)
   (max-sample-interval :parameter t :type real :initform 60)
   (min-sample-interval :parameter t :type real :initform 1)
   (random-back-off-interval-fraction :parameter t :initform '(uniform 0 1)
                                      :properties (:format 'eval))
   (timer-message :type message :reader timer-message
                  :initform (make-instance 'message :name 'request-sample)))
  (:metaclass module-class)
  (:documentation "Document class which will continually sample sensors
and send data to 'SINK over network"))

(defmethod startup((application value-reporting))
  (call-next-method)
  (with-slots(random-back-off-interval-fraction max-sample-interval)
          application
      (set-timer application (timer-message application)
                 (* random-back-off-interval-fraction max-sample-interval))))

(defmethod shutdown((application value-reporting))
  (call-next-method)
  (cancel (timer-message application)))

(defmethod handle-message((application value-reporting) message)
  (cond
    ((eql message (timer-message application))
     ;; timer fired so request sensor measurements and reset timer
     (dotimes(i (gate-size application 'sensor))
       (send application
             (make-instance 'sensor-message)
             (gate application 'sensor :index i)))
     (set-timer application message
                (slot-value application 'max-sample-interval)))
    ((typep message 'sensor-message)
     ;; got measurement from sensor
     (send application (measurement message) (sink-address application)))
    (t (warn 'unknown-message :message message :module application))))

(defmethod handle-message((application value-reporting) (pkt application-packet))
  (when (eql (network-address (node application))
             (sink-address application))
    (eventlog "Sink received from ~A value ~A"
              (source (control-info pkt))
              (payload pkt))))