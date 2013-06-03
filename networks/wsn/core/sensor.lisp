(in-package :lens.wsn)

(defstruct measurement
  (location (make-coord) :type coord :read-only t)
  (time (simulation-time) :type time-type )
  (value 0 :type real)
  sensor)

(defclass sensor-message(message)
  ((measurement :type measurement :initform nil :initarg :measurement
                :accessor measurement)))

(defclass sensor(wsn-module)
  ((owner :reader node)
   (power-consumption
    :parameter t :initform 0.02 :reader power-consumption
    :initarg :power-consumption
    :documentation "Power consumption for this sensor")
   (physical-process-id
    :parameter t :type integer :reader physical-process-id :initform 0
    :initarg :physical-process-id
    :documentation "Index of the physical process being measured")
   (physical-process
    :type physical-process :reader physical-process
    :documentation "Actual physical process instance")
   (measurand
    :parameter t :initform 'temperature :reader measurand
    :documentation "Type of sensor e.g. humidity, temperature, light etc")
   (bias
    :parameter t :initform (uniform 0 0.1) :reader bias :initarg :bias
    :properties (:format 'eval)
    :documentation "Device offset reading")
   (noise
    :parameter t ::volatile t :initform #'(lambda() (normal 0 0.1))
    :reader noise :initarg :noise
    :documentation "stddev of Gaussian Noise for device")
   (sensitivity
    :parameter t :initform 0 :reader sensitivity :type real
    :documentation "holds the minimum value which can be sensed by
    each sensing device.")
   (resolution
    :parameter t :initform 0.001 :reader resolution :type real
    :documentation "holds the sensing resolution. the returned value
     will always be a multiple of number, given here")
   (saturation
    :parameter t :initform  1 :reader saturation :type real
    :documentation "holds the saturation value for each sensing device")
   (last-measurement :type measurement :accessor last-measurement)
   (sample-interval
    :parameter t :initform 0 :type real :reader sample-interval
    :documentation "Interval between regular samples or 0 if not
    continually sampling")
   (measurement-delay
    :parameter t :initform 100e-6 :type real :reader measurement-delay
    :documentation "Conversion time - default is typical for AVR")
   (sample-timer :type message :accessor sample-timer
                   :initform nil :documentation "sampling self messsge")
   (measurement-timer
    :initform nil :type sensor-message :accessor measurement-timer
    :documentation "Self measurement message"))
  (:gates
   (application :inout))
  (:metaclass module-class)
  (:documentation "Module representing a single sensor. Sensors may
  either operate in continual sampling mode or in responsive mode (if
  sample-interval is 0).

In responsive mode there will be measurement-delay delay between
request message and sending back a readin message. If in sampling mode
thern the message will correspond to the last sampled time."))

(defmethod configure :after ((sensor sensor))
  (with-slots(measurement-delay sample-interval) sensor
    (assert (or (zerop measurement-delay)
                (> sample-interval measurement-delay))
            ()
            "Sample interval ~A is less than measurement time of ~A"
            sample-interval measurement-delay)))

(defmethod initialize((sensor sensor) &optional (stage 0))
  (case stage
    (0
     (setf (slot-value sensor 'physical-process)
          (submodule (network sensor)
                      'physical-processes
                      :index (physical-process-id sensor)))))
  (call-next-method))

(defmethod startup((instance sensor))
  (when (not (zerop (sample-interval instance)))
    (setf (measurement-timer instance)
          (make-instance 'sensor-message
                         :owner instance :name 'measurement))
    (setf (sample-timer instance)
          (make-instance 'message :owner instance :name 'sensor-sample))
    (set-timer instance (sample-timer instance) (sample-interval instance))))

(defmethod shutdown ((sensor sensor))
  (when (sample-timer sensor)
    (cancel (sample-timer sensor)))
  (when (measurement-timer sensor)
    (cancel (measurement-timer sensor))))

(defun sensor-measurement(sensor)
  "Sensor value at this time"
  (let* ((location (location (node sensor)))
         (time (simulation-time))
         (value (measure (physical-process sensor) (measurand sensor)
                         location time)))
    (with-slots(bias resolution sensitivity saturation) sensor
      (make-measurement
       :sensor sensor
       :location location
       :time time
       :value (* resolution
                 (floor
                  (max saturation
                       (min sensitivity
                            (+ bias value (noise sensor)))
                       resolution)))))))

(defmethod handle-message((sensor sensor) message)
  (cond
    ((eql message (sample-timer sensor))
     ;; regular sampling - schedule to upddate last-measurement after
     ;; measurement-delay and resample after sample interval
     (let ((m (measurement-timer sensor)))
       (setf (measurement m) (sensor-measurement sensor))
       (set-timer sensor m  (measurement-delay sensor))
       (set-timer sensor message  (sample-interval sensor))))
    ((eql message (measurement-timer sensor))
     ;; this is sampling self measurement message so update last-measurement
     (setf (last-measurement sensor) (measurement sensor)))
    ((typep message 'sensor-message)
     ;; measurement message from somewhere else
     (cond
       ((measurement message) ;; already has measurement so send out
        (send sensor message 'application))
       ((zerop (sample-interval sensor))
        ;; no sampling so take reading and schedule measurement for
        ;; measurement later
        (setf (measurement message) (sensor-measurement sensor))
        (set-timer sensor message (measurement-delay sensor)))
       (t
        ;; we are sampling so put return last-measurement in message
        (setf (measurement message) (last-measurement sensor))
        (send sensor message 'application))))
    (t
     (warn 'unknown-message :module sensor :message message))))
