(in-package :lens.wsn)

(defclass sensor-measurement(message)
  ((measurement-value :type real :initarg :value
                      :reader measurement-value)
   (measurement-location :type coord :initarg :location
                         :reader location )
   (measurement-time :type time-type :initarg :time
                     :reader measurement-time)
   (request-time :type time-type :initarg :request-time
                 :reader request-time
                 :documentation "Time measurement was requested")
   (measurand :type symbol :initarg :measurand
              :reader "Measurand")
   (physical-process :initarg

(defclass sensor(module)
  ((power-consumption
    :parameter t :initform 0.02 :reader power-consumption
    :initarg :power-consumption
    :documentation "Power consumption for this sensor")
   (physical-process
    :reader physical-process :initarg :physical-process
    :documentation "The physical process being measured")
   (sensor-type
    :parameter t :initform 'temperature :reader sensor-type
    :documentation "Type of sensor e.g. humidity, temperature, light etc")
   (device-bias
    :parameter t :initform 0.1 :reader device-bias :initarg :bias
    :documentation "Device offset reading")
   (device-drift
    :parameter t :initform 0 :reader device-drift :initarg :drift
    :documentation "drift in reading with time")
   (device-noise
    :parameter t :initform 0.1 :reader device-noise :initarg :noise
    :documentation "Noise for device")

  (:metaclass module-class)

(defmethod configure((sensor sensor))
  (call-next-method)
  (setf (slot-value sensor 'physical-process)
        (submodule (network *simulation*) 'physical-processes
                   (or (read-parameter sensor 'physical-process
                                       '(integer :min 0))
                       0))))
