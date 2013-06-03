(in-package :lens.wsn)

(defclass draw-power(message)
  ((power-consumed :type real :initarg :power-consumed :reader power-consumed))
  (:documentation "Message to change power consumption"))

(defclass ram-store(message)
  ((num-bytes :initarg :num-bytes :reader num-bytes
              :documentation "Use -ve value to free ram"))
  (:documentation "Message to change ram used"))

(defclass resources(module)
  ((ram-size
    :parameter t :type integer :initform 0 :initarg :ram-size
    :documentation "in kB")
   (flash-size
    :parameter t :type integer :initform 0 :initarg :flash-size
    :documentation "in kB")
   (total-ram-data :type integer :initform 0 :reader total-ram-data)
   (flash-write-cost
    :parameter t :type integer :initform 0 :initarg :flash-write-cost
    :documentation "in kB")
   (ram-read-cost
    :parameter t :type integer :initform 0 :initarg :ram-read-cost
    :documentation "in kB")
   (cpu-power-levels
    :parameter t :type list :initform nil :initarg :cpu-power-levels
    :documentation "plist mapping power level names to power")
   (cpu-power-level
    :parameter t :type symbol :initform nil :initarg :cpu-power-level
    :documentation "Current power level - initialised from parameter")
   (clock-drift-sigma
    :parameter t :type real :initform 0.00003
    :documentation "Standard deviation in cpu clock drift")
   (initial-energy :parameter t :type real :initform 18720
                   :documentation "In joules - default is 2 AA batteries")
   (remaining-energy :type real :accessor remaining-energy)
   (time-of-last-calculation :type time-type :initform 0)
   (baseline-node-power
    :parameter t :type real :initform 6e-3
    :documentation "Periodic power consumption")
   (current-node-power :type real :reader current-node-power)
   (old-power :type real :initform 0
              :documentation "Last power setting via message")
   (update-interval
    :parameter t :type time-type :reader update-interval :initform 1d-3
    :documentation "Interval for position updates along trajectory")
   (periodic-update-message
    :type message :reader periodic-update-message
    :initform (make-instance 'message :name 'resource-periodic-update))
   (clock-drift :reader clock-drift
                :documentation "Actual clock drift for this module")
   (disabled-p :type boolean :initform t :accessor disabled-p))
  (:metaclass module-class))

(defmethod configure :after ((module resources))
  (assert (>= (update-interval module) 0)
          ()
          "Resource update interval must be >=0")
  (with-slots(clock-drift clock-drift-sigma) module
    ;; randomise clock drift within reasonable bounds
    (setf clock-drift (normal 0 clock-drift-sigma))
    (setf clock-drift
          (max (min clock-drift (* 3 clock-drift-sigma))
               (* -3 clock-drift-sigma))))
  (with-slots(baseline-node-power current-node-power) module
    (assert (>= baseline-node-power 0)
            ()
            "Baseline node power must be >=0")
    (setf current-node-power baseline-node-power)))

(defun calculate-energy-spent(instance)
  (with-slots(remaining-energy time-of-last-calculation current-node-power
              periodic-update-message update-interval)
      instance
    (when (> remaining-energy 0)
      (let* ((time-passed (- time-of-last-calculation (simulation-time)))
             (energy-consumed (* time-passed current-node-power)))
        (eventlog "Energy consumed in last ~eS is ~eW"
                  time-passed energy-consumed)
        (consume-energy instance energy-consumed)
        (setf time-of-last-calculation (simulation-time))
        (cancel periodic-update-message)
        (schedule-at instance periodic-update-message
                     :time (+ (simulation-time) update-interval))))))

(defun consume-energy(instance amount)
  (with-slots(remaining-energy) instance
    (decf remaining-energy amount)
    (when (<= remaining-energy 0)
      (setf remaining-energy 0)
      (emit instance 'out-of-energy))))

(defmethod handle-message((instance resources) message)
  (cond
    ((eql message (periodic-update-message instance))
     (calculate-energy-spent instance)
     (schedule-at instance message
                  :delay (update-interval instance)))
    (t
     (warn 'unknown-message :module instance :message message))))

(defmethod handle-message((instance resources) (message draw-power))
  (calculate-energy-spent instance)
  (with-slots(old-power current-node-power) instance
    (let ((new-power (+ current-node-power (power-consumed message)
                        (- old-power))))
      (eventlog "New power consumption oldpower=~A newpower=~A"
                current-node-power new-power)
       (setf current-node-power new-power
             old-power (power-consumed message)))))

(defun ram-store(instance num-bytes)
   (with-slots(total-ram-data ram-size) instance
     (cond
       ((> (+ total-ram-data num-bytes) ram-size)
        (emit instance 'out-of-memory)
        nil)
       (t
        (incf total-ram-data num-bytes)
        (when (< total-ram-data 0) (setf total-ram-data 0))
        t))))

(defmethod handle-message((instance resources) (message ram-store))
  (ram-store instance (num-bytes message)))

(defmethod get-simulation-time((instance resources) local-time)
    (* local-time (clock-drift instance)))


