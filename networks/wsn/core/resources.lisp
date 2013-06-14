(in-package :lens.wsn)

(defclass ram-store(message)
  ((num-bytes :initarg :num-bytes :reader num-bytes
              :documentation "Use -ve value to free ram"))
  (:documentation "Message to change ram used"))

(register-signal
 'power-change
 "Sent by a module to indicate a new power consumption level (in W)")

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
    :parameter t :type time-type :initform 30d-6
    :documentation "Standard deviation in cpu clock drift")
   (remaining-energy
    :parameter t :type float :initform 18720 :accessor remaining-energy
                     :documentation "In joules - default is 2 AA batteries")
   (time-of-last-calculation :type time-type :initform 0.0d0)
   (baseline-node-power
    :parameter t :type real :initform 6e-3
    :documentation "Periodic power consumption")
   (current-node-power :initform 0.0 :type float :accessor current-node-power)
   (power-levels
    :type hash-table :initform (make-hash-table) :reader power-levels
    :documentation "Last power drawn indexed by module")
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

(defmethod initialize-instance :after
    ((module resources) &key &allow-other-keys)
  (assert (>= (update-interval module) 0.0d0)
          ()
          "Resource update interval must be >=0")
  (with-slots(clock-drift clock-drift-sigma) module
    ;; randomise clock drift within reasonable bounds
    (setf clock-drift (normal 0.0d0 clock-drift-sigma))
    (setf clock-drift
          (max (min clock-drift (* 3d0 clock-drift-sigma))
               (* -3d0 clock-drift-sigma))))
  (with-slots(baseline-node-power current-node-power) module
    (assert (>= baseline-node-power 0.0)
            ()
            "Baseline node power must be >=0")
    (setf current-node-power baseline-node-power))
  (subscribe (parent-module module) 'power-change module))

(defun calculate-energy-spent(instance)
  (with-slots(remaining-energy time-of-last-calculation current-node-power
              periodic-update-message update-interval)
      instance
    (when (> remaining-energy 0.0)
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
    (when (<= remaining-energy 0.0)
      (setf remaining-energy 0.0)
      (emit instance 'out-of-energy))))

(defmethod handle-message((instance resources) message)
  (cond
    ((eql message (periodic-update-message instance))
     (calculate-energy-spent instance)
     (schedule-at instance message
                  :delay (update-interval instance)))
    (t
     (call-next-method))))

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

(defmethod get-simulation-time((instance resources) local-time)
    (* local-time (clock-drift instance)))

(defmethod receive-signal((instance resources) (signal (eql 'power-change))
                          source power)
   (calculate-energy-spent instance)
   (let* ((old-power (gethash source (power-levels instance) 0.0))
          (new-power (+ (current-node-power instance) (- power old-power))))
     (eventlog "New power consumption oldpower=~A newpower=~A"
               (current-node-power instance) new-power)
     (setf (current-node-power instance) new-power)
     (setf (gethash source (power-levels instance)) power)))
