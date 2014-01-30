(in-package :lens.wsn)

(defclass ram-store(message)
  ((num-bytes :initarg :num-bytes :reader num-bytes
              :documentation "Use -ve value to free ram"))
  (:documentation "Message to change ram used"))

(register-signal
 'power-change
 "Sent by a module to indicate a new power consumption level (in W)")

(register-signal
 'energy-consumed
 "Sent every time an energy consumption is calculated")

(defclass resources(wsn-module)
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
    :parameter t :type double-float :initform 18720d0 :accessor remaining-energy
                     :documentation "In joules - default is 2 AA batteries")
   (time-of-last-calculation :type time-type)
   (baseline-node-power
    :parameter t :type real :initform 6e-3
    :documentation "Periodic power consumption")
   (current-node-power :initform 0.0 :type float :accessor current-node-power)
   (power-levels
    :type hash-table :initform (make-hash-table) :reader power-levels
    :documentation "Last power drawn indexed by module")
   (update-interval
    :parameter t :type time-type :reader update-interval :initform 1d0
    :documentation "Interval for periodic updates in energy")
   (periodic-update-message
    :type message :reader periodic-update-message
    :initform (make-instance 'message :name 'resource-periodic-update))
   (clock-drift :reader clock-drift
                :documentation "Actual clock drift for this module")
   (disabled-p :type boolean :initform t :accessor disabled-p))
  (:properties
     :statistic (energy-consumed :title "Consumed Energy" :default (sum)))
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
  (unless (slot-boundp module 'time-of-last-calculation)
    (setf (slot-value module 'time-of-last-calculation) (simulation-time))))

(defmethod initialize list ((module resources) &optional (stage 0))
  (case stage
    (0
     (subscribe (node module) 'power-change module)
     (subscribe (node module) 'energy-consumed module)))
  t)

(defun calculate-energy-spent(instance)
  (unless (slot-boundp instance 'time-of-last-calculation)
    (setf (slot-value instance 'time-of-last-calculation) (simulation-time)))
  (with-slots(remaining-energy time-of-last-calculation current-node-power)
      instance
    (when (> remaining-energy 0.0)
      (let* ((time-passed (- (simulation-time) time-of-last-calculation))
             (energy-consumed
              (coerce (* time-passed current-node-power) 'single-float)))
        (tracelog "Energy consumed in last ~:/dfv:eng/s is ~:/dfv:eng/J"
                  time-passed energy-consumed)
        (emit instance 'energy-consumed energy-consumed)
        (setf time-of-last-calculation (simulation-time))))))

(defmethod finish((instance resources))
  (calculate-energy-spent instance)
  (call-next-method))

(defmethod handle-message((instance resources) message)
  (cond
    ((eql message (periodic-update-message instance))
     (calculate-energy-spent instance)
     (schedule-at instance (periodic-update-message instance)
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
    (* local-time (+ 1d0 (clock-drift instance))))

(defmethod get-clock((instance resources))
  (* (1+ (clock-drift instance)) (simulation-time)))

(defmethod startup((instance resources))
  ;; note since power-change signal may be received before this we have to check
  (tracelog "Clock Drift ~:/dfv:eng/s" (clock-drift instance))
  (unless (zerop (update-interval instance))
    (schedule-at instance (periodic-update-message instance)
                 :delay (update-interval instance))))

(defmethod receive-signal((instance resources) (signal (eql 'power-change))
                          source power)
   (calculate-energy-spent instance)
   (let* ((old-power (gethash source (power-levels instance) 0.0))
          (new-power (+ (current-node-power instance) (- power old-power))))
     (tracelog "Power consumption changed ~:/dfv:eng/W --> ~:/dfv:eng/W"
               (current-node-power instance) new-power)
     (setf (current-node-power instance) new-power)
     (setf (gethash source (power-levels instance)) power)))

(defmethod receive-signal((instance resources) (signal (eql 'energy-consumed))
                          source amount)
  (with-slots(remaining-energy) instance
    (decf remaining-energy amount)
    (when (<= remaining-energy 0.0)
      (setf remaining-energy 0.0)
      (emit instance 'out-of-energy))))
