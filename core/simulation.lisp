;; LENS Simulation Simulation and event handling
;; Copyright 2013 Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>

;; This file is part of Lisp Educational Network Simulation (LENS)

;; This is free software released under the GNU General Public License (GPL)
;; See the LICENSE file provided or <http://www.gnu.org/licenses>

;;; Commentary:

;;

;;; Code:

(in-package :lens)

(defvar *simulation* nil "The global simulation instance")
(defvar *simulation-init-hooks* nil
  "A list of functions which take the simulation as an argument which
  are called after simulation etc is created. Can be used to add in
  layers which use signals such as statistical reporting or graphical
  presentation.")

(defvar *time-format* "~6,3f"  "Time output format control")

(deftype time-type() 'double-float)

(defmethod configuration(object)
  "Default to simulation configuration for all objects"
  (declare (ignore object))
  (configuration *simulation*))

(defclass simulation (named-object parameter-object)
  ((clock :type time-type :initform 0.0d0
	  :accessor clock :initarg :start-time
	  :documentation "simulation virtual time")
   (halted :type boolean :initform t :accessor halted)

   (thread :initform nil :reader simulation-thread
	    :documentation "Thread running simulation")
   (last-schedule-id :type integer :initform 0)
   (event-queue
    :initform
    (make-binary-heap
     :initial-size 1024
     :extend-size 1.4
     :element-type 'event
     :comp-fn #'event<
     :index #'event-rank))
   (configuration :reader configuration :initarg :configuration
           :documentation "Configuration data used for simulation")
   (initialized-p
    :initform nil :reader initialized-p
    :documentation "True if this component has been initialized.")
   (rng-map :type array :reader rng-map
            :documentation "Top level rng-map.")
   (num-rngs :parameter t :type fixnum :initform 1 :reader num-rngs
             :documentation "Total number of rngs for this simulation")
   (rng-class  :parameter t :type symbol :initform 'mt-random-state
              :documentation "Class for rng's")
   (warmup-period :parameter t :type time-type :initform 0 :reader warmup-period
                  :documentation "Warmup period for statistics collection")
   (cpu-time-limit
    :parameter t :type time-type :initform 300 :reader cpu-time-limit
    :documentation "Max cpu time for run")
   (sim-time-limit :parameter t :type time-type :initform (* 100 60 60 60)
                   :documentation "Maximum simulation run time"
                   :reader sim-time-limit)
   (network :parameter t :type symbol
            :documentation "Specified Network type parameter")
   (network-instance
    :reader network
    :documentation "Actual network instance in this simulation"))
  (:metaclass parameter-class)
  (:documentation "The simulation object"))

(defmethod print-object((simulation simulation) stream)
  (print-unreadable-object (simulation stream :type t :identity t)
     (format stream "time:~f~:[~; HALTED~] ~D pending events"
	     (clock simulation) (halted simulation)
       (alg:size (slot-value simulation 'event-queue)))))

(defmethod full-path((sim simulation)) (list nil))

(defmethod initialize-instance :around ((sim simulation) &key &allow-other-keys)
  (let ((*simulation* sim))
    (call-next-method)))

(defmethod configuration((instance owned-object))
  (configuration *simulation*))

(defmethod initialize-instance :after
    ((sim simulation) &key config run-number &allow-other-keys)
  ;; initialise global rng-map - use either seed-n or run-number
  (with-slots(rng-map num-rngs rng-class ) sim
    (setf (slot-value sim 'rng-map) (make-array num-rngs))
    (dotimes(i num-rngs)
      (multiple-value-bind(seed found-p)
          (read-parameter
           (list (intern (format nil "SEED-~D" i)))
           config
           `(integer :min 0 :max ,(1- num-rngs) ))
        (setf (aref rng-map i)
              (make-instance
               rng-class
               :seed (cond (found-p seed)
                           (run-number (* (1+ run-number) num-rngs))
                           (t t)))))))
  (setf (slot-value sim 'network-instance)
        (make-instance (slot-value sim 'network) :config config)))

(declaim (inline simulation-time))
(defun simulation-time(&optional (simulation *simulation*)) (clock simulation))

(defclass event()
  ((rank :type fixnum :initform -1
         :documentation "Rank in priority queue - used internally for
         efficient removal from queue.")
   (schedule-time :type time-type :reader schedule-time
                  :documentation "Time event was scheduled")
   (event-time
    :initarg :time :type time-type :accessor event-time :initform -1.0d0
    :documentation "simulation time at which event is to be handled")
   (priority
    :type fixnum :initform 0 :initarg :priority :reader priority
    :documentation "Determines delivery of messages with same arrival time")
   (schedule-id
    :initform -1 :type integer
    :documentation "Used to ensure events with same time and
   priority are scheduled in order of scheduling" :reader uid)
   (root-event :type event :reader root-event
               :documentation "Top level root for cloned messages"))
  (:documentation "Class representing a scheduled event"))

(defmethod initialize-instance :after ((event event) &key &allow-other-keys)
  (setf (slot-value event 'root-event) event))

(defmethod duplicate((event event) &optional (duplicate (make-instance 'event)))
  (call-next-method)
  (copy-slots '(schedule-time priority root-event) event duplicate))

(defclass simple-event(event)
  ((handler :reader handler :initarg :handler
            :documentation "The handler function for this event"))
  (:documentation "Class for simple events where handler is invoked"))

(defun event-rank(a &optional b)
  (if b (setf (slot-value a 'rank) b) (slot-value a 'rank)))

(defun event<(a b)
  (let ((ta (event-time a))
        (tb (event-time b)))
    (cond
      ((< ta tb))
      ((= ta tb)
       (let ((pa (priority a))
             (pb (priority b)))
         (cond
           ((> pa pb))
           ((= pa pb)
            (< (slot-value a 'event-id) (slot-value b 'event-id)))))))))

(defun scheduled-p(event) (>= (slot-value event 'rank) 0))

(defmethod print-object((event event) stream)
  (print-unreadable-object (event stream :type t :identity nil)
    (format stream "T=~,5f ~D"(event-time event)  (uid event) )))

(defgeneric handle(entity)
  (:documentation "Method called by simulation on a scheduled entity")
  (:method((f function)) (funcall f))
  (:method((form list)) (apply (first form) (rest form)))
  (:method((event simple-event)) (handle (handler event))))

(defgeneric schedule(delay handler)
  (:documentation "Schedule event on current simulation withy delay")
  (:method((delay number) (event event))
    (when (scheduled-p event)
      (error "Attempt to schedule an already active event ~A" event))
    (when (< delay 0)
      (error  "Attempt to schedule ~S ~D seconds in the past"
              event delay))
    (let ((simulation *simulation*))
      (setf (slot-value event 'event-time) (+ delay (simulation-time))
            (slot-value event 'schedule-time) (simulation-time)
            (slot-value event 'schedule-id)
            (incf (slot-value simulation 'last-schedule-id)))
      (enqueue event (slot-value simulation 'event-queue)))
    event)
  (:method ((delay number) handler)
    (schedule delay (make-instance 'simple-event :handler handler))))

(defgeneric cancel(event simulation)
  (:documentation "Cancel an event")
  (:method((event event) (simulation simulation))
    (when (>= (slot-value event 'rank) 0)
      (alg:delete event (slot-value simulation 'event-queue))
      (setf (slot-value event 'rank) -1))))

(defmethod stop((event event) &key &allow-other-keys)
  (cancel event *simulation*))

(defmethod reset((event event))
  (stop event))

(defmethod stop ((simulation simulation) &key abort)
  (setf (slot-value simulation 'halted) t)
  (when abort
    (when (slot-value simulation 'thread)
      (ignore-errors (kill-thread (slot-value simulation 'thread)))
      (setf (slot-value simulation 'thread) nil))))

(defun run(simulation &key (granularity 10000) step )
  "Execute the simulation returning the running
thread. granularity is the number of event to dispatch before
yielding the thread (default 10000). If granularity is nil all events
are dispatched in current thread"
  (flet((run(simulation)
          (setf (halted simulation) nil)
          (let ((*context* simulation)
                (cpu-end-time
                 (when (cpu-time-limit simulation)
                   (+ (get-internal-run-time)
                      (* (cpu-time-limit simulation)
                         internal-time-units-per-second))))
                (sim-time-limit (sim-time-limit simulation)))
            (loop
               :with count = 1
               :with q =  (slot-value simulation 'event-queue)
               :until (or (halted simulation)
                          (and sim-time-limit
                               (> (clock simulation) sim-time-limit))
                          (and cpu-end-time
                               (> (get-internal-run-time) cpu-end-time))
                          (empty-p q))
               :for event = (dequeue q)
               :do (progn
                     (setf (clock simulation) (event-time event))
                     (when step (funcall step event))
                     (handle event))
               :when (and granularity
                      (= (setf count (mod (1+ count) granularity)) 0))
               :do (yield-thread))
          (setf (halted simulation) t))))
    (stop simulation :abort t)
    (if granularity
        (setf (slot-value simulation 'thread)
              (make-thread
               #'(lambda()
                   (run simulation)
                   (setf (slot-value simulation 'thread) nil))
               "LENS Simulation"))
        (funcall #'run simulation))))

(defgeneric initialize(component &optional stage)
  (:documentation "Allowed depth-first staged initialization. Return
  true on final stage")
  (:method :around(component &optional stage)
    (declare (ignore stage))
    (let ((*context* component))
       (unless (initialized-p component)
         (setf (slot-value component 'initialized-p)
               (call-next-method)))))
  (:method(instance &optional stage)
    (declare (ignore instance stage))
    t)
  (:method((simulation simulation) &optional stage)
    (assert (not stage))
    (dolist(hook *simulation-init-hooks*) (funcall hook simulation))
    (let ((stage -1))
      (loop
         (when (initialize (network simulation) (incf stage))
           (return t))))))

(defgeneric initialized-p(entity)
  (:documentation "Return true if an entity has finished its initialization")
  (:method((simulation simulation)) (initialized-p (network simulation))))

(defgeneric finish(entity)
  (:documentation "Called depth first at end of simulation")
  (:method((simulation simulation))
    (stop simulation)
    (finish (network simulation))))

(defstruct timestamped
  (time (simulation-time) :type time-type)
  value)

(defun run-simulation(pathname &optional (key "General"))
  (let ((simulation
         (setf *simulation*
               (make-instance
                'simulation
                :configuration (read-configuration pathname key)))))
  (initialize simulation)
  (run simulation)
  (finish simulation)))
