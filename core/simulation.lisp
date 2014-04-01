;; Discrete time imulation and event handling
;; Copyright (C) 2013-2014 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;;; Copying:

;; This file is part of Lisp Educational Network Simulator (LENS)

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; LENS is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(in-package :lens)

(defvar *simulation-init-hooks* nil
  "A list of functions which take the simulation as an argument which
  are called after simulation etc is created. Can be used to add in
  layers which use signals such as statistical reporting or graphical
  presentation.")

(defvar run 0 "Current run number")
(defvar repetition 0 "Current repetition number")
(defvar configname "General" "Current configuration name")
(defvar datetime "" "Datetime string for current run")
(defvar network nil "Network name for current run")
(defvar runid "" "Runid for current run")
(defvar iterationvars "" "String representation of current run")

;; time requires double precision - set this as default for reader too
(deftype time-type() 'double-float)

(defclass simulation (named-object parameter-object)
  ((clock :type time-type :initform 0.0d0
	  :accessor clock :initarg :start-time
	  :documentation "Simulation virtual time")
   (halted :type boolean :initform t :accessor halted)
   (thread :initform nil :reader simulation-thread
	    :documentation "Thread running simulation")
   (last-schedule-id :type integer :initform 0)
   (event-queue :reader event-queue
    :initform
    (make-binary-heap
     :initial-size 1024
     :extend-size 1.4
     :element-type 'event
     :comp-fn #'event<
     :index #'event-rank))
   (configuration :reader configuration :initarg :configuration
                  :initform nil
                  :documentation "Configuration data used for simulation")
   (rng-map :type array :reader rng-map
            :documentation "Top level array of rngs")
   (seed-set :type integer :documentation "Seed set used in this simulation"
             :reader seed-set)
   (num-rngs :parameter t :type fixnum :initform 1 :reader num-rngs
             :documentation "Total number of rngs for this simulation")
   (rng-class  :parameter t :type symbol :initform 'mt-random-state
              :documentation "Class for rng's")
   (warmup-period :parameter t :type time-type :initform 0 :reader warmup-period
                  :documentation "Warmup period for statistics collection")
   (cpu-time-limit
    :parameter t :type time-type :initform 300 :reader cpu-time-limit
    :documentation "Max cpu time for run.")
   (sim-time-limit :parameter t :type time-type :initform (* 100 60 60 60)
                   :documentation "Maximum simulation run time"
                   :reader sim-time-limit)
   (network :parameter t :type symbol
            :documentation "Specified Network type parameter")
   (initialized-p :type boolean :initform nil)
   (network-instance
    :reader network
    :documentation "Actual network instance in this simulation")
   (vector-stream :initform nil :reader vector-stream :initarg :vector-stream
                  :documentation "Destination stream for vector results")
   (scalar-stream :initform nil :reader scalar-stream :initarg :scalar-stream
                  :documentation "Destination stream for scalar results"))
  (:metaclass parameter-class)
  (:default-initargs :name nil :owner nil :configuration-path (list nil))
  (:documentation "The main discrete time event simulation class reads
  global parameters, creates the network being simulated and manages
  the queue of events for the simulation. "))

(defmethod print-object((simulation simulation) stream)
  (print-unreadable-object (simulation stream :type t :identity t)
    (format stream "t=~f~:[~; HALTED~] ~D pending events"
	    (clock simulation) (halted simulation)
      (alg:size (slot-value simulation 'event-queue)))))

(defmethod full-path((sim simulation)) (list nil))

(defmethod network(instance) (network *simulation*))

(defmethod initialize-instance :around ((sim simulation) &key &allow-other-keys)
  (let ((*simulation* sim))
    (call-next-method)))

(defmethod configuration((instance owned-object))
  (configuration *simulation*))

(defmethod initialize-instance :after
    ((sim simulation) &key configuration &allow-other-keys)
  ;; initialise global rng-map - use either seed-n or run-number
  (with-slots(rng-map num-rngs rng-class seed-set) sim
    (setf (slot-value sim 'rng-map) (make-array num-rngs))
    (setf seed-set
          (let ((seed-set
                 (read-parameter '(nil SEED-SET) configuration '(read :package :lens))))
            (etypecase seed-set
              (null repetition)
              (integer seed-set)
              (symbol
               (ecase seed-set
                 (repetition repetition)
                 (run run)))
              (sequence (elt seed-set repetition)))))
      (dotimes(i num-rngs)
        (let ((seed
               (read-parameter
                (list nil (intern (format nil "SEED-~D" i)))
                configuration
                '(read :package :lens))))
          (setf (aref rng-map i)
                (make-instance
                 rng-class
                 :seed (typecase seed
                         (integer seed)
                         (null (+ i (* seed-set num-rngs)))
                         (sequence (elt seed repetition))
                         (t t)))))))
  (setf (slot-value sim 'network-instance)
        (make-instance (slot-value sim 'network)
                       :name (slot-value sim 'network)
                       :owner sim)))

(declaim (inline simulation-time))
(defun simulation-time(&optional (simulation *simulation*))
  "Return the simulation time of the current running simulation."
  (clock simulation))

(defgeneric arrival-time(event)
  (:documentation
   "Return the simulation time at which an event is to be handled"))

(defclass event()
  ((rank :type fixnum :initform -1
         :documentation "Rank in priority queue - used internally for
         efficient removal from queue.")
   (sent-time :type time-type :reader sent-time
                  :documentation "Time event was scheduled")
   (arrival-time
    :initarg :time :type time-type :accessor arrival-time :initform -1.0d0
    :documentation "simulation time at which event is to be handled")
   (priority
    :type fixnum :initform 0 :initarg :priority :accessor priority
    :documentation "Determines delivery of messages with same arrival time")
   (schedule-id
    :initform -1 :type integer :reader schedule-id
    :documentation "Used to ensure events with same time and
   priority are scheduled in order of scheduling" :reader uid)
   (root-event :type event :reader root-event
               :documentation "Top level root for cloned messages"))
  (:documentation "Class representing a scheduled event on a simulation."))

(declaim (inline scheduled-p))
(defun scheduled-p(event) (>= (slot-value event 'rank) 0))

(defun event<(a b)
  (let ((ta (arrival-time a))
        (tb (arrival-time b)))
    (cond
      ((< ta tb))
      ((= ta tb)
       (let ((pa (priority a))
             (pb (priority b)))
         (cond
           ((> pa pb))
           ((= pa pb)
            (< (slot-value a 'schedule-id) (slot-value b 'schedule-id)))))))))

(defun event=(a b)
  (= (arrival-time a) (arrival-time b)))

(defun event-queue-consistent-p(simulation)
  "Do a consistency check on event-queue by dequeing and checking
order and then reinqueueing"
  (let ((q (event-queue simulation))
        (result t)
        (events nil))
    (unless (empty-p q)
      (let ((prev (dequeue q)))
        (push prev events)
        (loop
           (when (empty-p q) (return))
           (let ((next (dequeue q)))
             (when (event< next prev)
               (setf result nil)
               (format t "~%Inconsistent at ~A -> ~A~%" (arrival-time prev) (arrival-time next)))
             (setf prev next)
             (push prev events))))
      (dolist(event (sort (copy-list events) #'< :key #'schedule-id))
        (enqueue event q)))
    (values result events)))

(defmethod (setf arrival-time) :before ((event event) time)
  (assert (not (scheduled-p event))))

(defmethod (setf priority) :before ((event event) time)
  (assert (not (scheduled-p event))))

(defmethod initialize-instance :after ((event event) &key &allow-other-keys)
  (setf (slot-value event 'root-event) event))

(defmethod duplicate((event event) &optional (duplicate (make-instance 'event)))
  (call-next-method)
  (copy-slots '(sent-time arrival-time priority root-event) event duplicate))

(defclass simple-event(event)
  ((handler :reader handler :initarg :handler
            :documentation "The handler for this event (an entity on
            which [[handle]] has been specialised. Support provided
            for functions and lisp function forms."))
  (:documentation "Class for simple events where a handler is invoked."))

(defun event-rank(a &optional b)
  (if b (setf (slot-value a 'rank) b) (slot-value a 'rank)))

(defmethod print-object((event event) stream)
  (print-unreadable-object (event stream :type t :identity nil)
    (if (>= (uid event) 0)
        (format stream "T=~,5f ~D"(arrival-time event)  (uid event) )
        (format stream "Unscheduled"))))

(defgeneric handle(entity)
  (:documentation "Method called by simulation on a scheduled entity")
  (:method((f function)) (funcall f))
  (:method((form list)) (apply (first form) (rest form)))
  (:method((event simple-event)) (handle (handler event))))

(defgeneric schedule(event &key delay time)
  (:documentation "* Arguments

- event :: and [[event]]

* Keyword Arguments

- delay :: a positive =real=
- time :: a positive =real=

* Description

Schedule event to be handled at the given simulation time /time/ or
/delay/ after current simulation time. If no /delay/ or /time/ is
provided the [[arrival-time]] value of the event is used. The
scheduled time must be >= current simulation-time i.e. in the
future.")
  (:method((event event) &key delay (time (arrival-time event) time-p))
    (let ((now (simulation-time)))
      (assert (not (scheduled-p event)))
      (when delay (setf time (+ delay now)))
      (when time-p (setf (slot-value event 'arrival-time) time))
      (assert (>= time now))
      (setf (slot-value event 'schedule-id)
            (incf (slot-value *simulation* 'last-schedule-id)))
      (enqueue event (event-queue *simulation*))
      (slot-value event 'schedule-id)))
  (:method(handler &key (delay 0) (time (+ delay (simulation-time))))
    (schedule (make-instance 'simple-event :handler handler) :time time)))

(defgeneric cancel(event)
  (:documentation "* Arguments

- event :: an [[event]]

* Description

Cancel event /event/ if it was scheduled.")
  (:method((event event))
    (when (>= (slot-value event 'rank) 0)
      (alg:delete event (slot-value *simulation* 'event-queue))
      (setf (slot-value event 'rank) -1)))
  (:method(handler)
    (alg:delete-if
     #'(lambda(event)
         (and (typep event 'simple-event) (eql handler (handler event))))
     (slot-value *simulation* 'event-queue))))

(defgeneric stop(simulation &key abort)
  (:documentation "* Arguments

- simulation :: a [[simulation]] object

* Keyword Arguments

- abort :: a =boolean=

* Description

Stop the /simulation/. If /abort/ is teu this will stop immediately by killing the thread in which the simulation is running otherwise it will stop after it has finished processing the current [[even]].")
  (:method((simulation simulation) &key abort)
    (setf (slot-value simulation 'halted) t)
    (when abort
      (when (slot-value simulation 'thread)
        (ignore-errors (kill-thread (slot-value simulation 'thread)))
        (setf (slot-value simulation 'thread) nil)))))

(defun run(simulation &key (granularity 10000) step )
  "* Arguments

- simulation :: a [[simulation]] object

* Keyword Arguments

- granularity :: an =integer= > 1
- step :: a =function=

* Returns

-- thread :: a thread object

* Description

Executes the simulation returning the running thread. /granularity/ is
the number of events to dispatch before yielding the thread (default
10000). If /granularity/ is nil all events are dispatched in current
thread. If a /step/ function is provided it will be called before each
[[event]] is dispatched."
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
                     (setf (clock simulation) (arrival-time event))
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
  (:documentation "* Arguments

- component :: a simulation [[component]]
- stage :: a positive =integer=

* Return

- finished :: a boolean

* Description

This method is called for every comonent in the simulation after the
whole simulation is created but before the first event is executed. It
allows depth-first staged initialization and configuration for
components which may depend on other components having being
created. It will be called multiple times with /stage/ increasing by
1 (from an initial value of 0) every time until it returns /finished/
as true. This allows for multiple stage initialisation between
codependent object types. Implementations should therefore check the
/stage/ value to insure they don't initialise more than once. An
object is only deemed to be initialised once it [[initialize]] method
and the [[intialize]] method of all subcomponents return true.

list method combination is used so that when subclassing all the
relevant [[initialize]] methods must return true for the effective
method to return true.")
  (:method-combination list)
  (:method :around(component &optional stage)
    (declare (ignore stage))
    (or (initialized-p component))
        (let ((*context* component))
          (setf (slot-value component 'initialized-p)
                      (every #'identity (call-next-method)))))
  (:method list (instance &optional stage)
    (declare (ignore instance stage))
    t)
  (:method list ((simulation simulation) &optional stage)
    (assert (not stage))
    (dolist(hook *simulation-init-hooks*) (funcall hook simulation))
    (let ((stage -1))
      (loop
         (when (initialize (network simulation) (incf stage))
           (return t))))))

(defgeneric initialized-p(component)
  (:documentation "* Arguments

- component ::  a simulation [[component]]

* Description

Returns true if an entity has finished its initialization using the
[[initialize]] method.")
  (:method((simulation simulation)) (initialized-p (network simulation))))

(defgeneric finish(component)
  (:documentation "* Arguments

- component ::  a simulation [[component]]

* Description

Called depth first for every /component/ at end of simulation. May bee used to finalise statistics and alalysis summary.")
  (:method(entity)
    (warn "No finish method defined for a ~A" entity))
  (:method((simulation simulation))
    (stop simulation)
    (finish (network simulation))))

(defvar *simulation-trace-stream* *standard-output*
  "The stream on which tracing information (using [[tracelog]]) is to be written.")

(let ((last-context nil))
  (defun %tracelog(&rest args)
    (unless (eql *context* last-context)
      (setf last-context *context*)
      (format *simulation-trace-stream* "~A~%" last-context))
    (write-char #\space *simulation-trace-stream*)
    (format *simulation-trace-stream* *time-format* (simulation-time))
    (write-char #\space *simulation-trace-stream*)
    (apply #'format *simulation-trace-stream* args)
    (write-char #\newline *simulation-trace-stream*)))

(defmacro tracelog(&rest args)
  "* Rest Arguments

- args :: list of format arguments.

* Description

Write to [[\*simulation-trace-stream\*]] using /args/ provided the
[[collect-trace-info]] parameter for the current [[\*context\*]] is true.

This is designed to enable efficient configuration file controlled
tracing of simulation execution."
  `(when (slot-value *context* 'collect-trace-info)
     (%tracelog ,@args)))

(defstruct timestamped
  "* Slots

- time :: a /time-type/ (default [[simulation-time]])
- value :: a value

* Description

Structure associating a time with a value."
  (time (simulation-time) :type time-type)
  value)

(defgeneric latency(entity)
  (:documentation "* Arguments

- entity :: an object

* Description

Return the latency of the packet or timestampted /entity/ - the time
difference between current [[simulation-time]] and the time the
/rntity/ was timestamped.
")
  (:method((entity timestamped))
    (- (simulation-time) (timestamped-time entity))))

;; TODO Parameter studies - looping Last one is innermost if unnamed
;; given names 0,1,2 etc syntax is {form ... } if one form which is a
;; symbol then the global symbol value is used if two forms and first
;; one is a symbol second one is evaluated to give iteration values
;; which are assigned in turn to global value if multiple values and
;; first is a symbol then reamining values form iteration list; if
;; first is not a symbol then it is just a list of values and a name
;; will be allocated.

;; structure to store lisp evaluations for parameters
;; this should occur for each lisp form which occurs as in as trie-value
;;
(defstruct parameter-expansion
  ;; global symbol name for variable if set
  trie ;; trie where defined
  (seq nil :type list) ;; list of strings and variable names to be substituted for value in this trie per iteration
  (vars nil)) ;; list of cons of variabe name and forms to be evaluated
            ;; to get itetation

(defun parameter-expansion-value(pe)
  (with-output-to-string(os)
    (dolist(term (parameter-expansion-seq pe))
       (if (symbolp term)
           (write (symbol-value term) :stream os)
           (write-string term os)))))

(defun get-expansion(trie)
  "return name and list of strings from an iteration value"
  (let*((value (trie-value trie))
        (matches (ppcre::all-matches "{[^\\}]+}" value)))
    (when matches
      (let ((upto 0)
            seq vars)
        (dotimes(x (/ (length matches) 2))
          (let* ((start (elt matches (* 2 x)))
                 (end (elt matches (1+ (* 2 x))))
                 (expression (subseq value (1+ start) (1- end)))
                 (forms (parse-input 'read
                                     expression
                                     :multiplep t
                                     :package #.*package*)))
            (when (<= upto (1- start))
              (push (subseq value upto start) seq))
            (setf upto end)
            (cond
              ((and (symbolp (first forms)) (not (rest forms)))
               ;; simply a variable expansion
               (push (first forms) seq))
              (t ;; an iteration assignment
               (multiple-value-bind(var assignment)
                   (if (symbolp (first forms))
                       (values (first forms) (rest forms))
                       (values (gentemp "$" (find-package :lens)) forms))
                 (push var seq)
                 (push
                  (cons var (if (rest assignment)
                                (list 'quote assignment)
                                (eval (first assignment))))
                  vars ))))))
        (when (< upto (length value))
          (push (subseq value upto) seq))
        (make-parameter-expansion
         :trie trie
         :seq (nreverse seq)
         :vars (nreverse vars))))))

(defun get-expansions(trie)
  (nconc
   (mapcan #'get-expansions (trie-children trie))
   (when (slot-boundp trie 'trie-value)
     (let ((exp (get-expansion trie)))
       (when exp (list exp))))))

(defun parameter-expansion-order(exp)
  (parameter-source-line-number (trie-source (parameter-expansion-trie exp))))

(defun run-simulations(pathname &key (config "General")
                       (repeat 1) runnumber preview)
  "* Arguments

- pathname :: a path designator

* Keyword Arguments

- config :: a =string= or list of strings (default \"General\")
- repeat :: an =integer= >= 1 (default 1)
- runnumber :: an =integer= >= 1
- preview :: a =boolean=

* Description

Executes one or more simulations using the configuration file at
/pathname/. /config/ lists one or more sections of the configuration
file to use for this run. They will be loaded in order and the
\"General\" section will always be read last. /repeat/ specifies how
many times to run the simulation. Each run will be with different seed
parameters of the random number generators. If /runnumber/ is
specified then it specifies a single run within a sequence if the
configuration file specifies iteration over some parameters.  If
/preview/ is true simulations are not run but onfiguration parameters
for each run are printed out."
  (let* ((trie (read-configuration pathname config))
         (exp (sort (get-expansions trie) #'<
                    :key #'parameter-expansion-order))
         (vars (mapcan #'parameter-expansion-vars exp))
         (run 0)
         (repetition 0)
         (configname
          (etypecase config
            (string config)
            (list (format nil "~{~A~^-~}" config))))
         (datetime
          (multiple-value-bind (se mi ho da mo ye)
              (decode-universal-time (get-universal-time))
            (format nil "~4d~2,'0d~2,'0d-~2,'0d:~2,'0d:~2,'0d"
                    ye mo da ho mi se)))
         (network (read-parameter '(nil network) trie 'symbol))
         (output-path
          (merge-pathnames
           (make-pathname
            :name (format nil "~A-~A" (pathname-name pathname)
                          configname))
           pathname))
         (scalar-path
          (multiple-value-bind(v f-p)
              (read-parameter '(nil scalar-recording) trie 'boolean)
            (when (or v (not f-p))
              (or (read-parameter '(nil scalar-file) trie 'pathname)
                  (merge-pathnames (make-pathname :type "sca") output-path)))))
         (vector-path
          (multiple-value-bind(v f-p)
              (read-parameter '(nil vector-recording) trie 'boolean)
            (when (or v (not f-p))
              (or (read-parameter '(nil vector-file) trie 'pathname)
                  (merge-pathnames (make-pathname :type "vec") output-path)))))
         (scalar-stream
          (when scalar-path
            (open  scalar-path
                  :direction :output
                  :if-exists :supersede
                  :if-does-not-exist :create)))
         (vector-stream
          (when vector-path
            (open vector-path
                  :direction :output
                  :if-exists :supersede
                  :if-does-not-exist :create))))
    (when scalar-stream (write-line "version 2" scalar-stream))
    (labels
       ((do-run()
          (setq runid (format nil "~A-~A-~A-~A"
                              (pathname-name pathname) configname run datetime))
           (when (or (not runnumber) (= runnumber run))
             (dolist(e exp) ;; setup trie expansions
                 (setf (trie-value (parameter-expansion-trie e))
                       (parameter-expansion-value e)))
             (setq iterationvars
                   (format nil "~{~A=~A~^ ~}"
                           (mapcan #'(lambda(v) (list (car v)
                                                      (symbol-value (car v))))
                                   vars)))
             (format t "~%run~T#~A~T~A~%...~%" run iterationvars)
             (unless preview
                 (let ((simulation
                        (setq *simulation*
                              (make-instance 'simulation
                                        :configuration trie
                                        :scalar-stream scalar-stream
                                        :vector-stream vector-stream))))
                   (initialize simulation)
                   (when scalar-stream
                     (format scalar-stream "run ~A~%~{attr ~A ~S~%~}"
                             runid
                             `("configname" ,configname
                               "inifile" ,(namestring pathname)
                               "datetime" ,datetime
                               "seedset" ,(seed-set simulation)
                               "runnumber" ,run
                               "repetition" ,repetition
                               "experiment"
                               ,(or (read-parameter '(nil experiment-label)
                                                    trie nil)
                                    configname)
                               "measurement"
                               ,(or (read-parameter '(nil measurement-label)
                                                    trie nil)
                                    iterationvars)
                               "replications"
                               ,(or (read-parameter '(nil replication-label)
                                                    trie nil)
                                    (format nil "~A,seed-set=~D"
                                            repetition
                                            (seed-set simulation)))))
                     (format scalar-stream "attr iterationvars ~A~%"
                             iterationvars))
                   (run simulation :granularity nil)
                   (finish simulation))))
           (incf run))
        (nested-loop(var inner)
          (dolist(x (eval (cdr var)))
            (setf (symbol-value (car var)) x)
            (if inner
                (nested-loop (car inner) (cdr inner))
                (do-run) ))))
      (dotimes(x repeat)
        (setq repetition x)
        (if vars
            (nested-loop (car vars) (cdr vars))
            (do-run)))
      (when scalar-stream (close scalar-stream))
      (when vector-stream (close vector-stream)))))




#|
(setf *simulation*
               (make-instance
                'simulation
                :configuration (read-configuration *tictoc* "TicToc1")))

(run-simulation *tictoc* "TicToc1")

|#
