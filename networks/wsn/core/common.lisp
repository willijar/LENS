(in-package :lens.wsn)

(register-signal
 'node-move
 "Emitted immediately after node location has changed.")

(register-signal
 'out-of-energy
 "Emitted as soon as a module is out of energy")

(register-signal
 'node-startup
 "Emitted to restart a node and all submodules")

(register-signal
 'node-shutdown
 "Signaled to shutdown node and all submodules")

(register-signal
 'out-of-memory
 "Emitted as soon as module is out of memory")

(register-signal
 'application-receive
 "Signaled with the packet when an application receives a packet")

(register-signal
 'application-send
 "Signaled with packet when an application receives a packet")

(defconstant broadcast-network-address -1)
(defconstant broadcast-mac-address -1)
(defvar *simulations*
  #p"/home/willijar/dev/lisp/src/lens/networks/wsn/simulations/")

(defclass wsn-module(module)
  ((disabled-p :initform t :initarg :disabled-p :reader disabled-p))
  (:metaclass module-class))

(defmethod print-object((m wsn-module) os)
  (print-unreadable-object(m os :type t :identity nil)
    (format os "~A ~A" (name m) (nodeid (node m)))))

(defmethod initialize and ((module wsn-module) &optional (stage 0))
  (case stage
    (0
     (subscribe (node module) 'node-shutdown module)
     (subscribe (node module) 'node-startup module)))
  t)

(defgeneric startup(module)
  (:documentation "Called to start or restart a module")
  (:method :before ((module wsn-module))
    (assert (disabled-p module)
            ()
            "Attempt to start an already running module ~A" module)
    (setf (slot-value module 'disabled-p) nil))
  (:method(instance) (declare (ignore instance))))

(defgeneric shutdown(module)
  (:method :before ((module wsn-module))
    (assert (not (disabled-p module))
            ()
            "Attempt to shutdown disabled module ~A" module)
    (setf (slot-value module 'disabled-p) t))
  (:method(instance) (declare (ignore instance))))

(defgeneric node(module)
  (:documentation "Return the node module for a particular submodule")
  (:method((module wsn-module)) (owner module)))

(defmethod receive-signal((instance wsn-module)
                          signal
                          source value)
  (case signal
    (node-shutown (unless (disabled-p instance) (shutdown instance)))
    (node-startup (when (disabled-p instance) (startup instance)))))

(defgeneric get-simulation-time(instance local-time)
  (:documentation "Convert a local time value into a simulation time")
  (:method(instance local-time)
    (get-simulation-time (submodule (node instance) 'resources) local-time)))

(defgeneric set-timer(module timer interval)
  (:documentation "Schedule a timer using local time to determine interval")
  (:method(module (timer message) interval)
    (schedule-at module timer
                 :delay (get-simulation-time module interval))))

(defmethod handle-message((module wsn-module) (message message))
  (case (name message)
    (node-startup (startup module))
    (node-shutdown (shutdown module))
    (t (call-next-method))))