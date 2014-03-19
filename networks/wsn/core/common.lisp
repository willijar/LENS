;; Some common definitions for wirless sensor network simulation
;; Copyright (C) 2014 Dr. John A.R. Williams

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

;; This includes some some standard signals, addresses and a wsn-module class
;; which provides timer handling (should maybe be refactored into LENS core)

;;; Code:

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

(defclass wsn-module(with-timers module)
  ((disabled-p :initform t :initarg :disabled-p :reader disabled-p))
  (:metaclass module-class))

(defmethod initialize list ((module wsn-module) &optional (stage 0))
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

(defmethod set-timer((instance wsn-module) (timer timer-message) interval
                     &optional name)
  "Schedule a timer using local time to determine interval"
  (call-next-method
   instance timer (get-simulation-time instance interval) name))

(defgeneric get-simulation-time(instance local-time)
  (:documentation "Convert a local time value into a simulation time")
  (:method(instance local-time)
    (get-simulation-time (submodule (node instance) 'resources) local-time)))

(defgeneric get-clock(instance)
  (:documentation "Return local absolute time")
  (:method(instance)
    (get-clock (submodule (node instance) 'resources))))
